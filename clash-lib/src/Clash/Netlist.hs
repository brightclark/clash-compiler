{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Create Netlists out of normalized CoreHW Terms
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist where

import           Control.Exception                (throw)
import           Control.Lens                     ((.=), (<~))
import qualified Control.Lens                     as Lens
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.State.Strict       (State, runStateT, runState)
import           Data.Binary.IEEE754              (floatToWord, doubleToWord)
import           Data.Char                        (ord)
import           Data.Either                      (partitionEithers, rights)
import           Data.Foldable                    (foldlM)
import qualified Data.HashMap.Strict              as HashMapS
import qualified Data.HashMap.Lazy                as HashMap
import           Data.List                        (elemIndex, partition, sortOn)
import           Data.List.Extra                  (zipEqual)
import           Data.Maybe
  (catMaybes, listToMaybe, mapMaybe, fromMaybe)
import qualified Data.Set                         as Set
import           Data.Primitive.ByteArray         (ByteArray (..))
import qualified Data.Text                        as StrictText
import           GHC.Integer.GMP.Internals        (Integer (..), BigNat (..))
import           System.FilePath                  ((</>), (<.>))
import           Text.Read                        (readMaybe)

import           Outputable                       (ppr, showSDocUnsafe)
import           SrcLoc                           (isGoodSrcSpan)

import           Clash.Annotations.Primitive      (extractPrim, HDL)
import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr'(..), ConstrRepr'(..), getDataRepr, getConstrRepr)
import           Clash.Core.DataCon               (DataCon (..))
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name                  (Name(..))
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Term
  ( Alt, Pat (..), Term (..), TickInfo (..), PrimInfo(primName), collectArgs
  , collectArgsTicks, collectTicks, mkApps, mkTicks, stripTicks)
import qualified Clash.Core.Term                  as Core
import           Clash.Core.TermInfo              (termType)
import           Clash.Core.Type
  (Type (..), coreView1, splitFunForallTy, splitCoreFunForallTy)
import           Clash.Core.TyCon                 (TyConMap)
import           Clash.Core.Util                  (splitShouldSplit)
import           Clash.Core.Var                   (Id, Var (..), isGlobalId)
import           Clash.Core.VarEnv
  (VarEnv, eltsVarEnv, emptyInScopeSet, emptyVarEnv, extendVarEnv, lookupVarEnv,
   lookupVarEnv')
import           Clash.Driver.Types               (BindingMap, Binding(..), ClashOpts (..))
import           Clash.Netlist.BlackBox
import qualified Clash.Netlist.Id                 as Id
import           Clash.Netlist.Types              as HW
import           Clash.Netlist.Util
import           Clash.Primitives.Types           as P
import           Clash.Util
import qualified Clash.Util.Interpolate           as I

-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist
  :: Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> VarEnv TopEntityT
  -- ^ TopEntity annotations
  -> VarEnv Identifier
  -- ^ Top entity names
  -> CompiledPrimMap
  -- ^ Primitive definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> SomeBackend
  -- ^ The current HDL backend
  -> IdentifierSet
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> Maybe StrictText.Text
  -- ^ Component name prefix
  -> Id
  -- ^ Name of the @topEntity@
  -> IO (Component, [([Bool],SrcSpan,IdentifierSet,Component)], IdentifierSet)
genNetlist isTb opts reprs globals tops topNames primMap tcm typeTrans iw ite be seen env prefixM topEntity = do
  ((_wereVoids, _sp, _is, topComponent), s) <-
    runNetlistMonad isTb opts reprs globals tops topNames primMap tcm typeTrans
                    iw ite be seen env prefixM $ genComponent topEntity
  return (topComponent, eltsVarEnv (_components s), _seenIds s)

-- | Run a NetlistMonad action in a given environment
runNetlistMonad
  :: Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> VarEnv TopEntityT
  -- ^ TopEntity annotations
  -> VarEnv Identifier
  -- ^ Top entity names
  -> CompiledPrimMap
  -- ^ Primitive Definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcode Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> SomeBackend
  -- ^ The current HDL backend
  -> IdentifierSet
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> Maybe StrictText.Text
  -- ^ Component name prefix
  -> NetlistMonad a
  -- ^ Action to run
  -> IO (a, NetlistState)
runNetlistMonad isTb opts reprs s tops topNames p tcm typeTrans iw ite be seenIds0 env prefixM
  = flip runReaderT (NetlistEnv "" "" Nothing)
  . flip runStateT s'
  . runNetlist
  where
    s' =
      NetlistState
        { _bindings=s
        , _components=emptyVarEnv
        , _primitives=p
        , _typeTranslator=typeTrans
        , _tcCache=tcm
        , _curCompNm=(error "genComponent should have set _curCompNm", noSrcSpan)
        , _intWidth=iw
        , _seenIds=seenIds1
        , _seenComps=seenIds1
        , _seenPrimitives=Set.empty
        , _componentNames=componentNames_
        , _topEntityAnns=tops
        , _hdlDir=env
        , _curBBlvl=0
        , _customReprs=reprs
        , _clashOpts=opts
        , _isTestBench=isTb
        , _backEndITE=ite
        , _backend=be
        , _htyCache=HashMapS.empty
        }

    (componentNames_, seenIds1) =
      genNames (opt_newInlineStrat opts) prefixM seenIds0 topNames s

-- | Generate names for all binders in "BindingMap", execpt for the ones already
-- present in given identifier varenv.
genNames
  :: Bool
  -- ^ New inline strategy enabled?
  -> Maybe StrictText.Text
  -- ^ Prefix
  -> IdentifierSet
  -- ^ Identifier set to extend
  -> VarEnv Identifier
  -- ^ Pre-generated names
  -> BindingMap
  -> (VarEnv Identifier, IdentifierSet)
genNames newInlineStrat prefixM is env bndrs =
  runState (foldlM go env bndrs) is
 where
  go env_ (bindingId -> id_) =
    case lookupVarEnv id_ env_ of
      Just _ -> pure env_
      Nothing -> do
        nm <- Id.makeBasic (genComponentName newInlineStrat prefixM id_)
        pure (extendVarEnv id_ nm env_)

-- | Generate names for top entities. Should be executed at the very start of
-- the synthesis process and shared between all passes.
genTopNames
  :: Maybe StrictText.Text
  -- ^ Prefix
  -> Bool
  -- ^ Allow escaped identifiers?
  -> HDL
  -- ^ HDL to generate identifiers for
  -> [TopEntityT]
  -> (VarEnv Identifier, IdentifierSet)
genTopNames prefixM esc hdl tops =
  -- TODO: Report error if fixed top entities have conflicting names
  flip runState (Id.emptyIdentifierSet esc hdl) $ do
    env0 <- foldlM goFixed emptyVarEnv fixedTops
    env1 <- foldlM goNonFixed env0 (nonFixedTops ++ tests)
    pure env1
 where
  fixedTops = [(topId, ann) | TopEntityT{topId, topAnnotation=Just ann} <- tops]
  nonFixedTops = [topId | TopEntityT{topId, topAnnotation=Nothing} <- tops]
  tests = [testId | TopEntityT{associatedTestbench=Just testId} <- tops]

  goFixed env (topId, ann) = do
    topNm <- genTopName prefixM ann
    pure (extendVarEnv topId topNm env)

  goNonFixed env id_ = do
    topNm <- Id.makeBasic (genComponentName True prefixM id_)
    pure (extendVarEnv id_ topNm env)

-- | Generate a component for a given function (caching)
genComponent
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> NetlistMonad ([Bool],SrcSpan,IdentifierSet,Component)
genComponent compName = do
  compExprM <- lookupVarEnv compName <$> Lens.use bindings
  case compExprM of
    Nothing -> do
      (_,sp) <- Lens.use curCompNm
      throw (ClashException sp ($(curLoc) ++ "No normalized expression found for: " ++ show compName) Nothing)
    Just b -> do
      makeCachedU compName components $ genComponentT compName (bindingTerm b)

-- | Generate a component for a given function
genComponentT
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> Term
  -- ^ Corresponding term
  -> NetlistMonad ([Bool],SrcSpan,IdentifierSet,Component)
genComponentT compName0 componentExpr = do
  tcm <- Lens.use tcCache
  compName1 <- (`lookupVarEnv'` compName0) <$> Lens.use componentNames
  sp <- (bindingLoc . (`lookupVarEnv'` compName0)) <$> Lens.use bindings
  curCompNm .= (compName1, sp)

  topEntityTM <- lookupVarEnv compName0 <$> Lens.use topEntityAnns
  let topAnnMM = topAnnotation <$> topEntityTM
      topVarTypeM = snd . splitCoreFunForallTy tcm . varType . topId <$> topEntityTM

  seenIds <~ Lens.use seenComps
  (wereVoids,compInps,argWrappers,compOutps,resUnwrappers,binders,resultM) <-
    case splitNormalized tcm componentExpr of
      Right (args, binds, res) -> do
        let varType1 = fromMaybe (varType res) topVarTypeM
        mkUniqueNormalized
          emptyInScopeSet
          topAnnMM
          -- HACK: Determine resulttype of this function by looking at its definition
          -- instead of looking at its last binder (which obscures any attributes
          -- [see: Clash.Annotations.SynthesisAttributes]).
          ((args, binds, res{varType=varType1}))
      Left err ->
        throw (ClashException sp ($curLoc ++ err) Nothing)

  netDecls <- fmap catMaybes . mapM mkNetDecl $ filter (maybe (const True) (/=) resultM . fst) binders
  decls    <- concat <$> mapM (uncurry mkDeclarations) binders

  case resultM of
    Just result -> do
      Just (NetDecl' _ rw _ _ rIM) <- mkNetDecl . head $ filter ((==result) . fst) binders

      let (compOutps',resUnwrappers') = case compOutps of
            [oport] -> ([(rw,oport,rIM)],resUnwrappers)
            _       -> let NetDecl n res resTy = head resUnwrappers
                       in  (map (Wire,,Nothing) compOutps
                           ,NetDecl' n rw res (Right resTy) Nothing:tail resUnwrappers
                           )
          component      = Component compName1 compInps compOutps'
                             (netDecls ++ argWrappers ++ decls ++ resUnwrappers')
      ids <- Lens.use seenIds
      return (wereVoids, sp, ids, component)
    -- No result declaration means that the result is empty, this only happens
    -- when the TopEntity has an empty result. We just create an empty component
    -- in this case.
    Nothing -> do
      let component = Component compName1 compInps [] (netDecls ++ argWrappers ++ decls)
      ids <- Lens.use seenIds
      return (wereVoids, sp, ids, component)

mkNetDecl :: (Id, Term) -> NetlistMonad (Maybe Declaration)
mkNetDecl (id_,tm) = preserveVarEnv $ do
  let typ             = varType id_
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) typ
  wr   <- termToWireOrReg tm
  rIM  <- getResInit (id_,tm)
  if isVoid hwTy
     then return Nothing
     else return . Just $ NetDecl' (addSrcNote sp)
             wr
             (id2identifier id_)
             (Right hwTy)
             rIM

  where
    nm = varName id_
    sp = case tm of {Tick (SrcSpan s) _ -> s; _ -> nameLoc nm}

    termToWireOrReg :: Term -> NetlistMonad WireOrReg
    termToWireOrReg (stripTicks -> Case scrut _ alts0@(_:_:_)) = do
      tcm <- Lens.use tcCache
      let scrutTy = termType tcm scrut
      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
      ite <- Lens.use backEndITE
      case iteAlts scrutHTy alts0 of
        Just _ | ite -> return Wire
        _ -> return Reg
    termToWireOrReg (collectArgs -> (Prim p,_)) = do
      bbM <- HashMap.lookup (primName p) <$> Lens.use primitives
      case bbM of
        Just (extractPrim -> Just BlackBox {..}) | outputReg -> return Reg
        _ | primName p == "Clash.Explicit.SimIO.mealyIO" -> return Reg
        _ -> return Wire
    termToWireOrReg _ = return Wire

    addSrcNote loc = if isGoodSrcSpan loc
                        then Just (StrictText.pack (showSDocUnsafe (ppr loc)))
                        else Nothing

    -- Set the initialization value of a signal when a primitive wants to set it
    getResInit
      :: (Id,Term) -> NetlistMonad (Maybe Expr)
    getResInit (i,collectArgsTicks -> (k,args,ticks)) = case k of
      Prim p -> extractPrimWarnOrFail (primName p) >>= go (primName p)
      _ -> return Nothing
     where
      go pNm (BlackBox {resultInit = Just nmD}) = withTicks ticks $ \_ -> do
        (bbCtx,_) <- mkBlackBoxContext pNm i args
        (bbTempl,templDecl) <- prepareBlackBox pNm nmD bbCtx
        case templDecl of
          [] -> return (Just (BlackBoxE pNm [] [] [] bbTempl bbCtx False))
          _  -> do
            (_,sloc) <- Lens.use curCompNm
            throw (ClashException sloc
                    (unwords [ $(curLoc)
                             , "signal initialization requires declarations:\n"
                             , show templDecl
                             ])
                    Nothing)
      go _ _ = return Nothing

-- | Generate a list of concurrent Declarations for a let-binder, return an
-- empty list if the bound expression is represented by 0 bits
mkDeclarations
  :: HasCallStack
  => Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations = mkDeclarations' Concurrent

-- | Generate a list of Declarations for a let-binder, return an empty list if
-- the bound expression is represented by 0 bits
mkDeclarations'
  :: HasCallStack
  => DeclarationType
  -- ^ Concurrent of sequential declaration
  -> Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations' _declType bndr (collectTicks -> (Var v,ticks)) =
  withTicks ticks $ \tickDecls -> do
  mkFunApp (id2identifier bndr) v [] tickDecls

mkDeclarations' _declType _bndr e@(collectTicks -> (Case _ _ [],_)) = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException
          sp
          ( unwords [ $(curLoc)
                    , "Not in normal form: Case-decompositions with an"
                    , "empty list of alternatives not supported:\n\n"
                    , showPpr e
                    ])
          Nothing

mkDeclarations' declType bndr (collectTicks -> (Case scrut altTy alts@(_:_:_),ticks)) =
  withTicks ticks $ \tickDecls -> do
  mkSelection declType (CoreId bndr) scrut altTy alts tickDecls

mkDeclarations' declType bndr app = do
  let (appF,args0,ticks) = collectArgsTicks app
      (args,tyArgs) = partitionEithers args0
  case appF of
    Var f
      | null tyArgs -> withTicks ticks (mkFunApp (id2identifier bndr) f args)
      | otherwise   -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
    _ -> do
      (exprApp,declsApp0) <- mkExpr False declType (CoreId bndr) app
      let dstId = id2identifier bndr
          assn  =
            case exprApp of
              Identifier _ Nothing ->
                -- Supplied 'bndr' was used to assign a result to, so we
                -- don't have to manually turn it into a declaration
                []
              Noop ->
                -- Rendered expression rendered a "noop" - a list of
                -- declarations without a result. Used for things like
                -- mealy IO / inline assertions.
                []
              _ ->
                -- Turn returned expression into declaration by assigning
                -- it to 'dstId'
                [Assignment dstId exprApp]
      declsApp1 <- if null declsApp0
                   then withTicks ticks return
                   else pure declsApp0
      return (declsApp1 ++ assn)

-- | Generate a declaration that selects an alternative based on the value of
-- the scrutinee
mkSelection
  :: DeclarationType
  -> NetlistId
  -> Term
  -> Type
  -> [Alt]
  -> [Declaration]
  -> NetlistMonad [Declaration]
mkSelection declType bndr scrut altTy alts0 tickDecls = do
  let dstId = netlistId1 id id2identifier bndr
  tcm <- Lens.use tcCache
  let scrutTy = termType tcm scrut
  scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  scrutId  <- Id.suffix dstId "selection"
  (_,sp) <- Lens.use curCompNm
  ite <- Lens.use backEndITE
  altHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
  case iteAlts scrutHTy alts0 of
    Just (altT,altF)
      | ite
      , Concurrent <- declType
      -> do
      (scrutExpr,scrutDecls) <- case scrutHTy of
        SP {} -> first (mkScrutExpr sp scrutHTy (fst (last alts0))) <$>
                   mkExpr True declType (NetlistId scrutId scrutTy) scrut
        _ -> mkExpr False declType (NetlistId scrutId scrutTy) scrut
      altTId <- Id.suffix dstId "sel_alt_t"
      altFId <- Id.suffix dstId "sel_alt_f"
      (altTExpr,altTDecls) <- mkExpr False declType (NetlistId altTId altTy) altT
      (altFExpr,altFDecls) <- mkExpr False declType (NetlistId altFId altTy) altF
      -- This logic (and the same logic a few lines below) is faulty in the
      -- sense that it won't generate "void decls" if the alternatives' type
      -- is void, but the type of the scrut isn't. Ideally, we'd like to pass
      -- a boolean to 'mkExpr' indicating that it should only render "void decls"
      -- but that it should skip any others.
      --
      -- TODO: Fix ^
      if | isVoid altHTy && isVoid scrutHTy
          -> return $! scrutDecls ++ altTDecls ++ altFDecls
         | isVoid altHTy
          -> return $! altTDecls ++ altFDecls
         | otherwise
          -> return $! scrutDecls ++ altTDecls ++ altFDecls ++ tickDecls ++
                [Assignment dstId (IfThenElse scrutExpr altTExpr altFExpr)]
    _ -> do
      reprs <- Lens.use customReprs
      let alts1 = (reorderDefault . reorderCustom tcm reprs scrutTy) alts0
      (scrutExpr,scrutDecls) <- first (mkScrutExpr sp scrutHTy (fst (head alts1))) <$>
                                  mkExpr True declType (NetlistId scrutId scrutTy) scrut
      (exprs,altsDecls)      <- unzip <$> mapM (mkCondExpr scrutHTy) alts1
      case declType of
        Sequential -> do
          -- Assign to the result in every branch
          let (altNets,exprAlts) = unzip (zipWith (altAssign dstId)
                                                  exprs altsDecls)
          return $! scrutDecls ++ tickDecls ++ concat altNets ++
                    [Seq [Branch scrutExpr scrutHTy exprAlts]]
        Concurrent ->
          if | isVoid altHTy && isVoid scrutHTy
              -> return $! concat altsDecls ++ scrutDecls
             | isVoid altHTy
              -> return $! concat altsDecls
             | otherwise
              -> return $! scrutDecls ++ concat altsDecls ++ tickDecls
                    ++ [CondAssignment dstId altHTy scrutExpr scrutHTy exprs]
 where
  mkCondExpr :: HWType -> (Pat,Term) -> NetlistMonad ((Maybe HW.Literal,Expr),[Declaration])
  mkCondExpr scrutHTy (pat,alt) = do
    altId <- Id.suffix (netlistId1 id id2identifier bndr) "sel_alt"
    (altExpr,altDecls) <- mkExpr False declType (NetlistId altId altTy) alt
    (,altDecls) <$> case pat of
      DefaultPat           -> return (Nothing,altExpr)
      DataPat dc _ _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
      LitPat  (IntegerLiteral i) -> return (Just (NumLit i),altExpr)
      LitPat  (IntLiteral i) -> return (Just (NumLit i), altExpr)
      LitPat  (WordLiteral w) -> return (Just (NumLit w), altExpr)
      LitPat  (CharLiteral c) -> return (Just (NumLit . toInteger $ ord c), altExpr)
      LitPat  (Int64Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Word64Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (NaturalLiteral n) -> return (Just (NumLit n), altExpr)
      _  -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not an integer literal in LitPat:\n\n" ++ showPpr pat) Nothing)

  mkScrutExpr :: SrcSpan -> HWType -> Pat -> Expr -> Expr
  mkScrutExpr sp scrutHTy pat scrutE = case pat of
    DataPat dc _ _ -> let modifier = Just (DC (scrutHTy,dcTag dc - 1))
                      in case scrutE of
                          Identifier scrutId Nothing -> Identifier scrutId modifier
                          _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Not a variable reference or primitive as subject of a case-statement:\n\n" ++ show scrutE) Nothing)
    _ -> scrutE

  altAssign :: Identifier -> (Maybe HW.Literal,Expr) -> [Declaration]
            -> ([Declaration],(Maybe HW.Literal,[Seq]))
  altAssign i (m,expr) ds =
    let (nets,rest) = partition isNet ds
        assn = case expr of { Noop -> []; _ -> [SeqDecl (Assignment i expr)] }
    in  (nets,(m,map SeqDecl rest ++ assn))
   where
    isNet NetDecl' {} = True
    isNet _ = False

-- GHC puts default patterns in the first position, we want them in the
-- last position.
reorderDefault
  :: [(Pat, Term)]
  -> [(Pat, Term)]
reorderDefault ((DefaultPat,e):alts') = alts' ++ [(DefaultPat,e)]
reorderDefault alts'                  = alts'

reorderCustom
  :: TyConMap
  -> CustomReprs
  -> Type
  -> [(Pat, Term)]
  -> [(Pat, Term)]
reorderCustom tcm reprs (coreView1 tcm -> Just ty) alts =
  reorderCustom tcm reprs ty alts
reorderCustom _tcm reprs (coreToType' -> Right typeName) alts =
  case getDataRepr typeName reprs of
    Just (DataRepr' _name _size _constrReprs) ->
      sortOn (patPos reprs . fst) alts
    Nothing ->
      alts
reorderCustom _tcm _reprs _type alts =
  alts

patPos
  :: CustomReprs
  -> Pat
  -> Int
patPos _reprs DefaultPat = -1
patPos _reprs (LitPat _) = 0
patPos reprs pat@(DataPat dataCon _ _) =
  -- We sort data patterns by their syntactical order
  let name = nameOcc $ dcName dataCon in
  case getConstrRepr name reprs of
    Nothing ->
      -- TODO: err
      error $ $(curLoc) ++ (show pat)
    Just (ConstrRepr' _name n _mask _value _anns) ->
      n


-- | Generate a list of Declarations for a let-binder where the RHS is a function application
mkFunApp
  :: HasCallStack
  => Identifier -- ^ LHS of the let-binder
  -> Id -- ^ Name of the applied function
  -> [Term] -- ^ Function arguments
  -> [Declaration] -- ^ Tick declarations
  -> NetlistMonad [Declaration]
mkFunApp dstId fun args tickDecls = do
  topAnns <- Lens.use topEntityAnns
  tcm     <- Lens.use tcCache
  case (isGlobalId fun, lookupVarEnv fun topAnns) of
    (True, Just topEntity)
      | let ty = varType (topId topEntity)
      , let (fArgTys0,fResTy) = splitFunForallTy ty
      -- Take into account that clocks and stuff are split off from any product
      -- types containing them
      , let fArgTys1 = splitShouldSplit tcm $ rights fArgTys0
      , length fArgTys1 == length args
      -> do
        let annM = topAnnotation topEntity
        topNameI <- lookupVarEnv' <$> Lens.use componentNames <*> pure fun
        let topName = StrictText.unpack (Id.toText topNameI)
        argHWTys <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) fArgTys1
        (argExprs, concat -> argDecls) <- unzip <$>
          mapM (\(e,t) -> mkExpr False Concurrent (NetlistId dstId t) e)
                                 (zip args fArgTys1)

        -- Filter void arguments, but make sure to render their declarations:
        let
          filteredTypeExprs = filter (not . isVoid . fst) (zip argHWTys argExprs)
          (hWTysFiltered, argExprsFiltered) = unzip filteredTypeExprs

        dstHWty  <- unsafeCoreTypeToHWTypeM' $(curLoc) fResTy
        env  <- Lens.use hdlDir
        let modName = takeWhile (/= '.') (StrictText.unpack (nameOcc (varName fun)))

        manFile <- case annM of
          Just _  -> return (env </> ".." </> modName </> topName </> topName <.> "manifest")
          Nothing -> return (env </> topName <.> "manifest")
        Just man <- readMaybe <$> liftIO (readFile manFile)
        instDecls <- mkTopUnWrapper fun annM man (dstId,dstHWty)
                       (zip argExprsFiltered hWTysFiltered)
                       tickDecls
        return (argDecls ++ instDecls)

      | otherwise -> error $ $(curLoc) ++ "under-applied TopEntity: " ++ showPpr fun
    (True, Nothing) -> do
      normalized <- Lens.use bindings
      case lookupVarEnv fun normalized of
        Nothing -> error [I.i|
          Internal error: unknown normalized binder:

            #{showPpr fun}
        |]
        Just (Binding{bindingTerm}) -> do
          (_,_,_,Component compName compInps co _) <- preserveVarEnv $ genComponent fun
          let argTys = map (termType tcm) args
          argHWTys <- mapM coreTypeToHWTypeM' argTys

          (argExprs, concat -> argDecls) <- unzip <$>
            mapM (\(e,t) -> mkExpr False Concurrent (NetlistId dstId t) e)
                 (zip args argTys)

          -- Filter void arguments, but make sure to render their declarations:
          let
            argTypeExprs = zip argHWTys (zip argTys argExprs)
            filteredTypeExprs = fmap snd $ filter (not . isVoidMaybe True . fst) argTypeExprs
            (argTysFiltered, argsFiltered) = unzip filteredTypeExprs

          let compOutp = (\(_,x,_) -> x) <$> listToMaybe co
          if length argTysFiltered == length compInps
            then do
              (argExprs',argDecls') <- (second concat . unzip) <$> mapM (toSimpleVar dstId) (zip argsFiltered argTysFiltered)
              let inpAssigns    = zipWith (\(i,t) e -> (Identifier i Nothing,In,t,e)) compInps argExprs'
                  outpAssign    = case compOutp of
                    Nothing -> []
                    Just (id_,hwtype) -> [(Identifier id_ Nothing,Out,hwtype,Identifier dstId Nothing)]
              let instLabel0 = StrictText.concat [Id.toText compName, "_", Id.toText dstId]
              instLabel1 <- fromMaybe instLabel0 <$> Lens.view setName
              instLabel2 <- affixName instLabel1
              instLabel3 <- Id.makeBasic instLabel2
              let instDecl = InstDecl Entity Nothing compName instLabel3 [] (outpAssign ++ inpAssigns)
              return (argDecls ++ argDecls' ++ tickDecls ++ [instDecl])
            else error [I.i|
              Under-applied normalized function at component #{compName}:

              #{showPpr fun}

              Core:

              #{showPpr bindingTerm}

              Applied to arguments:
              #{showPpr args}

              Applied to filtered arguments:
              #{argsFiltered}

              Component inputs:
              #{compInps}
            |]
    _ ->
      case args of
        [] ->
          -- TODO: Figure out what to do with zero-width constructs
          return [Assignment dstId (Identifier (id2identifier fun) Nothing)]
        _ -> error [I.i|
          Netlist generation encountered a local function. This should not
          happen. Function:

            #{showPpr fun}

          Arguments:

            #{showPpr args}

          Posssible user issues:

            * A top entity has an higher-order argument, e.g (Int -> Int) or
            Maybe (Int -> Int)

          Possible internal compiler issues:

            * 'bindOrLiftNonRep' failed to fire

            * 'caseCon' failed to eliminate something of a type such as
            "Maybe (Int -> Int)"
          |]

toSimpleVar :: Identifier
            -> (Expr,Type)
            -> NetlistMonad (Expr,[Declaration])
toSimpleVar _ (e@(Identifier _ _),_) = return (e,[])
toSimpleVar dstId (e,ty) = do
  argNm <- Id.suffix dstId "fun_arg"
  hTy <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
  let argDecl         = NetDecl Nothing argNm hTy
      argAssn         = Assignment argNm e
  return (Identifier argNm Nothing,[argDecl,argAssn])

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr :: HasCallStack
       => Bool -- ^ Treat BlackBox expression as declaration
       -> DeclarationType
       -- ^ Should the returned declarations be concurrent or sequential?
       -> NetlistId -- ^ Id to assign the result to
       -> Term -- ^ Term to convert to an expression
       -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ (stripTicks -> Core.Literal l) = do
  iw <- Lens.use intWidth
  case l of
    IntegerLiteral i -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    IntLiteral i     -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    WordLiteral w    -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit w, [])
    Int64Literal i   -> return (HW.Literal (Just (Signed 64,64)) $ NumLit i, [])
    Word64Literal w  -> return (HW.Literal (Just (Unsigned 64,64)) $ NumLit w, [])
    CharLiteral c    -> return (HW.Literal (Just (Unsigned 21,21)) . NumLit . toInteger $ ord c, [])
    FloatLiteral r   -> let f = fromRational r :: Float
                            i = toInteger (floatToWord f)
                        in  return (HW.Literal (Just (BitVector 32,32)) (NumLit i), [])
    DoubleLiteral r  -> let d = fromRational r :: Double
                            i = toInteger (doubleToWord d)
                        in  return (HW.Literal (Just (BitVector 64,64)) (NumLit i), [])
    NaturalLiteral n -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit n, [])
    ByteArrayLiteral (ByteArray ba) -> return (HW.Literal Nothing (NumLit (Jp# (BN# ba))),[])
    _ -> error $ $(curLoc) ++ "not an integer or char literal"

mkExpr bbEasD declType bndr app =
 let (appF,args,ticks) = collectArgsTicks app
     (tmArgs,tyArgs) = partitionEithers args
 in  withTicks ticks $ \tickDecls -> do
  hwTys  <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) (netlistTypes bndr)
  (_,sp) <- Lens.use curCompNm
  let hwTyA = head hwTys
  case appF of
    Data dc -> mkDcApplication hwTys bndr dc tmArgs
    Prim pInfo -> mkPrimitive False bbEasD bndr pInfo args tickDecls
    Var f
      | null tmArgs ->
          if isVoid hwTyA then
            return (Noop, [])
          else
            return (Identifier (id2identifier f) Nothing, [])
      | not (null tyArgs) ->
          throw (ClashException sp ($(curLoc) ++ "Not in normal form: "
            ++ "Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
      | otherwise -> do
          argNm <- Id.suffix (netlistId1 id id2identifier bndr) "fun_arg"
          decls  <- mkFunApp argNm f tmArgs tickDecls
          if isVoid hwTyA then
            return (Noop, decls)
          else
            return ( Identifier argNm Nothing
                   , NetDecl' Nothing Wire argNm (Right hwTyA) Nothing:decls)
    Case scrut ty' [alt] -> mkProjection bbEasD bndr scrut ty' alt
    Case scrut tyA alts -> do
      tcm <- Lens.use tcCache
      let scrutTy = termType tcm scrut
      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
      ite <- Lens.use backEndITE
      let wr = case iteAlts scrutHTy alts of
                 Just _ | ite -> Wire
                 _ -> Reg
      argNm <- Id.suffix (netlistId1 id id2identifier bndr) "sel_arg"
      decls  <- mkSelection declType (NetlistId argNm (netlistTypes1 bndr))
                            scrut tyA alts tickDecls
      if isVoid hwTyA then
        return (Noop, decls)
      else
        return ( Identifier argNm Nothing
               , NetDecl' Nothing wr argNm (Right hwTyA) Nothing:decls)
    Letrec binders body -> do
      netDecls <- fmap catMaybes $ mapM mkNetDecl binders
      decls    <- concat <$> mapM (uncurry mkDeclarations) binders
      (bodyE,bodyDecls) <- mkExpr bbEasD declType bndr (mkApps (mkTicks body ticks) args)
      return (bodyE,netDecls ++ decls ++ bodyDecls)
    _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: application of a Lambda-expression\n\n" ++ showPpr app) Nothing)

-- | Generate an expression that projects a field out of a data-constructor.
--
-- Works for both product types, as sum-of-product types.
mkProjection
  :: Bool
  -- ^ Projection must bind to a simple variable
  -> NetlistId
  -- ^ The signal to which the projection is (potentially) assigned
  -> Term
  -- ^ The subject/scrutinee of the projection
  -> Type
  -- ^ The type of the result
  -> Alt
  -- ^ The field to be projected
  -> NetlistMonad (Expr, [Declaration])
mkProjection mkDec bndr scrut altTy alt@(pat,v) = do
  tcm <- Lens.use tcCache
  let scrutTy = termType tcm scrut
      e = Case scrut scrutTy [alt]
  (_,sp) <- Lens.use curCompNm
  varTm <- case v of
    (Var n) -> return n
    _ -> throw (ClashException sp ($(curLoc) ++
                "Not in normal form: RHS of case-projection is not a variable:\n\n"
                 ++ showPpr e) Nothing)
  sHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  vHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
  scrutRendered <- do
    scrutNm <-
      netlistId1
        Id.next
        (\b -> Id.suffix (id2identifier b) "projection")
        bndr
    (scrutExpr,newDecls) <- mkExpr False Concurrent (NetlistId scrutNm scrutTy) scrut
    case scrutExpr of
      Identifier newId modM ->
        pure (Right (newId, modM, newDecls))
      Noop ->
        -- Scrutinee was a zero-width / void construct. We need to render its
        -- declarations, but it's no use assigning it to a new variable.
        -- TODO: Figure out whether we need to render alternatives too.
        -- TODO: seems useless?
        pure (Left newDecls)
      _ -> do
        let scrutDecl = NetDecl Nothing scrutNm sHwTy
            scrutAssn = Assignment scrutNm scrutExpr
        pure (Right (scrutNm, Nothing, newDecls ++ [scrutDecl, scrutAssn]))

  case scrutRendered of
    Left newDecls -> pure (Noop, newDecls)
    Right (selId, modM, decls) -> do
      let altVarId = id2identifier varTm
      modifier <- case pat of
        DataPat dc exts tms -> do
          let
            tms' =
              if bindsExistentials exts tms then
                throw (ClashException sp ($(curLoc)
                  ++ "Not in normal form: Pattern binds existential variables:\n\n"
                  ++ showPpr e) Nothing)
              else
                tms
          argHWTys <- mapM coreTypeToHWTypeM' (map varType tms)
          let tmsBundled   = zip argHWTys tms'
              tmsFiltered  = filter (maybe False (not . isVoid) . fst) tmsBundled
              tmsFiltered' = map snd tmsFiltered
          case elemIndex varTm {varType = altTy} tmsFiltered' of
            Nothing -> pure Nothing
            Just fI
              | sHwTy /= vHwTy ->
                pure $ nestModifier modM (Just (Indexed (sHwTy,dcTag dc - 1,fI)))
              -- When element and subject have the same HW-type,
              -- then the projections is just the identity
              | otherwise ->
                pure $ nestModifier modM (Just (DC (Void Nothing,0)))
        _ -> throw (ClashException sp ($(curLoc)
               ++ "Not in normal form: Unexpected pattern in case-projection:\n\n"
               ++ showPpr e) Nothing)
      let extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
      case bndr of
        NetlistId scrutNm _ | mkDec -> do
          scrutNm' <- Id.next scrutNm
          let scrutDecl = NetDecl Nothing scrutNm' vHwTy
              scrutAssn = Assignment scrutNm' extractExpr
          return (Identifier scrutNm' Nothing,scrutDecl:scrutAssn:decls)
        MultiId {} -> error "mkProjection: MultiId"
        _ -> return (extractExpr,decls)
  where
    nestModifier Nothing  m          = m
    nestModifier m Nothing           = m
    nestModifier (Just m1) (Just m2) = Just (Nested m1 m2)

-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication
    :: HasCallStack
    => [HWType]
    -- ^ HWType of the LHS of the let-binder, can multiple types when we're
    -- creating a "split" product type (e.g. a tuple of a Clock and Reset)
    -> NetlistId
    -- ^ Id to assign the result to
    -> DataCon
    -- ^ Applied DataCon
    -> [Term]
    -- ^ DataCon Arguments
    -> NetlistMonad (Expr,[Declaration])
    -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication [dstHType] bndr dc args = do
  let dcNm = nameOcc (dcName dc)
  tcm <- Lens.use tcCache
  let argTys = map (termType tcm) args
  argNm <- netlistId1 return (\b -> Id.suffix (id2identifier b) "_dc_arg") bndr
  argHWTys <- mapM coreTypeToHWTypeM' argTys

  (argExprs, concat -> argDecls) <- unzip <$>
    mapM (\(e,t) -> mkExpr False Concurrent (NetlistId argNm t) e) (zip args argTys)

  -- Filter void arguments, but make sure to render their declarations:
  let
    filteredTypeExprDecls =
      filter
        (not . isVoidMaybe True . fst)
        (zip argHWTys argExprs)

    (hWTysFiltered, argExprsFiltered) = unzip filteredTypeExprDecls

  fmap (,argDecls) $! case (hWTysFiltered,argExprsFiltered) of
    -- Is the DC just a newtype wrapper?
    ([Just argHwTy],[argExpr]) | argHwTy == dstHType ->
      return (HW.DataCon dstHType (DC (Void Nothing,-1)) [argExpr])
    _ -> case dstHType of
      SP _ dcArgPairs -> do
        let dcI      = dcTag dc - 1
            dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,dcI)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Product _ _ dcArgs ->
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      CustomProduct _ _ _ _ dcArgs ->
        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Sum _ _ ->
        return (HW.DataCon dstHType (DC (dstHType,dcTag dc - 1)) [])
      CustomSP _ _ _ dcArgsTups -> do
        -- Safely get item from list, or err with note
        let dcI    = dcTag dc - 1
        let note   = $(curLoc) ++ "No DC with tag: " ++ show dcI
        let argTup = indexNote note dcArgsTups dcI
        let (_, _, dcArgs) = argTup

        case compare (length dcArgs) (length argExprsFiltered) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType, dcI)) argExprsFiltered)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"

      CustomSum _ _ _ _ ->
        return (HW.DataCon dstHType (DC (dstHType, dcTag dc - 1)) [])
      Bool ->
        let dc' = case dcTag dc of
                   1  -> HW.Literal Nothing (BoolLit False)
                   2  -> HW.Literal Nothing (BoolLit True)
                   tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showPpr dc ++ "(tag: " ++ show tg ++ ")"
        in  return dc'
      Vector 0 _ -> return (HW.DataCon dstHType VecAppend [])
      Vector 1 _ -> case argExprsFiltered of
                      [e] -> return (HW.DataCon dstHType VecAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      Vector _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType VecAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      RTree 0 _ -> case argExprsFiltered of
                      [e] -> return (HW.DataCon dstHType RTreeAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showPpr args
      RTree _ _ -> case argExprsFiltered of
                      [e1,e2] -> return (HW.DataCon dstHType RTreeAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showPpr args
      String ->
        let dc' = case dcTag dc of
                    1 -> HW.Literal Nothing (StringLit "")
                    _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,args,argHWTys)
        in  return dc'
      Void {} -> return Noop
      Signed _
        | dcNm == "GHC.Integer.Type.S#"
        -> pure (head argExprsFiltered)
        -- ByteArray# are non-translatable / void, except when they're literals
        | dcNm == "GHC.Integer.Type.Jp#"
        , HW.Literal Nothing (NumLit _) <- head argExprs
        -> pure (head argExprs)
        | dcNm == "GHC.Integer.Type.Jn#"
        -- ByteArray# are non-translatable / void, except when they're literals
        , HW.Literal Nothing (NumLit i) <- head argExprs
        -> pure (HW.Literal Nothing (NumLit (negate i)))
      Unsigned _
        | dcNm == "GHC.Natural.NatS#"
        -> pure (head argExprsFiltered)
        | dcNm == "GHC.Natural.NatJ#"
        -- ByteArray# are non-translatable / void, except when they're literals
        , HW.Literal Nothing (NumLit _) <- head argExprs
        -> pure (head argExprs)
      _ ->
        error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)

-- Handle MultiId assignment
mkDcApplication dstHTypes (MultiId argNms) _ args = do
  tcm                 <- Lens.use tcCache
  let argTys          = map (termType tcm) args
  argHWTys            <- mapM coreTypeToHWTypeM' argTys
  -- Filter out the arguments of hwtype `Void` and only translate
  -- them to the intermediate HDL afterwards
  let argsBundled   = zip argHWTys (zipEqual (map CoreId argNms) args)
      (_hWTysFiltered,argsFiltered) = unzip
        (filter (maybe True (not . isVoid) . fst) argsBundled)
  (argExprs,argDecls) <- fmap (second concat . unzip) $!
                         mapM (uncurry (mkExpr False Concurrent)) argsFiltered
  if length dstHTypes == length argExprs then do
    let assns = mapMaybe
                  (\case (_,Noop) -> Nothing
                         (dstId,e) -> let nm = netlistId1 id id2identifier dstId
                                      in  case e of
                                            Identifier nm0 Nothing
                                              | nm == nm0 -> Nothing
                                            _ -> Just (Assignment nm e))
                  (zip (map CoreId argNms) argExprs)
    return (Noop,argDecls ++ assns)
  else
    error "internal error"

mkDcApplication _ _ _ _ = error "internal error"
