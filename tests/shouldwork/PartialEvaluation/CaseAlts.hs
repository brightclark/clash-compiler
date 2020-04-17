{-# LANGUAGE OverloadedStrings #-}

module CaseAlts where

import qualified Data.List as List (find)
import Data.Text (Text)

import Clash.Prelude

import Clash.Core.Evaluator.Models
import Clash.Core.Literal (Literal(..))
import Clash.Core.Name (Name(..))
import Clash.Core.Term
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Var (Var(..))
import Clash.Core.VarEnv (eltsVarEnv, emptyInScopeSet)
import Clash.Driver.Types (BindingMap, Binding(..))

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

topEntityKnown :: Int
topEntityKnown = caseExpr 0
{-# NOINLINE topEntityKnown #-}
{-# ANN topEntityKnown (Synthesize
        { t_name   = "topEntityKnown"
        , t_inputs = []
        , t_output = PortName "res" })
    #-}

topEntityUnknown :: Int -> Int
topEntityUnknown = caseExpr
{-# NOINLINE topEntityUnknown #-}
{-# ANN topEntityUnknown (Synthesize
        { t_name   = "topEntityUnknown"
        , t_inputs = [PortName "x"]
        , t_output = PortName "res" })
    #-}

caseExpr :: Int -> Int
caseExpr x = case x of
  0 -> 1
  1 -> 0
  n -> n
{-# INLINE caseExpr #-}

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/CaseAlts.hs"

findBinding :: Text -> BindingMap -> TyConMap -> Nf
findBinding name bm tcm =
  case List.find byName (eltsVarEnv bm) of
    Just bd ->
      fst3 $ nf ghcEvaluator bm (mempty, 0)
        tcm emptyInScopeSet undefined (bindingTerm bd)

    Nothing -> error ("No entity in module: " <> show name)
 where
  fst3 (x, _, _) = x

  byName b = name == nameOcc (varName $ bindingId b)

assertKnownAlt :: Nf -> IO ()
assertKnownAlt nf
  | NPrim p [Left (NLit (IntLiteral 1))] <- nf
  , primName p == "GHC.Types.I#"
  = pure ()

  | otherwise
  = error ("assertKnownAlt: Expected the literal 4, got " <> show nf)

assertUnknownAlt :: Nf -> IO ()
assertUnknownAlt nf
  | NLam i (NNeu (NeCase subj ty alts)) <- nf
  , NNeu (NeVar j) <- subj
  , i == j
  = pure ()

  | otherwise
  = error ("assertUnknownAlt: Expected normalized caseExpr, got " <> show nf)

mainVHDL :: IO ()
mainVHDL = do
  (bm, tcm) <- runToCoreStage SVHDL id testPath

  let teKnown   = findBinding "CaseAlts.topEntityKnown" bm tcm
      teUnknown = findBinding "CaseAlts.topEntityUnknown" bm tcm

  assertKnownAlt teKnown
  assertUnknownAlt teUnknown

mainVerilog :: IO ()
mainVerilog = do
  (bm, tcm) <- runToCoreStage SVerilog id testPath

  let teKnown   = findBinding "CaseAlts.topEntityKnown" bm tcm
      teUnknown = findBinding "CaseAlts.topEntityUnknown" bm tcm

  assertKnownAlt teKnown
  assertUnknownAlt teUnknown

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  (bm, tcm) <- runToCoreStage SSystemVerilog id testPath

  let teKnown   = findBinding "CaseAlts.topEntityKnown" bm tcm
      teUnknown = findBinding "CaseAlts.topEntityUnknown" bm tcm

  assertKnownAlt teKnown
  assertUnknownAlt teUnknown

