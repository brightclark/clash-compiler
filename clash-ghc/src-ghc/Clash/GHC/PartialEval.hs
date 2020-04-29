{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Clash.GHC.PartialEval
  ( ghcEvaluator
  ) where

import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import GHC.Integer.GMP.Internals (Integer(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Evaluator.Semantics
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.Type

ghcEvaluator :: Evaluator
ghcEvaluator = Evaluator ghcEvaluate ghcQuote

ghcEvaluate :: Term -> State Env Value
ghcEvaluate = \case
  Var v -> evaluateVarWith ghcEvaluate v
  Literal l -> evaluateLiteral l
  Data dc -> evaluateData dc
  Prim p -> evaluatePrimWith ghcEvaluatePrim p
  Lam x e -> evaluateLam x e
  TyLam x e -> evaluateTyLam x e
  App x y -> evaluateAppWith ghcEvaluate ghcApply x y
  TyApp x ty -> evaluateTyAppWith ghcEvaluate ghcTyApply x ty
  Letrec bs x -> evaluateLetrecWith ghcEvaluate bs x
  Case x ty xs -> evaluateCaseWith ghcEvaluate ghcIsLiteral ghcIsData x ty xs
  Cast x a b -> evaluateCastWith ghcEvaluate x a b
  Tick ti x -> evaluateTickWith ghcEvaluate ti x

-- TODO Implement evaluation for primitives and call here.
--
ghcEvaluatePrim :: PrimInfo -> [Either Value Type] -> State Env Value
ghcEvaluatePrim p args = pure (VPrim p args)

-- TODO
ghcIsLiteral :: Literal -> Pat -> Bool
ghcIsLiteral l = \case
  DataPat c [] [_]
    |  IntegerLiteral i <- l
    -> case i of
         S# _  -> dcTag c == 1
         Jp# _ -> dcTag c == 2
         Jn# _ -> dcTag c == 3

    |  NaturalLiteral i <- l
    -> case i of
         S# _  -> dcTag c == 1 && i >= 0
         Jp# _ -> dcTag c == 2
         Jn# _ -> False

    |  otherwise
    -> False

  LitPat m -> l == m
  DefaultPat -> True
  _ -> False

ghcIsData :: DataCon -> Pat -> Bool
ghcIsData dc = \case
  DataPat c _ _ -> dc == c
  LitPat _ -> False
  DefaultPat -> True

ghcApply :: Value -> Value -> State Env Value
ghcApply x y = do
  res <- case x' of
    VNeu (NeData dc args) -> applyToData dc (args <> [Left y])
    VNeu (NePrim p args) -> applyToPrim p (args <> [Left y])
    VNeu n -> pure (VNeu (NeApp n y))
    VPrim p args -> pure (VNeu (NeApp (NePrim p args) y))
    VLam i x'' env ->
      let addBinder = insertLocal i (Right y)
       in State.put env >> State.withState addBinder (ghcEvaluate x'')

    _ -> error ("ghcApply: Cannot apply value to " <> show x')

  pure (addTicks res ts)
 where
  (x', ts) = collectValueTicks x

ghcTyApply :: Value -> Type -> State Env Value
ghcTyApply x ty = do
  res <- case x' of
    VNeu (NeData dc args) -> applyToData dc (args <> [Right ty])
    VNeu (NePrim p args) -> applyToPrim p (args <> [Right ty])
    VNeu n -> pure (VNeu (NeTyApp n ty))
    VPrim p args -> pure (VNeu (NeTyApp (NePrim p args) ty))
    VTyLam i x'' env ->
      let addBinder = insertType i ty
       in State.put env >> State.withState addBinder (ghcEvaluate x'')

    _ -> error ("ghcTyApply: Cannot apply type to " <> show x')

  pure (addTicks res ts)
 where
  (x', ts) = collectValueTicks x

applyToData :: DataCon -> [Either Value Type] -> State Env Value
applyToData dc args
  | length args == length tys = pure (VData dc args)
  | otherwise = pure (VNeu (NeData dc args))
 where
  tys = fst $ splitFunForallTy (dcType dc)

applyToPrim :: PrimInfo -> [Either Value Type] -> State Env Value
applyToPrim p args
  | length args == length tys = ghcEvaluatePrim p args
  | otherwise = pure (VNeu (NePrim p args))
 where
  tys = fst $ splitFunForallTy (primType p)

ghcQuote :: Value -> State Env Nf
ghcQuote = \case
  VNeu n -> ghcQuoteNeutral n
  VLit l -> quoteLiteral l
  VData dc args -> quoteDataWith ghcQuote dc args
  VPrim p args -> quotePrimWith ghcQuote p args
  VLam i x env -> quoteLamWith ghcQuote ghcApply i x env
  VTyLam i x env -> quoteTyLamWith ghcQuote ghcTyApply i x env
  VCast x a b -> quoteCastWith ghcQuote x a b
  VTick x t -> quoteTickWith ghcQuote x t

ghcQuoteNeutral :: Neutral Value -> State Env Nf
ghcQuoteNeutral = \case
  NeVar v -> fmap NNeu (quoteNeVar v)
  NeData dc args -> quoteNeDataWith ghcQuote dc args
  NePrim p args -> quoteNePrimWith ghcQuote ghcEvaluatePrim p args
  NeApp x y -> fmap NNeu (quoteNeAppWith ghcQuote ghcQuoteNeutral x y)
  NeTyApp x ty -> fmap NNeu (quoteNeTyAppWith ghcQuoteNeutral x ty)
  NeCase x ty xs -> fmap NNeu (quoteNeCaseWith ghcQuote x ty xs)

