{-# LANGUAGE OverloadedStrings #-}

module NePrim where

import qualified Data.List as List (find)
import Data.Text (Text)

import Clash.Prelude

import Clash.Backend
import Clash.Core.Evaluator.Models
import Clash.Core.Name (Name(..))
import Clash.Core.Term
import Clash.Core.Type
import Clash.Core.TyCon (TyConMap)
import Clash.Core.TysPrim (intPrimTy)
import Clash.Core.Var (Var(..))
import Clash.Core.VarEnv (eltsVarEnv, emptyInScopeSet)
import Clash.Driver.Types (BindingMap, Binding(..))

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

topEntity :: String -> Int
topEntity = error

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/NePrim.hs"

findBinding :: BindingMap -> TyConMap -> Nf
findBinding bm tcm =
  case List.find byName (eltsVarEnv bm) of
    Just bd ->
      fst3 $ nf ghcEvaluator bm (mempty, 0)
        tcm emptyInScopeSet undefined (bindingTerm bd)

    Nothing -> error ("No topEntity in module")
 where
  fst3 (x, _, _) = x

  byName b =
    "NePrim.topEntity" == nameOcc (varName $ bindingId b)

assertNf :: Nf -> IO ()
assertNf nf
  | NNeu n <- nf
  = case n of
      NePrim p [Right _runtimeRep, Right ty, Left _callStack]
        |  ConstTy (TyCon tcN) <- ty
        ,  "GHC.Err.error" == primName p
        ,  "GHC.Types.Int" == nameOcc tcN
        -> pure ()

      _ -> error ("assertNf: Expected NePrim, got " <> show n)

  | otherwise
  = error ("assertNePrim: Expected NNeu, got " <> show nf)

assertTerm :: Term -> IO ()
assertTerm term
  | Lam i x <- term
  = case x of
      App (TyApp (TyApp (Prim p) _rr) ty) (Var j)
        |  "GHC.Err.error" == primName p
        ,  intPrimTy == ty
        ,  i == j
        -> pure ()

      _ -> error ("assertTerm: Expected App, got " <> show x)

  | otherwise
  = error ("assertTerm: Expected Lam, got " <> show term)

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  (bm, tcm) <- runToCoreStage hdl id testPath
  let entity = findBinding bm tcm

  assertNf entity
  assertTerm (asTerm entity)

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog

