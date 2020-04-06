{-# LANGUAGE OverloadedStrings #-}

module NePrim where

import qualified Data.List as List (find)
import Data.Text (Text)

import Clash.Prelude

import Clash.Core.Evaluator.Models (Neutral(..), Nf(..), partialEval)
import Clash.Core.Name (Name(..))
import Clash.Core.Term (PrimInfo(..))
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

findBinding :: Text -> BindingMap -> TyConMap -> Nf
findBinding name bm tcm =
  case List.find byName (eltsVarEnv bm) of
    Just bd ->
      fst $ partialEval ghcEvaluator bm (mempty, 0)
        tcm emptyInScopeSet undefined (bindingTerm bd)

    Nothing -> error ("No entity in module: " <> show name)
 where
  byName b =
    name == nameOcc (varName $ bindingId b)

assertNePrim :: Nf -> IO ()
assertNePrim nf
  | NLam i x <- nf
  = case x of
      NPrim p [Right _runtimeRep, Right ty, Left v]
        |  NNeu (NeVar j) <- v
        ,  primName p == "GHC.Err.error"
        ,  ty == intPrimTy
        ,  i == j
        -> pure ()

        |  otherwise
        -> error "assertNePrim: Unexpected arguments"

      _ -> error ("assertNePrim: Expected NPrim, got " <> show x)

  | otherwise
  = error ("assertNePrim: Normal form is not lambda")

mainVHDL :: IO ()
mainVHDL = do
  (bm, tcm) <- runToCoreStage SVHDL id testPath

  let te = findBinding "NePrim.topEntity" bm tcm
  assertNePrim te

mainVerilog :: IO ()
mainVerilog = do
  (bm, tcm) <- runToCoreStage SVerilog id testPath

  let te = findBinding "NePrim.topEntity" bm tcm
  assertNePrim te

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  (bm, tcm) <- runToCoreStage SSystemVerilog id testPath

  let te = findBinding "NePrim.topEntity" bm tcm
  assertNePrim te

