{-# LANGUAGE OverloadedStrings #-}

module NeData where

import qualified Data.List as List (find)
import Data.Text (Text)
import Debug.Trace

import Clash.Prelude

import Clash.Core.DataCon (dcName)
import Clash.Core.Evaluator.Models (Neutral(..), Nf(..), partialEval)
import Clash.Core.Name (Name(..))
import Clash.Core.TyCon (TyConMap)
import Clash.Core.TysPrim (intPrimTy)
import Clash.Core.Var (Var(..))
import Clash.Core.VarEnv (eltsVarEnv, emptyInScopeSet)
import Clash.Driver.Types (BindingMap, Binding(..))

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

-- Top entity for NeData on a non-record type.
--
topEntityNonRecord :: Int -> Maybe Int
topEntityNonRecord = Just
{-# NOINLINE topEntityNonRecord #-}
{-# ANN topEntityNonRecord (Synthesize
        { t_name   = "topEntityNonRecord"
        , t_inputs = [PortName "just"]
        , t_output = PortName "res" })
    #-}

data Misschien a = Niets | Iets { iets :: a }

topEntityRecord :: Int -> Misschien Int
topEntityRecord = Iets
{-# NOINLINE topEntityRecord #-}
{-# ANN topEntityRecord (Synthesize
        { t_name   = "topEntityRecord"
        , t_inputs = [PortName "iets"]
        , t_output = PortName "res" })
    #-}

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/NeData.hs"

findBinding :: Text -> BindingMap -> TyConMap -> Nf
findBinding name bm tcm =
  case List.find isTopEntity (eltsVarEnv bm) of
    Just te ->
      fst $ partialEval ghcEvaluator bm (mempty, 0)
        tcm emptyInScopeSet undefined (bindingTerm te)

    Nothing -> error ("No entity in module: " <> show name)
 where
  isTopEntity b =
    name == nameOcc (varName $ bindingId b)

assertNeData :: Nf -> IO ()
assertNeData nf
  | NLam i x <- nf
  = case x of
      NData dc [Right ty, Left v]
        |  dcn <- nameOcc (dcName dc)
        ,  NNeu (NeVar j) <- v
        ,  dcn == "GHC.Maybe.Just" || dcn == "NeData.Iets"
        ,  ty == intPrimTy
        ,  i == j
        -> pure ()

        |  otherwise -> error "assertNeData: Unexpected arguments"

      _ -> error ("assertNeData: Expected NData, got " <> show x)

  | otherwise
  = error ("assertNeData: Normal form is not lambda")

mainVHDL :: IO ()
mainVHDL = do
  (bm, tcm) <- runToCoreStage SVHDL id testPath

  let teNonRecord = findBinding "NeData.topEntityNonRecord" bm tcm
      teRecord    = findBinding "NeData.topEntityRecord" bm tcm

  assertNeData teNonRecord
  assertNeData teRecord

mainVerilog :: IO ()
mainVerilog = do
  (bm, tcm) <- runToCoreStage SVerilog id testPath

  let teNonRecord = findBinding "NeData.topEntityNonRecord" bm tcm
      teRecord    = findBinding "NeData.topEntityRecord" bm tcm

  assertNeData teNonRecord
  assertNeData teRecord

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  (bm, tcm) <- runToCoreStage SSystemVerilog id testPath

  let teNonRecord = findBinding "NeData.topEntityNonRecord" bm tcm
      teRecord    = findBinding "NeData.topEntityRecord" bm tcm

  assertNeData teNonRecord
  assertNeData teRecord

