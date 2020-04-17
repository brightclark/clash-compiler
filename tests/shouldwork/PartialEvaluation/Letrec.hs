{-# LANGUAGE OverloadedStrings #-}

module Letrec where

import qualified Data.List as List (find)
import Data.Text (Text)

import Clash.Prelude

import Clash.Core.Evaluator.Models (Neutral(..), Nf(..), partialEval)
import Clash.Core.Name (Name(..))
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Var (Var(..))
import Clash.Core.VarEnv (eltsVarEnv, emptyInScopeSet)
import Clash.Driver.Types (BindingMap, Binding(..))

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

-- TODO I want to test some things on letrec, namely around:
--
--   * non-recursive bindings
--   * recursive bindings
--
-- case-of-let should be in a different module, it's more specific


