-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.RU.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData(..), unitsAreCompatible)
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleTemperatureDegrees :: Rule
ruleTemperatureDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "(град(ус(ами|а|ов)?)?\\.?)|°"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTemperatureCelsius :: Rule
ruleTemperatureCelsius = Rule
  { name = "<temp> Celsius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "C|ц(ельсия)?|по цельсию"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTemperatureFahrenheit :: Rule
ruleTemperatureFahrenheit = Rule
  { name = "<temp> Fahrenheit"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "F|по фаренгейту"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

ruleTemperatureBelowZero :: Rule
ruleTemperatureBelowZero = Rule
  { name = "<temp> below zero"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "ниже нуля"
    ]
  , prod = \case
      (Token Temperature td@TemperatureData {TTemperature.value = Just v}:
       _) -> case TTemperature.unit td of
        Nothing -> Just . Token Temperature . withUnit TTemperature.Degree $
          td {TTemperature.value = Just (- v)}
        _ -> Just . Token Temperature $ td {TTemperature.value = Just (- v)}
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <temp> and|to <temp>"
  , pattern =
    [ regex "между|от"
    , Predicate isSimpleTemperature
    , regex "до|и"
    , Predicate isSimpleTemperature
    ]
  , prod = \case
      (_:
       Token Temperature TemperatureData
        {TTemperature.unit = u1 , TTemperature.value = Just from}:
       _:
       Token Temperature TemperatureData
        {TTemperature.unit = Just u2, TTemperature.value = Just to}:
       _) | from < to && unitsAreCompatible u1 u2 ->
        Just . Token Temperature . withInterval (from, to) $ unitOnly u2
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<temp> - <temp>"
  , pattern =
    [ Predicate isSimpleTemperature
    , regex "-"
    , Predicate isSimpleTemperature
    ]
  , prod = \case
      (Token Temperature TemperatureData
        {TTemperature.unit = u1, TTemperature.value = Just from}:
       _:
       Token Temperature TemperatureData
        {TTemperature.unit = Just u2, TTemperature.value = Just to}:
       _) | from < to && unitsAreCompatible u1 u2 ->
        Just . Token Temperature . withInterval (from, to) $ unitOnly u2
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <temp>"
  , pattern =
    [ regex "ниже|меньше|не больше"
    , Predicate isSimpleTemperature
    ]
  , prod = \case
      (_:
       Token Temperature TemperatureData{TTemperature.value = Just to,
                                         TTemperature.unit = Just u}:
       _) -> Just . Token Temperature . withMax to $ unitOnly u
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <temp>"
  , pattern =
    [ regex "выше|больше|не меньше"
    , Predicate isSimpleTemperature
    ]
  , prod = \case
      (_:
       Token Temperature TemperatureData{TTemperature.value = Just from,
                                         TTemperature.unit = Just u}:
       _) -> Just . Token Temperature . withMin from $ unitOnly u
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleTemperatureDegrees
  , ruleTemperatureCelsius
  , ruleTemperatureFahrenheit
  , ruleTemperatureBelowZero
  , ruleIntervalBetween
  , ruleIntervalDash
  , ruleIntervalMin
  , ruleIntervalMax
  ]
