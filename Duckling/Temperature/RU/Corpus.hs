-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.RU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "37 ° цельсия"
             , "37° Цельсия"
             , "37° по цельсию"
             , "37 градусов Цельсия"
             , "37 градусов по цельсию"
             , "тридцать семь градусов цельсия"
             , "тридцать семь градусов по Цельсию"
             , "тридцать семь цельсия"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "70 ° по Фаренгейту"
             , "70° по фаренгейту"
             , "70 градусов по фаренгейту"
             , "семьдесят градусов по Фаренгейту"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45 градусов"
             , "45 град."
             ]
  , examples (simple Degree (-2))
             [ "-2°"
             , "- 2 градуса"
             , "минус два градуса"
             , "2 градуса ниже нуля"
             , "2 ниже нуля"
             ]
  , examples (between Degree (30, 40))
             [ "между 30 и 40 градусами"
             , "от 30 до 40 градусов"
             ]
  , examples (between Celsius (30, 40))
             [ "между 30 и 40 по цельсию"
             , "от 30 цельсия до 40 цельсия"
             , "между 30 и 40 градусами цельсия"
             , "от 30 градусов по цельсию до 40 градусов по цельсию"
             , "30-40 градусов цельсия"
             ]
    , examples (above Degree 40)
             [ "выше 40 градусов"
             , "не меньше 40 градусов"
             , "больше 40 градусов"
             ]
    , examples (under Degree 40)
             [ "ниже 40 градусов"
             , "меньше 40 градусов"
             ]
  ]
