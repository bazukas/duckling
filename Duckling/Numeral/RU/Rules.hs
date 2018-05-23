-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.RU.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "двадцать", 20)
  , ( "двадцати", 30)
  , ( "тридцать", 30)
  , ( "тридцати", 30)
  , ( "сорок", 40)
  , ( "сорока", 40)
  , ( "пятьдесят", 50)
  , ( "пятидесяти", 50)
  , ( "шестьдесят", 60)
  , ( "шестидесяти", 60)
  , ( "семьдесят", 70)
  , ( "семидесяти", 70)
  , ( "восемьдесят", 80)
  , ( "восьмидесяти", 80)
  , ( "девяносто", 90)
  , ( "девяноста", 90)
  ]

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(двадцать|двадцати|тридцать|тридцати|сорока?|пятьдесят|пятидесяти|шестьдесят|шестидесяти|семьдесят|семидесяти|восемьдесят|восьмидесяти|девяност(а|о))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 2"
  , pattern =
    [ regex "(два|две|двое|двух|пара|пару|парочку|парочка)"
    ]
  , prod = \_ -> integer 2
  }

ruleDecimalOneAndAHalf :: Rule
ruleDecimalOneAndAHalf = Rule
  { name = "decimal one and a half"
   , pattern =
    [ regex "(полтора|полторы|полутора)"
    ]
   , prod = \_ -> double 1.5
  }

ruleIntegerAndAHalf :: Rule
ruleIntegerAndAHalf = Rule
  { name = "<integer> and a half"
  , pattern =
    [ Predicate isNatural
    , regex "с половиной"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 0.5
      _ -> Nothing
  }

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "сто", 100)
  , ( "ста", 200)
  , ( "двести", 200)
  , ( "двухсот", 200)
  , ( "триста", 300)
  , ( "трехсот", 300)
  , ( "трёхсот", 300)
  , ( "четыреста", 400)
  , ( "четырехсот", 400)
  , ( "четырёхсот", 400)
  , ( "пятьсот", 500)
  , ( "пятиста", 500)
  , ( "шестьсот", 600)
  , ( "шесьтиста", 600)
  , ( "семьсот", 700)
  , ( "семиста", 700)
  , ( "восемьсот", 800)
  , ( "восьмиста", 800)
  , ( "девятьсот", 900)
  , ( "девятиста", 900)
  ]

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(ст(а|о)|двести|двухсот|триста|тр(е|ё)хсот|четыреста|четыр(е|ё)хсот|пятьсот|пятисот|шестьсот|шесьтиста|семьсот|семиста|восемьсот|воьсмиста|девятьсот|девятиста)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>= integer
      _ -> Nothing
  }

ruleNumeralsPrefixWithMinus :: Rule
ruleNumeralsPrefixWithMinus = Rule
  { name = "numbers prefix with -, minus"
  , pattern =
    [ regex "-|минус"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "((к|м|г)|(К|М|Г))(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "к" -> double $ v * 1e3
         "К" -> double $ v * 1e3
         "м" -> double $ v * 1e6
         "М" -> double $ v * 1e6
         "г" -> double $ v * 1e9
         "Г" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [300, 600, 500, 100, 800, 200, 900, 700, 400]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "(ноль|нуля|нисколько)"
    ]
  , prod = \_ -> integer 0
  }

threeToNineteenMap:: HashMap Text Integer
threeToNineteenMap = HashMap.fromList
  [ ( "три", 3)
  , ( "трех", 3)
  , ( "трёх", 3)
  , ( "четыре", 4)
  , ( "четырех", 4)
  , ( "четырёх", 4)
  , ( "пять", 5)
  , ( "пяти", 5)
  , ( "шесть", 6)
  , ( "шести", 6)
  , ( "семь", 7)
  , ( "семи", 7)
  , ( "восемь", 8)
  , ( "восьми", 8)
  , ( "девять", 9)
  , ( "девяти", 9)
  , ( "десять", 10)
  , ( "десяти", 10)
  , ( "одиннадцать", 11)
  , ( "одиннадцати", 11)
  , ( "двенадцать", 12)
  , ( "двенадцати", 12)
  , ( "тринадцать", 13)
  , ( "тренадцати", 13)
  , ( "четырнадцать", 14)
  , ( "четырнадцати", 14)
  , ( "пятнадцать", 15)
  , ( "пятнадцати", 15)
  , ( "шестнадцать", 16)
  , ( "шестнадцати", 16)
  , ( "семнадцать", 17)
  , ( "семнадцати", 17)
  , ( "восемнадцать", 18)
  , ( "восемнадцати", 18)
  , ( "девятнадцать", 19)
  , ( "девятнадцати", 19)
  ]

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (3..19)"
  , pattern =
    [ regex "(три|тр(е|ё)х|четырнадцать|четырнадцати|четыр(ех|ёх|е)|пятнадцать|пятнадцати|пять|пяти|шестнадцать|шестнадцати|шесть|шести|семнадцать|семнадцати|семь|семи|восемнадцать|восемнадцати|восемь|восьми|девятнадцать|девятнадцати|девять|девяти|десять|десяти|одиннадцать|одиннадцати|двенадцать|двенадцати|тринадцать|тринадцати)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) threeToNineteenMap >>= integer
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "(один|одна|одну|одного)"
    ]
  , prod = \_ -> integer 1
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "точка"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger6
  , ruleInteger7
  , ruleInteger8
  , ruleIntegerAndAHalf
  , ruleDecimalOneAndAHalf
  , ruleIntegerWithThousandsSeparator
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithMinus
  , ruleNumeralsSuffixesKMG
  ]
