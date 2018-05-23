-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RU.Rules
  ( rules
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (isNatural, parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate $ or . sequence [isNotLatent, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectOf :: Rule
ruleIntersectOf = Rule
  { name = "intersect by \",\", \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "в|,|на"
    , Predicate $ or . sequence [isNotLatent, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleAbsorbOnTime :: Rule
ruleAbsorbOnTime = Rule
  { name = "в <date>"
  , pattern =
    [ regex "в|во|на"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbCommaTOD :: Rule
ruleAbsorbCommaTOD = Rule
  { name = "absorption of , after named day"
  , pattern =
    [ Predicate isADayOfWeek
    , regex ","
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("right now"    , TG.Second, 0  , "(прямо )?сейчас|незамедлительно|сиюсекундно")
  , ("today"        , TG.Day   , 0  , "сегодня(шний)?|(в этот день)")
  , ("tomorrow"     , TG.Day   , 1  , "завтра(шний)?"        )
  , ("after tomorrow" , TG.Day , 2 , "послезавтра"           )
  , ("yesterday"    , TG.Day   , -1, "вчера(шний)?"          )
  , ("before yesterday" , TG.Day, -2, "позавчера"            )
  ]

ruleNow :: Rule
ruleNow = Rule
  { name = "сейчас"
  , pattern =
    [ regex "сейчас"
    ]
  , prod = \_ -> tt now
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "этот|следующий <day-of-week>"
  , pattern =
    [ regex "эт(а|от?)|следующ(ий|ая|ее)"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "этот <time>"
  , pattern =
    [ regex "эт(о(т|й|м)?|а|им|их)|текущ(яя|ий|ей|ем|ее|им|их)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "следующий <time>"
  , pattern =
    [ regex "следующ(яя|ий|ее)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "прошлый <time>"
  , pattern =
    [ regex "прошл(ая|ый|ое)|прошедш(ая|ий|ее)|предыдущ(ий|ая|ее)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

ruleLastDOWOfTime :: Rule
ruleLastDOWOfTime = Rule
  { name = "последний <day-of-week> <time>"
  , pattern =
    [ regex "последн(ий|яя|ее)"
    , Predicate isADayOfWeek
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "последний <cycle> of <time>"
  , pattern =
    [ regex "последн(ий|яя|ее)"
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleLastNight :: Rule
ruleLastNight = Rule
  { name = "прошлой ночью"
  , pattern =
    [ regex "прошлой (поздней )?ночью"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let hours = if Text.toLower match == "поздней " then 3 else 6
            start = durationBefore (DurationData hours TG.Hour) end
            end = cycleNth TG.Day 0
        in Token Time . partOfDay . notLatent <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> после <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "после"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
      [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 10000]
      , regex "год(а|у|ов|ом)?|г\\.?"
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

ruleYearOrdinal :: Rule
ruleYearOrdinal = Rule
  { name = "year"
  , pattern =
      [ dimension Ordinal
      , regex "год(а|у|ов|ом)"
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
      [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 10000]
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYearADBC :: Rule
ruleYearADBC = Rule
  { name = "<year> (bc|ad)"
  , pattern =
    [ Predicate $ isIntegerBetween (-10000) 10000
    , regex "(от|до) р\\.?х\\.?"
    ]
  , prod = \case
    (token:Token RegexMatch (GroupMatch (ab:_)):_) -> do
      y <- getIntValue token
      tt . yearADBC $ if Text.head (Text.toLower ab) == 'д' then -y else y
    _ -> Nothing
  }

ruleDOMLatent :: Rule
ruleDOMLatent = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern = [Predicate isDOMOrdinal]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleDOMOrdinal :: Rule
ruleDOMOrdinal = Rule
  { name = "<day-of-month> числа(ordinal)"
  , pattern =
    [ Predicate isDOMValue
    , regex "числ(о|а)"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ dayOfMonth n
      _ -> Nothing
  }

ruleDOMOfTimeMonth :: Rule
ruleDOMOfTimeMonth = Rule
  { name = "<day-of-month> (ordinal or number) of <month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate $ isGrainOfTime TG.Month
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMDayOfTimeMonth :: Rule
ruleDOMDayOfTimeMonth = Rule
  { name = "<day-of-month> day (ordinal or number) of <month>"
  , pattern =
    [ Predicate isDOMValue
    , regex "числа"
    , Predicate $ isGrainOfTime TG.Month
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMMonth :: Rule
ruleDOMMonth = Rule
  { name = "<day-of-month> (ordinal or number) <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMOrdinalMonthYear :: Rule
ruleDOMOrdinalMonthYear = Rule
  { name = "<day-of-month>(ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:Token RegexMatch (GroupMatch (match:_)):_) -> do
        intVal <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year intVal)
      _ -> Nothing
  }

ruleTODLatent :: Rule
ruleTODLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ or . sequence [isIntegerBetween 0 23, isOrdinalBetween 0 23]
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour True n
      _ -> Nothing
  }

ruleAtTOD :: Rule
ruleAtTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "в"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleTODOClock :: Rule
ruleTODOClock = Rule
  { name = "<time-of-day> часов"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "часов"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3])):([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleHHMMSS :: Rule
ruleHHMMSS = Rule
  { name = "hh:mm:ss"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3])):([0-5]\\d):([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond False h m s
      _ -> Nothing
  }

ruleMilitarySpelledOut :: Rule
ruleMilitarySpelledOut = Rule
  { name = "military spelled out numbers"
  , pattern =
    [ Predicate $ isIntegerBetween 0 24
    , Predicate $ isIntegerBetween 1 59
    ]
    , prod = \tokens -> case tokens of
        (h:m:_) -> do
          hh <- getIntValue h
          mm <- getIntValue m
          tt $ hourMinute True hh mm
        _ -> Nothing
  }

ruleMilitarySpelledOut2 :: Rule
ruleMilitarySpelledOut2 = Rule
  { name = "military spelled out numbers 2"
  , pattern =
    [ Predicate $ isIntegerBetween 0 24
    , regex "час(а|ов)?"
    , Predicate $ isIntegerBetween 1 59
    , regex "минут"
    ]
    , prod = \tokens -> case tokens of
        (h:_:m:_) -> do
          hh <- getIntValue h
          mm <- getIntValue m
          tt $ hourMinute True hh mm
        _ -> Nothing
  }

ruleOneHourMinutes :: Rule
ruleOneHourMinutes = Rule
  { name = "час <minutes>"
  , pattern =
    [ regex "час"
    , Predicate $ isIntegerBetween 1 59
    ]
    , prod = \tokens -> case tokens of
        (_:m:_) -> do
          mm <- getIntValue m
          tt $ hourMinute True 1 mm
        _ -> Nothing
  }

isHourAm :: TimeData -> Text -> Bool
isHourAm (TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}) ap
  | hours > 12 = False
  | ap == "дня"  = hours >= 10
  | ap == "ночи" = hours < 6
  | ap == "после обеда" = False
  | ap == "утра" = True
  | ap == "вечера" = False
isHourAm _ ap = ap == "ночи" || ap == "утра"

ruleTODAMPM :: Rule
ruleTODAMPM = Rule
  { name = "<time-of-day> утра|дня|после обеда|вечера|ночи"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(утра|дня|после обеда|вечера|ночи)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
          let ap' = Text.toLower ap
           in tt $ timeOfDayAMPM (isHourAm td ap') td
      _ -> Nothing
  }

ruleHONumeral :: Rule
ruleHONumeral = Rule
  { name = "<hour-of-day> <integer>"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleNumeralToHOD :: Rule
ruleNumeralToHOD = Rule
  { name = "без <integer> <hour-of-day>"
  , pattern =
    [ regex "без"
    , Predicate $ isIntegerBetween 1 59
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token Time td:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHalfToHOD :: Rule
ruleHalfToHOD = Rule
  { name = "половина <hour-of-day>"
  , pattern =
    [ regex "пол(овин(а|е))?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleHalfAfterHOD :: Rule
ruleHalfAfterHOD = Rule
  { name = "<hour-of-day> с половиной"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "с половиной"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesAfter 30 td
      _ -> Nothing
  }

ruleMinutesOfHour :: Rule
ruleMinutesOfHour = Rule
  { name = "<integer> minutes of <hour>"
  , pattern =
    [ Predicate $ isIntegerBetween 0 60
    , regex "минут"
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (token:_:token2:_) -> do
          m <- getIntValue token
          h <- getIntValue token2
          tt $ hourMinute True (h-1) m
      _ -> Nothing
  }

ruleDDMMYYYY :: Rule
ruleDDMMYYYY = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[-/\\.](1[0-2]|0?[1-9])[-/\\.](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        d <- parseInt dd
        m <- parseInt mm
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleMMYYYY :: Rule
ruleMMYYYY = Rule
  { name = "mm/yyyy"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])[/\\-\\.](\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        tt $ yearMonthDay y m 1
      _ -> Nothing
  }

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleNoonMidnight :: Rule
ruleNoonMidnight = Rule
  { name = "полдень|полночь"
  , pattern =
    [ regex "(полдень|полночь|конец дня|конца дня)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> tt . hour False $
        if Text.toLower match == "полдень" then 12 else 0
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(утр(ом?|а)|обед|вечер(ом|а)?|ноч(ью?|и))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case (Text.take 4 . Text.toLower) match of
              "утро" -> (hour False 4, hour False 12)
              "утра" -> (hour False 4, hour False 12)
              "вече" -> (hour False 18, hour False 0)
              "ночь" -> (hour False 18, hour False 0)
              "ночи" -> (hour False 18, hour False 0)
              "обед" -> (hour False 12, hour False 14)
              _      -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt . partOfDay $ mkLatent td
      _ -> Nothing
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "early morning"
  , pattern =
    [ regex "ранн(им|ее|их) (часах )?утр(ом?|а)"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 9)
  }

rulePODThis :: Rule
rulePODThis = Rule
  { name = "этим <part-of-day>"
  , pattern =
    [ regex "этим"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect (cycleNth TG.Day 0) td
      _ -> Nothing
  }

-- Since part of days are latent, general time intersection is blocked
ruleTimePOD :: Rule
ruleTimePOD = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Time pod:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

rulePODofTime :: Rule
rulePODofTime = Rule
  { name = "<part-of-day> <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time pod:Token Time td:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "выходные"
  , pattern =
    [ regex "выходны(е|х|ми)"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "summer", "лет(ом?|а)"     , monthDay  6 21, monthDay  9 23 )
  , ( "fall"  , "осен(ью?|и)"    , monthDay  9 23, monthDay 12 21 )
  , ( "winter", "зим(а|ой|ы|у)"  , monthDay 12 21, monthDay  3 20 )
  , ( "spring", "весн(а|е|ой|у)" , monthDay  3 20, monthDay  6 21 )
  ]

ruleIntervalDDDDMonth :: Rule
ruleIntervalDDDDMonth = Rule
  { name = "dd-dd <month> (interval)"
  , pattern =
    [ Predicate isDOMValue
    , regex "\\-|до|по"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token1:
       _:
       token2:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalFromDDDDMonth :: Rule
ruleIntervalFromDDDDMonth = Rule
  { name = "from <day-of-month> (ordinal or number) to <day-of-month> (ordinal or number) <named-month> (interval)"
  , pattern =
    [ regex "от|с"
    , Predicate isDOMValue
    , regex "до|по"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:
       token1:
       _:
       token2:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

-- Blocked for :latent time. May need to accept certain latents only, like hours
ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|до|по"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalFrom :: Rule
ruleIntervalFrom = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "с|от"
    , dimension Time
    , regex "\\-|до|по"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between <time> and <time>"
  , pattern =
    [ regex "между"
    , dimension Time
    , regex "и"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

-- Specific for time-of-day, to help resolve ambiguities
ruleIntervalTODDash :: Rule
ruleIntervalTODDash = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|:|до|по"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTODFrom :: Rule
ruleIntervalTODFrom = Rule
  { name = "from <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ regex "(с|от|между|позже( чем)?)"
    , Predicate isATimeOfDay
    , regex "до|по|\\-|(но )?раньше( чем)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTODBetween :: Rule
ruleIntervalTODBetween = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "между"
    , Predicate isATimeOfDay
    , regex "и"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBy :: Rule
ruleIntervalBy = Rule
  { name = "by <time>"
  , pattern =
    [ regex "до"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Token Time <$> interval TTime.Open (cycleNth TG.Second 0) td
      _ -> Nothing
  }

ruleIntervalByTheEndOf :: Rule
ruleIntervalByTheEndOf = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "до (конца|окончания)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Token Time <$> interval TTime.Closed (cycleNth TG.Second 0) td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "понедельник(а|ом)?|пн\\.?" )
  , ( "Tuesday"  , "вторник(а|ом)?|вт\\.?"    )
  , ( "Wednesday", "сред(а|у|ы|ой)|ср\\.?"     )
  , ( "Thursday" , "четверг(а|ом)?|чт\\.?"     )
  , ( "Friday"   , "пятниц(а|у|ы|ой)|(пят|пт)\\.?"   )
  , ( "Saturday" , "суббот(а|у|ы|ой)|(суб|сб)\\.?"   )
  , ( "Sunday"   , "воскресень(е|я|ем)|(вскр|вс)\\.?" )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonthsWithLatent
  [ ( "January"  , "январ(ь|е|я|ем|ём)|янв\\.?"    , False )
  , ( "February" , "феврал(ь|е|я|ем|ём)|фев\\.?"   , False )
  , ( "March"    , "март(е|а|ом)?|мар\\.?"         , False )
  , ( "April"    , "апрел(ь|ем?|я)|апр\\.?"        , False )
  , ( "May"      , "ма(й|е|я|ем)"                  , False )
  , ( "June"     , "июн(ь|е|я|ем)|июн\\.?"         , False )
  , ( "July"     , "июл(ь|е|я|ем)|июл\\.?"         , False )
  , ( "August"   , "август(е|а|ом)?|авг\\.?"       , False )
  , ( "September", "сентябр(ь|е|я|ем|ём)|сен?\\.?" , False )
  , ( "October"  , "октябр(ь|е|я|ем|ём)|окт\\.?"   , False )
  , ( "November" , "ноябр(ь|е|я|ем|ём)|ноя\\.?"    , False )
  , ( "December" , "декабр(ь|е|я|ем|ём)|дек\\.?"   , False )
  ]

ruleEndOrBeginningOfMonth :: Rule
ruleEndOrBeginningOfMonth = Rule
  { name = "at the beginning|end of <named-month>"
  , pattern =
    [ regex "(в )?(начал(о|е)|кон(ец|це))"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case (Text.take 3 . Text.toLower) match of
          "нач" -> Just (1, 10)
          "кон" -> Just (21, -1)
          _     -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfMonth :: Rule
ruleEndOfMonth = Rule
  { name = "end of month"
  , pattern = [ regex "(в|до )?((кон(ец|це|ца)|окончани(е|я|и)) месяца)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_)
        | (Just start, Just end) <- parsed ->
          Token Time <$> interval TTime.Open start end
        where
          cycleMonth = cycleNth TG.Month
          parsed = if "до" `Text.isPrefixOf` Text.toLower match
            then
              ( Just $ cycleNth TG.Second 0
              , intersect (dayOfMonth 1) $ cycleMonth 1)
            else
              ( intersect (dayOfMonth 21) $ cycleMonth 0
              , Just $ cycleLastOf TG.Day $ cycleMonth 0)
      _ -> Nothing
  }

ruleBeginningOfMonth :: Rule
ruleBeginningOfMonth = Rule
  { name = "beginning of month"
  , pattern = [ regex "(в )?(начал(о|е) месяца)" ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      end <- intersect (dayOfMonth 10) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfYear :: Rule
ruleEndOrBeginningOfYear = Rule
  { name = "at the beginning|end of <year>"
  , pattern =
    [ regex "(в )?(начал(о|е)|кон(ец|це))"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case (Text.take 3 . Text.toLower) match of
          "нач" -> Just (1, 4)
          "кон" -> Just (9, -1)
          _     -> Nothing
        start <- intersect td $ month sd
        end <- if ed /= -1
          then intersect td $ cycleLastOf TG.Month $ month ed
          else cycleNthAfter False TG.Year 1 <$> intersect td (month 1)
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfYear :: Rule
ruleEndOfYear = Rule
  { name = "end of year"
  , pattern = [ regex "(до|в )?(кон(ец|ца|це) (этого )?года)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        start <- std
        end <- intersect (month 1) $ cycleYear 1
        Token Time <$> interval TTime.Open start end
          where
            std = if "до" `Text.isPrefixOf` Text.toLower match
              then Just $ cycleNth TG.Second 0
              else intersect (month 9) $ cycleYear 0
            cycleYear = cycleNth TG.Year
      _ -> Nothing
  }

ruleBeginningOfYear :: Rule
ruleBeginningOfYear = Rule
  { name = "beginning of year"
  , pattern = [ regex "(в )?(начал(о|е) (этого )?года)" ]
  , prod = \_ -> do
      start <- intersect (month 1) $ cycleNth TG.Year 0
      end <- intersect (month 4) $ cycleNth TG.Year 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfWeek :: Rule
ruleEndOrBeginningOfWeek = Rule
  { name = "at the beginning|end of <week>"
  , pattern =
    [ regex "(в )?(начал(о|е)|кон(ец|це))"
    , Predicate $ isGrainOfTime TG.Week
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match1:_)):Token Time td:_) -> do
        (sd, ed) <- case (Text.take 3 . Text.toLower) match1 of
          "нач" -> Just (1, 3)
          "кон"  -> Just (5, 7)
          _        -> Nothing
        start <- intersect td $ dayOfWeek sd
        end <- intersect td $ dayOfWeek ed
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleCycleThisLastNext :: Rule
ruleCycleThisLastNext = Rule
  { name = "this|last|next <cycle>"
  , pattern =
    [ regex "(эт(а|о(т|м|й|го)?)|текущ(ий|ем|ая|ей|его|ее)|следующ(ий|ем|ей|ая|ей|его|ее)|предыдущ(ий|ем|яя|ей|его|ее)|прошл(ый|ом|ая|ой|ого|ое)|прошедш(ий|ем|ая|ей|его|ее))"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        case (Text.take 3 . Text.toLower) match of
          "это" -> tt $ cycleNth grain 0
          "эта" -> tt $ cycleNth grain 0
          "тек" -> tt $ cycleNth grain 0
          "пре" -> tt . cycleNth grain $ - 1
          "про" -> tt . cycleNth grain $ - 1
          "сле" -> tt $ cycleNth grain 1
          _ -> Nothing
      _ -> Nothing
  }

ruleCycleAfterBeforeTime :: Rule
ruleCycleAfterBeforeTime = Rule
  { name = "<cycle> after|before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "(после|до)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) ->
        let n = if Text.toLower match == "после" then 1 else - 1 in
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

ruleCycleOrdinalTime :: Rule
ruleCycleOrdinalTime = Rule
  { name = "<ordinal> <cycle> <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleOrdinalInTime :: Rule
ruleCycleOrdinalInTime = Rule
  { name = "<ordinal> <cycle> in <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "в"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleOrdinalAfterTime :: Rule
ruleCycleOrdinalAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "после"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleOrdinalQuarter :: Rule
ruleCycleOrdinalQuarter = Rule
  { name = "<ordinal> quarter"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleCycleOrdinalQuarterYear :: Rule
ruleCycleOrdinalQuarterYear = Rule
  { name = "<ordinal> quarter <year>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter False TG.Quarter (n - 1) td
      _ -> Nothing
  }

ruleDurationInWithinAfter :: Rule
ruleDurationInWithinAfter = Rule
  { name = "in|within|after <duration>"
  , pattern =
    [ regex "через"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationLastNext :: Rule
ruleDurationLastNext = Rule
  { name = "last|past|next <duration>"
  , pattern =
    [ regex "(прошл(ый|ая|ое|ые)|предыдущ(ий|ая|ее|ие)|прошедш(ий|ая|ее|ие)|последн(ий|ие|яя|ее)|следующ(ий|ая|ее|ие))"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration DurationData{TDuration.grain, TDuration.value}:
       _) -> case (Text.take 5 . Text.toLower) match of
         "следу" -> tt $ cycleN True grain value
         "прошл" -> tt $ cycleN True grain (- value)
         "преды" -> tt $ cycleN True grain (- value)
         "проше" -> tt $ cycleN True grain (- value)
         "после" -> tt $ cycleN True grain (- value)
         _           -> Nothing
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> спустя|назад"
  , pattern =
    [ dimension Duration
    , regex "(спустя|назад)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "назад" -> tt $ durationAgo dd
        _     -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDayDurationHenceAgo :: Rule
ruleDayDurationHenceAgo = Rule
  { name = "<day> <duration> hence|ago"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , dimension Duration
    , regex "(спустя|назад)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "назад" -> Token Time <$> intersect td (durationIntervalAgo dd)
         _       -> Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleDayInDuration :: Rule
ruleDayInDuration = Rule
  { name = "<day> in <duration>"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , regex "через"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:Token Duration dd:_) ->
        Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleDurationAfterBeforeTime :: Rule
ruleDurationAfterBeforeTime = Rule
  { name = "<duration> after|before|from <time>"
  , pattern =
    [ dimension Duration
    , regex "(после|до)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
         "до" -> tt $ durationBefore dd td
         _    -> tt $ durationAfter dd td
      _ -> Nothing
  }

ruleIntervalForDurationFrom :: Rule
ruleIntervalForDurationFrom = Rule
  { name = "for <duration> from <time>"
  , pattern =
    [ regex "в течении"
    , dimension Duration
    , regex "(после|начиная( с)?)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td1:_) ->
        Token Time <$> interval TTime.Open td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntersect
  , ruleIntersectOf
  , ruleAbsorbOnTime
  , ruleAbsorbCommaTOD
  , ruleNextDOW
  , ruleNextTime
  , ruleThisTime
  , ruleLastTime
  , ruleLastDOWOfTime
  , ruleLastCycleOfTime
  , ruleLastNight
  , ruleNthTimeOfTime
  , ruleNthTimeAfterTime
  , ruleYear
  , ruleYearOrdinal
  , ruleYearLatent
  , ruleYearADBC
  , ruleDOMLatent
  , ruleDOMOrdinal
  , ruleDOMMonth
  , ruleDOMOrdinalMonthYear
  , ruleTODLatent
  , ruleAtTOD
  , ruleTODOClock
  , ruleHHMM
  , ruleHHMMSS
  , ruleMilitarySpelledOut
  , ruleMilitarySpelledOut2
  , ruleOneHourMinutes
  , ruleTODAMPM
  , ruleHONumeral
  , ruleNumeralToHOD
  , ruleHalfToHOD
  , ruleHalfAfterHOD
  , ruleMinutesOfHour
  , ruleYYYYMMDD
  , ruleDDMMYYYY
  , ruleMMYYYY
  , ruleNoonMidnight
  , rulePartOfDays
  , ruleEarlyMorning
  , rulePODThis
  , ruleTimePOD
  , rulePODofTime
  , ruleWeekend
  , ruleIntervalFromDDDDMonth
  , ruleIntervalDDDDMonth
  , ruleIntervalFrom
  , ruleIntervalBetween
  , ruleIntervalTODDash
  , ruleIntervalDash
  , ruleIntervalTODFrom
  , ruleIntervalTODBetween
  , ruleIntervalBy
  , ruleIntervalByTheEndOf
  , ruleCycleThisLastNext
  , ruleDOMOfTimeMonth
  , ruleDOMDayOfTimeMonth
  , ruleCycleAfterBeforeTime
  , ruleCycleOrdinalTime
  , ruleCycleOrdinalInTime
  , ruleCycleOrdinalAfterTime
  , ruleCycleOrdinalQuarter
  , ruleCycleOrdinalQuarterYear
  , ruleDurationInWithinAfter
  , ruleDurationLastNext
  , ruleDurationHenceAgo
  , ruleDayDurationHenceAgo
  , ruleDayInDuration
  , ruleDurationAfterBeforeTime
  , ruleIntervalForDurationFrom
  , ruleTimezone
  , ruleEndOrBeginningOfMonth
  , ruleEndOrBeginningOfYear
  , ruleEndOrBeginningOfWeek
  , ruleNow
  , ruleEndOfMonth
  , ruleBeginningOfMonth
  , ruleEndOfYear
  , ruleBeginningOfYear
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
