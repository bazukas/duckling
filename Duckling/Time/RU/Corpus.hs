-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RU.Corpus
  ( corpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "сейчас"
             , "прямо сейчас"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "сегодня"
             , "в этот день"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "в 2014г."
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "вчера"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "завтра"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "понедельник"
             , "Пн."
             , "в этот понедельник"
             , "Понедельник, 18 февраля"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "вторник"
             , "Вт."
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "среда"
             , "ср."
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "четверг"
             , "Чт"
             , "Чт."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "пятница"
             , "Пт"
             , "Пт."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "Суббота"
             , "суб"
             , "Сб."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "воскресенье"
             , "вскр."
             , "вс"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "первое марта"
             , "первого марта"
             , "1го марта"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3 марта"
             , "третьего марта"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 марта 2015 года"
             , "3 март 2015"
             , "третье марта 2015г."
             , "3.03.2015"
             , "03.03.2015"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15го февраля"
             , "15 февраля"
             , "15ое февраля"
             , "15 февраль"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8 августа"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ "Март через год"
             , "Март в следующем году"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Пятница, 18 июля"
             , "18 Июл, пятница"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             [ "Октябрь 2014"
             , "Октябрь 2014го года"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 апреля 2015г"
             , "14 апреля 2015 года"
             , "14 апреля 2015"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "следующий вторник"
             , "вторник на следующей неделе"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "следующий март"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Воскресенье, 10 февраля"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Среда, 13 февраля"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "на этой неделе"
             , "эта неделя"
             , "текущая неделя"
             , "на текущей неделе"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "прошлая неделя"
             , "на прошлой неделе"
             , "прошедшая неделя"
             , "на прошедшей неделе"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "следующая неделя"
             , "на следующей неделе"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "прошлый месяц"
             , "в прошлый месяц"
             , "прошедший месяц"
             , "предыдущий месяц"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "следующий месяц"
             , "в следующем месяце"
             ]
  , examples (datetime (2013, 3, 20, 0, 0, 0) Day)
             [ "20го числа следующего месяца"
             , "20го следующего месяца"
             , "20 следующего месяца"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "20го текущего месяца"
             , "20 числа этого месяца"
             , "20 текущего месяца"
             , "20 числа текущего месяца"
             ]
  , examples (datetime (2013, 1, 20, 0, 0, 0) Day)
             [ "20 числа прошедшего месяца"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "этот квартал"
             , "текущий квартал"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "следующий квартал"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "третий квартал"
             , "3ий квартал"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4й квартал 2018"
             , "четвертый квартал 2018"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "прошлый год"
             , "прошедший год"
             , "предыдущий год"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "этот год"
             , "текущий год"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "следующий год"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "2014 от Р.Х.",
               "в 2014 от Р.Х."
             ]
  , examples (datetime (-2014, 1, 1, 0, 0, 0) Year)
             [ "2014 до Р.Х.",
               "в 2014 до Р.Х."
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "прошлое воскресенье"
             , "прошедшее воскресенье"
             , "предыдущее воскресенье"
             , "воскресенье прошедшей недели"
             , "воскресенье предыдущей недели"
             , "воскресенье прошлой недели"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "прошлый вторник"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "следующий вторник"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "следующая среда"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "среда на следующей неделе"
             , "среда следующей недели"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "понедельник этой недели"
             , "понедельник на этой неделе"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "вторник этой недели"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "среда этой недели"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "послезавтра"
             ]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ "послезавтра в 5 вечера"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "позавчера"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "позавчера в 8 утра"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "последний понедельник марта"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "последнее воскресенье марта 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "третий день октября"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "первая неделя Октября 2014 года"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "последний день октября 2015"
             , "последний день в октябре 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "последняя неделя сентября 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "первый вторник октября"
             , "первый вторник в октябре"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "третий вторник сентября 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "первая среда октября 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "вторая среда октября 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "в 3 утра"
             , "в 3 ночи"
             , "в три утра"
             , "в три ночи"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             , "3:18 утра"
             , "3:18 ночи"
             ]
  , examples (datetime (2016, 2, 1, 7, 0, 0) Hour)
             [ "в 7 утра через 3 года"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "в 3 дня"
             , "в 15"
             , "три дня"
             , "в 3 после обеда"
             , "три после обеда"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "в 15 минут четвертого"
             , "в 3:15 после обеда"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "в 20 минут после 3"
             , "в 3:20 после обеда"
             , "в 15:20"
             , "двадцать минут после трех дня"
             , "в три двадцать"
             , "в три часа двадцать минут"
             ]
  , examples (datetime (2013, 2, 12, 13, 20, 0) Minute)
             [ "в час двадцать"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "в половине четвертого после обеда"
             , "в три с половиной дня"
             , "15:30"
             , "3:30 после обеда"
             , "3:30 дня"
             , "половина четвертого"
             ]
  , examples (datetime (2013, 2, 12, 9, 59, 0) Minute)
             [ "девять пятьдесят девять утра"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "без пятнадцати двенадцать"
             , "11:45 дня"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 вечера"
             , "восемь вечером"
             , "восемь вечера"
             , "в 8 вечером"
             , "вечером в 8"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "в 7:30 вечера в Пятницу, 20 сентября"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "в девять утра в субботу"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "в субботу в девять"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "Пят, 18 июля 2014г, 19:00"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "через секунду"
             , "через одну секунду"
             , "через 1 секунду"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "через минуту"
             , "через одну минуту"
             , "через 1 минуту"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "через 2 минуты"
             , "через две минуты"
             ]
  , examples (datetime (2013, 2, 12, 4, 33, 0) Second)
             [ "через три минуты"
             , "через 3 минуты"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "через 60 минут"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "через час"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "через 15 минут"
             , "через четверть часа"
             , "через пятнадцать минут"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "через пол часа"
             , "через 30 минут"
             , "через половину часа"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ "через 45 минут"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "через 2.5 часа"
             , "через два с половиной часа"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "через один час"
             , "через час"
             , "через 1 час"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "через два часа"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "через 24 часа"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "через день"
             ]
  , examples (datetime (2016, 2, 1, 0, 0, 0) Month)
             [ "через три года"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "через 7 дней"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "через неделю"
             , "через 1 неделю"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 дней назад"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 дней назад"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "неделю назад"
             , "одну неделю назад"
             , "1 неделю назад"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "три недели назад"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "три месяца назад"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "два года назад"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "7 дней спустя"
             ]
  , examples (datetime (2013, 2, 26, 4, 0, 0) Hour)
             [ "14 дней спустя"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "неделю спустя"
             , "одну неделю спустя"
             , "1 неделю спустя"
             ]
  , examples (datetime (2013, 3, 5, 0, 0, 0) Day)
             [ "3 недели спустя"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "три месяца спустя"
             ]
  , examples (datetime (2015, 2, 1, 0, 0, 0) Month)
             [ "два года спустя"
             ]
  , examples (datetimeInterval ((2013, 12, 18, 0, 0, 0), (2013, 12, 29, 0, 0, 0)) Day)
             [ "в течении 10 дней после 18 декабря"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "этим летом"
             , "это лето"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "этой зимой"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "прошлой ночью"
             , "вчерашний вечер"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "этим вечером"
             , "сегодня вечером"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "завтра вечером"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "завтра в обед"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "вчера вечером"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "на этих выходных"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "в понедельник утром"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 9, 0, 0)) Hour)
             [ "понедельник ранним утром"
             , "в понедельник ранним утром"
             , "в ранних часах утра понедельника"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "15 февраля утром"
             , "утром 15го февраля"
             , "утро 15 февраля"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "последние 2 секунды"
             , "последние две секунды"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "следующие 3 секунды"
             , "следующие три секунды"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "последние 2 минуты"
             , "последние две минуты"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "следующие 3 минуты"
             , "следующие три минуты"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "последний час"
             , "последний один час"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "следующие три часа"
             , "следующие 3 часа"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "последние 2 дня"
             , "последние два дня"
             , "предыдущие 2 дня"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "следующие 3 дня"
             , "следующие три дня"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "последние 2 недели"
             , "последние две недели"
             , "предыдущие 2 недели"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "следующие 3 недели"
             , "следующие три недели"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ "последние 2 месяца"
             , "последние два месяца"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Month)
             [ "следующие 3 месяца"
             , "следующие три месяца"
             ]
  , examples (datetimeInterval ((2011, 1, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Year)
             [ "последние 2 года"
             , "последние два года"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2017, 1, 1, 0, 0, 0)) Year)
             [ "следующие 3 года"
             , "следующие три года"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 Июля - 15 Июля"
             , "13-15 июля"
             , "13 до 15 июля"
             , "С 13 до 15 июля"
             , "С 13 по 15 июля"
             , "с 13го до 15е июля"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 Авг - 12 Авг"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "9:30 - 11:00 в четверг"
             , "между 9:30 и 11:00 в четверг"
             , "с 9:30 до 11:00 в четверг"
             , "позже чем 9:30 но раньше 11:00 в Четверг"
             , "Четверг с 9:30 до 11:00"
             , "9:30 до 11:00 в Четверг"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 1, 0, 0), (2013, 2, 13, 2, 31, 0)) Minute)
             [ "завтра между 1:00-2:30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ "13:30 в субботу, 21 сентября"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 13, 0, 0, 0)) Second)
             [ "до конца дня"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 3, 1, 0, 0, 0)) Second)
             [ "до конца месяца"
             , "до окончания месяца"
             ]
  , examples (datetimeInterval ((2013, 2, 1, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
             [ "начало месяца"
             , "в начале месяца"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "16:00 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "четверг 8:00 GMT"
             , "Четверг 8:00 gmt"
             , "Чт в 8 GMT"
             , "Чт в 8 gmt"
             ]
  , examples (datetime (2013, 2, 14, 14, 0, 0) Minute)
             [ "Четверг 8:00 PST"
             , "четверг 8:00 pst"
             , "Чт в 8 утра PST"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "сегодня в 14"
             , "сегодня в 2 дня"
             , "сегодня в 2 после обеда"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "завтра в 3 после обеда"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "в 13:30"
             , "в 1:30 дня"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "через 15 минут"
             , "через пятнадцать минут"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "следующий понедельник"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "в 12"
             , "в полдень"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "в полночь"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "Март"
             , "в марте"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "завтра в 5 вечера"
             , "в 17 завтра"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 13, 0, 0), (2013, 2, 13, 14, 1, 0)) Minute)
             [ "завтра 13:00-14:00"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "первого числа"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "в 10:30"
             , "в 10:30 утра"
             ]
  , examples (datetime (2013, 2, 12, 19, 30, 0) Minute)
             [ "в 7:30 вечера"
             , "в семь тридцать вечера"
             , "в 19:30"
             ]
  , examples (datetime (2013, 2, 13, 13, 50, 0) Minute)
             [ "завтра в 13:50"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "сегодня в 23"
             , "сегодня в 11 вечера"
             ]
  , examples (datetimeInterval ((2013, 8, 27, 0, 0, 0), (2013, 8, 30, 0, 0, 0)) Day)
             [ "27 - 29 Августа"
             , "с 27 по 29 августа"
             ]
  , examples (datetimeInterval ((2013, 10, 23, 0, 0, 0), (2013, 10, 27, 0, 0, 0)) Day)
             [ "23 по 26 октября"
             ]
  , examples (datetimeInterval ((2013, 9, 1, 0, 0, 0), (2013, 9, 9, 0, 0, 0)) Day)
             [ "1-8 сентября"
             ]
  , examples (datetimeInterval ((2013, 9, 12, 0, 0, 0), (2013, 9, 17, 0, 0, 0)) Day)
             [ "12 по 16 сентября"
             ]
  , examples (datetimeInterval ((2013, 8, 19, 0, 0, 0), (2013, 8, 22, 0, 0, 0)) Day)
             [ "19 по 21 августа"
             ]
  , examples (datetimeInterval ((2013, 4, 21, 0, 0, 0), (2013, 5, 1, 0, 0, 0)) Day)
             [ "конец апреля"
             , "в конце апреля"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2014, 1, 11, 0, 0, 0)) Day)
             [ "начало января"
             , "в начале Января"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 14, 0, 0, 0)) Day)
             [ "начало этой недели"
             , "в начале этой недели"
             , "начало текущей недели"
             , "в начале текущей недели"
             ]
  , examples (datetimeInterval ((2013, 2, 4, 0, 0, 0), (2013, 2, 7, 0, 0, 0)) Day)
             [ "начало прошлой недели"
             , "начало предыдущей недели"
             , "начало прошедшей недели"
             , "в начале прошлой недели"
             , "в начале предыдущей недели"
             , "в начале прошедшей недели"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 2, 21, 0, 0, 0)) Day)
             [ "начало следующей недели"
             , "в начале следующей недели"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 0, 0, 0), (2013, 2, 18, 0, 0, 0)) Day)
             [ "конец этой недели"
             , "конец текущей недели"
             , "в конце этой недели"
             , "в конце текущей недели"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
             [ "конец прошлой недели"
             , "конец предыдущей недели"
             , "конец прошедшей недели"
             , "в конце прошлой недели"
             , "в конце предыдущей недели"
             , "в конце прошедшей недели"
             ]
  , examples (datetimeInterval ((2013, 2, 22, 0, 0, 0), (2013, 2, 25, 0, 0, 0)) Day)
             [ "конец следующей недели"
             , "в конце следующей недели"
             ]
  ]

latentCorpus :: Corpus
latentCorpus = (testContext {locale = makeLocale RU Nothing}, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (datetime (2013, 2, 24, 0, 0, 0) Day)
                 [ "24го"
                 ]
      , examples (datetime (1954, 1, 1, 0, 0, 0) Year)
                 [ "1954"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
                 [ "утро"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ "вечер"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ "ночь"
                 ]
      ]
