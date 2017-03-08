-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Dimensions.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Distance.Tests as Distance
import qualified Duckling.Duration.Tests as Duration
import qualified Duckling.Email.Tests as Email
import qualified Duckling.Finance.Tests as Finance
import qualified Duckling.Number.Tests as Number
import qualified Duckling.Ordinal.Tests as Ordinal
import qualified Duckling.PhoneNumber.Tests as PhoneNumber
import qualified Duckling.Quantity.Tests as Quantity
import qualified Duckling.Temperature.Tests as Temperature
import qualified Duckling.Time.Tests as Time
import qualified Duckling.Volume.Tests as Volume
import qualified Duckling.Url.Tests as Url

tests :: TestTree
tests = testGroup "Dimensions Tests"
  [ Distance.tests
  , Duration.tests
  , Email.tests
  , Finance.tests
  , Number.tests
  , Ordinal.tests
  , PhoneNumber.tests
  , Quantity.tests
  , Temperature.tests
  , Time.tests
  , Volume.tests
  , Url.tests
  ]