{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TargetCount where

import qualified Data.Text as Text
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TargetCount as TargetCount

-- | Both invariants Pawl.Types.TargetCount states are enforced HERE, this being
-- where a range enters the engine at all: a card whose slot takes no target is
-- not a target slot, and one whose minimum exceeds its maximum names no legal
-- number for CR 601.2c to announce. They read the assembled record rather than
-- either field alone, which is what 'Fields.objectWith' is for.
--
-- Neither can be broken by an absent maximum -- CR 601.2c's "any number" allows
-- every number from `least` up -- so both tests pass an unbounded count.
isRange :: TargetCount.TargetCount -> Either Text.Text TargetCount.TargetCount
isRange c
  | TargetCount.most c == Just 0 = Left (Text.pack "TargetCount: most must be at least 1")
  | maybe False (TargetCount.least c >) (TargetCount.most c) = Left (Text.pack "TargetCount: least must not exceed most")
  | otherwise = Right c

-- An ABSENT "most" is CR 601.2c's "any number of target ...", the one phrasing
-- with no printed maximum; a null one reads the same. 'Fields.defaulted' writes
-- neither, so an unbounded count round trips as @{"least":0}@.
codec :: Codec.Codec TargetCount.TargetCount
codec = Fields.objectWith isRange $ do
  least <- Fields.required "least" Common.natural TargetCount.least
  most <- Fields.defaulted "most" Nothing (Common.maybe Common.natural) TargetCount.most
  pure TargetCount.MkTargetCount {TargetCount.least = least, TargetCount.most = most}
