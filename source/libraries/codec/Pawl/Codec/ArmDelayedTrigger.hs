{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ArmDelayedTrigger where

import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.Onset as Onset
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.Onset as Onset

-- | A bare object keyed by the record's field names, with both halves of the
-- envelope defaulted, so a one-shot arming writes only its @name@.
--
-- The positional payload this replaces was THREE shapes told apart by LENGTH --
-- a bare name, a name-and-duration pair, and a name-onset-duration triple --
-- because an onset and a duration are both tagged objects and JSON type could
-- not separate them. Two independently elided keys say the same thing without
-- the arity puzzle: a stated duration alone and a stated onset alone are each
-- just the one key.
codec :: Codec.Codec ArmDelayedTrigger.ArmDelayedTrigger
codec = Fields.object $ do
  name <- Fields.required "name" AbilityName.codec ArmDelayedTrigger.name
  onset <- Fields.defaulted "onset" Onset.Immediately Onset.codec ArmDelayedTrigger.onset
  duration <- Fields.defaulted "duration" Nothing (Common.maybe Duration.codec) ArmDelayedTrigger.duration
  pure
    ArmDelayedTrigger.MkArmDelayedTrigger
      { ArmDelayedTrigger.name = name,
        ArmDelayedTrigger.onset = onset,
        ArmDelayedTrigger.duration = duration
      }
