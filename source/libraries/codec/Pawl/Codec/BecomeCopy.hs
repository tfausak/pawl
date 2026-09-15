{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecomeCopy where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecomeCopy as BecomeCopy

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's BecomeCopy arm. @exceptions@ is defaulted for
-- Pawl.Codec.AsCopy's reason: CR 707.9's "except ..." clause is absent from most
-- printings, and @duration@ is optional for the same reason -- most copy effects
-- state none.
codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (BecomeCopy.BecomeCopy ability)
codec abilityCodec = Fields.object $ do
  original <- Fields.required "original" ObjectRef.codec BecomeCopy.original
  subject <- Fields.required "subject" ObjectRef.codec BecomeCopy.subject
  duration <- Fields.defaulted "duration" Nothing (Common.maybe Duration.codec) BecomeCopy.duration
  exceptions <- Fields.defaulted "exceptions" [] (Common.list (CopyException.codec abilityCodec)) BecomeCopy.exceptions
  pure
    BecomeCopy.MkBecomeCopy
      { BecomeCopy.original = original,
        BecomeCopy.subject = subject,
        BecomeCopy.duration = duration,
        BecomeCopy.exceptions = exceptions
      }
