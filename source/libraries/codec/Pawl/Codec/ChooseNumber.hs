{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChooseNumber where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChooseNumber as ChooseNumber

-- | The bound is ELIDED when absent, as Draw's slot is.
codec :: Codec.Codec ChooseNumber.ChooseNumber
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec ChooseNumber.slot
  upTo <- Fields.defaulted "upTo" Nothing (Common.maybe Common.natural) ChooseNumber.upTo
  pure
    ChooseNumber.MkChooseNumber
      { ChooseNumber.slot = slot,
        ChooseNumber.upTo = upTo
      }
