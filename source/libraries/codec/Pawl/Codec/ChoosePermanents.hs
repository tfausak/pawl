{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChoosePermanents where

import qualified Pawl.Codec.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChoosePermanents as ChoosePermanents
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, Pawl.Codec.AnyNumberDiscard's
-- shape; @chooser@ is defaulted, Pawl.Codec.ChosenPermanent's reason.
codec :: Codec.Codec ChoosePermanents.ChoosePermanents
codec = Fields.object $ do
  chooser <- Fields.defaulted "chooser" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec ChoosePermanents.chooser
  permanents <- Fields.required "permanents" AnyNumberMatching.codec ChoosePermanents.permanents
  slot <- Fields.required "slot" SlotName.codec ChoosePermanents.slot
  pure
    ChoosePermanents.MkChoosePermanents
      { ChoosePermanents.chooser = chooser,
        ChoosePermanents.permanents = permanents,
        ChoosePermanents.slot = slot
      }
