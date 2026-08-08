{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EventShapeSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EventShape" $ do
  Spec.it s "MovedBetween" $
    -- CR 700.4's "dies": moved from the battlefield to a graveyard.
    Common.assertJsonCodec
      s
      EventShape.toJson
      EventShape.fromJson
      (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard)
      """ {"type":"MovedBetween","value":[{"type":"Battlefield"},{"type":"Graveyard"}]} """
  -- CR 601.2i's cast: PAYLOADLESS, so the encoded form is the bare tag and a
  -- decoder that demanded a value would reject every card that names it.
  Spec.it s "SpellCast" $
    Common.assertJsonCodec
      s
      EventShape.toJson
      EventShape.fromJson
      EventShape.SpellCast
      """ {"type":"SpellCast"} """
