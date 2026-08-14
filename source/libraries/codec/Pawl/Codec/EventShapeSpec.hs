{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EventShapeSpec where

import qualified Pawl.Codec.EventShape as EventShape
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.MovedBetween as MovedBetween
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EventShape" $ do
  Spec.it s "MovedBetween" $
    -- CR 700.4's "dies": moved from the battlefield to a graveyard.
    Common.assertCodec
      s
      EventShape.codec
      (EventShape.MovedBetween (MovedBetween.MkMovedBetween Zone.Battlefield Zone.Graveyard))
      """ {"type":"MovedBetween","value":{"from":{"type":"Battlefield"},"to":{"type":"Graveyard"}}} """
  -- CR 601.2i's cast: PAYLOADLESS, so the encoded form is the bare tag and a
  -- decoder that demanded a value would reject every card that names it.
  Spec.it s "SpellCast" $
    Common.assertCodec
      s
      EventShape.codec
      EventShape.SpellCast
      """ {"type":"SpellCast"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s EventShape.codec
