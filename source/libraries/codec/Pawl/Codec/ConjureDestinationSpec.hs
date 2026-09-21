module Pawl.Codec.ConjureDestinationSpec where

import qualified Pawl.Codec.ConjureDestination as ConjureDestination
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.TapState as TapState

-- | A case PER CONSTRUCTOR, where an 'Pawl.JsonCodec.Arm.enum' codec needed
-- only a representative: 'Pawl.JsonCodec.Arm.tagged' derives the tag from a
-- total function but not the arm, and a tag with no arm encodes as @{}@, so each
-- constructor owes its own round trip.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ConjureDestination" $ do
  Spec.it s "Hand" $
    Common.assertCodec
      s
      ConjureDestination.codec
      ConjureDestination.Hand
      " {\"type\":\"Hand\"} "
  Spec.it s "Library" $
    Common.assertCodec
      s
      ConjureDestination.codec
      ConjureDestination.Library
      " {\"type\":\"Library\"} "
  Spec.it s "Graveyard" $
    Common.assertCodec
      s
      ConjureDestination.codec
      ConjureDestination.Graveyard
      " {\"type\":\"Graveyard\"} "
  -- CR 110.5b's untapped is the elided default, so the arm that states nothing
  -- writes the bare tag: "conjure a card named Monastery Mentor onto the
  -- battlefield" prints no status.
  Spec.it s "Battlefield" $
    Common.assertCodec
      s
      ConjureDestination.codec
      (ConjureDestination.Battlefield TapState.Untapped)
      " {\"type\":\"Battlefield\"} "
  -- Foundry Groundbreaker's "onto the battlefield tapped", the arm's whole
  -- reason for carrying a payload at all.
  Spec.it s "Battlefield tapped" $
    Common.assertCodec
      s
      ConjureDestination.codec
      (ConjureDestination.Battlefield TapState.Tapped)
      " {\"type\":\"Battlefield\",\"value\":{\"type\":\"Tapped\"}} "
  -- Decode-only, because it is the other spelling of the value above rather
  -- than a value of its own: Pawl.JsonCodec.Arm.optionalPayload accepts a
  -- stated default and the encoder writes the bare tag back, which is what keeps
  -- one card file shape canonical.
  Spec.it s "Battlefield reads a stated untapped" $
    Common.assertFromJson
      s
      (Codec.decode ConjureDestination.codec)
      " {\"type\":\"Battlefield\",\"value\":{\"type\":\"Untapped\"}} "
      (ConjureDestination.Battlefield TapState.Untapped)
  Spec.it s "has a schema" $
    Common.assertHasSchema s ConjureDestination.codec
