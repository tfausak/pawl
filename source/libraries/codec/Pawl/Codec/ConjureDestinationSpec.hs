module Pawl.Codec.ConjureDestinationSpec where

import qualified Pawl.Codec.ConjureDestination as ConjureDestination
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.ConjureEntry as ConjureEntry
import qualified Pawl.Types.LibraryDepth as LibraryDepth
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
      (ConjureDestination.Library Nothing)
      " {\"type\":\"Library\"} "
  -- Calim, Djinn Emperor's "into your library seventh from the top".
  Spec.it s "Library seventh from the top" $
    Common.assertCodec
      s
      ConjureDestination.codec
      (ConjureDestination.Library (Just (LibraryDepth.FromTop 7)))
      " {\"type\":\"Library\",\"value\":{\"type\":\"FromTop\",\"value\":7}} "
  Spec.it s "Graveyard" $
    Common.assertCodec
      s
      ConjureDestination.codec
      ConjureDestination.Graveyard
      " {\"type\":\"Graveyard\"} "
  Spec.it s "Exile" $
    Common.assertCodec
      s
      ConjureDestination.codec
      ConjureDestination.Exile
      " {\"type\":\"Exile\"} "
  -- CR 110.5b's untapped is the elided default, so the arm that states nothing
  -- writes the bare tag: "conjure a card named Monastery Mentor onto the
  -- battlefield" prints no status.
  Spec.it s "Battlefield" $
    Common.assertCodec
      s
      ConjureDestination.codec
      (ConjureDestination.Battlefield ConjureEntry.defaultValue)
      " {\"type\":\"Battlefield\"} "
  -- Foundry Groundbreaker's "onto the battlefield tapped", the arm's whole
  -- reason for carrying a payload at all.
  Spec.it s "Battlefield tapped" $
    Common.assertCodec
      s
      ConjureDestination.codec
      (ConjureDestination.Battlefield ConjureEntry.defaultValue {ConjureEntry.tapped = TapState.Tapped})
      " {\"type\":\"Battlefield\",\"value\":{\"tapped\":{\"type\":\"Tapped\"}}} "
  -- Kari Zev, Crew of Two's "onto the battlefield tapped and attacking" (CR
  -- 508.4).
  Spec.it s "Battlefield tapped and attacking" $
    Common.assertCodec
      s
      ConjureDestination.codec
      (ConjureDestination.Battlefield (ConjureEntry.MkConjureEntry TapState.Tapped True))
      " {\"type\":\"Battlefield\",\"value\":{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true}} "
  -- Decode-only, because it is the other spelling of the value above rather
  -- than a value of its own: Pawl.JsonCodec.Arm.optionalPayload accepts a
  -- stated default and the encoder writes the bare tag back, which is what keeps
  -- one card file shape canonical.
  Spec.it s "Battlefield reads a stated untapped" $
    Common.assertFromJson
      s
      (Codec.decode ConjureDestination.codec)
      " {\"type\":\"Battlefield\",\"value\":{\"tapped\":{\"type\":\"Untapped\"}}} "
      (ConjureDestination.Battlefield ConjureEntry.defaultValue)
  Spec.it s "has a schema" $
    Common.assertHasSchema s ConjureDestination.codec
