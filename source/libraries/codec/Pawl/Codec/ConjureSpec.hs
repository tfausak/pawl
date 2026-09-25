module Pawl.Codec.ConjureSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Codec.Conjure as Conjure
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureCards as ConjureCards
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.ConjureSelection as ConjureSelection
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The @card@ parameter is instantiated at 'Text.Text': this codec reaches it
-- only through the supplied codec, so any type proves the shape.
codec :: Codec.Codec (Conjure.Conjure Text.Text)
codec = Conjure.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Conjure" $ do
  -- The count elided at one, which is what "conjure a card named Ornithopter"
  -- prints.
  Spec.it s "MkConjure" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Conjure.defaultQuantity,
            Conjure.cards = ConjureCards.Written (Text.pack "Ornithopter" NonEmpty.:| []),
            Conjure.selection = Conjure.defaultSelection,
            Conjure.destination = ConjureDestination.Hand,
            Conjure.slot = Nothing
          }
      )
      " {\"cards\":{\"type\":\"Written\",\"value\":[\"Ornithopter\"]},\"destination\":{\"type\":\"Hand\"}} "
  -- The other form: a stated count, which has to survive the elision guard, and
  -- the destination arm that is not the default-looking one.
  Spec.it s "MkConjure with a stated count" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Quantity.Literal 4,
            Conjure.cards = ConjureCards.Written (Text.pack "Lightning Bolt" NonEmpty.:| []),
            Conjure.selection = Conjure.defaultSelection,
            Conjure.destination = ConjureDestination.Library,
            Conjure.slot = Nothing
          }
      )
      " {\"quantity\":{\"type\":\"Literal\",\"value\":4},\"cards\":{\"type\":\"Written\",\"value\":[\"Lightning Bolt\"]},\"destination\":{\"type\":\"Library\"}} "
  -- A printed spellbook: two candidates in the list, which is the only shape
  -- that tells this key from the singular one it replaced.
  Spec.it s "MkConjure over a printed spellbook" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Conjure.defaultQuantity,
            Conjure.cards = ConjureCards.Written (Text.pack "Ponder" NonEmpty.:| [Text.pack "Dark Ritual"]),
            Conjure.selection = Conjure.defaultSelection,
            Conjure.destination = ConjureDestination.Hand,
            Conjure.slot = Nothing
          }
      )
      " {\"cards\":{\"type\":\"Written\",\"value\":[\"Ponder\",\"Dark Ritual\"]},\"destination\":{\"type\":\"Hand\"}} "
  -- The chosen half of the spellbook shape, and the only value of the selection
  -- key that survives the elision guard.
  Spec.it s "MkConjure over a spellbook picked by choice" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Conjure.defaultQuantity,
            Conjure.cards = ConjureCards.Written (Text.pack "Ponder" NonEmpty.:| [Text.pack "Dark Ritual"]),
            Conjure.selection = ConjureSelection.ByChoice,
            Conjure.destination = ConjureDestination.Hand,
            Conjure.slot = Nothing
          }
      )
      " {\"cards\":{\"type\":\"Written\",\"value\":[\"Ponder\",\"Dark Ritual\"]},\"selection\":{\"type\":\"ByChoice\"},\"destination\":{\"type\":\"Hand\"}} "
  -- The other arm of the cards key: a duplicate names an object already in the
  -- game, so the card codec is never reached and the parameter goes unused.
  Spec.it s "MkConjure of a duplicate" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Conjure.defaultQuantity,
            Conjure.cards = ConjureCards.Duplicate (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
            Conjure.selection = Conjure.defaultSelection,
            Conjure.destination = ConjureDestination.Hand,
            Conjure.slot = Nothing
          }
      )
      " {\"cards\":{\"type\":\"Duplicate\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}},\"destination\":{\"type\":\"Hand\"}} "
  -- Kari Zev, Crew of Two's "that card": the slot a later clause names.
  Spec.it s "MkConjure bound to a slot" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Conjure.defaultQuantity,
            Conjure.cards = ConjureCards.Written (Text.pack "Ragavan" NonEmpty.:| []),
            Conjure.selection = Conjure.defaultSelection,
            Conjure.destination = ConjureDestination.Hand,
            Conjure.slot = Just (SlotName.MkSlotName (Text.pack "conjured"))
          }
      )
      " {\"cards\":{\"type\":\"Written\",\"value\":[\"Ragavan\"]},\"destination\":{\"type\":\"Hand\"},\"slot\":\"conjured\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
