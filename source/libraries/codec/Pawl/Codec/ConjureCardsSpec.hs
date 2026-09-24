module Pawl.Codec.ConjureCardsSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Codec.ConjureCards as ConjureCards
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ConjureCards as ConjureCards
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromReference as FromReference
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The @card@ parameter is instantiated at 'Text.Text', 'Pawl.Codec.ConjureSpec''s
-- reason: this codec reaches it only through the supplied codec.
codec :: Codec.Codec (ConjureCards.ConjureCards Text.Text)
codec = ConjureCards.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ConjureCards" $ do
  Spec.it s "Written" $
    Common.assertCodec
      s
      codec
      (ConjureCards.Written (Text.pack "Ornithopter" NonEmpty.:| []))
      " {\"type\":\"Written\",\"value\":[\"Ornithopter\"]} "
  Spec.it s "Duplicate" $
    Common.assertCodec
      s
      codec
      (ConjureCards.Duplicate (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"Duplicate\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "Reference" $
    Common.assertCodec
      s
      codec
      (ConjureCards.Reference (FromReference.MkFromReference (Filter.HasCardType CardType.Creature) Nothing))
      " {\"type\":\"Reference\",\"value\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
