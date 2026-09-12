module Pawl.Codec.ConjureSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Codec.Conjure as Conjure
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.Quantity as Quantity

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
            Conjure.cards = Text.pack "Ornithopter" NonEmpty.:| [],
            Conjure.destination = ConjureDestination.Hand
          }
      )
      " {\"cards\":[\"Ornithopter\"],\"destination\":{\"type\":\"Hand\"}} "
  -- The other form: a stated count, which has to survive the elision guard, and
  -- the destination arm that is not the default-looking one.
  Spec.it s "MkConjure with a stated count" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Quantity.Literal 4,
            Conjure.cards = Text.pack "Lightning Bolt" NonEmpty.:| [],
            Conjure.destination = ConjureDestination.Library
          }
      )
      " {\"quantity\":{\"type\":\"Literal\",\"value\":4},\"cards\":[\"Lightning Bolt\"],\"destination\":{\"type\":\"Library\"}} "
  -- A printed spellbook: two candidates in the list, which is the only shape
  -- that tells this key from the singular one it replaced.
  Spec.it s "MkConjure over a printed spellbook" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.quantity = Conjure.defaultQuantity,
            Conjure.cards = Text.pack "Ponder" NonEmpty.:| [Text.pack "Dark Ritual"],
            Conjure.destination = ConjureDestination.Hand
          }
      )
      " {\"cards\":[\"Ponder\",\"Dark Ritual\"],\"destination\":{\"type\":\"Hand\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
