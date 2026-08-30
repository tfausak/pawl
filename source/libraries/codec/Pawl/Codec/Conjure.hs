{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Conjure where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ConjureDestination as ConjureDestination
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Conjure as Conjure

-- | The card codec is a PARAMETER, Pawl.Codec.Create's posture: this arm is
-- where card data nests inside card data, and Pawl.Codec.Card is what ties the
-- knot.
--
-- The count is ELIDED at one, Pawl.Codec.CreateCopy's posture rather than
-- Pawl.Codec.Create's required key: "conjure a card named Ornithopter" prints no
-- number, so a card writing @1@ would be spelling out something the sentence
-- does not say. The destination has no default to elide to -- every conjuring
-- card states where the card goes.
codec :: (Typeable.Typeable card) => Codec.Codec card -> Codec.Codec (Conjure.Conjure card)
codec cardCodec = Fields.object $ do
  quantity <- Fields.defaulted "quantity" Conjure.defaultQuantity Quantity.codec Conjure.quantity
  card <- Fields.required "card" cardCodec Conjure.card
  destination <- Fields.required "destination" ConjureDestination.codec Conjure.destination
  pure
    Conjure.MkConjure
      { Conjure.quantity = quantity,
        Conjure.card = card,
        Conjure.destination = destination
      }
