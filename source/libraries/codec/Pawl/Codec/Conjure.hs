{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Conjure where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ConjureDestination as ConjureDestination
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Conjure as Conjure

-- | The card codec is a PARAMETER, Pawl.Codec.Create's posture: this arm is
-- where card data nests inside card data, and Pawl.Codec.Card is what ties the
-- knot.
--
-- Both keys are required. The destination has no default to elide to: every
-- conjuring card states where the card goes, and a one-armed
-- Pawl.Types.ConjureDestination would make the key look settled by the engine
-- rather than by the card.
codec :: (Typeable.Typeable card) => Codec.Codec card -> Codec.Codec (Conjure.Conjure card)
codec cardCodec = Fields.object $ do
  card <- Fields.required "card" cardCodec Conjure.card
  destination <- Fields.required "destination" ConjureDestination.codec Conjure.destination
  pure
    Conjure.MkConjure
      { Conjure.card = card,
        Conjure.destination = destination
      }
