{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Meld where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Meld as Meld

-- | Both keys required: CR 701.42a names the cards and the combined back face,
-- and neither has a default a card file could elide.
--
-- The card codec is a PARAMETER, Pawl.Codec.Create's posture: this arm is where
-- card data nests inside card data, and Pawl.Codec.Card is what ties the knot.
codec :: (Typeable.Typeable card) => Codec.Codec card -> Codec.Codec (Meld.Meld card)
codec cardCodec = Fields.object $ do
  objects <- Fields.required "objects" ObjectRef.codec Meld.objects
  result <- Fields.required "result" cardCodec Meld.result
  pure
    Meld.MkMeld
      { Meld.objects = objects,
        Meld.result = result
      }
