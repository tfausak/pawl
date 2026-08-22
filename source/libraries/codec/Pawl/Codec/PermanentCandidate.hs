{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentCandidate where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.InstanceOrdinal as InstanceOrdinal
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentCandidate as PermanentCandidate

-- | The effect codec is built from the card codec the same way every other
-- parameterized codec in this library is.
codec :: Codec.Codec PermanentCandidate.PermanentCandidate
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec PermanentCandidate.source
  effect <- Fields.required "effect" (ReplacementEffect.codec (Effect.codec Card.codec)) PermanentCandidate.effect
  ordinal <- Fields.required "ordinal" InstanceOrdinal.codec PermanentCandidate.ordinal
  pure
    PermanentCandidate.MkPermanentCandidate
      { PermanentCandidate.source = source,
        PermanentCandidate.effect = effect,
        PermanentCandidate.ordinal = ordinal
      }
