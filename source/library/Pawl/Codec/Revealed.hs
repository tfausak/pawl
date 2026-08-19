{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Revealed where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.RevealCause as RevealCause
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Revealed as Revealed

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec Revealed.Revealed
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec Revealed.player
  card <- Fields.required "card" ObjectId.codec Revealed.card
  cause <- Fields.required "cause" RevealCause.codec Revealed.cause
  characteristics <- Fields.required "characteristics" ProjectedCharacteristics.codec Revealed.characteristics
  pure
    Revealed.MkRevealed
      { Revealed.player = player,
        Revealed.card = card,
        Revealed.cause = cause,
        Revealed.characteristics = characteristics
      }
