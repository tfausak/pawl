{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CoinFlipped where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CoinFlipped as CoinFlipped

-- | A bare object keyed by the record's field names, Pawl.Codec.Mentored's
-- shape. Runtime-only: GameEvent serialises transcripts, never card data.
--
-- Both keys are REQUIRED. CR 705.2 settles the outcome for every flip this
-- record describes, so an elided "won" would be a default standing in for an
-- answer the rule always gives.
codec :: Codec.Codec CoinFlipped.CoinFlipped
codec = Fields.object $ do
  flipper <- Fields.required "flipper" PlayerId.codec CoinFlipped.flipper
  won <- Fields.required "won" Common.boolean CoinFlipped.won
  pure
    CoinFlipped.MkCoinFlipped
      { CoinFlipped.flipper = flipper,
        CoinFlipped.won = won
      }
