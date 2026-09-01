{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.StatedFlip where

import qualified Pawl.Codec.CoinFace as CoinFace
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.StatedFlip as StatedFlip

-- | A bare object keyed by the record's field names. Every field defaults, so a
-- card writes only the half of CR 705.3 it states.
codec :: Codec.Codec StatedFlip.StatedFlip
codec = Fields.object $ do
  face <- Fields.defaulted "face" Nothing (Common.maybe CoinFace.codec) StatedFlip.face
  wins <- Fields.defaulted "wins" False Common.boolean StatedFlip.wins
  firstEachTurn <- Fields.defaulted "firstEachTurn" False Common.boolean StatedFlip.firstEachTurn
  pure
    StatedFlip.MkStatedFlip
      { StatedFlip.face = face,
        StatedFlip.wins = wins,
        StatedFlip.firstEachTurn = firstEachTurn
      }
