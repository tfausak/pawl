{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CompletedDungeon where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CompletedDungeon as CompletedDungeon

-- | A bare object keyed by the record's field names, Pawl.Codec.PlayerCounterTally's
-- shape. The tag that picks it is Pawl.Codec.Quantity's "CompletedDungeon".
codec :: Codec.Codec CompletedDungeon.CompletedDungeon
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec CompletedDungeon.player
  dungeon <- Fields.required "dungeon" CardName.codec CompletedDungeon.dungeon
  pure CompletedDungeon.MkCompletedDungeon {CompletedDungeon.player = player, CompletedDungeon.dungeon = dungeon}
