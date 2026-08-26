{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Player where

import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.Codec.Status as Status
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Player as Player

-- | `counters` is 'Common.multiset', whose entries are key/count objects: the
-- field's absent-means-zero convention (Pawl.Types.Player) makes a zero entry
-- and an absent one the same to a card, but the map really can hold one once an
-- effect has taken the last counter off, and the round trip has to keep it.
--
-- `commanderDamage` is keyed by a PlayerId, a Natural newtype, so it takes
-- 'Common.naturalMap' -- a JSON object keyed by the decimal seat, which is what
-- Pawl.Codec.Combat writes for the same key type.
--
-- `speed` is 'Fields.required' over 'Common.maybe' rather than defaulted to 0,
-- because CR 702.179b makes the absence a THIRD state: CR 704.5aa fires on a
-- player with NO speed and would loop forever against one whose speed had been
-- driven to 0 if the two were spelled alike.
codec :: Codec.Codec Player.Player
codec = Fields.object $ do
  life <- Fields.required "life" Common.integer Player.life
  status <- Fields.required "status" Status.codec Player.status
  counters <- Fields.required "counters" (Common.multiset PlayerCounterKind.codec) Player.counters
  ringTemptations <- Fields.required "ringTemptations" Common.natural Player.ringTemptations
  speed <- Fields.required "speed" (Common.maybe Common.natural) Player.speed
  commander <- Fields.required "commander" (Common.maybe PrintingId.codec) Player.commander
  commanderCasts <- Fields.required "commanderCasts" Common.natural Player.commanderCasts
  commanderDamage <- Fields.required "commanderDamage" (Common.naturalMap PlayerId.codec Common.natural) Player.commanderDamage
  dungeons <- Fields.required "dungeons" (Common.set PrintingId.codec) Player.dungeons
  completedDungeons <- Fields.required "completedDungeons" Common.natural Player.completedDungeons
  pure
    Player.MkPlayer
      { Player.life = life,
        Player.status = status,
        Player.counters = counters,
        Player.ringTemptations = ringTemptations,
        Player.speed = speed,
        Player.commander = commander,
        Player.commanderCasts = commanderCasts,
        Player.commanderDamage = commanderDamage,
        Player.dungeons = dungeons,
        Player.completedDungeons = completedDungeons
      }
