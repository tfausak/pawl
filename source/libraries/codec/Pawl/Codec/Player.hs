{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Player where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.Codec.Status as Status
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.Status as Status.Type

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
  status <- Fields.defaulted "status" Status.Type.Playing Status.codec Player.status
  counters <- Fields.defaulted "counters" Map.empty (Common.multiset PlayerCounterKind.codec) Player.counters
  ringTemptations <- Fields.defaulted "ringTemptations" 0 Common.natural Player.ringTemptations
  speed <- Fields.required "speed" (Common.maybe Common.natural) Player.speed
  commander <- Fields.defaulted "commander" Nothing (Common.maybe PrintingId.codec) Player.commander
  commanderCasts <- Fields.defaulted "commanderCasts" 0 Common.natural Player.commanderCasts
  commanderDamage <- Fields.defaulted "commanderDamage" Map.empty (Common.naturalMap PlayerId.codec Common.natural) Player.commanderDamage
  dungeons <- Fields.defaulted "dungeons" Set.empty (Common.set PrintingId.codec) Player.dungeons
  outsideTheGame <- Fields.defaulted "outsideTheGame" Map.empty (Common.naturalMap PrintingId.codec Common.natural) Player.outsideTheGame
  completedDungeons <- Fields.defaulted "completedDungeons" 0 Common.natural Player.completedDungeons
  completedDungeonNames <- Fields.defaulted "completedDungeonNames" Set.empty (Common.set CardName.codec) Player.completedDungeonNames
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
        Player.outsideTheGame = outsideTheGame,
        Player.completedDungeons = completedDungeons,
        Player.completedDungeonNames = completedDungeonNames
      }
