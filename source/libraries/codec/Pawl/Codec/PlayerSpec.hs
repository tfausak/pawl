module Pawl.Codec.PlayerSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Pawl.Codec.Player as Player
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Status as Status

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Player" $ do
  -- CR 103.4's starting life total and nothing else: a player outside a
  -- Commander game (CR 903.3), with no speed at all (CR 702.179b) and no
  -- counters. `speed` is null here and 0 below, which CR 704.5aa makes two
  -- different players.
  Spec.it s "a player at the start of an ordinary game" $
    Common.assertCodec
      s
      Player.codec
      Player.MkPlayer
        { Player.life = 20,
          Player.status = Status.Playing,
          Player.counters = Map.empty,
          Player.ringTemptations = 0,
          Player.speed = Nothing,
          Player.commander = Nothing,
          Player.commanderCasts = 0,
          Player.commanderDamage = Map.empty,
          Player.dungeons = Set.empty,
          Player.completedDungeons = 0
        }
      " {\"life\":20,\"status\":{\"type\":\"Playing\"},\"counters\":[],\"ringTemptations\":0,\"speed\":null,\"commander\":null,\"commanderCasts\":0,\"commanderDamage\":{},\"dungeons\":[],\"completedDungeons\":0} "
  -- Every axis away from the case above. `life` is NEGATIVE, which CR 104.3b
  -- reaches through a state-based action rather than clamping at zero, so the
  -- field is an Integer and a Natural encoder would reject this state.
  -- `counters` carries a kind at ZERO beside one at two: an absent kind reads as
  -- zero (Pawl.Types.Player), so the two are the same to a card, but the map
  -- really holds the entry and the round trip has to keep it.
  Spec.it s "a departed Commander player with counters, speed and dungeon" $
    Common.assertCodec
      s
      Player.codec
      Player.MkPlayer
        { Player.life = -1,
          Player.status = Status.Departed Departure.Conceded,
          Player.counters =
            Map.fromList
              [ (PlayerCounterKind.Energy, 2),
                (PlayerCounterKind.Poison, 0)
              ],
          Player.ringTemptations = 3,
          Player.speed = Just 4,
          Player.commander = Just (PrintingId.MkPrintingId 5),
          Player.commanderCasts = 6,
          Player.commanderDamage = Map.singleton (PlayerId.MkPlayerId 7) 8,
          Player.dungeons = Set.fromList [PrintingId.MkPrintingId 9, PrintingId.MkPrintingId 11],
          Player.completedDungeons = 10
        }
      ( " {\"life\":-1,\"status\":{\"type\":\"Departed\",\"value\":{\"type\":\"Conceded\"}}"
          <> ",\"counters\":[{\"key\":{\"type\":\"Energy\"},\"value\":2}"
          <> ",{\"key\":{\"type\":\"Poison\"},\"value\":0}]"
          <> ",\"ringTemptations\":3,\"speed\":4,\"commander\":5,\"commanderCasts\":6"
          <> ",\"commanderDamage\":{\"7\":8},\"dungeons\":[9,11],\"completedDungeons\":10} "
      )
  Spec.it s "has a schema" $
    Common.assertHasSchema s Player.codec
