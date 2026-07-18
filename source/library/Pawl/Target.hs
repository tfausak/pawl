module Pawl.Target where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Sba as Sba
import Pawl.Type.GameState (GameState)
import Pawl.Type.Recipient (Recipient)
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone

-- CR 115.4: "any target" is a creature, player, planeswalker, or battle; the
-- last two card types do not exist yet, so this is creatures on the
-- battlefield plus players still in the game. No restriction (protection,
-- hexproof, shroud) exists in the pool -- this function is where they will
-- all land.
legalRecipients :: TargetSpec -> GameState -> Set Recipient
legalRecipients spec gs =
  let isCreatureId oid = fmap Card.isCreature (Game.cardOf oid gs) == Just True
      creatures =
        map Recipient.ToCreature $
          concatMap
            (filter isCreatureId . (\pid -> Game.zoneMembers Zone.Battlefield pid gs))
            (Sba.stillPlaying gs)
      players = map Recipient.ToPlayer (Sba.stillPlaying gs)
   in case spec of
        TargetSpec.AnyTarget -> Set.fromList (creatures ++ players)
        TargetSpec.CreatureTarget -> Set.fromList creatures

-- CR 608.2b: a target that left the zone it was chosen in is illegal (its id
-- names an object that no longer exists, per CR 400.7), and legality is
-- otherwise re-judged against the spec in the current state.
stillLegal :: Recipient -> TargetSpec -> GameState -> Bool
stillLegal recipient spec gs = Set.member recipient (legalRecipients spec gs)

-- One legal set per named slot; casting prompts with exactly this map.
legalSets :: Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSets specs gs = Map.map (\spec -> legalRecipients spec gs) specs
