module Pawl.Target where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Type.CardType as CardType
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.SlotName (SlotName)
import qualified Pawl.Type.Subtype as Subtype
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
  let isCreatureId oid = Projection.isCreatureOf oid gs
      creatures =
        map Recipient.ToCreature $
          concatMap
            (filter isCreatureId . (\pid -> Game.zoneMembers Zone.Battlefield pid gs))
            (Sba.stillPlaying gs)
      players = map Recipient.ToPlayer (Sba.stillPlaying gs)
   in case spec of
        TargetSpec.AnyTarget -> Set.fromList (creatures ++ players)
        TargetSpec.CreatureTarget -> Set.fromList creatures
        TargetSpec.SpellOrPermanentTarget ->
          let onStack = map Recipient.ToObject (GameState.stack gs)
              permanents = map Recipient.ToObject (Set.toList (GameState.battlefield gs))
           in Set.fromList (onStack ++ permanents)
        TargetSpec.LandTarget ->
          let isLand oid = Set.member CardType.Land (Projection.cardTypesOf oid gs)
              lands = filter isLand (Set.toList (GameState.battlefield gs))
           in Set.fromList (map Recipient.ToObject lands)
        TargetSpec.PlayerTarget -> Set.fromList players
        TargetSpec.CreatureOrEnchantmentTarget ->
          let ok oid =
                let ts = Projection.cardTypesOf oid gs
                 in Set.member CardType.Creature ts || Set.member CardType.Enchantment ts
              matches = filter ok (Set.toList (GameState.battlefield gs))
           in Set.fromList (map Recipient.ToObject matches)
        TargetSpec.SpellTarget ->
          -- CR 112.1: only spells (Source.OfCard) on the stack; abilities and
          -- permanents are excluded by Game.isSpell.
          Set.fromList (map Recipient.ToObject (filter (\oid -> Game.isSpell oid gs) (GameState.stack gs)))
        TargetSpec.WallTarget ->
          -- CR 115.1a / 700.2c: "target Wall" is CreatureTarget's set narrowed to
          -- creatures whose PROJECTED subtypes (M3c) include Wall (CR 205.3m) --
          -- a creature's subtypes can change under the layer system, so this reads
          -- the projection, never Card.typeLine directly.
          let isWallCreature recipient = case recipient of
                Recipient.ToCreature oid -> Set.member Subtype.Wall (Projection.subtypesOf oid gs)
                Recipient.ToPlayer _ -> False
                Recipient.ToObject _ -> False
           in Set.fromList (filter isWallCreature creatures)
        TargetSpec.NonlandPermanentTarget ->
          -- CR 109.2 / 110.4: a battlefield permanent (CR 110.1) is nonland if its
          -- PROJECTED card types (M3c) do not include Land -- source-blind (the
          -- exclusion of the targeting source, "another", is applied by
          -- legalSetsExcluding below).
          let notLand oid = not (Set.member CardType.Land (Projection.cardTypesOf oid gs))
              matches = filter notLand (Set.toList (GameState.battlefield gs))
           in Set.fromList (map Recipient.ToObject matches)

-- CR 608.2b: a target that left the zone it was chosen in is illegal (its id
-- names an object that no longer exists, per CR 400.7), and legality is
-- otherwise re-judged against the spec in the current state.
stillLegal :: Recipient -> TargetSpec -> GameState -> Bool
stillLegal recipient spec gs = Set.member recipient (legalRecipients spec gs)

-- One legal set per named slot; casting prompts with exactly this map.
legalSets :: Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSets specs gs = Map.map (\spec -> legalRecipients spec gs) specs

-- CR "another": specs that exclude the targeting source from their legal set.
-- Only NonlandPermanentTarget so far; a general per-slot "another" flag is future.
selfExcludes :: TargetSpec -> Bool
selfExcludes spec = case spec of
  TargetSpec.NonlandPermanentTarget -> True
  TargetSpec.AnyTarget -> False
  TargetSpec.CreatureTarget -> False
  TargetSpec.SpellOrPermanentTarget -> False
  TargetSpec.LandTarget -> False
  TargetSpec.PlayerTarget -> False
  TargetSpec.CreatureOrEnchantmentTarget -> False
  TargetSpec.SpellTarget -> False
  TargetSpec.WallTarget -> False

-- legalSets, then drop the source recipient from each self-excluding slot (CR
-- "another"). `source` is the object the targeting is relative to -- the spell
-- object at cast, the source permanent for an ability. A no-op for every non-self-
-- excluding spec (the source recipient is not removed), so Prodigal Sorcerer may
-- still target itself with AnyTarget (CR 115.4). stillLegal (re-validation) stays
-- source-blind: the chosen target is never the source, so it needs no exclusion.
legalSetsExcluding :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSetsExcluding source specs gs =
  let drop1 spec s = if selfExcludes spec then Set.delete (Recipient.ToObject source) s else s
   in Map.map (\spec -> drop1 spec (legalRecipients spec gs)) specs
