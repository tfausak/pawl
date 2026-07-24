module Pawl.Target where

import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Departure as Departure
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import Pawl.Type.Card (Card)
import qualified Pawl.Type.Exclusion as Exclusion
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import Pawl.Type.ModeIndex (ModeIndex)
import qualified Pawl.Type.ModeIndex as ModeIndex
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Pool as Pool
import Pawl.Type.Recipient (Recipient)
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone

-- CR 115: a target slot's legal recipients are its Pool's base candidate set
-- (CR 115.4's "any target" is creatures on the battlefield plus players still in
-- the game; planeswalkers/battles do not exist yet) narrowed by its Filter (a
-- bare "target creature" carries Nothing and narrows nothing). No restriction
-- (protection, hexproof, shroud) exists in the pool -- this function is where
-- they will all land.
--
-- `source` is the object the targeting is relative to: the spell object at cast,
-- the source permanent for an ability. It frames the Filter's perspective (CR
-- 109.5): "a creature an opponent controls" is a ControlledBy Opponent filter,
-- and a source that has left the battlefield has no projected controller, which
-- yields Nothing and matches nothing -- the EMPTY set, not last known information
-- (#85). A Filter ranges only over objects, never players, so a ToPlayer
-- recipient is never narrowed. Self-exclusion ("another") is NOT applied here;
-- that stays in legalSetsExcluding.
legalRecipients :: ObjectId -> TargetSpec -> GameState -> Set Recipient
legalRecipients source spec gs =
  let TargetSpec.MkTargetSpec pool restriction _ = spec
      context = Filter.MkContext (Projection.controllerOf source gs)
      keep recipient = case recipient of
        Recipient.ToPlayer _ -> True -- CR 115: a Filter ranges over objects; it never narrows a player.
        Recipient.ToCreature oid -> narrows oid
        Recipient.ToObject oid -> narrows oid
      narrows oid = case restriction of
        Nothing -> True
        Just f -> Filter.matches context (Projection.viewOfObject oid gs) f
   in Set.filter keep (basePool pool gs)

-- The closed part: build the pool's base recipient set over zones, tagging each
-- candidate with how it is referenced (CR 115). The per-zone member expressions
-- are exactly those the old per-constructor arms used.
basePool :: Pool.Pool -> GameState -> Set Recipient
basePool pool gs = case pool of
  Pool.Creatures -> creatureRecipients gs
  Pool.Players -> playerRecipients gs
  Pool.AnyTarget -> Set.union (creatureRecipients gs) (playerRecipients gs)
  Pool.Permanents -> permanentRecipients gs
  Pool.Spells -> spellRecipients gs
  Pool.SpellsAndPermanents -> Set.union (spellRecipients gs) (permanentRecipients gs)

-- CR 115.1a: creatures on the battlefield, per playing player's zone, tagged
-- ToCreature. Reads Projection.isCreatureOf so a permanent made a creature by the
-- layer system (M3c) counts and one that lost the type does not.
creatureRecipients :: GameState -> Set Recipient
creatureRecipients gs =
  let isCreatureId oid = Projection.isCreatureOf oid gs
   in Set.fromList
        . fmap Recipient.ToCreature
        $ concatMap
          (filter isCreatureId . (\pid -> Game.zoneMembers Zone.Battlefield pid gs))
          (Departure.stillPlaying gs)

-- CR 115: players still in the game, tagged ToPlayer.
playerRecipients :: GameState -> Set Recipient
playerRecipients gs = Set.fromList (fmap Recipient.ToPlayer (Departure.stillPlaying gs))

-- CR 110.1: permanents on the battlefield, tagged ToObject.
permanentRecipients :: GameState -> Set Recipient
permanentRecipients gs = Set.fromList (fmap Recipient.ToObject (Set.toList (GameState.battlefield gs)))

-- CR 112.1: only spells (Source.OfCard) on the stack, tagged ToObject; abilities
-- and permanents are excluded by Game.isSpell.
spellRecipients :: GameState -> Set Recipient
spellRecipients gs = Set.fromList (fmap Recipient.ToObject (filter (\oid -> Game.isSpell oid gs) (GameState.stack gs)))

-- CR 608.2b: a target that left the zone it was chosen in is illegal (its id
-- names an object that no longer exists, per CR 400.7), and legality is
-- otherwise re-judged against the spec in the current state.
stillLegal :: ObjectId -> Recipient -> TargetSpec -> GameState -> Bool
stillLegal source recipient spec gs = Set.member recipient (legalRecipients source spec gs)

-- One legal set per named slot; casting prompts with exactly this map.
legalSets :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSets source specs gs = fmap (\spec -> legalRecipients source spec gs) specs

-- CR 601.2c "another": a slot excludes the targeting source from its legal set
-- iff its Exclusion says so ("another nonland permanent", Aether Channeler).
selfExcludes :: TargetSpec -> Bool
selfExcludes spec =
  let TargetSpec.MkTargetSpec _ _ exclusion = spec
   in case exclusion of
        Exclusion.ExcludesSource -> True
        Exclusion.IncludesSource -> False

-- legalSets, then drop the source recipient from each self-excluding slot (CR
-- "another"). `source` is the object the targeting is relative to -- the spell
-- object at cast, the source permanent for an ability. A no-op for every non-self-
-- excluding spec (the source recipient is not removed), so Prodigal Sorcerer may
-- still target itself with AnyTarget (CR 115.4). stillLegal (re-validation) stays
-- source-blind: the chosen target is never the source, so it needs no exclusion.
legalSetsExcluding :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSetsExcluding source specs gs =
  let drop1 spec s = if selfExcludes spec then Set.delete (Recipient.ToObject source) s else s
   in fmap (\spec -> drop1 spec (legalRecipients source spec gs)) specs

-- CR 700.2a: the mode indices all of whose target slots have a legal recipient
-- (a mode with no slots is trivially fillable). Self-exclusion ("another") is
-- honored via legalSetsExcluding, so a mode whose only nonland-permanent target
-- is the source itself is NOT fillable. Shared by spells (Cast) and abilities
-- (Activate/Engine).
fillableModes :: ObjectId -> Modal.Modal Card -> GameState -> Set ModeIndex
fillableModes source modal gs =
  let ms = Foldable.toList (Modal.modes modal)
      fillable i m =
        let sets = legalSetsExcluding source (Mode.targetSpecs m) gs
         in if any Set.null (Map.elems sets)
              then Nothing
              else Just (ModeIndex.MkModeIndex (fromIntegral i))
   in Set.fromList (Maybe.mapMaybe (uncurry fillable) (zip [0 :: Int ..] ms))
