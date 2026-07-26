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
-- (#85). A player candidate is narrowed by the same fold, through the IsPlayer
-- atom (#168), and inherits that posture: "target opponent" from a source with no
-- projected controller has no legal targets either. CR 601.2c's "another" is applied here too, as the
-- Filter's own Not IsSource (#163) -- so it drops whichever tag the Pool
-- produced, and re-validation sees the same rule selection did.
legalRecipients :: ObjectId -> TargetSpec -> GameState -> Set Recipient
legalRecipients source spec gs =
  let TargetSpec.MkTargetSpec pool restriction = spec
      context = Filter.MkContext (Projection.controllerOf source gs) (Just source)
      keep recipient = case recipient of
        -- CR 115.1: a player candidate is narrowed too ("target opponent"), by a
        -- Filter that asks about the player rather than about an object -- the
        -- IsPlayer atom (#168). Every object-shaped atom is vacuously False
        -- against a player view, so a spec that says "target creature you
        -- control" cannot accidentally admit a player.
        Recipient.ToPlayer pid -> against (Filter.playerView pid)
        Recipient.ToCreature oid -> against (Projection.viewOfObject oid gs)
        Recipient.ToObject oid -> against (Projection.viewOfObject oid gs)
      against view = case restriction of
        Nothing -> True
        Just f -> Filter.matches context view f
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

-- One legal set per named slot; casting prompts with exactly this map. `source`
-- is the object the targeting is relative to -- the spell object at cast, the
-- source permanent for an ability. CR 601.2c's "another" needs no separate pass:
-- a slot that excludes its source says so with Not IsSource, and a slot that
-- does not is untouched, so Prodigal Sorcerer may still target itself with
-- AnyTarget (CR 115.4).
legalSets :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSets source specs gs = fmap (\spec -> legalRecipients source spec gs) specs

-- CR 700.2a: the mode indices all of whose target slots have a legal recipient
-- (a mode with no slots is trivially fillable). Self-exclusion ("another") is
-- honored because it lives in the slot's own Filter, so a mode whose only
-- nonland-permanent target is the source itself is NOT fillable. Shared by
-- spells (Cast) and abilities (Activate/Engine). `extra` is the slots EVERY
-- mode carries in addition to its own -- CR 303.4a's enchant slot, which is
-- declared by the card rather than by a mode, and which castability must see
-- or an Aura with no legal creature would be castable and then countered on
-- resolution (CR 601.2c says it could never have been cast). An ability has no
-- enchant spec and passes Map.empty, which makes that a fact of the call
-- rather than a special case here.
fillableModes :: ObjectId -> Map SlotName TargetSpec -> Modal.Modal Card -> GameState -> Set ModeIndex
fillableModes source extra modal gs =
  let ms = Foldable.toList (Modal.modes modal)
      fillable i m =
        let sets = legalSets source (Map.union extra (Mode.targetSpecs m)) gs
         in if any Set.null (Map.elems sets)
              then Nothing
              else Just (ModeIndex.MkModeIndex (fromIntegral i))
   in Set.fromList (Maybe.mapMaybe (uncurry fillable) (zip [0 :: Int ..] ms))
