module Pawl.Engine.Target where

import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Card (Card)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.ModeIndex (ModeIndex)
import qualified Pawl.Types.ModeIndex as ModeIndex
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.ProjectedCharacteristics as PC
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.TargetSpec (TargetSpec)
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- CR 115: a target slot's legal recipients are its Pool's base candidate set
-- (CR 115.4's "any target" is creatures on the battlefield plus players still in
-- the game; the planeswalkers and battles that rule also names are not admitted,
-- #494 and #302) narrowed by its Filter (a
-- bare "target creature" carries Nothing and narrows nothing). No restriction
-- (protection, hexproof, shroud) exists in the pool -- this function is where
-- they will all land.
--
-- The two frames are SEPARATE, and keeping them apart is the whole point:
--
--   * `source` is the object the targeting is relative to -- the spell object at
--     cast, the source permanent for an ability. It frames only CR 601.2c's
--     "another", carried as the Filter's own Not IsSource (#163), so that drops
--     whichever tag the Pool produced and re-validation sees the same rule
--     selection did.
--   * `perspective` is CR 109.5's "you", supplied by the caller. "A creature an
--     opponent controls" is a ControlledBy Opponent filter, and a player
--     candidate is narrowed by the same fold through the IsPlayer atom (#168).
--
-- They were one frame once, and splitting them is the fix. Deriving the
-- perspective here as `Projection.controllerOf source` returns Nothing after the
-- source leaves the battlefield, which makes ControlledBy vacuously False and
-- yields the EMPTY set
-- -- so CR 608.2b's re-check found every target illegal and fizzled an ability
-- whose source was merely killed in response. CR 608.2b says the opposite: "If
-- the source of an ability has left the zone it was in, its last known
-- information is used during this process."
--
-- The controller is knowable when the source is not, because an ability is its
-- own object on the stack. Callers on the resolution path read it from that
-- object's stamped owner (CR 113.8: fixed at the ability's creation, never
-- re-derived); callers on the cast/activate path already hold the acting player.
--
-- Maybe, not PlayerId, matching Filter.MkContext's own field: Nothing is a
-- genuinely absent perspective, which leaves a player-referencing filter
-- vacuously False.
legalRecipients :: Maybe PlayerId -> ObjectId -> TargetSpec -> GameState -> Set Recipient
legalRecipients perspective source spec gs =
  let TargetSpec.MkTargetSpec pool restriction = spec
      context = Filter.MkContext perspective (Just source)
      -- ONE whole-board projection and ONE control-grant walk for the whole
      -- slot: both the base pool's creature test and the Filter's per-candidate
      -- view are asked of every object on the battlefield, and each was a fresh
      -- Projection.gather (#200). The hoist Sba.performStateBasedActions takes
      -- for the CR 704.3 sweep, whose stillLegalEnchant haddock argues at length
      -- why re-deriving the board per candidate is the shape to avoid; the
      -- snapshot argument is at Projection.projectGiven, and holds here because
      -- this is a pure function of one GameState.
      --
      -- Thunks, so a slot that asks neither question pays for neither: a
      -- Pool.Players or Pool.Permanents spec with no Filter forces neither, which
      -- is what it cost before.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      keep recipient = case recipient of
        -- CR 115.1: a player candidate is narrowed too ("target opponent"), by a
        -- Filter that asks about the player rather than about an object -- the
        -- IsPlayer atom (#168). Every object-shaped atom is vacuously False
        -- against a player view, so a spec that says "target creature you
        -- control" cannot accidentally admit a player.
        Recipient.ToPlayer pid -> against (Filter.playerView pid)
        Recipient.ToCreature oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
        Recipient.ToObject oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
      against view = case restriction of
        Nothing -> True
        Just f -> Filter.matches context view f
   in Set.filter keep (basePoolGiven pcs pool gs)

-- The closed part: build the pool's base recipient set over zones, tagging each
-- candidate with how it is referenced (CR 115). The per-zone member expressions
-- are exactly those the old per-constructor arms used.
basePool :: Pool.Pool -> GameState -> Set Recipient
basePool pool gs = basePoolGiven (Projection.projectAll gs) pool gs

basePoolGiven :: Map ObjectId PC.ProjectedCharacteristics -> Pool.Pool -> GameState -> Set Recipient
basePoolGiven pcs pool gs = case pool of
  Pool.Creatures -> creatureRecipientsGiven pcs gs
  Pool.Players -> playerRecipients gs
  Pool.AnyTarget -> Set.union (creatureRecipientsGiven pcs gs) (playerRecipients gs)
  Pool.Permanents -> permanentRecipients gs
  Pool.Spells -> spellRecipients gs
  Pool.SpellsAndPermanents -> Set.union (spellRecipients gs) (permanentRecipients gs)

-- CR 115.1a: creatures on the battlefield, per playing player's zone, tagged
-- ToCreature. Reads Projection.isCreatureOf so a permanent made a creature by the
-- layer system (M3c) counts and one that lost the type does not.
creatureRecipients :: GameState -> Set Recipient
creatureRecipients gs = creatureRecipientsGiven (Projection.projectAll gs) gs

creatureRecipientsGiven :: Map ObjectId PC.ProjectedCharacteristics -> GameState -> Set Recipient
creatureRecipientsGiven pcs gs =
  let isCreatureId oid = Projection.isCreatureGiven pcs oid gs
   in Set.fromList
        . fmap Recipient.ToCreature
        $ concatMap
          (filter isCreatureId . (\pid -> Game.zoneMembers Zone.Battlefield pid gs))
          (Game.stillPlaying gs)

-- CR 115: players still in the game, tagged ToPlayer.
playerRecipients :: GameState -> Set Recipient
playerRecipients gs = Set.fromList (fmap Recipient.ToPlayer (Game.stillPlaying gs))

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
stillLegal :: Maybe PlayerId -> ObjectId -> Recipient -> TargetSpec -> GameState -> Bool
stillLegal perspective source recipient spec gs = Set.member recipient (legalRecipients perspective source spec gs)

-- One legal set per named slot; casting prompts with exactly this map. `source`
-- is the object the targeting is relative to -- the spell object at cast, the
-- source permanent for an ability. CR 601.2c's "another" needs no separate pass:
-- a slot that excludes its source says so with Not IsSource, and a slot that
-- does not is untouched, so Prodigal Sorcerer may still target itself with
-- AnyTarget (CR 115.4).
legalSets :: Maybe PlayerId -> ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSets perspective source specs gs = fmap (\spec -> legalRecipients perspective source spec gs) specs

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
fillableModes :: Maybe PlayerId -> ObjectId -> Map SlotName TargetSpec -> Modal.Modal Card -> GameState -> Set ModeIndex
fillableModes perspective source extra modal gs =
  let ms = Foldable.toList (Modal.modes modal)
      fillable i m =
        let sets = legalSets perspective source (Map.union extra (Mode.targetSpecs m)) gs
         in if any Set.null (Map.elems sets)
              then Nothing
              else Just (ModeIndex.MkModeIndex i)
   in Set.fromList (Maybe.mapMaybe (uncurry fillable) (zip [0 :: Natural ..] ms))
