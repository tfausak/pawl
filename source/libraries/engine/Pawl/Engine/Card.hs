-- Card-agnostic classifications only (CR 110.1 / 305.6, the closed half). The
-- ~30 hand-written card values moved out to the test suite's Pawl.Cards at M3.5:
-- the engine library can no longer name a card, so §1's invariant (the closed
-- half never depends on a card's identity) is enforced by the module graph.
module Pawl.Engine.Card where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import Pawl.Types.TargetSpec (TargetSpec)
import qualified Pawl.Types.TypeLine as TypeLine

-- Every effect across all of a card's modes, in printed (mode, then written)
-- order. CR 608.2c/700.2: the card's whole text spans its modes; the D4 lint
-- and the text-change scan (M3d) range over all of them regardless of what is
-- chosen.
--
-- Card.mulliganAction is deliberately NOT included: it is not part of the
-- spell, and CR 103.5b's action is performed from the hand rather than cast
-- (#184).
allEffects :: Card.Card -> [Effect Card.Card]
allEffects card = Modal.allEffects (Card.spell card)

-- CR 303.4a: an Aura spell's target is defined by its enchant ability, not by a
-- mode -- an Aura's spell payload is a single empty mode. Merging here is what
-- puts the enchant slot in front of Cast's prompt (Cast.hs) and Resolve's CR
-- 608.2b re-validation (Resolve.hs) without either learning what an Aura is.
--
-- Union is left-biased, and the CardSpec lint holds that no mode declares this
-- slot name, so the bias is never exercised.
--
-- The union of every mode's target specs (slot names are unique across a
-- card's modes by authoring discipline; the D4 lint enforces per-mode
-- resolution).
allTargetSpecs :: Card.Card -> Map SlotName TargetSpec
allTargetSpecs card = Map.union (enchantSpecs card) (Modal.allTargetSpecs (Card.spell card))

-- The target specs of one mode by index (CR 700.2c: only the chosen mode's
-- slots). Nothing if the index is out of range (total). The enchant slot is
-- NOT part of this -- it answers "what does mode i declare", and CR 303.4a's
-- slot is declared by the card, not by any mode.
modeTargetSpecs :: ModeIndex.ModeIndex -> Card.Card -> Maybe (Map SlotName TargetSpec)
modeTargetSpecs idx card = Modal.modeTargetSpecs idx (Card.spell card)

-- CR 608.2c/700.2: the CHOSEN modes only, each with its index, in printed order
-- -- the Set is already sorted by ModeIndex's Ord. Out-of-range indices
-- contribute nothing (total via Seq.lookup). Modes rather than a flat effect
-- list, for the reason Modal.chosenModes gives: the mode is the unit CR 603.5's
-- "may" covers.
chosenModes :: Set.Set ModeIndex.ModeIndex -> Card.Card -> [(ModeIndex.ModeIndex, Mode.Mode Card.Card)]
chosenModes chosen card = Modal.chosenModes chosen (Card.spell card)

-- CR 601.2c/700.2c: the target specs of the CHOSEN modes only (union), plus
-- the card's enchant slot (CR 303.4a) if it has one. Only these slots are
-- prompted at cast and re-validated at CR 608.2b.
modesTargetSpecs :: Set.Set ModeIndex.ModeIndex -> Card.Card -> Map SlotName TargetSpec
modesTargetSpecs chosen card = Map.union (enchantSpecs card) (Modal.modesTargetSpecs chosen (Card.spell card))

isLand :: Card.Card -> Bool
isLand c = Set.member CardType.Land (TypeLine.types (Card.typeLine c))

isCreature :: Card.Card -> Bool
isCreature c = Set.member CardType.Creature (TypeLine.types (Card.typeLine c))

-- CR 304.1: an instant is castable whenever its controller has priority. The
-- timing classification, shaped like isPermanent.
isInstant :: Card.Card -> Bool
isInstant c = Set.member CardType.Instant (TypeLine.types (Card.typeLine c))

-- CR 307.1: a sorcery is cast only in a main phase of its controller's own
-- turn. The other half of the timing classification isInstant is, and the other
-- card type CR 205.4e's casting restriction names.
isSorcery :: Card.Card -> Bool
isSorcery c = Set.member CardType.Sorcery (TypeLine.types (Card.typeLine c))

-- CR 205.4a: does the printed type line carry the "legendary" supertype? The
-- supertype half of the same closed-half classification isInstant is -- rule 205
-- is part of the rulebook, so reading it is no more a case on card identity than
-- isPermanentType's case on a card type. Two rules turn on it: CR 205.4d's
-- legend rule (CR 704.5j, Pawl.Engine.Sba) and CR 205.4e's casting restriction
-- (Pawl.Engine.Cast).
--
-- PRINTED, and only ever asked of a card rather than of a permanent: CR 704.5j's
-- reading has to see a Clone's COPIED supertype, so Pawl.Engine.Sba goes through the
-- projection instead of calling this.
isLegendary :: Card.Card -> Bool
isLegendary c = Set.member Supertype.Legendary (TypeLine.supertypes (Card.typeLine c))

-- | CR 110.4
isPermanentType :: CardType.CardType -> Bool
isPermanentType cardType = case cardType of
  CardType.Artifact -> True
  CardType.Battle -> True
  CardType.Conspiracy -> False
  CardType.Creature -> True
  CardType.Dungeon -> False
  CardType.Enchantment -> True
  CardType.Instant -> False
  CardType.Kindred -> False
  CardType.Land -> True
  CardType.Phenomenon -> False
  CardType.Plane -> False
  CardType.Planeswalker -> True
  CardType.Scheme -> False
  CardType.Sorcery -> False
  CardType.Vanguard -> False

-- The classification resolution dispatches on (CR 608.3). This is the whole
-- reason the engine never needs to know WHICH card is resolving.
isPermanent :: Card.Card -> Bool
isPermanent c = any isPermanentType (Set.toList (TypeLine.types (Card.typeLine c)))

-- CR 205.3h / 303.4: is this card an Aura? A SUBTYPE read off the printed type
-- line, exactly the kind of closed-half classification isPermanent is -- NOT a
-- case on the card's identity. Pawl.Engine.Stack dispatches on it (CR 303.4's "an Aura
-- enters the battlefield attached"), which is the one place the difference
-- between an Aura and any other enchantment is a rules difference.
isAura :: Card.Card -> Bool
isAura c = Set.member Subtype.Aura (TypeLine.subtypes (Card.typeLine c))

-- CR 303.4a: the slot an Aura spell's required target is bound under. A genuine
-- target, so it lives in the ordinary target namespace and is NOT one of
-- Pawl.Engine.Binding's reserved names -- those exist precisely because they are not
-- targets. The CardSpec lint holds that no mode declares this name, which is
-- what makes the merge below collision-free.
enchantSlot :: SlotName
enchantSlot = SlotName.MkSlotName (Text.pack "enchant")

-- CR 303.4a / 702.5a: the enchant ability's target spec as a one-entry slot map,
-- empty for every non-Aura. Merged into the two functions above, and passed to
-- Target.fillableModes by Pawl.Engine.Cast so castability accounts for it.
enchantSpecs :: Card.Card -> Map SlotName TargetSpec
enchantSpecs card = case Card.enchant card of
  Nothing -> Map.empty
  Just spec -> Map.singleton enchantSlot spec
