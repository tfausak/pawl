-- Card-agnostic classifications only (CR 110.1 / 305.6, the closed half). The
-- ~30 hand-written card values moved out to the test suite's Pawl.Cards at M3.5:
-- the engine library can no longer name a card, so §1's invariant (the closed
-- half never depends on a card's identity) is enforced by the module graph.
module Pawl.Card where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Modal as Modal
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.ModeIndex as ModeIndex
import Pawl.Type.SlotName (SlotName)
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import Pawl.Type.TargetSpec (TargetSpec)
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TypeLine as TypeLine

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

-- CR 608.2c/700.2: the effects of the CHOSEN modes only, in printed (mode
-- index, then written) order -- the Set is already sorted by ModeIndex's Ord.
-- Out-of-range indices contribute nothing (total via Seq.lookup).
modesEffects :: Set.Set ModeIndex.ModeIndex -> Card.Card -> [Effect Card.Card]
modesEffects chosen card = Modal.modesEffects chosen (Card.spell card)

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

-- CR 110.1: the permanent card types. An enumeration -- closed half, finite.
isPermanentType :: CardType.CardType -> Bool
isPermanentType cardType = case cardType of
  CardType.Land -> True
  CardType.Creature -> True
  CardType.Instant -> False
  CardType.Enchantment -> True
  CardType.Artifact -> True
  -- CR 307 / 608.3: a sorcery is not a permanent; it goes to the graveyard.
  CardType.Sorcery -> False

-- The classification resolution dispatches on (CR 608.3). This is the whole
-- reason the engine never needs to know WHICH card is resolving.
isPermanent :: Card.Card -> Bool
isPermanent c = any isPermanentType (Set.toList (TypeLine.types (Card.typeLine c)))

-- CR 603.7: every effect across all of a card's DELAYED abilities' modes. The
-- read half of the delayed-ability dataflow lint, as allEffects is for the spell.
delayedEffects :: Card.Card -> [Effect Card.Card]
delayedEffects card = concatMap (Modal.allEffects . TriggeredAbility.modal) (Map.elems (Card.delayedAbilities card))

-- CR 205.3h / 303.4: is this card an Aura? A SUBTYPE read off the printed type
-- line, exactly the kind of closed-half classification isPermanent is -- NOT a
-- case on the card's identity. Pawl.Stack dispatches on it (CR 303.4's "an Aura
-- enters the battlefield attached"), which is the one place the difference
-- between an Aura and any other enchantment is a rules difference.
isAura :: Card.Card -> Bool
isAura c = Set.member Subtype.Aura (TypeLine.subtypes (Card.typeLine c))

-- CR 303.4a: the slot an Aura spell's required target is bound under. A genuine
-- target, so it lives in the ordinary target namespace and is NOT one of
-- Pawl.Binding's reserved names -- those exist precisely because they are not
-- targets. The CardSpec lint holds that no mode declares this name, which is
-- what makes the merge below collision-free.
enchantSlot :: SlotName
enchantSlot = SlotName.MkSlotName (Text.pack "enchant")

-- CR 303.4a / 702.5a: the enchant ability's target spec as a one-entry slot map,
-- empty for every non-Aura. Merged into the two functions above, and passed to
-- Target.fillableModes by Pawl.Cast so castability accounts for it.
enchantSpecs :: Card.Card -> Map SlotName TargetSpec
enchantSpecs card = case Card.enchant card of
  Nothing -> Map.empty
  Just spec -> Map.singleton enchantSlot spec
