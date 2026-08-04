-- Card-agnostic classifications only (CR 110.1 / 305.6, the closed half). The
-- engine library cannot name a card -- the hand-written card values live in the
-- test suite's Pawl.Cards -- so design.md §1's invariant is enforced by the
-- module graph.
--
-- Also where a Card is resolved to the Face whose characteristics are live,
-- since CR 709.4 / 712.8a / 715.4 make that a question about the card's layout
-- and never about which card it is.
module Pawl.Engine.Card where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import Pawl.Types.TargetSpec (TargetSpec)
import qualified Pawl.Types.TypeLine as TypeLine

-- CR 709.4 / 712.8a / 715.4: the characteristics a card has where the rules do
-- not single out one face. Under Normal that is the sole face; the layouts that
-- genuinely combine get their arm when they land.
--
-- TOTAL, which is what Card.faces being NonEmpty buys: every characteristic read
-- in the engine funnels through here, and a Maybe would spread to all of them.
combined :: Card.Card -> Face.Face Card.Card
combined card = case Card.layout card of
  Layout.Normal -> NonEmpty.head (Card.faces card)

-- The face of this card with the given name, if it has one. CR 709.4a: a card's
-- faces are referred to BY NAME, which is what a player names in paper and what
-- survives in a DecisionLog; the Ord on Card.faces is printed order and carries
-- no identity.
--
-- Nothing when no face is so named. The Pawl.CardSpec corpus lint holds that a
-- card's face names are pairwise distinct, so a hit is unique.
faceNamed :: CardName.CardName -> Card.Card -> Maybe (Face.Face Card.Card)
faceNamed n card = List.find (\f -> Face.name f == n) (NonEmpty.toList (Card.faces card))

-- Every effect across all of a face's modes, in printed (mode, then written)
-- order. CR 608.2c/700.2: the face's whole text spans its modes, and both the
-- dataflow lint and the text-change scan range over all of them regardless of
-- what is chosen.
--
-- Face.mulliganAction is deliberately NOT included: it is not part of the
-- spell, and CR 103.5b's action is performed from the hand rather than cast
-- (#184).
allEffects :: Face.Face Card.Card -> [Effect Card.Card]
allEffects face = Modal.allEffects (Face.spell face)

-- The union of every mode's target specs, plus the enchant slot. CR 303.4a: an
-- Aura spell's target is defined by its enchant ability rather than by a mode,
-- and merging here is what puts that slot in front of Cast's prompt and
-- Resolve's CR 608.2b re-validation without either learning what an Aura is.
--
-- Union is left-biased, and the CardSpec lint holds that no mode declares this
-- slot name, so the bias is never exercised.
allTargetSpecs :: Face.Face Card.Card -> Map SlotName TargetSpec
allTargetSpecs face = Map.union (enchantSpecs face) (Modal.allTargetSpecs (Face.spell face))

-- The target specs of one mode by index (CR 700.2c: only the chosen mode's
-- slots). Nothing if the index is out of range (total). The enchant slot is
-- NOT part of this -- it answers "what does mode i declare", and CR 303.4a's
-- slot is declared by the card, not by any mode.
modeTargetSpecs :: ModeIndex.ModeIndex -> Face.Face Card.Card -> Maybe (Map SlotName TargetSpec)
modeTargetSpecs idx face = Modal.modeTargetSpecs idx (Face.spell face)

-- CR 608.2c/700.2: the CHOSEN modes only, each with its index, in printed order
-- -- the Set is already sorted by ModeIndex's Ord. Out-of-range indices
-- contribute nothing (total via Seq.lookup). Modes rather than a flat effect
-- list, for the reason Modal.chosenModes gives: the mode is the unit CR 603.5's
-- "may" covers.
chosenModes :: Set.Set ModeIndex.ModeIndex -> Face.Face Card.Card -> [(ModeIndex.ModeIndex, Mode.Mode Card.Card)]
chosenModes chosen face = Modal.chosenModes chosen (Face.spell face)

-- CR 601.2c/700.2c: the target specs of the CHOSEN modes only (union), plus
-- the card's enchant slot (CR 303.4a) if it has one. Only these slots are
-- prompted at cast and re-validated at CR 608.2b.
modesTargetSpecs :: Set.Set ModeIndex.ModeIndex -> Face.Face Card.Card -> Map SlotName TargetSpec
modesTargetSpecs chosen face = Map.union (enchantSpecs face) (Modal.modesTargetSpecs chosen (Face.spell face))

isLand :: Face.Face Card.Card -> Bool
isLand f = Set.member CardType.Land (TypeLine.types (Face.typeLine f))

isCreature :: Face.Face Card.Card -> Bool
isCreature f = Set.member CardType.Creature (TypeLine.types (Face.typeLine f))

-- CR 304.1: an instant is castable whenever its controller has priority. The
-- timing classification, shaped like isPermanent.
isInstant :: Face.Face Card.Card -> Bool
isInstant f = Set.member CardType.Instant (TypeLine.types (Face.typeLine f))

-- CR 307.1: a sorcery is cast only in a main phase of its controller's own
-- turn. The other half of the timing classification isInstant is, and the other
-- card type CR 205.4e's casting restriction names.
isSorcery :: Face.Face Card.Card -> Bool
isSorcery f = Set.member CardType.Sorcery (TypeLine.types (Face.typeLine f))

-- CR 205.4a: does the printed type line carry the "legendary" supertype? The
-- supertype half of the same closed-half classification isInstant is. Two rules
-- turn on it: CR 205.4d's legend rule (CR 704.5j, Pawl.Engine.Sba) and CR
-- 205.4e's casting restriction (Pawl.Engine.Cast).
--
-- PRINTED, and only ever asked of a face rather than of a permanent: CR
-- 704.5j's reading has to see a Clone's COPIED supertype, so Sba goes through
-- the projection instead of calling this.
isLegendary :: Face.Face Card.Card -> Bool
isLegendary f = Set.member Supertype.Legendary (TypeLine.supertypes (Face.typeLine f))

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
isPermanent :: Face.Face Card.Card -> Bool
isPermanent f = any isPermanentType (Set.toList (TypeLine.types (Face.typeLine f)))

-- CR 205.3h / 303.4: is this face an Aura? A SUBTYPE read off the printed type
-- line, the same kind of closed-half classification isPermanent is -- NOT a
-- case on the card's identity. Pawl.Engine.Stack dispatches on it, which is the
-- one place an Aura differs from any other enchantment by a rule.
isAura :: Face.Face Card.Card -> Bool
isAura f = Set.member Subtype.Aura (TypeLine.subtypes (Face.typeLine f))

-- CR 303.4a: the slot an Aura spell's required target is bound under. A genuine
-- target, so it lives in the ordinary target namespace rather than among
-- Pawl.Engine.Binding's reserved names. The CardSpec lint holds that no mode
-- declares this name, which is what makes the merge above collision-free.
enchantSlot :: SlotName
enchantSlot = SlotName.MkSlotName (Text.pack "enchant")

-- CR 303.4a / 702.5a: the enchant ability's target spec as a one-entry slot
-- map, empty for every non-Aura. Merged into the two functions above, and
-- passed to Target.fillableModes by Pawl.Engine.Cast so castability accounts
-- for it.
enchantSpecs :: Face.Face Card.Card -> Map SlotName TargetSpec
enchantSpecs face = case Face.enchant face of
  Nothing -> Map.empty
  Just spec -> Map.singleton enchantSlot spec
