-- Card-agnostic classifications only (CR 110.1 / 305.6, the closed half). The
-- ~30 hand-written card values moved out to the test suite's Pawl.Cards at M3.5:
-- the engine library can no longer name a card, so §1's invariant (the closed
-- half never depends on a card's identity) is enforced by the module graph.
module Pawl.Card where

import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)
import qualified Pawl.Type.TypeLine as TypeLine

-- Every effect across all of a card's modes, in printed (mode, then written)
-- order. CR 608.2c/700.2: the card's whole text spans its modes; the D4 lint
-- and the text-change scan (M3d) range over all of them regardless of what is
-- chosen.
allEffects :: Card.Card -> [Effect Card.Card]
allEffects card =
  concatMap (Foldable.toList . Mode.effects) (Modal.modes (Card.spell card))

-- The union of every mode's target specs (slot names are unique across a
-- card's modes by authoring discipline; the D4 lint enforces per-mode
-- resolution).
allTargetSpecs :: Card.Card -> Map SlotName TargetSpec
allTargetSpecs card =
  Map.unions (map Mode.targetSpecs (Foldable.toList (Modal.modes (Card.spell card))))

-- The target specs of one mode by index (CR 700.2c: only the chosen mode's
-- slots). Nothing if the index is out of range (total).
modeTargetSpecs :: ModeIndex.ModeIndex -> Card.Card -> Maybe (Map SlotName TargetSpec)
modeTargetSpecs (ModeIndex.MkModeIndex n) card =
  fmap Mode.targetSpecs (Seq.lookup (fromIntegral n) (Modal.modes (Card.spell card)))

-- CR 608.2c/700.2: the effects of the CHOSEN modes only, in printed (mode
-- index, then written) order -- the Set is already sorted by ModeIndex's Ord.
-- Out-of-range indices contribute nothing (total via Seq.lookup).
modesEffects :: Set.Set ModeIndex.ModeIndex -> Card.Card -> [Effect Card.Card]
modesEffects chosen card =
  let ms = Modal.modes (Card.spell card)
      modeAt (ModeIndex.MkModeIndex n) = maybe [] (Foldable.toList . Mode.effects) (Seq.lookup (fromIntegral n) ms)
   in concatMap modeAt (Set.toAscList chosen)

-- CR 601.2c/700.2c: the target specs of the CHOSEN modes only (union). Only
-- these slots are prompted at cast and re-validated at CR 608.2b.
modesTargetSpecs :: Set.Set ModeIndex.ModeIndex -> Card.Card -> Map SlotName TargetSpec
modesTargetSpecs chosen card =
  let ms = Modal.modes (Card.spell card)
      specsAt (ModeIndex.MkModeIndex n) = maybe Map.empty Mode.targetSpecs (Seq.lookup (fromIntegral n) ms)
   in Map.unions (map specsAt (Set.toAscList chosen))

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
