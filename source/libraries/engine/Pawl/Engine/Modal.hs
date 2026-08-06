-- Mode-scoped structural reads over a Modal payload (CR 700.2), shared by the
-- spell (Face.spell) and both ability types. Parametric in `card` and importing
-- only Type modules and Pawl.Extra, so there is no cycle -- Pawl.Engine.Card
-- imports THIS.
module Pawl.Engine.Modal where

import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.TargetSpec (TargetSpec)

-- Each mode's effects, in printed (mode, then written) order (CR 608.2c) -- one
-- inner list per mode, kept apart. The shape a caller wants when the MODE is
-- the unit of choice but resolution is not in play, where flattening would fuse
-- two alternatives into one. selectionEffects just below is what CR 700.2's
-- selection makes of it, and is what Pawl.Engine.Mana reads.
modeEffects :: Modal.Modal card -> [[Effect card]]
modeEffects m = fmap (Foldable.toList . Mode.effects) (Foldable.toList (Modal.modes m))

-- Every effect across all modes, printed (mode, then written) order (CR
-- 608.2c).
allEffects :: Modal.Modal card -> [Effect card]
allEffects m = concat (modeEffects m)

-- CR 700.2/700.2a: every way this payload's SELECTION could be satisfied -- one
-- inner list per legal set of modes, each already flattened into printed (mode,
-- then written) order. The shape a caller wants when the unit of choice is the
-- whole selection rather than a single mode: Pawl.Engine.Mana reads it to
-- enumerate the ways one mana ability could be activated, where a choose-two
-- ability's option is a PAIR of modes adding both modes' mana.
--
-- Reduces to modeEffects when the selection is "choose exactly one", which is
-- what every printed mana ability asks. Targeting is not consulted, so CR
-- 700.2a's "if one of the modes would be illegal ... that mode can't be chosen"
-- does not narrow the list here; the one caller is Pawl.Engine.Mana, and CR
-- 605.1a already keeps a targeting ability from being a mana ability at all.
selectionEffects :: Modal.Modal card -> [[Effect card]]
selectionEffects m = fmap concat (combinations (selectionCount m) (modeEffects m))

-- CR 700.2d: "If a player is allowed to choose more than one mode for a modal
-- spell or ability, that player normally can't choose the same mode more than
-- once." So these are the size-n SUBLISTS, taken without repetition and keeping
-- the printed order -- not the size-n sequences.
--
-- Empty when the list is shorter than n, since no selection satisfies such a
-- payload. The "You may choose the same mode more than once" exception CR 700.2d
-- goes on to name is not modelled (#791).
combinations :: Natural -> [a] -> [[a]]
combinations n xs
  | n == 0 = [[]]
  | otherwise = case xs of
      [] -> []
      h : t -> fmap (h :) (combinations (n - 1) t) <> combinations n t

-- The union of every mode's target specs. Slot names are unique across a card's
-- modes, and that is CHECKED rather than merely intended: a CardSpec lint
-- rejects the card whose two modes declare one name, which this union would
-- otherwise collapse into a single entry.
allTargetSpecs :: Modal.Modal card -> Map SlotName TargetSpec
allTargetSpecs m = Map.unions (fmap Mode.targetSpecs (Foldable.toList (Modal.modes m)))

-- CR 608.2c/700.2: the CHOSEN modes themselves, each with its own index, in
-- ModeIndex order (the Set is already sorted). Out-of-range indices contribute
-- nothing (total via Seq.lookup).
--
-- The ORDER is what CR 702.42b demands of an entwined spell, and it costs
-- nothing extra: ModeIndex order IS printed order, and Set.toAscList is already
-- sorted.
--
-- Modes rather than a flat effect list because a mode is the unit CR 603.5's
-- "may" covers (Mode.optionality) and the unit CR 700.2c scopes targets to, so
-- a resolver that flattened first could not ask the one question per mode that
-- Resolve.resolveModes asks. The index rides along so the prompt can name which
-- mode is asking.
chosenModes :: Set ModeIndex.ModeIndex -> Modal.Modal card -> [(ModeIndex.ModeIndex, Mode.Mode card)]
chosenModes chosen m =
  let modeAt idx@(ModeIndex.MkModeIndex n) =
        fmap ((,) idx) (Seq.lookup (Natural.toIntSaturating n) (Modal.modes m))
   in Maybe.mapMaybe modeAt (Set.toAscList chosen)

-- CR 608.2c/700.2: only the CHOSEN modes' effects, flattened in ModeIndex then
-- written order. A CLASSIFICATION read (does any of this search a library?),
-- where optionality is not part of the question; resolution goes through
-- chosenModes instead.
modesEffects :: Set ModeIndex.ModeIndex -> Modal.Modal card -> [Effect card]
modesEffects chosen m = concatMap (Foldable.toList . Mode.effects . snd) (chosenModes chosen m)

-- CR 601.2c/700.2c: only the CHOSEN modes' target specs (union). Two modes may
-- be chosen at once (CR 702.42a's entwine), which is what makes this union's
-- collapse an observable wrong answer rather than a latent one -- so the
-- CardSpec lint keeps a card whose modes share a slot name out of the pool.
modesTargetSpecs :: Set ModeIndex.ModeIndex -> Modal.Modal card -> Map SlotName TargetSpec
modesTargetSpecs chosen m =
  let specsAt (ModeIndex.MkModeIndex n) =
        maybe Map.empty Mode.targetSpecs (Seq.lookup (Natural.toIntSaturating n) (Modal.modes m))
   in Map.unions (fmap specsAt (Set.toAscList chosen))

-- The target specs of one mode by index (CR 700.2c: only the chosen mode's
-- slots). Nothing if the index is out of range (total).
modeTargetSpecs :: ModeIndex.ModeIndex -> Modal.Modal card -> Maybe (Map SlotName TargetSpec)
modeTargetSpecs (ModeIndex.MkModeIndex n) m =
  fmap Mode.targetSpecs (Seq.lookup (Natural.toIntSaturating n) (Modal.modes m))

-- CR 700.2: how many modes the selection demands (the ChooseExactly count).
selectionCount :: Modal.Modal card -> Natural
selectionCount m = case Modal.selection m of
  ModeSelection.ChooseExactly n -> n

-- How many modes are PRINTED, which is not the same question as how many the
-- selection demands. CR 702.42a's entwine is what asks it: this is the count
-- Pawl.Engine.Cast substitutes for selectionCount when the entwine cost is
-- paid.
modeCount :: Modal.Modal card -> Natural
modeCount = Natural.length . Modal.modes
