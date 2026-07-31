-- Mode-scoped structural reads over a Modal payload (CR 700.2), shared by the
-- spell (Card.spell) and both ability types. Card-free/parametric in `card` (M4c):
-- imports only Type modules and Pawl.Extra (a leaf of the module graph: it
-- imports nothing from Pawl outside itself), so no cycle -- Pawl.Card imports
-- THIS.
module Pawl.Modal where

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
-- inner list per mode, kept apart. The shape a caller wants when the MODE is the
-- unit of choice but resolution is not in play: Pawl.Mana reads it to enumerate
-- the ways one mana ability could be activated (CR 700.2), where flattening
-- would fuse two alternatives into one yield.
modeEffects :: Modal.Modal card -> [[Effect card]]
modeEffects m = fmap (Foldable.toList . Mode.effects) (Foldable.toList (Modal.modes m))

-- Every effect across all modes, printed (mode, then written) order (CR 608.2c).
allEffects :: Modal.Modal card -> [Effect card]
allEffects m = concat (modeEffects m)

-- The union of every mode's target specs (slot names unique by authoring
-- discipline; the D4 lint enforces per-mode resolution).
allTargetSpecs :: Modal.Modal card -> Map SlotName TargetSpec
allTargetSpecs m = Map.unions (fmap Mode.targetSpecs (Foldable.toList (Modal.modes m)))

-- CR 608.2c/700.2: the CHOSEN modes themselves, each with its own index, in
-- ModeIndex order (the Set is already sorted). Out-of-range indices contribute
-- nothing (total via Seq.lookup).
--
-- Modes rather than a flat effect list because a mode is the unit CR 603.5's
-- "may" covers (Mode.optionality) and the unit CR 700.2c scopes targets to, so a
-- resolver that flattened first could not ask the one question per mode that
-- Pawl.Resolve.resolveModes asks. The index rides along so the prompt can name
-- which mode is asking.
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

-- CR 601.2c/700.2c: only the CHOSEN modes' target specs (union).
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
