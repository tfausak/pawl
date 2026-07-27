-- Mode-scoped structural reads over a Modal payload (CR 700.2), shared by the
-- spell (Card.spell) and both ability types. Card-free/parametric in `card` (M4c):
-- imports only Type modules and Pawl.Extra (a leaf of the module graph: it
-- imports nothing from Pawl outside itself), so no cycle -- Pawl.Card imports
-- THIS.
module Pawl.Modal where

import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Natural as Natural
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)

-- Every effect across all modes, printed (mode, then written) order (CR 608.2c).
allEffects :: Modal.Modal card -> [Effect card]
allEffects m = concatMap (Foldable.toList . Mode.effects) (Modal.modes m)

-- The union of every mode's target specs (slot names unique by authoring
-- discipline; the D4 lint enforces per-mode resolution).
allTargetSpecs :: Modal.Modal card -> Map SlotName TargetSpec
allTargetSpecs m = Map.unions (fmap Mode.targetSpecs (Foldable.toList (Modal.modes m)))

-- CR 608.2c/700.2: only the CHOSEN modes' effects, in ModeIndex order (the Set is
-- already sorted). Out-of-range indices contribute nothing (total via Seq.lookup).
modesEffects :: Set ModeIndex.ModeIndex -> Modal.Modal card -> [Effect card]
modesEffects chosen m =
  let modeAt (ModeIndex.MkModeIndex n) =
        foldMap (Foldable.toList . Mode.effects) (Seq.lookup (Natural.toIntSaturating n) (Modal.modes m))
   in concatMap modeAt (Set.toAscList chosen)

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
