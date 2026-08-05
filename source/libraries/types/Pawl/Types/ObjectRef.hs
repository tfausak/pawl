module Pawl.Types.ObjectRef where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | WHICH OBJECTS an object-affecting effect names -- the object-side counterpart
-- of Pawl.Types.PlayerRef.
--
-- The two arms differ in whether the objects were named BEFORE the effect runs
-- -- a slot, filled at cast or as the ability was placed -- or are found AS it
-- runs. That is the distinction CR 115.10a draws: only InSlot can name a target;
-- EachMatching never does.
data ObjectRef
  = -- | The objects bound in a slot (CR 601.2c filled it by targeting, or the
    -- engine reserved it -- Binding.triggerSource). Usually ONE, the slot's
    -- single Recipient, subject to CR 608.2b's illegal-target check when the slot
    -- was a target.
    --
    -- But a slot a Create bound to a whole minted GROUP names every one of them
    -- -- Salt Road Skirmish's "they gain haste until end of turn" over the two
    -- tokens the sentence before it made. A group is a definition and never a
    -- target (CR 115.10a), so it owes CR 608.2b nothing; the arm still reads at
    -- most one object per slot for every OTHER kind of binding, which is why
    -- "target creature" cannot become two.
    InSlot SlotName.SlotName
  | -- | Every PERMANENT ON THE BATTLEFIELD matching the Filter -- Day of
    -- Judgment's "all creatures". The battlefield is where CR 109.2 puts it; a
    -- set drawn from any other zone has no card in the pool (#376).
    --
    -- Not a target and never one (CR 115.10a), so CR 608.2b has nothing to
    -- fizzle. The set is swept when the effect executes (CR 608.2c) and is then
    -- fixed for that instruction; judging which swept objects are affected
    -- before any of them is (CR 608.2f) belongs to the opcode's funnel rather
    -- than to this type.
    --
    -- A CONTINUOUS effect over a set must additionally freeze the swept set into
    -- the effect itself (CR 611.2c), storing Affected.TheseObjects; the one-shots
    -- that take this type store nothing.
    EachMatching (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
