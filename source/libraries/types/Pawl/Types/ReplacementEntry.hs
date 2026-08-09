module Pawl.Types.ReplacementEntry where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

-- | CR 616.1: ONE CANDIDATE of the choice of which applicable replacement or
-- prevention effect to apply next. Pawl.Types.ReplacementCandidate is the
-- engine's own record of such an instance; this is the part of it the player
-- being asked is shown.
--
-- Two fields of that record's six, and the obligation the choice puts on the
-- pair is that EQUAL ENTRIES IMPLY INTERCHANGEABLE CANDIDATES -- interchangeable
-- being Pawl.Engine.Replacement.choose's own `distinguishing`. The four dropped
-- fields each satisfy it:
--
--   * `lifetime` (CR 614.3) is Nothing for every permanent-sourced static
--     replacement and baked at install for a floating row, so two candidates
--     alike in `source` and `effect` yet differing here would need ONE object
--     installing two floating rows of one effect with different durations. Brine
--     Elemental beside Savor the Moment -- the CR 614.10a pair `choose`'s comment
--     names -- is two different sources, so `source` already separates it.
--   * `controller` (CR 109.5) is `source`'s current controller for a permanent's
--     static ability and baked at install for a floating row, so it would take
--     one source installing two rows across a control change.
--   * `origin` is constant across the bucket highestBucket already partitioned
--     on, and `identity` is CR 614.5 bookkeeping no player chooses by.
--
-- `effect` is the DISCRIMINATOR (#74): Coldsteel Heart's "This artifact enters
-- tapped" and "As this artifact enters, choose a color" are two applicable
-- entry replacements on one source, in one CR 616.1e bucket, and without the
-- effect they reach the player as the same entry twice.
--
-- The EFFECT ITSELF rather than an ordinal index into the source's replacements,
-- for TriggerEntry's reasons: a card printing the same replacement twice must
-- offer interchangeable entries, which value equality gives and an ordinal does
-- not, and there is no one list to index into -- a candidate reaches the loop
-- from a permanent's static ability, from rule 702's minting, from CR 306.5b's
-- intrinsic loyalty, or from the floating store, and the last is not a
-- replacement "of" the source at all.
--
-- The converse -- two INTERCHANGEABLE candidates from different sources showing
-- as different entries -- is harmless and is what OrderTriggers already does:
-- the prompt is raised at all only when some other pair in the list differs.
data ReplacementEntry = MkReplacementEntry
  { source :: ObjectId.ObjectId,
    effect :: ReplacementEffect.ReplacementEffect
  }
  deriving (Eq, Ord, Show)
