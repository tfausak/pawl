module Pawl.Types.ForEach where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2f's per-object loop: the objects and players the ObjectRef names,
-- each taken in turn with the body run once for it -- Soulfire Eruption's "for
-- each of them, exile the top card of your library, then ... deals damage equal
-- to that card's mana value to that permanent or player".
--
-- Parametric in the EFFECT for Pawl.Types.PreventNextDamage's reason: the
-- record holds effects and Pawl.Types.Effect holds the record, so naming Effect
-- here would need an hs-boot file. Instantiated at `Effect card` where the arm
-- is declared.
data ForEach effect = MkForEach
  { -- | The set swept ONCE, before the first iteration -- CR 608.2f's "which
    -- objects" half, the same read every other ObjectRef-taking opcode makes.
    -- Nothing the body does adds to or removes from it.
    ref :: ObjectRef.ObjectRef,
    -- | The name this iteration's member is bound under, for the body to read
    -- as an ObjectRef.InSlot or a PlayerRef.InSlot. A DEFINITION, never a
    -- target (CR 115.10a): the ref above may well have been filled by targeting
    -- and carries CR 608.2b's re-validation, but this name is the loop's, not
    -- the card's announcement.
    slot :: SlotName.SlotName,
    -- | The instructions run once per member, in written order (CR 608.2c). A
    -- SEQUENCE is the whole point: an opcode naming a set applies ITSELF across
    -- it, where this applies a list of them to each member in turn, so a later
    -- instruction can act on what an earlier one produced FOR THAT MEMBER.
    body :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
