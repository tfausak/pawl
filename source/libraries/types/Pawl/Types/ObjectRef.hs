module Pawl.Types.ObjectRef where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | WHICH OBJECTS an object-affecting effect names -- the object-side counterpart
-- of Pawl.Types.PlayerRef, and for the same reason: an opcode that could only
-- name one slot needed a sibling opcode the first time a card named a set, and
-- one opcode is easier to keep correct than two (PlayerRef's own history, via
-- Effect.Draw's comment).
--
-- The two arms differ in whether the objects were named BEFORE the effect runs
-- -- a slot, filled at cast or as the ability was placed -- or are found AS it
-- runs. That is exactly the distinction CR 115.10a draws:
-- "Just because an object or player is being affected by a spell or ability
-- doesn't make that object or player a target of that spell or ability. Unless
-- that object or player is identified by the word 'target' ..., it's not a
-- target." Only InSlot can name a target; EachMatching never does.
data ObjectRef
  = -- | The one object bound in a slot (CR 601.2c filled it by targeting, or the
    -- engine reserved it -- Binding.triggerSource). At most one: a slot holds a
    -- single Recipient. Subject to CR 608.2b's illegal-target check when the
    -- slot was a target.
    InSlot SlotName.SlotName
  | -- | Every PERMANENT ON THE BATTLEFIELD matching the Filter -- Day of
    -- Judgment's "all creatures". The battlefield is where CR 109.2 puts it:
    -- "If a spell or ability uses a description of an object that includes a
    -- card type or subtype, but doesn't refer to a specific zone or include the
    -- word 'card,' 'spell,' 'source,' or 'scheme,' it means a permanent of that
    -- card type or subtype on the battlefield." A set drawn from any other zone
    -- has no card in the pool (#376).
    --
    -- Not a target and never one: there is no TargetSpec, nothing is chosen at
    -- cast, and CR 608.2b therefore has nothing to fizzle (CR 115.10a, above).
    -- The set is swept when the effect executes, per CR 608.2c -- the controller
    -- "follows its instructions in the order written" -- and is then FIXED for
    -- that instruction. That is half of CR 608.2f's "each such action is
    -- processed simultaneously"; the other half is that whether each swept object
    -- is affected must be judged before any of them is, which belongs to the
    -- opcode's funnel rather than to this type (Pawl.Engine.Event.destroy).
    --
    -- A CONTINUOUS effect over a set must additionally freeze the swept set into
    -- the effect itself, because CR 611.2c fixes the affected set "when that
    -- continuous effect begins". Effect.ModifyTarget (Trumpet Blast's "attacking
    -- creatures") and Effect.GainControl (Aura Thief's "all enchantments") both
    -- do that, storing Affected.TheseObjects; the one-shots that take this type
    -- store nothing and are governed by CR 608.2c/608.2f instead.
    EachMatching (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
