module Pawl.Types.CopyTargets where

import qualified Pawl.Types.ObjectRef as ObjectRef

-- | Where a copy put onto the stack by Pawl.Types.Effect's CopyStackObject arm
-- gets its targets: four of CR 707.10's answers, which are different acts
-- rather than settings of one.
--
-- Not a Bool with a payload beside it, see #2209: CR 707.10c hands the choice to
-- a player, CR 707.10d makes the effect enumerate one copy per candidate, and
-- the unmarked case asks nobody anything. At most one can hold at a time, so the
-- type says so.
data CopyTargets
  = -- | CR 707.10 alone: the copy keeps the decisions the original made,
    -- targets included.
    Copied
  | -- | CR 707.10c: "you may choose new targets for the copy" (Twincast).
    ChosenByController
  | -- | CR 707.10d: one copy per object this ref names that the original could
    -- target, every one of that copy's targets being that object (Zada, Hedron
    -- Grinder's "copy that spell for each other creature you control that the
    -- spell could target").
    --
    -- The ref names the card's own description of the candidates ("each other
    -- creature you control"); rule 707.10d's "could target" narrowing is the
    -- executor's and is not written here.
    --
    -- Not implemented: CR 707.10d's "each PLAYER ... it could target", which
    -- Radiate needs and an ObjectRef cannot name (#3140).
    ForEach ObjectRef.ObjectRef
  | -- | CR 707.10e: ONE copy, whose every target is the object this ref names
    -- (Ivy, Gleeful Spellthief's "you may copy that spell. The copy targets
    -- Ivy").
    --
    -- The arm above's shape with the count fixed at one rather than at the
    -- candidate set's size, and rule 707.10e states the same test rule 707.10d
    -- does -- "if that player or object isn't a legal target for each instance of
    -- the word 'target', the copy isn't created" -- so the two share an
    -- executor.
    --
    -- A ref naming SEVERAL objects names none: the rule specifies "a new target",
    -- singular, and no printing states a set here.
    --
    -- Not implemented: a PLAYER as the new target, which Zevlor, Elturel Exile
    -- needs and an ObjectRef cannot name (#3140).
    Stated ObjectRef.ObjectRef
  deriving (Eq, Ord, Show)

-- | What a card copying with the original's targets writes, and the value the
-- codec elides.
defaultValue :: CopyTargets
defaultValue = Copied
