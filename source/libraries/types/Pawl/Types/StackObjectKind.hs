module Pawl.Types.StackObjectKind where

-- | Which of CR 601.2c's two announcement roads put an object on the stack: a
-- spell (CR 112.1), or an ability (CR 113.3). The rule itself names only spells
-- in its parenthetical, while CR 602.2b and CR 603.3d route an activated and a
-- triggered ability through the same targeting step -- so a card that says "the
-- target of A SPELL" needs the two told apart and a card that says "a spell or
-- ability" does not.
--
-- Recorded on Pawl.Types.BecameTarget rather than re-derived, for that record's
-- `controller` reason: Pawl.Engine.Event's matcher is pure over the condition,
-- the event, the bearer and CR 109.5's "you", with no GameState, and the
-- targeting object can leave the stack before anything reads the event.
--
-- Not a Bool, for Pawl.Types.PayObligation's reason: @Spell@ says which limb of
-- the rule is in play where @True@ would say nothing.
data StackObjectKind
  = -- | CR 112.1: "A spell is a card on the stack." Pawl.Engine.Cast's
    -- announcement.
    Spell
  | -- | CR 113.3b and CR 113.3c's abilities, the two categories that use the
    -- stack -- Pawl.Engine.Activate's and Pawl.Engine.Engine's announcements.
    --
    -- NOT split into activated and triggered. Both reach CR 601.2c by the same
    -- two rules and no printing in the pool reads the difference, so per
    -- docs/design.md section 4 the third constructor is not built.
    Ability
  deriving (Bounded, Enum, Eq, Ord, Show)
