module Pawl.Types.OrElse where

import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 608.2d's "or": the sibling clause this one is exclusive with, and WHO
-- announces which of the pair happens. Pawl.Types.Clause.orElse is where it
-- rides; that field's haddock says why the pair is named by CR 608.2e's ordinal
-- rather than by nesting one clause inside the other.
--
-- The chooser rides the rider for Pawl.Types.Optionality's reason: a clause
-- printing no either-or has nobody to name, and a Clause field would admit that
-- pairing.
data OrElse = MkOrElse
  { -- | The sibling, which must name this clause back -- see Pawl.CardSpec's
    -- cardBranchesAreAsymmetric, which is what holds the corpus to it.
    sibling :: ClauseIndex.ClauseIndex,
    -- | WHO announces the branch. CR 608.2d says only that "the player" does,
    -- and the printed sentence says which: Twiddle's is the resolving
    -- controller (CR 405.4), and Worms of the Earth's "any player may sacrifice
    -- two lands of their choice or have this enchantment deal 5 damage to that
    -- player" asks the whole table, one announcement each and CR 101.4's order
    -- over them.
    --
    -- Relative You is the unmarked value and the codec writes it as an absent
    -- key, so a card says nothing unless it means somebody else.
    --
    -- The players who announced THIS branch are the only ones its clause's own
    -- CR 603.5 "may" is offered to, and the only ones its CR 118.12 gate is
    -- offered to -- see Pawl.Engine.Resolve.chosenBranch, which hands that set
    -- to `exercises` and `payGateAdmits` rather than binding it to a slot: a
    -- slot bound here would be invisible to those two, which read the bindings
    -- captured before the branch was announced.
    --
    -- Not implemented: a chooser who wants neither branch still announces one
    -- and declines the rider that follows it, so the decline is a second
    -- question rather than a third answer to this one (#3088).
    --
    -- Both halves of a pair must name the SAME chooser, the announcement being
    -- made once at whichever branch the resolution reaches first; Pawl.CardSpec
    -- holds the corpus to that alongside the symmetry.
    chooser :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
