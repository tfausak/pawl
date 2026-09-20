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
    -- captured before the branch was announced. A VILLAINOUS pair is the
    -- exception, and CR 701.55d is why it can be: its per-seat pass re-reads the
    -- bindings after each seat is bound, so `villainous` below does bind a slot.
    --
    -- Not implemented: a chooser who wants neither branch still announces one
    -- and declines the rider that follows it, so the decline is a second
    -- question rather than a third answer to this one (#3088).
    --
    -- Both halves of a pair must name the SAME chooser, the announcement being
    -- made once at whichever branch the resolution reaches first; Pawl.CardSpec
    -- holds the corpus to that alongside the symmetry.
    chooser :: PlayerRef.PlayerRef,
    -- | CR 701.55a: this pair is a VILLAINOUS CHOICE, which rule 701.55b makes
    -- an exception to rule 608.2d -- the chooser "may choose an option that is
    -- illegal or impossible", performing as much of it as is possible. So
    -- Pawl.Engine.Resolve.villainousPass offers both branches here rather than
    -- filtering them through clauseIsImpossible first, and a chooser facing one
    -- impossible limb is still asked. Great Intelligence's Plan is one producer,
    -- and Pawl.ResolveSpec's "CR 701.55b Great Intelligence's Plan still offers
    -- the discard to an empty-handed opponent" is what proves it.
    --
    -- A marker on the CR 608.2d pair rather than an opcode of its own: rule
    -- 701.55a's "chooses [A] or [B], then all actions in the chosen option are
    -- performed" IS that rule's either-or, announced by somebody other than the
    -- resolving controller, and Pawl.Types.Effect is first-order so a branch
    -- carrying its own effect list could not sit in one.
    --
    -- Both halves of a pair must agree on it, for `sibling`'s reason.
    --
    -- Rule 701.55d's exception to rule 608.2e is why this marker steers
    -- resolution rather than only widening the offer: a pair carrying it is
    -- chosen AND performed one player at a time by
    -- Pawl.Engine.Resolve.villainousPass, never by chosenBranch, and the seat
    -- whose option is running is bound under Binding.facingPlayers.
    -- Pawl.ResolveSpec's "CR 701.55d two opponents each taking The Dalek
    -- Emperor's token limb make two tokens" is what proves it.
    --
    -- Not implemented: rule 701.55c's replacement of one facing by several,
    -- The Valeyard's (#3898).
    villainous :: Bool
  }
  deriving (Eq, Ord, Show)
