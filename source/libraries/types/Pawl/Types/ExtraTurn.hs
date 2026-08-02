module Pawl.Types.ExtraTurn where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 500.7: one turn that has been created and not yet taken, as an entry on
-- GameState.extraTurns. Runtime-only, like ActiveReplacement: card data writes
-- the Effect.TakeExtraTurn that makes one, never one of these.
--
-- `skipped` is CR 500.11's "cause a step, phase, or turn to be skipped", scoped
-- to THIS turn and to no other -- Savor the Moment's "skip the untap step of
-- that turn", where "that turn" is the one the same resolution just created.
-- Empty for Time Warp, which skips nothing.
--
-- The skips travel WITH the turn rather than being a reference to it, which is
-- what makes the scoping structural. CR 500.7's last sentence -- "the most
-- recently created turn will be taken first" -- lets a turn created later be
-- taken sooner, so a skip phrased as "your next untap step" (CR 614.10a, which
-- is what Effect.SkipNextPhase installs) would land on the wrong turn as soon as
-- a second extra-turn effect resolves after this one. Time Warp and Savor the
-- Moment cast in the same turn is exactly that board, and Pawl.TurnSpec pins it.
--
-- `source` is CR 113.7's source of the effect that created the turn, carried so
-- that the skips can become ActiveReplacements naming it when the turn begins
-- (Pawl.Engine.Replacement.installTurnSkips). CR 608.2n has put Savor the Moment
-- into its owner's graveyard long before then -- "as the final part of an instant
-- or sorcery spell's resolution, the spell is put into its owner's graveyard" --
-- exactly as it has Fatigue by the time Fatigue's own skip matters.
data ExtraTurn = MkExtraTurn
  { taker :: PlayerId.PlayerId,
    source :: ObjectId.ObjectId,
    skipped :: Set.Set PhaseSelector.PhaseSelector
  }
  deriving (Eq, Ord, Show)
