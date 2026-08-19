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
-- The skips travel WITH the turn rather than referencing it, which makes the
-- scoping structural. CR 500.7 takes the most recently created turn first, so a
-- skip phrased as "your next untap step" (CR 614.10a, what Effect.SkipNextPhase
-- installs) would land on the wrong turn as soon as a second extra-turn effect
-- resolved after this one -- Time Warp and Savor the Moment in the same turn.
--
-- `source` is CR 113.7's source of the effect that created the turn, carried so
-- the skips can become ActiveReplacements naming it when the turn begins
-- (Pawl.Engine.Replacement.installTurnSkips). CR 608.2n has put Savor the Moment
-- into its owner's graveyard long before then.
data ExtraTurn = MkExtraTurn
  { taker :: PlayerId.PlayerId,
    source :: ObjectId.ObjectId,
    skipped :: Set.Set PhaseSelector.PhaseSelector
  }
  deriving (Eq, Ord, Show)
