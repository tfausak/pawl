module Pawl.Types.PendingEntryEffect where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 614.1c: the effects of an "as [this permanent] enters, [do something]"
-- rewrite that has applied and not yet run -- Monstrous War-Leech's mill.
-- Pawl.Types.PreventionRider's twin one rule over, and queued for the same
-- reason: the module that applies the replacement (Pawl.Engine.Event) is below
-- the module that can run a card's effects.
--
-- A program plus the environment it runs in, where the environment is READ AT
-- APPLICATION rather than re-derived at drain time. `controller` is CR 109.5's
-- "you" -- the entering permanent's controller, settled by CR 614.12's "as it
-- would exist on the battlefield" -- and it cannot wait, because a rewrite later
-- in the same CR 616.1 loop may hand the permanent to somebody else
-- (EntryRewrite.UnderSourceControl).
--
-- `object` is the permanent that entered, which is both the effects' source and
-- the object every Filter.IsSource in them resolves against.
--
-- No `targets` field, where PreventionRider has one: a static ability targets
-- nothing (CR 115.10a), so there is no chosen map to snapshot.
data PendingEntryEffect = MkPendingEntryEffect
  { object :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    effects :: Seq.Seq (Effect.Effect Card.Card)
  }
  deriving (Eq, Ord, Show)
