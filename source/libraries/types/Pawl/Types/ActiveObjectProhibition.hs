module Pawl.Types.ActiveObjectProhibition where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prohibition as Prohibition
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 611.2a / 613.11: a stored, resolution-generated prohibition over one
-- permanent, held in GameState.objectProhibitions and left by
-- Pawl.Types.Effect's Prohibit.
--
-- Read through Pawl.Engine.Game.prohibitedObjects, one 'Prohibition.Prohibition'
-- at a time: CR 602.5 at Pawl.Engine.ActivationProhibition.cantActivate, CR
-- 509.1b at Pawl.Engine.CombatRestriction.blockProhibited, CR 502.3 at
-- Pawl.Engine.UntapRestriction.doesNotUntap -- each unioned beside its printed
-- carrier's rows, so no gate learns which road a prohibition took -- and CR
-- 701.19c at Pawl.Engine.Event.resolveDestruction, which turns the destruction
-- it proposes into one that can't be regenerated.
--
-- OUTSIDE the layer system (CR 613.11): each modifies the rules rather than an
-- object's characteristics, so no Pawl.Types.Modification arm could carry it
-- and an ability-removing effect leaves it standing.
--
-- A bare ObjectId: the ref is read ONCE, as the ability resolves. CR 400.7 is
-- answered by the id itself -- a zone change mints a fresh ObjectId, so a
-- permanent bounced and replayed is one this row never named and the row merely
-- dangles until its expiry sweeps it. Pawl.ActivationProhibitionSpec's "a Troll
-- bounced by Unsummon and replayed the same turn is no longer prohibited"
-- proves it.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b). `timestamp` is stored for CR 613.7, and nothing observes it: two
-- prohibitions cannot conflict.
--
-- No `controller`, where Pawl.Types.ActivePlayerEffect stores one: the row names
-- no player, so CR 109.5's "you" is never asked of it. No CR 116.2d name: a row
-- a resolution stored has outlived the ability that made it, so
-- Pawl.Engine.IgnoredAbility is never asked about one.
--
-- Runtime-only: card data writes Pawl.Types.Prohibit, never one of these. It
-- has a codec (Pawl.Codec.ActiveObjectProhibition) because a game in progress
-- has to be writable to JSON (#126).
data ActiveObjectProhibition = MkActiveObjectProhibition
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    what :: Prohibition.Prohibition,
    -- | The permanent the prohibition covers.
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
