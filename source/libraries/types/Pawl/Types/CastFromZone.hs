module Pawl.Types.CastFromZone where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PermissionLimit as PermissionLimit

-- | The payload of Pawl.Types.PlayerEffect's CastFrom arm: whose copy of which
-- zone the permission opens, which cards in it it covers, and how often it may
-- be used.
--
-- SPUN OUT rather than left as two fields on the arm, Pawl.Types.GrantPlayFromExile
-- being the precedent one type over -- that grant is the other CR 601.3 permission
-- crossing owners, and it carries its scope beside its criterion the same way.
--
-- The zone reference is the whole of what lets a permission name a hand or a
-- graveyard somebody else owns (Sen Triplets). Pawl.Engine.Cast.zoneCandidates
-- offers every player's copy of a per-player zone, so the OWNER conjunct is this
-- record's rather than that list's: Yawgmoth's Will writes PlayerRef.Relative You
-- and reaches no other graveyard for it.
--
-- The Filter reads the PRINTED card in the zone, so a continuous effect changing
-- a card's own characteristics there is invisible to the narrowing (#1859).
-- The LIMIT is the permission's own, which is what makes the whole record the
-- key Pawl.Types.GameState.castPermissionsUsedThisTurn spends: two Assemble the
-- Players grant two budgets, and the source object beside this value is what
-- tells them apart.
data CastFromZone = MkCastFromZone
  { from :: InZone.InZone,
    matching :: Filter.Filter Keyword.Keyword,
    limit :: PermissionLimit.PermissionLimit
  }
  deriving (Eq, Ord, Show)
