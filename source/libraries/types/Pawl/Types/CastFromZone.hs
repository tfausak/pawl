module Pawl.Types.CastFromZone where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's CastFrom arm: whose copy of which
-- zone the permission opens, and which cards in it it covers.
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
data CastFromZone = MkCastFromZone
  { from :: InZone.InZone,
    matching :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
