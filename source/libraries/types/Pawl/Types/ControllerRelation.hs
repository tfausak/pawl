module Pawl.Types.ControllerRelation where

-- | CR 614.1 / 109.5: whose object a replacement's pattern admits, relative to the
-- controller of the effect's SOURCE (that is what "you" means on a permanent's
-- static ability). Hardened Scales says "a creature you control" (Yours); Rest in
-- Peace's redirect has no controller clause at all (Anyones).
data ControllerRelation
  = Yours
  | Anyones
  | -- | CR 102.2/102.3: "an opponent" -- Leyline of the Void's "an opponent's
    -- graveyard". Read against the effect SOURCE's controller like its siblings,
    -- except that for a ZONE CHANGE Pawl.Engine.Replacement reads the object's
    -- OWNER (CR 400.3), the destination zone being theirs.
    --
    -- Read through Pawl.Types.Teams.areOpponents by all four arms that judge it
    -- (Pawl.Engine.Replacement's matchesPlayer, matchesCandidatePlayer,
    -- matchesController and matchesZoneOwner), so CR 102.3's teammate is not one
    -- and CR 806.1's free-for-all every other player is. Pawl.TeamSpec's
    -- "CR 102.3 a teammate's card is not put into an opponent's graveyard" is
    -- what proves the zone-change arm.
    Opponents
  deriving (Bounded, Enum, Eq, Ord, Show)
