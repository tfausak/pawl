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
    -- "Any other player" is CR 806.1's free-for-all reading. Teams (CR 102.3)
    -- would make it wrong and have no representation (#175).
    Opponents
  deriving (Eq, Ord, Show)
