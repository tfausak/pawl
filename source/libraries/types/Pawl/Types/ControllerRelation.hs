module Pawl.Types.ControllerRelation where

-- CR 614.1 / 109.5: whose object a replacement's pattern admits, relative to the
-- controller of the effect's SOURCE (that is what "you" means on a permanent's
-- static ability). Hardened Scales says "a creature you control" (Yours); Rest in
-- Peace's redirect has no controller clause at all (Anyones).
--
-- P9's filter language absorbs this; it is here so the two gate cards can be
-- distinguished by DATA rather than by a constructor apiece.
data ControllerRelation
  = Yours
  | Anyones
  | -- CR 102.1: "an opponent" -- any other player. Leyline of the Void's "an
    -- opponent's graveyard". Read against the effect SOURCE's controller, like
    -- its siblings, but see Pawl.Engine.Replacement: for a ZONE CHANGE the subject is
    -- the object's OWNER (CR 400.3), because the destination zone is theirs.
    --
    -- "Any other player" is CR 806.1's free-for-all reading, the same /= test
    -- Count.playersFor and Filter.matches already use. Teams (CR 102.3) would
    -- make it wrong and have no representation (#175).
    Opponents
  deriving (Eq, Ord, Show)
