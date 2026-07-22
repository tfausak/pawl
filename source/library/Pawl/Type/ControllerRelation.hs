module Pawl.Type.ControllerRelation where

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
  deriving (Eq, Ord, Show)
