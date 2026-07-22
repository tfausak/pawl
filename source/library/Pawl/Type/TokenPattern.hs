module Pawl.Type.TokenPattern where

import Pawl.Type.ControllerRelation (ControllerRelation)

-- CR 111.1 / 614.1: which token creations a scaling replacement intercepts.
-- Doubling Season's token clause is Yours ("under your control"). No card
-- criterion: nothing in the pool scopes token doubling by what the token IS.
newtype TokenPattern = MkTokenPattern
  { whose :: ControllerRelation
  }
  deriving (Eq, Ord, Show)
