module Pawl.Type.EntryRewrite where

import Pawl.Type.EntryOption (EntryOption)

-- CR 614.1c: how an "as this permanent enters" replacement modifies the entry.
-- AsCopy is Clone (CR 707.9a, and a real "may" -- declining is legal); ChoiceOf
-- is Primal Plasma (CR 208.2b). Both write into the object's COPIABLE snapshot,
-- which is what makes CR 707.2 fall out with no further machinery: the rule says
-- copiable values are the printed values as modified by copy effects and by
-- "as ... enters" abilities that set power and toughness.
data EntryRewrite
  = AsCopy
  | ChoiceOf [EntryOption]
  deriving (Eq, Ord, Show)
