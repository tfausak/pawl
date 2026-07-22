module Pawl.Type.EntryRewrite where

import Pawl.Type.EntryOption (EntryOption)

-- CR 614.1c: how an "as this permanent enters" replacement modifies the entry.
-- AsCopy is Clone (CR 707.5, and a real "may" -- declining is legal); ChoiceOf
-- is Primal Plasma (CR 208.2b). Both write into the object's COPIABLE snapshot,
-- which is what makes CR 707.2 fall out with no further machinery: the rule says
-- copiable values are the printed values as modified by copy effects and by
-- "as ... enters" abilities that set power and toughness.
--
-- CR 707.5's second half is load-bearing for this phase, not incidental: "If the
-- text that's being copied includes any abilities that replace the
-- enters-the-battlefield event (such as 'enters with' or 'as [this] enters'
-- abilities), those abilities will take effect." That is what makes a Clone of a
-- Primal Plasma run the COPIED "as it enters" choice rather than skip it -- the
-- CR 616.2 ordering behaviour later tasks in this phase implement.
data EntryRewrite
  = AsCopy
  | ChoiceOf [EntryOption]
  deriving (Eq, Ord, Show)
