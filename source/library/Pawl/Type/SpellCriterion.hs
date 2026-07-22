module Pawl.Type.SpellCriterion where

import Pawl.Type.Color (Color)

-- Which spells a cost-modifying continuous effect applies to. (CR 613.11 puts
-- such an effect on the rules-modifying tier and defers its application order
-- to CR 601.2f; it does not say which spells a criterion admits, which is what
-- this type decides.) The
-- third sibling of Pawl.Type.CardCriterion and Pawl.Type.PermanentCriterion,
-- deliberately NOT merged with either: P9 merges all of them into one filter
-- language, and merging two of them here would be building half of P9 with one
-- customer (#38/#39/#40 are the same deferral for the other three).
--
-- Both inhabitants are evaluated against the PROJECTION by
-- Pawl.PlayerEffect.matchesSpell, never against a printed characteristic: a card
-- type is CR 613 layer 4 and a colour is layer 5, so Blood Moon and a colour
-- changer both change the answer.
data SpellCriterion
  = -- Thalia, Guardian of Thraben: "Noncreature spells cost {1} more to cast."
    NoncreatureSpell
  | -- Sapphire Medallion: "Blue spells you cast cost {1} less to cast."
    SpellOfColor Color
  deriving (Eq, Ord, Show)
