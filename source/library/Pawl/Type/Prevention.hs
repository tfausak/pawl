module Pawl.Type.Prevention where

-- CR 615.1a: a prevention effect specification, classified by the damage events
-- it watches and cancels. PreventAllCombatDamage watches every Combat-kind event
-- and drops it (Fog). Its own leaf family, distinct from Effect (one-shot),
-- Modification (continuous, layered), and ReplacementEffect (zone-change redirect).
-- Only Pawl.Event may case on it. Grows PreventFromSource / PreventNextN as cards
-- need them (spec section 8).
data Prevention = PreventAllCombatDamage
  deriving (Eq, Ord, Show)
