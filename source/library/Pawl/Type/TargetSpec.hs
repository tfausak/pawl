{-# LANGUAGE DeriveLift #-}

module Pawl.Type.TargetSpec where

import Language.Haskell.TH.Syntax (Lift)

-- What a target slot may legally hold. Classification data, never a predicate
-- function. AnyTarget is "any target": a creature or a player (CR 115.4's
-- damageable set, minus the card types that do not exist yet -- planeswalkers
-- and battles grow this).
data TargetSpec
  = AnyTarget
  | -- CR 115.4: "target creature" -- a creature on the battlefield, no players.
    -- The first spec whose legal set can be EMPTY, which falsifies M3a's
    -- CR 601.2c targeting gate (Giant Growth with no creature is uncastable).
    CreatureTarget
  | -- CR 115: "target spell or permanent" -- any object on the stack, or any
    -- permanent on the battlefield. The first target that reaches the stack.
    SpellOrPermanentTarget
  | -- A land permanent on the battlefield (projected card-type Land). Used by the
    -- M3d fixture "target land becomes ...".
    LandTarget
  | -- CR 115: "target player" -- a player still in the game. The players-only
    -- restriction AnyTarget does not express (Mindslaver, M3g).
    PlayerTarget
  deriving (Eq, Lift, Ord, Show)
