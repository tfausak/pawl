{-# LANGUAGE DeriveLift #-}

module Pawl.Type.CardCriterion where

import Language.Haskell.TH.Syntax (Lift)

-- A first-order, analyzable predicate over a card, as data (CR 701.23a). Its one
-- inhabitant now is CR 205.4c's basic land. Grows: by color, by card type, by
-- name, .... Evaluated by Resolve.matchesCriterion, never a card's identity.
data CardCriterion
  = BasicLandCard
  deriving (Eq, Lift, Ord, Show)
