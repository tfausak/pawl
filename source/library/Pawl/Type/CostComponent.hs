module Pawl.Type.CostComponent where

import Numeric.Natural (Natural)
import Pawl.Type.PermanentCriterion (PermanentCriterion)

-- CR 601.2f's list of what a cost's non-mana part can be: "paying mana, tapping
-- permanents, sacrificing permanents, discarding cards, and so on." One
-- component of a Pawl.Type.Cost, alongside its mana part.
--
-- Discard-as-cost and exile-from-a-zone components do not exist yet (#108).
--
-- The successor to Pawl.Type.AdditionalCost, whose two nullary inhabitants were
-- named relative to that type ("Self"); here the object a cost is on is "This".
--
-- Open-half card data. Pawl.Cost is the ONLY module that may case on it: the
-- rules core reads the classification (can this be paid? does it require the tap
-- symbol?) and never the identity of a component.
data CostComponent
  = -- CR 107.5: "The tap symbol in an activation cost means 'Tap this
    -- permanent.' A permanent that's already tapped can't be tapped again to pay
    -- the cost." CR 302.6 gates it on summoning sickness.
    TapThis
  | -- CR 701.21a: sacrifice the object the cost is on -- its controller moves it
    -- from the battlefield directly to its owner's graveyard (Mindslaver).
    --
    -- Deliberately NOT `Sacrifice 1 <this permanent>`: CR 602.1a's
    -- self-referential cost names one object and offers no choice, so folding it
    -- into the criterion form would invent a prompt the rules do not have.
    SacrificeThis
  | -- CR 119.4 / Greed: pay this much life. "If a cost or effect allows a player
    -- to pay an amount of life greater than 0, the player may do so only if
    -- their life total is greater than or equal to the amount of the payment. If
    -- a player pays life, the payment is subtracted from their life total; in
    -- other words, the player loses that much life."
    --
    -- A Natural and not a Quantity: a Quantity's evaluation needs a binding
    -- environment, which a cost has no access to at CR 601.2f time, and no card
    -- in the pool pays a variable amount of life (#99).
    PayLife Natural
  | -- CR 701.21a / CR 601.2f's "sacrificing permanents": sacrifice this many
    -- permanents matching the criterion (Village Rites' one creature,
    -- Fireblast's two Mountains). The player chooses which (CR 701.21a), so this
    -- is a prompt and never an engine pick.
    --
    -- The criterion is matched against the PROJECTION, never a printed
    -- characteristic: Blood Moon makes a nonbasic land a Mountain, and it may be
    -- sacrificed as one.
    Sacrifice Natural PermanentCriterion
  deriving (Eq, Ord, Show)
