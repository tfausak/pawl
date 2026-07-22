module Pawl.Type.CostComponent where

-- CR 601.2f's list of what a cost's non-mana part can be: "paying mana, tapping
-- permanents, sacrificing permanents, discarding cards, and so on." One
-- component of a Pawl.Type.Cost, alongside its mana part.
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
  deriving (Eq, Ord, Show)
