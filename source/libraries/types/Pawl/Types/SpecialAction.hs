module Pawl.Types.SpecialAction where

-- | CR 116.2: a special action a face's printed text grants its controller --
-- something they may do with priority that does not use the stack.
--
-- Named for the OPERATION and never for the card. CR 116.2e is one card by name,
-- but the engine must not learn that name: Pawl.Engine.Action reads this
-- permission off the card data and Pawl.Types.Action carries the rulebook's own
-- vocabulary, so neither half cases on an identity.
--
-- The carrier for the rows of CR 116.2 that a CARD grants. The rows the rules
-- grant to every player (CR 116.2a's land play) are not here -- they need no
-- printed permission -- and CR 116.2c and CR 116.2d, which an EFFECT grants
-- rather than a printed line, will need a way to name a continuous effect before
-- they can join this type (#875).
data SpecialAction
  = -- | CR 116.2e: "You may discard this card any time you could cast an
    -- instant." Circling Vultures is the card the rule names.
    --
    -- The timing is NOT carried, because CR 116.2e's own last sentence overrides
    -- the printed wording: "a player can take such an action any time they have
    -- priority". So the permission is unconditional and nothing reads casting
    -- timing to offer it.
    DiscardThisAnyTime
  deriving (Eq, Ord, Show)
