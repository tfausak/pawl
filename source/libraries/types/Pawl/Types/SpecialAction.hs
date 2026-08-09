module Pawl.Types.SpecialAction where

-- | CR 116.2: a special action a face's printed text grants the player holding
-- the card -- something they may do with priority that does not use the stack.
--
-- Named for the OPERATION and never for the card. CR 116.2e is one card by name,
-- but the engine must not learn that name: Pawl.Engine.Action reads this
-- permission off the card data and Pawl.Types.Action carries the rulebook's own
-- vocabulary, so neither half cases on an identity.
--
-- The carrier for the rows of CR 116.2 a card grants IN PROSE, which is one row
-- today. The rules grant CR 116.2a's land play to every player, so it needs no
-- printed permission; CR 116.2f, CR 116.2h and CR 116.2k are granted by KEYWORDS
-- (suspend, foretell, plot), so they belong to Pawl.Types.Keyword rather than
-- here whenever those land; and CR 116.2c and CR 116.2d, which an EFFECT grants
-- rather than a printed line, need a way to name a continuous effect before they
-- can join this type (#875).
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
