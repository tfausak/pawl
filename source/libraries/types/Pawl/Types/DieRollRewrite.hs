module Pawl.Types.DieRollRewrite where

-- | CR 706.1 / 614.1a: how a replacement rewrites one instruction's die rolls.
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.CoinFlipRewrite's reason: a replacement effect is classified by the
-- event class it intercepts AND the rewrite shape it applies.
data DieRollRewrite
  = -- | CR 614.1a / 706.6: Pixie Guide's "instead roll that many dice plus one
    -- and ignore the lowest roll".
    --
    -- ONE die more and ONE more ignored, both, because the printed sentence is
    -- both -- the extra die is what the ignore is for, and a rewrite that added
    -- the die without the ignore would leave the instruction reading a result it
    -- never asked for. Read forward under CR 614.5 the way
    -- CoinFlipRewrite.Doubled is: each row gets one opportunity, on the event or
    -- on the modified event replacing it, so a second such row adds a second die
    -- and a second ignore -- roll N+2 and throw the two lowest away.
    --
    -- PLUS ONE rather than a Quantity, since that is what every printing of the
    -- sentence says (Pixie Guide, Barbarian Class, Wyll, Blade of Frontiers,
    -- Krark's Other Thumb). A rewrite that scaled the count would be a capability
    -- no card exercises.
    ExtraIgnoringLowest
  deriving (Bounded, Enum, Eq, Ord, Show)
