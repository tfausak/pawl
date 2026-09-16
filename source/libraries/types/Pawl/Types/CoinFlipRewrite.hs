module Pawl.Types.CoinFlipRewrite where

-- | CR 705.1 / 614.1a: how a replacement rewrites one coin flip.
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.DrawCountRewrite's reason: a replacement effect is classified by
-- the event class it intercepts AND the rewrite shape it applies.
data CoinFlipRewrite
  = -- | CR 614.1a: Krark's Thumb's "instead flip two coins and ignore one".
    --
    -- TWICE AS MANY coins rather than exactly two, which is CR 614.5 read
    -- forward: each row gets one opportunity, on the event or on the modified
    -- event replacing it, so a second such row doubles what the first left. The
    -- flipper keeps one of them either way -- rule 705.1's flip has one result,
    -- and Pawl.Types.Prompt's ChooseCoinResult is where the rest are ignored.
    Doubled
  deriving (Bounded, Enum, Eq, Ord, Show)
