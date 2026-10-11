module Pawl.Types.KeywordDesignator where

import qualified Pawl.Types.KeywordFamily as KeywordFamily

-- | CR 702: WHICH RULE-702 ABILITY a card's sentence names, when the sentence is
-- about the ability rather than about the object that has it -- Fluctuator's
-- "cycling abilities you activate", Boom Scholar's "exhaust abilities of other
-- permanents you control", Sunscape Battlemage's "if it was kicked with its
-- {1}{G} kicker". Pawl.Engine.Keyword.designates is the one reader, asked of
-- Pawl.Types.ActivatedAbility.keyword by ActivationCriteria's @grantedBy@ and of
-- Pawl.Types.Object.paidCosts' keys by Quantity.TimesPaid and Filter.Paid.
--
-- A family where the sentence drops the payload, the whole keyword where it
-- keeps one or the keyword has none, which KeywordFamily cannot name: an
-- @Exhaust@ family would give Filter.HasKeywordFamily a second spelling of
-- Filter.HasKeyword, and a representative keyword read as its family would make
-- @Equip {2}@ and @Equip {3}@ two spellings of one sentence (#522).
--
-- PARAMETRIC in the keyword for Pawl.Types.Filter's reason: Filter.Paid carries
-- one, and Keyword names Filter. Only @KeywordDesignator Keyword.Keyword@ is
-- ever written.
data KeywordDesignator keyword
  = -- | CR 702.6a: a keyword rule 702 writes with a payload, named with the
    -- payload dropped.
    OfFamily KeywordFamily.KeywordFamily
  | -- | CR 702.177a / 702.33f: one keyword, payload and all.
    OfKeyword keyword
  | -- | CR 702.33e: the kicker and multikicker abilities an object's "if it was
    -- kicked" is linked to, a sticker kicker's not among them (CR 702.33h).
    PrintedKicker
  deriving (Eq, Ord, Show)
