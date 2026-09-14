module Pawl.Types.KeywordDesignator where

import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily

-- | CR 702: WHICH RULE-702 ABILITY a card's sentence names, when the sentence is
-- about the ability rather than about the object that has it -- Fluctuator's
-- "cycling abilities you activate", Bureau Headmaster's "equip abilities you
-- activate", Boom Scholar's "exhaust abilities of other permanents you control".
-- Pawl.Types.ReduceActivationCost's @grantedBy@ is the one asker, and it is
-- compared against Pawl.Types.ActivatedAbility.keyword, the whole keyword an
-- ability carries.
--
-- TWO ARMS because rule 702 writes its ability-bearing keywords two ways, and
-- Pawl.Types.KeywordFamily can only name one of them. A keyword written with a
-- payload -- CR 702.6a's equip [cost], CR 702.29a's cycling [cost] -- is named by
-- its family, since the sentence drops the cost. A NULLARY keyword has no family
-- by that type's stated rule, so the sentence can only name the keyword itself:
-- CR 702.177a's exhaust is the pool's.
--
-- Not a widening of KeywordFamily, which was the alternative: an @Exhaust@ family
-- would give Filter.HasKeywordFamily a second spelling of Filter.HasKeyword, the
-- exact collision that type exists to prevent. Not a bare Keyword either, which
-- was the other: a representative keyword read as its family would make
-- @Equip {2}@ and @Equip {3}@ two spellings of one sentence, the objection that
-- sank widening HasKeyword in place (#522).
--
-- THE TWO ARMS DO NOT OVERLAP, because Pawl.Engine.Keyword.designates compares
-- the OfNullary arm's keyword whole: @OfNullary (Equip {2})@ would say "equip {2}
-- abilities" and not "equip abilities", which is a sentence no card prints rather
-- than a second spelling of one that does.
data KeywordDesignator
  = -- | CR 702.6a: a keyword rule 702 writes with a payload, named with the
    -- payload dropped.
    OfFamily KeywordFamily.KeywordFamily
  | -- | CR 702.177a: a keyword rule 702 writes with no payload, which has no
    -- family to be named by.
    OfNullary Keyword.Keyword
  deriving (Eq, Ord, Show)
