module Pawl.Types.ManaRestriction where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 106.6's first shape: "some spells or abilities that produce mana restrict
-- how that mana can be spent".
--
-- TWO fields and not one filter, because the restriction names a payment KIND
-- before it names a predicate. Rule 106.6 itself enumerates no kinds -- it says
-- only that the spending is restricted -- so the taxonomy comes from the printed
-- cards: Mishra's Workshop's "only to cast artifact spells" is CR 601.2h's
-- payment, Omen Hawker's "only to activate abilities" is CR 602.2b's, and
-- Dalakos, Crafter of Wonders' "only to cast artifact spells or activate
-- abilities of artifacts" is both at once with a predicate each. One filter
-- cannot say which of the two a mana is for, and the difference is not one a
-- predicate could carry: the two payments are about different objects, a spell
-- being cast and an ability's source.
--
-- Nothing on a field REFUSES that kind of payment outright; @Just f@ permits it
-- when the object being paid for matches @f@. @Just (Filter.And [])@ is the
-- unconditional permission -- Omen Hawker says nothing about WHICH abilities --
-- and Pawl.Types.Filter's own haddock names that spelling as the trivial
-- predicate. Both fields Nothing would be mana no payment may spend, which no
-- printing means; Pawl.CardSpec lints it out of the pool.
--
-- The whole record sits under a Maybe on both carriers
-- (Pawl.Types.ManaAddition, Pawl.Types.ManaUnit), where Nothing is "no CR 106.6
-- clause at all" -- almost every mana ever made.
--
-- Pawl.Engine.Mana.spendableAmong is the one reader, and it picks the field by
-- the payment's Pawl.Types.PaymentSubject rather than by anything about the
-- mana.
data ManaRestriction = MkManaRestriction
  { -- | CR 601.2h: the payment made while a spell is being cast. The filter is
    -- evaluated against that SPELL.
    casts :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 602.2b: the payment made while an ability is being activated. The
    -- filter is evaluated against the ability's SOURCE, which is the object
    -- "activate abilities of artifacts" is about.
    activations :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)

-- | The CR 106.6 clause permitting only casts that match this filter -- Geosurge
-- and Mishra's Workshop, which is every restricted printing in @data\/cards@ bar
-- Omen Hawker.
onlyCasts :: Filter.Filter Keyword.Keyword -> ManaRestriction
onlyCasts f = MkManaRestriction {casts = Just f, activations = Nothing}
