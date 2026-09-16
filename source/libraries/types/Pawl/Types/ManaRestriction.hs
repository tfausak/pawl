module Pawl.Types.ManaRestriction where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 106.6's first shape: "some spells or abilities that produce mana restrict
-- how that mana can be spent".
--
-- A FIELD PER PAYMENT KIND and not one filter, because the restriction names a
-- payment KIND before it names a predicate. Rule 106.6 itself enumerates no
-- kinds -- it says only that the spending is restricted -- so the taxonomy comes
-- from the printed cards: Mishra's Workshop's "only to cast artifact spells" is
-- CR 601.2h's payment, Omen Hawker's "only to activate abilities" is CR 602.2b's,
-- Overgrown Zealot's "only to turn permanents face up" is CR 116.2b's special
-- action, and Creeping Peeper's "only to cast an enchantment spell, unlock a
-- door, or turn a permanent face up" names three kinds in one clause -- which is
-- also what says unlocking and turning face up are two kinds and not one. One
-- filter cannot say which kind a mana is for, and the difference is not one a
-- predicate could carry: the payments are about different objects, a spell being
-- cast, an ability's source, and the permanent a special action is taken on.
--
-- FOUR fields and not one per payment the engine can make: CR 116.2 alone lists
-- eleven special actions, and what earns a field is a PRINTED rider naming that
-- payment. Foretelling (CR 116.2h), plotting (CR 116.2k), CR 508.1j \/ 509.1f's
-- combat toll and CR 118.12's resolution-time payment share
-- Pawl.Types.PaymentSubject's ForNeither arm with nothing here, none of them
-- being named on a card.
--
-- Nothing on a field REFUSES that kind of payment outright; @Just f@ permits it
-- when the object being paid for matches @f@. @Just (Filter.And [])@ is the
-- unconditional permission -- Omen Hawker says nothing about WHICH abilities --
-- and Pawl.Types.Filter's own haddock names that spelling as the trivial
-- predicate. Every field Nothing would be mana no payment may spend, which no
-- printing means; Pawl.CardSpec lints it out of the pool.
--
-- The whole record sits under a Maybe on both carriers
-- (Pawl.Types.ManaAddition, Pawl.Types.ManaUnit), where Nothing is "no CR 106.6
-- clause at all" -- almost every mana ever made.
--
-- Pawl.Engine.Mana.admitsUnder is the one reader, and it picks the field by
-- the payment's Pawl.Types.PaymentSubject rather than by anything about the
-- mana.
data ManaRestriction = MkManaRestriction
  { -- | CR 601.2h: the payment made while a spell is being cast. The filter is
    -- evaluated against that SPELL.
    casts :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 602.2b: the payment made while an ability is being activated. The
    -- filter is evaluated against the ability's SOURCE, which is the object
    -- "activate abilities of artifacts" is about.
    activations :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 116.2m \/ 709.5e: the unlock cost of a locked half, paid as a special
    -- action. The filter is evaluated against the PERMANENT being unlocked.
    unlocks :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 116.2b: the cost of turning a permanent face up, paid as a special
    -- action. The filter is evaluated against the PERMANENT being turned face
    -- up, which is the object Tin Street Gossip's "turn creatures face up" is
    -- about.
    turnsFaceUp :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)

-- | Every field Nothing: the record no printing means on its own. THE one
-- exhaustive construction site outside the codec, so a field added later is
-- named here by -Werror rather than silently defaulted at each caller.
none :: ManaRestriction
none =
  MkManaRestriction
    { casts = Nothing,
      activations = Nothing,
      unlocks = Nothing,
      turnsFaceUp = Nothing
    }

-- | The CR 106.6 clause permitting only casts that match this filter, which is
-- the shape Geosurge and Mishra's Workshop print. Not every restricted printing
-- has it: Omen Hawker fills the activation field instead and Overgrown Zealot
-- the turn-face-up one.
onlyCasts :: Filter.Filter Keyword.Keyword -> ManaRestriction
onlyCasts f = none {casts = Just f}
