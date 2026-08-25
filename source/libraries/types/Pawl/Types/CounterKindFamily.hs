module Pawl.Types.CounterKindFamily where

-- | WHICH KIND a Pawl.Types.CounterKind is, with its payload dropped.
-- Pawl.Types.KeywordFamily's shape and, in the one place they differ, its
-- opposite: that type has a constructor only for the PAYLOAD-CARRYING keywords,
-- because a nullary keyword's family would say what the keyword already says,
-- while this one has a constructor for every 'Pawl.Types.CounterKind'
-- constructor without exception. It is not asked what a card means; it is asked
-- what the whole set of kinds is, and a family missing its nullary kinds would
-- answer that question wrong.
--
-- THAT is the reason it exists. CR 122.1's last sentence makes counters with the
-- same name interchangeable, so Pawl.Codec.CounterName must reject the spelling
-- of every kind below, and the set it rejects has to be COMPLETE. Deriving
-- Bounded and Enum here is the only complete enumeration of those constructors
-- the compiler will give: 'Pawl.Types.CounterKind' cannot derive either, since
-- CR 122.1b's keyword arm and CR 122.1's named arm carry payloads.
-- Pawl.Codec.CounterName.familyOf takes no wildcard, so a new kind cannot reach
-- the reserved set without passing through a constructor here.
--
-- It imports NOTHING, for Pawl.Types.KeywordFamily's reason: 'Pawl.Types.CounterKind'
-- names Pawl.Types.CounterName, so a family type that named the kind back would
-- close a cycle around the type this whole mechanism exists to guard.
--
-- No codec. Nothing puts a family on the wire -- a card names a kind, never a
-- family -- so this is an internal classification, unlike Pawl.Types.KeywordFamily,
-- which Pawl.Types.Filter asks for by name.
data CounterKindFamily
  = -- | CR 122.1a: Pawl.Types.CounterKind.PlusOnePlusOne.
    PlusOnePlusOne
  | -- | CR 122.1a: Pawl.Types.CounterKind.MinusOneMinusOne.
    MinusOneMinusOne
  | -- | CR 122.1b: Pawl.Types.CounterKind.Keyword, whichever keyword it carries.
    -- The payload is dropped exactly as Pawl.Types.KeywordFamily drops toxic's N.
    Keyword
  | -- | CR 122.1e: Pawl.Types.CounterKind.Loyalty.
    Loyalty
  | -- | Rule 714: Pawl.Types.CounterKind.Lore.
    Lore
  | -- | CR 122.1g: Pawl.Types.CounterKind.Defense.
    Defense
  | -- | CR 702.63a: Pawl.Types.CounterKind.Time.
    Time
  | -- | CR 702.32a: Pawl.Types.CounterKind.Fade.
    Fade
  | -- | CR 122.1c: Pawl.Types.CounterKind.Shield.
    Shield
  | -- | CR 711.2: Pawl.Types.CounterKind.Level.
    Level
  | -- | CR 122.1j: Pawl.Types.CounterKind.Hone.
    Hone
  | -- | CR 122.1: Pawl.Types.CounterKind.Named, whichever name it carries. The
    -- open arm, and the only one that reserves no spelling -- the names it
    -- carries are what the reserved set is checked AGAINST.
    Named
  deriving (Bounded, Enum, Eq, Ord, Show)
