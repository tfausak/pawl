module Pawl.Types.Pool where

-- CR 115: the closed set of recipient kinds a target slot may draw from, fixing
-- both WHICH objects are candidates and HOW they are referenced
-- (Recipient.ToCreature / ToPlayer / ToObject). Closed-half vocabulary, like the
-- old TargetSpec enum -- it grows only when the rules define a new kind of
-- targetable object, never per card.
data Pool
  = Creatures -- CR 115.1a: creatures on the battlefield (ToCreature).
  | Players -- CR 115: players still in the game (ToPlayer).
  | -- CR 115.4 names creatures, players, planeswalkers AND battles; this admits
    -- only the first two (#494, #302).
    AnyTarget
  | Permanents -- CR 110.1: permanents on the battlefield (ToObject).
  | Spells -- CR 112.1: spells on the stack (ToObject).
  | SpellsAndPermanents -- CR 115: spells on the stack + battlefield permanents (ToObject).
  deriving (Eq, Ord, Show)
