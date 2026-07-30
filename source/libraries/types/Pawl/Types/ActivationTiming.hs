module Pawl.Types.ActivationTiming where

-- CR 307.5: when an activated ability may be activated.
--
-- A sum type rather than a Bool on the ability: no boolean blindness, and the
-- next rider (flash, "only during your turn", split second) is a constructor
-- here rather than a second flag to keep consistent with the first.
--
-- CR 307.5 defines the restricted case exactly, and narrowly: "it means only
-- that the player must have priority, it must be during the main phase of their
-- turn, and the stack must be empty. The player doesn't need to have a sorcery
-- card they could cast. Effects that would preclude that player from casting a
-- sorcery spell don't affect the player's capability to perform that action."
--
-- That last sentence is load-bearing and easy to get wrong: the check must NOT
-- consult casting prohibitions (Rule of Law, Silence). It is three facts about
-- the game state and nothing else.
data ActivationTiming
  = -- No rider: any time its controller has priority (CR 602.2).
    AnyTime
  | -- CR 702.6a's "Activate only as a sorcery", and every other ability that
    -- carries the same phrase.
    SorcerySpeed
  deriving (Eq, Ord, Show)
