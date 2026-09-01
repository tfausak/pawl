module Pawl.Types.AttackOption where

-- | CR 806.2b: which attack option a game uses -- "exactly one of the attack
-- left, attack right, and attack multiple players options must be used".
--
-- ONE sum and not three Bools, because that rule makes them mutually exclusive:
-- three flags would spell seven states of which four are no game at all. The
-- absence of an option is the absence of a value (@Nothing@ in
-- Pawl.Types.GameSettings.attackOption), the posture Pawl.Types.Daytime takes
-- toward a game that is neither day nor night. CR 800.2 makes these options a
-- MULTIPLAYER game adds, so a game without one is CR 506.2's two-player game,
-- playing by CR 507.1's free choice of one opponent.
--
-- Leftward and Rightward rather than Left and Right so that no use site has to
-- disambiguate them from Prelude's Either.
--
-- Not implemented: CR 809.3c's adjacent-seat restriction, which is neither of
-- these -- either neighbour, chosen -- and arrives with the Emperor variant
-- (#175).
data AttackOption
  = -- | CR 802.1: every one of the attacking player's opponents is a defending
    -- player, so CR 507.1 chooses nobody.
    MultiplePlayers
  | -- | CR 803.1a: only the opponent seated immediately to the attacking
    -- player's left may be attacked. The Grand Melee default (CR 807.2b).
    Leftward
  | -- | CR 803.1b: the same, to the right.
    Rightward
  deriving (Bounded, Enum, Eq, Ord, Show)
