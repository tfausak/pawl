module Pawl.Types.Concession where

-- | CR 104.3a: a player's answer when asked whether they concede.
--
-- A sum type rather than a Bool: this is an outcome ("I am leaving the game"),
-- not a predicate. See Pawl.Types.Prompt's Concede constructor for why the ask
-- exists at all and why it carries no Decider.
data Concession
  = Concedes
  | Continues
  deriving (Eq, Ord, Show)
