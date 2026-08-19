module Pawl.Types.PlayerCounterTally where

import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Quantity's PlayerCounters arm (#1305): how many
-- counters of a kind a player has (CR 122).
--
-- NOT parametric, unlike Pawl.Types.Plus and its two siblings: this arm is a
-- LEAF, holding no Quantity, so nothing here can close a cycle with Quantity.
--
-- NOT Pawl.Types.PlayerCounters either, despite the arm's tag, and the name
-- differs precisely because the types must not be shared: that record carries a
-- quantity field for Effect's Gain/RemovePlayerCounters arms -- how many
-- counters to move -- and this one asks how many are already there. Its haddock
-- forbids the merge in its own words: a record carrying a field for only some of
-- its users has become an untagged union. The wire tag stays "PlayerCounters",
-- since Quantity's tags and Effect's are separate namespaces.
data PlayerCounterTally = MkPlayerCounterTally
  { player :: PlayerRef.PlayerRef,
    kind :: PlayerCounterKind.PlayerCounterKind
  }
  deriving (Eq, Ord, Show)
