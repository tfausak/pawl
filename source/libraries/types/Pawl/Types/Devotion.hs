module Pawl.Types.Devotion where

import qualified Data.Set as Set
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Quantity's Devotion arm (CR 700.5): whose devotion,
-- and to which colour or colours.
--
-- A SET rather than one colour, because CR 700.5's second sentence is not the
-- first one applied twice: devotion to two colours counts a hybrid symbol that is
-- both of them ONCE, so summing two single-colour readings would double it. The
-- set is what lets Pawl.Engine.Quantity ask each symbol a single question.
--
-- An empty set is degenerate rather than illegal -- it reads 0, since no symbol
-- is any of no colours -- and no card prints one.
data Devotion = MkDevotion
  { player :: PlayerRef.PlayerRef,
    colors :: Set.Set Color.Color
  }
  deriving (Eq, Ord, Show)
