module Pawl.Types.StateDigest where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.GameState as GameState

-- | CR 732.3: a game state as Pawl.Engine.Fragmented.digest leaves it for
-- comparison. Ordered by a few cheap fields before the whole record, so two
-- states that differ in who holds priority or what is on the stack -- most
-- pairs a loop compares -- never walk every object.
newtype StateDigest = MkStateDigest
  { unwrap :: GameState.GameState
  }
  deriving (Show)

instance Eq StateDigest where
  a == b = compare a b == EQ

instance Ord StateDigest where
  compare (MkStateDigest a) (MkStateDigest b) =
    compare (cheap a) (cheap b) <> compare a b
    where
      cheap gs =
        ( GameState.priority gs,
          GameState.passed gs,
          GameState.stack gs,
          GameState.phase gs,
          Map.size (GameState.objects gs)
        )
