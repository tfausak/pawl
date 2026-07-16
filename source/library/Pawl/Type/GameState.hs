module Pawl.Type.GameState where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.Object (Object)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Phase (Phase)
import Pawl.Type.Player (Player)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Result (Result)

data GameState = MkGameState
  { objects :: Map ObjectId Object,
    library :: Map PlayerId (Seq ObjectId),
    hand :: Map PlayerId (Seq ObjectId),
    graveyard :: Map PlayerId (Seq ObjectId),
    battlefield :: Set ObjectId,
    exile :: Set ObjectId,
    stack :: [ObjectId],
    players :: Map PlayerId Player,
    turnOrder :: [PlayerId],
    activePlayer :: PlayerId,
    phase :: Phase,
    priority :: Maybe PlayerId,
    passes :: Natural,
    turnNumber :: Natural,
    result :: Maybe Result,
    nextObjectId :: ObjectId,
    drewFromEmpty :: Set PlayerId,
    landPlayed :: Set PlayerId
  }
  deriving (Eq, Show)
