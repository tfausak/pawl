module Pawl.Types.Arrival where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ArrivalEnd as ArrivalEnd
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 401.4 / 404.3: one move into a library or a graveyard during an
-- Event.arrivingTogether scope, held for its owner to arrange as the scope
-- ends (Pawl.Engine.Event.arrangeArrivals).
data Arrival = MkArrival
  { -- | The pile's owner (CR 400.3), who arranges it.
    owner :: PlayerId.PlayerId,
    -- | Where in the pile it arrived.
    end :: ArrivalEnd.ArrivalEnd,
    -- | The cards the move placed: one, or a melded permanent's components
    -- (CR 712.21a).
    cards :: Seq.Seq ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
