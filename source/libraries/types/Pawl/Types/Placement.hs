module Pawl.Types.Placement where

import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Readiness as Readiness
import qualified Pawl.Types.TapState as TapState

-- | One card a scenario's board places, and what a client would display about
-- it. Its seat is its owner and supplies its zone.
data Placement = MkPlacement
  { card :: CardName.CardName,
    label :: Maybe Label.Label,
    tapped :: TapState.TapState,
    readiness :: Readiness.Readiness,
    damage :: Natural,
    counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural,
    -- | The seat controlling it, when not its owner.
    controller :: Maybe Label.Label
  }
  deriving (Eq, Ord, Show)
