module Pawl.Types.Placement where

import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.FaceDownReason as FaceDownReason
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
    -- | CR 111.1: a token rather than a card, so on the battlefield only (CR 111.7).
    token :: Bool,
    -- | The seat controlling it, when not its owner.
    controller :: Maybe Label.Label,
    -- | CR 301.5 / 303.4: the labelled object or seat it is attached to.
    attached :: Maybe Label.Label,
    -- | CR 903.3: its owner's commander.
    commander :: Bool,
    -- | CR 712.8e / 712.8f: the face it shows, when not the one the layout decides.
    face :: Maybe CardName.CardName,
    -- | CR 708.2a: face down, by the rules that allowed it (CR 708.6).
    faceDown :: Maybe FaceDownReason.FaceDownReason,
    -- | CR 310.9: the seat protecting it, a battle.
    protector :: Maybe Label.Label
  }
  deriving (Eq, Ord, Show)
