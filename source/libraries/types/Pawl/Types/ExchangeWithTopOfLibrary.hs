module Pawl.Types.ExchangeWithTopOfLibrary where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 701.12d: the one object `ref` names and the top card of the library of
-- the one player `player` names exchange zones -- Darkpact's "Exchange that
-- card with the top card of your library".
data ExchangeWithTopOfLibrary = MkExchangeWithTopOfLibrary
  { -- | The card that goes to the top of its owner's library (CR 400.3).
    ref :: ObjectRef.ObjectRef,
    -- | Whose library's top card goes where `ref`'s card was.
    player :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
