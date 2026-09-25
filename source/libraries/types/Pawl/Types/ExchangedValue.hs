module Pawl.Types.ExchangedValue where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | One side of CR 701.12g's exchange of two numerical values. Each reference
-- must name exactly one player or object, or the exchange can't be completed
-- (CR 701.12a).
data ExchangedValue
  = -- | CR 701.12g: a player's life total, reached by gaining or losing life.
    LifeTotal PlayerRef.PlayerRef
  | -- | CR 701.12g / 613.4b: a creature's power, reached by a layer-7b setting effect.
    Power ObjectRef.ObjectRef
  | -- | CR 701.12g / 613.4b: a creature's toughness, reached by a layer-7b setting effect.
    Toughness ObjectRef.ObjectRef
  deriving (Eq, Ord, Show)
