module Pawl.Types.Prohibition where

-- | What a stored per-object prohibition forbids: one CR 101.2 "can't" that a
-- resolution puts on the permanents it names (CR 611.2a). The classification
-- Pawl.Engine.Game.prohibitedObjects is asked by; each reader asks for one.
data Prohibition
  = -- | Its activated abilities can't be activated (CR 602.5).
    Activate
  | -- | It can't block (CR 509.1b).
    Block
  | -- | It can't be regenerated (CR 701.19c).
    Regenerate
  | -- | It doesn't untap during its controller's untap step (CR 502.3).
    Untap
  deriving (Bounded, Enum, Eq, Ord, Show)
