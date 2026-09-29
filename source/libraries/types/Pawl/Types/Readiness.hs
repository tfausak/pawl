module Pawl.Types.Readiness where

-- | CR 302.6, as a scenario places a permanent: whether its controller has
-- controlled it continuously since their most recent turn began.
data Readiness
  = Sick
  | Ready
  deriving (Bounded, Enum, Eq, Ord, Show)
