module Pawl.Type.Player where

import Pawl.Type.Status (Status)

data Player = MkPlayer
  { life :: Integer,
    status :: Status
  }
  deriving (Eq, Show)
