-- | A flat JSON value. scrod's seven-newtype split
-- (@Null@\/@Boolean@\/@Number@\/@String@\/@Array@\/@Object@\/@Pair@) is
-- flattened into one sum per pawl's one-type-per-module rule (§1 of the M3.5
-- spec). 'Object' is an ordered assoc list, not a 'Data.Map.Map': the codec
-- controls key order, so a canonical render falls out of emitting fields in a
-- fixed order.
module Pawl.Type.Json where

import Data.Text (Text)
import Pawl.Type.Decimal (Decimal)

data Value
  = Null
  | Boolean Bool
  | Number Decimal
  | String Text
  | Array [Value]
  | Object [(Text, Value)]
  deriving (Eq, Show)
