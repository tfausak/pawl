-- | A flat JSON value. scrod's seven-newtype split
-- (@Null@\/@Boolean@\/@Number@\/@String@\/@Array@\/@Object@\/@Pair@) is
-- flattened into one sum per pawl's one-type-per-module rule (§1 of the M3.5
-- spec). 'Object' is an assoc list, not a 'Data.Map.Map', so the codec can emit
-- fields in a readable order rather than an alphabetical one. That order is
-- incidental: JSON objects are unordered, nothing checks the bytes of a card
-- file, and 'Pawl.Json.sortKeys' exists to compare two values regardless of it.
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
