-- | An encoder, a decoder and a schema as one value, so the three cannot drift
-- apart.
--
-- The schema is a 'Define.SchemaM' rather than a bare 'Schema.Schema' because a
-- recursive type needs the @$defs@ table to have a schema at all, and because
-- this is the field a later change would be most expensive to alter: every
-- codec in the tree is written against it.
--
-- This module holds the type and nothing else. The combinators live in
-- @Pawl.JsonCodec.Common@, which imports this: 'Common.maybe' wraps the
-- existing element helpers rather than reimplementing them, and 'assertCodec'
-- reads this record, so putting them here would make the two modules mutually
-- recursive.
module Pawl.JsonCodec.Codec where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Schema as Schema

data Codec a = MkCodec
  { encode :: a -> Value.Value,
    decode :: Value.Value -> Either Text.Text a,
    schema :: Define.SchemaM Schema.Schema
  }
