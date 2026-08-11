{-# LANGUAGE ScopedTypeVariables #-}

-- | Building a codec for a record from its fields, so that the encoder, the
-- decoder, the schema's @properties@ and its @required@ all come out of one
-- expression.
--
-- 'Fields' is contravariant in @o@ on its encoding side, so it is 'Applicative'
-- and can never be 'Monad'. That is deliberate and load-bearing: with
-- @ApplicativeDo@, a field whose value depends on an earlier one fails to
-- compile rather than quietly desugaring through a bind the encoder could not
-- honour.
module Pawl.JsonCodec.Fields where

import Control.Monad ((>=>))
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema

data Fields o a = MkFields
  { encodeFields :: o -> [Pair.Pair Value.Value],
    decodeFields :: [Pair.Pair Value.Value] -> Either Text.Text a,
    schemaFields :: Define.SchemaM ([Pair.Pair Value.Value], [Text.Text])
  }

-- | Only the decoding side mentions @a@, so this is a type-changing record
-- update rather than a rebuild.
instance Functor (Fields o) where
  fmap f fields = fields {decodeFields = fmap f . decodeFields fields}

instance Applicative (Fields o) where
  pure x =
    MkFields
      { encodeFields = const [],
        decodeFields = const (Right x),
        schemaFields = pure ([], [])
      }
  f <*> x =
    MkFields
      { encodeFields = \o -> encodeFields f o <> encodeFields x o,
        decodeFields = \ps -> decodeFields f ps <*> decodeFields x ps,
        schemaFields = do
          (fp, fr) <- schemaFields f
          (xp, xr) <- schemaFields x
          pure (fp <> xp, fr <> xr)
      }

-- | A field written whatever its value, and named in the schema's @required@.
required :: String -> Codec.Codec a -> (o -> a) -> Fields o a
required key c get =
  MkFields
    { encodeFields = \o -> [Common.pair key (Codec.encode c (get o))],
      decodeFields = Common.field key >=> Codec.decode c,
      schemaFields = do
        s <- Codec.schema c
        pure ([Common.pair key (Schema.unwrap s)], [Text.pack key])
    }

-- | A field written only when it differs from a default, absent from the
-- schema's @required@, and carrying that default as the schema's @default@.
--
-- The default is passed once. It was previously held by hand across
-- 'Common.optionalPair' and 'Common.defaultedField', which had to agree or a
-- round trip stopped being the identity; here they cannot disagree, and neither
-- can the schema, whose @default@ is this same value through this same encoder.
defaulted :: (Eq a) => String -> a -> Codec.Codec a -> (o -> a) -> Fields o a
defaulted key d c get =
  MkFields
    { encodeFields = \o ->
        let x = get o
         in if x == d then [] else [Common.pair key (Codec.encode c x)],
      decodeFields = Common.defaultedField key d (Codec.decode c),
      schemaFields = do
        s <- Codec.schema c
        pure ([Common.pair key (Schema.unwrap (Schema.withDefault (Codec.encode c d) s))], [])
    }

-- | Takes no name: @o@ is fixed by the return type, so 'Name.typeName' supplies
-- one. A field's KEY is still a string, because a JSON key is not a Haskell
-- name.
object :: forall o. (Typeable.Typeable o) => Fields o o -> Codec.Codec o
object fields =
  Codec.MkCodec
    { Codec.encode = Common.object . encodeFields fields,
      Codec.decode = Common.asObject >=> decodeFields fields,
      Codec.schema = Define.define (Name.typeName (Typeable.Proxy :: Typeable.Proxy o)) $ do
        (properties, req) <- schemaFields fields
        pure (Schema.object properties req)
    }
