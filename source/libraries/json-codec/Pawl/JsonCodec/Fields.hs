{-# LANGUAGE ScopedTypeVariables #-}

-- | Building a codec for a record from its fields, so that the encoder, the
-- decoder, the schema's @properties@ and its @required@ all come out of one
-- expression.
--
-- 'Fields' has no 'Monad' instance and cannot get one: 'encodeFields' never
-- mentions @a@ at all, so '>>=' would have no @a@ to hand its continuation on
-- the encoding side. (Contravariance in @o@ alone would not rule out 'Monad'
-- in @a@ -- @newtype F o a = F (o -> Maybe a)@ is contravariant in @o@ and is
-- a lawful monad; the missing @a@ is the actual obstruction.) That is
-- deliberate and load-bearing: with @ApplicativeDo@, a field whose value
-- depends on an earlier one fails to compile rather than quietly desugaring
-- through a bind the encoder could not honour.
--
-- What a 'Monad' would add on top is a bind between fields; 'objectWith'
-- adds something else, a check on the whole assembled record after every
-- field has already decoded. It cannot see individual fields to make one
-- depend on another -- only the finished @o@ -- and it runs on the decode
-- side only, since encoding cannot fail.
module Pawl.JsonCodec.Fields where

import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema

-- | Assumes distinct keys across the 'required'/'defaulted' fields composed
-- into one 'Fields': 'Common.field' (via 'lookupPair') takes the first match
-- on decode, so a duplicate key is dead code, and 'encodeFields' does not
-- dedupe either, so a duplicate emits two identical JSON object members --
-- the first of which wins on the next decode.
data Fields o a = MkFields
  { encode :: o -> [Pair.Pair Value.Value],
    decode :: [Pair.Pair Value.Value] -> Either Text.Text a,
    schema :: Define.SchemaM ([Pair.Pair Value.Value], [Text.Text])
  }

-- | Only the decoding side mentions @a@, so this is a type-changing record
-- update rather than a rebuild.
instance Functor (Fields o) where
  fmap f fields = fields {decode = fmap f . decode fields}

instance Applicative (Fields o) where
  pure x =
    MkFields
      { encode = const [],
        decode = const (Right x),
        schema = pure ([], [])
      }
  f <*> x =
    MkFields
      { encode = \o -> encode f o <> encode x o,
        decode = \ps -> decode f ps <*> decode x ps,
        schema = do
          (fp, fr) <- schema f
          (xp, xr) <- schema x
          pure (fp <> xp, fr <> xr)
      }

-- | A field written whatever its value, and named in the schema's @required@.
required :: String -> Codec.Codec a -> (o -> a) -> Fields o a
required key c get =
  MkFields
    { encode = \o -> [Value.pair key (Codec.encode c (get o))],
      decode = \ps -> Common.field key ps >>= Codec.decode c,
      schema = do
        s <- Codec.schema c
        pure ([Value.pair key (Schema.unwrap s)], [Text.pack key])
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
    { encode = \o ->
        let x = get o
         in if x == d then [] else [Value.pair key (Codec.encode c x)],
      decode = Common.defaultedField key d (Codec.decode c),
      schema = do
        s <- Codec.schema c
        pure ([Value.pair key (Schema.unwrap (Schema.withDefault (Codec.encode c d) s))], [])
    }

-- | Like 'object', but runs a check against the assembled record after
-- 'decodeFields' succeeds, rejecting it on 'Left' -- e.g. 'TypeLine' rejecting
-- an empty @types@ set per CR 205.1. Encoding cannot fail, so the check never
-- runs there, and the schema is unaffected by it: JSON Schema could express
-- some such rules (@minItems@ for a non-empty set), but the rule being
-- enforced is a rule of Magic, not a property of the wire format, so it has
-- no schema representation.
objectWith :: forall o. (Typeable.Typeable o) => (o -> Either Text.Text o) -> Fields o o -> Codec.Codec o
objectWith check fields =
  Codec.MkCodec
    { Codec.encode = Value.object . encode fields,
      Codec.decode = \v -> Common.asObject v >>= decode fields >>= check,
      Codec.schema = Define.define (Name.typeName (Typeable.Proxy :: Typeable.Proxy o)) $ do
        (properties, req) <- schema fields
        pure (Schema.object properties req)
    }

-- | Takes no name: @o@ is fixed by the return type, so 'Name.typeName' supplies
-- one. A field's KEY is still a string, because a JSON key is not a Haskell
-- name.
object :: forall o. (Typeable.Typeable o) => Fields o o -> Codec.Codec o
object = objectWith pure
