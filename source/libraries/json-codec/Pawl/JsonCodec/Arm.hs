{-# LANGUAGE ScopedTypeVariables #-}

-- | Building a codec for a tagged sum from its arms.
--
-- The decoder and the schema are derived from the arm list; the ENCODER is
-- passed in as a hand-written total case. Deriving it too would need a
-- projection per arm, and then a constructor added to the type would compile
-- clean and silently stop being encodable. A case is exhaustiveness-checked,
-- and keeping @-Wincomplete-patterns@ able to see a new constructor is worth
-- more than collapsing a table that is written twice either way.
module Pawl.JsonCodec.Arm where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema

-- | 'Payload' stores the element codec's decoder and schema rather than the
-- codec itself, so the element type does not escape into an existential and
-- this stays an ordinary sum. 'Nullary' is not a special case of it: a nullary
-- arm's schema carries no @value@ property at all, and its decode ignores one
-- that is present.
data Arm a
  = Nullary String a
  | Payload String (Value.Value -> Either Text.Text a) (Define.SchemaM Schema.Schema)

nullary :: String -> a -> Arm a
nullary = Nullary

payload :: String -> Codec.Codec b -> (b -> a) -> Arm a
payload t c inject = Payload t (fmap inject . Codec.decode c) (Codec.schema c)

tag :: Arm a -> String
tag arm = case arm of
  Nullary t _ -> t
  Payload t _ _ -> t

-- | Assumes distinct tags across 'arms': 'List.find' takes the first match on
-- decode, so a duplicate tag is dead code, and 'armSchema' does not dedupe
-- either, so a duplicate emits two identical 'Schema.oneOf' branches that
-- nothing (including the value the decoder accepts) validates against.
--
-- A known tag missing its @value@ reports 'Common.withValue''s
-- @"missing tagged value"@, not an unknown-tag message. Most hand-written
-- codecs fall through their wildcard on a @(tag, mv)@ match instead and
-- report the tag as unknown; converting one to 'tagged' changes that string,
-- deliberately, since the tag genuinely is known here.
tagged :: forall a. (Typeable.Typeable a) => (a -> Value.Value) -> [Arm a] -> Codec.Codec a
tagged enc arms =
  Codec.MkCodec
    { Codec.encode = enc,
      Codec.decode = \value -> do
        (t, mv) <- Common.asTagged value
        case List.find ((== t) . tag) arms of
          Nothing -> Left . Text.pack $ "unknown " <> name <> ": " <> t
          Just (Nullary _ x) -> Right x
          Just (Payload _ dec _) -> Common.withValue mv dec,
      Codec.schema = Define.define (Name.typeName proxy) $ do
        schemas <- traverse armSchema arms
        pure (Schema.oneOf schemas)
    }
  where
    proxy = Typeable.Proxy :: Typeable.Proxy a
    name = Text.unpack . Name.unwrap $ Name.typeName proxy

armSchema :: Arm a -> Define.SchemaM Schema.Schema
armSchema arm = case arm of
  Nullary t _ -> pure (armObject t Nothing)
  Payload t _ s -> fmap (armObject t . Just) s

-- | No @additionalProperties: false@: 'Common.asTagged' ignores unknown keys,
-- and a nullary arm ignores a @value@ outright, so forbidding them would reject
-- documents the decoder accepts. @value@ IS required on a payload arm, because
-- 'Common.withValue' fails without it.
armObject :: String -> Maybe Schema.Schema -> Schema.Schema
armObject t ms =
  let typePair = Value.pair "type" (Schema.unwrap (Schema.constant (Text.pack t)))
   in case ms of
        Nothing -> Schema.object [typePair] [Text.pack "type"]
        Just s ->
          Schema.object
            [typePair, Value.pair "value" (Schema.unwrap s)]
            [Text.pack "type", Text.pack "value"]
