{-# LANGUAGE ScopedTypeVariables #-}

-- | Building a codec for a tagged sum from its arms.
--
-- All THREE of a decoder, an encoder and a schema come off the arm list, so a
-- tag string, a payload codec and a payload's structure are each written once
-- (#1461). An arm carries a MATCHER for that -- @a -> Maybe payload@, the
-- inverse of the injection beside it -- so each constructor is one
-- self-contained bidirectional expression and a codec that disagrees with its
-- own payload is a type error rather than a divergence to be spotted by eye.
--
-- WHAT THAT GIVES UP is the exhaustiveness check: 'tagged' can no longer tell
-- that its arm list names every constructor, so one added to the type compiles
-- clean here. Three things soften it. 'tagged' encodes an unmatched value as
-- @{}@, which is the one shape 'Common.asTagged' cannot read, so the gap fails
-- loudly at the first round trip instead of writing something plausible. The
-- arm list was ALREADY the sole source of truth for decoding and for the
-- schema, so a missing constructor was already undetected in two of the three
-- directions. And 'taggedWith' keeps the hand-written total encoder for any
-- caller that wants @-Wincomplete-patterns@ back.
--
-- 'enum' needs neither: an ALL-NULLARY type needs no projection at all, so both
-- directions come off @Bounded@ and @Show@ and a new constructor is picked up
-- automatically.
module Pawl.JsonCodec.Arm where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema

-- | ONE shape for every arm, because all three differ in exactly one thing --
-- what the @value@ key may be -- and agree on the rest. The decoder and the
-- projection are stored with the element codec ALREADY APPLIED, which is what
-- keeps the payload type from escaping into an existential and this from
-- needing one.
--
-- Both directions speak the same @Maybe Value.Value@: 'Nothing' is the bare
-- tag a nullary arm writes and an optional one may, 'Just' is a @value@ key.
-- So encoding is one expression for every arm and decoding is one call, where
-- three constructors needed a case each.
data Arm a = MkArm
  { tag :: String,
    -- | The @value@ key if the document had one. What an absence MEANS is the
    -- arm's own business: a nullary arm ignores it, a required one fails, an
    -- optional one reads it as 'Nothing'.
    decodeValue :: Maybe Value.Value -> Either Text.Text a,
    valueSchema :: ValueSchema,
    -- | 'Nothing' when this arm does not describe the value at all, which is
    -- how 'tagged' picks the arm that does.
    projectValue :: a -> Maybe (Maybe Value.Value)
  }

-- | Whether an arm's tagged object carries a @value@, and whether it must.
-- The one axis the three arm shapes differ on, and the only one that could not
-- collapse: a nullary arm's schema names no @value@ property AT ALL, which is
-- different from naming an optional one.
data ValueSchema
  = NoValue
  | RequiredValue (Define.SchemaM Schema.Schema)
  | OptionalValue (Define.SchemaM Schema.Schema)

-- | A nullary arm derives its own matcher from 'Eq', so its call sites do not
-- change: there is no payload to project, and the value it carries is the only
-- thing it could match. Its decode IGNORES a @value@ that is present, rather
-- than rejecting it.
nullary :: (Eq a) => String -> a -> Arm a
nullary t x =
  MkArm
    { tag = t,
      decodeValue = \_ -> Right x,
      valueSchema = NoValue,
      projectValue = \y -> if y == x then Just Nothing else Nothing
    }

-- | @inject@ and @project@ are inverses: @project@ is what makes this arm
-- encodable, and it answers 'Nothing' for every OTHER constructor of the type.
payload :: String -> Codec.Codec b -> (b -> a) -> (a -> Maybe b) -> Arm a
payload t c inject project =
  MkArm
    { tag = t,
      decodeValue = \mv -> Common.withValue mv (fmap inject . Codec.decode c),
      valueSchema = RequiredValue (Codec.schema c),
      projectValue = fmap (Just . Codec.encode c) . project
    }

-- | A payload arm whose @value@ key may be absent as well as present, both
-- under the same tag -- 'Pawl.Codec.Keyword'\'s @Hexproof@ is the first caller:
-- CR 702.11b's bare hexproof omits @value@ entirely and CR 702.11d's
-- "hexproof from [quality]" carries it, and both decode to the one
-- constructor. 'payload' cannot express this, because it needs ONE constructor
-- to accept TWO shapes and a single 'payload' arm only ever accepts one.
--
-- @project@ answers @Just Nothing@ for the shape that writes a bare tag and
-- @Just (Just b)@ for the one that writes a value, which is the distinction
-- this arm exists to carry; 'Nothing' means some other constructor entirely.
optionalPayload :: String -> Codec.Codec b -> (Maybe b -> a) -> (a -> Maybe (Maybe b)) -> Arm a
optionalPayload t c inject project =
  MkArm
    { tag = t,
      decodeValue = fmap inject . traverse (Codec.decode c),
      valueSchema = OptionalValue (Codec.schema c),
      projectValue = fmap (fmap (Codec.encode c)) . project
    }

-- | The arm's half of encoding: 'Nothing' when this arm does not describe the
-- value, so 'tagged' can take the first that does.
armEncode :: Arm a -> a -> Maybe Value.Value
armEncode arm x = fmap (Common.tagged (tag arm)) (projectValue arm x)

-- | 'taggedWith' with the encoder derived from the arms' matchers.
--
-- An unmatched value encodes as @{}@ -- deliberately the one object
-- 'Common.asTagged' rejects, since it has no @type@ key. That keeps 'encode'
-- total without inventing a plausible-looking wrong answer: a constructor
-- missing from the arm list fails the moment anything round-trips it, rather
-- than writing a document that decodes to something else.
tagged :: (Typeable.Typeable a) => [Arm a] -> Codec.Codec a
tagged arms = taggedWith (\x -> Maybe.fromMaybe (Value.object []) (Foldable.asum (fmap (`armEncode` x) arms))) arms

-- | 'tagged' with the encoder written out instead of derived, for a caller that
-- wants @-Wincomplete-patterns@ to see its constructor list. 'enum' is the one
-- in-tree user: deriving its encoder would make encoding a scan over every
-- constructor, which for Pawl.Types.Subtype is several hundred 'Eq' tests per
-- value, where 'show' answers directly.
--
-- Assumes distinct tags across @arms@: 'List.find' takes the first match on
-- decode, so a duplicate tag is dead code, and 'armSchema' does not dedupe
-- either, so a duplicate emits two identical 'Schema.oneOf' branches that
-- nothing (including the value the decoder accepts) validates against.
--
-- A 'payload' tag missing its @value@ reports 'Common.withValue''s
-- @"missing tagged value"@, not an unknown-tag message -- an 'optionalPayload'
-- tag takes the same absence as 'Nothing' instead of failing. Most
-- hand-written codecs fall through their wildcard on a @(tag, mv)@ match
-- instead and report the tag as unknown; converting one to 'tagged' changes
-- that string, deliberately, since the tag genuinely is known here.
taggedWith :: forall a. (Typeable.Typeable a) => (a -> Value.Value) -> [Arm a] -> Codec.Codec a
taggedWith enc arms =
  Codec.MkCodec
    { Codec.encode = enc,
      Codec.decode = \value -> do
        (t, mv) <- Common.asTagged value
        case List.find ((== t) . tag) arms of
          Nothing -> Left . Text.pack $ "unknown " <> name <> ": " <> t
          Just arm -> decodeValue arm mv,
      Codec.schema = Define.define (Name.typeName proxy) $ do
        schemas <- traverse armSchema arms
        pure (Schema.oneOf schemas)
    }
  where
    proxy = Typeable.Proxy :: Typeable.Proxy a
    name = Text.unpack . Name.unwrap $ Name.typeName proxy

-- | The whole codec for an ALL-NULLARY tagged sum, derived from the datatype.
--
-- @[minBound ..]@ is the arm list and derived 'Show' is the tag, so neither half
-- carries anything the type does not already say. A constructor added to the
-- type is encodable, decodable and in the schema without touching this module or
-- the caller's.
--
-- DERIVED 'Show' BECOMES THE WIRE FORMAT: renaming a constructor renames its
-- tag, and so silently changes every card file that names it. That coupling is
-- not new -- every hand-written arm under @Pawl.Codec@ already spells the tag as
-- the constructor's name, as an unenforced convention -- but this makes it
-- structural, and a rename is now a data migration.
--
-- Only for types whose constructors are ALL nullary. One that grows a payload
-- loses @Enum@, which is a compile error here rather than a silent wrong answer.
--
-- Decoding is 'tagged'\'s linear scan over the arm list, unchanged: this
-- replaces a hand-written list of the same length rather than adding one.
enum :: forall a. (Bounded a, Enum a, Eq a, Show a, Typeable.Typeable a) => Codec.Codec a
enum = taggedWith (Common.nullary . show) (fmap (\c -> nullary (show c) c) [minBound .. maxBound :: a])

armSchema :: Arm a -> Define.SchemaM Schema.Schema
armSchema arm = case valueSchema arm of
  NoValue -> pure (armObject (tag arm) Nothing)
  RequiredValue s -> fmap (armObject (tag arm) . Just) s
  OptionalValue s -> fmap (armObjectOptional (tag arm)) s

-- | No @additionalProperties: false@: 'Common.asTagged' ignores unknown keys,
-- and a nullary arm ignores a @value@ outright, so forbidding them would reject
-- documents the decoder accepts. @value@ IS required on a payload arm, because
-- 'Common.withValue' fails without it -- 'armObjectOptional' is the one arm
-- shape where it is not.
armObject :: String -> Maybe Schema.Schema -> Schema.Schema
armObject t ms =
  let typePair = Value.pair "type" (Schema.unwrap (Schema.constant (Text.pack t)))
   in case ms of
        Nothing -> Schema.object [typePair] [Text.pack "type"]
        Just s ->
          Schema.object
            [typePair, Value.pair "value" (Schema.unwrap s)]
            [Text.pack "type", Text.pack "value"]

-- | 'armObject'\'s payload case with @value@ NOT in @required@, for
-- 'OptionalPayload': the decoder accepts an absent @value@, so a schema that
-- required it would reject a document the codec itself writes and reads.
armObjectOptional :: String -> Schema.Schema -> Schema.Schema
armObjectOptional t s =
  let typePair = Value.pair "type" (Schema.unwrap (Schema.constant (Text.pack t)))
   in Schema.object
        [typePair, Value.pair "value" (Schema.unwrap s)]
        [Text.pack "type"]
