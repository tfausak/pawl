-- | The @Keyword ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Keyword where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Cost (costToJson, jsonToCost)
import Pawl.Codec.Filter (filterToJson, optionalFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Keyword as Keyword

-- Not Json.decodeNullary's table shape any more: CR 702.164a's toxic and CR 702.70a's
-- poisonous each carry an N, so this is the tagged-with-an-optional-payload case
-- jsonToQuantity uses.
keywordToJson :: Keyword.Keyword -> Value
keywordToJson k = case k of
  Keyword.Deathtouch -> Json.nullary (Text.pack "Deathtouch")
  Keyword.Defender -> Json.nullary (Text.pack "Defender")
  Keyword.DoubleStrike -> Json.nullary (Text.pack "DoubleStrike")
  Keyword.FirstStrike -> Json.nullary (Text.pack "FirstStrike")
  Keyword.Flying -> Json.nullary (Text.pack "Flying")
  Keyword.Haste -> Json.nullary (Text.pack "Haste")
  Keyword.Indestructible -> Json.nullary (Text.pack "Indestructible")
  Keyword.Reach -> Json.nullary (Text.pack "Reach")
  Keyword.Trample -> Json.nullary (Text.pack "Trample")
  Keyword.Vigilance -> Json.nullary (Text.pack "Vigilance")
  Keyword.Cycling cost searchFor -> Json.tagged (Text.pack "Cycling") (Just (Array (MkArray [costToJson cost, maybe Json.jNull filterToJson searchFor])))
  Keyword.Flashback cost -> Json.tagged (Text.pack "Flashback") (Just (costToJson cost))
  Keyword.Fear -> Json.nullary (Text.pack "Fear")
  Keyword.Entwine cost -> Json.tagged (Text.pack "Entwine") (Just (costToJson cost))
  Keyword.Poisonous n -> Json.tagged (Text.pack "Poisonous") (Just (Json.natTo n))
  Keyword.Infect -> Json.nullary (Text.pack "Infect")
  Keyword.Devoid -> Json.nullary (Text.pack "Devoid")
  Keyword.Toxic n -> Json.tagged (Text.pack "Toxic") (Just (Json.natTo n))

jsonToKeyword :: Value -> Either Text Keyword.Keyword
jsonToKeyword value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Deathtouch", _) -> Right Keyword.Deathtouch
    ("Defender", _) -> Right Keyword.Defender
    ("DoubleStrike", _) -> Right Keyword.DoubleStrike
    ("FirstStrike", _) -> Right Keyword.FirstStrike
    ("Flying", _) -> Right Keyword.Flying
    ("Haste", _) -> Right Keyword.Haste
    ("Indestructible", _) -> Right Keyword.Indestructible
    ("Reach", _) -> Right Keyword.Reach
    ("Trample", _) -> Right Keyword.Trample
    ("Vigilance", _) -> Right Keyword.Vigilance
    ("Cycling", Just (Array (MkArray [c, f]))) -> Keyword.Cycling <$> jsonToCost c <*> optionalFilter f
    ("Flashback", Just v) -> Keyword.Flashback <$> jsonToCost v
    ("Fear", _) -> Right Keyword.Fear
    ("Entwine", Just v) -> Keyword.Entwine <$> jsonToCost v
    ("Poisonous", Just v) -> Keyword.Poisonous <$> Json.natFrom v
    ("Infect", _) -> Right Keyword.Infect
    ("Devoid", _) -> Right Keyword.Devoid
    ("Toxic", Just v) -> Keyword.Toxic <$> Json.natFrom v
    _ -> Left (Text.pack "unknown Keyword: " <> t)
