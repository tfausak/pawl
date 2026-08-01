-- | The @Keyword ⇆ Json@ codec (#481).
module Pawl.Codec.Keyword where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Cost (costToJson, jsonToCost)
import Pawl.Codec.Filter (filterToJson, optionalFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Subtype (jsonToSubtype, subtypeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Keyword as Keyword

-- This module TIES THE CODEC KNOT that Pawl.Codec.Filter's keyword parameter
-- opens, exactly as Pawl.Types.Keyword ties the data-type one: 702.29e's
-- typecycling filter is decoded here by passing this module's own codec back into
-- the parametric one, which is legal in a single module and would be a cycle
-- across two.
--
-- Not Json.decodeNullary's table shape any more: CR 702.164a's toxic and CR 702.70a's
-- poisonous each carry an N, so this is the tagged-with-an-optional-payload case
-- jsonToQuantity uses.
keywordToJson :: Keyword.Keyword -> Value
keywordToJson k = case k of
  Keyword.Deathtouch -> Json.nullary (Text.pack "Deathtouch")
  Keyword.Defender -> Json.nullary (Text.pack "Defender")
  Keyword.DoubleStrike -> Json.nullary (Text.pack "DoubleStrike")
  Keyword.FirstStrike -> Json.nullary (Text.pack "FirstStrike")
  Keyword.Flash -> Json.nullary (Text.pack "Flash")
  Keyword.Flying -> Json.nullary (Text.pack "Flying")
  Keyword.Haste -> Json.nullary (Text.pack "Haste")
  Keyword.Hexproof -> Json.nullary (Text.pack "Hexproof")
  Keyword.Indestructible -> Json.nullary (Text.pack "Indestructible")
  Keyword.Landwalk subtype -> Json.tagged (Text.pack "Landwalk") (Just (subtypeToJson subtype))
  Keyword.Reach -> Json.nullary (Text.pack "Reach")
  Keyword.Shroud -> Json.nullary (Text.pack "Shroud")
  Keyword.Trample -> Json.nullary (Text.pack "Trample")
  Keyword.Vigilance -> Json.nullary (Text.pack "Vigilance")
  Keyword.Cycling cost searchFor -> Json.tagged (Text.pack "Cycling") (Just (Array (MkArray [costToJson keywordToJson cost, maybe Json.jNull (filterToJson keywordToJson) searchFor])))
  Keyword.Flashback cost -> Json.tagged (Text.pack "Flashback") (Just (costToJson keywordToJson cost))
  Keyword.Fear -> Json.nullary (Text.pack "Fear")
  Keyword.Entwine cost -> Json.tagged (Text.pack "Entwine") (Just (costToJson keywordToJson cost))
  Keyword.Poisonous n -> Json.tagged (Text.pack "Poisonous") (Just (Json.natTo n))
  Keyword.Infect -> Json.nullary (Text.pack "Infect")
  Keyword.Menace -> Json.nullary (Text.pack "Menace")
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
    ("Flash", _) -> Right Keyword.Flash
    ("Flying", _) -> Right Keyword.Flying
    ("Haste", _) -> Right Keyword.Haste
    ("Hexproof", _) -> Right Keyword.Hexproof
    ("Indestructible", _) -> Right Keyword.Indestructible
    ("Landwalk", Just v) -> Keyword.Landwalk <$> jsonToSubtype v
    ("Reach", _) -> Right Keyword.Reach
    ("Shroud", _) -> Right Keyword.Shroud
    ("Trample", _) -> Right Keyword.Trample
    ("Vigilance", _) -> Right Keyword.Vigilance
    ("Cycling", Just (Array (MkArray [c, f]))) -> Keyword.Cycling <$> jsonToCost jsonToKeyword c <*> optionalFilter jsonToKeyword f
    ("Flashback", Just v) -> Keyword.Flashback <$> jsonToCost jsonToKeyword v
    ("Fear", _) -> Right Keyword.Fear
    ("Entwine", Just v) -> Keyword.Entwine <$> jsonToCost jsonToKeyword v
    ("Poisonous", Just v) -> Keyword.Poisonous <$> Json.natFrom v
    ("Infect", _) -> Right Keyword.Infect
    ("Menace", _) -> Right Keyword.Menace
    ("Devoid", _) -> Right Keyword.Devoid
    ("Toxic", Just v) -> Keyword.Toxic <$> Json.natFrom v
    _ -> Left (Text.pack "unknown Keyword: " <> t)
