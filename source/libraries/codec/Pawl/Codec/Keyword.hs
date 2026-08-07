module Pawl.Codec.Keyword where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Keyword as Keyword

-- | This module TIES THE CODEC KNOT that Pawl.Codec.Filter's keyword parameter
-- opens, exactly as Pawl.Types.Keyword ties the data-type one: CR 702.29e's
-- typecycling filter is decoded here by passing this module's own codec back
-- into the parametric one, which is legal in a single module and would be a
-- cycle across two.
--
-- Not Common.decodeNullary's table shape, since CR 702.164a's toxic and
-- CR 702.70a's poisonous each carry an N.
toJson :: Keyword.Keyword -> Value.Value
toJson k = case k of
  Keyword.Deathtouch -> Common.nullary "Deathtouch"
  Keyword.Defender -> Common.nullary "Defender"
  Keyword.DoubleStrike -> Common.nullary "DoubleStrike"
  Keyword.FirstStrike -> Common.nullary "FirstStrike"
  Keyword.Flash -> Common.nullary "Flash"
  Keyword.Flying -> Common.nullary "Flying"
  Keyword.Haste -> Common.nullary "Haste"
  -- CR 702.11b encodes as the bare tag and CR 702.11d as the tag plus its
  -- quality, rather than both carrying an explicit null: rule 702.11b's ability
  -- takes no parameter, so the card that prints it should say no more than
  -- Shroud's does, and `asTagged` reports an absent "value" as Nothing anyway.
  Keyword.Hexproof Nothing -> Common.nullary "Hexproof"
  Keyword.Hexproof (Just quality) -> Common.tagged "Hexproof" . Just $ Filter.toJson toJson quality
  Keyword.Indestructible -> Common.nullary "Indestructible"
  Keyword.Landwalk criterion -> Common.tagged "Landwalk" . Just $ Filter.toJson toJson criterion
  Keyword.Lifelink -> Common.nullary "Lifelink"
  Keyword.Reach -> Common.nullary "Reach"
  Keyword.Shroud -> Common.nullary "Shroud"
  Keyword.Trample -> Common.nullary "Trample"
  Keyword.Vigilance -> Common.nullary "Vigilance"
  Keyword.Banding -> Common.nullary "Banding"
  Keyword.Phasing -> Common.nullary "Phasing"
  Keyword.Aftermath -> Common.nullary "Aftermath"
  Keyword.Cycling cost searchFor -> Common.tagged "Cycling" . Just . Common.array $ [Cost.toJson toJson cost, Common.encodeMaybe (Filter.toJson toJson) searchFor]
  Keyword.Flashback cost -> Common.tagged "Flashback" . Just $ Cost.toJson toJson cost
  Keyword.Fear -> Common.nullary "Fear"
  Keyword.Morph cost -> Common.tagged "Morph" . Just $ Cost.toJson toJson cost
  Keyword.Entwine cost -> Common.tagged "Entwine" . Just $ Cost.toJson toJson cost
  Keyword.Poisonous n -> Common.tagged "Poisonous" . Just $ Common.encodeNatural n
  Keyword.Infect -> Common.nullary "Infect"
  Keyword.BattleCry -> Common.nullary "BattleCry"
  Keyword.Menace -> Common.nullary "Menace"
  Keyword.Devoid -> Common.nullary "Devoid"
  Keyword.Crew n -> Common.tagged "Crew" . Just $ Common.encodeNatural n
  Keyword.Riot -> Common.nullary "Riot"
  Keyword.Daybound -> Common.nullary "Daybound"
  Keyword.Nightbound -> Common.nullary "Nightbound"
  Keyword.Toxic n -> Common.tagged "Toxic" . Just $ Common.encodeNatural n
  Keyword.StartYourEngines -> Common.nullary "StartYourEngines"

fromJson :: Value.Value -> Either Text.Text Keyword.Keyword
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Deathtouch", _) -> Right Keyword.Deathtouch
    ("Defender", _) -> Right Keyword.Defender
    ("DoubleStrike", _) -> Right Keyword.DoubleStrike
    ("FirstStrike", _) -> Right Keyword.FirstStrike
    ("Flash", _) -> Right Keyword.Flash
    ("Flying", _) -> Right Keyword.Flying
    ("Haste", _) -> Right Keyword.Haste
    ("Hexproof", Just v) -> Keyword.Hexproof . Just <$> Filter.fromJson fromJson v
    ("Hexproof", Nothing) -> Right (Keyword.Hexproof Nothing)
    ("Indestructible", _) -> Right Keyword.Indestructible
    ("Landwalk", Just v) -> Keyword.Landwalk <$> Filter.fromJson fromJson v
    ("Lifelink", _) -> Right Keyword.Lifelink
    ("Reach", _) -> Right Keyword.Reach
    ("Shroud", _) -> Right Keyword.Shroud
    ("Trample", _) -> Right Keyword.Trample
    ("Vigilance", _) -> Right Keyword.Vigilance
    ("Banding", _) -> Right Keyword.Banding
    ("Phasing", _) -> Right Keyword.Phasing
    ("Aftermath", _) -> Right Keyword.Aftermath
    ("Cycling", Just (Value.Array (Array.MkArray [c, f]))) -> Keyword.Cycling <$> Cost.fromJson fromJson c <*> Filter.optional fromJson f
    ("Flashback", Just v) -> Keyword.Flashback <$> Cost.fromJson fromJson v
    ("Fear", _) -> Right Keyword.Fear
    ("Morph", Just v) -> Keyword.Morph <$> Cost.fromJson fromJson v
    ("Entwine", Just v) -> Keyword.Entwine <$> Cost.fromJson fromJson v
    ("Poisonous", Just v) -> Keyword.Poisonous <$> Common.decodeNatural v
    ("Infect", _) -> Right Keyword.Infect
    ("BattleCry", _) -> Right Keyword.BattleCry
    ("Menace", _) -> Right Keyword.Menace
    ("Devoid", _) -> Right Keyword.Devoid
    ("Crew", Just v) -> Keyword.Crew <$> Common.decodeNatural v
    ("Riot", _) -> Right Keyword.Riot
    ("Daybound", _) -> Right Keyword.Daybound
    ("Nightbound", _) -> Right Keyword.Nightbound
    ("Toxic", Just v) -> Keyword.Toxic <$> Common.decodeNatural v
    ("StartYourEngines", _) -> Right Keyword.StartYourEngines
    _ -> Left . Text.pack $ "unknown Keyword: " <> t
