module Pawl.Codec.Keyword where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Keyword as Keyword

-- | Not Common.decodeNullary's table shape any more: CR 702.164a's toxic and CR
-- 702.70a's poisonous each carry an N, so this is the tagged-with-an-optional-
-- payload case Quantity.toJson uses.
toJson :: Keyword.Keyword -> Value.Value
toJson k = case k of
  Keyword.Deathtouch -> Common.nullary "Deathtouch"
  Keyword.Defender -> Common.nullary "Defender"
  Keyword.DoubleStrike -> Common.nullary "DoubleStrike"
  Keyword.FirstStrike -> Common.nullary "FirstStrike"
  Keyword.Flying -> Common.nullary "Flying"
  Keyword.Haste -> Common.nullary "Haste"
  Keyword.Indestructible -> Common.nullary "Indestructible"
  Keyword.Landwalk subtype -> Common.tagged "Landwalk" . Just $ Subtype.toJson subtype
  Keyword.Reach -> Common.nullary "Reach"
  Keyword.Shroud -> Common.nullary "Shroud"
  Keyword.Trample -> Common.nullary "Trample"
  Keyword.Vigilance -> Common.nullary "Vigilance"
  Keyword.Cycling cost searchFor -> Common.tagged "Cycling" . Just . Common.array $ [Cost.toJson cost, Common.encodeMaybe Filter.toJson searchFor]
  Keyword.Flashback cost -> Common.tagged "Flashback" . Just $ Cost.toJson cost
  Keyword.Fear -> Common.nullary "Fear"
  Keyword.Entwine cost -> Common.tagged "Entwine" . Just $ Cost.toJson cost
  Keyword.Poisonous n -> Common.tagged "Poisonous" . Just $ Common.encodeNatural n
  Keyword.Infect -> Common.nullary "Infect"
  Keyword.Devoid -> Common.nullary "Devoid"
  Keyword.Toxic n -> Common.tagged "Toxic" . Just $ Common.encodeNatural n

fromJson :: Value.Value -> Either Text.Text Keyword.Keyword
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Deathtouch", _) -> Right Keyword.Deathtouch
    ("Defender", _) -> Right Keyword.Defender
    ("DoubleStrike", _) -> Right Keyword.DoubleStrike
    ("FirstStrike", _) -> Right Keyword.FirstStrike
    ("Flying", _) -> Right Keyword.Flying
    ("Haste", _) -> Right Keyword.Haste
    ("Indestructible", _) -> Right Keyword.Indestructible
    ("Landwalk", Just v) -> Keyword.Landwalk <$> Subtype.fromJson v
    ("Reach", _) -> Right Keyword.Reach
    ("Shroud", _) -> Right Keyword.Shroud
    ("Trample", _) -> Right Keyword.Trample
    ("Vigilance", _) -> Right Keyword.Vigilance
    ("Cycling", Just (Value.Array (Array.MkArray [c, f]))) -> Keyword.Cycling <$> Cost.fromJson c <*> Filter.optional f
    ("Flashback", Just v) -> Keyword.Flashback <$> Cost.fromJson v
    ("Fear", _) -> Right Keyword.Fear
    ("Entwine", Just v) -> Keyword.Entwine <$> Cost.fromJson v
    ("Poisonous", Just v) -> Keyword.Poisonous <$> Common.decodeNatural v
    ("Infect", _) -> Right Keyword.Infect
    ("Devoid", _) -> Right Keyword.Devoid
    ("Toxic", Just v) -> Keyword.Toxic <$> Common.decodeNatural v
    _ -> Left . Text.pack $ "unknown Keyword: " <> t
