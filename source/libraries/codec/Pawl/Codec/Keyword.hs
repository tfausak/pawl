module Pawl.Codec.Keyword where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.MorphVariant as MorphVariant
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
  Keyword.TrampleOverPlaneswalkers -> Common.nullary "TrampleOverPlaneswalkers"
  Keyword.Vigilance -> Common.nullary "Vigilance"
  Keyword.Banding -> Common.nullary "Banding"
  Keyword.Rampage n -> Common.tagged "Rampage" . Just $ Common.encodeNatural n
  Keyword.Flanking -> Common.nullary "Flanking"
  Keyword.Phasing -> Common.nullary "Phasing"
  Keyword.Shadow -> Common.nullary "Shadow"
  Keyword.Horsemanship -> Common.nullary "Horsemanship"
  Keyword.Aftermath -> Common.nullary "Aftermath"
  Keyword.Afflict n -> Common.tagged "Afflict" . Just $ Common.encodeNatural n
  Keyword.Cycling cost searchFor -> Common.tagged "Cycling" . Just . Common.array $ [Cost.toJson toJson cost, Common.encodeMaybe (Filter.toJson toJson) searchFor]
  Keyword.Flashback cost -> Common.tagged "Flashback" . Just $ Cost.toJson toJson cost
  Keyword.Fear -> Common.nullary "Fear"
  Keyword.Intimidate -> Common.nullary "Intimidate"
  -- An ARRAY, as Cycling's is, because CR 702.37b's megamorph is the same
  -- keyword with a second field rather than a tag of its own.
  Keyword.Morph cost variant -> Common.tagged "Morph" . Just . Common.array $ [Cost.toJson toJson cost, MorphVariant.toJson variant]
  Keyword.Entwine cost -> Common.tagged "Entwine" . Just $ Cost.toJson toJson cost
  Keyword.Modular n -> Common.tagged "Modular" . Just $ Common.encodeNatural n
  Keyword.Bushido n -> Common.tagged "Bushido" . Just $ Common.encodeNatural n
  Keyword.Vanishing n -> Common.tagged "Vanishing" . Just $ Common.encodeNatural n
  Keyword.Poisonous n -> Common.tagged "Poisonous" . Just $ Common.encodeNatural n
  Keyword.Annihilator n -> Common.tagged "Annihilator" . Just $ Common.encodeNatural n
  Keyword.Infect -> Common.nullary "Infect"
  Keyword.Wither -> Common.nullary "Wither"
  Keyword.Exalted -> Common.nullary "Exalted"
  Keyword.Mentor -> Common.nullary "Mentor"
  Keyword.Provoke -> Common.nullary "Provoke"
  Keyword.BattleCry -> Common.nullary "BattleCry"
  Keyword.Evolve -> Common.nullary "Evolve"
  Keyword.Outlast cost -> Common.tagged "Outlast" . Just $ Cost.toJson toJson cost
  Keyword.Prowess -> Common.nullary "Prowess"
  Keyword.Menace -> Common.nullary "Menace"
  Keyword.Renown n -> Common.tagged "Renown" . Just $ Common.encodeNatural n
  Keyword.Devoid -> Common.nullary "Devoid"
  Keyword.Skulk -> Common.nullary "Skulk"
  Keyword.Melee -> Common.nullary "Melee"
  Keyword.Crew n -> Common.tagged "Crew" . Just $ Common.encodeNatural n
  Keyword.Riot -> Common.nullary "Riot"
  Keyword.Daybound -> Common.nullary "Daybound"
  Keyword.Nightbound -> Common.nullary "Nightbound"
  Keyword.Training -> Common.nullary "Training"
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
    ("TrampleOverPlaneswalkers", _) -> Right Keyword.TrampleOverPlaneswalkers
    ("Vigilance", _) -> Right Keyword.Vigilance
    ("Banding", _) -> Right Keyword.Banding
    ("Rampage", Just v) -> Keyword.Rampage <$> Common.decodeNatural v
    ("Flanking", _) -> Right Keyword.Flanking
    ("Phasing", _) -> Right Keyword.Phasing
    ("Shadow", _) -> Right Keyword.Shadow
    ("Horsemanship", _) -> Right Keyword.Horsemanship
    ("Aftermath", _) -> Right Keyword.Aftermath
    ("Afflict", Just v) -> Keyword.Afflict <$> Common.decodeNatural v
    ("Cycling", Just (Value.Array (Array.MkArray [c, f]))) -> Keyword.Cycling <$> Cost.fromJson fromJson c <*> Filter.optional fromJson f
    ("Flashback", Just v) -> Keyword.Flashback <$> Cost.fromJson fromJson v
    ("Fear", _) -> Right Keyword.Fear
    ("Intimidate", _) -> Right Keyword.Intimidate
    ("Morph", Just (Value.Array (Array.MkArray [c, v]))) -> Keyword.Morph <$> Cost.fromJson fromJson c <*> MorphVariant.fromJson v
    ("Entwine", Just v) -> Keyword.Entwine <$> Cost.fromJson fromJson v
    ("Modular", Just v) -> Keyword.Modular <$> Common.decodeNatural v
    ("Bushido", Just v) -> Keyword.Bushido <$> Common.decodeNatural v
    ("Vanishing", Just v) -> Keyword.Vanishing <$> Common.decodeNatural v
    ("Poisonous", Just v) -> Keyword.Poisonous <$> Common.decodeNatural v
    ("Annihilator", Just v) -> Keyword.Annihilator <$> Common.decodeNatural v
    ("Infect", _) -> Right Keyword.Infect
    ("Wither", _) -> Right Keyword.Wither
    ("Exalted", _) -> Right Keyword.Exalted
    ("Mentor", _) -> Right Keyword.Mentor
    ("Provoke", _) -> Right Keyword.Provoke
    ("BattleCry", _) -> Right Keyword.BattleCry
    ("Evolve", _) -> Right Keyword.Evolve
    ("Outlast", Just v) -> Keyword.Outlast <$> Cost.fromJson fromJson v
    ("Prowess", _) -> Right Keyword.Prowess
    ("Menace", _) -> Right Keyword.Menace
    ("Renown", Just v) -> Keyword.Renown <$> Common.decodeNatural v
    ("Devoid", _) -> Right Keyword.Devoid
    ("Skulk", _) -> Right Keyword.Skulk
    ("Melee", _) -> Right Keyword.Melee
    ("Crew", Just v) -> Keyword.Crew <$> Common.decodeNatural v
    ("Riot", _) -> Right Keyword.Riot
    ("Daybound", _) -> Right Keyword.Daybound
    ("Nightbound", _) -> Right Keyword.Nightbound
    ("Training", _) -> Right Keyword.Training
    ("Toxic", Just v) -> Keyword.Toxic <$> Common.decodeNatural v
    ("StartYourEngines", _) -> Right Keyword.StartYourEngines
    _ -> Left . Text.pack $ "unknown Keyword: " <> t
