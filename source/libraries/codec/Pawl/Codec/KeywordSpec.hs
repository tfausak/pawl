{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.KeywordSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Keyword" $ do
  Spec.it s "Deathtouch" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Deathtouch
      """ {"type":"Deathtouch"} """
  Spec.it s "Defender" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Defender
      """ {"type":"Defender"} """
  Spec.it s "DoubleStrike" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.DoubleStrike
      """ {"type":"DoubleStrike"} """
  Spec.it s "FirstStrike" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.FirstStrike
      """ {"type":"FirstStrike"} """
  Spec.it s "Flash" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Flash
      """ {"type":"Flash"} """
  Spec.it s "Flying" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Flying
      """ {"type":"Flying"} """
  Spec.it s "Haste" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Haste
      """ {"type":"Haste"} """
  Spec.it s "Hexproof" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Hexproof
      """ {"type":"Hexproof"} """
  Spec.it s "Indestructible" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Indestructible
      """ {"type":"Indestructible"} """
  -- CR 702.14a's "[type]" rides the constructor, so swampwalk and islandwalk are
  -- DIFFERENT keywords and must encode differently -- a Bog Wraith that decoded
  -- as an islandwalker would be blockable exactly when it should not be.
  Spec.it s "Landwalk carries a land-type criterion" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Landwalk (Filter.HasSubtype Subtype.Swamp))
      """ {"type":"Landwalk","value":{"type":"HasSubtype","value":{"type":"Swamp"}}} """
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Landwalk (Filter.HasSubtype Subtype.Island))
      """ {"type":"Landwalk","value":{"type":"HasSubtype","value":{"type":"Island"}}} """
    Spec.assertBool
      s
      (Keyword.toJson (Keyword.Landwalk (Filter.HasSubtype Subtype.Swamp)) /= Keyword.toJson (Keyword.Landwalk (Filter.HasSubtype Subtype.Island)))
      "swampwalk and islandwalk encode differently"
  -- The other three shapes CR 702.14c names, which a bare Subtype could not say.
  -- Each has a printing in the pool, and each has to survive the trip intact --
  -- a codec that flattened the criterion back to a subtype would round-trip the
  -- swampwalk above and lose all three.
  Spec.it s "Landwalk carries CR 702.14c's other three shapes" $ do
    -- Dryad Sophisticate: "without the specified type or supertype".
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Landwalk (Filter.Not (Filter.HasSupertype Supertype.Basic)))
      """ {"type":"Landwalk","value":{"type":"Not","value":{"type":"HasSupertype","value":{"type":"Basic"}}}} """
    -- Legions of Lim-Dûl: "with both the specified type or supertype and the
    -- specified subtype".
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Landwalk (Filter.And [Filter.HasSupertype Supertype.Snow, Filter.HasSubtype Subtype.Swamp]))
      """ {"type":"Landwalk","value":{"type":"And","value":[{"type":"HasSupertype","value":{"type":"Snow"}},{"type":"HasSubtype","value":{"type":"Swamp"}}]}} """
    -- "With the specified type or supertype" -- artifact landwalk, whose one
    -- paper source is Vectis Gloves (an Equipment that GRANTS it; no creature
    -- prints it).
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Landwalk (Filter.HasCardType CardType.Artifact))
      """ {"type":"Landwalk","value":{"type":"HasCardType","value":{"type":"Artifact"}}} """
  Spec.it s "Lifelink" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Lifelink
      """ {"type":"Lifelink"} """
  Spec.it s "Reach" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Reach
      """ {"type":"Reach"} """
  -- CR 702.18a's shroud is nullary, so what this pins is the TAG: a Blurred
  -- Mongoose that decoded as anything else would be a legal Doom Blade target.
  Spec.it s "Shroud" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Shroud
      """ {"type":"Shroud"} """
    Spec.assertBool s (Keyword.toJson Keyword.Shroud /= Keyword.toJson Keyword.Trample) "shroud is not trample"
  Spec.it s "Trample" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Trample
      """ {"type":"Trample"} """
  Spec.it s "Vigilance" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Vigilance
      """ {"type":"Vigilance"} """
  -- CR 702.29c's event, carrying the incarnation the cycled card became. CR
  -- 702.29e: the typecycling filter rides the same keyword arm, absent for
  -- plain cycling -- so both spellings have to survive the trip.
  Spec.it s "Cycling round-trips with and without a typecycling filter" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Cycling cost Nothing)
      """ {"type":"Cycling","value":[{"mana":[{"type":"Generic","value":1}],"components":[]},null]} """
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Cycling cost (Just (Filter.HasCardType CardType.Land)))
      """ {"type":"Cycling","value":[{"mana":[{"type":"Generic","value":1}],"components":[]},{"type":"HasCardType","value":{"type":"Land"}}]} """
  -- CR 702.34a's payload is a whole Cost, not a number -- the first keyword
  -- whose parameter is itself a composite.
  Spec.it s "Flashback carries its cost" $ do
    let flashback n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (flashback 1)
      """ {"type":"Flashback","value":{"mana":[{"type":"Generic","value":1}],"components":[]}} """
    Spec.assertBool s (Keyword.toJson (flashback 1) /= Keyword.toJson (flashback 4)) "the cost is part of the encoding"
  Spec.it s "Fear" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Fear
      """ {"type":"Fear"} """
  -- CR 702.42a's payload is a whole Cost too, and it must not share Flashback's
  -- tag: Dream's Grip may not decode as a card castable from a graveyard.
  Spec.it s "Entwine carries its cost, and is not Flashback" $ do
    let entwine n = Keyword.Entwine (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (entwine 1)
      """ {"type":"Entwine","value":{"mana":[{"type":"Generic","value":1}],"components":[]}} """
    Spec.assertBool s (Keyword.toJson (entwine 1) /= Keyword.toJson (flashbackOf 1)) "entwine {1} is not flashback {1}"
  -- CR 702.70a's N rides the constructor the same way. The two payloaded
  -- keywords must not share a tag, or Snake Cult Initiation would decode as
  -- toxic 3.
  Spec.it s "Poisonous carries its N" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Poisonous 1)
      """ {"type":"Poisonous","value":1} """
    Spec.assertBool
      s
      (Keyword.toJson (Keyword.Poisonous 3) /= Keyword.toJson (Keyword.Toxic 3))
      "poisonous 3 is not toxic 3"
  Spec.it s "Infect" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Infect
      """ {"type":"Infect"} """
  -- CR 702.91a's battle cry is a triggered ability, but it takes no parameter,
  -- so it encodes as a bare tag like every other nullary keyword -- what CR
  -- 702.91b makes multiple is the COUNT the projection keeps, never the value.
  Spec.it s "BattleCry" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.BattleCry
      """ {"type":"BattleCry"} """
  Spec.it s "Menace" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Menace
      """ {"type":"Menace"} """
  Spec.it s "Devoid" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Devoid
      """ {"type":"Devoid"} """
  -- CR 702.164a's N rides the constructor, so this is the first keyword that is
  -- not a bare tag.
  Spec.it s "Toxic carries its N" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Toxic 1)
      """ {"type":"Toxic","value":1} """
    Spec.assertBool s (Keyword.toJson (Keyword.Toxic 1) /= Keyword.toJson (Keyword.Toxic 2)) "toxic 1 and toxic 2 encode differently"
