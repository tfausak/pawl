{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.KeywordSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.MorphVariant as MorphVariant
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
  -- CR 702.11b's plain hexproof takes no parameter, so it encodes as the bare
  -- tag -- the wire format Slippery Bogle's committed printing already carries,
  -- unchanged by rule 702.11d's quality arriving beside it.
  Spec.it s "Hexproof" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Hexproof Nothing)
      """ {"type":"Hexproof"} """
  -- CR 702.11d's "[quality]" rides the same constructor, so "hexproof from
  -- black" and plain hexproof must encode differently -- a codec that dropped the
  -- quality would round-trip Slippery Bogle unharmed and silently turn Knight of
  -- Grace into it, which is exactly the far-too-strong reading the variant exists
  -- to avoid.
  Spec.it s "Hexproof carries CR 702.11d's quality" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Hexproof (Just (Filter.HasColor Color.Black)))
      """ {"type":"Hexproof","value":{"type":"HasColor","value":{"type":"Black"}}} """
    -- CR 702.16a's "any characteristic value or information": a quality need not
    -- be a colour. Eradicator Valkyrie's "hexproof from planeswalkers".
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Hexproof (Just (Filter.HasCardType CardType.Planeswalker)))
      """ {"type":"Hexproof","value":{"type":"HasCardType","value":{"type":"Planeswalker"}}} """
    Spec.assertBool
      s
      (Keyword.toJson (Keyword.Hexproof Nothing) /= Keyword.toJson (Keyword.Hexproof (Just (Filter.HasColor Color.Black))))
      "hexproof and hexproof from black encode differently"
    Spec.assertBool
      s
      (Keyword.toJson (Keyword.Hexproof (Just (Filter.HasColor Color.Black))) /= Keyword.toJson (Keyword.Hexproof (Just (Filter.HasColor Color.White))))
      "and so do hexproof from black and hexproof from white"
  Spec.it s "Indestructible" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Indestructible
      """ {"type":"Indestructible"} """
  -- CR 702.14a's "[type]" rides the constructor, so swampwalk and islandwalk
  -- are DIFFERENT keywords and must encode differently.
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
  -- The other three shapes CR 702.14c names, which a bare Subtype could not
  -- say: a codec that flattened the criterion back to a subtype would
  -- round-trip the swampwalk above and lose all three.
  Spec.it s "Landwalk carries CR 702.14c's other three shapes" $ do
    -- Dryad Sophisticate: "without the specified type or supertype".
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Landwalk (Filter.Not (Filter.HasSupertype Supertype.Basic)))
      """ {"type":"Landwalk","value":{"type":"Not","value":{"type":"HasSupertype","value":{"type":"Basic"}}}} """
    -- With both the specified type or supertype and the specified subtype.
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Landwalk (Filter.And [Filter.HasSupertype Supertype.Snow, Filter.HasSubtype Subtype.Swamp]))
      """ {"type":"Landwalk","value":{"type":"And","value":[{"type":"HasSupertype","value":{"type":"Snow"}},{"type":"HasSubtype","value":{"type":"Swamp"}}]}} """
    -- With the specified type or supertype: artifact landwalk, which no
    -- creature prints -- it is only ever granted.
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
  -- CR 702.18a's shroud is nullary, so what this pins is the TAG.
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
  -- CR 702.19c is a keyword of its own and not a flavour of the one above, so
  -- the pair is asserted distinct: a fromJson arm that fell through to Trample
  -- would round-trip the tag and quietly drop the variant.
  Spec.it s "TrampleOverPlaneswalkers" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.TrampleOverPlaneswalkers
      """ {"type":"TrampleOverPlaneswalkers"} """
    Spec.assertBool s (Keyword.toJson Keyword.TrampleOverPlaneswalkers /= Keyword.toJson Keyword.Trample) "the variant is not trample"
  Spec.it s "Vigilance" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Vigilance
      """ {"type":"Vigilance"} """
  -- CR 702.22: only the combat-damage-division halves are modeled; see the type.
  Spec.it s "Banding" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Banding
      """ {"type":"Banding"} """
  -- CR 702.26a: nullary, because the rule takes no parameter -- what a phasing
  -- permanent does is entirely the untap step's business.
  Spec.it s "Phasing" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Phasing
      """ {"type":"Phasing"} """
  -- CR 702.28b: nullary, because the rule takes no parameter -- both of its
  -- sentences ask only whether the other creature has the same keyword.
  Spec.it s "Shadow" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Shadow
      """ {"type":"Shadow"} """
  -- CR 702.127a: nullary, because what an aftermath half costs is its own printed
  -- mana cost -- unlike flashback, whose alternative cost rides the constructor.
  Spec.it s "Aftermath" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Aftermath
      """ {"type":"Aftermath"} """
  -- CR 702.29e: the typecycling filter rides the same keyword arm and
  -- is absent for plain cycling, so both spellings have to survive the trip.
  Spec.it s "Cycling round-trips with and without a typecycling filter" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Cycling cost Nothing)
      """ {"type":"Cycling","value":[{"mana":[{"type":"Generic","value":1}]},null]} """
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Cycling cost (Just (Filter.HasCardType CardType.Land)))
      """ {"type":"Cycling","value":[{"mana":[{"type":"Generic","value":1}]},{"type":"HasCardType","value":{"type":"Land"}}]} """
  -- CR 702.34a's payload is a whole Cost, not a number -- the first keyword
  -- whose parameter is itself a composite.
  Spec.it s "Flashback carries its cost" $ do
    let flashback n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (flashback 1)
      """ {"type":"Flashback","value":{"mana":[{"type":"Generic","value":1}]}} """
    Spec.assertBool s (Keyword.toJson (flashback 1) /= Keyword.toJson (flashback 4)) "the cost is part of the encoding"
  Spec.it s "Fear" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Fear
      """ {"type":"Fear"} """
  Spec.it s "Intimidate" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Intimidate
      """ {"type":"Intimidate"} """
  -- CR 702.37a's payload is a whole Cost too -- the MORPH cost, which CR 702.37e
  -- pays to turn the permanent face up, never the {3} the cast pays.
  Spec.it s "Morph carries its cost, and is not Flashback" $ do
    let morph n = Keyword.Morph (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) MorphVariant.Plain
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (morph 1)
      """ {"type":"Morph","value":[{"mana":[{"type":"Generic","value":1}]},{"type":"Plain"}]} """
    Spec.assertBool s (Keyword.toJson (morph 1) /= Keyword.toJson (flashbackOf 1)) "morph {1} is not flashback {1}"
  -- CR 702.37b: megamorph is the SAME constructor with a different variant, so
  -- the two must not encode alike -- a codec that dropped the variant would make
  -- Misthoof Kirin's megamorph {1}{W} indistinguishable from a morph {1}{W}.
  Spec.it s "Morph's variant tells megamorph from plain morph" $ do
    let morphOf = Keyword.Morph (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (morphOf MorphVariant.Mega)
      """ {"type":"Morph","value":[{"mana":[{"type":"Generic","value":1}]},{"type":"Mega"}]} """
    Spec.assertBool s (Keyword.toJson (morphOf MorphVariant.Mega) /= Keyword.toJson (morphOf MorphVariant.Plain)) "megamorph {1} is not morph {1}"
  -- CR 702.42a's payload is a whole Cost too, and it must not share Flashback's
  -- tag.
  Spec.it s "Entwine carries its cost, and is not Flashback" $ do
    let entwine n = Keyword.Entwine (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (entwine 1)
      """ {"type":"Entwine","value":{"mana":[{"type":"Generic","value":1}]}} """
    Spec.assertBool s (Keyword.toJson (entwine 1) /= Keyword.toJson (flashbackOf 1)) "entwine {1} is not flashback {1}"
  -- CR 702.45a's N rides the constructor as poisonous' does.
  Spec.it s "Bushido carries its N" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Bushido 2)
      """ {"type":"Bushido","value":2} """
    Spec.assertBool
      s
      (Keyword.toJson (Keyword.Bushido 3) /= Keyword.toJson (Keyword.Poisonous 3))
      "bushido 3 is not poisonous 3"
  -- CR 702.70a's N rides the constructor the same way, and the two payloaded
  -- keywords must not share a tag.
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
  -- CR 702.86a's N rides the constructor the same way poisonous' does, and the
  -- two must not share a tag either.
  Spec.it s "Annihilator carries its N" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Annihilator 1)
      """ {"type":"Annihilator","value":1} """
    Spec.assertBool
      s
      (Keyword.toJson (Keyword.Annihilator 3) /= Keyword.toJson (Keyword.Poisonous 3))
      "annihilator 3 is not poisonous 3"
  Spec.it s "Infect" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Infect
      """ {"type":"Infect"} """
  -- CR 702.80d makes multiple instances redundant, so wither is a bare tag with
  -- nothing to count.
  Spec.it s "Wither" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Wither
      """ {"type":"Wither"} """
  -- CR 702.91a's battle cry takes no parameter, so it encodes as a bare tag.
  -- What CR 702.91b makes multiple is the COUNT the projection keeps, never the
  -- value.
  Spec.it s "BattleCry" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.BattleCry
      """ {"type":"BattleCry"} """
  -- CR 702.108a's prowess takes no parameter either, and CR 702.108b makes the
  -- COUNT multiple rather than the value.
  Spec.it s "Prowess" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Prowess
      """ {"type":"Prowess"} """
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
  -- CR 702.122a's N rides the constructor, so crew 1 and crew 6 are distinct
  -- keywords and must encode distinguishably.
  Spec.it s "Crew carries its N" $ do
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      (Keyword.Crew 6)
      """ {"type":"Crew","value":6} """
    Spec.assertBool s (Keyword.toJson (Keyword.Crew 1) /= Keyword.toJson (Keyword.Crew 6)) "crew 1 and crew 6 encode differently"
  Spec.it s "Riot" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Riot
      """ {"type":"Riot"} """
  Spec.it s "Daybound" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Daybound
      """ {"type":"Daybound"} """
  Spec.it s "Nightbound" $
    Common.assertJsonCodec
      s
      Keyword.toJson
      Keyword.fromJson
      Keyword.Nightbound
      """ {"type":"Nightbound"} """
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
