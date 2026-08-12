{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerEffectSpec where

import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerEffect" $ do
  -- CR 601.3 / Silence.
  Spec.it s "CantCastSpells" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      PlayerEffect.CantCastSpells
      """ {"type":"CantCastSpells"} """
  -- CR 601.3 / Rule of Law.
  Spec.it s "CantCastMoreThan" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantCastMoreThan 1)
      """ {"type":"CantCastMoreThan","value":1} """
  -- CR 601.3 / Null Chamber's cast half. Payload-free: the names come from the
  -- source's Object.chosenNames, which no card can write.
  Spec.it s "CantCastChosenName" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      PlayerEffect.CantCastChosenName
      """ {"type":"CantCastChosenName"} """
  -- CR 305.1 / Null Chamber's play half, which is a different gate.
  Spec.it s "CantPlayLandChosenName" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      PlayerEffect.CantPlayLandChosenName
      """ {"type":"CantPlayLandChosenName"} """
  -- CR 613.11 / 601.2f / Thalia.
  Spec.it s "IncreaseSpellCost" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.IncreaseSpellCost (Filter.Not (Filter.HasCardType CardType.Creature)) 1)
      """ {"type":"IncreaseSpellCost","value":[{"type":"Not","value":{"type":"HasCardType","value":{"type":"Creature"}}},1]} """
  -- CR 613.11 / 601.2f / Sapphire Medallion.
  Spec.it s "ReduceSpellCost" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.ReduceSpellCost (Filter.HasColor Color.Blue) (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
      """ {"type":"ReduceSpellCost","value":[{"type":"HasColor","value":{"type":"Blue"}},[{"type":"Generic","value":1}]]} """
  -- The reduction that names a mana type, which the generic one above would not
  -- catch a regression in.
  Spec.it s "ReduceSpellCost, naming a mana type" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      ( PlayerEffect.ReduceSpellCost
          (Filter.HasSubtype Subtype.Cleric)
          (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White), ManaSymbol.OfType (ManaType.Colored Color.Black)])
      )
      """ {"type":"ReduceSpellCost","value":[{"type":"HasSubtype","value":{"type":"Cleric"}},[{"type":"OfType","value":{"type":"Colored","value":{"type":"White"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}]]} """
  -- CR 613.11 / 601.2f / Heartstone, floor and all.
  Spec.it s "ReduceActivationCost" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.ReduceActivationCost (Filter.HasCardType CardType.Creature) (ManaCost.MkManaCost [ManaSymbol.Generic 1]) 1)
      """ {"type":"ReduceActivationCost","value":[{"type":"HasCardType","value":{"type":"Creature"}},[{"type":"Generic","value":1}],1]} """
  -- Training Grounds' amount and floor, which differ from each other -- a codec
  -- that swapped the two payloads would round-trip Heartstone's above and not
  -- this one.
  Spec.it s "ReduceActivationCost, Training Grounds' two" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.ReduceActivationCost (Filter.HasCardType CardType.Creature) (ManaCost.MkManaCost [ManaSymbol.Generic 2]) 1)
      """ {"type":"ReduceActivationCost","value":[{"type":"HasCardType","value":{"type":"Creature"}},[{"type":"Generic","value":2}],1]} """
  -- CR 305.2 / Exploration.
  Spec.it s "PlayAdditionalLands, Exploration's one" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.PlayAdditionalLands 1)
      """ {"type":"PlayAdditionalLands","value":1} """
  -- CR 305.2 / Azusa, Lost but Seeking: the OTHER amount, so a codec that
  -- dropped the payload would round-trip one of these and not both.
  Spec.it s "PlayAdditionalLands, Azusa's two" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.PlayAdditionalLands 2)
      """ {"type":"PlayAdditionalLands","value":2} """
  -- CR 402.2 / Reliquary Tower.
  Spec.it s "NoMaximumHandSize" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      PlayerEffect.NoMaximumHandSize
      """ {"type":"NoMaximumHandSize"} """
  -- CR 402.2 / The Ten Rings. This is the shape data/cards/the-ten-rings.json
  -- carries.
  Spec.it s "SetMaximumHandSize, The Ten Rings' ten" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.SetMaximumHandSize 10)
      """ {"type":"SetMaximumHandSize","value":10} """
  -- CR 402.2 / Cursed Rack: the OTHER number, so a codec that dropped the payload
  -- would round-trip one of these and not both.
  Spec.it s "SetMaximumHandSize, Cursed Rack's four" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.SetMaximumHandSize 4)
      """ {"type":"SetMaximumHandSize","value":4} """
  -- CR 500.5 / 703.4q / Upwelling: no mana type named.
  Spec.it s "DontLoseUnspentMana, Upwelling's whole pool" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.DontLoseUnspentMana ManaFilter.Any)
      """ {"type":"DontLoseUnspentMana","value":{"type":"Any"}} """
  -- CR 106.1a: the OTHER filter, so a codec that dropped the payload would
  -- round-trip one of these and not both.
  Spec.it s "DontLoseUnspentMana, Omnath's green only" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.DontLoseUnspentMana (ManaFilter.OfType (ManaType.Colored Color.Green)))
      """ {"type":"DontLoseUnspentMana","value":{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}} """
  -- CR 702.18a / Ivory Mask: shroud names no player, so the scope is everybody.
  Spec.it s "CantBeTargetedBy, shroud's EachPlayer" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantBeTargetedBy PlayerScope.EachPlayer)
      """ {"type":"CantBeTargetedBy","value":{"type":"EachPlayer"}} """
  -- CR 702.11c: the OTHER scope, so a codec that dropped the payload would
  -- round-trip one of these and not both.
  Spec.it s "CantBeTargetedBy, hexproof's Opponents" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantBeTargetedBy PlayerScope.Opponents)
      """ {"type":"CantBeTargetedBy","value":{"type":"Opponents"}} """
  -- CR 601.3b / Vedalken Orrery: "spells" names no quality, so the filter is the
  -- trivial predicate. This is the shape data/cards/vedalken-orrery.json carries.
  Spec.it s "CastAsThoughItHadFlash, Vedalken Orrery's unqualified spells" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CastAsThoughItHadFlash (Filter.And []))
      """ {"type":"CastAsThoughItHadFlash","value":{"type":"And","value":[]}} """
  -- CR 601.3b's "certain qualities" (Yeva, Nature's Herald), so a codec that
  -- dropped the payload would round-trip one of these and not both.
  Spec.it s "CastAsThoughItHadFlash, a filter that names qualities" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CastAsThoughItHadFlash (Filter.And [Filter.HasColor Color.Green, Filter.HasCardType CardType.Creature]))
      """ {"type":"CastAsThoughItHadFlash","value":{"type":"And","value":[{"type":"HasColor","value":{"type":"Green"}},{"type":"HasCardType","value":{"type":"Creature"}}]}} """
  -- CR 701.6a, twice for CastAsThoughItHadFlash's reason: Spider-Punk narrows by
  -- nothing and Prowling Serpopard narrows by a card type, so a codec that
  -- dropped the payload would round-trip one of these and not both. WHOSE spells
  -- is the CARRIER's scope (Pawl.Codec.PlayerStaticAbility) and never rides here.
  Spec.it s "CantBeCountered, an empty filter" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantBeCountered (Filter.And []))
      """ {"type":"CantBeCountered","value":{"type":"And","value":[]}} """
  Spec.it s "CantBeCountered, a filter that names qualities" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantBeCountered (Filter.HasCardType CardType.Creature))
      """ {"type":"CantBeCountered","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- CR 615.12 / Spider-Punk, whose sentence names no quality of the damage: the
  -- pattern that admits everything, whose every field is its default, so the
  -- payload is an empty object rather than absent. WHOSE damage is the carrier's
  -- scope (Pawl.Codec.PlayerStaticAbility) rather than anything riding here.
  Spec.it s "DamageCantBePrevented, naming no quality of the damage" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.DamageCantBePrevented (DamagePattern.MkDamagePattern Nothing (Filter.And []) Nothing))
      """ {"type":"DamageCantBePrevented","value":{}} """
  -- CR 615.12 narrowed / Excruciator, "damage that would be dealt by this
  -- creature": the same effect keyed to its own source (CR 614.15's relation),
  -- which is what makes the payload worth carrying.
  Spec.it s "DamageCantBePrevented, naming its own source" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.DamageCantBePrevented (DamagePattern.MkDamagePattern Nothing Filter.IsSource Nothing))
      """ {"type":"DamageCantBePrevented","value":{"whatSource":{"type":"IsSource"}}} """
  -- CR 701.23 / Leonin Arbiter.
  Spec.it s "CantSearchLibraries" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      PlayerEffect.CantSearchLibraries
      """ {"type":"CantSearchLibraries"} """
  -- CR 725 / Jared Carthalion, True Heir.
  Spec.it s "CantBecomeMonarch" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      PlayerEffect.CantBecomeMonarch
      """ {"type":"CantBecomeMonarch"} """
  -- CR 601.3a / Damping Engine's cast half.
  Spec.it s "CantCastMatching" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantCastMatching (Filter.Or [Filter.HasCardType CardType.Artifact, Filter.HasCardType CardType.Creature]))
      """ {"type":"CantCastMatching","value":{"type":"Or","value":[{"type":"HasCardType","value":{"type":"Artifact"}},{"type":"HasCardType","value":{"type":"Creature"}}]}} """
  -- CR 305.1 / Damping Engine's land half.
  Spec.it s "CantPlayLands" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      PlayerEffect.CantPlayLands
      """ {"type":"CantPlayLands"} """
  -- CR 601.3 / Yawgmoth's Will, whose sentence names no quality of the spell.
  Spec.it s "CastFromGraveyard, an empty filter" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CastFromGraveyard (Filter.And []))
      """ {"type":"CastFromGraveyard","value":{"type":"And","value":[]}} """
  -- CR 601.3 narrowed / Haakon, Stromgald Scourge's "Knight spells".
  Spec.it s "CastFromGraveyard, a filter that names qualities" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CastFromGraveyard (Filter.HasCardType CardType.Creature))
      """ {"type":"CastFromGraveyard","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
