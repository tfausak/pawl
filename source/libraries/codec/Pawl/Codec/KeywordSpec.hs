{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.KeywordSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.MorphVariant as MorphVariant
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Keyword" $ do
  Spec.it s "Deathtouch" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Deathtouch
      """ {"type":"Deathtouch"} """
  Spec.it s "Defender" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Defender
      """ {"type":"Defender"} """
  Spec.it s "DoubleStrike" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.DoubleStrike
      """ {"type":"DoubleStrike"} """
  Spec.it s "FirstStrike" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.FirstStrike
      """ {"type":"FirstStrike"} """
  Spec.it s "Flash" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Flash
      """ {"type":"Flash"} """
  Spec.it s "Flying" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Flying
      """ {"type":"Flying"} """
  Spec.it s "Haste" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Haste
      """ {"type":"Haste"} """
  -- CR 702.11b's plain hexproof takes no parameter, so it encodes as the bare
  -- tag -- the wire format Slippery Bogle's committed printing already carries,
  -- unchanged by rule 702.11d's quality arriving beside it.
  Spec.it s "Hexproof" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Hexproof Nothing)
      """ {"type":"Hexproof"} """
  -- CR 702.11d's "[quality]" rides the same constructor, so "hexproof from
  -- black" and plain hexproof must encode differently -- a codec that dropped the
  -- quality would round-trip Slippery Bogle unharmed and silently turn Knight of
  -- Grace into it, which is exactly the far-too-strong reading the variant exists
  -- to avoid.
  Spec.it s "Hexproof carries CR 702.11d's quality" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Hexproof (Just (Filter.HasColor Color.Black)))
      """ {"type":"Hexproof","value":{"type":"HasColor","value":{"type":"Black"}}} """
    -- CR 702.16a's "any characteristic value or information": a quality need not
    -- be a colour. Eradicator Valkyrie's "hexproof from planeswalkers".
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Hexproof (Just (Filter.HasCardType CardType.Planeswalker)))
      """ {"type":"Hexproof","value":{"type":"HasCardType","value":{"type":"Planeswalker"}}} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Hexproof Nothing) /= Codec.encode Keyword.codec (Keyword.Hexproof (Just (Filter.HasColor Color.Black))))
      "hexproof and hexproof from black encode differently"
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Hexproof (Just (Filter.HasColor Color.Black))) /= Codec.encode Keyword.codec (Keyword.Hexproof (Just (Filter.HasColor Color.White))))
      "and so do hexproof from black and hexproof from white"
  Spec.it s "Indestructible" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Indestructible
      """ {"type":"Indestructible"} """
  -- CR 702.14a's "[type]" rides the constructor, so swampwalk and islandwalk
  -- are DIFFERENT keywords and must encode differently.
  Spec.it s "Landwalk carries a land-type criterion" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.HasSubtype Subtype.Swamp))
      """ {"type":"Landwalk","value":{"type":"HasSubtype","value":{"type":"Swamp"}}} """
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.HasSubtype Subtype.Island))
      """ {"type":"Landwalk","value":{"type":"HasSubtype","value":{"type":"Island"}}} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Landwalk (Filter.HasSubtype Subtype.Swamp)) /= Codec.encode Keyword.codec (Keyword.Landwalk (Filter.HasSubtype Subtype.Island)))
      "swampwalk and islandwalk encode differently"
  -- The other three shapes CR 702.14c names, which a bare Subtype could not
  -- say: a codec that flattened the criterion back to a subtype would
  -- round-trip the swampwalk above and lose all three.
  Spec.it s "Landwalk carries CR 702.14c's other three shapes" $ do
    -- Dryad Sophisticate: "without the specified type or supertype".
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.Not (Filter.HasSupertype Supertype.Basic)))
      """ {"type":"Landwalk","value":{"type":"Not","value":{"type":"HasSupertype","value":{"type":"Basic"}}}} """
    -- With both the specified type or supertype and the specified subtype.
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.And [Filter.HasSupertype Supertype.Snow, Filter.HasSubtype Subtype.Swamp]))
      """ {"type":"Landwalk","value":{"type":"And","value":[{"type":"HasSupertype","value":{"type":"Snow"}},{"type":"HasSubtype","value":{"type":"Swamp"}}]}} """
    -- With the specified type or supertype: artifact landwalk, which no
    -- creature prints -- it is only ever granted.
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.HasCardType CardType.Artifact))
      """ {"type":"Landwalk","value":{"type":"HasCardType","value":{"type":"Artifact"}}} """
  Spec.it s "Lifelink" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Lifelink
      """ {"type":"Lifelink"} """
  Spec.it s "Reach" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Reach
      """ {"type":"Reach"} """
  -- CR 702.18a's shroud is nullary, so what this pins is the TAG.
  Spec.it s "Shroud" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Shroud
      """ {"type":"Shroud"} """
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Shroud /= Codec.encode Keyword.codec Keyword.Trample) "shroud is not trample"
  Spec.it s "Trample" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Trample
      """ {"type":"Trample"} """
  -- CR 702.19c is a keyword of its own and not a flavour of the one above, so
  -- the pair is asserted distinct: an arm that decoded to Trample instead
  -- would round-trip the tag and quietly drop the variant.
  Spec.it s "TrampleOverPlaneswalkers" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.TrampleOverPlaneswalkers
      """ {"type":"TrampleOverPlaneswalkers"} """
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.TrampleOverPlaneswalkers /= Codec.encode Keyword.codec Keyword.Trample) "the variant is not trample"
  Spec.it s "Vigilance" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Vigilance
      """ {"type":"Vigilance"} """
  -- CR 702.21a's payload is a Cost, and must not share Flashback's or Plot's tag:
  -- ward's is paid by an OPPONENT as the minted trigger resolves, where every
  -- other cost-bearing keyword names a cost its own controller pays.
  Spec.it s "Ward carries its cost, and is not Flashback" $ do
    let ward n = Keyword.Ward (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (ward 2)
      """ {"type":"Ward","value":{"mana":[{"type":"Generic","value":2}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (ward 2) /= Codec.encode Keyword.codec (flashbackOf 2)) "the same cost under two keywords encodes differently"
  -- CR 702.22: only the combat-damage-division halves are modeled; see the type.
  Spec.it s "Banding" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Banding
      """ {"type":"Banding"} """
  -- CR 702.25a: nullary, because the rule takes no parameter -- its "without
  -- flanking" is a Filter over the blocker in the ability this mints, not a
  -- payload.
  Spec.it s "Flanking" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Flanking
      """ {"type":"Flanking"} """
  -- CR 702.26a: nullary, because the rule takes no parameter -- what a phasing
  -- permanent does is entirely the untap step's business.
  Spec.it s "Phasing" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Phasing
      """ {"type":"Phasing"} """
  -- CR 702.28b: nullary, because the rule takes no parameter -- both of its
  -- sentences ask only whether the other creature has the same keyword.
  Spec.it s "Shadow" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Shadow
      """ {"type":"Shadow"} """
  -- CR 702.31b: nullary, because the rule takes no parameter -- the only thing it
  -- asks about a blocker is whether it has horsemanship too.
  Spec.it s "Horsemanship" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Horsemanship
      """ {"type":"Horsemanship"} """
  -- CR 702.127a: nullary, because what an aftermath half costs is its own printed
  -- mana cost -- unlike flashback, whose alternative cost rides the constructor.
  Spec.it s "Aftermath" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Aftermath
      """ {"type":"Aftermath"} """
  -- CR 702.133a: nullary for aftermath's reason and one more -- the discard the
  -- rule names is the rule's, not the card's, so there is no payload to carry.
  Spec.it s "JumpStart" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.JumpStart
      """ {"type":"JumpStart"} """
  -- CR 702.29e: the typecycling filter rides the same keyword arm and
  -- is absent for plain cycling, so both spellings have to survive the trip.
  Spec.it s "Cycling round-trips with and without a typecycling filter" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Cycling (Cycling.MkCycling cost Nothing))
      """ {"type":"Cycling","value":{"cost":{"mana":[{"type":"Generic","value":1}]},"searchFor":null}} """
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Cycling (Cycling.MkCycling cost (Just (Filter.HasCardType CardType.Land))))
      """ {"type":"Cycling","value":{"cost":{"mana":[{"type":"Generic","value":1}]},"searchFor":{"type":"HasCardType","value":{"type":"Land"}}}} """
  -- CR 702.34a's payload is a whole Cost, not a number -- the first keyword
  -- whose parameter is itself a composite.
  Spec.it s "Flashback carries its cost" $ do
    let flashback n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (flashback 1)
      """ {"type":"Flashback","value":{"mana":[{"type":"Generic","value":1}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (flashback 1) /= Codec.encode Keyword.codec (flashback 4)) "the cost is part of the encoding"
  -- CR 702.170a's payload is a whole Cost too, and it must not share Flashback's
  -- tag: the two name different costs on the same card -- flashback's is the
  -- cast's and plot's is the special action's.
  Spec.it s "Plot carries its cost, and is not Flashback" $ do
    let plot n = Keyword.Plot (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (plot 3)
      """ {"type":"Plot","value":{"mana":[{"type":"Generic","value":3}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (plot 3) /= Codec.encode Keyword.codec (flashbackOf 3)) "the same cost under two keywords encodes differently"
  -- CR 702.143a's payload is a Cost too, and must not share Plot's tag: the two
  -- name costs of opposite halves -- plot's is CR 116.2k's special action and
  -- foretell's is the later cast, where CR 116.2h fixes the action at {2}.
  Spec.it s "Foretell carries its cost, and is not Plot" $ do
    let foretell n = Keyword.Foretell (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        plotOf n = Keyword.Plot (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (foretell 1)
      """ {"type":"Foretell","value":{"mana":[{"type":"Generic","value":1}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (foretell 1) /= Codec.encode Keyword.codec (plotOf 1)) "the same cost under two keywords encodes differently"
  -- CR 702.94a's payload is a Cost too, and must not share Plot's or Flashback's
  -- tag: all three name a cost on a card, and miracle's is the one CR 118.9
  -- alternative the reveal window offers.
  Spec.it s "Miracle carries its cost, and is not Plot" $ do
    let miracle n = Keyword.Miracle (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        plotOf n = Keyword.Plot (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (miracle 2)
      """ {"type":"Miracle","value":{"mana":[{"type":"Generic","value":2}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (miracle 2) /= Codec.encode Keyword.codec (plotOf 2)) "the same cost under two keywords encodes differently"
  -- CR 702.107a's payload is a Cost too, Flashback's shape rather than Crew's
  -- Natural.
  Spec.it s "Outlast carries its cost" $ do
    let outlast n = Keyword.Outlast (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (outlast 1)
      """ {"type":"Outlast","value":{"mana":[{"type":"Generic","value":1}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (outlast 1) /= Codec.encode Keyword.codec (outlast 4)) "the cost is part of the encoding"
  Spec.it s "Fear" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Fear
      """ {"type":"Fear"} """
  Spec.it s "Intimidate" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Intimidate
      """ {"type":"Intimidate"} """
  -- CR 702.37a's payload is a whole Cost too -- the MORPH cost, which CR 702.37e
  -- pays to turn the permanent face up, never the {3} the cast pays.
  Spec.it s "Morph carries its cost, and is not Flashback" $ do
    let morph n = Keyword.Morph (Morph.MkMorph (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) MorphVariant.Plain)
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (morph 1)
      """ {"type":"Morph","value":{"cost":{"mana":[{"type":"Generic","value":1}]},"variant":{"type":"Plain"}}} """
    Spec.assertBool s (Codec.encode Keyword.codec (morph 1) /= Codec.encode Keyword.codec (flashbackOf 1)) "morph {1} is not flashback {1}"
  -- CR 702.37b: megamorph is the SAME constructor with a different variant, so
  -- the two must not encode alike -- a codec that dropped the variant would make
  -- Misthoof Kirin's megamorph {1}{W} indistinguishable from a morph {1}{W}.
  Spec.it s "Morph's variant tells megamorph from plain morph" $ do
    let morphOf = Keyword.Morph . Morph.MkMorph (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])
    Common.assertCodec
      s
      Keyword.codec
      (morphOf MorphVariant.Mega)
      """ {"type":"Morph","value":{"cost":{"mana":[{"type":"Generic","value":1}]},"variant":{"type":"Mega"}}} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (morphOf MorphVariant.Mega) /= Codec.encode Keyword.codec (morphOf MorphVariant.Plain))
      "megamorph {1} is not morph {1}"
  -- CR 702.33a's payload is a whole Cost too, and it must not share Flashback's
  -- tag either.
  Spec.it s "Kicker carries its cost, and is not Flashback" $ do
    let kicker n = Keyword.Kicker (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (kicker 4)
      """ {"type":"Kicker","value":{"mana":[{"type":"Generic","value":4}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (kicker 4) /= Codec.encode Keyword.codec (flashbackOf 4)) "kicker {4} is not flashback {4}"
  -- CR 702.42a's payload is a whole Cost too, and it must not share Flashback's
  -- tag.
  Spec.it s "Entwine carries its cost, and is not Flashback" $ do
    let entwine n = Keyword.Entwine (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (entwine 1)
      """ {"type":"Entwine","value":{"mana":[{"type":"Generic","value":1}]}} """
    Spec.assertBool s (Codec.encode Keyword.codec (entwine 1) /= Codec.encode Keyword.codec (flashbackOf 1)) "entwine {1} is not flashback {1}"
  -- CR 702.45a's N rides the constructor as poisonous' does.
  Spec.it s "Bushido carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Bushido 2)
      """ {"type":"Bushido","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Bushido 3) /= Codec.encode Keyword.codec (Keyword.Poisonous 3))
      "bushido 3 is not poisonous 3"
  -- CR 702.43a's N is a COUNT OF COUNTERS too, so the tag is again the only thing
  -- separating modular 2 from bushido 2 on the wire.
  Spec.it s "Modular carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Modular 2)
      """ {"type":"Modular","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Modular 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "modular 3 is not bushido 3"
  -- CR 702.63a's N is a COUNT OF COUNTERS rather than a size or a threshold, and
  -- the wire cannot tell those apart -- so the tag is all that keeps vanishing 2
  -- from bushido 2.
  Spec.it s "Vanishing carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Vanishing 2)
      """ {"type":"Vanishing","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Vanishing 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "vanishing 3 is not bushido 3"
  -- CR 702.32a's N is the same kind of number as rule 702.63a's, and the two
  -- keywords differ in more than the counter's name -- so the tag is what keeps
  -- fading 2 from vanishing 2 as well.
  Spec.it s "Fading carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Fading 2)
      """ {"type":"Fading","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Fading 3) /= Codec.encode Keyword.codec (Keyword.Vanishing 3))
      "fading 3 is not vanishing 3"
  Spec.it s "SplitSecond" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.SplitSecond
      """ {"type":"SplitSecond"} """
  -- CR 702.68a's N rides the constructor as CR 702.45a's does, and the tag is
  -- what keeps `Frenzy 2` off bushido's wire form.
  Spec.it s "Frenzy carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Frenzy 1)
      """ {"type":"Frenzy","value":1} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Frenzy 2) /= Codec.encode Keyword.codec (Keyword.Bushido 2))
      "frenzy 2 is not bushido 2"
  -- CR 702.70a's N rides the constructor the same way, and the two payloaded
  -- keywords must not share a tag.
  Spec.it s "Poisonous carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Poisonous 1)
      """ {"type":"Poisonous","value":1} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Poisonous 3) /= Codec.encode Keyword.codec (Keyword.Toxic 3))
      "poisonous 3 is not toxic 3"
  -- CR 702.112a's N is written like every other keyword's, so the TAG is what
  -- keeps them apart: `Renown 2` and `Poisonous 2` differ only by it on the wire.
  Spec.it s "Renown carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Renown 2)
      """ {"type":"Renown","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Renown 2) /= Codec.encode Keyword.codec (Keyword.Poisonous 2))
      "renown 2 is not poisonous 2"
  -- CR 702.135a's N is written like the rest, so it must not collide either.
  Spec.it s "Afterlife carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Afterlife 2)
      """ {"type":"Afterlife","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Afterlife 2) /= Codec.encode Keyword.codec (Keyword.Renown 2))
      "afterlife 2 is not renown 2"
  -- CR 702.46a's N is written like the rest, and it is a MANA VALUE BOUND rather
  -- than a count, so a collision with a same-numbered keyword would be a real
  -- misread.
  Spec.it s "Soulshift carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Soulshift 3)
      """ {"type":"Soulshift","value":3} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Soulshift 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "soulshift 3 is not bushido 3"
  -- CR 702.54a's N is a count of +1/+1 counters, so a collision with a
  -- same-numbered keyword would be a real misread here too.
  Spec.it s "Bloodthirst carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Bloodthirst 1)
      """ {"type":"Bloodthirst","value":1} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Bloodthirst 1) /= Codec.encode Keyword.codec (Keyword.Modular 1))
      "bloodthirst 1 is not modular 1"
  -- CR 702.55a's haunt writes no payload at all -- the haunted object is board
  -- state (GameState.haunting), not a field of the keyword.
  Spec.it s "Haunt" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Haunt
      """ {"type":"Haunt"} """
  -- CR 702.77a writes BOTH an N and a cost, so the array carries two fields the
  -- way cycling's and morph's do, and both must survive the round trip.
  Spec.it s "Reinforce carries its N and its cost" $ do
    let reinforce n g = Keyword.Reinforce (Reinforce.MkReinforce n (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic g])) []))
    Common.assertCodec
      s
      Keyword.codec
      (reinforce 2 1)
      """ {"type":"Reinforce","value":{"amount":2,"cost":{"mana":[{"type":"Generic","value":1}]}}} """
    Spec.assertBool s (Codec.encode Keyword.codec (reinforce 2 1) /= Codec.encode Keyword.codec (reinforce 3 1)) "the N is part of the encoding"
    Spec.assertBool s (Codec.encode Keyword.codec (reinforce 2 1) /= Codec.encode Keyword.codec (reinforce 2 4)) "the cost is part of the encoding"
  -- CR 702.86a's N rides the constructor the same way poisonous' does, and the
  -- two must not share a tag either.
  Spec.it s "Annihilator carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Annihilator 1)
      """ {"type":"Annihilator","value":1} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Annihilator 3) /= Codec.encode Keyword.codec (Keyword.Poisonous 3))
      "annihilator 3 is not poisonous 3"
  -- CR 702.23a's N rides the constructor the same way.
  Spec.it s "Rampage carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Rampage 2)
      """ {"type":"Rampage","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Rampage 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "rampage 3 is not bushido 3"
  -- CR 702.130a's N rides the constructor the same way, and must not share a tag
  -- with the other payloaded keywords either.
  Spec.it s "Afflict carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Afflict 2)
      """ {"type":"Afflict","value":2} """
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Afflict 3) /= Codec.encode Keyword.codec (Keyword.Annihilator 3))
      "afflict 3 is not annihilator 3"
  Spec.it s "Infect" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Infect
      """ {"type":"Infect"} """
  -- CR 702.80d makes multiple instances redundant, so wither is a bare tag with
  -- nothing to count.
  Spec.it s "Wither" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Wither
      """ {"type":"Wither"} """
  -- CR 702.83a takes no parameter either, so exalted is a bare tag. What is
  -- multiple is the COUNT the projection keeps -- rule 702.83 prints no
  -- redundancy clause, unlike wither's above.
  Spec.it s "Exalted" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Exalted
      """ {"type":"Exalted"} """
  -- CR 702.134a takes no parameter either, and CR 702.134b makes the instances
  -- separate rather than redundant -- so, like exalted, a bare tag over a count.
  Spec.it s "Mentor" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Mentor
      """ {"type":"Mentor"} """
  -- CR 702.149a is nullary and CR 702.149b separate, so mentor's shape exactly.
  Spec.it s "Training" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Training
      """ {"type":"Training"} """
  -- CR 702.39a takes no parameter either, and CR 702.39b makes the instances
  -- separate -- so a bare tag over a count, as mentor is.
  Spec.it s "Provoke" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Provoke
      """ {"type":"Provoke"} """
  -- CR 702.91a's battle cry takes no parameter, so it encodes as a bare tag.
  -- What CR 702.91b makes multiple is the COUNT the projection keeps, never the
  -- value.
  Spec.it s "BattleCry" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.BattleCry
      """ {"type":"BattleCry"} """
  -- CR 702.108a's prowess takes no parameter either, and CR 702.108b makes the
  -- COUNT multiple rather than the value.
  Spec.it s "Prowess" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Prowess
      """ {"type":"Prowess"} """
  -- CR 702.100a's evolve is nullary too, and CR 702.100d makes the COUNT
  -- multiple rather than the value.
  Spec.it s "Evolve" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Evolve
      """ {"type":"Evolve"} """
  -- CR 702.105a's dethrone is nullary as well, CR 702.105b making the COUNT
  -- multiple rather than the value.
  Spec.it s "Dethrone" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Dethrone
      """ {"type":"Dethrone"} """
  Spec.it s "Menace" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Menace
      """ {"type":"Menace"} """
  Spec.it s "Changeling" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Changeling
      """ {"type":"Changeling"} """
  Spec.it s "Devoid" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Devoid
      """ {"type":"Devoid"} """
  Spec.it s "Ingest" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Ingest
      """ {"type":"Ingest"} """
  Spec.it s "Skulk" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Skulk
      """ {"type":"Skulk"} """
  Spec.it s "Melee" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Melee
      """ {"type":"Melee"} """
  -- CR 702.122a's N rides the constructor, so crew 1 and crew 6 are distinct
  -- keywords and must encode distinguishably.
  Spec.it s "Crew carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Crew 6)
      """ {"type":"Crew","value":6} """
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Crew 1) /= Codec.encode Keyword.codec (Keyword.Crew 6)) "crew 1 and crew 6 encode differently"
  -- CR 702.123a's N is both the counters and the tokens, so fabricate 1 and
  -- fabricate 2 are distinct keywords, Crew's shape.
  Spec.it s "Fabricate carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Fabricate 2)
      """ {"type":"Fabricate","value":2} """
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Fabricate 1) /= Codec.encode Keyword.codec (Keyword.Fabricate 2)) "fabricate 1 and fabricate 2 encode differently"
  Spec.it s "Riot" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Riot
      """ {"type":"Riot"} """
  Spec.it s "Unleash" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Unleash
      """ {"type":"Unleash"} """
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Unleash /= Codec.encode Keyword.codec Keyword.Riot) "unleash and riot encode differently"
  Spec.it s "Daybound" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Daybound
      """ {"type":"Daybound"} """
  Spec.it s "Nightbound" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Nightbound
      """ {"type":"Nightbound"} """
  Spec.it s "Decayed" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Decayed
      """ {"type":"Decayed"} """
  -- CR 702.164a's N rides the constructor.
  Spec.it s "Toxic carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Toxic 1)
      """ {"type":"Toxic","value":1} """
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Toxic 1) /= Codec.encode Keyword.codec (Keyword.Toxic 2)) "toxic 1 and toxic 2 encode differently"
  Spec.it s "Persist" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Persist
      """ {"type":"Persist"} """
  -- Persist's mirror, and told apart from it by the tag alone: rules 702.79a and
  -- 702.93a differ only in a counter kind neither encoding carries.
  Spec.it s "Undying" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Undying
      """ {"type":"Undying"} """
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Undying /= Codec.encode Keyword.codec Keyword.Persist) "undying and persist encode differently"
  Spec.it s "has a schema" $
    Common.assertHasSchema s Keyword.codec
