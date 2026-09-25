module Pawl.Codec.KeywordSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Craft as Craft
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Devour as Devour
import qualified Pawl.Types.DevourCount as DevourCount
import qualified Pawl.Types.Emerge as Emerge
import qualified Pawl.Types.Equip as Equip
import qualified Pawl.Types.ExileMaterials as ExileMaterials
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Gift as Gift
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.MorphVariant as MorphVariant
import qualified Pawl.Types.PartnerText as PartnerText
import qualified Pawl.Types.Protection as Protection
import qualified Pawl.Types.Prototype as Prototype
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.Splice as Splice
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Suspend as Suspend
import qualified Pawl.Types.SuspendCounters as SuspendCounters
import qualified Pawl.Types.Ward as Ward

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Keyword" $ do
  Spec.it s "Deathtouch" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Deathtouch
      " {\"type\":\"Deathtouch\"} "
  Spec.it s "Defender" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Defender
      " {\"type\":\"Defender\"} "
  Spec.it s "DoubleStrike" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.DoubleStrike
      " {\"type\":\"DoubleStrike\"} "
  Spec.it s "FirstStrike" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.FirstStrike
      " {\"type\":\"FirstStrike\"} "
  Spec.it s "Flash" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Flash
      " {\"type\":\"Flash\"} "
  Spec.it s "Flying" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Flying
      " {\"type\":\"Flying\"} "
  Spec.it s "Haste" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Haste
      " {\"type\":\"Haste\"} "
  -- CR 702.11b's plain hexproof takes no parameter, so it encodes as the bare
  -- tag -- the wire format Slippery Bogle's committed printing already carries,
  -- unchanged by rule 702.11d's quality arriving beside it.
  Spec.it s "Hexproof" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Hexproof Nothing)
      " {\"type\":\"Hexproof\"} "
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
      " {\"type\":\"Hexproof\",\"value\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Black\"}}} "
    -- CR 702.16a's "any characteristic value or information": a quality need not
    -- be a colour. Eradicator Valkyrie's "hexproof from planeswalkers".
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Hexproof (Just (Filter.HasCardType CardType.Planeswalker)))
      " {\"type\":\"Hexproof\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Planeswalker\"}}} "
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
      " {\"type\":\"Indestructible\"} "
  -- CR 702.14a's "[type]" rides the constructor, so swampwalk and islandwalk
  -- are DIFFERENT keywords and must encode differently.
  Spec.it s "Landwalk carries a land-type criterion" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.HasSubtype Subtype.Swamp))
      " {\"type\":\"Landwalk\",\"value\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Swamp\"}}} "
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.HasSubtype Subtype.Island))
      " {\"type\":\"Landwalk\",\"value\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Island\"}}} "
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
      " {\"type\":\"Landwalk\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"HasSupertype\",\"value\":{\"type\":\"Basic\"}}}} "
    -- With both the specified type or supertype and the specified subtype.
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.And [Filter.HasSupertype Supertype.Snow, Filter.HasSubtype Subtype.Swamp]))
      " {\"type\":\"Landwalk\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasSupertype\",\"value\":{\"type\":\"Snow\"}},{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Swamp\"}}]}} "
    -- With the specified type or supertype: artifact landwalk, which no
    -- creature prints -- it is only ever granted.
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Landwalk (Filter.HasCardType CardType.Artifact))
      " {\"type\":\"Landwalk\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  Spec.it s "Lifelink" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Lifelink
      " {\"type\":\"Lifelink\"} "
  -- CR 702.161a. Nullary on the wire, the rule stating no parameter.
  Spec.it s "LivingMetal" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.LivingMetal
      " {\"type\":\"LivingMetal\"} "
  -- CR 702.162a states its cost as part of the keyword, so the payload is
  -- required. Pinned by hand because Arm.tagged forces a constructor's TAG and
  -- not its arm, so a constructor could ship with no arm and no failing round
  -- trip.
  Spec.it s "MoreThanMeetsTheEye carries its cost" $ do
    let mtmte n = Keyword.MoreThanMeetsTheEye (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (mtmte 1)
      " {\"type\":\"MoreThanMeetsTheEye\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (mtmte 1) /= Codec.encode Keyword.codec (mtmte 4)) "the cost is part of the encoding"
  -- CR 702.16a states a quality on EVERY protection ability, so the payload is
  -- required where hexproof's is optional: there is no bare protection tag to
  -- write, and rule 702.16j's "protection from everything" is spelled as the
  -- quality that matches every object, `And []`, rather than as an absent one.
  Spec.it s "Protection carries CR 702.16a's quality" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Protection Protection.MkProtection {Protection.quality = Filter.HasColor Color.Black, Protection.spares = Nothing})
      " {\"type\":\"Protection\",\"value\":{\"quality\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Black\"}},\"spares\":null}} "
    -- CR 702.16a's "any characteristic value or information": protection from
    -- artifacts is as printed as protection from black.
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Protection Protection.MkProtection {Protection.quality = Filter.HasCardType CardType.Artifact, Protection.spares = Nothing})
      " {\"type\":\"Protection\",\"value\":{\"quality\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}},\"spares\":null}} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Protection Protection.MkProtection {Protection.quality = Filter.HasColor Color.Black, Protection.spares = Nothing}) /= Codec.encode Keyword.codec (Keyword.Hexproof (Just (Filter.HasColor Color.Black))))
      "protection from black and hexproof from black encode differently"
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Protection Protection.MkProtection {Protection.quality = Filter.HasColor Color.Black, Protection.spares = Nothing}) /= Codec.encode Keyword.codec (Keyword.Protection Protection.MkProtection {Protection.quality = Filter.HasColor Color.White, Protection.spares = Nothing}))
      "and so do protection from black and protection from white"
  Spec.it s "Reach" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Reach
      " {\"type\":\"Reach\"} "
  -- CR 702.18a's shroud is nullary, so what this pins is the TAG.
  Spec.it s "Shroud" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Shroud
      " {\"type\":\"Shroud\"} "
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Shroud /= Codec.encode Keyword.codec Keyword.Trample) "shroud is not trample"
  Spec.it s "Trample" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Trample
      " {\"type\":\"Trample\"} "
  -- CR 702.19c is a keyword of its own and not a flavour of the one above, so
  -- the pair is asserted distinct: an arm that decoded to Trample instead
  -- would round-trip the tag and quietly drop the variant.
  Spec.it s "TrampleOverPlaneswalkers" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.TrampleOverPlaneswalkers
      " {\"type\":\"TrampleOverPlaneswalkers\"} "
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.TrampleOverPlaneswalkers /= Codec.encode Keyword.codec Keyword.Trample) "the variant is not trample"
  Spec.it s "Vigilance" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Vigilance
      " {\"type\":\"Vigilance\"} "
  -- CR 702.21a's payload is a Cost, and must not share Flashback's or Plot's tag:
  -- ward's is paid by an OPPONENT as the minted trigger resolves, where every
  -- other cost-bearing keyword names a cost its own controller pays.
  Spec.it s "Ward carries its cost, and is not Flashback" $ do
    let ward n = Keyword.Ward (Ward.MkWard (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) Nothing)
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (ward 2)
      " {\"type\":\"Ward\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}}} "
    Spec.assertBool s (Codec.encode Keyword.codec (ward 2) /= Codec.encode Keyword.codec (flashbackOf 2)) "the same cost under two keywords encodes differently"
  -- CR 702.22: only the combat-damage-division halves are modeled; see the type.
  Spec.it s "Banding" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Banding
      " {\"type\":\"Banding\"} "
  -- CR 702.25a: nullary, because the rule takes no parameter -- its "without
  -- flanking" is a Filter over the blocker in the ability this mints, not a
  -- payload.
  Spec.it s "Flanking" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Flanking
      " {\"type\":\"Flanking\"} "
  -- CR 702.26a: nullary, because the rule takes no parameter -- what a phasing
  -- permanent does is entirely the untap step's business.
  Spec.it s "Phasing" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Phasing
      " {\"type\":\"Phasing\"} "
  -- CR 702.28b: nullary, because the rule takes no parameter -- both of its
  -- sentences ask only whether the other creature has the same keyword.
  Spec.it s "Shadow" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Shadow
      " {\"type\":\"Shadow\"} "
  -- CR 702.31b: nullary, because the rule takes no parameter -- the only thing it
  -- asks about a blocker is whether it has horsemanship too.
  Spec.it s "Horsemanship" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Horsemanship
      " {\"type\":\"Horsemanship\"} "
  -- CR 702.127a: nullary, because what an aftermath half costs is its own printed
  -- mana cost -- unlike flashback, whose alternative cost rides the constructor.
  Spec.it s "Aftermath" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Aftermath
      " {\"type\":\"Aftermath\"} "
  -- CR 702.133a: nullary for aftermath's reason and one more -- the discard the
  -- rule names is the rule's, not the card's, so there is no payload to carry.
  Spec.it s "JumpStart" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.JumpStart
      " {\"type\":\"JumpStart\"} "
  -- CR 702.29e: the typecycling filter rides the same keyword arm and
  -- is absent for plain cycling, so both spellings have to survive the trip.
  Spec.it s "Cycling round-trips with and without a typecycling filter" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Cycling (Cycling.MkCycling cost Nothing))
      " {\"type\":\"Cycling\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"searchFor\":null}} "
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Cycling (Cycling.MkCycling cost (Just (Filter.HasCardType CardType.Land))))
      " {\"type\":\"Cycling\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"searchFor\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  -- CR 702.34a's payload is a whole Cost, not a number -- the first keyword
  -- whose parameter is itself a composite.
  Spec.it s "Flashback carries its cost" $ do
    let flashback n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (flashback 1)
      " {\"type\":\"Flashback\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (flashback 1) /= Codec.encode Keyword.codec (flashback 4)) "the cost is part of the encoding"
  -- CR 702.138a's payload is a whole Cost too, and it must not share Flashback's
  -- tag: the two buy the same cast from the same zone, but rule 702.34a gates its
  -- permission on the resulting spell being an instant or sorcery and rule
  -- 702.138a gates nothing, so Loathsome Chimera's creature cast turns on which
  -- tag was read.
  Spec.it s "Escape carries its cost, and is not Flashback" $ do
    let escape n = Keyword.Escape (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (escape 6)
      " {\"type\":\"Escape\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":6}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (escape 6) /= Codec.encode Keyword.codec (flashbackOf 6)) "the same cost under two keywords encodes differently"
  -- CR 702.74a's payload is a whole Cost, Mulldrifter's {2}{U}.
  Spec.it s "Evoke carries its cost" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Evoke (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []))
      " {\"type\":\"Evoke\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
  -- CR 702.109a and CR 702.152a carry a whole Cost each, under tags of their own.
  Spec.it s "Dash and Blitz carry their costs" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertCodec s Keyword.codec (Keyword.Dash cost) " {\"type\":\"Dash\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Common.assertCodec s Keyword.codec (Keyword.Blitz cost) " {\"type\":\"Blitz\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
  -- CR 702.185a's payload is a whole Cost too, Bygone Colossus's {3}, under a tag
  -- of its own: rule 702.185a's cost is offered from the HAND alone where dash's
  -- and blitz's name no zone, so a warp arriving under either tag would be
  -- offered from every zone.
  Spec.it s "Warp carries its cost" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Warp (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) []))
      " {\"type\":\"Warp\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}} "
  -- CR 702.148a's payload is a whole Cost, Path of Peril's {4}{W}{B}.
  Spec.it s "Cleave carries its cost" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Cleave (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []))
      " {\"type\":\"Cleave\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":4}]}} "
  -- CR 702.96a's payload is a whole Cost too, Cyclonic Rift's {6}{U}.
  Spec.it s "Overload carries its cost" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Overload (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 6])) []))
      " {\"type\":\"Overload\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":6}]}} "
  -- CR 702.113a's payload is a whole Cost, cleave's shape -- Part the Waterveil's
  -- {6}{U}{U}{U}. Rule 702.113a's N is not in the payload: the spell ability the
  -- rule's second half states rides the card's own clause.
  Spec.it s "Awaken carries its cost" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Awaken (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 6])) []))
      " {\"type\":\"Awaken\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":6}]}} "
  -- CR 702.119a's payload is a whole Cost and CR 702.119b's optional quality
  -- beside it, Equip's shape -- Drownyard Behemoth's {7}{U} and no quality. The
  -- SACRIFICE rule 702.119a states is not in the payload: it is appended at the
  -- offer, by Pawl.Engine.Cost.candidateCostsGiven.
  Spec.it s "Emerge carries its cost and its quality" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Emerge (Emerge.MkEmerge (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 7])) []) Nothing))
      " {\"type\":\"Emerge\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":7}]},\"quality\":null}} "
  -- CR 702.188a and CR 702.190a carry a whole Cost each, cleave's shape. The
  -- return each rule states is NOT in the payload: it is appended at the offer, by
  -- Pawl.Engine.Keyword.plainAlternativeCosts, off the rule rather than the card.
  Spec.it s "WebSlinging and Sneak carry their costs" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertCodec s Keyword.codec (Keyword.WebSlinging cost) " {\"type\":\"WebSlinging\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Common.assertCodec s Keyword.codec (Keyword.Sneak cost) " {\"type\":\"Sneak\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
  -- CR 702.117a and CR 702.137a carry a whole Cost each, under tags of their own.
  Spec.it s "Surge and Spectacle carry their costs" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertCodec s Keyword.codec (Keyword.Surge cost) " {\"type\":\"Surge\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Common.assertCodec s Keyword.codec (Keyword.Spectacle cost) " {\"type\":\"Spectacle\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
  -- CR 702.76a and CR 702.173a carry a whole Cost each, under tags of their own.
  Spec.it s "Prowl and Freerunning carry their costs" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertCodec s Keyword.codec (Keyword.Prowl cost) " {\"type\":\"Prowl\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Common.assertCodec s Keyword.codec (Keyword.Freerunning cost) " {\"type\":\"Freerunning\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
  -- CR 702.157a and CR 702.175a carry a whole Cost each, under tags of their own.
  Spec.it s "Squad and Offspring carry their costs" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []
    Common.assertCodec s Keyword.codec (Keyword.Squad cost) " {\"type\":\"Squad\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
    Common.assertCodec s Keyword.codec (Keyword.Offspring cost) " {\"type\":\"Offspring\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
  -- CR 702.174a carries its [something] and not a cost: rule 702.174a writes the
  -- additional cost out in the rulebook, so the card prints only the word.
  Spec.it s "Gift carries its something" $
    Common.assertCodec s Keyword.codec (Keyword.Gift Gift.Card) " {\"type\":\"Gift\",\"value\":{\"type\":\"Card\"}} "
  -- CR 702.56a carries a Cost as squad does; CR 702.153a carries only its N, the
  -- creature it names being written in the rulebook rather than on the card.
  Spec.it s "Replicate carries its cost and Casualty its N" $ do
    Common.assertCodec s Keyword.codec (Keyword.Replicate (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) [])) " {\"type\":\"Replicate\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}} "
    Common.assertCodec s Keyword.codec (Keyword.Casualty 2) " {\"type\":\"Casualty\",\"value\":2} "
  -- CR 702.59a writes its cost on the card, replicate's shape.
  Spec.it s "Recover carries its cost" $
    Common.assertCodec s Keyword.codec (Keyword.Recover (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) [])) " {\"type\":\"Recover\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":4}]}} "
  -- CR 702.166a and CR 702.194a mint their costs in the rulebook, so the card
  -- writes bargain bare and teamwork with its N alone.
  Spec.it s "Bargain is nullary and Teamwork carries its N" $ do
    Common.assertCodec s Keyword.codec Keyword.Bargain " {\"type\":\"Bargain\"} "
    Common.assertCodec s Keyword.codec (Keyword.Teamwork 4) " {\"type\":\"Teamwork\",\"value\":4} "
  -- CR 702.168a's payload is a Cost too, and it must not share Morph's tag: the
  -- two name the turn-face-up cost of DIFFERENT procedures (CR 702.37e and CR
  -- 702.168d), and the objects they list differ by CR 702.168b's ward {2}.
  Spec.it s "Disguise carries its cost, and is not Morph" $ do
    let disguise n = Keyword.Disguise (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        morphOf n = Keyword.Morph (Morph.MkMorph (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) MorphVariant.Plain)
    Common.assertCodec
      s
      Keyword.codec
      (disguise 5)
      " {\"type\":\"Disguise\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":5}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (disguise 5) /= Codec.encode Keyword.codec (morphOf 5)) "the same cost under two keywords encodes differently"
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
      " {\"type\":\"Plot\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}} "
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
      " {\"type\":\"Foretell\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (foretell 1) /= Codec.encode Keyword.codec (plotOf 1)) "the same cost under two keywords encodes differently"
  -- CR 702.62a's payload is a Cost and rule 702.62a's N beside it, Equip's shape
  -- one field over -- so it is an OBJECT rather than a bare cost, and cannot
  -- collide with Plot's tag whatever the cost. Nothing but this case guards the
  -- ENCODE direction, since Arm.tagged forces a constructor's TAG and not its
  -- arm, so a missing arm compiles.
  Spec.it s "Suspend carries its counters and its cost" $ do
    let suspend n cost = Keyword.Suspend (Suspend.MkSuspend (SuspendCounters.Literal n) (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic cost])) []))
    Common.assertCodec
      s
      Keyword.codec
      (suspend 1 3)
      " {\"type\":\"Suspend\",\"value\":{\"counters\":{\"type\":\"Literal\",\"value\":1},\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}}} "
    Spec.assertBool s (Codec.encode Keyword.codec (suspend 1 3) /= Codec.encode Keyword.codec (suspend 2 3)) "the counters are carried, not defaulted"
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
      " {\"type\":\"Miracle\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (miracle 2) /= Codec.encode Keyword.codec (plotOf 2)) "the same cost under two keywords encodes differently"
  -- CR 702.6a's payload is a Cost and CR 702.6c's quality, Cycling's shape. The
  -- corpus load guards the DECODE direction for free -- the Equipment in
  -- data/cards/ fail to parse without the arm -- and nothing but this case
  -- guards the encode, since Arm.tagged forces a constructor's TAG and not its
  -- arm, so a missing arm compiles.
  Spec.it s "Equip carries its cost and its quality" $ do
    let equip n = Keyword.Equip (Equip.MkEquip (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) Nothing)
        equipHuman n = Keyword.Equip (Equip.MkEquip (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) (Just (Filter.HasSubtype Subtype.Human)))
    Common.assertCodec
      s
      Keyword.codec
      (equip 1)
      " {\"type\":\"Equip\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"quality\":null}} "
    Spec.assertBool s (Codec.encode Keyword.codec (equip 1) /= Codec.encode Keyword.codec (equip 3)) "the cost is part of the encoding"
    Spec.assertBool s (Codec.encode Keyword.codec (equip 1) /= Codec.encode Keyword.codec (equipHuman 1)) "and so is the quality"
  -- CR 702.67a's payload is equip's, and the two tags must not collide: a
  -- Fortification prints fortify and never equip, so a card decoded into the
  -- wrong one would mint the wrong minted ability.
  Spec.it s "Fortify carries its cost" $ do
    let fortify n = Keyword.Fortify (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (fortify 3)
      " {\"type\":\"Fortify\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (fortify 3) /= Codec.encode Keyword.codec (Keyword.Equip (Equip.MkEquip (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) []) Nothing))) "and is not equip"
  -- CR 702.6e's payload is fortify's bare Cost, under its own tag.
  Spec.it s "EquipPlaneswalker carries its cost" $ do
    let equipPlaneswalker n = Keyword.EquipPlaneswalker (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (equipPlaneswalker 1)
      " {\"type\":\"EquipPlaneswalker\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (equipPlaneswalker 3) /= Codec.encode Keyword.codec (Keyword.Fortify (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) []))) "and is not fortify"
  -- CR 702.49a's payload is a bare Cost, fortify's shape, and the arm is
  -- Arm.tagged: a Keyword constructor ships with no wire format and nothing red
  -- unless a case like this one is written (#1715).
  Spec.it s "Ninjutsu carries its cost" $ do
    let ninjutsu n = Keyword.Ninjutsu (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (ninjutsu 2)
      " {\"type\":\"Ninjutsu\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (ninjutsu 2) /= Codec.encode Keyword.codec (ninjutsu 3)) "the cost is part of the encoding"
  -- CR 702.84a's payload is a Cost, LevelUp's shape below: the cost is the whole of
  -- what the card writes, rule 702.84a supplying the rest of the ability.
  Spec.it s "Unearth carries its cost" $ do
    let unearth n = Keyword.Unearth (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (unearth 1)
      " {\"type\":\"Unearth\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (unearth 1) /= Codec.encode Keyword.codec (unearth 5)) "the cost is part of the encoding"
  -- CR 702.128a's and CR 702.129a's payloads are a Cost, Unearth's shape.
  Spec.it s "Embalm carries its cost" $ do
    let embalm n = Keyword.Embalm (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (embalm 3)
      " {\"type\":\"Embalm\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (embalm 3) /= Codec.encode Keyword.codec (embalm 4)) "the cost is part of the encoding"
  Spec.it s "Eternalize carries its cost" $ do
    let eternalize n = Keyword.Eternalize (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (eternalize 4)
      " {\"type\":\"Eternalize\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":4}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (eternalize 4) /= Codec.encode Keyword.codec (eternalize 5)) "the cost is part of the encoding"
    Spec.assertBool s (Codec.encode Keyword.codec (eternalize 4) /= Codec.encode Keyword.codec (Keyword.Embalm (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []))) "and the tag tells it from embalm"
  -- CR 702.87a's payload is a Cost, and the tag must not collide with the level
  -- COUNTER's -- CounterKind's "Level" and this keyword's "LevelUp" are two
  -- different wire tags for the two halves of rule 711.
  Spec.it s "LevelUp carries its cost" $ do
    let levelUp n = Keyword.LevelUp (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (levelUp 1)
      " {\"type\":\"LevelUp\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (levelUp 1) /= Codec.encode Keyword.codec (levelUp 4)) "the cost is part of the encoding"
  -- CR 702.107a's payload is a Cost too, Flashback's shape rather than Crew's
  -- Natural.
  Spec.it s "Outlast carries its cost" $ do
    let outlast n = Keyword.Outlast (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (outlast 1)
      " {\"type\":\"Outlast\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (outlast 1) /= Codec.encode Keyword.codec (outlast 4)) "the cost is part of the encoding"
  Spec.it s "Fear" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Fear
      " {\"type\":\"Fear\"} "
  Spec.it s "Intimidate" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Intimidate
      " {\"type\":\"Intimidate\"} "
  -- CR 702.37a's payload is a whole Cost too -- the MORPH cost, which CR 702.37e
  -- pays to turn the permanent face up, never the {3} the cast pays.
  Spec.it s "Morph carries its cost, and is not Flashback" $ do
    let morph n = Keyword.Morph (Morph.MkMorph (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) MorphVariant.Plain)
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (morph 1)
      " {\"type\":\"Morph\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"variant\":{\"type\":\"Plain\"}}} "
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
      " {\"type\":\"Morph\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"variant\":{\"type\":\"Mega\"}}} "
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
      " {\"type\":\"Kicker\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":4}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (kicker 4) /= Codec.encode Keyword.codec (flashbackOf 4)) "kicker {4} is not flashback {4}"
  -- CR 702.33c's payload is the same Cost, and the two must not share a tag: the
  -- announcement kicker asks once is the one multikicker asks any number of times.
  Spec.it s "Multikicker carries its cost, and is not Kicker" $ do
    let kicker n = Keyword.Kicker (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        multikicker n = Keyword.Multikicker (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (multikicker 2)
      " {\"type\":\"Multikicker\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (multikicker 2) /= Codec.encode Keyword.codec (kicker 2)) "multikicker {2} is not kicker {2}"
  -- CR 702.42a's payload is a whole Cost too, and it must not share Flashback's
  -- tag.
  Spec.it s "Entwine carries its cost, and is not Flashback" $ do
    let entwine n = Keyword.Entwine (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (entwine 1)
      " {\"type\":\"Entwine\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (entwine 1) /= Codec.encode Keyword.codec (flashbackOf 1)) "entwine {1} is not flashback {1}"
  -- CR 702.120a's payload is a whole Cost too, and it must not share Entwine's
  -- tag: entwine widens the selection and charges once, escalate charges per
  -- extra mode.
  Spec.it s "Escalate carries its cost, and is not Entwine" $ do
    let escalate n = Keyword.Escalate (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        entwineOf n = Keyword.Entwine (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (escalate 2)
      " {\"type\":\"Escalate\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (escalate 2) /= Codec.encode Keyword.codec (entwineOf 2)) "escalate {2} is not entwine {2}"
  -- CR 702.27a's payload is a whole Cost too, and it must not share Kicker's
  -- tag: both are additional costs announced at CR 601.2b, and only buyback's
  -- payment changes where CR 608.2n sends the spell.
  Spec.it s "Buyback carries its cost, and is not Kicker" $ do
    let buyback n = Keyword.Buyback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (buyback 4)
      " {\"type\":\"Buyback\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":4}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (buyback 4) /= Codec.encode Keyword.codec (Keyword.Kicker (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []))) "buyback {4} is not kicker {4}"
  -- CR 702.45a's N rides the constructor as poisonous' does.
  Spec.it s "Bushido carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Bushido 2)
      " {\"type\":\"Bushido\",\"value\":2} "
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
      " {\"type\":\"Modular\",\"value\":2} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Modular 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "modular 3 is not bushido 3"
  -- CR 702.44a's sunburst is nullary: rule 702.44a fixes both counter kinds, and
  -- CR 702.44b's count is the entering object's mana record rather than a printed
  -- number.
  Spec.it s "Sunburst" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Sunburst
      " {\"type\":\"Sunburst\"} "
  -- CR 702.156a's ravenous is nullary: rule 702.156a fixes the counter kind and
  -- the threshold, and its count is CR 107.3m's announced X rather than a printed
  -- number.
  Spec.it s "Ravenous" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Ravenous
      " {\"type\":\"Ravenous\"} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec Keyword.Ravenous /= Codec.encode Keyword.codec Keyword.Sunburst)
      "ravenous and sunburst encode differently"
  -- CR 702.63a's N is a COUNT OF COUNTERS rather than a size or a threshold, and
  -- the wire cannot tell those apart -- so the tag is all that keeps vanishing 2
  -- from bushido 2.
  Spec.it s "Vanishing carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Vanishing (Just 2))
      " {\"type\":\"Vanishing\",\"value\":2} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Vanishing (Just 3)) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "vanishing 3 is not bushido 3"
  -- CR 702.63b: vanishing printed with NO number, which is hexproof's absent
  -- "value" key rather than a zero -- Tidewalker's counters come from its own
  -- text, and vanishing 0 would be a permanent that entered already empty.
  Spec.it s "Vanishing without CR 702.63b's number" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Vanishing Nothing)
      " {\"type\":\"Vanishing\"} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Vanishing Nothing) /= Codec.encode Keyword.codec (Keyword.Vanishing (Just 0)))
      "numberless vanishing is not vanishing 0"
  -- CR 702.32a's N is the same kind of number as rule 702.63a's, and the two
  -- keywords differ in more than the counter's name -- so the tag is what keeps
  -- fading 2 from vanishing 2 as well.
  Spec.it s "Fading carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Fading 2)
      " {\"type\":\"Fading\",\"value\":2} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Fading 3) /= Codec.encode Keyword.codec (Keyword.Vanishing (Just 3)))
      "fading 3 is not vanishing 3"
  Spec.it s "SplitSecond" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.SplitSecond
      " {\"type\":\"SplitSecond\"} "
  -- CR 702.68a's N rides the constructor as CR 702.45a's does, and the tag is
  -- what keeps `Frenzy 2` off bushido's wire form.
  Spec.it s "Frenzy carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Frenzy 1)
      " {\"type\":\"Frenzy\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Frenzy 2) /= Codec.encode Keyword.codec (Keyword.Bushido 2))
      "frenzy 2 is not bushido 2"
  -- CR 702.58a's N is a COUNT OF COUNTERS the permanent enters with, where rule
  -- 702.68a's is an amount of damage; the wire cannot tell those apart, so the
  -- tag is what keeps graft 2 off frenzy's form.
  Spec.it s "Graft carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Graft 1)
      " {\"type\":\"Graft\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Graft 2) /= Codec.encode Keyword.codec (Keyword.Frenzy 2))
      "graft 2 is not frenzy 2"
  -- CR 702.64a's N is an amount of damage PREVENTED, the opposite direction from
  -- frenzy's dealt one, and the tag is again the whole of the difference.
  Spec.it s "Absorb carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Absorb 1)
      " {\"type\":\"Absorb\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Absorb 2) /= Codec.encode Keyword.codec (Keyword.Graft 2))
      "absorb 2 is not graft 2"
  -- CR 702.70a's N rides the constructor the same way, and the two payloaded
  -- keywords must not share a tag.
  Spec.it s "Poisonous carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Poisonous 1)
      " {\"type\":\"Poisonous\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Poisonous 3) /= Codec.encode Keyword.codec (Keyword.Toxic 3))
      "poisonous 3 is not toxic 3"
  -- CR 702.72a's [object] rides on the wire as a Filter, affinity's shape: the
  -- printed qualities are creature types (Wanderwine Prophets' Merfolk) but rule
  -- 702.72a fixes no shape, so the criterion cannot flatten to a subtype.
  Spec.it s "Champion carries its quality" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Champion (Filter.HasSubtype Subtype.Merfolk))
      " {\"type\":\"Champion\",\"value\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Merfolk\"}}} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Champion (Filter.HasSubtype Subtype.Merfolk)) /= Codec.encode Keyword.codec (Keyword.Champion (Filter.HasSubtype Subtype.Island)))
      "champion a Merfolk and champion an Island encode differently"
  -- CR 702.112a's N is written like every other keyword's, so the TAG is what
  -- keeps them apart: `Renown 2` and `Poisonous 2` differ only by it on the wire.
  Spec.it s "Renown carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Renown 2)
      " {\"type\":\"Renown\",\"value\":2} "
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
      " {\"type\":\"Afterlife\",\"value\":2} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Afterlife 2) /= Codec.encode Keyword.codec (Keyword.Renown 2))
      "afterlife 2 is not renown 2"
  -- CR 702.46a's N is written like the rest, and it is a MANA VALUE BOUND rather
  -- than a count, so a collision with a same-numbered keyword would be a real
  -- misread.
  Spec.it s "Dredge carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Dredge 3)
      " {\"type\":\"Dredge\",\"value\":3} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Dredge 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "dredge 3 is not bushido 3"
  Spec.it s "Soulshift carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Soulshift 3)
      " {\"type\":\"Soulshift\",\"value\":3} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Soulshift 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "soulshift 3 is not bushido 3"
  -- CR 702.75a's N is a DEPTH into a library rather than a count of anything on
  -- the board, so a collision with a same-numbered keyword would be a real
  -- misread.
  Spec.it s "Hideaway carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Hideaway 4)
      " {\"type\":\"Hideaway\",\"value\":4} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Hideaway 4) /= Codec.encode Keyword.codec (Keyword.Fading 4))
      "hideaway 4 is not fading 4"
  -- CR 702.54a's N is a count of +1/+1 counters, so a collision with a
  -- same-numbered keyword would be a real misread here too.
  Spec.it s "Bloodthirst carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Bloodthirst (Just 1))
      " {\"type\":\"Bloodthirst\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Bloodthirst (Just 1)) /= Codec.encode Keyword.codec (Keyword.Modular 1))
      "bloodthirst 1 is not modular 1"
  -- CR 702.54b's X names no number, so it is the absent "value" key -- and it must
  -- not collide with a printed zero, which rule 702.54a would gate on damage.
  Spec.it s "Bloodthirst X without rule 702.54b's number" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Bloodthirst Nothing)
      " {\"type\":\"Bloodthirst\"} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Bloodthirst Nothing) /= Codec.encode Keyword.codec (Keyword.Bloodthirst (Just 0)))
      "bloodthirst X is not bloodthirst 0"
  -- CR 702.104a's N is a count of +1/+1 counters too, so it must not collide with
  -- a same-numbered keyword either.
  Spec.it s "Tribute carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Tribute 3)
      " {\"type\":\"Tribute\",\"value\":3} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Tribute 3) /= Codec.encode Keyword.codec (Keyword.Bloodthirst (Just 3)))
      "tribute 3 is not bloodthirst 3"
  -- CR 702.55a's haunt writes no payload at all -- the haunted object is board
  -- state (GameState.haunting), not a field of the keyword.
  Spec.it s "Haunt" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Haunt
      " {\"type\":\"Haunt\"} "
  -- CR 702.77a writes BOTH an N and a cost, so the array carries two fields the
  -- way cycling's and morph's do, and both must survive the round trip.
  Spec.it s "Reinforce carries its N and its cost" $ do
    let reinforce n g = Keyword.Reinforce (Reinforce.MkReinforce n (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic g])) []))
    Common.assertCodec
      s
      Keyword.codec
      (reinforce 2 1)
      " {\"type\":\"Reinforce\",\"value\":{\"amount\":2,\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}}} "
    Spec.assertBool s (Codec.encode Keyword.codec (reinforce 2 1) /= Codec.encode Keyword.codec (reinforce 3 1)) "the N is part of the encoding"
    Spec.assertBool s (Codec.encode Keyword.codec (reinforce 2 1) /= Codec.encode Keyword.codec (reinforce 2 4)) "the cost is part of the encoding"
  -- CR 702.86a's N rides the constructor the same way poisonous' does, and the
  -- two must not share a tag either.
  Spec.it s "Annihilator carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Annihilator 1)
      " {\"type\":\"Annihilator\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Annihilator 3) /= Codec.encode Keyword.codec (Keyword.Poisonous 3))
      "annihilator 3 is not poisonous 3"
  -- CR 702.165a's N rides the constructor the same way.
  Spec.it s "Backup carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Backup 1)
      " {\"type\":\"Backup\",\"value\":1} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Backup 3) /= Codec.encode Keyword.codec (Keyword.Annihilator 3))
      "backup 3 is not annihilator 3"
  -- CR 702.60a's N rides the constructor the same way, and must not share a tag
  -- with the other Ns either.
  Spec.it s "Ripple carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Ripple 4)
      " {\"type\":\"Ripple\",\"value\":4} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Ripple 3) /= Codec.encode Keyword.codec (Keyword.Annihilator 3))
      "ripple 3 is not annihilator 3"
  -- CR 702.181a's and CR 702.189a's Ns ride their constructors the same way, and
  -- the two must not share a tag with each other.
  Spec.it s "Mobilize and Firebending carry their Ns" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Mobilize 3)
      " {\"type\":\"Mobilize\",\"value\":3} "
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Firebending 2)
      " {\"type\":\"Firebending\",\"value\":2} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Mobilize 2) /= Codec.encode Keyword.codec (Keyword.Firebending 2))
      "mobilize 2 is not firebending 2"
  -- CR 702.23a's N rides the constructor the same way.
  Spec.it s "Rampage carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Rampage 2)
      " {\"type\":\"Rampage\",\"value\":2} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Rampage 3) /= Codec.encode Keyword.codec (Keyword.Bushido 3))
      "rampage 3 is not bushido 3"
  -- CR 702.24a's payload is a Cost like equip's and ward's, and the tag must not
  -- collide with either: what a card owes each upkeep is not what it owes to
  -- attach or to be targeted.
  Spec.it s "CumulativeUpkeep carries its cost" $ do
    let upkeep n = Keyword.CumulativeUpkeep (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (upkeep 1)
      " {\"type\":\"CumulativeUpkeep\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (upkeep 2) /= Codec.encode Keyword.codec (Keyword.Ward (Ward.MkWard (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []) Nothing))) "and is not ward"
  -- CR 702.30a's payload is a Cost too, and the tag must not collide with the
  -- upkeep cost above: what a permanent owes at the first upkeep after it came
  -- under your control is not what it owes at every upkeep thereafter. No arm of
  -- the keyword codec is forced by the compiler, only its tag, so this is the
  -- round trip that would catch a missing one.
  Spec.it s "Echo carries its cost, and is not CumulativeUpkeep" $ do
    let echo n = Keyword.Echo (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (echo 1)
      " {\"type\":\"Echo\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (echo 2) /= Codec.encode Keyword.codec (Keyword.CumulativeUpkeep (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []))) "and is not cumulative upkeep"
  -- CR 702.130a's N rides the constructor the same way, and must not share a tag
  -- with the other payloaded keywords either.
  Spec.it s "Afflict carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Afflict 2)
      " {\"type\":\"Afflict\",\"value\":2} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Afflict 3) /= Codec.encode Keyword.codec (Keyword.Annihilator 3))
      "afflict 3 is not annihilator 3"
  Spec.it s "Infect" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Infect
      " {\"type\":\"Infect\"} "
  -- CR 702.80d makes multiple instances redundant, so wither is a bare tag with
  -- nothing to count.
  Spec.it s "Wither" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Wither
      " {\"type\":\"Wither\"} "
  -- CR 702.83a takes no parameter either, so exalted is a bare tag. What is
  -- multiple is the COUNT the projection keeps -- rule 702.83 prints no
  -- redundancy clause, unlike wither's above.
  Spec.it s "Exalted" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Exalted
      " {\"type\":\"Exalted\"} "
  -- CR 702.134a takes no parameter either, and CR 702.134b makes the instances
  -- separate rather than redundant -- so, like exalted, a bare tag over a count.
  Spec.it s "Mentor" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Mentor
      " {\"type\":\"Mentor\"} "
  -- CR 702.149a is nullary and CR 702.149b separate, so mentor's shape exactly.
  Spec.it s "Training" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Training
      " {\"type\":\"Training\"} "
  -- CR 702.39a takes no parameter either, and CR 702.39b makes the instances
  -- separate -- so a bare tag over a count, as mentor is.
  Spec.it s "Provoke" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Provoke
      " {\"type\":\"Provoke\"} "
  -- CR 702.91a's battle cry takes no parameter, so it encodes as a bare tag.
  -- What CR 702.91b makes multiple is the COUNT the projection keeps, never the
  -- value.
  Spec.it s "BattleCry" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.BattleCry
      " {\"type\":\"BattleCry\"} "
  -- CR 702.101a fixes extort's {W/B} and CR 702.191a names no cost at all, so
  -- both are nullary, and their 702.101b/702.191b are counts rather than values.
  Spec.it s "Extort" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Extort
      " {\"type\":\"Extort\"} "
  Spec.it s "Increment" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Increment
      " {\"type\":\"Increment\"} "
  -- CR 702.108a's prowess takes no parameter either, and CR 702.108b makes the
  -- COUNT multiple rather than the value.
  Spec.it s "Prowess" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Prowess
      " {\"type\":\"Prowess\"} "
  -- CR 702.100a's evolve is nullary too, and CR 702.100d makes the COUNT
  -- multiple rather than the value.
  Spec.it s "Evolve" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Evolve
      " {\"type\":\"Evolve\"} "
  -- CR 702.110a's exploit is nullary: the rule states no parameter, the
  -- sacrificed creature being chosen as the ability resolves.
  Spec.it s "Exploit" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Exploit
      " {\"type\":\"Exploit\"} "
  -- CR 702.105a's dethrone is nullary as well, CR 702.105b making the COUNT
  -- multiple rather than the value.
  Spec.it s "Dethrone" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Dethrone
      " {\"type\":\"Dethrone\"} "
  -- CR 702.102a's fuse is nullary too: the rule states a permission and names no
  -- value, so the arm carries no payload.
  Spec.it s "Fuse" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Fuse
      " {\"type\":\"Fuse\"} "
  Spec.it s "Menace" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Menace
      " {\"type\":\"Menace\"} "
  Spec.it s "Changeling" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Changeling
      " {\"type\":\"Changeling\"} "
  Spec.it s "Devoid" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Devoid
      " {\"type\":\"Devoid\"} "
  Spec.it s "Ingest" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Ingest
      " {\"type\":\"Ingest\"} "
  Spec.it s "Myriad" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Myriad
      " {\"type\":\"Myriad\"} "
  Spec.it s "Skulk" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Skulk
      " {\"type\":\"Skulk\"} "
  Spec.it s "Melee" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Melee
      " {\"type\":\"Melee\"} "
  Spec.it s "Partner" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Partner
      " {\"type\":\"Partner\"} "
  -- CR 702.124i's text rides the constructor, since only the same text pairs.
  Spec.it s "PartnerText carries its text" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.PartnerText PartnerText.FriendsForever)
      " {\"type\":\"PartnerText\",\"value\":{\"type\":\"FriendsForever\"}} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.PartnerText PartnerText.FriendsForever) /= Codec.encode Keyword.codec (Keyword.PartnerText PartnerText.CharacterSelect)) "two texts encode differently"
  -- CR 702.124j's name rides the constructor, since the pairing is by name.
  Spec.it s "PartnerWith carries its name" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.PartnerWith (CardName.MkCardName (Text.pack "Trynn, Champion of Freedom")))
      " {\"type\":\"PartnerWith\",\"value\":\"Trynn, Champion of Freedom\"} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.PartnerWith (CardName.MkCardName (Text.pack "Trynn, Champion of Freedom"))) /= Codec.encode Keyword.codec (Keyword.PartnerWith (CardName.MkCardName (Text.pack "Ley Weaver")))) "two names encode differently"
  Spec.it s "ChooseABackground" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.ChooseABackground
      " {\"type\":\"ChooseABackground\"} "
  Spec.it s "DoctorsCompanion" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.DoctorsCompanion
      " {\"type\":\"DoctorsCompanion\"} "
  -- CR 702.122a's N rides the constructor, so crew 1 and crew 6 are distinct
  -- keywords and must encode distinguishably.
  Spec.it s "Crew carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Crew 6)
      " {\"type\":\"Crew\",\"value\":6} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Crew 1) /= Codec.encode Keyword.codec (Keyword.Crew 6)) "crew 1 and crew 6 encode differently"
  -- CR 702.171a's N is crew's, one rule over, and is distinguishable for the
  -- same reason.
  Spec.it s "Saddle carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Saddle 2)
      " {\"type\":\"Saddle\",\"value\":2} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Saddle 1) /= Codec.encode Keyword.codec (Keyword.Saddle 2)) "saddle 1 and saddle 2 encode differently"
  -- CR 702.123a's N is both the counters and the tokens, so fabricate 1 and
  -- fabricate 2 are distinct keywords, Crew's shape.
  Spec.it s "Fabricate carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Fabricate 2)
      " {\"type\":\"Fabricate\",\"value\":2} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Fabricate 1) /= Codec.encode Keyword.codec (Keyword.Fabricate 2)) "fabricate 1 and fabricate 2 encode differently"
  -- CR 702.82a's N multiplies the counters each sacrifice buys, so devour 1 and
  -- devour 3 are distinct keywords, Fabricate's shape.
  Spec.it s "Devour carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Devour Devour.MkDevour {Devour.quality = Nothing, Devour.count = DevourCount.Fixed 3})
      " {\"type\":\"Devour\",\"value\":{\"count\":{\"type\":\"Fixed\",\"value\":3}}} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Devour Devour.MkDevour {Devour.quality = Nothing, Devour.count = DevourCount.Fixed 1}) /= Codec.encode Keyword.codec (Keyword.Devour Devour.MkDevour {Devour.quality = Nothing, Devour.count = DevourCount.Fixed 3})) "devour 1 and devour 3 encode differently"
  -- CR 702.82c: the quality rides on the same payload, so devour 1 and devour
  -- artifact 1 are distinct keywords too.
  Spec.it s "Devour carries CR 702.82c's quality" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Devour Devour.MkDevour {Devour.quality = Just (Filter.HasCardType CardType.Artifact), Devour.count = DevourCount.Fixed 1})
      " {\"type\":\"Devour\",\"value\":{\"count\":{\"type\":\"Fixed\",\"value\":1},\"quality\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}}} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Devour Devour.MkDevour {Devour.quality = Nothing, Devour.count = DevourCount.Fixed 1}) /= Codec.encode Keyword.codec (Keyword.Devour Devour.MkDevour {Devour.quality = Just (Filter.HasCardType CardType.Artifact), Devour.count = DevourCount.Fixed 1})) "devour 1 and devour artifact 1 encode differently"
  -- CR 702.82b: Thromok the Insatiable's N is the sacrifice's own count, which
  -- carries no number to be distinct by.
  Spec.it s "Devour carries CR 702.82b's restated N" $
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Devour Devour.MkDevour {Devour.quality = Nothing, Devour.count = DevourCount.Devoured})
      " {\"type\":\"Devour\",\"value\":{\"count\":{\"type\":\"Devoured\"}}} "
  -- CR 702.38a's N multiplies the counters each revealed card buys, Devour's
  -- shape one zone over.
  Spec.it s "Amplify carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Amplify 2)
      " {\"type\":\"Amplify\",\"value\":2} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Amplify 1) /= Codec.encode Keyword.codec (Keyword.Amplify 2)) "amplify 1 and amplify 2 encode differently"
  Spec.it s "Riot" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Riot
      " {\"type\":\"Riot\"} "
  Spec.it s "Unleash" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Unleash
      " {\"type\":\"Unleash\"} "
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Unleash /= Codec.encode Keyword.codec Keyword.Riot) "unleash and riot encode differently"
  -- CR 702.150a: nullary, the two loyalty counters being the rule's number.
  Spec.it s "Compleated" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Compleated
      " {\"type\":\"Compleated\"} "
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Compleated /= Codec.encode Keyword.codec Keyword.Riot) "compleated and riot encode differently"
  -- CR 702.155a/c: nullary, and redundant in multiples, so nothing rides it.
  Spec.it s "ReadAhead" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.ReadAhead
      " {\"type\":\"ReadAhead\"} "
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.ReadAhead /= Codec.encode Keyword.codec Keyword.Riot) "read ahead and riot encode differently"
  Spec.it s "Demonstrate" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Demonstrate
      " {\"type\":\"Demonstrate\"} "
  Spec.it s "Daybound" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Daybound
      " {\"type\":\"Daybound\"} "
  Spec.it s "Nightbound" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Nightbound
      " {\"type\":\"Nightbound\"} "
  Spec.it s "Decayed" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Decayed
      " {\"type\":\"Decayed\"} "
  -- CR 702.160a's inset frame rides the constructor: cost, power and toughness
  -- together, since CR 718.3 chooses all three at once.
  Spec.it s "Prototype carries its inset frame" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Prototype assemblerFrame)
      " {\"type\":\"Prototype\",\"value\":{\"cost\":[{\"type\":\"Generic\",\"value\":1},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}}],\"power\":2,\"toughness\":2}} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Prototype assemblerFrame) /= Codec.encode Keyword.codec (Keyword.Prototype assemblerFrame {Prototype.power = 4})) "two inset frames differing only in power encode differently"
  -- CR 702.164a's N rides the constructor.
  Spec.it s "Toxic carries its N" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Toxic 1)
      " {\"type\":\"Toxic\",\"value\":1} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Toxic 1) /= Codec.encode Keyword.codec (Keyword.Toxic 2)) "toxic 1 and toxic 2 encode differently"
  -- CR 702.85a. Nullary, and the tag is the whole encoding: the mana value the
  -- rule compares against is the spell's, read at resolution.
  Spec.it s "Cascade" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Cascade
      " {\"type\":\"Cascade\"} "
  -- CR 702.40a. Nullary: the count is the log's, read at resolution.
  Spec.it s "Storm" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Storm
      " {\"type\":\"Storm\"} "
  -- CR 702.69a. Nullary for Storm's reason: its count is the log's too.
  Spec.it s "Gravestorm" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Gravestorm
      " {\"type\":\"Gravestorm\"} "
  -- CR 702.78a. Nullary: the creatures it taps are written in the cost
  -- Pawl.Engine.Keyword.conspireCost mints, not onto the card.
  Spec.it s "Conspire" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Conspire
      " {\"type\":\"Conspire\"} "
  Spec.it s "Persist" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Persist
      " {\"type\":\"Persist\"} "
  -- Persist's mirror, and told apart from it by the tag alone: rules 702.79a and
  -- 702.93a differ only in a counter kind neither encoding carries.
  Spec.it s "Undying" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Undying
      " {\"type\":\"Undying\"} "
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Undying /= Codec.encode Keyword.codec Keyword.Persist) "undying and persist encode differently"
  -- CR 702.95a. Nullary: the two triggered abilities are the rule's, so the card
  -- states the word and nothing else (Wolfir Silverheart).
  Spec.it s "Soulbond" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Soulbond
      " {\"type\":\"Soulbond\"} "
  -- CR 702.131b. Nullary: the ten and the mark are the rule's, not the card's.
  Spec.it s "Ascend" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Ascend
      " {\"type\":\"Ascend\"} "
  -- CR 702.132a. Nullary: the amount the chosen player may pay is the generic
  -- mana in the total cost, so the card states no number of its own.
  Spec.it s "Assist" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Assist
      " {\"type\":\"Assist\"} "
  -- CR 702.195a. Ascend's mirror at a different count and a different mark, and
  -- told apart from it by the tag alone.
  Spec.it s "Storied" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Storied
      " {\"type\":\"Storied\"} "
    Spec.assertBool s (Codec.encode Keyword.codec Keyword.Storied /= Codec.encode Keyword.codec Keyword.Ascend) "storied and ascend encode differently"
  -- CR 702.177a. Nullary, and the tag is the whole encoding: the rider exhaust
  -- adds belongs to the ability printed after it, not to this value.
  Spec.it s "Exhaust" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Exhaust
      " {\"type\":\"Exhaust\"} "
  -- CR 702.142a. Nullary for exhaust's reason: the rider boast adds belongs to
  -- the ability printed after it, not to this value.
  Spec.it s "Boast" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Boast
      " {\"type\":\"Boast\"} "
  -- CR 702.57a. Nullary for boast's reason.
  Spec.it s "Forecast" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Forecast
      " {\"type\":\"Forecast\"} "
  -- CR 702.179a. Nullary, and the tag is the whole encoding.
  Spec.it s "StartYourEngines" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.StartYourEngines
      " {\"type\":\"StartYourEngines\"} "
  -- CR 701.43d. The one arm here that is not a rule 702 ability, and nullary
  -- because rule 701.43d's sentence carries no parameter.
  Spec.it s "Exert" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Exert
      " {\"type\":\"Exert\"} "
  -- CR 702.154a. Nullary: rule 702.154a states the whole ability, so an enlist
  -- card prints no parameter for it.
  Spec.it s "Enlist" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Enlist
      " {\"type\":\"Enlist\"} "
  -- CR 702.184a. Nullary: rule 702.184a states the whole ability, so a station
  -- card prints no parameter for it.
  Spec.it s "Station" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Station
      " {\"type\":\"Station\"} "
  -- CR 702.140a's payload is a whole Cost, and it must not share Bestow's tag:
  -- both name an alternative cost that rewrites the spell, and only the tag
  -- tells CR 702.103b's Aura from rule 702.140c's merge.
  Spec.it s "Mutate carries its cost, and is not Bestow" $ do
    let mutate n = Keyword.Mutate (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        bestowOf n = Keyword.Bestow (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (mutate 4)
      " {\"type\":\"Mutate\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":4}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (mutate 4) /= Codec.encode Keyword.codec (bestowOf 4)) "the same cost under two keywords encodes differently"
  -- CR 702.89b: the wire spelling is the CURRENT Oracle name, so a card printed
  -- "totem armor" is transcribed as this.
  Spec.it s "UmbraArmor" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.UmbraArmor
      " {\"type\":\"UmbraArmor\"} "
  -- CR 702.51d, CR 702.66c and CR 702.126c make a second instance redundant, so
  -- none of the three carries a count on the wire either.
  Spec.it s "Delve" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Delve
      " {\"type\":\"Delve\"} "
  Spec.it s "Epic" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Epic
      " {\"type\":\"Epic\"} "
  Spec.it s "Cipher" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Cipher
      " {\"type\":\"Cipher\"} "
  Spec.it s "Convoke" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Convoke
      " {\"type\":\"Convoke\"} "
  Spec.it s "Improvise" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Improvise
      " {\"type\":\"Improvise\"} "
  -- CR 702.41a's [text] rides on the wire as a Filter, landwalk's shape: the
  -- printed qualities run past card types (Frogmite's artifacts) to subtypes
  -- ("affinity for Islands") and to conjunctions ("affinity for artifact
  -- creatures"), so the criterion cannot flatten to a card type.
  Spec.it s "Affinity carries its quality" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Affinity (Filter.HasCardType CardType.Artifact))
      " {\"type\":\"Affinity\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
    Spec.assertBool
      s
      (Codec.encode Keyword.codec (Keyword.Affinity (Filter.HasCardType CardType.Artifact)) /= Codec.encode Keyword.codec (Keyword.Affinity (Filter.HasSubtype Subtype.Island)))
      "affinity for artifacts and affinity for Islands encode differently"
  Spec.it s "Undaunted" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Undaunted
      " {\"type\":\"Undaunted\"} "
  -- CR 702.81a: nullary for jump-start's reason -- the land discard the rule
  -- names is the rule's, not the card's.
  Spec.it s "Retrace" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Retrace
      " {\"type\":\"Retrace\"} "
  -- CR 702.88a: nullary. The exile, the delayed ability and its free cast are
  -- all the rule's, so a card printing rebound states nothing else.
  Spec.it s "Rebound" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Rebound
      " {\"type\":\"Rebound\"} "
  -- CR 702.187b's payload is a whole Cost, Electro's Bolt's {1}{R}, and it must
  -- not share Flashback's tag: rule 702.187b gates its permission on the discard
  -- this turn and rule 702.34a on the card type, so the two answer differently.
  Spec.it s "Mayhem carries its cost, and is not Flashback" $ do
    let mayhem n = Keyword.Mayhem (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        flashbackOf n = Keyword.Flashback (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (mayhem 2)
      " {\"type\":\"Mayhem\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (mayhem 2) /= Codec.encode Keyword.codec (flashbackOf 2)) "the same cost under two keywords encodes differently"
  -- CR 702.35a's payload is a whole Cost, Arrogant Wurm's {2}{G}, and it must not
  -- share Mayhem's tag: rule 702.35a casts from exile off a trigger and rule
  -- 702.187b from a graveyard off a permission.
  Spec.it s "Madness carries its cost, and is not Mayhem" $ do
    let madness n = Keyword.Madness (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
        mayhemOf n = Keyword.Mayhem (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) [])
    Common.assertCodec
      s
      Keyword.codec
      (madness 3)
      " {\"type\":\"Madness\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (madness 3) /= Codec.encode Keyword.codec (mayhemOf 3)) "the same cost under two keywords encodes differently"
  -- CR 702.92a, CR 702.163a and CR 702.182a: nullary, because each rule states
  -- one fixed token and takes no parameter. Three arms rather than one
  -- parameterised by the token, so a card cannot spell a token the CR never
  -- printed.
  Spec.it s "LivingWeapon" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.LivingWeapon
      " {\"type\":\"LivingWeapon\"} "
  Spec.it s "ForMirrodin" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.ForMirrodin
      " {\"type\":\"ForMirrodin\"} "
  Spec.it s "JobSelect" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.JobSelect
      " {\"type\":\"JobSelect\"} "
  Spec.it s "Spree" $
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Spree
      " {\"type\":\"Spree\"} "
  Spec.it s "Tiered, and not Spree" $ do
    Common.assertCodec
      s
      Keyword.codec
      Keyword.Tiered
      " {\"type\":\"Tiered\"} "
    Spec.assertNeWith s "CR 702.183a is not CR 702.172a: two payload-free arms must not share one tag" (Codec.encode Keyword.codec Keyword.Tiered) (Codec.encode Keyword.codec Keyword.Spree)
  -- CR 702.167a: Tithing Blade's "Craft with creature {4}{B}" -- the payload is
  -- the cost AND the materials, which is what tells this tag from every other
  -- cost-carrying arm.
  Spec.it s "Craft carries its cost and its materials" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, ManaSymbol.OfType (ManaType.Colored Color.Black)])) []
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Craft (Craft.MkCraft cost (ExileMaterials.MkExileMaterials 1 False (Filter.HasCardType CardType.Creature))))
      " {\"type\":\"Craft\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":4},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Black\"}}}]},\"materials\":{\"count\":1,\"whichObjects\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}}} "
  -- CR 702.47a: Desperate Ritual's "Splice onto Arcane {1}{R}".
  Spec.it s "Splice carries its quality and its cost" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)])) []
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Splice (Splice.MkSplice (Filter.HasSubtype Subtype.Arcane) cost))
      " {\"type\":\"Splice\",\"value\":{\"onto\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Arcane\"}},\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}}}]}}} "
  Spec.it s "Scavenge carries its cost" $ do
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Scavenge (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []))
      " {\"type\":\"Scavenge\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":4}]}} "
  Spec.it s "Encore carries its cost, and the tag tells it from scavenge" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) []
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Encore cost)
      " {\"type\":\"Encore\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Encore cost) /= Codec.encode Keyword.codec (Keyword.Scavenge cost)) "CR 702.141a is not CR 702.97a"
  -- CR 702.53a and CR 702.71a are one ability in two zones, so the two arms carry
  -- the same payload type and only the tag tells them apart.
  Spec.it s "Transmute carries its cost, and the tag tells it from transfigure" $ do
    let cost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Transmute cost)
      " {\"type\":\"Transmute\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Transfigure cost)
      " {\"type\":\"Transfigure\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Transmute cost) /= Codec.encode Keyword.codec (Keyword.Transfigure cost)) "CR 702.53a is not CR 702.71a"
  -- CR 702.146a's and CR 702.180a's payloads are whole Costs, and neither may
  -- share Flashback's tag: all three permit a cast from a graveyard, but rule
  -- 702.146a casts the card TRANSFORMED and rule 702.180a adds a tap and a
  -- reduction, so the three answer differently everywhere it matters.
  Spec.it s "Disturb and Harmonize carry their costs, and neither is Flashback" $ do
    let cost n = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Disturb (cost 2))
      " {\"type\":\"Disturb\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}} "
    Common.assertCodec
      s
      Keyword.codec
      (Keyword.Harmonize (cost 6))
      " {\"type\":\"Harmonize\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":6}]}} "
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Disturb (cost 2)) /= Codec.encode Keyword.codec (Keyword.Flashback (cost 2))) "CR 702.146a is not CR 702.34a"
    Spec.assertBool s (Codec.encode Keyword.codec (Keyword.Harmonize (cost 2)) /= Codec.encode Keyword.codec (Keyword.Disturb (cost 2))) "CR 702.180a is not CR 702.146a"
  Spec.it s "has a schema" $
    Common.assertHasSchema s Keyword.codec

-- CR 718.1: Autonomous Assembler's inset frame, "Prototype {1}{W} -- 2/2".
assemblerFrame :: Prototype.Prototype
assemblerFrame =
  Prototype.MkPrototype
    { Prototype.cost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.White)],
      Prototype.power = 2,
      Prototype.toughness = 2
    }
