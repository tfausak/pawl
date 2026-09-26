module Pawl.Types.Keyword where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Craft as Craft
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Devour as Devour
import qualified Pawl.Types.Emerge as Emerge
import qualified Pawl.Types.Equip as Equip
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ForetellCost as ForetellCost
import qualified Pawl.Types.Gift as Gift
import qualified Pawl.Types.Impending as Impending
import qualified Pawl.Types.KeywordCount as KeywordCount
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.PartnerText as PartnerText
import qualified Pawl.Types.Protection as Protection
import qualified Pawl.Types.Prototype as Prototype
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.Splice as Splice
import qualified Pawl.Types.Suspend as Suspend
import qualified Pawl.Types.Ward as Ward

-- | CR 702. A keyword is a CITATION, not an effect: rule 702 is part of the
-- comprehensive rules, the same as rule 506 or rule 302. So casing on this is NOT
-- a violation of the closed/open invariant, which forbids the rules core casing
-- on the IDENTITY OF AN EFFECT. The test is "is it in the rulebook?" -- Flying is
-- 702.9; Goblin Piker is not. Constructors are ordered by RULE NUMBER, not by
-- arrival, so this type stays diffable against rule 702 itself.
--
-- One constructor is NOT a rule 702 ability and sits after the rest rather than
-- inside that ordering: Exert is rule 701.43's KEYWORD ACTION, and CR 701.43d is
-- what puts a static ability naming it on a card. It is a citation by the same
-- test -- rule 701 is as much the rulebook as rule 702 -- and Wizards' own card
-- data calls it a keyword, so it belongs here rather than in a Face field of its
-- own.
--
-- A keyword is not necessarily a STATIC ability: rule 702.70 spells poisonous out
-- as a TRIGGERED one. What it grants is still a citation and not an effect
-- identity, so Pawl.Engine.Keyword may read a constructor and mint the rule's
-- ability from it.
--
-- Multiplicity is NOT this type's problem: an object can have the same keyword
-- ability twice, which Pawl.Types.ProjectedCharacteristics.keywords carries as a
-- count and Pawl.Types.Face.keywords carries the same way for the PRINTED ones
-- (CR 702.85c). This type says only WHICH ability.
--
-- This module TIES THE KNOT that Pawl.Types.Filter's keyword parameter opens:
-- Filter has a HasKeyword arm and this type carries a Filter (702.6c, 702.11d,
-- 702.14c, 702.29e) and a Cost (702.29a/702.33a/702.34a/702.42a) whose components carry one too, so the
-- three would be a module cycle if any were concrete. They are parametric and
-- this one is not, which makes `Filter Keyword` and `Cost Keyword` the only
-- instantiations anywhere.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | -- | 702.6a: "Equip [cost]" means "[Cost]: Attach this permanent to target
    -- creature you control. Activate only as a sorcery." One ability per
    -- instance (CR 702.6d), and CR 702.6c's optional quality on the payload.
    Equip (Equip.Equip Keyword)
  | -- | 702.6e: "Equip planeswalker [cost]", equip's variant targeting a
    -- planeswalker you control as though it were a creature.
    EquipPlaneswalker (Cost.Cost Keyword)
  | FirstStrike -- 702.7
  | -- | 702.8a: may be played any time you could cast an instant.
    Flash
  | Flying -- 702.9
  | Haste -- 702.10
  | -- | 702.11b with Nothing, 702.11d with Just: can't be the target of
    -- opponents' spells or abilities, optionally narrowed to a quality (CR
    -- 702.11f, CR 702.11g).
    Hexproof (Maybe (Filter.Filter Keyword))
  | Indestructible -- 702.12
  | Intimidate -- 702.13
  | -- | 702.14a: "[type]walk", the qualification riding a Filter because CR
    -- 702.14c's four shapes reach past a bare land type.
    Landwalk (Filter.Filter Keyword)
  | -- | 702.15b: damage dealt by a source with lifelink causes that source's
    -- controller to gain that much life (CR 702.15c, CR 702.15d).
    Lifelink
  | -- | 702.161a: "During your turn, this permanent is an artifact creature in
    -- addition to its other types." A layer 4 static ability (CR 613.1d), not a
    -- characteristic-defining one (CR 604.3).
    LivingMetal
  | -- | 702.162a: "You may cast this card converted by paying [cost] rather than
    -- its mana cost", read off the front face (CR 712.11d).
    MoreThanMeetsTheEye (Cost.Cost Keyword)
  | -- | 702.16a: four prohibitions under one key -- targeting (702.16b), damage
    -- (702.16e), blocking (702.16f), and attachment (702.16c, 702.16d).
    --
    -- Rule 702.16j's "protection from everything" is Filter.And [], no variant
    -- constructor: Progenitus writes it, and Pawl.TargetSpec's "CR 702.16j" and
    -- Pawl.DamageSpec's "CR 702.16j" prove the targeting and damage halves.
    --
    -- Rule 702.16k's "protection from [a player]" is Filter.OfChosenPlayer, no
    -- variant constructor either: True-Name Nemesis writes it, and its four
    -- prohibitions are proved by Pawl.DamageSpec's, Pawl.TargetSpec's,
    -- Pawl.CombatSpec's and Pawl.AuraSpec's "CR 702.16k" cases.
    --
    -- Not implemented: a LINT over the "from each" shorthands of rules 702.16h
    -- and 702.16i, which expand to one instance of this constructor per quality
    -- and so are a transcription nothing checks (#3200).
    --
    -- Rule 702.16n's exception is the payload's second field rather than a
    -- variant constructor: Spectra Ward writes it, and Pawl.AuraSpec's "CR
    -- 702.16n" proves that the spared Aura stays and an unspared attachment
    -- still goes.
    Protection (Protection.Protection Keyword)
  | Reach -- 702.17
  | -- | 702.18a: this permanent or player can't be the target of spells or
    -- abilities.
    Shroud
  | Trample -- 702.19
  | -- | 702.19c: trample over planeswalkers, a second tier to CR 702.19b's gate;
    -- a sibling of Trample, CR 702.19d naming the two side by side (CR 702.19e).
    TrampleOverPlaneswalkers
  | Vigilance -- 702.20
  | -- | 702.21a: ward [cost], a triggered ability countering the targeting spell
    -- or ability unless its controller pays. CR 702.21b's X rides the payload's
    -- second field; see Pawl.Types.Ward.
    Ward (Ward.Ward Keyword)
  | -- | 702.22: banding, of which only the two combat-damage-division halves are
    -- modeled (CR 702.22j and CR 702.22k). CR 702.22c's band of attackers is a
    -- shape the declare-attackers step does not have.
    Banding -- 702.22
  | -- | 702.23a: rampage N -- whenever this creature becomes blocked, it gets
    -- +N/+N until end of turn for each creature blocking it beyond the first;
    -- each instance triggers separately (CR 702.23c).
    Rampage Natural.Natural
  | -- | 702.24a: at the beginning of your upkeep, put an age counter on this
    -- permanent, then sacrifice it unless [cost] is paid once per age counter;
    -- each instance triggers separately and every instance counts the one pile
    -- (CR 702.24b).
    CumulativeUpkeep (Cost.Cost Keyword)
  | -- | 702.25a: whenever this creature becomes blocked by a creature without
    -- flanking, the blocking creature gets -1/-1 until end of turn; each instance
    -- triggers separately (CR 702.25b).
    Flanking -- 702.25
  | -- | 702.26a: a static ability modifying the rules of the untap step, read by
    -- Pawl.Engine.Phasing off the CR 502.1 turn-based action rather than minted.
    Phasing
  | -- | 702.27a: "You may pay an additional [cost] as you cast this spell", plus
    -- "if the buyback cost was paid, put this spell into its owner's hand instead
    -- of into that player's graveyard as it resolves".
    Buyback (Cost.Cost Keyword)
  | -- | 702.28b: a creature with shadow can't be blocked by creatures without
    -- shadow, and one without shadow can't be blocked by creatures with it.
    Shadow
  | -- | 702.29a: "[Cost], Discard this card: Draw a card", functioning only while
    -- the card is in a player's hand; Just is CR 702.29e's typecycling search,
    -- riding this constructor so CR 702.29f holds for free.
    Cycling (Cycling.Cycling Keyword)
  | -- | 702.30a: at the beginning of your upkeep, if this permanent came under
    -- your control since the beginning of your last upkeep, sacrifice it unless
    -- you pay [cost]. CR 702.30b's Urza-block errata is already in the Oracle
    -- text, so the cost is always printed and never defaulted from the mana cost.
    Echo (Cost.Cost Keyword)
  | -- | 702.31b: a creature with horsemanship can't be blocked by creatures
    -- without horsemanship.
    Horsemanship
  | -- | 702.32a: fading N -- enter with N fade counters, and at each upkeep
    -- remove one or sacrifice the permanent.
    Fading Natural.Natural
  | -- | 702.33a: "You may pay an additional [cost] as you cast this spell", plus
    -- CR 702.33d's "kicked" designation. CR 702.33b's two kicker costs are two of
    -- these on one face, which the keyword Set keeps apart by cost.
    --
    -- Not implemented: CR 702.33h's sticker kicker, which needs stickers (#872).
    Kicker (Cost.Cost Keyword)
  | -- | 702.33c: "You may pay an additional [cost] any number of times as you
    -- cast this spell". A multikicker cost IS a kicker cost, so everything
    -- downstream of the announcement treats the two alike.
    Multikicker (Cost.Cost Keyword)
  | -- | 702.34a: cast this card from its owner's graveyard for the given cost,
    -- exiling it as it leaves the stack.
    Flashback (Cost.Cost Keyword)
  | Fear -- 702.36
  | -- | 702.37a: cast this card as a 2/2 face-down creature for {3}; the Cost is
    -- CR 702.37e's turn-up special action, and CR 702.37b's megamorph is a
    -- MorphVariant.
    Morph (Morph.Morph Keyword)
  | -- | 702.38a: amplify N -- as this object enters, reveal any number of cards
    -- from your hand that share a creature type with it, and it enters with N
    -- +1/+1 counters for each card revealed this way.
    Amplify Natural.Natural
  | -- | 702.39a: whenever this creature attacks, you may have target creature
    -- defending player controls block it if able; if you do, untap that creature.
    Provoke
  | -- | 702.40a: storm -- a triggered ability that functions on the stack. When
    -- you cast the spell, copy it for each other spell cast before it this turn;
    -- you may choose new targets for the copies.
    Storm
  | -- | 702.41a: "affinity for [text]" -- this spell costs {1} less to cast for
    -- each [text] you control. The Filter is the [text] alone; "you control" is
    -- the rule's own and is written into the reduction
    -- Pawl.Engine.Keyword.selfCostReductionsFor mints.
    Affinity (Filter.Filter Keyword)
  | -- | 702.42a: you may choose all modes of this modal spell (CR 700.2) instead
    -- of the number specified, paying an additional cost if you do.
    Entwine (Cost.Cost Keyword)
  | -- | 702.43a: modular N -- enter with N +1/+1 counters, and on death move the
    -- counters actually present to target artifact creature; each instance works
    -- separately (CR 702.43b).
    --
    -- A Natural and not a Quantity: CR 702.44c's "Modular--Sunburst" (Arcbound
    -- Wanderer) is the one printing whose N is another keyword's count.
    --
    -- Not implemented: that printing (gap #3699).
    Modular Natural.Natural
  | -- | 702.44a: a static ability functioning as this object enters -- with a
    -- +1/+1 counter for each color of mana spent to cast it if it is entering as
    -- a creature ignoring type-changing effects, and a charge counter for each
    -- otherwise; each instance works separately (CR 702.44d).
    --
    -- Nullary: rule 702.44a fixes both halves, and CR 702.44b's count is the
    -- entering object's own CR 400.7d mana record rather than anything a card
    -- prints.
    --
    -- Not implemented: CR 702.44c's use of the word to set another ability's
    -- number -- Arcbound Wanderer's "Modular--Sunburst" (gap #3699).
    Sunburst
  | -- | 702.45a: whenever this creature blocks or becomes blocked, it gets +N/+N
    -- until end of turn (CR 509.3a, CR 509.3c); each instance triggers separately
    -- (CR 702.45b).
    Bushido Natural.Natural
  | -- | 702.46a: soulshift N -- when this permanent dies, you may return target
    -- Spirit card with mana value N or less from your graveyard to your hand;
    -- each instance triggers separately (CR 702.46b).
    Soulshift Natural.Natural
  | -- | 702.47a: splice onto [quality] [cost] -- a static ability functioning
    -- from a hand, read by Pawl.Engine.Cast's CR 601.2b announcement.
    Splice (Splice.Splice Keyword)
  | -- | 702.48a: [quality] offering -- an optional additional cost sacrificing a
    -- [quality] permanent, which reduces the total by its mana cost and lets the
    -- spell be cast any time its caster could cast an instant.
    Offering (Filter.Filter Keyword)
  | -- | 702.49a: ninjutsu [cost] -- an activated ability functioning only from a
    -- hand, whose cost returns an unblocked attacker you control and whose effect
    -- puts this card onto the battlefield tapped and attacking.
    --
    -- Rule 702.49a's "Reveal this card from your hand" is not a component of the
    -- minted cost. CR 602.2a reveals the card anyway -- the ability is activated
    -- from a hidden zone -- and CR 701.20a gives the two reveals the same
    -- duration, so a second one would only reveal an already revealed card.
    -- Pawl.Engine.Activate.revealIfHidden is where it happens.
    --
    -- Not implemented: CR 702.49d's commander ninjutsu, which also functions
    -- from the command zone (#3436).
    Ninjutsu (Cost.Cost Keyword)
  | -- | 702.50a: two spell abilities -- "for the rest of the game, you can't
    -- cast spells", and a delayed triggered ability that copies this spell
    -- except for its epic ability at the beginning of each of your upkeeps for
    -- the rest of the game (CR 707.10). Pawl.Engine.Resolve.applyEpic performs
    -- both as the spell finishes resolving, rule 702.50a calling them SPELL
    -- abilities.
    --
    -- Not implemented: CR 707.10's copied decisions -- the targets, the modes and
    -- the value of X -- which the archived spell carries none of, so an epic card
    -- that announces one cannot be transcribed (Eternal Dominion, #3708).
    Epic
  | -- | 702.51a: for each colored mana in this spell's total cost you may tap an
    -- untapped creature of that color you control rather than pay that mana, and
    -- for each generic mana an untapped creature you control. CR 702.51b puts it
    -- after the total cost is determined, so the offer lives in
    -- Pawl.Engine.Cost.manaSubstitutions rather than among the candidate costs;
    -- CR 702.51d makes a second instance redundant, which a Set already is.
    --
    -- CR 702.51c's record of which creatures convoked the spell is
    -- GameEvent.Convoked, written by Pawl.Engine.Cast as the cost is paid and
    -- read by Filter.ConvokedSourceThisTurn; Pawl.CostSpec's "CR 702.51c the
    -- entry trigger grows the creatures that convoked the Loxodon, and nothing
    -- else" is the proof.
    Convoke
  | -- | 702.52a: dredge N -- a static ability functioning only from a graveyard,
    -- replacing a draw with "mill N cards and return this card from your
    -- graveyard to your hand"; Pawl.Engine.Keyword.graveyardReplacementsOf
    -- mints the row.
    Dredge Natural.Natural
  | -- | 702.54a: bloodthirst N -- if an opponent was dealt damage this turn, this
    -- permanent enters with N +1/+1 counters on it; a CR 614.1c entry replacement
    -- carrying its own condition, each instance applying separately (CR 702.54c).
    --
    -- Nothing is rule 702.54b's "bloodthirst X", Vanishing's spelling of the same
    -- want: X is the total damage this permanent's controller's opponents were
    -- dealt this turn, so it names no printed number and states no condition.
    Bloodthirst (Maybe Natural.Natural)
  | -- | 702.55a: when this permanent dies -- or, on an instant or sorcery, when
    -- that spell is put into a graveyard during its resolution -- exile it
    -- haunting target creature; CR 702.55b's haunted object is board state rather
    -- than a characteristic. Pawl.Engine.Keyword.graveyardTriggeredAbilitiesOf
    -- mints the spell sentence and triggeredAbilitiesOf the permanent one.
    Haunt
  | -- | 702.56a: replicate [cost] -- an optional additional cost payable any
    -- number of times, and a cast trigger copying the spell once per payment,
    -- minted by Pawl.Engine.Keyword.stackTriggeredAbilitiesOf.
    Replicate (Cost.Cost Keyword)
  | -- | 702.58a: graft N -- a static ability entering the permanent with N
    -- +1\/+1 counters, and a triggered ability offering to move one of them onto
    -- another creature as that creature enters. The first is minted by
    -- Pawl.Engine.Keyword.mintedReplacementsFor and the second by
    -- Pawl.Engine.Keyword.triggeredAbilitiesOf; CR 702.58b's several instances
    -- each work separately, which is that function's count.
    Graft Natural.Natural
  | -- | 702.59a: recover [cost] -- a triggered ability that functions only while
    -- the card with recover is in a player's graveyard, returning it to hand for
    -- [cost] when a creature dies and exiling it otherwise. Minted by
    -- Pawl.Engine.Keyword.graveyardTriggeredAbilitiesOf.
    Recover (Cost.Cost Keyword)
  | -- | 702.60a: ripple N -- a triggered ability that functions only while the
    -- card with ripple is on the stack. When you cast the spell, you may reveal
    -- the top N cards of your library, cast any of them with the same name as
    -- this spell for free, and put the rest on the bottom of your library in any
    -- order. Minted by Pawl.Engine.Keyword.stackTriggeredAbilitiesOf, cascade's
    -- neighbour there; CR 702.60b's several instances trigger separately, which
    -- is that function's count.
    Ripple Natural.Natural
  | -- | 702.61a: "As long as this spell is on the stack, players can't cast
    -- spells or activate abilities that aren't mana abilities." A
    -- rules-modifying continuous effect (CR 611.1) other players' gates ask
    -- about, so nothing is minted.
    SplitSecond
  | -- | 702.62a: suspend N--[cost] -- three abilities. CR 116.2f's special
    -- action exiles the card from hand with N time counters for [cost]
    -- (Pawl.Engine.Suspend); the two triggered abilities function in exile and
    -- are minted by Pawl.Engine.Keyword.exileTriggeredAbilitiesOf. The last
    -- sentence's haste is Pawl.Engine.Stack.armBecame's, off the tag the third
    -- ability's offer leaves on Pawl.Types.Object's castUsing.
    Suspend (Suspend.Suspend Keyword)
  | -- | 702.63a: vanishing N -- enter with N time counters, remove one at each
    -- upkeep, and sacrifice the permanent when the last one goes. Nothing is CR
    -- 702.63b's numberless printing, which states only the last two abilities.
    Vanishing (Maybe Natural.Natural)
  | -- | 702.64a: absorb N -- a static ability preventing N of the damage a source
    -- would deal to this creature, minted as a damage replacement by
    -- Pawl.Engine.Keyword.mintedReplacementsFor. CR 702.64b's ceiling is per
    -- event rather than a countdown, and CR 702.64c's several instances each
    -- apply separately, which is that function's count.
    Absorb Natural.Natural
  | -- | 702.66a: for each generic mana in this spell's total cost you may exile a
    -- card from your graveyard rather than pay that mana. Convoke's neighbour
    -- below in placement -- CR 702.66b puts it where CR 702.51b puts that one, so
    -- the offer lives in Pawl.Engine.Cost.manaSubstitutions -- and its opposite in
    -- what it spends: cards out of a graveyard rather than untapped permanents.
    -- CR 702.66c makes a second instance redundant, which a Set already is.
    Delve
  | -- | 702.67a: "[Cost]: Attach this Fortification to target land you control.
    -- Activate only as a sorcery." One ability per instance (CR 702.67c).
    Fortify (Cost.Cost Keyword)
  | -- | 702.68a: frenzy N -- whenever this creature attacks and isn't blocked, it
    -- gets +N/+0 until end of turn; each instance triggers separately (CR
    -- 702.68b).
    Frenzy Natural.Natural
  | -- | 702.69a: gravestorm -- a triggered ability that functions on the stack.
    -- When you cast the spell, copy it for each permanent put into a graveyard
    -- from the battlefield this turn; you may choose new targets for the copies.
    Gravestorm
  | -- | 702.70a: whenever this creature deals combat damage to a player, that
    -- player gets N poison counters; the Ns are not summed, each instance
    -- triggering separately (CR 702.70b).
    Poisonous Natural.Natural
  | -- | 702.72a: "champion an [object]" -- two triggered abilities, minted by
    -- Pawl.Engine.Keyword.abilitiesFor. The Filter is the [object] alone;
    -- "another" and "you control" are the rule's own and are written into the
    -- entry ability, Affinity's split. Rule 702.72b links the pair through the
    -- exile pile (CR 607.2k), which is why the return names
    -- ObjectRef.EachCardExiledWithSource rather than a slot of its own.
    --
    -- Not implemented: CR 702.72c's "championed by", which no trigger can watch
    -- -- Mistbind Clique's "When a Faerie is championed with this creature"
    -- (#3825).
    Champion (Filter.Filter Keyword)
  | -- | 702.73a: "This object is every creature type." A characteristic-defining
    -- ability (CR 604.3) landing in layer 4.
    Changeling
  | -- | 702.74a: evoke [cost] -- cast this card for [cost] rather than its mana
    -- cost, and sacrifice it as it enters if that cost was paid.
    Evoke (Cost.Cost Keyword)
  | -- | 702.75a: hideaway N -- a triggered ability. On entry, look at the top N
    -- cards of your library, exile one of them face down and put the rest on the
    -- bottom in a random order; the exiled card gains a look for whoever controls
    -- the permanent that exiled it. CR 702.75b's "enters tapped" is that rule's
    -- errata on the older cards rather than part of the keyword.
    Hideaway Natural.Natural
  | -- | 702.76a: prowl [cost] -- you may pay [cost] rather than this spell's mana
    -- cost if a player was dealt combat damage this turn by a source that was
    -- then under your control and had any of this spell's creature types, which
    -- CR 601.2b and CR 601.2f-h price as an alternative cost.
    Prowl (Cost.Cost Keyword)
  | -- | 702.77a: reinforce N-[cost] -- "[Cost], Discard this card: Put N +1/+1
    -- counters on target creature", functioning only while the card is in a
    -- player's hand.
    --
    -- The hand-only half is not a field here: rule 702.77b keeps the ability in
    -- existence in every other zone. Pawl.UntapRestrictionSpec's "CR
    -- 502.3/702.77b whole cards: under Tsabo's Web the Rustic Clachan does not
    -- untap" is what proves that half.
    Reinforce (Reinforce.Reinforce Keyword)
  | -- | 702.78a: conspire -- an optional additional cost of tapping two untapped
    -- creatures you control that each share a color with the spell, and a cast
    -- trigger copying the spell once if it was paid, both minted by
    -- Pawl.Engine.Keyword.
    Conspire
  | -- | 702.79a: when this permanent dies, if it had no -1/-1 counters on it,
    -- return it to the battlefield under its owner's control with one.
    Persist
  | -- | 702.80a: damage this source deals to a creature isn't marked on it;
    -- instead its controller puts that many -1/-1 counters on that creature (CR
    -- 120.3d).
    Wither
  | -- | 702.82a \/ 702.82c: devour N, or devour [quality] N -- as this object
    -- enters, you may sacrifice any number of creatures (or of [quality]
    -- permanents), and it enters with N +1/+1 counters for each.
    Devour (Devour.Devour Keyword)
  | -- | 702.83a: whenever a creature you control attacks alone, that creature
    -- gets +1/+1 until end of turn. Two instances are two abilities.
    Exalted
  | -- | 702.84a: "[Cost]: Return this card from your graveyard to the
    -- battlefield. It gains haste. Exile it at the beginning of the next end step.
    -- If it would leave the battlefield, exile it instead of putting it anywhere
    -- else. Activate only as a sorcery." An activated ability that functions in a
    -- graveyard, so Pawl.Engine.Keyword.graveyardAbilitiesFor mints it.
    Unearth (Cost.Cost Keyword)
  | -- | 702.85a: cascade -- a triggered ability that functions only while the
    -- spell with cascade is on the stack. When you cast the spell, exile cards
    -- from the top of your library until you exile a nonland card whose mana
    -- value is less than the spell's, you may cast that card without paying its
    -- mana cost, and the rest go on the bottom of your library in a random order.
    Cascade
  | -- | 702.86a: whenever this creature attacks, defending player (CR 508.5)
    -- sacrifices N permanents; each instance triggers separately (CR 702.86b).
    Annihilator Natural.Natural
  | -- | 702.87a: "[Cost]: Put a level counter on this permanent. Activate only as
    -- a sorcery." The card's level symbols are ordinary conditional static
    -- abilities (CR 711.2a, CR 711.3, CR 711.4) rather than part of this keyword.
    LevelUp (Cost.Cost Keyword)
  | Infect -- 702.90
  | -- | 702.91a: whenever this creature attacks, each other attacking creature
    -- gets +1/+0 until end of turn; each instance triggers separately (CR
    -- 702.91b).
    BattleCry
  | -- | 702.92a: "When this Equipment enters, create a 0\/0 black Phyrexian Germ
    -- creature token, then attach this Equipment to it."
    LivingWeapon
  | -- | 702.93a: when this permanent dies, if it had no +1/+1 counters on it,
    -- return it to the battlefield under its owner's control with one --
    -- Persist's mirror.
    Undying
  | -- | 702.94a: miracle [cost] -- a static ability linked (CR 603.11) to a
    -- triggered one, letting the card be revealed as the turn's first draw and
    -- then cast for [cost].
    --
    -- Not implemented: both halves live in the hand (CR 113.6b) and so are read
    -- off a card's printed keywords, which misses an effect that granted miracle
    -- there (#1859).
    Miracle (Cost.Cost Keyword)
  | -- | 702.95a: soulbond -- two triggered abilities that may pair this creature
    -- with another unpaired creature its controller controls, on either one's
    -- entry.
    Soulbond
  | -- | 702.96a: overload [cost] -- an alternative cost, plus a CR 612.1
    -- text-changing effect replacing every "target" with "each" if it was paid.
    --
    -- Not implemented: the text change as a text change (#3686). What the engine
    -- reads is rule 702.96b's consequence alone, the spell requiring no targets
    -- (Pawl.Engine.Keyword.castOverloaded), and the card states its "each"
    -- reading as a second clause of the same mode gated on Quantity.CastUsing --
    -- Cleave's shape below, and Cyclonic Rift's.
    Overload (Cost.Cost Keyword)
  | -- | 702.98a: "You may have this permanent enter with an additional +1/+1
    -- counter on it" and "This permanent can't block as long as it has a +1/+1
    -- counter on it". The second half is not conditional on the first.
    Unleash
  | -- | 702.99a: a spell ability that may exile this card encoded on a creature
    -- its controller controls, and an exile-zone static ability granting that
    -- creature a trigger that casts a copy of the card; performed by
    -- Pawl.Engine.Resolve.applyCipher and Pawl.Engine.Projection.encodedGathered.
    Cipher
  | -- | 702.100a:whenever a creature you control enters, if that creature's
    -- power and/or toughness is greater than this creature's, put a +1/+1 counter
    -- on this creature; each instance triggers separately (CR 702.100d).
    Evolve
  | -- | 702.101a: whenever you cast a spell, you may pay {W\/B}; if you do, each
    -- opponent loses 1 life and you gain that much life. Each instance triggers
    -- separately (CR 702.101b).
    Extort
  | -- | 702.102a: a player casting this split card from their hand may cast both
    -- halves as one fused split spell (CR 702.102b-d).
    Fuse
  | -- | 702.104a: tribute N -- as this creature enters, its controller chooses an
    -- opponent, and that opponent may put N additional +1/+1 counters on it; a CR
    -- 614.1c entry replacement, minted by
    -- Pawl.Engine.Keyword.mintedReplacementsFor. CR 702.104b's "if tribute wasn't
    -- paid" reads the opponent's answer back off Object.tributePaid.
    Tribute Natural.Natural
  | -- | 702.105a: whenever this creature attacks the player with the most life or
    -- tied for most life, put a +1/+1 counter on it; each instance triggers
    -- separately (CR 702.105b).
    Dethrone
  | -- | 702.107a: "[Cost], {T}: Put a +1/+1 counter on this creature. Activate
    -- only as a sorcery."
    Outlast (Cost.Cost Keyword)
  | -- | 702.108a: whenever you cast a noncreature spell, this creature gets +1/+1
    -- until end of turn; each instance triggers separately (CR 702.108b).
    Prowess
  | -- | 702.109a: dash [cost] -- an alternative cost; the permanent has haste
    -- and returns to its owner's hand at the next end step.
    Dash (Cost.Cost Keyword)
  | -- | 702.110a: "When this creature enters, you may sacrifice a creature."
    --
    -- Not implemented: rule 702.110b's phrase asked of a creature that is not the
    -- bearer -- Skull Skaab's "whenever a creature you control exploits a nontoken
    -- creature" (#3845).
    Exploit
  | -- | 702.111b: a creature with menace can't be blocked except by two or more
    -- creatures.
    Menace
  | -- | 702.112a: renown N -- when this creature deals combat damage to a player,
    -- if it isn't renowned, put N +1/+1 counters on it and it becomes renowned;
    -- renowned itself is a designation (CR 702.112b).
    Renown Natural.Natural
  | -- | 702.113a: awaken N--[cost] -- an alternative cost, plus a spell ability
    -- that runs only if that cost was paid, putting N +1/+1 counters on a target
    -- land you control and animating it.
    --
    -- THE COST ALONE is the payload, Cleave's shape: nothing in pawl mints a
    -- SPELL ability from a keyword, so the card states that half itself, as a
    -- clause gated on Quantity.CastUsing, and rule 702.113a's N is written where
    -- the counters are put rather than copied here. No printing grants awaken to
    -- another object, which is what makes the two spellings observably the same.
    -- Rule 702.113b's "the spell is cast as if it didn't have that target" is
    -- Pawl.Engine.Cast.trimModeTargetSlots, proved by Pawl.CastSpec's "CR
    -- 702.113b the awaken-only land target is asked only on the awakened cast".
    Awaken (Cost.Cost Keyword)
  | Devoid -- 702.114
  | -- | 702.115a: whenever this creature deals combat damage to a player, that
    -- player exiles the top card of their library, face up (CR 406.3); each
    -- instance triggers separately (CR 702.115b).
    Ingest
  | -- | 702.116a: whenever this creature attacks, for each opponent other than
    -- defending player, you may create a tapped token copy of it attacking that
    -- player or a planeswalker they control, and exile those tokens at end of
    -- combat; each instance triggers separately (CR 702.116b).
    Myriad
  | -- | 702.117a: surge [cost] -- you may pay [cost] rather than this spell's
    -- mana cost as you cast it if you or a teammate has cast another spell this
    -- turn, which CR 601.2b and CR 601.2f-h price as an alternative cost.
    Surge (Cost.Cost Keyword)
  | -- | 702.118b: a creature with skulk can't be blocked by creatures with
    -- greater power.
    Skulk
  | -- | 702.119a: emerge [cost] -- an alternative cost of [cost] plus sacrificing
    -- a creature, whose total is then reduced by generic mana equal to that
    -- creature's mana value, and CR 702.119b's optional quality on the payload,
    -- which names a PERMANENT pool in place of the creatures. Priced by
    -- Pawl.Engine.Cost.candidateCostsGiven, which offers one candidate per
    -- sacrificeable permanent, naming it: CR 702.119c makes the victim a CR
    -- 601.2b choice and CR 601.2f needs its mana value before CR 601.2h
    -- sacrifices that same permanent.
    Emerge (Emerge.Emerge Keyword)
  | -- | 702.120a: for each mode of this modal spell (CR 700.2) chosen beyond the
    -- first, an additional cost paid as it is cast.
    Escalate (Cost.Cost Keyword)
  | -- | 702.121a: whenever this creature attacks, it gets +1/+1 until end of turn
    -- for each opponent you attacked with a creature this combat.
    Melee
  | -- | 702.122a: "Tap any number of other untapped creatures you control with
    -- total power N or greater: This permanent becomes an artifact creature until
    -- end of turn."
    Crew Natural.Natural
  | -- | 702.123a: fabricate N -- when this permanent enters, you may put N +1/+1
    -- counters on it, and if you don't, create N 1/1 Servo tokens.
    Fabricate Natural.Natural
  | -- | 702.124h: "You may designate two legendary cards as your commander
    -- rather than one if each of them has partner." A deck-construction ability
    -- that functions before the game begins (CR 702.124a), read by
    -- Pawl.Engine.Commander.designations.
    Partner
  | -- | 702.124i: partner—[text], pairing only with the same text; read by
    -- Pawl.Engine.Commander.designations.
    PartnerText PartnerText.PartnerText
  | -- | 702.124j: partner with [name], TWO abilities -- the deck-construction
    -- pairing Pawl.Engine.Commander.designations reads, and the entry trigger
    -- Pawl.Engine.Keyword.partnerWith mints. The payload is the OTHER card's
    -- name, which both halves read.
    PartnerWith CardName.CardName
  | -- | 702.124k: "You may designate two cards as your commander rather than one
    -- if one of them is this card and the other is a legendary Background
    -- enchantment card." A deck-construction ability like Partner above, read by
    -- Pawl.Engine.Commander.designations.
    ChooseABackground
  | -- | 702.124m: "You may designate two legendary creature cards as your
    -- commander rather than one if one of them is this card and the other is a
    -- legendary Time Lord Doctor creature card that has no other creature
    -- types." A deck-construction ability like Partner above, read by
    -- Pawl.Engine.Commander.designations.
    DoctorsCompanion
  | -- | 702.125a: this spell costs {1} less to cast for each opponent you have.
    Undaunted
  | -- | 702.126a: for each generic mana in this spell's total cost you may tap an
    -- untapped artifact you control rather than pay that mana. Convoke's
    -- neighbour above in every respect -- CR 702.126b places it where CR 702.51b
    -- places that one and CR 702.126c makes a second instance redundant -- and
    -- narrower in two: artifacts rather than creatures, and no colored mana.
    Improvise
  | -- | 702.127a: three static abilities in one word -- cast this half from your
    -- graveyard, never from anywhere else, and exile it as it leaves the stack.
    Aftermath
  | -- | 702.128a: a graveyard ability exiling this card for a white Zombie token
    -- copy of it with no mana cost.
    Embalm (Cost.Cost Keyword)
  | -- | 702.129a: embalm's sibling, whose token copy is black and 4/4.
    Eternalize (Cost.Cost Keyword)
  | -- | 702.130a: whenever this creature becomes blocked, defending player loses
    -- N life; each instance triggers separately (CR 702.130b).
    Afflict Natural.Natural
  | -- | 702.131a \/ 702.131b: "you control ten or more permanents and you don't
    -- have the city's blessing" grants it for the rest of the game -- a spell
    -- ability on an instant or sorcery, a static one anywhere else, both carried
    -- out by Pawl.Engine.PlayerDesignation rather than minted.
    Ascend
  | -- | 702.132a: a static ability modifying the rules of paying for the spell
    -- (CR 601.2g-h) -- before you activate mana abilities you may choose another
    -- player, who then activates theirs and may pay for any amount of the
    -- generic mana in the total cost. Pawl.Engine.Cost.offerAssist and
    -- Pawl.Engine.Cost.payAssist are the two halves.
    Assist
  | -- | 702.133a: cast this card from your graveyard by discarding a card as an
    -- ADDITIONAL cost, and exile it as it leaves the stack.
    JumpStart
  | -- | 702.134a: whenever this creature attacks, put a +1/+1 counter on target
    -- attacking creature with power less than this creature's power.
    --
    -- Not implemented: CR 702.134c's "an ability that triggers whenever a
    -- creature mentors another creature", a trigger condition no card in pawl's
    -- pool prints (#1159).
    Mentor
  | -- | 702.135a: afterlife N -- when this permanent dies, create N 1/1 white and
    -- black Spirit creature tokens with flying; each instance triggers separately
    -- (CR 702.135b).
    Afterlife Natural.Natural
  | -- | 702.136a: "You may have this permanent enter with an additional +1/+1
    -- counter on it. If you don't, it gains haste." A CR 614.1c as-enters
    -- replacement; each instance works separately (CR 702.136b).
    Riot
  | -- | 702.137a: spectacle [cost] -- you may pay [cost] rather than this
    -- spell's mana cost if an opponent lost life this turn, which CR 601.2b and
    -- CR 601.2f-h price as an alternative cost.
    Spectacle (Cost.Cost Keyword)
  | -- | 702.138a: "escape--[cost]" -- you may cast this card from your graveyard
    -- by paying [cost] rather than paying its mana cost, which CR 601.2b and CR
    -- 601.2f-h price as an alternative cost.
    Escape (Cost.Cost Keyword)
  | -- | 702.139a: "Companion--[Condition]" -- reveal this card from outside the
    -- game before the game begins if your starting deck fulfills the condition
    -- (CR 103.2b), then once during the game pay {3} to put it into your hand (CR
    -- 116.2g). The Filter is the condition, and EVERY card in the starting deck
    -- must match it: Zirda, the Dawnwaker's "each permanent card in your starting
    -- deck has an activated ability" is `Or [Not permanentCard, HasActivatedAbility]`.
    --
    -- A PER-CARD predicate, which is what makes one Filter enough: rule 702.139a
    -- fixes no shape, and the subject/requirement pair every printed companion but
    -- four writes collapses into an implication a single Filter states. Not
    -- implemented: the four whose condition is not per-card -- Yorion, Sky Nomad's
    -- deck size, Lutri, the Spellchaser's singleton, Umori, the Collector's shared
    -- card type, and Jegantha, the Wellspring's repeated mana symbol (#3261).
    Companion (Filter.Filter Keyword)
  | -- | 702.143a: foretell [cost] -- exile this card from hand face down for {2}
    -- (CR 116.2h's special action), then cast it later for [cost]. The payload
    -- is the CAST's cost.
    --
    -- CR 702.143d's other producer -- an effect that makes an exiled card
    -- foretold and may give it a foretell cost -- is Effect.MakeForetold, and
    -- needs no keyword at all.
    Foretell (ForetellCost.ForetellCost Keyword)
  | -- | 702.144a: demonstrate -- a cast trigger under which you may copy the
    -- spell, and an opponent you then choose copies it too.
    Demonstrate
  | -- | 702.145b: daybound, the front-face half of rule 702.145's pair, and three
    -- static abilities read by Pawl.Engine.Daytime rather than minted. Day and
    -- night themselves are on the game (CR 731.1).
    Daybound
  | -- | 702.145e: nightbound, daybound's back-face mirror and two static
    -- abilities rather than three (CR 702.145d, CR 702.145g).
    Nightbound
  | -- | 702.147a: "This creature can't block" and "When this creature attacks,
    -- sacrifice it at end of combat", the second arming a CR 603.7 delayed
    -- triggered ability.
    Decayed
  | -- | 702.148a: cleave [cost] -- an alternative cost, plus a CR 612.1
    -- text-changing effect that removes the card's bracketed words if it was
    -- paid.
    --
    -- Not implemented: the text change as a text change (#3686). A card carries
    -- its two readings instead as two clauses of one mode, each gated on
    -- Quantity.CastUsing -- Morsel Theft's prowl shape -- which states a bracket
    -- that is a whole clause or a whole ObjectRef.EachMatching filter (Path of
    -- Peril) and cannot state one inside a TARGET (Wash Away). CR 601.2c lets a
    -- spell's targets depend on which alternative cost was chosen, and the only
    -- shape pawl states is rule 702.96b's, the whole slot dropped (Overload
    -- above); a bracket that narrows a target's FILTER has none.
    Cleave (Cost.Cost Keyword)
  | -- | 702.149a: whenever this creature and at least one other creature with
    -- greater power attack, put a +1/+1 counter on this creature. The counter
    -- goes on through Effect.Train, so CR 702.149c's "whenever this creature
    -- trains" can tell it from any other.
    Training
  | -- | 702.150a: a planeswalker entering with loyalty counters enters with two
    -- fewer for each Phyrexian mana symbol its caster paid life for. A minted
    -- EntryRewrite row, so it can be ordered against CR 614.16's multipliers
    -- under CR 616.1e.
    Compleated
  | -- | 702.151a: reconfigure [cost] -- an activated ability attaching this
    -- Equipment to another target creature you control, plus CR 702.151b's
    -- layer-4 suppression of its own creature type while it is attached.
    --
    -- Not implemented: rule 702.151a's SECOND ability, "[Cost]: Unattach this
    -- permanent", which has no effect opcode to resolve into (#3849).
    Reconfigure (Cost.Cost Keyword)
  | -- | 702.152a: blitz [cost] -- an alternative cost; the permanent has haste
    -- and a dies-draw, and is sacrificed at the next end step.
    Blitz (Cost.Cost Keyword)
  | -- | 702.153a: casualty N -- an optional additional cost of sacrificing a
    -- creature with power N or greater, and a cast trigger copying the spell
    -- once if it was paid, both minted by Pawl.Engine.Keyword.
    Casualty Natural.Natural
  | -- | 702.155a: chapter abilities of this Saga can't trigger the turn it
    -- entered unless it has exactly that chapter's number of lore counters; rule
    -- 714.3b REPLACES rule 714.3a's ability rather than adding to it.
    ReadAhead
  | -- | 702.156a: "This permanent enters with X +1\/+1 counters on it" and "When
    -- this permanent enters, if X is 5 or more, draw a card", both X being CR
    -- 107.3m's announced one.
    --
    -- Nullary: rule 702.156a fixes the counter kind, the threshold and the draw,
    -- and the number is the spell's announced X rather than anything a card
    -- prints.
    Ravenous
  | -- | 702.157a: "As an additional cost to cast this spell, you may pay [cost]
    -- any number of times", and an enters trigger making a token copy per payment.
    Squad (Cost.Cost Keyword)
  | -- | 702.160a: a second mana cost, power and toughness the caster may choose
    -- instead of the printed ones as the card is cast (CR 718.3).
    Prototype Prototype.Prototype
  | -- | 702.163a: "When this Equipment enters, create a 2\/2 red Rebel creature
    -- token, then attach this Equipment to it."
    ForMirrodin
  | -- | 702.164a: toxic N. CR 702.164b's total toxic value is the SUM over every
    -- toxic ability the creature has (Pawl.Engine.Projection.totalToxic).
    Toxic Natural.Natural
  | -- | 702.165a: backup N -- "when this creature enters, put N +1\/+1 counters
    -- on target creature. If that's another creature, it also gains the
    -- non-backup abilities of this creature printed below this one until end of
    -- turn." Minted by Pawl.Engine.Keyword.backup.
    --
    -- Not implemented: CR 702.165a's "printed below this one", which needs the
    -- printed ORDER of a face's keywords and abilities against one another --
    -- Pawl.Types.Face keeps the keywords in a Set beside the ability lists, so
    -- every non-backup printed ability is granted whether it was printed above
    -- or below (gap #3938). Cragsmasher Yeti is the card that needs it.
    Backup Natural.Natural
  | -- | 702.166a: bargain -- an optional additional cost of sacrificing an
    -- artifact, enchantment or token, minted by Pawl.Engine.Keyword.bargainCost.
    -- CR 702.166c's "if it was bargained" clauses are the card's own, gated on
    -- Quantity.TimesPaid (Archon's Glory).
    Bargain
  | -- | 702.167a: craft with [materials] [cost] -- an activated ability paying
    -- [cost], exiling this permanent and [materials] from among permanents you
    -- control and cards in your graveyard, and returning this card to the
    -- battlefield transformed under its owner's control at sorcery speed. Minted
    -- by Pawl.Engine.Keyword.craft.
    --
    -- Not implemented: CR 702.167c's "the exiled cards used to craft it", which
    -- needs the materials recorded for a later clause to read (#3931).
    Craft (Craft.Craft Keyword)
  | -- | 702.168a: disguise [cost] -- Morph's twin, casting the card as a 2\/2
    -- face-down creature with ward {2} for {3}; the Cost is what CR 702.168d
    -- charges to turn the permanent face up.
    --
    -- Not implemented: CR 702.168e's X in a disguise cost, which needs the value
    -- chosen as the special action was taken to reach the permanent's other
    -- abilities (#2056).
    Disguise (Cost.Cost Keyword)
  | -- | 702.170a: plot [cost] -- exile this card from hand for [cost] as CR
    -- 116.2k's special action, then cast it free on a later turn (CR 702.170d).
    --
    -- Not implemented: CR 702.170f's plot from a zone other than a hand (Fblthp,
    -- Lost on the Range), which Pawl.Engine.Plot.canPlot's Zone.Hand test refuses
    -- (#2091).
    Plot (Cost.Cost Keyword)
  | -- | 702.171a: "Tap any number of other untapped creatures you control with
    -- total power N or greater: This permanent becomes saddled until end of
    -- turn. Activate only as a sorcery." Crew's cost one rule over, with CR
    -- 702.171b's designation where crew's arm sets card types.
    --
    -- CR 702.171c's "saddles" relation is GameEvent.Saddled, written by
    -- Pawl.Engine.Activate as the cost is paid and read by
    -- Filter.SaddledSourceThisTurn; Pawl.SaddleSpec's "CR 702.171c the attacking
    -- Beaver's counter goes on the creature that saddled it" is the proof.
    Saddle Natural.Natural
  | -- | 702.172a: "Choose one or more modes. As an additional cost to cast this
    -- spell, pay the costs associated with those modes." Payload-free: the
    -- selection is the card's Pawl.Types.ModeSelection and the per-mode costs are
    -- CR 700.2h's Pawl.Types.Face.modeCosts.
    Spree
  | -- | 702.175a: "You may pay an additional [cost] as you cast this spell", and
    -- an enters trigger making a 1\/1 token copy if it was paid.
    Offspring (Cost.Cost Keyword)
  | -- | 702.174a: "Gift a [something]" -- an optional additional cost that
    -- chooses an opponent, plus a second ability giving that player the
    -- [something], which on a permanent is CR 702.174b's enters trigger.
    --
    -- THE [SOMETHING] ALONE is the payload: rule 702.174a fixes the first
    -- ability's words for every printing, so Pawl.Engine.Keyword.optionalCost
    -- mints the cost, bargain's shape, and rule 702.174b reads the effect off
    -- this word.
    --
    -- Rule 702.174b's OTHER half, on an instant or a sorcery, is a SPELL ability
    -- and is minted nowhere: Pawl.Engine.Resolve.giftOnSpellResolution performs
    -- it as the spell resolves.
    --
    -- Not implemented: CR 702.174c's "whenever a player gives a gift" (#3945).
    Gift Gift.Gift
  | -- | 702.173a: freerunning [cost] -- you may pay [cost] rather than this
    -- spell's mana cost if a player was dealt combat damage this turn by a
    -- creature that was then an Assassin or a commander under your control,
    -- which CR 601.2b and CR 601.2f-h price as an alternative cost.
    Freerunning (Cost.Cost Keyword)
  | -- | 702.176a: impending N--[cost], an alternative cost that holds off being a creature.
    Impending (Impending.Impending Keyword)
  | -- | 702.177a: exhaust adds rules to the activated ability printed AFTER it --
    -- "Exhaust -- [Cost]: [Effect]" means "[Cost]: [Effect]. Activate only once".
    -- PRINTED rather than minted, so a card writes this on the ability itself
    -- through Pawl.Types.ActivatedAbility.keyword, which is what lets Boom
    -- Scholar's "exhaust abilities of other permanents you control" name it.
    --
    -- Not implemented: the rewriting itself, so a card carrying this keyword
    -- writes CR 702.177a's "activate only once" as its own
    -- ActivationRestriction.OnlyOnce rather than having the keyword add it
    -- (#3044). Not implemented either: CR 702.177b's "as long as you haven't
    -- activated an exhaust ability this turn" (#3044).
    Exhaust
  | -- | 702.142a: boast adds rules to the activated ability printed AFTER it --
    -- "Boast -- [Cost]: [Effect]" means "[Cost]: [Effect]. Activate only if this
    -- creature attacked this turn and only once each turn". Exhaust's shape one
    -- rule over: PRINTED rather than minted, so the card writes this on the
    -- ability through Pawl.Types.ActivatedAbility.keyword, which is what CR
    -- 702.142b's "a creature boasting" would name.
    --
    -- Not implemented: the rewriting itself, so Varragoth, Bloodsky Sire writes
    -- rule 702.142a's two riders as its own ActivationRestriction.OnlyIf and
    -- ActivationRestriction.OnlyOnceEachTurn rather than having the keyword add
    -- them (#3044).
    Boast
  | -- | 702.57a: a forecast ability is the activated ability printed after it,
    -- activatable only from a hand, during its owner's upkeep, once each turn
    -- (CR 702.57b); written on Pawl.Types.ActivatedAbility.keyword.
    Forecast
  | -- | 702.179a: a static ability whose whole content is CR 704.5aa's
    -- state-based action, read off the projection by Pawl.Engine.Sba rather than
    -- minted.
    StartYourEngines
  | -- | 702.182a: "When this Equipment enters, create a 1\/1 colorless Hero
    -- creature token, then attach this Equipment to it."
    JobSelect
  | -- | 702.183a: "Choose one. As an additional cost to cast this spell, pay the
    -- cost associated with that mode." Spree's twin at one mode, and payload-free
    -- for its reason.
    Tiered
  | -- | 701.43d: "you may exert this creature as it attacks" is an optional cost
    -- to attack (CR 508.1g), read by Pawl.Engine.Combat.declareAttackers.
    Exert
  | -- | 702.154a: "as this creature attacks, you may tap up to one untapped
    -- creature you control that you didn't choose to attack with and that either
    -- has haste or has been under your control continuously since this turn
    -- began. When you do, this creature gets +X\/+0 until end of turn, where X is
    -- the tapped creature's power." Rule 508.1g's other optional cost to attack,
    -- offered by Pawl.Engine.Combat.declareAttackers beside Exert's; rule
    -- 702.154b's linked triggered ability is
    -- Pawl.Engine.Keyword.enlistReflexive, armed as CR 603.12's reflexive entry
    -- only where the tap happened.
    Enlist
  | -- | 702.103a: "As you cast this spell, you may choose to cast it bestowed. If
    -- you do, you pay [cost] rather than its mana cost." CR 702.103b's rewrite
    -- into an Aura with enchant creature is minted from this constructor rather
    -- than printed.
    Bestow (Cost.Cost Keyword)
  | -- | 702.184a: tap another untapped creature you control to load this
    -- permanent with that creature's power in charge counters, as a sorcery.
    -- CR 702.184c's substitution of another characteristic is
    -- Quantity.StationMeasure's own arm, not a payload here.
    Station
  | -- | 702.140a: "mutate [cost]" -- "you may pay [cost] rather than pay this
    -- spell's mana cost. If you do, it becomes a mutating creature spell and
    -- targets a non-Human creature with the same owner as this spell". The
    -- target slot and CR 730's merge are minted from this constructor rather
    -- than printed.
    Mutate (Cost.Cost Keyword)
  | -- | 702.89a: "If enchanted permanent would be destroyed, instead remove all
    -- damage marked on it and destroy this Aura." A CR 614.1 destruction
    -- replacement, minted from this constructor onto the Aura rather than
    -- printed; CR 702.89b's "totem armor" is the same ability's old name.
    UmbraArmor
  | -- | 702.81a: cast this card from your graveyard by discarding a land card as
    -- an ADDITIONAL cost, jump-start's shape with a quality on the discard and
    -- no exile after.
    Retrace
  | -- | 702.187b: "mayhem [cost]" -- cast this card from your graveyard for
    -- [cost] rather than its mana cost, as long as you discarded it this turn.
    --
    -- Not implemented: CR 702.187c's mayhem with NO cost, which permits PLAYING
    -- the card rather than casting it and so needs a land-play permission
    -- (#3645).
    Mayhem (Cost.Cost Keyword)
  | -- | 702.35a: "madness [cost]" -- two abilities. The static one replaces a
    -- discard of this card with an exile
    -- (Pawl.Engine.Keyword.handReplacementsOf); the triggered one then offers
    -- its owner the cast for [cost], or the graveyard
    -- (Pawl.Engine.Keyword.exileTriggeredAbilitiesOf).
    Madness (Cost.Cost Keyword)
  | -- | 702.88a: a spell cast from its controller's hand is exiled as it
    -- resolves instead of going to the graveyard, and a delayed triggered
    -- ability offers its controller the cast from exile for nothing at the
    -- beginning of their next upkeep. Both halves are minted from this
    -- constructor (Pawl.Engine.Resolve.finishSpell,
    -- Pawl.Engine.Keyword.reboundUpkeep) rather than printed.
    Rebound
  | -- | 702.97a: a graveyard ability exiling this card to put +1\/+1 counters
    -- equal to its power on target creature, at sorcery speed.
    Scavenge (Cost.Cost Keyword)
  | -- | 702.141a: scavenge's neighbour, exiling this card for one hasty token
    -- copy per opponent, each required to attack that opponent and sacrificed
    -- at the beginning of the next end step.
    Encore (Cost.Cost Keyword)
  | -- | 702.194a: teamwork N -- an optional additional cost of tapping any number
    -- of untapped creatures you control with total power N or more, minted by
    -- Pawl.Engine.Keyword.teamworkCost. CR 702.194b's "cast using teamwork"
    -- clauses are the card's own, gated on Quantity.TimesPaid (Team Tactics).
    Teamwork Natural.Natural
  | -- | 702.188a: web-slinging [cost] -- an alternative cost of [cost] plus
    -- returning a tapped creature you control to its owner's hand, minted by
    -- Pawl.Engine.Keyword.plainAlternativeCosts.
    WebSlinging (Cost.Cost Keyword)
  | -- | 702.190a: sneak [cost] -- web-slinging's alternative cost two rules over,
    -- returning an unblocked creature instead of a tapped one and carrying a
    -- casting window of its own (Pawl.Engine.Cast.windowedCandidates).
    --
    -- CR 702.190b's rider -- the permanent enters tapped and attacking whatever
    -- the returned creature was attacking -- is minted by
    -- Pawl.Engine.Keyword.castUsingEntry and applied by Pawl.Engine.Stack's
    -- permanent entry; Pawl.CastSpec's Sneak group proves it.
    Sneak (Cost.Cost Keyword)
  | -- | 702.191a: whenever you cast a spell, if this permanent is a creature and
    -- the amount of mana spent to cast that spell is greater than this creature's
    -- power or its toughness, put a +1\/+1 counter on it. Each instance triggers
    -- separately (CR 702.191b).
    Increment
  | -- | 702.195a: Ascend's shape at a different count and a different mark -- a
    -- static ability whose whole content is "any time you control three or more
    -- permanents that are artifacts, Sagas, and\/or legendary and you don't have
    -- an enduring story, you have an enduring story for the rest of the game",
    -- read off the projection by Pawl.Engine.PlayerDesignation rather than
    -- minted.
    Storied
  | -- | 702.181a: mobilize N -- whenever this creature attacks, create N tapped
    -- and attacking 1\/1 red Warrior tokens, sacrificed at the beginning of the
    -- next end step.
    Mobilize (KeywordCount.KeywordCount Keyword)
  | -- | 702.189a: firebending N -- whenever this creature attacks, add N {R}
    -- that its controller does not lose as steps and phases end until end of
    -- combat.
    Firebending (KeywordCount.KeywordCount Keyword)
  | -- | 702.53a: transmute [cost] -- an ability functioning only in a hand,
    -- discarding this card to search your library for a card with the same mana
    -- value, at sorcery speed.
    Transmute (Cost.Cost Keyword)
  | -- | 702.71a: transfigure [cost] -- transmute's battlefield twin, sacrificing
    -- this permanent to put a CREATURE card with the same mana value onto the
    -- battlefield.
    Transfigure (Cost.Cost Keyword)
  | -- | 702.185a: warp [cost] -- an alternative cost paid from the HAND alone;
    -- the permanent it becomes is exiled at the next end step and its owner may
    -- cast it from exile on a later turn (CR 702.185b's warped card).
    Warp (Cost.Cost Keyword)
  | -- | 702.146a: disturb [cost] -- cast this card TRANSFORMED from your
    -- graveyard for [cost] rather than its mana cost (CR 712.8c). The ability is
    -- the FRONT face's and the spell is the BACK face, which is why
    -- Pawl.Engine.Card.convertedFace and
    -- Pawl.Engine.Cast.permitsCastFromGraveyard both read it off the front face
    -- of the card being cast (CR 712.11d).
    Disturb (Cost.Cost Keyword)
  | -- | 702.180a: harmonize [cost] -- three static abilities, all minted from
    -- this constructor: cast this card from your graveyard for [cost] plus
    -- tapping up to one untapped creature you control, reduce the total cost by
    -- that creature's power, and exile the card instead of putting it anywhere
    -- else it would go from the stack.
    Harmonize (Cost.Cost Keyword)
  deriving (Eq, Ord, Show)

-- Devoid takes TWO routes, decided by where the instance came from. A PRINTED one
-- is a characteristic-defining ability and is folded at the start of layer 5
-- (Projection.applyColorDefining), per CR 613.3. A GRANTED one is not, CR 604.3a's
-- second criterion reaching only what is printed on the card, granted to a token
-- by the effect that made it, or acquired through a copy or text-changing effect, so
-- Projection.grantedDefiningParts routes it into layer 5 as an ordinary
-- timestamped colour effect -- stamped with the granting permanent when a static
-- ability grants it (CR 613.7a), and at creation when a resolution does (CR
-- 613.7b). Slivdrazi Monstrosity and Synthetic Colorless Blessing are the two
-- grants, and Pawl.ColorSpec's "CR 613.7a a granted devoid clears an OLDER 'in
-- addition' colour" and "CR 702.114a devoid granted by a RESOLUTION makes the
-- creature colourless" are the proofs.
--
-- CR 702.73a's changeling takes the same routes one layer down, through the
-- same pair of functions -- plus a third, CR 604.3a's copy-effect clause, which
-- needs no function of its own: Replacement.applyCopyException writes CR 707.9a's
-- gained keyword into the copiable snapshot, which is where the printed route
-- reads from (Omni-Changeling).
