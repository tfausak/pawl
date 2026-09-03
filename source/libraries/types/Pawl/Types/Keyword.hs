module Pawl.Types.Keyword where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.Reinforce as Reinforce

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
-- count. This type says only WHICH ability, so a card's printed keywords stay a
-- Set -- see Pawl.Types.Face.keywords.
--
-- This module TIES THE KNOT that Pawl.Types.Filter's keyword parameter opens:
-- Filter has a HasKeyword arm and this type carries a Filter (702.11d, 702.14c,
-- 702.29e) and a Cost (702.29a/702.33a/702.34a/702.42a) whose components carry one too, so the
-- three would be a module cycle if any were concrete. They are parametric and
-- this one is not, which makes `Filter Keyword` and `Cost Keyword` the only
-- instantiations anywhere.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | -- | 702.6a: "Equip [cost]" means "[Cost]: Attach this permanent to target
    -- creature you control. Activate only as a sorcery." One ability per
    -- instance (CR 702.6d).
    --
    -- Not implemented: CR 702.6c's "equip [quality] creature", which narrows the
    -- TARGET only, and CR 702.6e's "equip planeswalker" (#2291).
    Equip (Cost.Cost Keyword)
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
    -- Not implemented: rule 702.16k's "protection from [a player]" -- whose
    -- quality is a PLAYER and not a characteristic, so no Filter says it -- and
    -- the "from each" shorthands of rules 702.16h and 702.16i (#2229).
    Protection (Filter.Filter Keyword)
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
    -- or ability unless its controller pays.
    --
    -- Not implemented: CR 702.21b's X in a ward cost, which needs a value
    -- determined as the ability RESOLVES (#1526).
    Ward (Cost.Cost Keyword)
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
  | -- | 702.28b: a creature with shadow can't be blocked by creatures without
    -- shadow, and one without shadow can't be blocked by creatures with it.
    Shadow
  | -- | 702.29a: "[Cost], Discard this card: Draw a card", functioning only while
    -- the card is in a player's hand; Just is CR 702.29e's typecycling search,
    -- riding this constructor so CR 702.29f holds for free.
    Cycling (Cycling.Cycling Keyword)
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
  | -- | 702.39a: whenever this creature attacks, you may have target creature
    -- defending player controls block it if able; if you do, untap that creature.
    Provoke
  | -- | 702.42a: you may choose all modes of this modal spell (CR 700.2) instead
    -- of the number specified, paying an additional cost if you do.
    Entwine (Cost.Cost Keyword)
  | -- | 702.43a: modular N -- enter with N +1/+1 counters, and on death move the
    -- counters actually present to target artifact creature; each instance works
    -- separately (CR 702.43b).
    --
    -- A Natural and not a Quantity: CR 702.44c's "Modular--Sunburst" (Arcbound
    -- Wanderer) is the one printing whose N is another keyword's count, and
    -- sunburst itself has no representation yet (#877).
    Modular Natural.Natural
  | -- | 702.45a: whenever this creature blocks or becomes blocked, it gets +N/+N
    -- until end of turn (CR 509.3a, CR 509.3c); each instance triggers separately
    -- (CR 702.45b).
    Bushido Natural.Natural
  | -- | 702.46a: soulshift N -- when this permanent dies, you may return target
    -- Spirit card with mana value N or less from your graveyard to your hand;
    -- each instance triggers separately (CR 702.46b).
    Soulshift Natural.Natural
  | -- | 702.54a: bloodthirst N -- if an opponent was dealt damage this turn, this
    -- permanent enters with N +1/+1 counters on it; a CR 614.1c entry replacement
    -- carrying its own condition, each instance applying separately (CR 702.54c).
    --
    -- Nothing is rule 702.54b's "bloodthirst X", Vanishing's spelling of the same
    -- want: X is the total damage this permanent's controller's opponents were
    -- dealt this turn, so it names no printed number and states no condition.
    Bloodthirst (Maybe Natural.Natural)
  | -- | 702.55a: when this permanent dies, exile it haunting target creature; CR
    -- 702.55b's haunted object is board state rather than a characteristic.
    --
    -- Not implemented: rule 702.55a's other sentence, haunt on an instant or
    -- sorcery -- the mint is handed a keyword and a count, never a card type, so
    -- the two sentences cannot be told apart there (#1404).
    Haunt
  | -- | 702.61a: "As long as this spell is on the stack, players can't cast
    -- spells or activate abilities that aren't mana abilities." A
    -- rules-modifying continuous effect (CR 611.1) other players' gates ask
    -- about, so nothing is minted.
    SplitSecond
  | -- | 702.63a: vanishing N -- enter with N time counters, remove one at each
    -- upkeep, and sacrifice the permanent when the last one goes. Nothing is CR
    -- 702.63b's numberless printing, which states only the last two abilities.
    Vanishing (Maybe Natural.Natural)
  | -- | 702.67a: "[Cost]: Attach this Fortification to target land you control.
    -- Activate only as a sorcery." One ability per instance (CR 702.67c).
    Fortify (Cost.Cost Keyword)
  | -- | 702.68a: frenzy N -- whenever this creature attacks and isn't blocked, it
    -- gets +N/+0 until end of turn; each instance triggers separately (CR
    -- 702.68b).
    Frenzy Natural.Natural
  | -- | 702.70a: whenever this creature deals combat damage to a player, that
    -- player gets N poison counters; the Ns are not summed, each instance
    -- triggering separately (CR 702.70b).
    Poisonous Natural.Natural
  | -- | 702.73a: "This object is every creature type." A characteristic-defining
    -- ability (CR 604.3) landing in layer 4.
    Changeling
  | -- | 702.77a: reinforce N-[cost] -- "[Cost], Discard this card: Put N +1/+1
    -- counters on target creature", functioning only while the card is in a
    -- player's hand.
    --
    -- The hand-only half is not a field here: rule 702.77b keeps the ability in
    -- existence in every other zone. Pawl.UntapRestrictionSpec's "CR
    -- 502.3/702.77b whole cards: under Tsabo's Web the Rustic Clachan does not
    -- untap" is what proves that half.
    Reinforce (Reinforce.Reinforce Keyword)
  | -- | 702.79a: when this permanent dies, if it had no -1/-1 counters on it,
    -- return it to the battlefield under its owner's control with one.
    Persist
  | -- | 702.80a: damage this source deals to a creature isn't marked on it;
    -- instead its controller puts that many -1/-1 counters on that creature (CR
    -- 120.3d).
    Wither
  | -- | 702.83a: whenever a creature you control attacks alone, that creature
    -- gets +1/+1 until end of turn. Two instances are two abilities.
    Exalted
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
  | -- | 702.98a: "You may have this permanent enter with an additional +1/+1
    -- counter on it" and "This permanent can't block as long as it has a +1/+1
    -- counter on it". The second half is not conditional on the first.
    Unleash
  | -- | 702.100a: whenever a creature you control enters, if that creature's
    -- power and/or toughness is greater than this creature's, put a +1/+1 counter
    -- on this creature; each instance triggers separately (CR 702.100d).
    Evolve
  | -- | 702.102a: a player casting this split card from their hand may cast both
    -- halves as one fused split spell (CR 702.102b-d).
    Fuse
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
  | -- | 702.111b: a creature with menace can't be blocked except by two or more
    -- creatures.
    Menace
  | -- | 702.112a: renown N -- when this creature deals combat damage to a player,
    -- if it isn't renowned, put N +1/+1 counters on it and it becomes renowned;
    -- renowned itself is a designation (CR 702.112b).
    Renown Natural.Natural
  | Devoid -- 702.114
  | -- | 702.115a: whenever this creature deals combat damage to a player, that
    -- player exiles the top card of their library, face up (CR 406.3); each
    -- instance triggers separately (CR 702.115b).
    Ingest
  | -- | 702.118b: a creature with skulk can't be blocked by creatures with
    -- greater power.
    Skulk
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
  | -- | 702.127a: three static abilities in one word -- cast this half from your
    -- graveyard, never from anywhere else, and exile it as it leaves the stack.
    Aftermath
  | -- | 702.130a: whenever this creature becomes blocked, defending player loses
    -- N life; each instance triggers separately (CR 702.130b).
    Afflict Natural.Natural
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
  | -- | 702.143a: foretell [cost] -- exile this card from hand face down for {2}
    -- (CR 116.2h's special action), then cast it later for [cost]. The Cost is
    -- the CAST's.
    --
    -- Not implemented: CR 702.143d's other producer -- an effect that makes an
    -- exiled card foretold without this keyword, and may give it a foretell cost
    -- -- nor CR 702.143c's "a card or spell that was foretold" as something an
    -- effect can refer to (#1486).
    Foretell (Cost.Cost Keyword)
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
  | -- | 702.155a: chapter abilities of this Saga can't trigger the turn it
    -- entered unless it has exactly that chapter's number of lore counters; rule
    -- 714.3b REPLACES rule 714.3a's ability rather than adding to it.
    ReadAhead
  | -- | 702.164a: toxic N. CR 702.164b's total toxic value is the SUM over every
    -- toxic ability the creature has (Pawl.Engine.Projection.totalToxic).
    Toxic Natural.Natural
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
  | -- | 702.179a: a static ability whose whole content is CR 704.5aa's
    -- state-based action, read off the projection by Pawl.Engine.Sba rather than
    -- minted.
    StartYourEngines
  | -- | 701.43d: "you may exert this creature as it attacks" is an optional cost
    -- to attack (CR 508.1g), read by Pawl.Engine.Combat.declareAttackers.
    --
    -- Not implemented: CR 702.154's enlist, rule 508.1g's other optional cost to
    -- attack, whose cost is tapping a filtered creature rather than a yes-or-no
    -- (#877).
    Exert
  | -- | 702.103a: "As you cast this spell, you may choose to cast it bestowed. If
    -- you do, you pay [cost] rather than its mana cost." CR 702.103b's rewrite
    -- into an Aura with enchant creature is minted from this constructor rather
    -- than printed.
    Bestow (Cost.Cost Keyword)
  | -- | 702.184a: tap another untapped creature you control to load this
    -- permanent with that creature's power in charge counters, as a sorcery.
    --
    -- Not implemented: CR 702.184c's static abilities that make a station
    -- ability read a characteristic other than the tapped creature's power
    -- (Tapestry Warden) (#3127).
    Station
  deriving (Eq, Ord, Show)

-- Devoid takes TWO routes, decided by where the instance came from. A PRINTED one
-- is a characteristic-defining ability and is folded at the start of layer 5
-- (Projection.applyColorDefining), per CR 613.3. A GRANTED one is not, CR 604.3a
-- denying CDA status to an ability that is not printed on the card it affects, so
-- Projection.grantedDefiningParts routes it into layer 5 as an ordinary
-- timestamped colour effect -- stamped with the granting permanent when a static
-- ability grants it (CR 613.7a), and at creation when a resolution does (CR
-- 613.7b). Slivdrazi Monstrosity and Synthetic Colorless Blessing are the two
-- grants, and Pawl.ColorSpec's "CR 613.7a a granted devoid clears an OLDER 'in
-- addition' colour" and "CR 702.114a devoid granted by a RESOLUTION makes the
-- creature colourless" are the proofs.
--
-- CR 702.73a's changeling takes the same routes one layer down, through the
-- same pair of functions.
