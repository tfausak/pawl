module Pawl.Engine.Keyword where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import Pawl.Types.ActivatedAbility (ActivatedAbility)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.CastingPermission (CastingPermission)
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import Pawl.Types.Filter (Filter)
import qualified Pawl.Types.Filter as Filter
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

-- Rule 702 in its OTHER voice. Most keywords this pool has are read where they
-- matter -- Projection.hasKeyword for an evasion or combat bit, Pawl.Engine.Damage for
-- infect's and toxic's damage riders -- because the rule states them as static
-- abilities that some rules-core reader already asks about. Rule 702.70 does
-- not: it spells poisonous out as a TRIGGERED ability, in the same words a card
-- would print, so it has to be MINTED and handed to the ordinary CR 603
-- machinery rather than merely consulted.
--
-- Casing on Keyword here is legitimate for the reason Pawl.Types.Keyword's own
-- comment gives: a keyword is a numbered rule, not an effect's identity. What
-- this module must never do is grow an arm for a CARD.
--
-- The abilities are derived from a projection's POST-LAYER keyword counts, so
-- Humility (LoseAllAbilities, which empties PC.keywords at layer 6) takes rule
-- 702.70a's ability away for free, and an Aura's layer-6 grant adds it for free
-- -- neither needs an arm here.
--
-- triggeredAbilitiesOf's one caller is Pawl.Engine.Event's EVENT scan (eventTriggers).
-- Rule 702 has no state-triggered (CR 603.8) or delayed (CR 603.7) keyword
-- ability, so stateTriggers and delayedPending do not consult this; the first
-- keyword that needs them to is the one that must widen those two scans.
--
-- Rule 702.34a's flashback is the second minting customer, and the one that
-- shows how wide this voice is: ONE keyword becomes a cost
-- (flashbackCost, read by Pawl.Engine.Cost), a casting permission
-- (castingPermissionsOf, read by Pawl.Engine.Cast) and a replacement effect
-- (flashbackExile, installed by Pawl.Engine.Cast). Those three readers get ordinary
-- rules objects and never learn that flashback is what produced them.
--
-- These three are derived from a card's PRINTED keywords rather than a
-- projection's post-layer ones, unlike triggeredAbilitiesOf: all three abilities
-- function in the graveyard or on the stack (CR 113.6), neither of which pawl's
-- projection reaches (#160). entwineCost is read the same way and for the same
-- reason: rule 702.42a says entwine "functions while the spell is on the stack",
-- and Pawl.Engine.Cast reads it one step earlier still, off a card in a hand.

-- CR 702.70b: "If a creature has multiple instances of poisonous, each triggers
-- separately." So this returns one ability PER INSTANCE, which is exactly what
-- the projection's per-keyword count says: `Poisonous 1` twice is two abilities
-- and two poison counters, not one ability for 2. (Contrast CR 702.164b, where
-- toxic's N values are SUMMED into a single rider -- Projection.totalToxic.)
-- CR 702.91b says the same of battle cry, in the same words, so the two minting
-- arms below are the same shape.
--
-- Order is the Map's, which is Keyword's Ord -- rule-number order, and stable.
-- The CR 603.3b ordering prompt indexes into the scan's canonical order, so this
-- being deterministic is what keeps that prompt reproducible.
triggeredAbilitiesOf :: Map Keyword Natural -> [TriggeredAbility Card]
triggeredAbilitiesOf counts = concatMap (uncurry abilitiesFor) (Map.toAscList counts)

-- The abilities one keyword, held `count` times, contributes.
abilitiesFor :: Keyword -> Natural -> [TriggeredAbility Card]
abilitiesFor keyword count = case keyword of
  Keyword.Poisonous n -> List.genericReplicate count (poisonous n)
  Keyword.BattleCry -> List.genericReplicate count battleCry
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Menace -> []
  Keyword.Cycling _ _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 602.1: the ACTIVATED abilities rule 702 gives a card while it sits in its
-- owner's hand -- the third sibling of triggeredAbilitiesOf above and
-- castingPermissionsOf below, and the first that mints something a player takes
-- an action with.
--
-- Named for the ZONE rather than for cycling, because that is the classification
-- its one reader wants: Pawl.Engine.Activate.abilitiesFor asks "what can be activated
-- from here", and gets an ordinary list of abilities back without learning that
-- rule 702.29 produced any of them. Rule 702 has more hand abilities to come
-- (forecast, CR 702.57, is the next), and each joins this list without its
-- reader changing.
--
-- Printed keywords rather than a projection's post-layer ones, the same rules
-- fact castingPermissionsOf records: CR 113.6b says "an ability that states
-- which zones it functions in functions only from those zones", and rule 702.29a
-- states the hand -- which pawl's projection does not reach (#160).
handAbilitiesOf :: Set Keyword -> [ActivatedAbility Card]
handAbilitiesOf = concatMap handAbilitiesFor . Set.toAscList

-- Exhaustive for the reason permissionsFor is: rule 702 keeps adding abilities
-- that function from a hand, so the next one must break this build rather than
-- silently produce nothing.
handAbilitiesFor :: Keyword -> [ActivatedAbility Card]
handAbilitiesFor keyword = case keyword of
  Keyword.Cycling cost searchFor -> [cycling cost searchFor]
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Menace -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Poisonous _ -> []
  Keyword.BattleCry -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 702.29a: "'Cycling [cost]' means '[Cost], Discard this card: Draw a card.'"
-- The whole ability, minted from the one cost the keyword carries.
--
-- The discard is a COMPONENT of the activation cost and not an effect, because
-- rule 702.29a puts it before the colon. Three things follow that would all be
-- wrong the other way round: an activation the player backs out of discards
-- nothing (Pawl.Engine.Cost.pay restores the entry state), the card is already in the
-- graveyard while the draw is still on the stack, and CR 702.29c's "when you
-- cycle this card" has a cost payment to trigger off rather than a resolution
-- (Pawl.Engine.Cost records the event; Pawl.Engine.Event matches it).
--
-- The card's own data carries only what is PRINTED on it -- "Cycling {2}" is a
-- mana cost and nothing else -- and rule 702.29a's discard is added here. That
-- is the split the whole module exists for: the card says which keyword, the
-- rule says what it means.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability
-- is activated: the Pawl.Engine.Monarch.oneEffect shape poisonous above also takes.
--
-- AnyTime because rule 702.29a states no timing restriction, which leaves CR
-- 117.1b's default: "a player may activate an activated ability any time they
-- have priority."
cycling :: Cost Keyword -> Maybe (Filter Keyword) -> ActivatedAbility Card
cycling cost searchFor =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.DiscardThis]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.timing = ActivationTiming.AnyTime
    }
  where
    -- The only difference between rule 702.29a and rule 702.29e: what the
    -- ability does once its cost is paid. Everything above -- the discard in the
    -- cost, the timing, the single forced mode -- is shared, which is CR 702.29f
    -- ("typecycling abilities are cycling abilities") holding by construction.
    --
    -- CR 702.29a draws for the ability's controller, which CR 113.8 makes "the
    -- player who activated it" -- so You, the perspective Pawl.Engine.Resolve evaluates
    -- a PlayerRef against.
    --
    -- CR 702.29e searches instead: "Search your library for a [type] card,
    -- reveal it, and put it into your hand." The reveal is part of the
    -- destination because it is part of that sentence -- see
    -- Pawl.Types.SearchDestination, and CR 701.23e for why a search does not
    -- reveal on its own.
    effect = case searchFor of
      Nothing -> Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)
      Just filter_ -> Effect.Search filter_ SearchDestination.RevealThenHand

-- CR 601.3: the casting permissions rule 702 gives a card for holding a keyword,
-- the sibling of triggeredAbilitiesOf above. A card's own printed permissions
-- (Card.castingPermissions) are a separate, additive list; Pawl.Engine.Cast reads both.
--
-- Taken over the card's PRINTED keywords rather than a projection's post-layer
-- ones, and that is the same rules fact Card.castingPermissions' own comment
-- records: this permission functions in the GRAVEYARD (CR 113.6), which pawl's
-- projection does not reach (#160).
castingPermissionsOf :: Set Keyword -> [CastingPermission]
castingPermissionsOf = concatMap permissionsFor . Set.toAscList

-- Exhaustive, exactly as abilitiesFor is, and for the same reason: rule 702 is
-- full of keywords that grant a zone permission (madness, retrace, escape,
-- disturb), so the next one added must break this build rather than silently
-- grant nothing.
permissionsFor :: Keyword -> [CastingPermission]
permissionsFor keyword = case keyword of
  -- CR 702.34a: "You may cast this card from your graveyard ..." Rule 702.34a's
  -- "if the resulting spell is an instant or sorcery spell" is not checked
  -- (#295).
  Keyword.Flashback _ -> [CastingPermission.CastFromGraveyard]
  -- CR 702.29a is an ACTIVATED ability, not a casting permission: cycling
  -- discards the card, it never casts it. See handAbilitiesOf above.
  Keyword.Cycling _ _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  -- CR 702.8a grants no permission either, and it is the near miss flashback's
  -- neighbour makes worth stating: rule 702.8a's SECOND sentence widens the TIME
  -- a cast may be proposed at (Pawl.Engine.Cast.instantSpeed) and names no zone,
  -- while its first names the zones the ABILITY functions in rather than the
  -- zones the card may be cast from. So a card with flash is castable from
  -- exactly the zones it was castable from without it.
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Menace -> []
  -- CR 702.42a grants no permission: entwine widens a MODE choice and adds a
  -- cost to a cast that some other rule already allowed; it never allows one.
  Keyword.Entwine _ -> []
  Keyword.Poisonous _ -> []
  Keyword.BattleCry -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 702.8a: "'Flash' means 'You may play this card any time you could cast an
-- instant.'" Does this card's keyword set say so? Its one reader is
-- Pawl.Engine.Cast.instantSpeed, which turns it into the CR 302.1 / 307.1 window
-- being lifted for that one card.
--
-- MEMBERSHIP, not a count: CR 702.8b says "Multiple instances of flash on the
-- same object are redundant," so a second one has nothing left to widen.
--
-- Two separate facts make reading a Set of PRINTED keywords right here, and
-- neither of them is the other.
--
-- WHERE the ability functions is the rules half, and rule 702.8a states it in
-- its first sentence -- flash "functions in any zone from which you could play
-- the card it's on" -- which CR 113.6e states generally: "An object's ability
-- that restricts or modifies how that particular object can be played or cast
-- functions in any zone from which it could be played or cast and also on the
-- stack." So a hand and a graveyard are zones this must be readable in at all,
-- which is why the caller asks a card rather than a permanent.
--
-- WHETHER printed is the right source is the engine half, and the rules do NOT
-- say it is: CR 613.1 names no zone, and CR 122.1b's keyword counter reaches "a
-- card in a zone other than the battlefield" explicitly. What makes the printed
-- read safe is that it is INDISTINGUISHABLE from a projected one today, which is
-- a claim about what pawl CANNOT EXPRESS rather than about Magic. Nothing can
-- put a keyword-changing effect on a card in a hand, and that takes all four of
-- Pawl.Types.Affected:
--
--   * Matching and AttachedPlayerControls are gated on battlefield membership,
--     structurally, inside Projection.affects.
--   * Attached names the object the SOURCE is attached to, which an Aura only
--     ever has while both are on the battlefield.
--   * TheseObjects is the one that could in principle reach elsewhere -- it is
--     CR 611.2c's frozen set, and Magical Hack's ChangeText already stores one
--     naming a spell on the STACK. What stops it here is the pool: no
--     Pawl.Types.Pool arm names a card in a hand at all (the nearest are
--     CardsInGraveyard and CardsInExile), and every Modification.GainKeyword
--     producer in the pool is aimed at creatures on the battlefield.
--
-- So a card in a hand projects exactly its printed keywords, and nothing can
-- grant or remove flash there (#160). The first effect that changes a
-- non-battlefield card's characteristics is what parts the two, and this becomes
-- a projected read then.
--
-- The same posture castingPermissionsOf and handAbilitiesOf above take, whose
-- comments state the rules half only (#567).
--
-- A membership test rather than an exhaustive case, the flashbackCost and
-- entwineCost precedent: this asks about ONE named constructor rather than
-- classifying every keyword, so a new arm has nothing to say here.
hasFlash :: Set Keyword -> Bool
hasFlash = Set.member Keyword.Flash

-- CR 702.34a's "... by paying [cost] rather than paying its mana cost": the cost
-- this card may be cast from the graveyard for, or Nothing when it has no
-- flashback. Read by Pawl.Engine.Cost.costsFor, which offers it ONLY while the object
-- is in a graveyard -- the zone half of the same sentence.
--
-- A wildcard rather than an exhaustive case, the Projection.totalToxic
-- precedent: this asks about ONE named constructor rather than classifying every
-- keyword, so a new arm has nothing to say here.
--
-- Nothing beyond the FIRST flashback cost is reachable. A card printing two
-- flashback abilities is expressible (a Set of two Flashback values with
-- different costs) and unrepresented in what this returns; no printing does it
-- (#294).
flashbackCost :: Set Keyword -> Maybe (Cost Keyword)
flashbackCost keywords =
  let costOf keyword = case keyword of
        Keyword.Flashback cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.42a's "[cost]": the ADDITIONAL cost this card's controller may pay to
-- choose all of its modes, or Nothing when it has no entwine. Read by Pawl.Engine.Cast,
-- which offers it at CR 601.2b and adds it to whichever candidate cost was
-- announced (CR 601.2f's "plus all additional costs").
--
-- A wildcard rather than an exhaustive case, exactly as flashbackCost above:
-- this asks about ONE named constructor rather than classifying every keyword.
--
-- Nothing beyond the FIRST entwine cost is reachable: a card printing two
-- entwine abilities is expressible and unrepresented (#474).
entwineCost :: Set Keyword -> Maybe (Cost Keyword)
entwineCost keywords =
  let costOf keyword = case keyword of
        Keyword.Entwine cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.34a's SECOND static ability, "another that functions while the card is
-- on the stack": "exile this card instead of putting it anywhere else any time
-- it would leave the stack." TheSource, because the rule says "this card" -- the
-- spell itself and no other object.
--
-- The destination is Graveyard rather than "anywhere else": a
-- Pawl.Types.ZoneChangePattern names ONE destination, and the graveyard is the
-- only place a spell leaves the stack for in this pool (resolution CR 608.2n,
-- the CR 608.2b fizzle, and CR 701.6a's counter all call Event.changeZone with
-- it) (#293).
--
-- Not gated on rule 702.34a's "if the flashback cost was paid": nothing here can
-- see which cost was paid (#101). Pawl.Engine.Cast installs this only for a spell cast
-- FROM THE GRAVEYARD, where the two coincide -- see its own comment.
--
-- The door Pawl.Engine.Cast uses, so that module installs a REPLACEMENT EFFECT it never
-- inspects rather than asking whether a card has flashback: the replacement
-- effects rule 702 gives a card that was cast from a graveyard, for the whole
-- time it is on the stack.
castFromGraveyardReplacementsOf :: Set Keyword -> [ReplacementEffect]
castFromGraveyardReplacementsOf keywords =
  [flashbackExile | Maybe.isJust (flashbackCost keywords)]

flashbackExile :: ReplacementEffect
flashbackExile =
  ReplacementEffect.ZoneChangeR
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = Zone.Graveyard,
        ZoneChangePattern.whoseObject = ControllerRelation.Anyones,
        ZoneChangePattern.whichObject = ZoneChangeSubject.TheSource
      }
    Zone.Exile

-- CR 702.70a: "'Poisonous N' means 'Whenever this creature deals combat damage
-- to a player, that player gets N poison counters.'"
--
-- "That player" is the player the trigger's own event named, which
-- Pawl.Engine.Event.eventBindings stamps under the reserved Binding.triggerPlayer slot
-- as the trigger is gathered -- so the payload is an ordinary slot read and this
-- ability needs no opcode of its own. NOT the ability's controller: CR 603.3a
-- makes that the creature's controller, and the poison goes to their victim.
--
-- Single mode, no targets, ChooseExactly 1 -- so nothing is asked as the ability
-- is placed, which is right because rule 702.70a leaves nothing to choose. Same
-- shape Pawl.Engine.Monarch.oneEffect builds for rule 725's inherent abilities.
-- intervening = Nothing: rule 702.70a has no "if" clause.
poisonous :: Natural -> TriggeredAbility Card
poisonous n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }
  where
    effect =
      Effect.GainPlayerCounters
        (PlayerRef.InSlot Binding.triggerPlayer)
        PlayerCounterKind.Poison
        (Quantity.Literal (toInteger n))

-- CR 702.91a: "Battle cry is a triggered ability. 'Battle cry' means 'Whenever
-- this creature attacks, each other attacking creature gets +1/+0 until end of
-- turn.'" Rule 702.70a's poisonous above is the only other keyword in this pool
-- stated as a triggered ability, and this is built the same way: minted here and
-- handed to the ordinary CR 603 machinery, which never learns a keyword produced
-- it.
--
-- CR 508.3a is what "attacks" means -- "an ability that reads 'Whenever [a
-- creature] attacks, . . .' triggers if that creature is declared as an
-- attacker" -- so the condition is the self-scoped SelfAttacks, EveryTime: rule
-- 702.91a states no "for the first time each turn" narrowing.
--
-- "EACH OTHER ATTACKING CREATURE" is a SET, swept at resolution and then frozen
-- (CR 611.2c), which is exactly Trumpet Blast's ObjectRef.EachMatching -- so this
-- needs no opcode of its own either. The three conjuncts are the three printed
-- words: a creature (CR 109.2 draws the set from the battlefield), attacking,
-- and OTHER, which is `Not IsSource` -- the spelling Filter.IsSource fixes for
-- "another" (#163), and the reason a battle-crying creature never pumps itself.
--
-- The tokens Hero of Bladehold's SECOND ability puts onto the battlefield
-- attacking are in the set or not according to WHEN this resolves, and that is
-- the whole of CR 603.3b's ordering choice: CR 611.2c fixes the affected set as
-- the effect begins, so a token that arrives afterwards is not pumped. Pawl.TriggerSpec's
-- "CR 603.3b/702.91a resolving the token-maker first pumps the Soldiers"
-- is the pair of tests that proves both directions.
--
-- Single mode, no targets, ChooseExactly 1 -- so nothing is asked as the ability
-- is placed, which is right because rule 702.91a leaves nothing to choose.
-- intervening = Nothing: rule 702.91a has no "if" clause.
battleCry :: TriggeredAbility Card
battleCry =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }
  where
    effect =
      Effect.ModifyTarget
        Duration.UntilEndOfTurn
        (Modification.ModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 0))
        ( ObjectRef.EachMatching
            (Filter.And [Filter.HasCardType CardType.Creature, Filter.IsAttacking, Filter.Not Filter.IsSource])
        )
