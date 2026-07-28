module Pawl.Keyword where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import Pawl.Type.ActivatedAbility (ActivatedAbility)
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivationTiming as ActivationTiming
import Pawl.Type.Card (Card)
import Pawl.Type.CastingPermission (CastingPermission)
import qualified Pawl.Type.CastingPermission as CastingPermission
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import Pawl.Type.Cost (Cost)
import qualified Pawl.Type.Cost as Cost
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.Effect as Effect
import Pawl.Type.Filter (Filter)
import Pawl.Type.Keyword (Keyword)
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Quantity as Quantity
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.SearchDestination as SearchDestination
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import Pawl.Type.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Type.ZoneChangeSubject as ZoneChangeSubject

-- Rule 702 in its OTHER voice. Most keywords this pool has are read where they
-- matter -- Projection.hasKeyword for an evasion or combat bit, Pawl.Damage for
-- infect's and toxic's damage riders -- because the rule states them as static
-- abilities that some rules-core reader already asks about. Rule 702.70 does
-- not: it spells poisonous out as a TRIGGERED ability, in the same words a card
-- would print, so it has to be MINTED and handed to the ordinary CR 603
-- machinery rather than merely consulted.
--
-- Casing on Keyword here is legitimate for the reason Pawl.Type.Keyword's own
-- comment gives: a keyword is a numbered rule, not an effect's identity. What
-- this module must never do is grow an arm for a CARD.
--
-- The abilities are derived from a projection's POST-LAYER keyword counts, so
-- Humility (LoseAllAbilities, which empties PC.keywords at layer 6) takes rule
-- 702.70a's ability away for free, and an Aura's layer-6 grant adds it for free
-- -- neither needs an arm here.
--
-- triggeredAbilitiesOf's one caller is Pawl.Event's EVENT scan (eventTriggers).
-- Rule 702 has no state-triggered (CR 603.8) or delayed (CR 603.7) keyword
-- ability, so stateTriggers and delayedPending do not consult this; the first
-- keyword that needs them to is the one that must widen those two scans.
--
-- Rule 702.34a's flashback is the second minting customer, and the one that
-- shows how wide this voice is: ONE keyword becomes a cost
-- (flashbackCost, read by Pawl.Cost), a casting permission
-- (castingPermissionsOf, read by Pawl.Cast) and a replacement effect
-- (flashbackExile, installed by Pawl.Cast). Those three readers get ordinary
-- rules objects and never learn that flashback is what produced them.
--
-- These three are derived from a card's PRINTED keywords rather than a
-- projection's post-layer ones, unlike triggeredAbilitiesOf: all three abilities
-- function in the graveyard or on the stack (CR 113.6), where the CR 613 layer
-- system does not reach.

-- CR 702.70b: "If a creature has multiple instances of poisonous, each triggers
-- separately." So this returns one ability PER INSTANCE, which is exactly what
-- the projection's per-keyword count says: `Poisonous 1` twice is two abilities
-- and two poison counters, not one ability for 2. (Contrast CR 702.164b, where
-- toxic's N values are SUMMED into a single rider -- Projection.totalToxic.)
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
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Indestructible -> []
  Keyword.Reach -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Cycling _ _ -> []
  Keyword.Flashback _ -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 602.1: the ACTIVATED abilities rule 702 gives a card while it sits in its
-- owner's hand -- the third sibling of triggeredAbilitiesOf above and
-- castingPermissionsOf below, and the first that mints something a player takes
-- an action with.
--
-- Named for the ZONE rather than for cycling, because that is the classification
-- its one reader wants: Pawl.Activate.abilitiesFor asks "what can be activated
-- from here", and gets an ordinary list of abilities back without learning that
-- rule 702.29 produced any of them. Rule 702 has more hand abilities to come
-- (forecast, CR 702.57, is the next), and each joins this list without its
-- reader changing.
--
-- Printed keywords rather than a projection's post-layer ones, the same rules
-- fact castingPermissionsOf records: CR 113.6b says "an ability that states
-- which zones it functions in functions only from those zones", and rule 702.29a
-- states the hand -- which the CR 613 layer system does not reach.
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
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Indestructible -> []
  Keyword.Reach -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Flashback _ -> []
  Keyword.Poisonous _ -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 702.29a: "'Cycling [cost]' means '[Cost], Discard this card: Draw a card.'"
-- The whole ability, minted from the one cost the keyword carries.
--
-- The discard is a COMPONENT of the activation cost and not an effect, because
-- rule 702.29a puts it before the colon. Three things follow that would all be
-- wrong the other way round: an activation the player backs out of discards
-- nothing (Pawl.Cost.pay restores the entry state), the card is already in the
-- graveyard while the draw is still on the stack, and CR 702.29c's "when you
-- cycle this card" has a cost payment to trigger off rather than a resolution
-- (Pawl.Cost records the event; Pawl.Event matches it).
--
-- The card's own data carries only what is PRINTED on it -- "Cycling {2}" is a
-- mana cost and nothing else -- and rule 702.29a's discard is added here. That
-- is the split the whole module exists for: the card says which keyword, the
-- rule says what it means.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability
-- is activated: the Pawl.Monarch.oneEffect shape poisonous above also takes.
--
-- AnyTime because rule 702.29a states no timing restriction, which leaves CR
-- 117.1b's default: "a player may activate an activated ability any time they
-- have priority."
cycling :: Cost -> Maybe Filter -> ActivatedAbility Card
cycling cost searchFor =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.DiscardThis]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty))
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
    -- player who activated it" -- so You, the perspective Pawl.Resolve evaluates
    -- a PlayerRef against.
    --
    -- CR 702.29e searches instead: "Search your library for a [type] card,
    -- reveal it, and put it into your hand." The reveal is part of the
    -- destination because it is part of that sentence -- see
    -- Pawl.Type.SearchDestination, and CR 701.23e for why a search does not
    -- reveal on its own.
    effect = case searchFor of
      Nothing -> Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)
      Just filter_ -> Effect.Search filter_ SearchDestination.RevealThenHand

-- CR 601.3: the casting permissions rule 702 gives a card for holding a keyword,
-- the sibling of triggeredAbilitiesOf above. A card's own printed permissions
-- (Card.castingPermissions) are a separate, additive list; Pawl.Cast reads both.
--
-- Taken over the card's PRINTED keywords rather than a projection's post-layer
-- ones, and that is the same rules fact Card.castingPermissions' own comment
-- records: this permission functions in the GRAVEYARD (CR 113.6), where the CR
-- 613 layer system does not reach.
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
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Indestructible -> []
  Keyword.Reach -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Poisonous _ -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 702.34a's "... by paying [cost] rather than paying its mana cost": the cost
-- this card may be cast from the graveyard for, or Nothing when it has no
-- flashback. Read by Pawl.Cost.costsFor, which offers it ONLY while the object
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
flashbackCost :: Set Keyword -> Maybe Cost
flashbackCost keywords =
  let costOf keyword = case keyword of
        Keyword.Flashback cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.34a's SECOND static ability, "another that functions while the card is
-- on the stack": "exile this card instead of putting it anywhere else any time
-- it would leave the stack." TheSource, because the rule says "this card" -- the
-- spell itself and no other object.
--
-- The destination is Graveyard rather than "anywhere else": a
-- Pawl.Type.ZoneChangePattern names ONE destination, and the graveyard is the
-- only place a spell leaves the stack for in this pool (resolution CR 608.2n,
-- the CR 608.2b fizzle, and CR 701.6a's counter all call Event.changeZone with
-- it) (#293).
--
-- Not gated on rule 702.34a's "if the flashback cost was paid": nothing here can
-- see which cost was paid (#101). Pawl.Cast installs this only for a spell cast
-- FROM THE GRAVEYARD, where the two coincide -- see its own comment.
--
-- The door Pawl.Cast uses, so that module installs a REPLACEMENT EFFECT it never
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
-- Pawl.Event.eventBindings stamps under the reserved Binding.triggerPlayer slot
-- as the trigger is gathered -- so the payload is an ordinary slot read and this
-- ability needs no opcode of its own. NOT the ability's controller: CR 603.3a
-- makes that the creature's controller, and the poison goes to their victim.
--
-- Single mode, no targets, ChooseExactly 1 -- so nothing is asked as the ability
-- is placed, which is right because rule 702.70a leaves nothing to choose. Same
-- shape Pawl.Monarch.oneEffect builds for rule 725's inherent abilities.
-- intervening = Nothing: rule 702.70a has no "if" clause.
poisonous :: Natural -> TriggeredAbility Card
poisonous n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }
  where
    effect =
      Effect.GainPlayerCounters
        (PlayerRef.InSlot Binding.triggerPlayer)
        PlayerCounterKind.Poison
        (Quantity.Literal (toInteger n))
