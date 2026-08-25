module Pawl.Engine.Keyword where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.AbilityName as AbilityName
import Pawl.Types.ActivatedAbility (ActivatedAbility)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CastOffer as CastOffer
import Pawl.Types.CastingPermission (CastingPermission)
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.Face as Face
import Pawl.Types.Filter (Filter)
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.MorphVariant as MorphVariant
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- Rule 702 in its OTHER voice. Most keywords are read where they matter, the rule
-- stating them as static abilities some rules-core reader already asks about. The
-- ones rule 702 spells out as TRIGGERED or ACTIVATED abilities have to be MINTED
-- here and handed to the ordinary CR 603 and CR 602 machinery instead.
--
-- Casing on Keyword here is legitimate for the reason Pawl.Types.Keyword's own
-- comment gives: a keyword is a numbered rule, not an effect's identity. What this
-- module must never do is grow an arm for a CARD. Rule 702 has no state-triggered
-- (CR 603.8) keyword ability, so the first keyword that needs one must widen
-- Event's two scans.
--
-- Rule 702.34a's flashback shows how wide this voice is: ONE keyword becomes a
-- cost, a casting permission and a replacement effect, none of whose readers learn
-- that flashback produced them. All three function in the graveyard or on the
-- stack (CR 113.6), and the graveyard reads go through the projection
-- (Cost.costsFor, Cast.graveyardKeywords).
--
-- Not implemented: what is read while the object is on the STACK stays printed
-- (#1859).

-- CR 702.70b: multiple instances of poisonous each trigger separately, so this
-- returns one ability PER INSTANCE -- `Poisonous 1` twice is two abilities and two
-- poison counters, not one ability for 2. (Contrast CR 702.164b, where toxic's N
-- values are SUMMED -- Projection.totalToxic.) Most of the arms below say the same
-- under their own rule; a keyword whose rule states no such clause gets it from CR
-- 603.2's general reason instead.
--
-- Order is the Map's, which is Keyword's Ord -- rule-number order, and stable. The
-- CR 603.3b ordering prompt indexes into the scan's canonical order, so this being
-- deterministic is what keeps that prompt reproducible.
triggeredAbilitiesOf :: Map Keyword Natural -> [TriggeredAbility Card]
triggeredAbilitiesOf counts = concatMap (uncurry abilitiesFor) (Map.toAscList counts)

-- The abilities one keyword, held `count` times, contributes. The ROSTER of the
-- keywords rule 702 states as triggered abilities, exhaustive under -Werror so it
-- cannot fall behind rule 702 the way a count in prose can.
abilitiesFor :: Keyword -> Natural -> [TriggeredAbility Card]
abilitiesFor keyword count = case keyword of
  Keyword.Poisonous n -> List.genericReplicate count (poisonous n)
  -- TWO abilities per instance -- hence the `concat`: rule 702.45a's ability
  -- watches two events, and a TriggeredAbility carries one condition.
  Keyword.Bushido n -> concat (List.genericReplicate count (bushido n))
  -- CR 702.46b: each instance triggers separately.
  Keyword.Soulshift n -> List.genericReplicate count (soulshift n)
  Keyword.Bloodthirst _ -> []
  Keyword.Haunt -> List.genericReplicate count haunt
  Keyword.SplitSecond -> []
  -- Rule 702.63a states three abilities, the first of them a replacement effect
  -- rather than a trigger, so two land here.
  Keyword.Vanishing _ -> concat (List.genericReplicate count vanishing)
  -- CR 702.32a's SECOND ability: the rule states two, the first a replacement
  -- effect, so unlike vanishing only one trigger lands here.
  Keyword.Fading _ -> List.genericReplicate count fading
  -- CR 702.68b: each instance triggers separately.
  Keyword.Frenzy n -> List.genericReplicate count (frenzy n)
  -- CR 702.43a's SECOND ability, one per instance (CR 702.43b).
  Keyword.Modular _ -> List.genericReplicate count modular
  Keyword.Annihilator n -> List.genericReplicate count (annihilator n)
  Keyword.Afflict n -> List.genericReplicate count (afflict n)
  Keyword.BattleCry -> List.genericReplicate count battleCry
  Keyword.Evolve -> List.genericReplicate count evolve
  -- CR 702.105b: each instance triggers separately.
  Keyword.Dethrone -> List.genericReplicate count dethrone
  Keyword.LevelUp _ -> []
  Keyword.Outlast _ -> []
  Keyword.Prowess -> List.genericReplicate count prowess
  Keyword.Flanking -> List.genericReplicate count flanking
  Keyword.Exalted -> List.genericReplicate count exalted
  Keyword.Melee -> List.genericReplicate count melee
  Keyword.Mentor -> List.genericReplicate count mentor
  Keyword.Afterlife n -> List.genericReplicate count (afterlife n)
  -- CR 702.123b: each instance triggers separately.
  Keyword.Fabricate n -> List.genericReplicate count (fabricate n)
  Keyword.Provoke -> List.genericReplicate count provoke
  Keyword.Rampage n -> List.genericReplicate count (rampage n)
  Keyword.Compleated -> []
  Keyword.ReadAhead -> []
  Keyword.Training -> List.genericReplicate count training
  Keyword.Renown n -> List.genericReplicate count (renown n)
  Keyword.Persist -> List.genericReplicate count persist
  Keyword.Undying -> List.genericReplicate count undying
  -- CR 702.115b: each instance triggers separately.
  Keyword.Ingest -> List.genericReplicate count ingest
  Keyword.Crew _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Protection _ -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.TrampleOverPlaneswalkers -> []
  Keyword.Vigilance -> []
  -- CR 702.21a's ability, one per instance for CR 603.2's general reason, so a
  -- spell targeting a doubly warded permanent is offered both costs.
  Keyword.Ward cost -> List.genericReplicate count (ward cost)
  Keyword.Banding -> []
  Keyword.Phasing -> []
  Keyword.Shadow -> []
  Keyword.Horsemanship -> []
  Keyword.Aftermath -> []
  Keyword.JumpStart -> []
  Keyword.Fear -> []
  Keyword.Intimidate -> []
  Keyword.Morph {} -> []
  Keyword.Menace -> []
  Keyword.Cycling {} -> []
  Keyword.Kicker _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Infect -> []
  Keyword.Wither -> []
  Keyword.Changeling -> []
  Keyword.Reinforce {} -> []
  Keyword.Devoid -> []
  Keyword.Skulk -> []
  Keyword.Riot -> []
  Keyword.Unleash -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Decayed -> List.genericReplicate count decayed
  Keyword.Toxic _ -> []
  Keyword.Disguise _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  -- CR 702.94a's linked triggered half, one per instance for CR 603.2's general
  -- reason. Minted HERE rather than in a hand-only roster because WHERE it
  -- functions is CR 113.6k's question, answered in Event.zonesTriggeredFrom.
  Keyword.Miracle cost -> List.genericReplicate count (miracle cost)
  Keyword.StartYourEngines -> []
  -- CR 701.43d's static ability mints NO triggered ability: the rule lets a card
  -- print a linked "when you do" beside it without saying what that ability does,
  -- so each printing authors its own on TriggerCondition.SelfExerted.
  Keyword.Exert -> []

-- CR 602.1: the ACTIVATED abilities rule 702 gives a card while it sits in its
-- owner's hand. Named for the ZONE rather than for cycling, because that is the
-- classification its reader wants: Activate.abilitiesFor asks "what can be
-- activated from here" and never learns which rule produced any of them.
--
-- Printed keywords rather than a projection's post-layer ones, the same rules fact
-- castingPermissionsOf records: CR 113.6b confines an ability to the zones it
-- states, and rules 702.29a and 702.77a state the hand.
--
-- Not implemented: a card in a hand whose CYCLING or REINFORCE an effect granted
-- or removed, which the printed set misses (#1859). Narrowed to those two rules
-- rather than to keywords at large -- Teferi, Mage of Zhalfir does grant a
-- keyword to a card in a hand, and Cast.instantSpeed reads that one through the
-- projection.
handAbilitiesOf :: Set Keyword -> [ActivatedAbility Card]
handAbilitiesOf = concatMap handAbilitiesFor . Set.toAscList

-- Exhaustive for the reason permissionsFor is: the next keyword that functions
-- from a hand must break this build rather than silently produce nothing.
handAbilitiesFor :: Keyword -> [ActivatedAbility Card]
handAbilitiesFor keyword = case keyword of
  Keyword.Cycling (Cycling.MkCycling cost searchFor) -> [cycling cost searchFor]
  Keyword.Reinforce (Reinforce.MkReinforce n cost) -> [reinforce n cost]
  Keyword.Afflict _ -> []
  Keyword.Crew _ -> []
  Keyword.Fabricate _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Protection _ -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.TrampleOverPlaneswalkers -> []
  Keyword.Vigilance -> []
  Keyword.Ward _ -> []
  Keyword.Banding -> []
  Keyword.Flanking -> []
  Keyword.Phasing -> []
  Keyword.Shadow -> []
  Keyword.Horsemanship -> []
  Keyword.Aftermath -> []
  Keyword.JumpStart -> []
  Keyword.Fear -> []
  Keyword.Intimidate -> []
  Keyword.Morph {} -> []
  Keyword.Menace -> []
  Keyword.Renown _ -> []
  Keyword.Kicker _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Bushido _ -> []
  Keyword.Soulshift _ -> []
  Keyword.Bloodthirst _ -> []
  Keyword.Haunt -> []
  Keyword.SplitSecond -> []
  Keyword.Poisonous _ -> []
  Keyword.Annihilator _ -> []
  Keyword.BattleCry -> []
  Keyword.Evolve -> []
  Keyword.Dethrone -> []
  Keyword.LevelUp _ -> []
  Keyword.Outlast _ -> []
  Keyword.Prowess -> []
  Keyword.Infect -> []
  Keyword.Wither -> []
  Keyword.Exalted -> []
  Keyword.Mentor -> []
  Keyword.Afterlife _ -> []
  Keyword.Provoke -> []
  Keyword.Changeling -> []
  Keyword.Devoid -> []
  Keyword.Ingest -> []
  Keyword.Skulk -> []
  Keyword.Melee -> []
  Keyword.Rampage _ -> []
  Keyword.Riot -> []
  Keyword.Unleash -> []
  Keyword.Modular _ -> []
  Keyword.Vanishing _ -> []
  Keyword.Fading _ -> []
  Keyword.Frenzy _ -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Decayed -> []
  Keyword.Compleated -> []
  Keyword.ReadAhead -> []
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Disguise _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  -- CR 702.94a's hand ability is TRIGGERED rather than activated, so it is
  -- minted by `abilitiesFor` above and reached from a hand by CR 113.6k.
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  Keyword.Exert -> []
  Keyword.Persist -> []
  Keyword.Undying -> []

-- CR 702.29a's whole ability, minted from the one cost the keyword carries.
--
-- The discard is a COMPONENT of the activation cost and not an effect, rule
-- 702.29a putting it before the colon. Three things follow: an activation the
-- player backs out of discards nothing, the card is already in the graveyard while
-- the draw is still on the stack, and CR 702.29c's "when you cycle this card" has
-- a cost payment to trigger off rather than a resolution. ToPayCyclingCost is what
-- that rule means by "an activation cost of a cycling ability", and it covers rule
-- 702.29e's typecycling too, rule 702.29f making those cycling abilities and this
-- one function minting both.
cycling :: Cost Keyword -> Maybe (Filter Keyword) -> ActivatedAbility Card
cycling cost searchFor =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.DiscardThis DiscardCause.ToPayCyclingCost]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [],
      -- CR 702.29a gives the card this ability outright, with no "as long as".
      ActivatedAbility.condition = Nothing,
      -- Nothing on every keyword-minted ability: no clause of a card refers to
      -- one, CR 702's own text being what mints it.
      ActivatedAbility.name = Nothing
    }
  where
    -- The only difference between rule 702.29a and rule 702.29e: what the ability
    -- does once its cost is paid. Everything above is shared, which is CR 702.29f
    -- holding by construction.
    --
    -- You either way: CR 113.8 makes the ability's controller the player who
    -- activated it, and rule 702.29e prints "your library". The reveal is part of
    -- the destination because it is part of that same sentence (CR 701.23e).
    effect = case searchFor of
      Nothing -> Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1) Nothing)
      -- CR 702.29e's "search your library for a [quality] card", so one card is
      -- the whole instruction's count.
      Just filter_ ->
        Effect.Search
          Search.MkSearch
            { Search.searcher = PlayerRef.Relative PlayerRelation.You,
              Search.owner = PlayerRef.Relative PlayerRelation.You,
              -- CR 702.29e prints "your library" and no other zone.
              Search.zones = Set.singleton Zone.Library,
              Search.quantity = Just (Quantity.Literal 1),
              Search.filter = filter_,
              -- CR 702.29e prints no "up to", and its quality-stating filter puts
              -- the search under CR 701.23b anyway, so this value is unobservable.
              Search.upTo = False,
              Search.destination = SearchDestination.RevealThenHand
            }

-- CR 702.77a's whole ability. Cycling's one clause over, and the first hand
-- ability with a TARGET: the target is chosen at CR 601.2c, before CR 601.2h
-- pays and so before the discard, and the ability outlives the card it discards
-- (CR 113.7a).
--
-- The discard is a COMPONENT of the activation cost for cycling's reasons. Its
-- cause is Ordinary and not ToPayCyclingCost: rule 702.77 never says reinforce is
-- a cycling ability, so CR 702.29c's "when you cycle this card" must not see it.
--
-- Quantity.Literal and not a counter reading: N is written on the card, where
-- modular's count is measured off the dying permanent.
reinforce :: Natural -> Cost Keyword -> ActivatedAbility Card
reinforce n cost =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.DiscardThis DiscardCause.Ordinary]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) (Map.singleton reinforceTarget slot)))
          (ModeSelection.ChooseExactly 1),
      -- CR 702.77a states no timing restriction, which leaves CR 117.1b's
      -- default, and gives the ability outright with no "as long as".
      ActivatedAbility.restrictions = [],
      ActivatedAbility.condition = Nothing,
      -- Nothing on every keyword-minted ability: no clause of a card refers to
      -- one, CR 702's own text being what mints it.
      ActivatedAbility.name = Nothing
    }
  where
    slot = TargetSlot.required Pool.Creatures Nothing
    effect =
      Effect.PutCounters
        ( PutCounters.MkPutCounters
            CounterKind.PlusOnePlusOne
            (Quantity.Literal (toInteger n))
            (ObjectRef.InSlot reinforceTarget)
        )

-- The slot rule 702.77a's one target is chosen into, mentorTarget's position.
reinforceTarget :: SlotName.SlotName
reinforceTarget = SlotName.MkSlotName (Text.pack "reinforced")

-- CR 602.1: the ACTIVATED abilities rule 702 gives a PERMANENT, handAbilitiesOf's
-- sibling one zone over.
--
-- POST-LAYER keywords, unlike handAbilitiesOf's printed ones, and the contrast is
-- CR 113.6 again: this ability functions on the battlefield, which the projection
-- does reach. So Humility takes crew away at CR 613.1f layer 6 for free.
--
-- One ability PER INSTANCE, rule 702.70b's reading rather than rule 702.164b's: CR
-- 702.122a states a whole self-contained ability, so a permanent with crew twice
-- has two of them to activate and two thresholds. Order is the Map's, for
-- triggeredAbilitiesOf's reason.
battlefieldAbilitiesOf :: Map Keyword Natural -> [ActivatedAbility Card]
battlefieldAbilitiesOf counts = concatMap (uncurry battlefieldAbilitiesFor) (Map.toAscList counts)

-- Exhaustive, exactly as handAbilitiesFor is, and for the same reason.
battlefieldAbilitiesFor :: Keyword -> Natural -> [ActivatedAbility Card]
battlefieldAbilitiesFor keyword count = case keyword of
  Keyword.Crew n -> List.genericReplicate count (crew n)
  Keyword.Fabricate _ -> []
  Keyword.Cycling {} -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Protection _ -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.TrampleOverPlaneswalkers -> []
  Keyword.Vigilance -> []
  Keyword.Ward _ -> []
  Keyword.Banding -> []
  Keyword.Flanking -> []
  Keyword.Phasing -> []
  Keyword.Shadow -> []
  Keyword.Horsemanship -> []
  Keyword.Aftermath -> []
  Keyword.JumpStart -> []
  Keyword.Afflict _ -> []
  Keyword.Fear -> []
  Keyword.Intimidate -> []
  -- CR 702.37e: turning a face-down permanent face up is a SPECIAL ACTION and
  -- doesn't use the stack (CR 116), so morph gives no activated ability;
  -- morphCost below serves that action instead.
  Keyword.Morph {} -> []
  Keyword.Menace -> []
  Keyword.Renown _ -> []
  Keyword.Kicker _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Bushido _ -> []
  Keyword.Soulshift _ -> []
  Keyword.Bloodthirst _ -> []
  Keyword.Haunt -> []
  Keyword.SplitSecond -> []
  Keyword.Poisonous _ -> []
  Keyword.Annihilator _ -> []
  Keyword.BattleCry -> []
  Keyword.Evolve -> []
  Keyword.Dethrone -> []
  Keyword.LevelUp cost -> List.genericReplicate count (levelUp cost)
  Keyword.Outlast cost -> List.genericReplicate count (outlast cost)
  Keyword.Prowess -> []
  Keyword.Infect -> []
  Keyword.Wither -> []
  Keyword.Exalted -> []
  Keyword.Mentor -> []
  Keyword.Afterlife _ -> []
  Keyword.Provoke -> []
  Keyword.Changeling -> []
  Keyword.Reinforce {} -> []
  Keyword.Devoid -> []
  Keyword.Ingest -> []
  Keyword.Skulk -> []
  Keyword.Melee -> []
  Keyword.Rampage _ -> []
  Keyword.Riot -> []
  Keyword.Unleash -> []
  Keyword.Modular _ -> []
  Keyword.Vanishing _ -> []
  Keyword.Fading _ -> []
  Keyword.Frenzy _ -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Decayed -> []
  Keyword.Compleated -> []
  Keyword.ReadAhead -> []
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Disguise _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  -- Exerting is a cost paid at CR 508.1g, which Combat.declareAttackers offers
  -- rather than the stack.
  Keyword.Exert -> []
  Keyword.Persist -> []
  Keyword.Undying -> []

-- CR 702.122a's whole ability, minted from the one number the keyword carries.
--
-- THE COST. Only the AGGREGATE is the component's own, total power being a
-- property of the chosen set rather than of any candidate. `Not IsSource` is
-- load-bearing: a Vehicle that has already become a creature could otherwise crew
-- itself. CR 302.6 does NOT reach this cost, in either direction.
--
-- The Vehicle needs no haste, the tap symbol not being in the cost; and a creature
-- that arrived this turn may still be tapped to crew, rule 302.6 gating only a
-- creature's OWN activated ability with the tap symbol in it.
--
-- THE EFFECT. "Becomes an artifact creature" ADDS two card types and sets nothing,
-- which is CR 205.1b naming this exact phrase -- so the Vehicle stays a Vehicle,
-- and AddCardType is right where SetCardType's CR 205.1a replacement would take
-- the artifact type and the Vehicle subtype away. TWO of them (CR 300.1) in one
-- mode, since CR 613.7b stamps both at once. Layer 4 either way (CR 613.1d), and
-- CR 208.3 needs no clause: the Vehicle's printed power and toughness are already
-- gated on its being a creature at Projection's read points.
--
-- Binding.triggerSource, so the Vehicle is named and never TARGETED (CR 115.10a):
-- a targeted crew would fizzle to shroud and fire "becomes the target" triggers
-- the printed ability does not.
crew :: Natural -> ActivatedAbility Card
crew n =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost =
        Cost.MkCost
          { -- CR 118.5: rule 702.122a's cost has no mana part, which is
            -- `Just` an empty one and not the Nothing that means unpayable.
            Cost.mana = Just (ManaCost.MkManaCost []),
            Cost.components = [CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion)]
          },
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [becomes CardType.Artifact, becomes CardType.Creature]))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [],
      -- CR 702.122a gives the permanent this ability outright, with no "as long
      -- as", cycling's answer.
      ActivatedAbility.condition = Nothing,
      -- Nothing on every keyword-minted ability: no clause of a card refers to
      -- one, CR 702's own text being what mints it.
      ActivatedAbility.name = Nothing
    }
  where
    criterion =
      Filter.And
        [ Filter.HasCardType CardType.Creature,
          Filter.Not Filter.IsTapped,
          Filter.ControlledBy PlayerRelation.You,
          Filter.Not Filter.IsSource
        ]
    becomes cardType =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.AddCardType cardType)
            (ObjectRef.InSlot Binding.triggerSource)
        )

-- CR 702.87a's whole ability, outlast's twin below.
--
-- THE COST is the printed one UNCHANGED -- rule 702.87a appends no ", {T}", so
-- unlike outlast there is no CostComponent.TapThis and CR 302.6 does not reach
-- this ability: a leveler that arrived this turn can level up.
--
-- THE COUNTER grants nothing by itself: CR 711.2a's level symbols are ordinary
-- conditional static abilities on the card, reading this tally through
-- Quantity.ObjectCounters.
--
-- CR 602.5d is the timing clause and the ONLY restriction. The condition is
-- Nothing because CR 711.4 says so outright, so the ability is still offered past
-- the last level symbol's range.
levelUp :: Cost Keyword -> ActivatedAbility Card
levelUp cost =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost,
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton gain))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [ActivationRestriction.SorcerySpeed],
      ActivatedAbility.condition = Nothing,
      -- Nothing on every keyword-minted ability: no clause of a card refers to
      -- one, CR 702's own text being what mints it.
      ActivatedAbility.name = Nothing
    }
  where
    gain = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.Level (Quantity.Literal 1) (ObjectRef.InSlot Binding.triggerSource))

-- CR 702.107a's whole ability, levelUp's twin; the card names only the cost.
--
-- THE COST is the printed one with CostComponent.TapThis APPENDED, rule 702.107a's
-- ", {T}". Unlike crew's cost the tap symbol is the permanent's own, so CR 302.6
-- does reach this ability and a creature that arrived this turn cannot outlast.
--
-- THE EFFECT names the permanent through Binding.triggerSource, so rule 702.107a's
-- "this creature" is named and never TARGETED (CR 115.10a). CR 602.5d is the
-- timing clause and the ONLY restriction.
outlast :: Cost Keyword -> ActivatedAbility Card
outlast cost =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.TapThis]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton grow))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [ActivationRestriction.SorcerySpeed],
      ActivatedAbility.condition = Nothing,
      -- Nothing on every keyword-minted ability: no clause of a card refers to
      -- one, CR 702's own text being what mints it.
      ActivatedAbility.name = Nothing
    }
  where
    grow = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.InSlot Binding.triggerSource))

-- CR 601.3: the casting permissions rule 702 gives a card for holding a keyword.
-- A card's own printed permissions are a separate, additive list.
--
-- WHICH keyword set is the caller's to choose: a card in a GRAVEYARD is read
-- through the projection, so a granted flashback grants its permission too.
--
-- Not implemented: a card in a LIBRARY is read as printed instead (#1859), which
-- is where Panglacial Wurm's permission is consulted.
--
-- The card types come along because rule 702.34a's permission is CONDITIONAL on
-- them, and they are the types of the one FACE being proposed.
castingPermissionsOf :: Set CardType.CardType -> Set Keyword -> [CastingPermission]
castingPermissionsOf cardTypes = concatMap (permissionsFor cardTypes) . Set.toAscList

-- Exhaustive, exactly as abilitiesFor is: the next keyword that grants a zone
-- permission must break this build rather than silently grant nothing.
permissionsFor :: Set CardType.CardType -> Keyword -> [CastingPermission]
permissionsFor cardTypes keyword = case keyword of
  -- CR 702.34a: "You may cast this card from your graveyard IF THE RESULTING
  -- SPELL IS AN INSTANT OR SORCERY SPELL by paying [cost] rather than paying its
  -- mana cost." The clause gates the permission itself, so a card that fails it
  -- gets no permission at all (Pawl.CastSpec's "FlashbackCardType" group).
  --
  -- Gated HERE rather than in Cast.permitsCastFromGraveyard: the condition belongs
  -- to rule 702.34a, and a card that PRINTS the same permission must not inherit
  -- flashback's clause.
  Keyword.Flashback _
    | Set.member CardType.Instant cardTypes || Set.member CardType.Sorcery cardTypes ->
        [CastingPermission.CastFromGraveyard]
    | otherwise -> []
  -- CR 702.29a is an ACTIVATED ability, not a casting permission: cycling discards
  -- the card, it never casts it. Rule 702.122a is one too, one zone over.
  Keyword.Cycling {} -> []
  Keyword.Crew _ -> []
  Keyword.Fabricate _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  -- CR 702.8a grants no permission, and it is the near miss worth stating: its
  -- SECOND sentence widens the TIME a cast may be proposed at (Cast.instantSpeed)
  -- and names no zone, while its first names the zones the ABILITY functions in
  -- rather than the zones the card may be cast from.
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  Keyword.Protection _ -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.TrampleOverPlaneswalkers -> []
  Keyword.Vigilance -> []
  Keyword.Ward _ -> []
  Keyword.Banding -> []
  Keyword.Flanking -> []
  Keyword.Phasing -> []
  Keyword.Shadow -> []
  Keyword.Horsemanship -> []
  -- CR 702.127a's FIRST static ability. Ungated, unlike flashback's arm above,
  -- rule 702.127a's own first sentence already confining aftermath to split cards.
  -- Its SECOND ability, "can't be cast from any zone other than a graveyard", is a
  -- PROHIBITION and so Pawl.Engine.Cast's, at its Zone.Hand arm.
  Keyword.Aftermath -> [CastingPermission.CastFromGraveyard]
  -- CR 702.133a's FIRST static ability, gated exactly as flashback's arm above.
  -- What the two rules do NOT share is the cost -- flashback replaces the mana
  -- cost and this one adds a discard to it -- and that half is Cost.costsFor's.
  Keyword.JumpStart
    | Set.member CardType.Instant cardTypes || Set.member CardType.Sorcery cardTypes ->
        [CastingPermission.CastFromGraveyard]
    | otherwise -> []
  Keyword.Afflict _ -> []
  Keyword.Fear -> []
  Keyword.Intimidate -> []
  Keyword.Morph {} -> []
  Keyword.Menace -> []
  Keyword.Renown _ -> []
  -- CR 702.33a's "as you cast this spell" is not a zone or a timing permission --
  -- it is when the additional cost is announced (CR 601.2b). CR 702.42a's entwine
  -- is the same shape one clause over.
  Keyword.Kicker _ -> []
  Keyword.Entwine _ -> []
  Keyword.Bushido _ -> []
  Keyword.Soulshift _ -> []
  Keyword.Bloodthirst _ -> []
  Keyword.Haunt -> []
  Keyword.SplitSecond -> []
  Keyword.Poisonous _ -> []
  Keyword.Annihilator _ -> []
  Keyword.BattleCry -> []
  Keyword.Evolve -> []
  Keyword.Dethrone -> []
  Keyword.LevelUp _ -> []
  Keyword.Outlast _ -> []
  Keyword.Prowess -> []
  Keyword.Infect -> []
  Keyword.Wither -> []
  Keyword.Exalted -> []
  Keyword.Mentor -> []
  Keyword.Afterlife _ -> []
  Keyword.Provoke -> []
  Keyword.Changeling -> []
  Keyword.Reinforce {} -> []
  Keyword.Devoid -> []
  Keyword.Ingest -> []
  Keyword.Skulk -> []
  Keyword.Melee -> []
  Keyword.Rampage _ -> []
  Keyword.Riot -> []
  Keyword.Unleash -> []
  Keyword.Modular _ -> []
  Keyword.Vanishing _ -> []
  Keyword.Fading _ -> []
  Keyword.Frenzy _ -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Decayed -> []
  Keyword.Compleated -> []
  Keyword.ReadAhead -> []
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  -- CR 702.168a grants NO permission, and it is the near miss worth stating: the
  -- ability "functions in any zone FROM WHICH YOU COULD PLAY THE CARD it's on",
  -- so it widens no zone -- CR 702.168b's "you can use a disguise ability to cast
  -- a card from any zone from which you could NORMALLY cast it" is the same fact
  -- said the other way. What it does grant is an ALTERNATIVE COST, which is
  -- Cost.faceDownCost's, and morph's arm below takes the same reading.
  Keyword.Disguise _ -> []
  -- CR 702.170a is a static ability functioning in a HAND, and what it grants is
  -- CR 116.2k's special action rather than a cast. CR 702.170d's permission to cast
  -- from EXILE belongs to the PLOTTED card and not to the keyword, so it is object
  -- state (Object.plotted) that Cast.permitsCastFromExile reads.
  Keyword.Plot _ -> []
  -- CR 702.143a, the arm above's argument unchanged: CR 116.2h's special action in
  -- a hand, and CR 702.143d's permission belonging to the FORETOLD card
  -- (Object.foretold) rather than the keyword.
  Keyword.Foretell _ -> []
  -- CR 702.94a's cast is one CR 608.2g offers during the linked ability's
  -- resolution, so it is not a standing CR 601.3 permission the way flashback's
  -- graveyard cast is: nothing may be cast from a hand by miracle at a player's
  -- own timing.
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  Keyword.Exert -> []
  Keyword.Persist -> []
  Keyword.Undying -> []

-- | CR 702.127a's SECOND static ability: "this half of this split card can't be
-- cast from any zone other than a graveyard". A PROHIBITION, so it is a question
-- Pawl.Engine.Cast asks of the zone it is about to offer. Membership rather than a
-- count, rule 702.127a taking no parameter.
hasAftermath :: Set Keyword -> Bool
hasAftermath = Set.member Keyword.Aftermath

-- CR 702.8a: does this card's keyword set let it be played any time its
-- controller could cast an instant? Its one reader is
-- Pawl.Engine.Cast.instantSpeed, which turns it into the CR 302.1 / 307.1 window
-- being lifted for that one card.
--
-- MEMBERSHIP, not a count: CR 702.8b makes multiple instances redundant.
--
-- Rule 702.8a's first sentence and CR 113.6e put the ability in any zone the card
-- could be played from, and on the stack, which is why the caller asks a card
-- rather than a permanent. WHICH keyword set is the caller's to choose, as it is
-- for castingPermissionsOf: Cast.instantSpeed asks this of the proposed face's
-- printed keywords AND of the object's post-layer ones, so Teferi, Mage of
-- Zhalfir's grant to a card in a hand reaches it (Pawl.CastSpec's Teferi pair).
hasFlash :: Set Keyword -> Bool
hasFlash = Set.member Keyword.Flash

-- | CR 702.133a's ADDITIONAL cost, "discarding a card": whether this card's
-- keywords add one to a cast from a graveyard. Read by Cost.costsFor only while
-- the object is in a graveyard, the zone half of the same sentence.
--
-- A Bool rather than morphCost's Maybe Cost: rule 702.133a states the cost
-- itself, so there is nothing to read off the card. Membership rather than a
-- count, the rule taking no parameter.
hasJumpStart :: Set Keyword -> Bool
hasJumpStart = Set.member Keyword.JumpStart

-- CR 702.34a: every cost this card may be cast from the graveyard for, in
-- ascending Set order, and empty when it has no flashback. Read by
-- Pawl.Engine.Cost.costsFor, which offers them ONLY while the object is in a
-- graveyard -- the zone half of the same sentence.
--
-- A LIST where every other keyword-cost reader in this module answers a Maybe,
-- and rule 702.34a is why: it states no limit on how many flashback abilities an
-- object has, and CR 601.2b's "a player can't apply two alternative methods of
-- casting or two alternative costs to a single spell" makes two of them a CHOICE
-- between the two rather than a sum. The Fugitive Doctor grants one on top of a
-- printed one, so a card holding two is a board rather than a hypothesis; the
-- test that proves it is Pawl.CastSpec's FugitiveDoctor group.
--
-- A wildcard rather than an exhaustive case: this asks about ONE named
-- constructor rather than classifying every keyword.
flashbackCosts :: Set Keyword -> [Cost Keyword]
flashbackCosts keywords =
  let costOf keyword = case keyword of
        Keyword.Flashback cost -> Just cost
        _ -> Nothing
   in Maybe.mapMaybe costOf (Set.toAscList keywords)

-- CR 702.37a / 702.37e: the MORPH cost -- what a face-down permanent's controller
-- pays to turn it face up as CR 116.2b's special action -- or Nothing when the
-- card has no morph ability. NOT the cost of the morph CAST, which rule 702.37a
-- writes into the rule itself, so that one comes from Cost.faceDownCost.
--
-- Asked of the card's PRINTED keywords: a face-down permanent projects only what
-- its allower listed (CR 708.2), so a projected read would find no morph ability
-- to charge for.
--
-- CR 702.37b: MEGAMORPH REACHES HERE TOO, which is why Pawl.Types.Keyword's Morph
-- carries a variant rather than having a sibling constructor -- the case below is
-- a WILDCARD, so a `Megamorph` constructor beside `Morph` would fall through to
-- Nothing and silently make every megamorph card uncastable face down and
-- unturnable face up, with nothing for -Werror to report.
--
-- ONE cost per card: the ascending-least printed instance, ordered by
-- Pawl.Types.Morph's derived Ord, which compares the Cost before the variant.
morphCost :: Set Keyword -> Maybe (Cost Keyword)
morphCost keywords =
  let costOf keyword = case keyword of
        Keyword.Morph (Morph.MkMorph cost _) -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.168a / 702.168d: the DISGUISE cost -- what a face-down permanent's
-- controller pays to turn it face up as CR 116.2b's special action -- or Nothing
-- when the card has no disguise ability. NOT the cost of the disguise CAST,
-- which rule 702.168a fixes at {3} for every printing, so that one comes from
-- Cost.faceDownCost as morph's does.
--
-- Asked of the card's PRINTED keywords, morphCost's reason exactly: CR 702.168b
-- leaves the face-down permanent with no keyword but the ward it lists, so a
-- projected read would find no disguise ability to charge for.
--
-- A SEPARATE function from morphCost and not a widening of it, which is what
-- keeps CR 702.168d's price apart from CR 702.37e's: a permanent with both
-- abilities is turnable by either procedure at either cost, and one function
-- answering for both would charge whichever the Set happened to hold first.
--
-- ONE cost per card (the ascending-least), morphCost's shape.
disguiseCost :: Set Keyword -> Maybe (Cost Keyword)
disguiseCost keywords =
  let costOf keyword = case keyword of
        Keyword.Disguise cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.33a: the ADDITIONAL cost this card's controller may pay as they cast it,
-- or Nothing when it has no kicker. Offered at CR 601.2b and added to whichever
-- candidate cost was announced (CR 601.2f). A wildcard, morphCost's shape.
--
-- Nothing beyond the FIRST kicker cost is reachable, so CR 702.33b's "kicker
-- [cost 1] and/or [cost 2]" is unrepresented (gap #1235).
kickerCost :: Set Keyword -> Maybe (Cost Keyword)
kickerCost keywords =
  let costOf keyword = case keyword of
        Keyword.Kicker cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.42a: every ADDITIONAL cost this card's controller may pay to choose all
-- of its modes, in ascending Set order, and empty when it has no entwine. Offered
-- at CR 601.2b and added to whichever candidate cost was announced (CR 601.2f).
--
-- A LIST, flashbackCosts' shape, and for the mirror-image reason: rule 702.42
-- states no limit on how many entwine abilities an object has (contrast CR
-- 702.41b for affinity and CR 702.43b for modular), and entwine's cost is
-- ADDITIONAL rather than alternative, so CR 118.8a's "any number of additional
-- costs may be applied" makes two of them a SUM rather than a choice. The
-- summing is Pawl.Engine.Cast.entwineOffer's, since CR 601.2f's addition lives
-- in Pawl.Engine.Cost, which reads this module.
--
-- A wildcard rather than an exhaustive case, flashbackCosts' reason: this asks
-- about ONE named constructor rather than classifying every keyword.
entwineCosts :: Set Keyword -> [Cost Keyword]
entwineCosts keywords =
  let costOf keyword = case keyword of
        Keyword.Entwine cost -> Just cost
        _ -> Nothing
   in Maybe.mapMaybe costOf (Set.toAscList keywords)

-- CR 702.170a: what CR 116.2k's special action costs, or Nothing when the card has
-- no plot.
--
-- The cost of the ACTION and never of the cast: rule 702.170d makes the later cast
-- free, so nothing consults this from Pawl.Engine.Cost.
--
-- A wildcard, and ONE cost per card (the ascending-least), morphCost's shape.
plotCost :: Set Keyword -> Maybe (Cost Keyword)
plotCost keywords =
  let costOf keyword = case keyword of
        Keyword.Plot cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.143a: what a foretold card is CAST for, or Nothing when the card has no
-- foretell. The cost of the CAST and never of the special action, plotCost's
-- mirror: CR 116.2h fixes the action's cost at {2} for every printing, so
-- Pawl.Engine.Foretell mints that itself.
--
-- A wildcard, and ONE cost per card (the ascending-least), morphCost's shape.
foretellCost :: Set Keyword -> Maybe (Cost Keyword)
foretellCost keywords =
  let costOf keyword = case keyword of
        Keyword.Foretell cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- The one exile CR 702.34a, CR 702.127a and CR 702.133a all print, in the same
-- words. Filter.IsSource, because the rule says "this card". `whenDestination =
-- Nothing` is the rule's "instead of putting it anywhere else" -- every
-- destination, not the graveyard alone, so Reprieve returning a flashed-back
-- spell to its owner's hand exiles it instead (Pawl.CastSpec's "CR 702.34a a
-- flashback spell bounced off the stack is exiled, not returned to hand").
--
-- GATED on the clause each of the three rules puts in front of it, and the three
-- clauses are not the same question. `castFor` is the keyword whose candidate cost
-- the cast was announced for, which rule 702.34a's "if the flashback cost was
-- paid" and rule 702.133a's jump-start clause each ask about; rule 702.127a asks
-- only whether the cast came from a graveyard. Pawl.Engine.Cast installs what this
-- returns without ever inspecting it.
castFromGraveyardReplacementsOf :: Set Keyword -> Maybe Keyword -> [ReplacementEffect (Effect.Effect Card)]
castFromGraveyardReplacementsOf keywords castFor =
  let paidFor keyword = castFor == Just keyword
   in -- The cost the cast PAID FOR, and not merely a flashback the card has:
      -- paying the printed cost under a CR 601.3 permission leaves rule 702.34a's
      -- clause unsatisfied. Compared against the cost-bearing keyword itself, and
      -- against EVERY flashback the card has, which is how one with two flashback
      -- abilities answers for the cost it was cast for rather than for the least
      -- of them.
      [castFromGraveyardExile | any (paidFor . Keyword.Flashback) (flashbackCosts keywords)]
        -- CR 702.127a's THIRD static ability, word for word CR 702.34a's second
        -- ability, so it is the same effect and not a sibling. The one of the
        -- three that does NOT read `castFor`: rule 702.127a conditions its exile
        -- on the zone alone, so an aftermath half cast from a graveyard for any
        -- cost is exiled.
        <> [castFromGraveyardExile | Set.member Keyword.Aftermath keywords]
        -- CR 702.133a's SECOND static ability, the third rule to print that
        -- sentence and so the third to share the one effect. Its "using its
        -- jump-start ability" reads the same record flashback's clause does: the
        -- jump-start candidate is the printed cost plus rule 702.133a's discard,
        -- which a permission offering the printed cost alone is not.
        <> [castFromGraveyardExile | hasJumpStart keywords, paidFor Keyword.JumpStart]

castFromGraveyardExile :: ReplacementEffect (Effect.Effect Card)
castFromGraveyardExile =
  ReplacementEffect.ZoneChangeR
    ( ZoneChangeR.MkZoneChangeR
        ZoneChangePattern.MkZoneChangePattern
          { ZoneChangePattern.whenDestination = Nothing,
            ZoneChangePattern.whoseObject = ControllerRelation.Anyones,
            ZoneChangePattern.whatObject = Filter.IsSource
          }
        Zone.Exile
    )

-- CR 702.136a: the AS-ENTERS REPLACEMENT rule 702 gives a permanent for holding
-- riot. Gathered by the PROJECTION off POST-LAYER keyword COUNTS, since rule
-- 702.136a functions on the battlefield -- so Humility takes it away and a static
-- ability granting riot adds it, both for free. The pattern is Filter.IsSource: CR
-- 614.1c's ability is the entering object's own.
--
-- ONE ROW PER INSTANCE, because CR 702.136b says each instance works separately.
-- The two rows are EQUAL VALUES, so what gives the second its own CR 614.5
-- opportunity is the instance ordinal Replacement.collect assigns;
-- Pawl.ReplacementSpec's "CR 702.136b riot twice" proves it.
--
-- "Minted" rather than "entry" because CR 702.37b's megamorph rides the same
-- function with a CR 614.1e replacement, so this answers with rows of two event
-- classes; the CR 616.1 loop matches each against the event it is offered.
mintedReplacementsOf :: Map Keyword Natural -> [ReplacementEffect (Effect.Effect Card)]
mintedReplacementsOf counts = concatMap (uncurry mintedReplacementsFor) (Map.toAscList counts)

-- Exhaustive for abilitiesFor's reason: the next keyword that rewrites an entry
-- must break this build rather than silently produce nothing.
mintedReplacementsFor :: Keyword -> Natural -> [ReplacementEffect (Effect.Effect Card)]
mintedReplacementsFor keyword count = case keyword of
  Keyword.Riot -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource EntryRewrite.Riot))
  -- CR 702.98a's FIRST static ability, riot's row with the declining half deleted.
  -- Filter.IsSource and one row per instance for riot's reasons.
  Keyword.Unleash -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource EntryRewrite.Unleash))
  -- CR 702.63a's FIRST ability, a CR 614.1c self-replacement in riot's exact
  -- position. Riot's rewrite asks a question and this one does not, so the count
  -- rides the rewrite rather than a prompt. ONE ROW PER INSTANCE, and CR 702.63c
  -- makes the counters add up, each rewrite placing its own N.
  --
  -- NO ROW AT ALL for rule 702.63b's numberless vanishing, whatever the count:
  -- that form states only the two triggers, and there is no N to enter with. The
  -- permanent's own text is what puts the time counters on (Tidewalker's "for
  -- each Island you control").
  --
  -- A row of Quantity.Literal 0 is not observable on a BOARD: Pawl.Engine.Event's
  -- WithCounters arm places nothing for a zero, so Tidewalker enters the same
  -- either way. What holds the empty list is rule 702.63b itself, fenced by
  -- Pawl.KeywordTriggerSpec's "and no entry rewrite, however many instances";
  -- a row minting any POSITIVE number is what that spec's 3/3 catches.
  Keyword.Vanishing n -> foldMap (List.genericReplicate count . ReplacementEffect.EntryR . EntryR.MkEntryR Filter.IsSource . EntryRewrite.WithCounters . WithCounters.MkWithCounters CounterKind.Time . Quantity.Literal . toInteger) n
  -- CR 702.32a's FIRST ability, vanishing's row in the fade counter. One row per
  -- instance for riot's reason, rule 702.32 stating no multiplicity clause.
  Keyword.Fading n -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Fade (Quantity.Literal (toInteger n))))))
  Keyword.Frenzy _ -> []
  -- CR 702.43a's FIRST ability, vanishing's row with a different counter kind. One
  -- row per instance, and CR 702.43b makes them add up.
  Keyword.Modular n -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.PlusOnePlusOne (Quantity.Literal (toInteger n))))))
  Keyword.Crew _ -> []
  Keyword.Fabricate _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  -- CR 702.16e's clause: "Any damage that would be dealt by sources that have
  -- the stated quality to a permanent or player with protection is prevented."
  -- Glittering Lion's printed shield with the quality written into the SOURCE
  -- half -- Filter.IsSource in the recipient position is the permanent this
  -- keyword is on (Stormwild Capridor's spelling), and the quality is matched
  -- against the damage's source at the event, which is CR 609.7b's recheck.
  --
  -- ONE ROW whatever the count, unlike riot's replicate: CR 702.16m makes a
  -- second instance of protection from the same quality redundant, and a second
  -- prevent-all row would have nothing left to prevent anyway.
  --
  -- The PERMANENT half only. Rule 702.16e's "or player" is unreachable from a
  -- keyword -- a player has no keywords, the same split
  -- Pawl.Engine.Target.targetable states for CR 702.16b.
  Keyword.Protection quality ->
    [ ReplacementEffect.DamageR
        DamageR.MkDamageR
          { DamageR.matching =
              DamagePattern.MkDamagePattern
                { DamagePattern.whichKind = Nothing,
                  DamagePattern.whatSource = quality,
                  DamagePattern.whatRecipient = Just Filter.IsSource,
                  DamagePattern.whichRecipient = Nothing,
                  DamagePattern.whichSource = Nothing
                },
            DamageR.rewrite = DamageRewrite.PreventAll,
            DamageR.riders = Seq.empty
          }
    ]
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.TrampleOverPlaneswalkers -> []
  Keyword.Vigilance -> []
  Keyword.Ward _ -> []
  Keyword.Banding -> []
  Keyword.Flanking -> []
  Keyword.Phasing -> []
  Keyword.Shadow -> []
  Keyword.Horsemanship -> []
  Keyword.Aftermath -> []
  Keyword.JumpStart -> []
  Keyword.Afflict _ -> []
  Keyword.Fear -> []
  Keyword.Intimidate -> []
  -- CR 702.37a's plain morph mints nothing: rule 702.37a is one alternative cost
  -- and one special action, and neither rewrites an event.
  Keyword.Morph (Morph.MkMorph _ MorphVariant.Plain) -> []
  -- CR 702.37b's SECOND clause, minted the way riot's is: "As this permanent is
  -- turned face up, put a +1/+1 counter on it if its megamorph cost was paid to
  -- turn it face up." Filter.IsSource, because CR 614.1e's ability is the turning
  -- permanent's own.
  --
  -- The rule's "IF ITS MEGAMORPH COST WAS PAID" is checked at the row's match,
  -- Replacement.applies: CR 701.40c gives a manifested megamorph card a second
  -- road face up at its MANA cost, and the row is refused down that one
  -- (Pawl.FaceDownSpec's Misthoof Kirin pair). morphCost answers one cost per
  -- permanent, so the rest of the condition needs no test.
  Keyword.Morph (Morph.MkMorph _ MorphVariant.Mega) ->
    List.genericReplicate count (ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR Filter.IsSource (TurnUpRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1)))))
  Keyword.Menace -> []
  Keyword.Renown _ -> []
  Keyword.Cycling {} -> []
  Keyword.Kicker _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Bushido _ -> []
  Keyword.Soulshift _ -> []
  -- CR 702.54a's ONE static ability, vanishing's row with rule 702.54a's condition
  -- on it. That condition is Replacement.admitsEntry's rather than this function's
  -- -- nothing knowable from a keyword and a count can answer it. ONE ROW PER
  -- INSTANCE (CR 702.54c), both admitted or neither.
  Keyword.Bloodthirst n -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource (EntryRewrite.Bloodthirst n)))
  Keyword.Haunt -> []
  Keyword.SplitSecond -> []
  Keyword.Poisonous _ -> []
  Keyword.Annihilator _ -> []
  Keyword.BattleCry -> []
  Keyword.Evolve -> []
  Keyword.Dethrone -> []
  Keyword.LevelUp _ -> []
  Keyword.Outlast _ -> []
  Keyword.Prowess -> []
  Keyword.Infect -> []
  Keyword.Wither -> []
  Keyword.Exalted -> []
  Keyword.Mentor -> []
  Keyword.Afterlife _ -> []
  Keyword.Provoke -> []
  Keyword.Changeling -> []
  Keyword.Reinforce {} -> []
  Keyword.Devoid -> []
  Keyword.Ingest -> []
  Keyword.Skulk -> []
  Keyword.Melee -> []
  Keyword.Rampage _ -> []
  -- CR 702.145b's FIRST static ability: "if it is night and this permanent is
  -- represented by a double-faced card, it enters transformed". Filter.IsSource
  -- for riot's reason, CR 614.1d's "[this permanent] enters" being the entering
  -- object's own ability. The rule's two conditions are asked by
  -- Replacement.applies, neither being knowable from a keyword count.
  --
  -- ONE ROW PER INSTANCE, and a second row is idempotent: the write names
  -- Card.backFace outright rather than the successor of whatever face is up now.
  Keyword.Daybound -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource EntryRewrite.EntersTransformed))
  -- CR 702.145e gives nightbound only TWO static abilities, and neither rewrites
  -- an entry: the enters-transformed half is daybound's alone.
  Keyword.Nightbound -> []
  Keyword.Decayed -> []
  -- CR 702.150a is a replacement effect and is NOT minted here: it changes
  -- how many counters CR 306.5b's intrinsic row places rather than placing
  -- its own, so Pawl.Engine.Projection.intrinsicReplacementsOf reads it off
  -- the same projection this list is minted from.
  Keyword.Compleated -> []
  -- CR 702.155b's two intrinsic abilities are NOT minted here, and CR 714.3b is
  -- why: rule 714.3b REPLACES rule 714.3a's "enters with a lore counter"
  -- ability rather than adding to it, and that one is minted from the finished
  -- projection by Pawl.Engine.Saga.entryReplacementsOf. A row minted here would
  -- sit beside it and the Saga would enter with one lore counter too many.
  --
  -- That is also where CR 702.155c is discharged: `entryReplacementsOf` keys on
  -- MEMBERSHIP of the keyword set, so the `count` this function takes cannot
  -- make a second instance mint a second row.
  Keyword.ReadAhead -> []
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Disguise _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  -- CR 702.94a's static half is a PERMISSION to reveal, not a replacement: CR
  -- 121.9's window changes nothing about the draw, so there is no event to
  -- rewrite. Pawl.Engine.Event's draw funnel asks miracleCost directly.
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  -- CR 508.1g's choice is a step of a turn-based action, and the exert itself
  -- writes Object.exertedBy directly.
  Keyword.Exert -> []
  Keyword.Persist -> []
  Keyword.Undying -> []

-- The SHORT-CIRCUIT's voice: Projection.replacementsAffecting skips the whole
-- board when no permanent's BASE card could hold a replacement effect, and a
-- minted row is not printed in a face's list, so the gate has to be told which
-- keywords mint one. Membership rather than a count, the gate asking whether
-- there is any.
mintsReplacement :: Keyword -> Bool
mintsReplacement keyword = not (null (mintedReplacementsFor keyword 1))

-- CR 508.1c / CR 509.1b: every combat restriction rule 702 gives an object for
-- holding a keyword. CombatRestriction.inForce adds these to the ones a face
-- PRINTS.
--
-- Its own mint point rather than an arm of the three above: a combat restriction
-- is not an ability object -- nothing puts it on the stack -- and rewrites no
-- event. It is a fact CR 613.11 has a reader ask about.
--
-- MEMBERSHIP and not a per-keyword count: a restriction is read by asking whether
-- the creature is in the forbidden set, so a second copy forbids nothing further.
mintedCombatRestrictionsOf :: Map Keyword Natural -> [CombatRestriction.CombatRestriction]
mintedCombatRestrictionsOf = concatMap mintedCombatRestrictionsFor . Map.keys

-- Exhaustive for `abilitiesFor`'s reason: the next keyword that forbids an attack
-- or a block must break this build rather than silently forbid nothing.
mintedCombatRestrictionsFor :: Keyword -> [CombatRestriction.CombatRestriction]
mintedCombatRestrictionsFor keyword = case keyword of
  -- CR 702.98a's SECOND static ability: "This permanent can't block as long as it
  -- has a +1/+1 counter on it."
  --
  -- The counter clause rides the AFFECTED SET rather than the `unless` gate beside
  -- it, the two having opposite polarity: CR 509.1b's gate is the condition a
  -- creature can't block UNLESS, and this is a condition it can't block WHILE. An
  -- affected set is re-derived every read, which is also rule 702.98a's "as long
  -- as".
  --
  -- ANY +1/+1 counter, not the one unleash's own entry replacement may have
  -- placed: rule 702.98a says "a +1/+1 counter".
  Keyword.Unleash ->
    [ CombatRestriction.CantBlock
        AffectedUnless.MkAffectedUnless
          { AffectedUnless.affected = Affected.Matching (Filter.And [Filter.IsSource, Filter.HasCounters CounterKind.PlusOnePlusOne]),
            AffectedUnless.unless = Nothing
          }
    ]
  Keyword.Riot -> []
  Keyword.Vanishing _ -> []
  Keyword.Fading _ -> []
  Keyword.Frenzy _ -> []
  Keyword.Modular _ -> []
  Keyword.Crew _ -> []
  Keyword.Fabricate _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof _ -> []
  Keyword.Indestructible -> []
  Keyword.Landwalk _ -> []
  Keyword.Lifelink -> []
  -- CR 702.16f: "Attacking creatures with protection can't be blocked by
  -- creatures that have the stated quality." The one clause of rule 702.16 that
  -- is already a printed shape -- Questing Beast's row with the quality in the
  -- blocker position and Filter.IsSource naming the attacker.
  --
  -- The rule restricts an ATTACKING creature only, which needs no conjunct here:
  -- Pawl.Engine.CombatRestriction.cantBeBlockedBy is asked about attacker/blocker
  -- pairs, so a creature that is not attacking is in no pair.
  --
  -- Ungated (Nothing), rule 702.16f stating no condition, and membership rather
  -- than a count for the type's own reason.
  Keyword.Protection quality ->
    [ CombatRestriction.CantBeBlockedBy
        CantBeBlockedBy.MkCantBeBlockedBy
          { CantBeBlockedBy.affected = Affected.Matching Filter.IsSource,
            CantBeBlockedBy.blockers = quality,
            CantBeBlockedBy.unless = Nothing
          }
    ]
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.TrampleOverPlaneswalkers -> []
  Keyword.Vigilance -> []
  Keyword.Ward _ -> []
  Keyword.Banding -> []
  Keyword.Flanking -> []
  Keyword.Phasing -> []
  Keyword.Shadow -> []
  Keyword.Horsemanship -> []
  Keyword.Aftermath -> []
  Keyword.JumpStart -> []
  Keyword.Afflict _ -> []
  Keyword.Fear -> []
  Keyword.Intimidate -> []
  Keyword.Morph {} -> []
  Keyword.Menace -> []
  Keyword.Renown _ -> []
  Keyword.Cycling {} -> []
  Keyword.Kicker _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Bushido _ -> []
  Keyword.Soulshift _ -> []
  Keyword.Bloodthirst _ -> []
  Keyword.Haunt -> []
  Keyword.SplitSecond -> []
  Keyword.Poisonous _ -> []
  Keyword.Annihilator _ -> []
  Keyword.BattleCry -> []
  Keyword.Evolve -> []
  Keyword.Dethrone -> []
  Keyword.LevelUp _ -> []
  Keyword.Outlast _ -> []
  Keyword.Prowess -> []
  Keyword.Infect -> []
  Keyword.Wither -> []
  Keyword.Exalted -> []
  Keyword.Mentor -> []
  Keyword.Afterlife _ -> []
  Keyword.Provoke -> []
  Keyword.Devoid -> []
  Keyword.Ingest -> []
  Keyword.Skulk -> []
  Keyword.Melee -> []
  Keyword.Rampage _ -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  -- CR 702.147a's static half: "This creature can't block." Unleash's row with the
  -- counter clause removed, rule 702.147a stating the restriction flat.
  Keyword.Decayed -> [CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless (Affected.Matching Filter.IsSource) Nothing)]
  Keyword.Compleated -> []
  Keyword.ReadAhead -> []
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Disguise _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  -- CR 701.43d's optional COST to attack never makes an attack illegal: the active
  -- player may always decline it (CR 508.1g).
  Keyword.Exert -> []
  Keyword.Persist -> []
  Keyword.Undying -> []
  Keyword.Changeling -> []
  Keyword.Reinforce {} -> []

-- `mintsReplacement`'s twin, and read by the same kind of short-circuit:
-- Pawl.Engine.CombatRestriction.inForce projects a permanent only when some base
-- face on the battlefield could put a restriction-minting keyword on one.
mintsCombatRestriction :: Keyword -> Bool
mintsCombatRestriction = not . null . mintedCombatRestrictionsFor

-- CR 702: WHICH RULE MINTED this activated ability of an object whose keywords
-- are `counts`, as a family designator -- the classification
-- Pawl.Types.ReduceActivationCost.grantedBy compares, so that Fluctuator's
-- "cycling abilities you activate" reaches rule 702.29a's minted ability and
-- nothing else (#1431). Nothing is "no keyword of this object minted it", which
-- is every ability the card itself prints.
--
-- Answered by re-minting rather than by a provenance field on the ability:
-- handAbilitiesOf and battlefieldAbilitiesOf both throw the keyword away, and an
-- ActivatedAbility that carried its own origin would put an engine-only fact on
-- the wire where a card could author a lie. Value equality is the idiom
-- Pawl.Types.ActivatedAbility's header already fixes for this type -- "carries
-- the value and validates by membership, never an index".
--
-- BOTH minters are asked, so that this is one question rather than one per zone:
-- the caller supplies whichever keyword map its zone calls for, and the two are
-- disjoint by construction -- no keyword has a non-empty arm in both
-- handAbilitiesFor and battlefieldAbilitiesFor -- so asking both cannot answer
-- twice for one keyword.
--
-- The five minting keywords -- cycling, reinforce, crew, level up, outlast -- all
-- carry a payload, so familyOf answers Just for every one of them and its Nothing
-- is unreachable from here. Its Nothing is still let through rather than made an
-- error: a nullary keyword that minted an activated ability would have no family
-- to name, and CR 702 would have to grow one first.
--
-- Not implemented: telling a keyword-minted ability from a PRINTED ability that
-- happens to be byte-identical to one -- a card printing "{2}, Discard this card:
-- Draw a card" functioning from a hand would be read as cycling here. No printing
-- in data/cards/ is such a twin (gap #2072).
familyGranting :: Map Keyword Natural -> ActivatedAbility Card -> Maybe KeywordFamily.KeywordFamily
familyGranting counts ability =
  Maybe.listToMaybe
    ( Maybe.mapMaybe
        ( \(keyword, count) ->
            if elem ability (handAbilitiesFor keyword <> battlefieldAbilitiesFor keyword count)
              then familyOf keyword
              else Nothing
        )
        (Map.toAscList counts)
    )

-- CR 702: WHICH KEYWORD this is, with its payload dropped -- the classification
-- Filter.HasKeywordFamily matches on, so that Flensing Raptor's "creature you
-- control with toxic" reaches toxic 1 and toxic 3 alike (CR 702.164a).
--
-- Nothing for a NULLARY keyword, and not because the answer is unknown: it has no
-- payload to drop, so Filter.HasKeyword already asks its family question exactly.
-- KeywordFamily has no constructor for one, which keeps "a creature with flying"
-- from having two spellings.
--
-- EXHAUSTIVE, with no wildcard: adding a Keyword constructor must fail to compile
-- until its family is decided, where a wildcard would silently answer Nothing for
-- the next parameterized keyword.
familyOf :: Keyword -> Maybe KeywordFamily.KeywordFamily
familyOf keyword = case keyword of
  Keyword.Hexproof _ -> Just KeywordFamily.Hexproof
  Keyword.Landwalk _ -> Just KeywordFamily.Landwalk
  Keyword.Cycling {} -> Just KeywordFamily.Cycling
  Keyword.Kicker _ -> Just KeywordFamily.Kicker
  Keyword.Flashback _ -> Just KeywordFamily.Flashback
  Keyword.Morph {} -> Just KeywordFamily.Morph
  Keyword.Entwine _ -> Just KeywordFamily.Entwine
  Keyword.Bushido _ -> Just KeywordFamily.Bushido
  Keyword.Soulshift _ -> Just KeywordFamily.Soulshift
  Keyword.Bloodthirst _ -> Just KeywordFamily.Bloodthirst
  Keyword.Reinforce {} -> Just KeywordFamily.Reinforce
  Keyword.Modular _ -> Just KeywordFamily.Modular
  Keyword.Vanishing _ -> Just KeywordFamily.Vanishing
  Keyword.Fading _ -> Just KeywordFamily.Fading
  Keyword.Frenzy _ -> Just KeywordFamily.Frenzy
  Keyword.Poisonous _ -> Just KeywordFamily.Poisonous
  Keyword.Annihilator _ -> Just KeywordFamily.Annihilator
  Keyword.Crew _ -> Just KeywordFamily.Crew
  Keyword.Fabricate _ -> Just KeywordFamily.Fabricate
  Keyword.Rampage _ -> Just KeywordFamily.Rampage
  Keyword.Afflict _ -> Just KeywordFamily.Afflict
  Keyword.Toxic _ -> Just KeywordFamily.Toxic
  Keyword.Disguise _ -> Just KeywordFamily.Disguise
  Keyword.Plot _ -> Just KeywordFamily.Plot
  Keyword.Foretell _ -> Just KeywordFamily.Foretell
  -- CR 702.94a's parameterized keyword: "a card with miracle" drops the cost.
  Keyword.Miracle _ -> Just KeywordFamily.Miracle
  -- CR 702.16a's parameterized keyword: "a creature with protection" drops the
  -- stated quality.
  Keyword.Protection _ -> Just KeywordFamily.Protection
  Keyword.Deathtouch -> Nothing
  Keyword.Defender -> Nothing
  Keyword.DoubleStrike -> Nothing
  Keyword.FirstStrike -> Nothing
  Keyword.Flash -> Nothing
  Keyword.Flying -> Nothing
  Keyword.Haste -> Nothing
  Keyword.Indestructible -> Nothing
  Keyword.Lifelink -> Nothing
  Keyword.Reach -> Nothing
  Keyword.Shroud -> Nothing
  Keyword.Trample -> Nothing
  Keyword.TrampleOverPlaneswalkers -> Nothing
  Keyword.Vigilance -> Nothing
  Keyword.Ward _ -> Just KeywordFamily.Ward
  Keyword.Banding -> Nothing
  Keyword.Flanking -> Nothing
  Keyword.Haunt -> Nothing
  Keyword.Phasing -> Nothing
  Keyword.Shadow -> Nothing
  Keyword.Horsemanship -> Nothing
  Keyword.Fear -> Nothing
  Keyword.Intimidate -> Nothing
  Keyword.Infect -> Nothing
  Keyword.Wither -> Nothing
  Keyword.Exalted -> Nothing
  Keyword.Mentor -> Nothing
  Keyword.Afterlife _ -> Just KeywordFamily.Afterlife
  Keyword.Provoke -> Nothing
  Keyword.BattleCry -> Nothing
  Keyword.Evolve -> Nothing
  Keyword.Dethrone -> Nothing
  Keyword.LevelUp _ -> Just KeywordFamily.LevelUp
  Keyword.Outlast _ -> Just KeywordFamily.Outlast
  Keyword.Prowess -> Nothing
  Keyword.Menace -> Nothing
  Keyword.Renown _ -> Just KeywordFamily.Renown
  Keyword.Changeling -> Nothing
  Keyword.SplitSecond -> Nothing
  Keyword.Devoid -> Nothing
  Keyword.Ingest -> Nothing
  Keyword.Skulk -> Nothing
  Keyword.Melee -> Nothing
  Keyword.Aftermath -> Nothing
  Keyword.JumpStart -> Nothing
  Keyword.Riot -> Nothing
  Keyword.Unleash -> Nothing
  Keyword.Daybound -> Nothing
  Keyword.Nightbound -> Nothing
  Keyword.Decayed -> Nothing
  Keyword.Compleated -> Nothing
  Keyword.ReadAhead -> Nothing
  Keyword.Training -> Nothing
  Keyword.StartYourEngines -> Nothing
  Keyword.Exert -> Nothing
  Keyword.Persist -> Nothing
  Keyword.Undying -> Nothing

-- CR 702.70a: a creature with poisonous N gives a player it deals combat damage
-- to that many poison counters.
--
-- "That player" is the player the trigger's own event named, which
-- Event.eventBindings stamps under the reserved Binding.triggerPlayer slot -- so
-- the payload is an ordinary slot read and needs no opcode. NOT the ability's
-- controller: CR 603.3a makes that the creature's controller, and the poison goes
-- to their victim.
poisonous :: Natural -> TriggeredAbility Card
poisonous n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.GainPlayerCounters
        ( PlayerCounters.MkPlayerCounters
            (PlayerRef.InSlot Binding.triggerPlayer)
            PlayerCounterKind.Poison
            (Quantity.Literal (toInteger n))
        )

-- CR 702.115a: poisonous' condition and poisonous' "that player" -- the same
-- Binding.triggerPlayer slot -- over a different payload.
--
-- The payload is a zone move rather than a mint of its own: ObjectRef.TopOfLibrary
-- carries WHOSE library, so the whole sentence is one MoveToZone. An empty library
-- exiles nothing, which is what rule 702.115a's silence about a shortfall asks
-- for.
--
-- Face up, CR 406.3's default with no exception stated. The EntryRiders and the
-- LibraryPlacement are inert for an exile destination, no slot is bound since
-- nothing later reads what arrived, and the origin zone is Nothing because
-- TopOfLibrary can only name a card already in that library.
ingest :: TriggeredAbility Card
ingest =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.MoveToZone
        ( MoveToZone.MkMoveToZone
            (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Literal 1)))
            Zone.Exile
            EntryRiders.MkEntryRiders
              { EntryRiders.tapped = TapState.Untapped,
                EntryRiders.attacking = False,
                EntryRiders.blocking = Nothing,
                EntryRiders.transformed = False,
                EntryRiders.counters = Map.empty,
                EntryRiders.underOwner = False,
                EntryRiders.exiledFaceDown = False,
                EntryRiders.faceDown = Nothing
              }
            Nothing
            Nothing
            LibraryPlacement.defaultValue
        )

-- CR 702.86a. CR 508.3a is what "attacks" means -- being declared as an attacker
-- -- so the condition is battle cry's SelfAttacks EveryTime.
--
-- "DEFENDING PLAYER" is CR 508.5's, stamped onto GameEvent.AttackerDeclared at the
-- declaration and read back into Binding.triggerPlayer, poisonous' "that player".
-- NOT the ability's controller (CR 603.3a).
--
-- The sacrifice is CR 701.21a's edict, so the SACRIFICING PLAYER chooses which
-- permanents go. The Filter is the empty conjunction: rule 702.86a says "N
-- permanents" with no qualification.
annihilator :: Natural -> TriggeredAbility Card
annihilator n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.PlayerSacrifices
        ( PlayerSacrifices.MkPlayerSacrifices
            Binding.triggerPlayer
            (Filter.And [])
            (Quantity.Literal (toInteger n))
        )

-- CR 702.91a, on annihilator's SelfAttacks EveryTime condition.
--
-- "EACH OTHER ATTACKING CREATURE" is a SET, swept at resolution and then frozen
-- (CR 611.2c), so an ObjectRef.EachMatching. The three conjuncts are the three
-- printed words; OTHER is `Not IsSource`, which is why a battle-crying creature
-- never pumps itself, and CR 611.2c fixing the set as the effect begins is why a
-- token that arrives afterwards is not pumped.
battleCry :: TriggeredAbility Card
battleCry =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 0)))
            ( ObjectRef.EachMatching
                (Filter.And [Filter.HasCardType CardType.Creature, Filter.IsAttacking, Filter.Not Filter.IsSource])
            )
        )

-- CR 702.100a. The bearer is NOT excluded from the condition's Filter, which is
-- the rule rather
-- than an omission: a creature entering compares itself against itself, which no
-- comparison below can answer true.
-- The comparison rides the intervening "if" (CR 603.4) and not the Filter, where
-- training's lives, because rule 702.100a prints "if": CR 608.2a re-checks it as
-- the ability resolves, so pumping the bearer in response takes the counter away.
-- It reaches the entrant -- neither the bearer nor a target -- through
-- Quantity.AgainstSlot at Binding.became, and through CR 608.2h, so one killed
-- while the trigger waits is compared at its last known power and toughness.
--
-- Condition.Any because rule 702.100a's "and/or" compares two DIFFERENT
-- characteristics. STRICTLY greater, spelled as "at least one more" since CR
-- 208.1's power and toughness are whole numbers and Comparison has no strict arm.
-- CR 702.100c then falls out: a permanent that is not a creature has no power or
-- toughness (CR 208.3), and Condition.holds reads an unanswerable side as False.
--
-- Effect.Evolve rather than Effect.PutCounters, which is rule 702.100b: one opcode
-- is what ties the "evolves" marker to the placement. Renegade Krasis reads it.
evolve :: TriggeredAbility Card
evolve =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition =
        TriggerCondition.PermanentEnters
          (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You]),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Just (Condition.Any [entrantExceeds Quantity.Power, entrantExceeds Quantity.Toughness]),
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.Evolve Binding.triggerSource
    entrantExceeds quantity =
      Condition.Compares
        ( Compares.MkCompares
            (Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot Binding.became quantity))
            Comparison.AtLeast
            (Quantity.Plus (Plus.MkPlus quantity (Quantity.Literal 1)))
        )

-- CR 702.108a: whenever you cast a noncreature spell, this creature gets +1/+1
-- until end of turn.
--
-- The first minted trigger whose event is NOT its bearer's combat: CR 601.2i is
-- the event, so the condition is TriggerCondition.SpellCast. "You cast" is
-- Filter.ControlledBy You against CR 109.5's "you", the ability's controller (CR
-- 603.3a); "a noncreature spell" is Filter.Not of the card type, the printed word
-- rather than a disjunction of the other types. TurnScope.EachTurn because rule
-- 702.108a names no turn.
--
-- "THIS CREATURE" is the bearer, so the payload is battle cry's with
-- Filter.IsSource rather than its negation.
prowess :: TriggeredAbility Card
prowess =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition =
        TriggerCondition.SpellCast
          SpellCast.MkSpellCast
            { SpellCast.filter = Filter.And [Filter.ControlledBy PlayerRelation.You, Filter.Not (Filter.HasCardType CardType.Creature)],
              SpellCast.scope = TurnScope.EachTurn,
              -- CR 702.108a names no zone: prowess triggers on a noncreature
              -- spell cast from anywhere.
              SpellCast.zone = Nothing,
              -- And no ordinal either: every noncreature spell cast fires it,
              -- not one chosen occurrence of the turn.
              SpellCast.ordinal = Nothing
            },
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))
            (ObjectRef.EachMatching Filter.IsSource)
        )

-- CR 702.121a, on battle cry's SelfAttacks condition with prowess' payload.
-- Attacking a PLANESWALKER fires it just the same -- CR 508.1a chooses the
-- attackers and CR 508.1b only then says what each attacks -- so what the
-- planeswalker changes is the bonus.
--
-- The BONUS is the one payload here that is not a literal:
-- Quantity.OpponentsAttacked reads CR 508.3b's record against CR 109.5's "you".
-- Zero is an ordinary answer. CR 611.2d freezes it as this resolves, which the
-- printed duration needs: CR 511.3 clears the record at end of combat, so a live
-- re-read would shrink the pump to 0 the moment combat ended.
melee :: TriggeredAbility Card
melee =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    bonus = Quantity.OpponentsAttacked (PlayerRef.Relative PlayerRelation.You)
    effect =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness bonus bonus))
            (ObjectRef.EachMatching Filter.IsSource)
        )

-- CR 702.23a. The condition is bushido's blocked half, CR 509.3c, which fires ONCE
-- however many creatures blocked, where flanking's CR 509.3d fires once per
-- blocker. Rule 702.23a's bonus already counts the blockers itself, so a
-- per-blocker trigger would count them twice.
--
-- The BONUS is N COPIES of Quantity.BlockersBeyondFirst summed through
-- Quantity.Plus, Pawl.Types.Quantity having no product node; the fold's Literal 0
-- base answers rampage 0 rather than failing.
--
-- CR 702.23b's "calculated only once per combat" is CR 611.2d's freeze and needs
-- nothing of its own.
rampage :: Natural -> TriggeredAbility Card
rampage n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfBecomesBlocked,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    bonus = foldr (\a b -> Quantity.Plus (Plus.MkPlus a b)) (Quantity.Literal 0) (List.genericReplicate n Quantity.BlockersBeyondFirst)
    effect =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness bonus bonus))
            (ObjectRef.EachMatching Filter.IsSource)
        )

-- CR 702.25a. CR 509.3d is the event -- "becomes blocked by a creature", once for
-- each creature that blocks -- and NOT CR 509.3c's "becomes blocked", which fires
-- once however many blockers there are. Two blockers on one flanker is two
-- triggers and two -1/-1s; bushido below reads that other, grouped event.
--
-- "WITHOUT FLANKING" rides the condition rather than the payload, which is rule
-- 509.3f: a blocker's characteristics are checked as it becomes a blocking
-- creature, so a creature that gains flanking afterwards is still pumped down.
--
-- "THE BLOCKING CREATURE" is the object the event named, bound under
-- Binding.blockingCreature -- an ordinary slot read, and NOT a set sweep over
-- whoever is blocking at resolution.
flanking :: TriggeredAbility Card
flanking =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition =
        TriggerCondition.SelfBecomesBlockedBy
          (Filter.And [Filter.HasCardType CardType.Creature, Filter.Not (Filter.HasKeyword Keyword.Flanking)]),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton flankingEffect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

-- The -1/-1 rule 702.25a hands the blocker, read out of the slot CR 509.3d bound.
flankingEffect :: Effect.Effect Card
flankingEffect =
  Effect.ModifyTarget
    ( ModifyTarget.MkModifyTarget
        Duration.UntilEndOfTurn
        (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal (-1)) (Quantity.Literal (-1))))
        (ObjectRef.InSlot Binding.blockingCreature)
    )

-- CR 702.83a, the one minted ability borne by a BYSTANDER on both sides at once:
-- the condition watches
-- somebody else's declaration AND the payload pumps somebody else, so the bearer
-- appears in neither. It supplies only CR 109.5's "you" -- the ability's
-- controller (CR 603.3a) -- and the Filter context's source.
--
-- ALONE is TriggerCondition.CreatureAttacksAlone's own, not a Filter conjunct: CR
-- 506.5 makes it a fact about the declaration rather than a characteristic.
--
-- "THAT CREATURE" is the creature the event named, read out of
-- Binding.attackingCreature -- NOT Filter.IsSource, which would pump the wrong
-- permanent whenever a card other than the exalted bearer attacks.
exalted :: TriggeredAbility Card
exalted =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition =
        TriggerCondition.CreatureAttacksAlone
          (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You]),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))
            (ObjectRef.InSlot Binding.attackingCreature)
        )

-- CR 702.45a, the one keyword whose sentence names TWO events: "blocks" is CR
-- 509.3a and "becomes blocked" is CR 509.3c.
--
-- So this returns a LIST of two abilities where its siblings return one. The
-- alternative -- one TriggeredAbility with a disjunctive condition -- would need a
-- TriggerCondition combinator nothing else in rule 702 wants, and the split costs
-- nothing: CR 603.2 triggers once per occurrence, so both spellings put the same
-- number of objects on the stack.
--
-- The payload is prowess' with N in place of its 1, and both abilities carry it,
-- rule 702.45a stating one.
bushido :: Natural -> [TriggeredAbility Card]
bushido n = [bushidoBlocks n, bushidoBecomesBlocked n]

-- CR 509.3a's half of rule 702.45a.
bushidoBlocks :: Natural -> TriggeredAbility Card
bushidoBlocks = bushidoHalf TriggerCondition.SelfBlocks

-- CR 509.3c's half of rule 702.45a.
bushidoBecomesBlocked :: Natural -> TriggeredAbility Card
bushidoBecomesBlocked = bushidoHalf TriggerCondition.SelfBecomesBlocked

-- The +N/+N the two halves share.
bushidoHalf :: TriggerCondition.TriggerCondition -> Natural -> TriggeredAbility Card
bushidoHalf condition n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal (toInteger n)) (Quantity.Literal (toInteger n))))
            (ObjectRef.EachMatching Filter.IsSource)
        )

-- CR 702.68a: bushidoHalf with the toughness bonus zeroed.
--
-- The BONUS is a continuous effect from a RESOLVING ability (CR 611.2): it
-- modifies power without setting it, so CR 613.4c's layer 7c applies it, and CR
-- 611.2a is the duration.
--
-- The condition is TriggerCondition.SelfAttacksUnblocked, which the glossary's
-- "attacks and isn't blocked" entry sends to CR 509.1h -- so the bonus lands in
-- the declare blockers step, after the declaration, rather than with CR 508.2's
-- attack triggers. Rule 509.1h's last sentence keeps a creature whose only blocker
-- left combat from getting it.
frenzy :: Natural -> TriggeredAbility Card
frenzy n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacksUnblocked,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.ModifyTarget
        ( ModifyTarget.MkModifyTarget
            Duration.UntilEndOfTurn
            (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal (toInteger n)) (Quantity.Literal 0)))
            (ObjectRef.EachMatching Filter.IsSource)
        )

-- CR 702.130a: bushido's CR 509.3c condition and annihilator's CR 508.5 player,
-- read off GameEvent.AttackerBlocked through Binding.triggerPlayer. NOT the
-- ability's controller: CR 603.3a makes that the ATTACKING creature's controller,
-- and the life leaves whom they attacked.
--
-- Effect.LoseLife and not damage: rule 702.130a says "loses N life", so this is CR
-- 119.3's life loss and none of CR 120's damage machinery sees it.
afflict :: Natural -> TriggeredAbility Card
afflict n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfBecomesBlocked,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.LoseLife
        ( PlayerQuantity.MkPlayerQuantity
            (PlayerRef.InSlot Binding.triggerPlayer)
            (Quantity.Literal (toInteger n))
        )

-- CR 702.134a, the first minted ability that TARGETS. A REAL choice, since with
-- two smaller attackers the rules leave which one open.
--
-- Filter.PowerLessThanSource compares against the SOURCE, which is why that atom
-- carries no literal, and is strict, which is what excludes the BEARER with no
-- `Not IsSource`.
--
-- Effect.Mentor and not Effect.PutCounters, for evolve's reason one rule over: CR
-- 702.134c makes "a creature mentors another creature" a trigger event, so the
-- placement has to be distinguishable from every other +1/+1 counter. The counter
-- still goes through Event.putCounters, so CR 122.6's funnel is unaffected.
mentor :: TriggeredAbility Card
mentor =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) (Map.singleton mentorTarget slot)))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    slot = TargetSlot.required Pool.Creatures (Just (Filter.And [Filter.IsAttacking, Filter.PowerLessThanSource]))
    effect = Effect.Mentor mentorTarget

-- The slot rule 702.134a's one target is chosen into. Named here rather than in
-- Pawl.Engine.Binding, which holds the RESERVED names a card may not use: this is
-- an ordinary target slot, declared by the ability that reads it.
mentorTarget :: SlotName.SlotName
mentorTarget = SlotName.MkSlotName (Text.pack "mentored")

-- CR 702.149a: mentor's clause with the comparison reversed and the target
-- dropped, rule 702.149a pumping the BEARER, so there is nothing to choose.
--
-- The comparison therefore rides the CONDITION. CR 702.149a's companion is part of
-- the trigger event, not an intervening-if clause, so it is checked once as the
-- attackers are declared and never again on resolution -- a bigger co-attacker
-- that dies in response still leaves the counter. "Other" is the condition's own,
-- an identity check the Filter has no atom for.
--
-- Through Effect.Train, evolve's opcode one rule over: rule 702.149c makes "when
-- this creature trains" mean the placement, so it has to be distinguishable from
-- any other +1/+1 counter arriving.
training :: TriggeredAbility Card
training =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition =
        TriggerCondition.SelfAttacksWithAnother
          (Filter.And [Filter.HasCardType CardType.Creature, Filter.PowerGreaterThanSource]),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.Train Binding.triggerSource

-- CR 702.21a: ward [cost].
--
-- ONE CLAUSE and no branching opcode, fabricate's shape: CR 118.12a rewrites "[do
-- something] unless [a player does something else]" as an offer followed by the
-- thing, so the Counter is the clause's "if they don't" branch and the PayGate is
-- the offer, paid at RESOLUTION (CR 118.12). Optionality.Mandatory, since that
-- offer IS the only choice rule 702.21a gives.
--
-- THE PAYER IS THE TARGETER'S CONTROLLER, not the bearer's, so the same
-- Binding.targetingObject slot answers both halves of the sentence; Binding.you
-- would offer the cost to the wrong player. That slot is NOT a target slot: rule
-- 702.21a targets nothing, so nothing here is re-checked at CR 608.2b and a
-- shroud-bearing spell is countered as readily as any other.
ward :: Cost Keyword -> TriggeredAbility Card
ward cost =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfBecomesTargeted PlayerRelation.Opponent,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton clause) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    clause = Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory (Just gate) (Seq.singleton effect)
    -- PayObligation.Optional and no offeredAt: rule 702.21a's "unless that
    -- player pays" is CR 118.12a's "may", and one clause makes its own offer.
    gate =
      PayGate.MkPayGate
        { PayGate.payer = PlayerRef.ControllerOfBound Binding.targetingObject,
          PayGate.cost = cost,
          PayGate.branch = PayBranch.IfNotPaid,
          PayGate.obligation = PayObligation.Optional,
          PayGate.offeredAt = Nothing
        }
    effect = Effect.Counter (Counter.MkCounter (ObjectRef.InSlot Binding.targetingObject) Nothing)

-- CR 702.147a's TRIGGERED half. CR 508.3a is what "attacks" means, so the
-- condition is mentor's and provoke's SelfAttacks EveryTime.
--
-- The payload is not a sacrifice. "At end of combat" makes the sacrifice a CR
-- 603.7 DELAYED triggered ability, created as this one resolves, so the effect is
-- the arming opcode and `decayedSacrifice` is what it arms.
--
-- Onset.Immediately with no stated duration -- CR 603.7a's floor and CR 603.7b's
-- default. Rule 702.147a gates neither end of the envelope, so the delayed
-- ability watches from the moment it is armed and fires once.
decayed :: TriggeredAbility Card
decayed =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.ArmDelayedTrigger
        ArmDelayedTrigger.MkArmDelayedTrigger
          { ArmDelayedTrigger.name = decayedSacrificeName,
            ArmDelayedTrigger.onset = Onset.Immediately,
            ArmDelayedTrigger.duration = Nothing
          }

-- CR 603.7: the delayed triggered abilities RULE 702 declares, keyed by the name
-- its own arming opcode names. Pawl.Engine.Resolve falls back to this map when a
-- name is on no face's Face.delayedAbilities, a keyword having no card text to
-- declare the far end in.
--
-- Read by NAME and never off the board, which is what CR 603.7 asks for: a source
-- that has since lost the keyword -- or left the battlefield -- still sacrifices.
-- NOT a nested ability inside the opcode: Pawl.Types.Effect is first-order and
-- non-recursive on purpose.
--
-- What no type enforces is a LATER keyword whose arm is written and whose row here
-- is forgotten -- a dangling name is a silent no-op. Pawl.CardSpec closes the
-- other direction, so no card's declaration can shadow a row here.
mintedDelayedAbilities :: Map AbilityName (TriggeredAbility Card)
mintedDelayedAbilities = Map.singleton decayedSacrificeName decayedSacrifice

-- The lookup Pawl.Engine.Resolve does, which learns only that rule 702 declared
-- an ability under this name and never which keyword did.
mintedDelayedAbility :: AbilityName -> Maybe (TriggeredAbility Card)
mintedDelayedAbility name = Map.lookup name mintedDelayedAbilities

-- The name rule 702.147a's delayed ability is filed under. A card may not declare
-- one under this name (Pawl.CardSpec), which is what makes the fallback order in
-- Pawl.Engine.Resolve immaterial.
decayedSacrificeName :: AbilityName
decayedSacrificeName = AbilityName.MkAbilityName (Text.pack "decayed")

-- CR 702.147a's "sacrifice it at end of combat", as CR 511.2 states the timing:
-- abilities that trigger "at end of combat" trigger as the end of combat step
-- begins. TurnScope.EachTurn, rule 702.147a naming no player's turn.
--
-- Effect.Sacrifice against Binding.triggerSource: rule 702.147a says "it", and CR
-- 603.7c makes that the environment captured as the ability was armed rather than
-- a fresh read. CR 701.21a keeps it a sacrifice and not a destruction, so an
-- indestructible attacker still goes.
decayedSacrifice :: TriggeredAbility Card
decayedSacrifice =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Combat CombatStep.EndOfCombat) TurnScope.EachTurn),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.Sacrifice Binding.triggerSource

-- CR 702.39a's provoke, the first minted payload that creates a CR 509.1c blocking
-- REQUIREMENT rather than changing a characteristic.
--
-- The target slot is narrowed by Filter.ControlledByDefendingPlayer (CR 508.5),
-- one atom rather than ControlledBy Opponent, which CR 506.2a makes too wide: at
-- three seats only one opponent is the defending player (CR 508.5a).
--
-- ONE clause holding BOTH effects, under one Optionality.Optional -- rule 702.39a
-- prints one "may", and its "if you do" makes the untap conditional on the same
-- answer (CR 608.2e). The requirement's ATTACKER is Binding.triggerSource and
-- never a target (CR 115.10a).
provoke :: TriggeredAbility Card
provoke =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing (Optionality.Optional (PlayerRef.Relative PlayerRelation.You)) Nothing (Seq.fromList [requirement, untap]))) (Map.singleton provokeTarget slot)))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    slot = TargetSlot.required Pool.Creatures (Just Filter.ControlledByDefendingPlayer)
    requirement =
      Effect.RequireBlock
        ( RequireBlock.MkRequireBlock
            Duration.UntilEndOfCombat
            (ObjectRef.InSlot provokeTarget)
            (ObjectRef.InSlot Binding.triggerSource)
        )
    untap = Effect.Untap (ObjectRef.InSlot provokeTarget)

-- The slot rule 702.39a's one target is chosen into, mentorTarget's position.
provokeTarget :: SlotName.SlotName
provokeTarget = SlotName.MkSlotName (Text.pack "provoked")

-- CR 702.112a: poisonous' condition with a plain placement onto
-- Binding.triggerSource. No
-- marking opcode, unlike training and evolve one rule apiece away: rule 702.112a's
-- own marker is the DESIGNATION the next clause gives.
--
-- THE INTERVENING "IF" is what this row adds. CR 603.4 checks it as the ability
-- would trigger AND CR 608.2a again as it resolves, which is what CR 702.112c
-- leans on: with two instances the first to resolve designates the creature, and
-- the second finds it renowned and is removed from the stack. A printed "may"/"if"
-- clause (CR 608.2e) would check only on resolution.
--
-- ONE clause holding BOTH effects, the rule printing one sentence.
renown :: Natural -> TriggeredAbility Card
renown n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [grow, designate]))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening =
        Just (Condition.Compares (Compares.MkCompares (Quantity.HasDesignation Designation.Renowned) Comparison.AtMost (Quantity.Literal 0))),
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    grow = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal (toInteger n)) (ObjectRef.InSlot Binding.triggerSource))
    designate = Effect.Designate (Designate.MkDesignate Designation.Renowned Binding.triggerSource)

-- CR 702.105a. The whole of the keyword is in the CONDITION,
-- TriggerCondition.SelfAttacksPlayerWithMostLife, which is why the payload is
-- renown's first effect with no second. NOT an intervening "if" (CR 603.4), which
-- is where renown puts its comparison:
-- rule 702.105a prints no "if", and CR 608.2a would re-check one on resolution --
-- so an opponent gaining life in response would wrongly remove the ability from
-- the stack.
dethrone :: TriggeredAbility Card
dethrone =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacksPlayerWithMostLife,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton grow))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    grow = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.InSlot Binding.triggerSource))

-- CR 702.79a: persist. Its event is CR 700.4's dies, which is why the condition
-- below is SelfDies.
persist :: TriggeredAbility Card
persist = returns CounterKind.MinusOneMinusOne

-- CR 702.93a: undying, persist's mirror in +1/+1 counters.
undying :: TriggeredAbility Card
undying = returns CounterKind.PlusOnePlusOne

-- The sentence both keywords state, in the counter kind that tells them apart: it
-- decides which counter the permanent comes back with AND which one the "if"
-- clause looks for.
--
-- TWO INCARNATIONS, and the split is why this works: CR 400.7 mints a fresh object
-- when the permanent dies, so the ability's SOURCE (CR 113.7a) and the CARD IT
-- MOVES are different ids. CR 603.4's intervening "if" -- not a Clause condition,
-- the ability having to not trigger at all -- is evaluated against the source
-- through CR 608.2h last known information, what "it HAD no counters on it" asks
-- for, while the move names Binding.became. Endless Cockroaches proves the second
-- half and Promising Duskmage the first.
--
-- The counter rides the ENTRY (CR 122.6a) rather than following as a second
-- effect, so the permanent is never on the battlefield without it -- for persist,
-- the difference between a 2/2 coming back as a 1/1 and one that briefly was not.
--
-- `underOwner` is rule 702.79a's "under its owner's control", which CR 110.2a
-- otherwise answers with the ability's controller: a permanent stolen at layer 2
-- dies under the thief's control and still comes back to its owner.
returns :: CounterKind.CounterKind Keyword.Keyword -> TriggeredAbility Card
returns kind =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton back))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening =
        Just (Condition.Compares (Compares.MkCompares (Quantity.ObjectCounters kind) Comparison.AtMost (Quantity.Literal 0))),
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    back =
      Effect.MoveToZone
        ( MoveToZone.MkMoveToZone
            (ObjectRef.InSlot Binding.became)
            Zone.Battlefield
            EntryRiders.MkEntryRiders
              { EntryRiders.tapped = TapState.Untapped,
                EntryRiders.attacking = False,
                EntryRiders.blocking = Nothing,
                EntryRiders.transformed = False,
                EntryRiders.counters = Map.singleton kind (Quantity.Literal 1),
                EntryRiders.underOwner = True,
                EntryRiders.exiledFaceDown = False,
                EntryRiders.faceDown = Nothing
              }
            Nothing
            Nothing
            LibraryPlacement.defaultValue
        )

-- CR 702.135a: afterlife N, on the same CR 700.4 dies event `returns` watches.
--
-- Nothing is bound: unlike undying and persist this never touches the permanent
-- that died, so the ability is indifferent to the CR 400.7 incarnation split. CR
-- 111.2 gives the tokens to the ability's controller, which is why
-- EntryRiders.underOwner is inert under a Create.
--
-- THE TOKEN IS MINTED HERE, not carried in card data: its characteristics are
-- printed in the comprehensive rules, and CR 111.3 makes rule 702.135a's own
-- adjectives the token's whole text. Both colours ride the colorIndicator, a token
-- having no mana cost to read a colour off (CR 105.2).
--
-- CR 612.2a's text change reaches the Spirit written here even though the mint
-- runs after the CR 613 layer fold: layer 3 records its pairs on the projection
-- and Projection.mintedTriggeredAbilitiesOf applies them to whatever this returns.
afterlife :: Natural -> TriggeredAbility Card
afterlife n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton spawn))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    spawn =
      Effect.Create
        Create.MkCreate
          { Create.quantity = Quantity.Literal (toInteger n),
            Create.card = spiritToken,
            Create.riders =
              EntryRiders.MkEntryRiders
                { EntryRiders.tapped = TapState.Untapped,
                  EntryRiders.attacking = False,
                  EntryRiders.blocking = Nothing,
                  EntryRiders.transformed = False,
                  EntryRiders.counters = Map.empty,
                  EntryRiders.underOwner = False,
                  EntryRiders.exiledFaceDown = False,
                  EntryRiders.faceDown = Nothing
                },
            Create.slot = Nothing,
            -- CR 111.2 under CR 109.5: the keyword ability's own controller.
            Create.creator = PlayerRef.Relative PlayerRelation.You
          }

-- | CR 702.135a's token: 1/1 white and black Spirit creature with flying. Rule
-- 702.135a names no name, so CR 111.4 supplies one -- "its subtype(s) plus the
-- word 'Token'" -- which is the shape Doomed Traveler's hand-written Spirit
-- token already takes.
spiritToken :: Card
spiritToken =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          Face.MkFace
            { Face.name = CardName.MkCardName (Text.pack "Spirit Token"),
              Face.manaCost = Nothing,
              Face.typeLine =
                TypeLine.MkTypeLine
                  Set.empty
                  (Set.singleton CardType.Creature)
                  (Set.singleton Subtype.Spirit),
              Face.power = Just (Power.MkPower (Quantity.Literal 1)),
              Face.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.keywords = Set.singleton Keyword.Flying,
              Face.colorIndicator = Set.fromList [Color.White, Color.Black],
              Face.characteristicPT = Nothing,
              Face.staticAbilities = [],
              Face.spell = Face.defaultSpell,
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.rooms = Seq.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.maximumX = Nothing,
              Face.alternativeCosts = [],
              Face.costReductions = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.blockPermissions = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.sacrificeRestrictions = [],
              Face.untapRestrictions = [],
              Face.attachRestrictions = [],
              Face.entryRestrictions = [],
              Face.attackCosts = [],
              Face.blockCosts = [],
              Face.mulliganActions = [],
              Face.openingHandActions = [],
              Face.specialActions = []
            }
    }

-- CR 702.123a: fabricate N. "When this permanent enters, you may put N +1/+1
-- counters on it. If you don't, create N 1/1 colorless Servo artifact creature
-- tokens." Afterlife's mint over CR 603.6a's entry event, so the condition is
-- TriggerCondition.SelfEnters; Glint-Sleeve Artisan is the printing.
--
-- ONE CLAUSE and no branching opcode: rule 702.123a prints CR 118.12a's rewriting
-- already performed, so CR 118.12 makes the counters a COST paid as the ability
-- resolves and the clause's own effects are its "if you don't" branch.
--
-- Optionality.Mandatory, because the printed "you may" IS the gate's offer.
-- Marking the clause optional as well would ask twice and let a player decline
-- both halves, which rule 702.123a does not allow. THE TOKEN IS MINTED HERE for
-- afterlife's reason.
fabricate :: Natural -> TriggeredAbility Card
fabricate n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfEnters,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton clause) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    clause = Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory (Just gate) (Seq.singleton spawn)
    gate =
      PayGate.MkPayGate
        { PayGate.payer = PlayerRef.InSlot Binding.you,
          PayGate.cost =
            Cost.MkCost
              { -- CR 118.5, crew's note above: no mana part is `Just` an empty
                -- one, never the Nothing that means unpayable.
                Cost.mana = Just (ManaCost.MkManaCost []),
                Cost.components = [CostComponent.PutPlusOneCountersOnThis n]
              },
          PayGate.branch = PayBranch.IfNotPaid,
          -- Optional because rule 702.123a prints the "may" itself; no offeredAt,
          -- one clause making its own offer.
          PayGate.obligation = PayObligation.Optional,
          PayGate.offeredAt = Nothing
        }
    spawn =
      Effect.Create
        Create.MkCreate
          { Create.quantity = Quantity.Literal (toInteger n),
            Create.card = servoToken,
            Create.riders =
              EntryRiders.MkEntryRiders
                { EntryRiders.tapped = TapState.Untapped,
                  EntryRiders.attacking = False,
                  EntryRiders.blocking = Nothing,
                  EntryRiders.transformed = False,
                  EntryRiders.counters = Map.empty,
                  EntryRiders.underOwner = False,
                  EntryRiders.exiledFaceDown = False,
                  EntryRiders.faceDown = Nothing
                },
            Create.slot = Nothing,
            -- CR 111.2 under CR 109.5: the keyword ability's own controller.
            Create.creator = PlayerRef.Relative PlayerRelation.You
          }

-- | CR 702.123a's token: 1/1 colorless Servo artifact creature. Colorless is the
-- ABSENCE of a colorIndicator (CR 105.2, CR 202.2e) rather than a colour, which
-- is the one way this differs in shape from afterlife's Spirit; CR 111.4 supplies
-- the name.
servoToken :: Card
servoToken =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          Face.MkFace
            { Face.name = CardName.MkCardName (Text.pack "Servo Token"),
              Face.manaCost = Nothing,
              Face.typeLine =
                TypeLine.MkTypeLine
                  Set.empty
                  (Set.fromList [CardType.Artifact, CardType.Creature])
                  (Set.singleton Subtype.Servo),
              Face.power = Just (Power.MkPower (Quantity.Literal 1)),
              Face.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.empty,
              Face.characteristicPT = Nothing,
              Face.staticAbilities = [],
              Face.spell = Face.defaultSpell,
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.rooms = Seq.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.maximumX = Nothing,
              Face.alternativeCosts = [],
              Face.costReductions = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.blockPermissions = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.sacrificeRestrictions = [],
              Face.untapRestrictions = [],
              Face.attachRestrictions = [],
              Face.entryRestrictions = [],
              Face.attackCosts = [],
              Face.blockCosts = [],
              Face.mulliganActions = [],
              Face.openingHandActions = [],
              Face.specialActions = []
            }
    }

-- CR 702.46a: soulshift N. "When this permanent is put into a graveyard from the
-- battlefield, you may return target Spirit card with mana value N or less from
-- your graveyard to your hand." Afterlife's condition with provoke's shape --
-- the CR 700.4 dies event, one optional clause, one target slot.
--
-- The bearer never appears in the payload, so the CR 400.7 incarnation split is
-- inert here, and nothing excludes it from the target either: rule 702.46a does
-- not say "another".
--
-- A CardsInGraveyard pool scoped to You is rule 702.46a's "your graveyard", read
-- as CR 115.2's clause (a) -- and the reason the pool carries a GraveyardScope
-- rather than a Filter is that CR 108.4 gives a card in a graveyard no controller.
-- The move states no origin zone for that same reason.
soulshift :: Natural -> TriggeredAbility Card
soulshift n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing (Optionality.Optional (PlayerRef.Relative PlayerRelation.You)) Nothing (Seq.singleton back))) (Map.singleton soulshiftTarget slot)))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    slot =
      TargetSlot.required
        (Pool.CardsInGraveyard (GraveyardScope.Scoped PlayerScope.You))
        (Just (Filter.And [Filter.HasSubtype Subtype.Spirit, Filter.ManaValueAtMost (toInteger n)]))
    back =
      Effect.MoveToZone
        ( MoveToZone.MkMoveToZone
            (ObjectRef.InSlot soulshiftTarget)
            Zone.Hand
            EntryRiders.MkEntryRiders
              { EntryRiders.tapped = TapState.Untapped,
                EntryRiders.attacking = False,
                EntryRiders.blocking = Nothing,
                EntryRiders.transformed = False,
                EntryRiders.counters = Map.empty,
                EntryRiders.underOwner = False,
                EntryRiders.exiledFaceDown = False,
                EntryRiders.faceDown = Nothing
              }
            Nothing
            Nothing
            LibraryPlacement.defaultValue
        )

-- The slot rule 702.46a's one target is chosen into, mentorTarget's position.
soulshiftTarget :: SlotName.SlotName
soulshiftTarget = SlotName.MkSlotName (Text.pack "soulshifted")

-- CR 702.55a: haunt. Soulshift's shape -- the CR 700.4 dies event and one target
-- slot -- with the clause mandatory, rule 702.55a stating no "may".
--
-- ONLY the permanent sentence. Rule 702.55a's other one, haunt on an instant or
-- sorcery, is not minted (#1404).
--
-- THE CARD, NOT THE PERMANENT (CR 400.7): rule 702.55a's "exile IT" is the
-- graveyard incarnation the death minted, Binding.became, which is why this cannot
-- name Binding.triggerSource.
--
-- Effect.ExileHaunting rather than a MoveToZone to Zone.Exile, because the move is
-- only half of it: CR 702.55b's link from the exiled card to the object targeted
-- is what the exile-zone half of the card reads, and only that opcode writes it.
haunt :: TriggeredAbility Card
haunt =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton exile))) (Map.singleton hauntTarget slot)))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    slot = TargetSlot.required Pool.Creatures Nothing
    exile = Effect.ExileHaunting (ExileHaunting.MkExileHaunting Binding.became hauntTarget)

-- The slot rule 702.55a's one target is chosen into, on soulshiftTarget's terms.
hauntTarget :: SlotName.SlotName
hauntTarget = SlotName.MkSlotName (Text.pack "haunted")

-- CR 702.94a's linked triggered ability: "When you reveal this card this way, you
-- may cast it by paying [cost] rather than its mana cost."
--
-- One MANDATORY clause: the "you may" governs the CASTING alone, which is
-- Prompt.OfferedCast's own question (CR 608.2g). Marking the clause optional would
-- raise a second prompt for one printed "may".
--
-- No move ahead of the offer: CR 701.20b says revealing does not move the card, so
-- what may be cast is Binding.triggerSource (CR 113.7a), not Binding.became.
--
-- The cost rides the OFFER (CastOffer.payingInstead) rather than the card, CR
-- 118.9 making it an alternative cost applied "from another effect": Thunderous
-- Wrath in a hand nobody drew this turn still costs {4}{R}{R}.
miracle :: Cost Keyword -> TriggeredAbility Card
miracle cost =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfRevealedForMiracle,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton offer))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    offer =
      Effect.OfferCast
        OfferCast.MkOfferCast
          { OfferCast.slot = Binding.triggerSource,
            -- Rule 702.94a's "YOU may cast it": the revealer, who is the
            -- trigger's controller, and a "may".
            OfferCast.caster = PlayerRef.Relative PlayerRelation.You,
            OfferCast.optionality = CastObligation.Optional,
            OfferCast.offer = CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = False, CastOffer.payingInstead = Just cost}
          }

-- CR 702.94a's STATIC half, read as the one thing its reader needs: what this
-- card would cost if its controller took the reveal. Nothing when the card has no
-- miracle ability at all, which is also "no window to open".
--
-- morphCost's shape exactly, and asked of the card's PRINTED keywords for
-- flashbackCosts' reason: rule 702.94a's abilities function in the hand
-- (CR 113.6b).
-- ONE cost per card (the ascending-least), morphCost's shape.
--
-- Not implemented: a card in a hand whose MIRACLE an effect granted or removed
-- (#1859).
miracleCost :: Set Keyword -> Maybe (Cost Keyword)
miracleCost keywords =
  let costOf keyword = case keyword of
        Keyword.Miracle cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- The triggered abilities rule 702 mints for a card read OUTSIDE the battlefield,
-- off its printed keywords. `triggeredAbilitiesOf`'s sibling, and the same roster:
-- a printed keyword set holds one instance of each, which is what the count of 1
-- says.
--
-- Rule 702.94a's miracle is the only one any of them reaches today, and CR 113.6k
-- is what decides that -- Pawl.Engine.Event filters this list by
-- `functionsIn`, so a drawn Doomed Traveler's dies trigger is offered from no
-- hand.
printedTriggeredAbilitiesOf :: Set Keyword -> [TriggeredAbility Card]
printedTriggeredAbilitiesOf = triggeredAbilitiesOf . Map.fromSet (const 1)

-- CR 702.63a's SECOND and THIRD abilities, the first being mintedReplacementsFor's
-- -- so vanishing's rule text spans both mints. Ordered as rule 702.63a prints
-- them, which is also the order they fire in: the upkeep removal takes the last
-- counter off, and the sacrifice watches that removal.
vanishing :: [TriggeredAbility Card]
vanishing = [vanishingUpkeep, vanishingLastCounter]

-- "At the beginning of your upkeep, if this permanent has a time counter on it,
-- remove a time counter from it."
--
-- TurnScope.ControllersTurn is rule 702.63a's "YOUR upkeep" (CR 603.3a).
--
-- THE INTERVENING "IF" is renown's, one quantity over: rule 702.63a prints "if",
-- so CR 603.4 keeps the ability off the stack on an upkeep where the counters are
-- already gone. CR 608.2a's re-check is unobservable here, an instance that
-- resolved with the condition false removing nothing and raising no event.
--
-- ONE counter per instance, not per counter present: rule 702.63a removes a single
-- one, and CR 702.63c is what makes a second instance remove a second.
vanishingUpkeep :: TriggeredAbility Card
vanishingUpkeep =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening =
        Just (Condition.Compares (Compares.MkCompares (Quantity.ObjectCounters CounterKind.Time) Comparison.AtLeast (Quantity.Literal 1))),
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.Time (Quantity.Literal 1) Binding.triggerSource)

-- "When the last time counter is removed from this permanent, sacrifice it."
--
-- Watches the REMOVAL and not the count, so a permanent whose time counters were
-- all removed before it entered has nothing to trigger, and an upkeep that removes
-- nothing raises no GameEvent.CountersRemoved to match either.
--
-- Effect.Sacrifice, never Destroy: CR 701.21a says a sacrifice is not a
-- destruction, so an indestructible permanent with vanishing still goes.
vanishingLastCounter :: TriggeredAbility Card
vanishingLastCounter =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfLastCounterRemoved CounterKind.Time,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.Sacrifice Binding.triggerSource

-- CR 702.32a's SECOND ability, on vanishingUpkeep's trigger condition exactly.
-- Rule 702.32a states NO intervening "if", so this fires on every one of its
-- controller's upkeeps including the one where the pile is already empty; that
-- firing is the whole of the rule's sacrifice.
--
-- ONE ability with TWO clauses, not two abilities: two triggers under
-- complementary intervening "if"s would not be equivalent, since CR 603.4
-- re-checks each at resolution.
--
-- THE CLAUSES ARE INVERTED against the printed order, and the printed order is
-- unwritable: a gate is read as its clause is REACHED (CR 608.2c), so a sacrifice
-- clause standing after the removal would read a pile the removal had already
-- emptied. Observably equivalent, since nothing runs between two clauses of one
-- resolution (CR 117.3b, CR 704.3).
fading :: TriggeredAbility Card
fading =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.fromList [sacrificeClause, removeClause]) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    counted = Quantity.ObjectCounters CounterKind.Fade
    sacrificeClause =
      Clause.MkClause
        Nothing
        (Just (Condition.Compares (Compares.MkCompares counted Comparison.AtMost (Quantity.Literal 0))))
        Nothing
        Optionality.Mandatory
        Nothing
        (Seq.singleton (Effect.Sacrifice Binding.triggerSource))
    removeClause =
      Clause.MkClause
        Nothing
        (Just (Condition.Compares (Compares.MkCompares counted Comparison.AtLeast (Quantity.Literal 1))))
        Nothing
        Optionality.Mandatory
        Nothing
        (Seq.singleton (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.Fade (Quantity.Literal 1) Binding.triggerSource)))

-- CR 702.43a's SECOND ability: "when this permanent is put into a graveyard from
-- the battlefield, you may put a +1/+1 counter on target artifact creature for
-- each +1/+1 counter on this permanent." Mentor's shape -- one target slot, one
-- counter-placing effect -- with a "may" and a counted quantity, and a plain
-- Effect.PutCounters where mentor has CR 702.134c's marker to record.
--
-- THE COUNT is Quantity.ObjectCounters, read off CR 113.7a's source through
-- Projection.viewWithLastKnown. That is CR 608.2h doing the work: the permanent is
-- in a graveyard by the time this resolves and CR 122.2 made its counters cease
-- with it, so the last known record is the only place the number still is
-- (Pawl.TriggerSpec's modularSpec). ZERO counters is an ordinary answer.
--
-- Optionality.Optional is rule 702.43a's "you may". A REAL choice: declining with
-- a legal target on the board leaves the counters nowhere.
modular :: TriggeredAbility Card
modular =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing (Optionality.Optional (PlayerRef.Relative PlayerRelation.You)) Nothing (Seq.singleton effect))) (Map.singleton modularTarget slot)))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    slot = TargetSlot.required Pool.Creatures (Just (Filter.HasCardType CardType.Artifact))
    effect =
      Effect.PutCounters
        ( PutCounters.MkPutCounters
            CounterKind.PlusOnePlusOne
            (Quantity.ObjectCounters CounterKind.PlusOnePlusOne)
            (ObjectRef.InSlot modularTarget)
        )

-- The slot rule 702.43a's one target is chosen into, mentorTarget's position.
modularTarget :: SlotName.SlotName
modularTarget = SlotName.MkSlotName (Text.pack "modularRecipient")
