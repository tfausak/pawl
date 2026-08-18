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
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
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
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.DiscardCause as DiscardCause
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

-- Rule 702 in its OTHER voice. Most keywords are read where they matter --
-- Projection.hasKeyword for an evasion bit, Pawl.Engine.Damage for infect's and
-- toxic's riders -- because the rule states them as static abilities some
-- rules-core reader already asks about. The ones rule 702 spells out as TRIGGERED
-- or ACTIVATED abilities have to be MINTED here and handed to the ordinary CR 603
-- and CR 602 machinery instead.
--
-- Casing on Keyword here is legitimate for the reason Pawl.Types.Keyword's own
-- comment gives: a keyword is a numbered rule, not an effect's identity. What
-- this module must never do is grow an arm for a CARD.
--
-- Rule 702 has no state-triggered (CR 603.8) or delayed (CR 603.7) keyword
-- ability, so the first keyword that needs one must widen Event's two scans.
--
-- Rule 702.34a's flashback shows how wide this voice is: ONE keyword becomes a
-- cost (flashbackCost), a casting permission (castingPermissionsOf) and a
-- replacement effect (castFromGraveyardExile), none of whose readers learn that
-- flashback produced them. All three are derived from a card's PRINTED keywords
-- rather than a projection's post-layer ones, because all three function in the
-- graveyard or on the stack (CR 113.6), neither of which pawl's projection
-- reaches (#160). entwineCost is read the same way (CR 702.42a).

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
-- states, and rules 702.29a and 702.77a state the hand -- where no pool effect
-- changes a card's abilities (#160).
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
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  -- CR 702.94a's hand ability is TRIGGERED rather than activated, so it is
  -- minted by `abilitiesFor` above and reached from a hand by CR 113.6k.
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  -- CR 701.43d's ability is static and functions on the battlefield, so it mints
  -- nothing activatable from a hand.
  Keyword.Exert -> []
  Keyword.Persist -> []
  Keyword.Undying -> []

-- CR 702.29a: cycling means paying its cost and discarding the card to draw a
-- card. The whole ability, minted from the one cost the keyword carries.
--
-- The discard is a COMPONENT of the activation cost and not an effect, rule
-- 702.29a putting it before the colon. Three things follow: an activation the
-- player backs out of discards nothing, the card is already in the graveyard while
-- the draw is still on the stack, and CR 702.29c's "when you cycle this card" has
-- a cost payment to trigger off rather than a resolution.
--
-- ToPayCyclingCost, which is what CR 702.29c means by "an activation cost of a
-- cycling ability" -- and it covers rule 702.29e's typecycling too, because rule
-- 702.29f makes those cycling abilities and this one function mints both.
--
-- No restriction clause, rule 702.29a stating no timing restriction, which leaves
-- CR 117.1b's default.
cycling :: Cost Keyword -> Maybe (Filter Keyword) -> ActivatedAbility Card
cycling cost searchFor =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.DiscardThis DiscardCause.ToPayCyclingCost]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [],
      -- CR 702.29a gives the card this ability outright, with no "as long as".
      ActivatedAbility.condition = Nothing
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
      Nothing -> Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
      -- CR 702.29e's "search your library for a [quality] card", so one card is
      -- the whole instruction's count.
      Just filter_ ->
        Effect.Search
          Search.MkSearch
            { Search.searcher = PlayerRef.Relative PlayerRelation.You,
              Search.owner = PlayerRef.Relative PlayerRelation.You,
              Search.quantity = Quantity.Literal 1,
              Search.filter = filter_,
              -- CR 702.29e prints no "up to", and its quality-stating filter puts
              -- the search under CR 701.23b anyway, so this value is unobservable.
              Search.upTo = False,
              Search.destination = SearchDestination.RevealThenHand
            }

-- CR 702.77a: "reinforce N-[cost]" means "[cost], Discard this card: Put N +1/+1
-- counters on target creature." Cycling's ability one clause over, and the first
-- hand ability with a TARGET: the target is chosen at CR 601.2c, before CR 601.2h
-- pays and so before the discard, and the ability outlives the card it discards
-- (CR 113.7a).
--
-- The discard is a COMPONENT of the activation cost for cycling's reasons. Its
-- cause is Ordinary and not ToPayCyclingCost: rule 702.77 never says reinforce is
-- a cycling ability, so CR 702.29c's "when you cycle this card" must not see it.
--
-- The target is Pool.Creatures unqualified, rule 702.77a printing "target
-- creature" and no more, and Mandatory, the rule stating no "may".
--
-- Quantity.Literal and not a counter reading: N is written on the card, where
-- modular's count is measured off the dying permanent.
reinforce :: Natural -> Cost Keyword -> ActivatedAbility Card
reinforce n cost =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.DiscardThis DiscardCause.Ordinary]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) (Map.singleton reinforceTarget slot)))
          (ModeSelection.ChooseExactly 1),
      -- CR 702.77a states no timing restriction, which leaves CR 117.1b's
      -- default, and gives the ability outright with no "as long as".
      ActivatedAbility.restrictions = [],
      ActivatedAbility.condition = Nothing
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

-- The slot rule 702.77a's one target is chosen into, modularTarget's position and
-- for its reason.
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
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  -- CR 701.43d states no activated ability: exerting is a cost paid at CR 508.1g,
  -- which Pawl.Engine.Combat.declareAttackers offers rather than the stack.
  Keyword.Exert -> []
  Keyword.Persist -> []
  Keyword.Undying -> []

-- CR 702.122a: "Crew N" means "Tap any number of other untapped creatures you
-- control with total power N or greater: This permanent becomes an artifact
-- creature until end of turn." The whole ability, minted from the one number the
-- keyword carries.
--
-- THE COST. Every word of rule 702.122a's criterion is written in the existing
-- Filter vocabulary rather than baked into the component; only the AGGREGATE is
-- the component's own, total power being a property of the chosen set rather than
-- of any candidate. `Not IsSource` is load-bearing: a Vehicle that has already
-- become a creature could otherwise crew itself.
--
-- CR 302.6 does NOT reach this cost, in either direction. The Vehicle needs no
-- haste, the tap symbol not being in the cost; and a creature that arrived this
-- turn may still be tapped to crew, rule 302.6 gating only a creature's OWN
-- activated ability with the tap symbol in it.
--
-- THE EFFECT. "Becomes an artifact creature" ADDS two card types and sets nothing,
-- which is CR 205.1b naming this exact phrase -- so the Vehicle stays a Vehicle,
-- and AddCardType is right where its sibling SetCardType's CR 205.1a replacement
-- would take the artifact type and the Vehicle subtype away. TWO of them (CR
-- 300.1) in one mode, since CR 613.7b stamps both at the moment this one
-- resolution creates them. Layer 4 either way (CR 613.1d).
--
-- Binding.triggerSource, so the Vehicle is named and never TARGETED (CR 115.10a):
-- a targeted crew would fizzle to shroud and fire "becomes the target" triggers
-- that the printed ability does not.
--
-- CR 208.3 needs no clause here: the Vehicle's printed power and toughness are
-- gated on its being a creature at Projection's read points, so adding the type is
-- the whole of CR 301.7b.
--
-- No restriction clause, rule 702.122a stating no timing restriction, and its "any
-- number of times" needs no expression.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [becomes CardType.Artifact, becomes CardType.Creature]))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [],
      -- CR 702.122a gives the permanent this ability outright, with no "as long
      -- as", cycling's answer.
      ActivatedAbility.condition = Nothing
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

-- CR 702.87a: "Level up [cost]" means "[Cost]: Put a level counter on this
-- permanent. Activate only as a sorcery." Outlast's twin below, and the two are
-- worth reading together for what differs.
--
-- THE COST is the printed one UNCHANGED -- rule 702.87a appends no ", {T}", so
-- unlike outlast there is no CostComponent.TapThis and CR 302.6 does not reach
-- this ability: a leveler that arrived this turn can level up.
--
-- THE EFFECT names the permanent through Binding.triggerSource, so rule 702.87a's
-- "this permanent" is named and never TARGETED (CR 115.10a), outlast's posture.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton gain))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [ActivationRestriction.SorcerySpeed],
      ActivatedAbility.condition = Nothing
    }
  where
    gain = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.Level (Quantity.Literal 1) (ObjectRef.InSlot Binding.triggerSource))

-- CR 702.107a: "Outlast [cost]" means "[Cost], {T}: Put a +1/+1 counter on this
-- creature. Activate only as a sorcery." The card names the cost; every other word
-- is the rule's.
--
-- THE COST is the printed one with CostComponent.TapThis APPENDED, rule 702.107a's
-- ", {T}". Unlike crew's cost the tap symbol is the permanent's own, so CR 302.6
-- does reach this ability and a creature that arrived this turn cannot outlast.
--
-- THE EFFECT names the permanent through Binding.triggerSource, so rule 702.107a's
-- "this creature" is named and never TARGETED (CR 115.10a). One counter always,
-- the rule writing that number itself where rule 702.112a leaves renown's to the
-- card.
--
-- CR 602.5d is the timing clause and the ONLY restriction, rule 702.107a stating
-- no once-per-turn limit.
outlast :: Cost Keyword -> ActivatedAbility Card
outlast cost =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = cost {Cost.components = Cost.components cost <> [CostComponent.TapThis]},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton grow))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [ActivationRestriction.SorcerySpeed],
      ActivatedAbility.condition = Nothing
    }
  where
    grow = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.InSlot Binding.triggerSource))

-- CR 601.3: the casting permissions rule 702 gives a card for holding a keyword.
-- A card's own printed permissions are a separate, additive list.
--
-- WHICH keyword set is the caller's to choose: a card in a GRAVEYARD is read
-- through the projection, so a granted flashback grants its permission too, while
-- a card in a LIBRARY is read as printed (#160).
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
  -- CR 702.29a is an ACTIVATED ability, not a casting permission: cycling
  -- discards the card, it never casts it. See handAbilitiesOf above.
  Keyword.Cycling {} -> []
  -- CR 702.122a is an activated ability too, and one that functions on the
  -- battlefield -- see battlefieldAbilitiesOf above.
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
  -- CR 702.33a grants no permission either, for entwine's reason below: kicker
  -- adds a cost to a cast some other rule already allowed. CR 702.33a's "as you
  -- cast this spell" is not a zone or a timing permission -- it is when the
  -- additional cost is announced (CR 601.2b).
  Keyword.Kicker _ -> []
  -- CR 702.42a grants no permission: entwine widens a MODE choice and adds a
  -- cost to a cast that some other rule already allowed; it never allows one.
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
  Keyword.Training -> []
  Keyword.Toxic _ -> []
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
  -- CR 701.43d names no zone a card may be cast from.
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
-- rather than a permanent. PRINTED keywords are indistinguishable from projected
-- ones on every board pawl can build (#160), which is a claim about the pool
-- rather than about Magic; the same posture handAbilitiesOf takes.
hasFlash :: Set Keyword -> Bool
hasFlash = Set.member Keyword.Flash

-- | CR 702.133a's ADDITIONAL cost, "discarding a card": whether this card's
-- keywords add one to a cast from a graveyard. Read by Cost.costsFor only while
-- the object is in a graveyard, the zone half of the same sentence.
--
-- A Bool rather than flashbackCost's Maybe Cost: rule 702.133a states the cost
-- itself, so there is nothing to read off the card. Membership rather than a
-- count, the rule taking no parameter.
hasJumpStart :: Set Keyword -> Bool
hasJumpStart = Set.member Keyword.JumpStart

-- CR 702.34a: the cost this card may be cast from the graveyard for, or Nothing
-- when it has no flashback. Read by Pawl.Engine.Cost.costsFor, which offers it
-- ONLY while the object is in a graveyard -- the zone half of the same sentence.
--
-- A wildcard rather than an exhaustive case: this asks about ONE named
-- constructor rather than classifying every keyword.
--
-- Nothing beyond the FIRST flashback cost is reachable, where rule 702.34a states
-- no limit and CR 601.2b would give a choice between them; only the lesser (Set
-- order) is offered (gap #294).
flashbackCost :: Set Keyword -> Maybe (Cost Keyword)
flashbackCost keywords =
  let costOf keyword = case keyword of
        Keyword.Flashback cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.37a / 702.37e: the MORPH cost -- what a face-down permanent's
-- controller pays to turn it face up as CR 116.2b's special action -- or Nothing
-- when the card has no morph ability. Read by Pawl.Engine.FaceDown.
--
-- NOT the cost of the morph CAST: rule 702.37a writes that one into the rule
-- itself ("by paying {3}"), so it comes from Cost.faceDownCost.
--
-- Asked of the card's PRINTED keywords, which for morph is the rule's own scope:
-- CR 702.37a's ability "functions in any zone from which you could play the card
-- it's on", and a face-down permanent projects no keywords at all (CR 708.2a), so
-- a projected read would find nothing to pay.
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

-- CR 702.33a: the ADDITIONAL cost this card's controller may pay as they cast it,
-- or Nothing when it has no kicker. Offered at CR 601.2b and added to whichever
-- candidate cost was announced (CR 601.2f). A wildcard, flashbackCost's shape.
--
-- Nothing beyond the FIRST kicker cost is reachable, so CR 702.33b's "kicker
-- [cost 1] and/or [cost 2]" is unrepresented (gap #1235).
kickerCost :: Set Keyword -> Maybe (Cost Keyword)
kickerCost keywords =
  let costOf keyword = case keyword of
        Keyword.Kicker cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.42a: the ADDITIONAL cost this card's controller may pay to choose all of
-- its modes, or Nothing when it has no entwine. Offered at CR 601.2b and added to
-- whichever candidate cost was announced (CR 601.2f). A wildcard, flashbackCost's
-- shape.
--
-- Nothing beyond the FIRST entwine cost is reachable: a card printing two
-- entwine abilities is expressible and unrepresented (gap #474).
entwineCost :: Set Keyword -> Maybe (Cost Keyword)
entwineCost keywords =
  let costOf keyword = case keyword of
        Keyword.Entwine cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- CR 702.170a: what CR 116.2k's special action costs -- "you may exile this card
-- from your hand and PAY [COST]" -- or Nothing when the card has no plot.
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

-- CR 702.143a: what a foretold card is CAST for -- "they may cast that card
-- after the current turn has ended by PAYING ANY FORETELL COST it has" -- or
-- Nothing when the card has no foretell.
--
-- The cost of the CAST and never of the special action, plotCost's mirror: CR
-- 116.2h fixes the action's cost at {2} for every printing, so Pawl.Engine.Foretell
-- mints that itself.
--
-- A wildcard, and ONE cost per card (the ascending-least), morphCost's shape.
foretellCost :: Set Keyword -> Maybe (Cost Keyword)
foretellCost keywords =
  let costOf keyword = case keyword of
        Keyword.Foretell cost -> Just cost
        _ -> Nothing
   in Maybe.listToMaybe (Maybe.mapMaybe costOf (Set.toAscList keywords))

-- The one exile CR 702.34a, CR 702.127a and CR 702.133a all print, in the same
-- words: the ability functioning while the card is on the stack, exiling it
-- instead of putting it anywhere else as it leaves.
--
-- Filter.IsSource, because the rule says "this card". Pawl.Engine.Event proposes
-- the move before it performs one, so the object is still on the stack when this
-- is asked.
--
-- The destination is Graveyard rather than "anywhere else": a ZoneChangePattern
-- names ONE destination, and the graveyard is the only place a spell leaves the
-- stack for in this pool (#293).
--
-- GATED on the clause each of the three rules puts in front of it, and the three
-- clauses are not the same question. `castFor` is the keyword whose candidate cost
-- the cast was announced for, which is what rule 702.34a's "if the flashback cost
-- was paid" and rule 702.133a's jump-start clause each ask about; rule 702.127a
-- asks only whether the cast came from a graveyard, which the caller has already
-- established.
--
-- The door Pawl.Engine.Cast uses, so that module installs a REPLACEMENT EFFECT it
-- never inspects rather than asking which of the three keywords a card has.
castFromGraveyardReplacementsOf :: Set Keyword -> Maybe Keyword -> [ReplacementEffect (Effect.Effect Card)]
castFromGraveyardReplacementsOf keywords castFor =
  let paidFor keyword = castFor == Just keyword
   in -- The cost the cast PAID FOR, and not merely a flashback the card has:
      -- paying the printed cost under a CR 601.3 permission leaves rule 702.34a's
      -- clause unsatisfied. Compared against the cost-bearing keyword itself,
      -- which is how a card with two flashback abilities (#294) answers for the
      -- one it was cast for rather than for both.
      [castFromGraveyardExile | any paidFor (Maybe.maybeToList (fmap Keyword.Flashback (flashbackCost keywords)))]
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
          { ZoneChangePattern.whenDestination = Zone.Graveyard,
            ZoneChangePattern.whoseObject = ControllerRelation.Anyones,
            ZoneChangePattern.whatObject = Filter.IsSource
          }
        Zone.Exile
    )

-- CR 702.136a: the AS-ENTERS REPLACEMENT rule 702 gives a permanent for holding
-- riot -- "You may have this permanent enter with an additional +1/+1 counter on
-- it. If you don't, it gains haste."
--
-- Gathered by the PROJECTION, where castFromGraveyardExile's row is installed by
-- Pawl.Engine.Cast on a spell. POST-LAYER keyword COUNTS, since rule 702.136a
-- functions on the battlefield, so Humility takes it away and a static ability
-- granting riot adds it, both for free.
--
-- ONE ROW PER INSTANCE, because CR 702.136b says each instance works separately.
-- The two rows are EQUAL VALUES, so what gives the second its own CR 614.5
-- opportunity is the instance ordinal Replacement.collect assigns; the proving
-- test is Pawl.ReplacementSpec's "CR 702.136b riot twice".
--
-- The pattern is Filter.IsSource: CR 614.1c's ability is the entering object's own.
--
-- "Minted" rather than "entry" because CR 702.37b's megamorph rides the same
-- function with a CR 614.1e replacement, so this answers with rows of two event
-- classes; the CR 616.1 loop matches each row against the event it is offered.
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
  Keyword.Vanishing n -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Time n))))
  -- CR 702.32a's FIRST ability, vanishing's row in the fade counter. One row per
  -- instance for riot's reason, rule 702.32 stating no multiplicity clause.
  Keyword.Fading n -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Fade n))))
  Keyword.Frenzy _ -> []
  -- CR 702.43a's FIRST ability, vanishing's row with a different counter kind. One
  -- row per instance, and CR 702.43b makes them add up.
  Keyword.Modular n -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.PlusOnePlusOne n))))
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
  -- Pawl.Engine.Replacement.applies: CR 701.40c gives a manifested megamorph card
  -- a second road face up at its MANA cost, and the row is refused down that one
  -- (Pawl.FaceDownSpec's Misthoof Kirin pair). morphCost answers one cost per
  -- permanent, so CR 702.37e's road has a single price and the rest of the
  -- condition needs no test.
  --
  -- ONE ROW PER INSTANCE, as riot's is. Unexercised: no card grants megamorph to a
  -- creature that already has it.
  Keyword.Morph (Morph.MkMorph _ MorphVariant.Mega) ->
    List.genericReplicate count (ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR Filter.IsSource (TurnUpRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.PlusOnePlusOne 1))))
  Keyword.Menace -> []
  Keyword.Renown _ -> []
  Keyword.Cycling {} -> []
  Keyword.Kicker _ -> []
  Keyword.Flashback _ -> []
  Keyword.Entwine _ -> []
  Keyword.Bushido _ -> []
  Keyword.Soulshift _ -> []
  -- CR 702.54a's ONE static ability, vanishing's row with rule 702.54a's
  -- condition on it: "if an opponent was dealt damage this turn, this permanent
  -- enters with N +1/+1 counters on it". Filter.IsSource for riot's reason, and
  -- the condition is Pawl.Engine.Replacement.admitsEntry's rather than this
  -- function's -- nothing knowable from a keyword and a count can answer it.
  --
  -- ONE ROW PER INSTANCE, and CR 702.54c says so outright ("if an object has
  -- multiple instances of bloodthirst, each applies separately"), so two
  -- instances place two lots of N -- both admitted or neither, since the two rows
  -- ask one condition of one board.
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
  -- represented by a double-faced card, it enters transformed" -- CR 712.13a's
  -- rule seen from the keyword side, and the one producer CR 616.1d's bucket has.
  -- Filter.IsSource for riot's reason, one subrule over: CR 614.1d's "[this
  -- permanent] enters . . ." is the entering object's own ability.
  --
  -- The rule's two conditions are asked by Pawl.Engine.Replacement.applies rather
  -- than here, because neither is knowable from a keyword count: the designation
  -- is the game's and the layout is the entering object's.
  --
  -- ONE ROW PER INSTANCE for riot's reason, and safe twice over for neither of
  -- riot's: the second row is a distinct CandidateId, so CR 614.5 would let it
  -- apply, and it would write the same back face the first one did. Idempotent,
  -- because the write names Card.backFace outright rather than the successor of
  -- whatever face is up now. Unreachable anyway -- rule 702.145a puts daybound on
  -- a front face, and nothing in the pool grants a second instance.
  Keyword.Daybound -> List.genericReplicate count (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource EntryRewrite.EntersTransformed))
  -- CR 702.145e gives nightbound only TWO static abilities, and neither rewrites
  -- an entry: the enters-transformed half is daybound's alone.
  Keyword.Nightbound -> []
  Keyword.Decayed -> []
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  -- CR 702.94a's static half is a PERMISSION to reveal, not a replacement: CR
  -- 121.9's window changes nothing about the draw, so there is no event to
  -- rewrite. Pawl.Engine.Event's draw funnel asks miracleCost directly.
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  -- CR 701.43d replaces no event: CR 508.1g's choice is a step of a turn-based
  -- action, and the exert itself writes Object.exertedBy directly.
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
  Keyword.Training -> []
  Keyword.Toxic _ -> []
  Keyword.Plot _ -> []
  Keyword.Foretell _ -> []
  -- CR 702.94a states no combat restriction.
  Keyword.Miracle _ -> []
  Keyword.StartYourEngines -> []
  -- CR 701.43d states no combat restriction. It states an optional COST to
  -- attack, which never makes an attack illegal -- the active player may always
  -- decline it (CR 508.1g).
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
  Keyword.Plot _ -> Just KeywordFamily.Plot
  Keyword.Foretell _ -> Just KeywordFamily.Foretell
  -- CR 702.94a's parameterized keyword: "a card with miracle" drops the cost.
  Keyword.Miracle _ -> Just KeywordFamily.Miracle
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
  -- CR 702.83a takes no parameter, so there is no variant for a card to name.
  Keyword.Exalted -> Nothing
  -- CR 702.134a takes no parameter either, so mentor has no family of its own.
  Keyword.Mentor -> Nothing
  Keyword.Afterlife _ -> Just KeywordFamily.Afterlife
  -- CR 702.39a takes no parameter, so provoke has no family of its own.
  Keyword.Provoke -> Nothing
  Keyword.BattleCry -> Nothing
  -- CR 702.100a takes no parameter either, so evolve has no family of its own.
  Keyword.Evolve -> Nothing
  -- CR 702.105a takes no parameter either, so dethrone has no family of its own.
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
  -- CR 702.121a takes no parameter, so there is no variant for a card to name.
  Keyword.Melee -> Nothing
  Keyword.Aftermath -> Nothing
  Keyword.JumpStart -> Nothing
  Keyword.Riot -> Nothing
  Keyword.Unleash -> Nothing
  Keyword.Daybound -> Nothing
  Keyword.Nightbound -> Nothing
  Keyword.Decayed -> Nothing
  -- CR 702.149a takes no parameter, so training has no family of its own.
  Keyword.Training -> Nothing
  Keyword.StartYourEngines -> Nothing
  -- CR 701.43d takes no parameter, so exert has no family of its own.
  Keyword.Exert -> Nothing
  -- CR 702.79a and CR 702.93a take no parameter, so neither has a family.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.115a: whenever this creature deals combat damage to a player, that
-- player exiles the top card of their library. Poisonous' condition and
-- poisonous' "that player" -- the same Binding.triggerPlayer slot the scan
-- stamps -- over a different payload.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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
                EntryRiders.transformed = False,
                EntryRiders.counters = Map.empty,
                EntryRiders.underOwner = False,
                EntryRiders.exiledFaceDown = False,
                EntryRiders.faceDown = False
              }
            Nothing
            Nothing
            LibraryPlacement.defaultValue
        )

-- CR 702.86a: whenever this creature attacks, defending player sacrifices N
-- permanents.
--
-- CR 508.3a is what "attacks" means -- being declared as an attacker -- so the
-- condition is battle cry's SelfAttacks EveryTime, rule 702.86a stating no "for
-- the first time each turn" narrowing.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.91a: whenever this creature attacks, each other attacking creature gets
-- +1/+0 until end of turn.
--
-- CR 508.3a is what "attacks" means, so the condition is the self-scoped
-- SelfAttacks EveryTime, rule 702.91a stating no narrowing.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.100a: whenever a creature you control enters, if that creature's power
-- and/or toughness is greater than this creature's, put a +1/+1 counter on this
-- creature.
--
-- The condition is TriggerCondition.PermanentEnters, the event being somebody ELSE
-- entering, so the Filter is the rule's two printed words. The bearer is NOT
-- excluded, which is the rule rather than an omission: a creature entering
-- compares itself against itself, which no comparison below can answer true.
--
-- The comparison rides the intervening "if" (CR 603.4) and not the condition's
-- Filter, where training's lives, because rule 702.100a prints "if": CR 608.2a
-- re-checks it as the ability resolves, so pumping the bearer in response takes
-- the counter away.
--
-- "THAT CREATURE" is the entrant, neither the bearer nor a target, so the
-- condition reaches it through Quantity.AgainstSlot at Binding.became. Read
-- through CR 608.2h, so an entrant killed while the trigger waits is compared at
-- the power and toughness it last had on the battlefield; Pawl.TriggerSpec's
-- Evolve group proves it at the resolution check.
--
-- Condition.Any because rule 702.100a's "and/or" compares two DIFFERENT
-- characteristics, and one Compares reads one pair of quantities. STRICTLY
-- greater, spelled as "at least one more" since CR 208.1's power and toughness are
-- whole numbers and Comparison has no strict arm.
--
-- CR 702.100c falls out rather than being written: a permanent that is not a
-- creature has no power or toughness (CR 208.3), and Condition.holds reads an
-- unanswerable side as False.
--
-- THE COUNTER goes on through Effect.Evolve rather than Effect.PutCounters, which
-- is rule 702.100b: one opcode is what ties the "evolves" marker to the placement.
-- Renegade Krasis reads it.
evolve :: TriggeredAbility Card
evolve =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition =
        TriggerCondition.PermanentEnters
          (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You]),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.121a: whenever this creature attacks, it gets +1/+1 until end of turn
-- for each opponent you attacked with a creature this combat.
--
-- The condition is battle cry's SelfAttacks. Attacking a PLANESWALKER fires it
-- just the same -- CR 508.1a chooses the attackers and CR 508.1b only then says
-- what each attacks -- so what the planeswalker changes is the bonus.
--
-- "IT" is the bearer, so the payload is prowess'.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.23a: whenever this creature becomes blocked, it gets +N/+N until end of
-- turn for each creature blocking it beyond the first.
--
-- The condition is bushido's blocked half, CR 509.3c, which fires ONCE however
-- many creatures blocked, where flanking's CR 509.3d fires once per blocker. Rule
-- 702.23a's bonus already counts the blockers itself, so a per-blocker trigger
-- would count them twice.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.25a: whenever this creature becomes blocked by a creature without
-- flanking, the blocking creature gets -1/-1 until end of turn.
--
-- CR 509.3d is the event -- "becomes blocked by a creature", once for each
-- creature that blocks -- and NOT CR 509.3c's "becomes blocked", which fires once
-- however many blockers there are. Two blockers on one flanker is two triggers and
-- two -1/-1s. Bushido below reads that other, grouped event.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton flankingEffect))) Map.empty))
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

-- CR 702.83a: whenever a creature you control attacks alone, that creature gets
-- +1/+1 until end of turn.
--
-- The ability is borne by a BYSTANDER on both sides at once: the condition watches
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.45a: "'Bushido N' means 'Whenever this creature blocks or becomes
-- blocked, it gets +N/+N until end of turn.'" The one keyword whose sentence names
-- TWO events: "blocks" is CR 509.3a and "becomes blocked" is CR 509.3c.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.68a: "'Frenzy N' means 'Whenever this creature attacks and isn't
-- blocked, it gets +N/+0 until end of turn.'" bushidoHalf with the toughness bonus
-- zeroed.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.130a: whenever this creature becomes blocked, defending player loses N
-- life. Its condition is bushido's CR 509.3c half and its player is annihilator's
-- CR 508.5 one, read off GameEvent.AttackerBlocked through Binding.triggerPlayer.
-- NOT the ability's controller: CR 603.3a makes that the ATTACKING creature's
-- controller, and the life leaves whom they attacked.
--
-- Effect.LoseLife and not damage: rule 702.130a says "loses N life", so this is CR
-- 119.3's life loss and none of CR 120's damage machinery sees it.
afflict :: Natural -> TriggeredAbility Card
afflict n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfBecomesBlocked,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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

-- CR 702.134a: whenever this creature attacks, put a +1/+1 counter on target
-- attacking creature with power less than this creature's power. The first minted
-- ability that TARGETS: the slot is filled by CR 603.3d as the ability is placed
-- and re-checked by CR 608.2b as it resolves. A REAL choice, asked of the
-- controller, since with two smaller attackers the rules leave which one open.
--
-- The target slot's three parts are the rule's three printed words;
-- Filter.PowerLessThanSource compares against the SOURCE, which is why that atom
-- carries no literal. No controller conjunct, rule 702.134a stating none: CR 508.1
-- makes every attacking creature the active player's. The BEARER excludes itself
-- with no `Not IsSource`, nothing having power less than its own power, which is
-- why the atom is strict.
--
-- Effect.Mentor and not Effect.PutCounters, for evolve's reason one rule over: CR
-- 702.134c makes "a creature mentors another creature" a trigger event, so the
-- placement has to be distinguishable from every other +1/+1 counter. The counter
-- still goes through Event.putCounters, so CR 122.6's funnel and CR 614.16's
-- replacement opportunity are unaffected; Pawl.TriggerSpec's Doubling Season case
-- proves it.
mentor :: TriggeredAbility Card
mentor =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) (Map.singleton mentorTarget slot)))
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

-- CR 702.149a: whenever this creature and at least one other creature with power
-- greater than this creature's power attack, put a +1/+1 counter on this creature.
-- Mentor's clause with the comparison reversed and the target dropped: rule
-- 702.149a pumps the BEARER, so there is nothing to choose and no slot.
--
-- The comparison therefore rides the CONDITION. CR 702.149a's companion is part of
-- the trigger event, not an intervening-if clause, so it is checked once as the
-- attackers are declared and never again on resolution -- a bigger co-attacker
-- that dies in response still leaves the counter.
--
-- "Other" is the condition's own, an identity check the Filter has no atom for. No
-- controller conjunct, for mentor's reason.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.Train Binding.triggerSource

-- CR 702.21a: ward [cost]. "Whenever this permanent becomes the target of a spell
-- or ability an opponent controls, counter that spell or ability unless that
-- player pays [cost]."
--
-- ONE CLAUSE and no branching opcode, fabricate's shape: CR 118.12a rewrites "[do
-- something] unless [a player does something else]" as an offer followed by the
-- thing, so the Counter is the clause's "if they don't" branch and the PayGate is
-- the offer. CR 118.12 puts that payment at RESOLUTION, which is what rule 702.21a
-- needs.
--
-- THE PAYER IS THE TARGETER'S CONTROLLER, not the bearer's, so the same
-- Binding.targetingObject slot answers both halves of the sentence; Binding.you
-- would offer the cost to the wrong player.
--
-- "THAT SPELL OR ABILITY" is read out of that same slot and is NOT a target slot:
-- rule 702.21a targets nothing, so nothing here is re-checked at CR 608.2b and a
-- shroud-bearing spell is countered as readily as any other.
--
-- Optionality.Mandatory: the gate's offer IS the only choice rule 702.21a gives.
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
    clause = Clause.MkClause Nothing Optionality.Mandatory (Just gate) (Seq.singleton effect)
    -- PayObligation.Optional and no offeredAt: rule 702.21a's "unless that
    -- player pays" is CR 118.12a's "may", and one clause makes its own offer.
    gate =
      PayGate.MkPayGate
        { PayGate.payer = Binding.targetingObject,
          PayGate.cost = cost,
          PayGate.branch = PayBranch.IfNotPaid,
          PayGate.obligation = PayObligation.Optional,
          PayGate.offeredAt = Nothing
        }
    effect = Effect.Counter (Counter.MkCounter (ObjectRef.InSlot Binding.targetingObject) Nothing)

-- CR 702.147a's TRIGGERED half: "When this creature attacks, sacrifice it at end
-- of combat." CR 508.3a is what "attacks" means, so the condition is mentor's and
-- provoke's -- SelfAttacks, EveryTime, rule 702.147a stating no once-a-turn
-- narrowing.
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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
--
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.Sacrifice Binding.triggerSource

-- CR 702.39a's provoke: whenever this creature attacks, you may choose to have
-- target creature defending player controls block this creature this combat if
-- able; if you do, untap that creature. Rule 702 states it as a triggered
-- ability, and it is the first whose payload creates a CR 509.1c blocking
-- REQUIREMENT rather than changing a characteristic.
--
-- CR 508.3a is what "attacks" means, so the condition is mentor's -- SelfAttacks,
-- EveryTime, rule 702.39a stating no once-a-turn narrowing.
--
-- The target slot is Pool.Creatures ("creature", drawn from the battlefield by
-- CR 109.2) narrowed by Filter.ControlledByDefendingPlayer ("defending player
-- controls", CR 508.5). One atom rather than ControlledBy Opponent, which CR
-- 506.2a makes too wide: with three seats only one opponent is the defending
-- player, and CR 508.5a says an ability means that one.
--
-- ONE clause holding BOTH effects, under one Optionality.Optional. That is CR
-- 608.2e's span: rule 702.39a prints one "may", and its "if you do" makes the
-- untap conditional on the same answer -- so this is one question, not two.
--
-- The order is rule 702.39a's -- require, then untap -- and it is not observable
-- either way: both apply while the ability resolves, and CR 509.1a reads the
-- board only at the declare blockers step. The printed reminder text says
-- "untap and block" for the same one instruction.
--
-- The requirement's ATTACKER is Binding.triggerSource and never a target: rule
-- 702.39a says "this creature", and CR 115.10a makes a named object not a target
-- (crew's argument). Its BLOCKER is the target slot, so a creature that has
-- become an illegal target by resolution (CR 608.2b) leaves both effects with an
-- empty set and provoke does nothing.
--
-- Duration.UntilEndOfCombat is "this combat" -- CR 500.5a puts the end at the end
-- of the combat PHASE, which is where CR 509.1c's requirement stops mattering
-- anyway.
--
-- Single mode, ChooseExactly 1, no intervening clause: the only things rule
-- 702.39a leaves to choose are the target and the "may".
provoke :: TriggeredAbility Card
provoke =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Optional Nothing (Seq.fromList [requirement, untap]))) (Map.singleton provokeTarget slot)))
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

-- The slot rule 702.39a's one target is chosen into, declared by the ability that
-- reads it for mentorTarget's reason.
provokeTarget :: SlotName.SlotName
provokeTarget = SlotName.MkSlotName (Text.pack "provoked")

-- CR 702.112a: when this creature deals combat damage to a player, if it isn't
-- renowned, put N +1/+1 counters on it and it becomes renowned. Rule 702 states
-- it as a triggered ability, minted here like its siblings in `abilitiesFor`.
--
-- Poisonous' condition with a plain placement: rule 702.112a's event is the
-- bearer's combat damage to a player (SelfDealsCombatDamageToPlayer, rule
-- 702.70a's) and its counters go on the bearer (Effect.PutCounters against the
-- reserved Binding.triggerSource slot). No marking opcode, unlike training and
-- evolve one rule apiece away: rule 702.112a's own marker is the DESIGNATION the
-- next clause gives, which a later ability reads off the permanent rather than off
-- an event. Not mentor's target slot either:
-- rule 702.112a says "it", and CR 115.10a makes a named object not a target.
--
-- THE INTERVENING "IF" is what this row adds -- the first minted ability with one.
-- CR 603.4 checks it as the ability would trigger AND CR 608.2a again as it
-- resolves, which is exactly what CR 702.112c leans on: with two instances the
-- first to resolve designates the creature, and the second finds it renowned and
-- is removed from the stack. Pawl.Types.Clause's printed "may"/"if" (CR 608.2e)
-- would check only on resolution and let the trigger onto the stack regardless,
-- which rule 603.4 forbids.
--
-- Quantity.HasDesignation Renowned AtMost 0 is "isn't renowned": the designation read as a 0/1
-- off the object the condition is evaluated against, which for a triggered ability
-- is CR 113.7a's source (Pawl.Engine.Stack's OfTrigger arm).
--
-- ONE clause holding BOTH effects, in rule 702.112a's printed order and under one
-- Optionality.Mandatory -- the rule prints one sentence and no "may". Nobody gets
-- priority between them (CR 117.3b), and no CR 614.16 counter replacement in the
-- pool reads the designation, so the order is unobservable today.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability is
-- placed -- rule 702.112a leaves nothing to choose.
renown :: Natural -> TriggeredAbility Card
renown n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [grow, designate]))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening =
        Just (Condition.Compares (Compares.MkCompares (Quantity.HasDesignation Designation.Renowned) Comparison.AtMost (Quantity.Literal 0))),
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    grow = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal (toInteger n)) (ObjectRef.InSlot Binding.triggerSource))
    designate = Effect.Designate (Designate.MkDesignate Designation.Renowned Binding.triggerSource)

-- CR 702.105a: whenever this creature attacks the player with the most life or
-- tied for most life, put a +1/+1 counter on it. Rule 702 states it as a
-- triggered ability, minted like the rest of this roster.
--
-- The whole of the keyword is in the CONDITION, which is why the payload below is
-- renown's first effect with no second: rule 702.105a's "the player with the most
-- life or tied for most life" is a fact about the BOARD read against the
-- declaration, and TriggerCondition.SelfAttacksPlayerWithMostLife carries it.
--
-- NOT an intervening "if" (CR 603.4), which is where renown puts its comparison:
-- rule 702.105a prints no "if", and CR 608.2a would re-check an intervening one on
-- resolution -- so an opponent gaining life in response would wrongly remove the
-- ability from the stack.
--
-- "It" is the bearer, CR 113.7a's source, already reserved in
-- Binding.triggerSource -- so nothing is bound off the event and this ability
-- needs no opcode of its own.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability is
-- placed -- rule 702.105a leaves nothing to choose.
dethrone :: TriggeredAbility Card
dethrone =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfAttacksPlayerWithMostLife,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton grow))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    grow = Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (ObjectRef.InSlot Binding.triggerSource))

-- CR 702.79a: persist. "When this permanent is put into a graveyard from the
-- battlefield, if it had no -1/-1 counters on it, return it to the battlefield
-- under its owner's control with a -1/-1 counter on it" -- CR 700.4's dies, which
-- is why the condition below is SelfDies.
persist :: TriggeredAbility Card
persist = returns CounterKind.MinusOneMinusOne

-- CR 702.93a: undying, persist's mirror -- rule 702.79a's sentence in +1/+1
-- counters.
undying :: TriggeredAbility Card
undying = returns CounterKind.PlusOnePlusOne

-- The sentence both keywords state, in the counter kind that tells them apart.
-- ONE body rather than two, because rules 702.79a and 702.93a differ in nothing
-- else: the kind decides which counter the permanent comes back with AND which
-- one the "if" clause looks for, and it is the same kind in both places.
--
-- TWO INCARNATIONS, and the split is the whole reason this works: CR 400.7 mints
-- a fresh object when the permanent dies, so the ability's SOURCE (CR 113.7a, the
-- permanent as it was on the battlefield) and the CARD IT MOVES (the graveyard
-- incarnation) are different ids. The intervening "if" is evaluated against the
-- source, read from CR 608.2h last known information -- which is what "it HAD no
-- counters on it" asks for -- while the move names Binding.became, the arriving
-- incarnation CR 400.7e binds. Endless Cockroaches proves the second half and
-- Promising Duskmage the first.
--
-- CR 603.4's intervening "if" and not a Clause condition: the ability must not
-- trigger at all when the permanent died with a counter on it, and it is checked
-- AGAIN on resolution (CR 608.2a). AtMost 0 is "had no counters", the shape
-- renown's clause takes.
--
-- The counter rides the ENTRY (EntryRiders.counters, CR 122.6a) rather than
-- following as a second effect, so the permanent is never on the battlefield
-- without it -- which for persist is the difference between a 2/2 coming back as
-- a 1/1 and a 2/2 that briefly was not.
--
-- `underOwner` is rule 702.79a's "under its owner's control", which CR 110.2a
-- otherwise answers with the ability's controller: a permanent stolen by CR 613's
-- layer 2 dies under the thief's control (CR 603.3a hands them the trigger) and
-- still comes back to its owner.
--
-- No stated origin zone and no bound destination slot: neither rule says where
-- the card is returned FROM -- which is what the CR 113.6m field records, and
-- this is a battlefield ability whatever it moves -- and nothing later in the
-- resolution names what arrived.
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability is
-- placed -- neither rule leaves anything to choose.
returns :: CounterKind.CounterKind Keyword.Keyword -> TriggeredAbility Card
returns kind =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton back))) Map.empty))
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
                EntryRiders.transformed = False,
                EntryRiders.counters = Map.singleton kind 1,
                EntryRiders.underOwner = True,
                EntryRiders.exiledFaceDown = False,
                EntryRiders.faceDown = False
              }
            Nothing
            Nothing
            LibraryPlacement.defaultValue
        )

-- CR 702.135a: afterlife N. "When this permanent is put into a graveyard from
-- the battlefield, create N 1/1 white and black Spirit creature tokens with
-- flying." The same CR 700.4 dies event `returns` above watches, so the
-- condition is TriggerCondition.SelfDies.
--
-- No intervening "if" and nothing bound: rule 702.135a states one sentence with
-- no condition, and unlike undying and persist it never touches the permanent
-- that died -- so neither Binding.triggerSource nor Binding.became appears, and
-- the ability is indifferent to the CR 400.7 incarnation split.
--
-- CR 111.2 gives the tokens to the ability's controller, which for a dies
-- trigger is whoever controlled the permanent as it left (CR 603.3a). That is
-- rule 111.2 rather than the rider undying and persist need:
-- EntryRiders.underOwner is inert under a Create, since a token's owner and its
-- controller are the same player by that rule.
--
-- THE TOKEN IS MINTED HERE, not carried in card data, on Pawl.Engine.Ring's
-- terms: its characteristics are printed in the comprehensive rules rather than
-- on Ministrant of Obligation. CR 111.3 is what makes rule 702.135a's own
-- adjectives the token's whole text; both colours ride the colorIndicator,
-- since rule 702.135a says "white and black" and a token has no mana cost to
-- read a colour off (CR 105.2).
--
-- CR 612.2a's text change reaches the Spirit written here, even though the mint
-- runs after the CR 613 layer fold: layer 3 records its pairs on the projection
-- and Pawl.Engine.Projection.mintedTriggeredAbilitiesOf applies them to whatever
-- this returns. Proven by Pawl.ResolveSpec's "CR 612.2a whole card: an evolved
-- Ministrant of Obligation leaves Elves".
--
-- Single mode, no targets, ChooseExactly 1, so nothing is asked as the ability
-- is placed -- rule 702.135a leaves nothing to choose.
afterlife :: Natural -> TriggeredAbility Card
afterlife n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton spawn))) Map.empty))
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
                  EntryRiders.transformed = False,
                  EntryRiders.counters = Map.empty,
                  EntryRiders.underOwner = False,
                  EntryRiders.exiledFaceDown = False,
                  EntryRiders.faceDown = False
                },
            Create.slot = Nothing
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
              Face.alternativeCosts = [],
              Face.costReductions = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.blockPermissions = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.sacrificeRestrictions = [],
              Face.untapRestrictions = [],
              Face.attackCosts = [],
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
-- ONE CLAUSE and not two, and no branching opcode: rule 702.123a prints CR
-- 118.12a's rewriting already performed, so CR 118.12 makes the counters a COST
-- paid as the ability resolves (Pawl.Types.PayGate, over
-- CostComponent.PutPlusOneCountersOnThis) and the clause's own effects are its
-- "if you don't" branch. The counters go on the ability's SOURCE (CR 113.7a),
-- which is what rule 702.123a's "it" names, so no slot is needed for them.
--
-- Optionality.Mandatory, because the printed "you may" IS the gate's offer.
-- Marking the clause optional as well would ask twice and let a player decline
-- both halves, which rule 702.123a does not allow.
--
-- Binding.you is CR 109.5's answer for a triggered ability -- "the controller of
-- the object when the ability triggered" -- and CR 111.2 gives that same player
-- the tokens.
--
-- THE TOKEN IS MINTED HERE for afterlife's reason: rule 702.123a prints its
-- characteristics, so they are the rulebook's rather than the card's. A CR 612.2a
-- text change of the word "Servo" reaches it for afterlife's reason, through the
-- same mintedTriggeredAbilitiesOf.
--
-- Single mode, no targets, ChooseExactly 1: pay or not is the only choice.
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
    clause = Clause.MkClause Nothing Optionality.Mandatory (Just gate) (Seq.singleton spawn)
    gate =
      PayGate.MkPayGate
        { PayGate.payer = Binding.you,
          PayGate.cost =
            Cost.MkCost
              { -- CR 118.5, crew's note above: no mana part is `Just` an empty
                -- one, never the Nothing that means unpayable.
                Cost.mana = Just (ManaCost.MkManaCost []),
                Cost.components = [CostComponent.PutPlusOneCountersOnThis n]
              },
          -- Rule 702.123a prints CR 118.12a's rewriting already done, so the
          -- Servos are the "if you don't" branch.
          PayGate.branch = PayBranch.IfNotPaid,
          -- PayObligation.Optional: rule 702.123a prints the "may" itself. No
          -- offeredAt, one clause making its own offer.
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
                  EntryRiders.transformed = False,
                  EntryRiders.counters = Map.empty,
                  EntryRiders.underOwner = False,
                  EntryRiders.exiledFaceDown = False,
                  EntryRiders.faceDown = False
                },
            Create.slot = Nothing
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
              Face.alternativeCosts = [],
              Face.costReductions = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.blockPermissions = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.sacrificeRestrictions = [],
              Face.untapRestrictions = [],
              Face.attackCosts = [],
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
-- The bearer never appears in the payload, so the CR 400.7 incarnation split
-- undying and persist care about is inert here: the ability moves the card its
-- TARGET names, which is chosen when the trigger is put on the stack (CR 603.3d)
-- and long after the bearer's own trip to the graveyard.
--
-- A CardsInGraveyard pool scoped to You is rule 702.46a's "your graveyard", read
-- as CR 115.2's clause (a) -- Raise Dead's pool, and the reason the pool carries
-- a GraveyardScope rather than a Filter: CR 108.4 gives a card in a graveyard no
-- controller at all.
--
-- The Filter is the rest of the printed phrase. Filter.ManaValueAtMost is CR
-- 202.3's mana value, and the bound is the keyword's own N -- so a mint that
-- dropped it would return Shimatsu the Bloodcloaked to a soulshift 3 trigger.
-- Nothing excludes the bearer: rule 702.46a does not say "another", and its own
-- graveyard incarnation is an ordinary candidate whenever it matches -- which no
-- printing reaches, all 26 pairing an N below their own mana value.
--
-- No stated origin zone on the move, for the reason Pawl.Types.Effect's
-- MoveToZone gives: "from your graveyard" is stated in the TARGET's pool, where
-- choosing the target enforces it. The EntryRiders are inert for a hand
-- destination and nothing later reads what arrived, so no slot is bound.
--
-- Single mode, ChooseExactly 1: the target and the "may" are all rule 702.46a
-- leaves to choose.
soulshift :: Natural -> TriggeredAbility Card
soulshift n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Optional Nothing (Seq.singleton back))) (Map.singleton soulshiftTarget slot)))
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
                EntryRiders.transformed = False,
                EntryRiders.counters = Map.empty,
                EntryRiders.underOwner = False,
                EntryRiders.exiledFaceDown = False,
                EntryRiders.faceDown = False
              }
            Nothing
            Nothing
            LibraryPlacement.defaultValue
        )

-- The slot rule 702.46a's one target is chosen into, declared by the ability that
-- reads it for mentorTarget's reason.
soulshiftTarget :: SlotName.SlotName
soulshiftTarget = SlotName.MkSlotName (Text.pack "soulshifted")

-- CR 702.55a: haunt. "When this permanent is put into a graveyard from the
-- battlefield, exile it haunting target creature." Soulshift's shape -- the CR
-- 700.4 dies event and one target slot -- with the clause mandatory, since rule
-- 702.55a states no "may".
--
-- ONLY the permanent sentence. Rule 702.55a's other one, haunt on an instant or
-- sorcery, is not minted (#1404).
--
-- THE CARD, NOT THE PERMANENT (CR 400.7): rule 702.55a's "exile IT" is the
-- graveyard incarnation the death minted, which is Binding.became -- undying's
-- and persist's split, and the reason this cannot name Binding.triggerSource.
-- The ability's SOURCE is the permanent as it was on the battlefield, and that
-- object no longer exists to exile.
--
-- A bare Creatures pool with no Filter is rule 702.55a's whole phrase: "target
-- creature", nothing excluded. The bearer cannot be among the candidates anyway,
-- since it is a card in a graveyard by the time the trigger is put on the stack.
--
-- Effect.ExileHaunting rather than a MoveToZone to Zone.Exile, because the move
-- is only half of it: CR 702.55b's link from the exiled card to the object
-- targeted is what the exile-zone half of the card reads, and only that opcode
-- writes it.
--
-- Single mode, ChooseExactly 1: the target is all rule 702.55a leaves to choose.
haunt :: TriggeredAbility Card
haunt =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton exile))) (Map.singleton hauntTarget slot)))
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
-- One mode, one MANDATORY clause: the "you may" governs the CASTING alone, and
-- that is Prompt.OfferedCast's own question -- Pawl.Engine.Battle.siegeDefeat
-- makes the same call for the same reason, and for the same rule (CR 608.2g).
-- Marking the clause optional would raise a second prompt for one printed "may".
--
-- ONE effect and no move ahead of it, which is where this parts from siegeDefeat:
-- CR 701.20b says revealing does not move the card, so what may be cast is the
-- very object the ability triggered from -- Binding.triggerSource (CR 113.7a),
-- not Binding.became. The card is in its owner's hand throughout.
--
-- The cost rides the OFFER (CastOffer.payingInstead) rather than the card,
-- because CR 118.9 makes it an alternative cost applied to the spell "from
-- another effect": Thunderous Wrath in a hand nobody drew this turn costs
-- {4}{R}{R}, and nothing about the card changes when it does not.
miracle :: Cost Keyword -> TriggeredAbility Card
miracle cost =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfRevealedForMiracle,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton offer))) Map.empty))
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
            OfferCast.optionality = Optionality.Optional,
            OfferCast.offer = CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = False, CastOffer.payingInstead = Just cost}
          }

-- CR 702.94a's STATIC half, read as the one thing its reader needs: what this
-- card would cost if its controller took the reveal. Nothing when the card has no
-- miracle ability at all, which is also "no window to open".
--
-- flashbackCost's shape exactly, including the wildcard -- this asks about ONE
-- constructor rather than classifying every keyword -- and asked of the card's
-- PRINTED keywords for that function's reason: rule 702.94a's abilities function
-- in the hand (CR 113.6b), where no pool effect changes a card's keywords (#160).
--
-- ONE cost per card, morphCost's shape: the ascending-least of the miracle
-- costs the card prints, so the reveal opens a window at that price and no
-- other.
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

-- CR 702.63a's SECOND and THIRD abilities. Rule 702.63a states three and the
-- first is mintedReplacementsFor's, so vanishing is the first keyword here whose
-- rule text spans both mints. Bushido's "one instance, two abilities" shape, and
-- another of abilitiesFor's `concat` arms.
--
-- Ordered as rule 702.63a prints them, which is also the order they fire in: the
-- upkeep removal is what takes the last counter off, and the sacrifice watches
-- that removal.
vanishing :: [TriggeredAbility Card]
vanishing = [vanishingUpkeep, vanishingLastCounter]

-- "At the beginning of your upkeep, if this permanent has a time counter on it,
-- remove a time counter from it."
--
-- TurnScope.ControllersTurn is rule 702.63a's "YOUR upkeep": CR 603.3a makes the
-- ability's controller the permanent's controller, so the scope reads off the
-- same player the rule's "you" names, and an opponent's upkeep is not this
-- trigger.
--
-- THE INTERVENING "IF" is renown's, one quantity over: rule 702.63a prints "if",
-- so CR 603.4 keeps the ability off the stack on an upkeep where the counters
-- are already gone. Quantity.ObjectCounters reads the object the condition is
-- evaluated against, which for a triggered ability is CR 113.7a's source; AtLeast
-- 1 is the rule's "has a time counter on it".
--
-- CR 608.2a's re-check comes with the clause and is unobservable here: an
-- instance that resolved with the condition false would remove nothing, and
-- Effect.RemoveCounters raises no event for a removal of nothing -- so no
-- vanishing board tells the two readings apart.
--
-- Effect.RemoveCounters against Binding.triggerSource for renown's reason: rule
-- 702.63a says "from it", and CR 115.10a makes a named object not a target.
--
-- ONE counter per instance, not per counter present: rule 702.63a removes a
-- single one, and CR 702.63c is what makes a second instance remove a second.
vanishingUpkeep :: TriggeredAbility Card
vanishingUpkeep =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn),
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening =
        Just (Condition.Compares (Compares.MkCompares (Quantity.ObjectCounters CounterKind.Time) Comparison.AtLeast (Quantity.Literal 1))),
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.Time (Quantity.Literal 1) Binding.triggerSource)

-- "When the last time counter is removed from this permanent, sacrifice it."
--
-- Pawl.Engine.Battle.siegeDefeat's condition with a different kind and a
-- different payload: CR 310.12b and rule 702.63a ask the same question of
-- Object.counters, which is why TriggerCondition.SelfLastCounterRemoved needed no
-- widening for this row.
--
-- Watches the REMOVAL and not the count, so a permanent whose time counters were
-- all removed before it entered has nothing to trigger -- and an upkeep that
-- removes nothing (the intervening "if" above being false) raises no
-- GameEvent.CountersRemoved to match either.
--
-- Effect.Sacrifice, never Destroy: CR 701.21a says a sacrifice is not a
-- destruction, so an indestructible permanent with vanishing still goes.
vanishingLastCounter :: TriggeredAbility Card
vanishingLastCounter =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfLastCounterRemoved CounterKind.Time,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect = Effect.Sacrifice Binding.triggerSource

-- CR 702.32a's SECOND ability: "at the beginning of your upkeep, remove a fade
-- counter from this permanent. If you can't, sacrifice the permanent."
--
-- vanishingUpkeep's trigger condition exactly -- rule 702.32a prints the same
-- "your upkeep", so TurnScope.ControllersTurn for that arm's reason -- and
-- everything after it differs, which is why fading is not vanishing with a
-- counter kind swapped. Rule 702.32a states NO intervening "if", so this fires on
-- every one of its controller's upkeeps including the one where the pile is
-- already empty; that firing is the whole of the rule's sacrifice.
--
-- ONE ability with TWO clauses, not two: rule 702.32a's "if you can't" is a
-- second sentence of the same ability. The alternative -- two triggers under
-- complementary intervening "if"s -- is not equivalent, since CR 603.4 re-checks
-- each at resolution: a fade counter removed in response would leave the removal
-- half doing nothing, with no sacrifice half to have triggered, and the permanent
-- would survive an upkeep rule 702.32a takes it on.
--
-- THE CLAUSES ARE INVERTED against the printed order, and the printed order is
-- unwritable: a gate is read as its clause is REACHED (Pawl.Engine.Resolve's
-- gateHolds, CR 608.2c's written order), so a sacrifice clause standing after the
-- removal would read a pile the removal had already emptied and take the
-- permanent on the very upkeep the rule keeps it. Asking "are there none?" first
-- reads the count rule 702.32a's "can't" is about. Observably equivalent to the
-- printed order because nothing runs between two clauses of one resolution: no
-- player gets priority until it finishes (CR 117.3b), and CR 704.3 hangs both the
-- state-based actions and the waiting triggers off that same moment.
--
-- Both clauses Mandatory: rule 702.32a prints no "may" on either sentence.
--
-- Effect.Sacrifice, never Destroy, and Binding.triggerSource for the reasons
-- vanishing's two arms give.
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
        (Just (Condition.Compares (Compares.MkCompares counted Comparison.AtMost (Quantity.Literal 0))))
        Optionality.Mandatory
        Nothing
        (Seq.singleton (Effect.Sacrifice Binding.triggerSource))
    removeClause =
      Clause.MkClause
        (Just (Condition.Compares (Compares.MkCompares counted Comparison.AtLeast (Quantity.Literal 1))))
        Optionality.Mandatory
        Nothing
        (Seq.singleton (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.Fade (Quantity.Literal 1) Binding.triggerSource)))

-- CR 702.43a's SECOND ability: "when this permanent is put into a graveyard from
-- the battlefield, you may put a +1/+1 counter on target artifact creature for
-- each +1/+1 counter on this permanent." Mentor's shape -- one target slot, one
-- counter-placing effect -- with a "may" and a counted quantity where mentor has a
-- mandatory clause and a literal 1, and a plain Effect.PutCounters where that one
-- has CR 702.134c's marker to record.
--
-- TriggerCondition.SelfDies is CR 700.4's battlefield-to-graveyard pair, which is
-- what rule 702.43a's longhand spells out. So the ability's source is the
-- DEPARTING incarnation, the id GameState.lastKnown files under -- which is what
-- makes the count below answerable at all.
--
-- THE COUNT is Quantity.ObjectCounters, which names no object and takes the one
-- the evaluation is aimed at: CR 113.7a's source, read through
-- Projection.viewWithLastKnown. That is CR 608.2h doing the work -- the permanent
-- is in a graveyard by the time this resolves and CR 122.2 made its counters
-- cease with it, so the last known record is the only place the number still is.
-- Promising Duskmage's intervening "if" reads the same record the same way; this
-- is the first to read it at RESOLUTION, which Pawl.TriggerSpec's modularSpec
-- proves by emptying the record while the trigger sits on the stack.
--
-- Optionality.Optional is rule 702.43a's "you may", asked of the controller as
-- the ability resolves (Pawl.Engine.Resolve.exercises). A REAL choice: declining
-- with a legal target on the board leaves the counters nowhere, which is why
-- nothing here elides it.
--
-- The target is Pool.Creatures narrowed to artifacts -- rule 702.43a's "target
-- artifact creature", CR 109.2 drawing the candidates from the battlefield. No
-- controller conjunct, because the rule states none, and no `Not IsSource`: the
-- bearer is in a graveyard and is not a candidate anyway.
--
-- ZERO counters is an ordinary answer rather than a failure, the position
-- Quantity.OpponentsAttacked takes: a modular permanent whose counters were all
-- removed before it died still triggers, still targets, and puts nothing on.
modular :: TriggeredAbility Card
modular =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDies,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Optional Nothing (Seq.singleton effect))) (Map.singleton modularTarget slot)))
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

-- The slot rule 702.43a's one target is chosen into, mentorTarget's position and
-- for its reason.
modularTarget :: SlotName.SlotName
modularTarget = SlotName.MkSlotName (Text.pack "modularRecipient")
