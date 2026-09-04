module Pawl.Engine.Resolve where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Amass as Amass
import qualified Pawl.Engine.Attach as Attach
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Blight as Blight
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Coin as Coin
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.CounterRestriction as CounterRestriction
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Daytime as Daytime
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.Dungeon as Dungeon
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Goad as Goad
import qualified Pawl.Engine.Initiative as Initiative
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.MoveDuration as MoveDuration
import qualified Pawl.Engine.OutsideTheGame as OutsideTheGame
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Plot as Plot
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.QuantitySlot as QuantitySlot
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Restamp as Restamp
import qualified Pawl.Engine.Ring as Ring
import qualified Pawl.Engine.Room as Room
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Star as Star
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Amass as Amass.Type
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.CandidateCost as CandidateCost
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CarryOver as CarryOver
import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.Clause as Clause
import Pawl.Types.ClauseIndex (ClauseIndex)
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.CoinReading as CoinReading
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureDestination as ConjureDestination
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndTurnSignal as EndTurnSignal
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.HandActionPerformer as HandActionPerformer
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeLossCause as LifeLossCause
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.ModeIndex (ModeIndex)
import Pawl.Types.ModeInstance (ModeInstance)
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveDuration as MoveDuration.Type
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.ObjectRef (ObjectRef)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.OrElse as OrElse
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import Pawl.Types.PlayerRef (PlayerRef)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity.Type
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.ReturnWatch as ReturnWatch
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import Pawl.Types.SlotArity (SlotArity)
import qualified Pawl.Types.SlotArity as SlotArity
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR
import qualified Pawl.Types.ZoneScope as ZoneScope

-- The read side of the D4 dataflow lint: WHICH slots, and HOW MANY recipients
-- apiece (a slot read through an ObjectRef can hold CR 601.2c's "up to two").
-- These combinators join, taking the conservative arity wherever two reads
-- disagree; written out rather than left to Map's left-biased Monoid.
joinTwo :: Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity
joinTwo = Map.unionWith min

joinSlots :: [Map.Map SlotName SlotArity] -> Map.Map SlotName SlotArity
joinSlots = foldr joinTwo Map.empty

oneSlot :: SlotName -> Map.Map SlotName SlotArity
oneSlot slot = Map.singleton slot SlotArity.One

insertOne :: SlotName -> Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity
insertOne slot = joinTwo (oneSlot slot)

-- The slots a Quantity reads, on both halves of Pawl.Types.SlotName's one flat
-- namespace. Quantity.objectSlots names an OBJECT and evaluates against it, so a
-- slot naming several leaves Pawl.Engine.Filter.slotOneObject with nothing to
-- pick and the whole number unanswered -- SlotArity.One. Every other slot
-- QuantitySlot.slots reports is a Quantity.InSlot, which reads the slot's AMOUNT
-- instead (Pawl.Engine.Binding.amountOf) and cannot be damaged by a plural slot
-- at all -- SlotArity.Amount, an entry stating a read without claiming an arity.
--
-- Both halves are reported so that the KEYS stay QuantitySlot.slots' whole answer:
-- an InSlot read is a read, and the D4 dataflow lint counts it; see #2774.
-- Map.union is left-biased, so a slot read both ways is One.
quantitySlots :: Quantity.Type.Quantity -> Map.Map SlotName SlotArity
quantitySlots quantity =
  Map.union
    (Map.fromSet (const SlotArity.One) (Quantity.objectSlots quantity))
    (Map.fromSet (const SlotArity.Amount) (QuantitySlot.slots quantity))

-- The Quantities an entry rider carries: CR 122.6's count per counter kind, which
-- a card may write as anything a Quantity spells. A position the three walkers
-- below would otherwise pass over -- every arm that reads a rider matches it as
-- `_` -- so the reads are spelled out here and each walker goes through this.
riderQuantities :: EntryRiders.EntryRiders Quantity.Type.Quantity -> [Quantity.Type.Quantity]
riderQuantities = Map.elems . EntryRiders.counters

-- The slot an entry rider READS, which is CR 509.4's blocking rider and only it:
-- every other rider is a flag or a Quantity (riderQuantities above). Read singly
-- -- CR 509.4 names one attacking creature.
--
-- BOTH opcodes reach it, and both apply it: a Create hands its tokens to
-- Pawl.Engine.Combat.putOntoBattlefieldBlocking from the minting loop (Flash
-- Foliage), a MoveToZone hands the card it moved to the same function from
-- moveOne (Aetherplasm). What stays inert is the rider on a destination other
-- than the battlefield, which Pawl.CardSpec lints.
riderSlots :: EntryRiders.EntryRiders count -> Map.Map SlotName SlotArity
riderSlots = maybe Map.empty oneSlot . EntryRiders.blocking

-- The slots a PlayerRef reads. Five arms name one: EachPlayerExcept, InSlot,
-- ControllerOfBound and Attacking at arity One, EachInSlot at arity Many. The
-- other four name none, and the arms below carry the reason for each arity that
-- is not self-evident.
playerRefSlots :: PlayerRef -> Map.Map SlotName SlotArity
playerRefSlots ref = case ref of
  PlayerRef.EachPlayer -> Map.empty
  -- The excluded seat is one player, so one slot read singly.
  PlayerRef.EachPlayerExcept slot -> Map.singleton slot SlotArity.One
  PlayerRef.Relative _ -> Map.empty
  PlayerRef.InSlot slot -> Map.singleton slot SlotArity.One
  -- Read at arity MANY, which is the whole of what parts it from the arm above.
  PlayerRef.EachInSlot slot -> Map.singleton slot SlotArity.Many
  PlayerRef.Specific _ -> Map.empty
  PlayerRef.Candidate -> Map.empty
  -- Read at arity one: a slot naming several objects names no one controller.
  PlayerRef.ControllerOfBound slot -> Map.singleton slot SlotArity.One
  -- Read at arity one for that arm's reason: a slot naming several players names
  -- no one player to have been attacked.
  PlayerRef.Attacking (AttackingPlayers.MkAttackingPlayers _ slot) -> Map.singleton slot SlotArity.One

-- The slots an AffectedPlayers reads. Only Named does, and only ever one player.
affectedPlayersSlots :: AffectedPlayers.AffectedPlayers SlotName -> Map.Map SlotName SlotArity
affectedPlayersSlots affected = case affected of
  AffectedPlayers.Scoped _ -> Map.empty
  AffectedPlayers.Named slot -> Map.singleton slot SlotArity.One

-- The slots a Chooser reads: only BoundInSlot, and only ever one player.
chooserSlots :: Chooser.Chooser -> Map.Map SlotName SlotArity
chooserSlots chooser = case chooser of
  Chooser.TheController -> Map.empty
  Chooser.EachInScope -> Map.empty
  Chooser.BoundInSlot slot -> Map.singleton slot SlotArity.One

-- The slots a ZoneScope reads. Only InSlot names one, and at arity Many: the
-- reader takes the whole recipient set, so "each of up to two target players'
-- graveyards" would be seen whole.
zoneScopeSlots :: ZoneScope.ZoneScope -> Map.Map SlotName SlotArity
zoneScopeSlots scope = case scope of
  ZoneScope.Scoped _ -> Map.empty
  ZoneScope.InSlot slot -> Map.singleton slot SlotArity.Many

-- The slots an ObjectRef reads. InSlot names one directly, and
-- EachCardInGraveyard and EachCardInHand name one through their scope; the other
-- sweeping arms name none. Reporting a scope's slot does not make the SWEPT CARDS targets -- CR
-- 115.10a needs the word "target" against them, and a graveyard scope says it
-- against the PLAYER -- so what CR 608.2b judges is still the card's own target
-- slot, holding that player, and not this read.
--
-- The PlayerRefs the four per-player arms hold are taken from
-- objectRefPlayerRefs below rather than arm by arm, so no arm of the case names
-- one.
objectRefSlots :: ObjectRef -> Map.Map SlotName SlotArity
objectRefSlots ref = joinTwo (joinSlots (fmap playerRefSlots (objectRefPlayerRefs ref))) $ case ref of
  ObjectRef.InSlot slot -> Map.singleton slot SlotArity.Many
  ObjectRef.EachMatching _ -> Map.empty
  -- The sweeping arms that DO name a slot are this one and EachCardInHand below:
  -- CR 400.1's per-player zones leave "whose" to be said, and Angel of Finality
  -- says it by pointing at the player another slot of the same announcement
  -- targets. Reported, not dropped, because the D4 dataflow lint reads this: a
  -- card whose ONLY use of that slot is the scope would otherwise declare a
  -- target nothing reads.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard scope _) -> zoneScopeSlots scope
  ObjectRef.EachCardInYourHand -> Map.empty
  -- The arm above's scope over the other per-player zone, reported for its
  -- reason: Amnesia's ONLY use of its target slot is this scope, so dropping the
  -- read would have the D4 dataflow lint call that target unread.
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand scope _) -> zoneScopeSlots scope
  -- EachCardInYourHand's answer over the other hidden per-player zone: the
  -- seat is CR 109.5's "you", so no slot names it.
  ObjectRef.EachCardInYourLibrary _ -> Map.empty
  ObjectRef.EachCardExiledWithSource {} -> Map.empty
  ObjectRef.EachSpell _ -> Map.empty
  ObjectRef.EachOnStack _ -> Map.empty
  ObjectRef.EachPlayer -> Map.empty
  ObjectRef.EachOpponent -> Map.empty
  -- The seat comes from the source's own entry choice (CR 614.12a), not a slot.
  ObjectRef.ChosenPlayer -> Map.empty
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary _ count) -> quantitySlots count
  -- The arm above's count, and no more: the SEAT is objectRefPlayerRefs' half.
  -- What a MATCH is is a Filter, and no arm here reports the slots a Filter
  -- reads, for the reason the header states.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil _ _ count) -> quantitySlots count
  -- Both halves name a slot: WHO CHOOSES through the Chooser, and WHOSE
  -- graveyards through the scope -- reported for EachCardInGraveyard's reason,
  -- since Grasping Tentacles' scope is a read of the slot its own mill targets.
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser scope _) -> joinTwo (chooserSlots chooser) (zoneScopeSlots scope)
  -- CR 402.3: the choosers own the hands, so the PlayerRef is the whole read --
  -- reported by objectRefPlayerRefs rather than here.
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand _ _) -> Map.empty
  -- The first of the two arms whose CANDIDATES come from a slot -- the plural is
  -- below -- where the two chosen arms above name a slot only through their
  -- chooser. Reported for EachCardInGraveyard's
  -- reason -- the D4 dataflow lint reads this, and a group one clause binds and a
  -- later one reads is exactly the dataflow that lint checks. Many, not One,
  -- which is the arity InSlot reports of the same binding: the ref reads every
  -- member of the group to offer them.
  --
  -- Joined with the COUNT's own slots, TopOfLibrary's arm above and for its
  -- reason; the CHOOSER's are the generic playerRefSlots fold this case is joined
  -- into, which is what makes Animal Magnetism's ChooseOpponent slot a read.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong slot _ count _) -> joinTwo (Map.singleton slot SlotArity.Many) (quantitySlots count)
  -- The arm above's read, for its reasons: the candidates come from a slot, and
  -- the ref reads every member of the group to match them.
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong slot _) -> Map.singleton slot SlotArity.Many
  -- The seats whose hands randomness reads are ChosenCardInHand's, and reported
  -- where that arm's are.
  ObjectRef.RandomCardInHand _ -> Map.empty
  -- EachMatching's answer: the candidates come off the battlefield, so no slot
  -- names them and the chooser is CR 608.2c's resolving controller.
  ObjectRef.AnyNumberMatching _ -> Map.empty
  -- The arm above's answer, for its reason: the candidates come off the
  -- battlefield, so no slot names them and the chooser is CR 608.2c's resolving
  -- controller.
  ObjectRef.ChosenPermanent _ -> Map.empty
  -- The arm above's answer, for its reason: neither the source nor the
  -- candidates come out of a slot.
  ObjectRef.SourceAndChosenPermanent _ -> Map.empty

-- The Quantities an ObjectRef carries: the two library walks' counts.
-- Exhaustive, no wildcard, and every payload destructured positionally rather
-- than as `{}`: slotsAreExhaustive, readsX and Pawl.CardSpec's Count traversal
-- all reach a nested Quantity through this, over effectObjectRefs below rather
-- than arm by arm, so this is the one place a payload gaining a Quantity field
-- has to be revisited -- a `{}` here would keep compiling, which is the shape
-- that let a widened field go unread (#2729).
objectRefQuantities :: ObjectRef -> [Quantity.Type.Quantity]
objectRefQuantities ref = case ref of
  ObjectRef.InSlot _ -> []
  ObjectRef.EachMatching _ -> []
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard _ _) -> []
  ObjectRef.EachCardInYourHand -> []
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand _ _) -> []
  ObjectRef.EachCardInYourLibrary _ -> []
  ObjectRef.EachCardExiledWithSource _ -> []
  ObjectRef.EachSpell _ -> []
  ObjectRef.EachOnStack _ -> []
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  ObjectRef.ChosenPlayer -> []
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary _ count) -> [count]
  -- The arm above's count, measured in MATCHES rather than in cards.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil _ _ count) -> [count]
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard _ _ _) -> []
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand _ _) -> []
  -- How many cards are picked out of the group -- Ancestral Memories' printed
  -- two, the library walks' counts above being the only other ObjectRef numbers.
  -- A REGRESSION FENCE rather than proven behaviour: every count in the pool is a
  -- Literal, which reads no slot, so dropping this leaves the suite green.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong _ _ count _) -> [count]
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong _ _) -> []
  ObjectRef.RandomCardInHand _ -> []
  ObjectRef.AnyNumberMatching _ -> []
  ObjectRef.ChosenPermanent _ -> []
  ObjectRef.SourceAndChosenPermanent _ -> []

-- Every PlayerRef nested in one ObjectRef -- effectPlayerRefs' other half, and
-- the seat a per-player walk counts against. objectRefSlots takes its player
-- reads from here, so a reference dropped here stops being reported there.
--
-- No wildcard, objectRefQuantities' discipline above.
objectRefPlayerRefs :: ObjectRef -> [PlayerRef]
objectRefPlayerRefs ref = case ref of
  ObjectRef.InSlot _ -> []
  ObjectRef.EachMatching _ -> []
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard _ _) -> []
  ObjectRef.EachCardInYourHand -> []
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand _ _) -> []
  ObjectRef.EachCardInYourLibrary _ -> []
  ObjectRef.EachCardExiledWithSource _ -> []
  ObjectRef.EachSpell _ -> []
  ObjectRef.EachOnStack _ -> []
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  ObjectRef.ChosenPlayer -> []
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player _) -> [player]
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil player _ _) -> [player]
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard _ _ _) -> []
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand player _) -> [player]
  -- The seat that picks out of the group -- Animal Magnetism's opponent, and by
  -- default CR 608.2c's resolving controller.
  --
  -- The arm above's regression fence, for a different reason: the D4 dataflow lint
  -- subtracts a slot the mode's own ChooseOpponent DEFINES from both sides of its
  -- equality, so the only card whose chooser names a slot cannot observe this
  -- report. A chooser naming a DECLARED target slot would, and no printing writes
  -- one -- Pawl.Types.Chooser's BoundInSlot note says the same of its own.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong _ _ _ chooser) -> [chooser]
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong _ _) -> []
  ObjectRef.RandomCardInHand player -> [player]
  ObjectRef.AnyNumberMatching _ -> []
  ObjectRef.ChosenPermanent _ -> []
  ObjectRef.SourceAndChosenPermanent _ -> []

-- The refs a CR 707.10 answer names: rule 707.10d's candidates, and nothing for
-- the other two, neither of which describes anything.
copyTargetsRefs :: CopyTargets.CopyTargets -> [ObjectRef]
copyTargetsRefs targets = case targets of
  CopyTargets.Copied -> []
  CopyTargets.ChosenByController -> []
  CopyTargets.ForEach ref -> [ref]

-- Every ObjectRef this ONE effect holds, its own only: a nested effect's refs
-- are its own answer here, reached by whichever caller recurses.
--
-- The single enumeration of where an ObjectRef sits in an opcode. Its readers
-- -- slotsOf here, and slotsAreExhaustive, readsX and Pawl.CardSpec's Count
-- traversal below -- call this rather than naming the opcodes themselves, so
-- they cannot come to disagree about which opcodes hold a ref. The test a
-- reader can apply: no arm of any of those four names an ObjectRef field.
--
-- No wildcard, and the arms that hold a ref destructure positionally: a new
-- opcode the compiler forces, and so does a new FIELD on a payload that already
-- holds one. What neither the compiler nor this shape catches is an existing
-- field WIDENED to an ObjectRef, which is how a nested Quantity went unread
-- once (#2729). Two things pay for that: slotsOf's corpus lints, since a ref
-- dropped here stops being reported there, and Pawl.CardSpec's planted
-- objectRefPositions, which is the only observer of a position no card writes.
effectObjectRefs :: Effect card ability -> [ObjectRef]
effectObjectRefs effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> Foldable.toList (fmap DamagePart.ref parts)
  Effect.Fight {} -> []
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ _ ref) -> [ref]
  Effect.ChangeText {} -> []
  Effect.AddMana {} -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  -- CR 727.5's exemption, absent when nothing is exempt.
  Effect.RestartGame exempt -> Maybe.maybeToList exempt
  Effect.ControlPlayerNextTurn {} -> []
  Effect.Destroy (Destroy.MkDestroy ref _ _ _ _) -> [ref]
  Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect ref _) -> [ref]
  Effect.Attach {} -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ _ _ _) -> [ref]
  Effect.Draw {} -> []
  Effect.Mill {} -> []
  Effect.Reveal (Reveal.MkReveal ref _) -> [ref]
  Effect.FromOutsideTheGame {} -> []
  Effect.ExileThisSpell -> []
  Effect.LookAt (LookAt.MkLookAt ref _) -> [ref]
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore ref -> [ref]
  Effect.Discard subject -> case subject of
    Discard.Counted {} -> []
    Discard.These ref -> [ref]
  Effect.LoseLife {} -> []
  Effect.GainLife {} -> []
  Effect.ExchangeLifeTotals {} -> []
  Effect.SetLifeTotal {} -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed {} -> []
  Effect.DecreaseSpeed {} -> []
  Effect.Create {} -> []
  Effect.Conjure {} -> []
  Effect.CreateCopy (CreateCopy.MkCreateCopy _ ref _) -> [ref]
  -- Both sides: CR 707.2's copiable values come off one and go onto the other.
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject) -> [original, subject]
  -- BOTH refs: CR 707.10d's candidates are named by a ref of their own, and a
  -- slot it reads is as much a read of this effect's as the copied object's.
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets) -> ref : copyTargetsRefs targets
  Effect.Replace {} -> []
  Effect.SkipNextPhase {} -> []
  -- Absent where the shield's recipients are described rather than named.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ ref _ _ _ _ _) -> Maybe.maybeToList ref
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ ref _ _ _ _ _) -> Maybe.maybeToList ref
  -- CR 614.9's two sides, the damage's old recipient -- absent where the card
  -- describes it instead -- and its new one.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage _ _ _ from _ _ to _) -> Maybe.maybeToList from <> [to]
  Effect.Counter (Counter.MkCounter ref _ _) -> [ref]
  Effect.PutCounters (PutCounters.MkPutCounters _ _ ref) -> [ref]
  Effect.RemoveCounters {} -> []
  -- CR 122.5's two sides, either of which may name a group.
  Effect.MoveCounters (MoveCounters.MkMoveCounters from _ _ to) -> [from, to]
  -- CR 122.8's taker; the giver is a slot.
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom _ _ ref) -> [ref]
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.PayAnyEnergy {} -> []
  Effect.Tap ref -> [ref]
  Effect.Untap ref -> [ref]
  Effect.Detain ref -> [ref]
  Effect.Goad ref -> [ref]
  Effect.DoesNotUntapNext ref -> [ref]
  Effect.Transform ref -> [ref]
  Effect.Convert ref -> [ref]
  -- The components; the combined face beside them is card data, not a ref.
  Effect.Meld (Meld.MkMeld objects _) -> [objects]
  Effect.PhaseOut ref -> [ref]
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown ref _) -> [ref]
  Effect.TurnFaceUp {} -> []
  Effect.RemoveFromCombat ref -> [ref]
  Effect.BecomesBlocked {} -> []
  Effect.AddPhases {} -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef _ ref) -> [ref]
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  -- CR 509.1a's two sides, the creature required to block and what it blocks.
  Effect.RequireBlock (RequireBlock.MkRequireBlock _ blocker attacker) -> [blocker, attacker]
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated _ ref) -> [ref]
  -- One side only: what a creature attacks is a player (CR 508.1b), so the arm
  -- beside this one is a PlayerRef.
  Effect.RequireAttack (RequireAttack.MkRequireAttack _ attacker _) -> [attacker]
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock _ ref) -> [ref]
  -- One side only, and only when a ref names it: the Matching arm is a Filter
  -- (CR 611.2c's class), and what the attack is aimed at is a PlayerScope.
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack _ affected _) -> case affected of
    RestrictedCreatures.Named ref -> [ref]
    RestrictedCreatures.Matching _ -> []
  Effect.CreateEmblem {} -> []
  Effect.BecomeMonarch {} -> []
  Effect.TakeTheInitiative {} -> []
  Effect.Designate {} -> []
  Effect.SetClassLevel {} -> []
  Effect.Unsuspect ref -> [ref]
  Effect.SetHalfLocked {} -> []
  Effect.Evolve {} -> []
  Effect.Mentor {} -> []
  Effect.Train {} -> []
  Effect.ItBecomes {} -> []
  Effect.ExileUntilMonarch {} -> []
  Effect.ExileHaunting {} -> []
  Effect.PlaySubgame {} -> []
  Effect.ChooseOpponent {} -> []
  Effect.ChooseOpponentAtRandom {} -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName {} -> []
  Effect.Bolster {} -> []
  Effect.Amass {} -> []
  Effect.Blight {} -> []
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.PlayerSacrifices {} -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary _ ref) -> [ref]
  Effect.Shuffle {} -> []
  Effect.OfferCast {} -> []
  Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile _ ref _) -> [ref]
  Effect.MakePlotted ref -> [ref]
  -- CR 608.2f's set, swept once; the body's own refs are the caller's recursion.
  Effect.ForEach (ForEach.MkForEach ref _ _) -> [ref]

-- Every PlayerRef this ONE effect holds in a field of its own: not the ones
-- nested in an ObjectRef it carries (objectRefPlayerRefs), not the ones nested
-- in a Quantity (Pawl.Engine.Quantity's readers), and not a nested effect's,
-- which are its own answer here.
--
-- The single enumeration of where a PlayerRef sits in an opcode, and
-- effectObjectRefs' twin one type over. Its readers -- slotsOf here, and
-- Pawl.CardSpec's plural-slot lint -- call this rather than naming the opcodes
-- themselves, so they cannot come to disagree about which opcodes hold one. The
-- test a reader can apply: no arm of either names a PlayerRef field.
--
-- No wildcard, and the arms that hold a reference destructure positionally: a
-- new opcode the compiler forces, and so does a new FIELD on a payload that
-- already holds one. What neither the compiler nor this shape catches is an
-- existing field WIDENED to a PlayerRef; slotsOf's corpus lints pay for that,
-- since a reference dropped here stops being reported there, and so does
-- Pawl.CardSpec's planted playerRefPositions.
effectPlayerRefs :: Effect card ability -> [PlayerRef]
effectPlayerRefs effect = case effect of
  Effect.DealDamage {} -> []
  Effect.Fight {} -> []
  Effect.ModifyTarget {} -> []
  Effect.ChangeText {} -> []
  Effect.AddMana (ManaAddition.MkManaAddition ref _ _ _ _) -> [ref]
  Effect.Search (Search.MkSearch searcher owner _ _ _ _ _) -> [searcher, owner]
  Effect.ExileAllGraveyards -> []
  Effect.RestartGame {} -> []
  Effect.ControlPlayerNextTurn {} -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice {} -> []
  Effect.Attach {} -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.MoveToZone {} -> []
  Effect.Draw (Draw.MkDraw ref _ _) -> [ref]
  Effect.Mill (Mill.MkMill ref _ _ _) -> [ref]
  Effect.Reveal {} -> []
  Effect.FromOutsideTheGame {} -> []
  Effect.ExileThisSpell -> []
  Effect.LookAt {} -> []
  Effect.Scry (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.Explore {} -> []
  Effect.Discard {} -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.ExchangeLifeTotals {} -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.DecreaseSpeed (SpeedDecrease.MkSpeedDecrease ref _ _) -> [ref]
  Effect.Create (Create.MkCreate _ _ _ _ creator) -> [creator]
  Effect.Conjure {} -> []
  Effect.CreateCopy {} -> []
  Effect.BecomeCopy {} -> []
  Effect.CopyStackObject {} -> []
  Effect.Replace {} -> []
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase ref _) -> [ref]
  Effect.PreventNextDamage {} -> []
  Effect.PreventAllDamage {} -> []
  Effect.RedirectDamage {} -> []
  Effect.Counter {} -> []
  Effect.PutCounters {} -> []
  Effect.RemoveCounters {} -> []
  Effect.MoveCounters {} -> []
  Effect.PutCountersFrom {} -> []
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters ref _ _) -> [ref]
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters ref _ _) -> [ref]
  Effect.PayAnyEnergy {} -> []
  Effect.Tap {} -> []
  Effect.Untap {} -> []
  Effect.Detain {} -> []
  Effect.Goad {} -> []
  Effect.DoesNotUntapNext {} -> []
  Effect.Transform {} -> []
  Effect.Convert {} -> []
  Effect.Meld {} -> []
  Effect.PhaseOut {} -> []
  Effect.TurnFaceDown {} -> []
  Effect.TurnFaceUp {} -> []
  Effect.RemoveFromCombat {} -> []
  Effect.BecomesBlocked {} -> []
  Effect.AddPhases {} -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl {} -> []
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.RequireBlock {} -> []
  Effect.CantBeRegenerated {} -> []
  Effect.RequireAttack (RequireAttack.MkRequireAttack _ _ defender) -> [defender]
  Effect.ForbidBlock {} -> []
  Effect.ForbidAttack {} -> []
  Effect.CreateEmblem {} -> []
  Effect.BecomeMonarch {} -> []
  Effect.TakeTheInitiative {} -> []
  Effect.Designate {} -> []
  Effect.SetClassLevel {} -> []
  Effect.Unsuspect {} -> []
  Effect.SetHalfLocked {} -> []
  Effect.Evolve {} -> []
  Effect.Mentor {} -> []
  Effect.Train {} -> []
  Effect.ItBecomes {} -> []
  Effect.ExileUntilMonarch {} -> []
  Effect.ExileHaunting {} -> []
  Effect.PlaySubgame {} -> []
  Effect.ChooseOpponent {} -> []
  Effect.ChooseOpponentAtRandom {} -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName {} -> []
  Effect.Bolster {} -> []
  Effect.Amass {} -> []
  Effect.Blight (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.PlayerSacrifices {} -> []
  Effect.TakeExtraTurn takeExtraTurn -> [TakeExtraTurn.player takeExtraTurn]
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named _) -> Maybe.maybeToList named
  Effect.Shuffle ref -> [ref]
  Effect.OfferCast (OfferCast.MkOfferCast _ caster _ _) -> [caster]
  Effect.GrantPlayFromExile {} -> []
  Effect.MakePlotted {} -> []
  Effect.ForEach {} -> []

-- The slots a MonarchTarget reads: only the targeted arm names one.
monarchTargetSlots :: MonarchTarget.MonarchTarget -> Map.Map SlotName SlotArity
monarchTargetSlots target = case target of
  MonarchTarget.TheController -> Map.empty
  MonarchTarget.ControllerOfSource -> Map.empty
  MonarchTarget.InSlot slot -> Map.singleton slot SlotArity.One

-- WithController reads one target; BetweenTargets takes both sides out of one
-- slot (CR 601.2c) and so must see the whole set.
exchangeSidesSlots :: ExchangeSides.ExchangeSides -> Map.Map SlotName SlotArity
exchangeSidesSlots sides = case sides of
  ExchangeSides.WithController slot -> Map.singleton slot SlotArity.One
  ExchangeSides.BetweenTargets slot -> Map.singleton slot SlotArity.Many

-- The one legitimate home of `case effect of`: this module is the VM's opcode
-- semantics (design.md section 1). slotsOf is the read half of the dataflow lint;
-- X is not one of its reads, readsX below being X's own half.
--
-- The ObjectRefs and the PlayerRefs this effect holds are taken from
-- effectObjectRefs and effectPlayerRefs at the head rather than arm by arm, so
-- no arm of the case names either.
slotsOf :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Map.Map SlotName SlotArity
slotsOf effect = joinTwo (joinTwo (joinSlots (fmap objectRefSlots (effectObjectRefs effect))) (joinSlots (fmap playerRefSlots (effectPlayerRefs effect)))) $ case effect of
  -- The dealer is a read like any other (CR 120.2b), and one object (CR 120.1).
  Effect.DealDamage (DealDamage.MkDealDamage parts dealer _) ->
    joinTwo
      (joinSlots (fmap (quantitySlots . DamagePart.quantity) (Foldable.toList parts)))
      (maybe Map.empty oneSlot dealer)
  -- BOTH fighters: CR 701.14a reads each one's power against the other, so a
  -- slot named by only one half would still look dangling.
  Effect.Fight (Fight.MkFight first second) -> joinTwo (oneSlot first) (oneSlot second)
  -- The modification's own quantities read slots too, through
  -- Projection.quantitiesOf.
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) ->
    joinTwo (joinSlots (fmap quantitySlots (Projection.quantitiesOf modification))) (durationSlots duration)
  Effect.ChangeText (ChangeText.MkChangeText _ _ slot) -> oneSlot slot
  Effect.AddMana {} -> Map.empty
  -- The count and the FILTER: both references are effectPlayerRefs' half, and a
  -- search filter naming a slot is a read like any other now that the arm
  -- matches it in the resolution's own context -- Bifurcate's "with the same
  -- name as target nontoken creature" is the whole of what its target slot is
  -- for, so without this the D4 dataflow lint would call that slot unread.
  Effect.Search (Search.MkSearch _ _ _ quantity filter_ _ _) ->
    joinTwo (joinSlots (fmap quantitySlots (Maybe.maybeToList quantity))) (filterSlotsOf filter_)
  Effect.ExileAllGraveyards -> Map.empty
  Effect.Proliferate -> Map.empty
  -- CR 201.4's name is not an object, so the choice binds no slot and the
  -- restriction Filter names none either -- a Filter reads a slot only through
  -- Filter.boundSlots, and no card writes one of those atoms here.
  Effect.ChooseCardName _ -> Map.empty
  -- No slot: the card comes from outside the game, where CR 400.11c lets nothing
  -- target and so nothing was announced (CR 601.2c).
  Effect.FromOutsideTheGame _ -> Map.empty
  Effect.ExileThisSpell -> Map.empty
  Effect.Bolster quantity -> quantitySlots quantity
  Effect.Amass (Amass.Type.MkAmass quantity _) -> quantitySlots quantity
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.TemptWithTheRing -> Map.empty
  Effect.Venture {} -> Map.empty
  Effect.ExileHandThenDraw -> Map.empty
  -- CR 101.4's "each player sacrifices": the arm takes every player recipient
  -- the slot holds, so the read is Many.
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot _ quantity) -> joinTwo (Map.singleton slot SlotArity.Many) (quantitySlots quantity)
  Effect.RestartGame _ -> Map.empty
  Effect.ControlPlayerNextTurn slot -> oneSlot slot
  -- The three slot fields are DEFINITIONS, not reads; they belong to boundSlots
  -- below.
  Effect.Destroy {} -> Map.empty
  -- The ref is the whole read, and the head above already took it.
  Effect.Sacrifice {} -> Map.empty
  Effect.TurnFaceDown {} -> Map.empty
  Effect.TurnFaceUp slot -> oneSlot slot
  Effect.RemoveFromCombat _ -> Map.empty
  Effect.BecomesBlocked slot -> oneSlot slot
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> joinTwo (joinSlots (fmap quantitySlots (riderQuantities riders))) (riderSlots riders)
  -- CR 121.1's bound slot is a DEFINITION, not a read: see boundSlots below.
  Effect.Draw (Draw.MkDraw _ quantity _) -> quantitySlots quantity
  -- The tally's slot and CR 701.17c's are DEFINITIONS, not reads: see boundSlots
  -- below.
  Effect.Mill (Mill.MkMill _ quantity _ _) -> quantitySlots quantity
  -- The bound slot is a DEFINITION, not a read.
  Effect.Reveal {} -> Map.empty
  -- The bound slot is a DEFINITION, not a read.
  Effect.LookAt {} -> Map.empty
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.Explore _ -> Map.empty
  Effect.Discard subject -> case subject of
    -- The bound slot is a DEFINITION, not a read, so it is not joined in here.
    -- Many, PlayerSacrifices' arity and for its reason: CR 101.4's worked
    -- example is a table-wide edict, and the resolution arm below folds over
    -- every player the slot names.
    Discard.Counted (CountedDiscard.MkCountedDiscard slot quantity _) -> joinTwo (Map.singleton slot SlotArity.Many) (quantitySlots quantity)
    Discard.These {} -> Map.empty
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.ExchangeLifeTotals sides -> exchangeSidesSlots sides
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.RedistributeLifeTotals -> Map.empty
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.DecreaseSpeed d -> quantitySlots (SpeedDecrease.quantity d)
  -- Create's slot is a DEFINITION, not a read, so the lint must not see it here.
  -- CR 111.2's creator is a READ -- Rampage of the Clans names the controller of
  -- the permanent the loop around it bound -- reported at the head with every
  -- other PlayerRef. So is CR 509.4's blocking rider, which names the attacker
  -- the token enters blocking (Flash Foliage's target), and that one is here.
  Effect.Create (Create.MkCreate quantity _ riders _ _) -> joinSlots [quantitySlots quantity, joinSlots (fmap quantitySlots (riderQuantities riders)), riderSlots riders]
  -- The COUNT only: the conjured card is literal card data, its destination is
  -- a constructor, and the conjurer is the resolving controller.
  Effect.Conjure (Conjure.MkConjure quantity _ _) -> quantitySlots quantity
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ riders) -> joinSlots [quantitySlots quantity, joinSlots (fmap quantitySlots (riderQuantities riders)), riderSlots riders]
  Effect.BecomeCopy {} -> Map.empty
  Effect.CopyStackObject {} -> Map.empty
  -- The Duration and Condition each carry Quantities; a Quantity.InSlot is a read.
  -- The ROW's own Filters and Quantities are READS too (replacementRowReads):
  -- Filter.IsBound in one names an object an earlier effect of this same
  -- resolution defined (Dire Fleet Daredevil's "that spell"), which is what the
  -- row's captured environment answers at CR 616.1.
  Effect.Replace (Replace.MkReplace duration _ _ condition re) ->
    joinSlots [durationSlots duration, joinSlots (fmap conditionSlots (Maybe.maybeToList condition)), replacementRowSlots re]
  Effect.SkipNextPhase {} -> Map.empty
  -- CR 615.5's rider reads slots of its own, so its reads join this effect's,
  -- LESS the reserved amount slot: the prevention binds that one itself
  -- (Event.eventBindingSlots), and Resolve.runPreventionRider is the writer.
  --
  -- The card-authored FILTERS are reads too, replacementRowSlots' answer for
  -- the same DamageR row one carrier over: the recipient description rides the
  -- installed row and is re-asked at each damage event, and CR 609.7a's
  -- chosen-source predicate is asked once as this effect resolves. CR 609.7b's
  -- printed source properties are not among them -- this opcode has no such
  -- field, and its installDamageRow call passes the trivial predicate. A
  -- Filter.IsBound in either names a slot of this resolution.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ _ whatRecipient _ chosenSource quantity rider) ->
    joinSlots
      [ durationSlots duration,
        quantitySlots quantity,
        joinSlots (fmap filterSlotsOf (Maybe.maybeToList whatRecipient <> Maybe.maybeToList chosenSource)),
        Map.delete Binding.eventAmount (joinSlots (fmap slotsOf (Foldable.toList rider)))
      ]
  -- The same reads, minus the shield size this opcode does not carry and plus CR
  -- 609.7b's printed source properties, the one field only this opcode spells
  -- out; they ride the row and are rechecked at the damage event (CR 615.9).
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ _ whatRecipient _ chosenSource whatSource rider) ->
    joinSlots
      [ durationSlots duration,
        joinSlots (fmap filterSlotsOf (Maybe.maybeToList whatRecipient <> Maybe.maybeToList chosenSource <> [whatSource])),
        Map.delete Binding.eventAmount (joinSlots (fmap slotsOf (Foldable.toList rider)))
      ]
  -- CR 614.9's redirection reads what PreventNextDamage reads, minus a rider it
  -- cannot carry: the recipient description rides the row, CR 609.7a's
  -- chosen-source predicate is asked once here, and the counted amount is
  -- evaluated once here too.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ amount _ whatRecipient _ _ chosenSource) ->
    joinSlots
      [ durationSlots duration,
        joinSlots (fmap quantitySlots (Maybe.maybeToList amount)),
        joinSlots (fmap filterSlotsOf (Maybe.maybeToList whatRecipient <> Maybe.maybeToList chosenSource))
      ]
  -- The bound slot is a DEFINITION, not a read: see boundSlots below.
  Effect.Counter {} -> Map.empty
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> quantitySlots quantity
  -- CR 122.8 reads its tally off ONE object, so `from` is read singly, where the
  -- destination is an ObjectRef and may sweep.
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom from _ _) -> oneSlot from
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity slot) -> insertOne slot (quantitySlots quantity)
  -- CR 122.5's pair: BOTH sides are ObjectRefs, joined at slotsOf's head with
  -- every other ref, so neither is read here. The count reads
  -- slots of its own -- Black Panther, Wakandan King's "all +1/+1 counters" is a
  -- Quantity.AgainstSlot aimed at the slot its `from` names -- so it joins in
  -- here rather than being left to look dangling. The bound slot is a
  -- DEFINITION, not a read: see boundSlots below.
  --
  -- Both sides report SlotArity.Many, every ObjectRef.InSlot being a whole-set
  -- read. A slot the COUNT names as an OBJECT still comes out One:
  -- joinTwo is Map.unionWith min and a Quantity.AgainstSlot reads its object
  -- singly, so Black Panther's `land` -- named by both halves -- keeps the arity
  -- that says "up to two target creatures" cannot fill it. A count reading that
  -- same name's AMOUNT would not narrow it, reading no object at all
  -- (quantitySlots above).
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> foldMap quantitySlots (MovedKinds.quantityOf kinds)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantitySlots quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantitySlots quantity
  -- The SlotName is a DEFINITION, not a read; it belongs to boundSlots below.
  Effect.PayAnyEnergy _ -> Map.empty
  Effect.Tap _ -> Map.empty
  Effect.Untap _ -> Map.empty
  Effect.Detain _ -> Map.empty
  Effect.Goad _ -> Map.empty
  Effect.MakePlotted _ -> Map.empty
  Effect.DoesNotUntapNext _ -> Map.empty
  Effect.Transform _ -> Map.empty
  Effect.Convert _ -> Map.empty
  -- The combined back face is literal card data and names no slot.
  Effect.Meld {} -> Map.empty
  Effect.PhaseOut _ -> Map.empty
  Effect.AddPhases _ -> Map.empty
  Effect.EndTurn -> Map.empty
  Effect.EndCombatPhase -> Map.empty
  Effect.GainControl {} -> Map.empty
  Effect.ArmDelayedTrigger {} -> Map.empty
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers _ affected _) -> affectedPlayersSlots affected
  Effect.RequireBlock {} -> Map.empty
  Effect.CantBeRegenerated {} -> Map.empty
  Effect.ForbidBlock {} -> Map.empty
  Effect.ForbidAttack {} -> Map.empty
  -- CR 508.1b's two sides are a PlayerRef and an ObjectRef, both reported at the
  -- head, so this arm has nothing of its own.
  Effect.RequireAttack {} -> Map.empty
  Effect.CreateEmblem {} -> Map.empty
  -- CR 725.1's crown names a target slot only in the InSlot arm.
  Effect.BecomeMonarch target -> monarchTargetSlots target
  -- CR 726.1 names no target slot at all: neither InitiativeTarget arm reads one.
  Effect.TakeTheInitiative _ -> Map.empty
  -- A READ: the slot names the permanent gaining the designation.
  Effect.Designate (Designate.MkDesignate _ slot) -> oneSlot slot
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ slot) -> oneSlot slot
  Effect.Unsuspect _ -> Map.empty
  -- A READ, Designate's: the slot names the permanent whose half is locked or
  -- unlocked. WHICH half is chosen at resolution and is no slot of any kind.
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked _ _ slot) -> oneSlot slot
  -- A READ, Designate's: the slot names where rule 702.100a's counter goes.
  Effect.Evolve slot -> oneSlot slot
  Effect.Mentor slot -> oneSlot slot
  Effect.Train slot -> oneSlot slot
  Effect.ItBecomes _ -> Map.empty
  Effect.ExileUntilMonarch slot -> oneSlot slot
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting card slot) -> joinSlots [oneSlot card, oneSlot slot]
  Effect.Attach slot -> oneSlot slot
  Effect.AttachTarget (AttachTarget.MkAttachTarget slot _) -> oneSlot slot
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot _) -> oneSlot slot
  -- Two READS: the binding the entrant sits in and the slot the card targeted.
  -- Both are read rather than bound, so both belong here; CardSpec's
  -- declared-equals-read lint subtracts the reserved `became` from this side.
  Effect.AttachBound (AttachBound.MkAttachBound subject destination) -> joinSlots [oneSlot subject, oneSlot destination]
  -- CR 729.1/729.1b: the slot is a DEFINITION (the subgame's winner), not a read.
  Effect.PlaySubgame _ -> Map.empty
  -- A DEFINITION too: chosen as this effect is applied (CR 608.2d), never read.
  Effect.ChooseOpponent _ -> Map.empty
  Effect.ChooseOpponentAtRandom _ -> Map.empty
  -- A DEFINITION for the result slot (boundSlots below), but CR 706.2's modifier
  -- is a READ: the instruction's own Quantity may name a slot an earlier effect
  -- of this same resolution bound, CR 608.2c following the list in written order.
  --
  -- A SHAPE CORRECTION, not a tested behaviour: every modifier in data/cards/ is
  -- a Count naming no slot (Diviner's Portent), so leaving this Map.empty leaves
  -- the suite green. A card whose roll added "the number of cards you drew this
  -- way" would refute that. The same holds of the two arms below.
  Effect.RollDie rollDie -> maybe Map.empty quantitySlots (RollDie.modifier rollDie)
  -- And a DEFINITION too, on top of the slots the coin count reads.
  Effect.FlipCoin flipCoin -> quantitySlots (FlipCoin.count flipCoin)
  -- The slots the turn count reads (Ral Zarek's tally of heads).
  Effect.TakeExtraTurn takeExtraTurn -> quantitySlots (TakeExtraTurn.count takeExtraTurn)
  Effect.ShuffleIntoLibrary {} -> Map.empty
  -- The arm above's library read, reported at the head; nothing is shuffled into
  -- it, so there is no ref beside it either.
  Effect.Shuffle {} -> Map.empty
  -- The SLOT alone: the caster is a PlayerRef and is reported at the head. This
  -- one is a read, bound by an earlier effect of the list (CR 400.7).
  Effect.OfferCast (OfferCast.MkOfferCast slot _ _ _) -> oneSlot slot
  Effect.GrantPlayFromExile grant -> durationSlots (GrantPlayFromExile.duration grant)
  -- Everything the BODY reads. The loop's own slot is NOT subtracted as the
  -- rider's reserved slot is: boundSlots below defines it.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> joinSlots (fmap slotsOf (Foldable.toList body))

-- CR 611.2b: only ForAsLongAs carries a Quantity, through its Condition.
durationSlots :: Duration.Duration -> Map.Map SlotName SlotArity
durationSlots duration = case duration of
  Duration.UntilEndOfTurn -> Map.empty
  Duration.Indefinite -> Map.empty
  Duration.Perpetual -> Map.empty
  Duration.UntilYourNextTurn -> Map.empty
  Duration.UntilEndOfYourNextTurn -> Map.empty
  Duration.ForAsLongAs condition -> conditionSlots condition
  -- A Cost reads no slot: the activation cost of an ability is not walked by
  -- modeSlots either, and CR 116.2c's price is paid outside any resolution, so
  -- there is no binding environment for it to name.
  Duration.UntilPaid _ -> Map.empty
  Duration.UntilEndOfCombat -> Map.empty
  -- No slot either: Expiry.WhenUsed reads the effect's own Filter, which is
  -- walked wherever that effect's payload is (Pawl.Engine.PlayerEffect), not
  -- here.
  Duration.UntilUsed -> Map.empty

-- Every slot ONE target slot reads: its pool's, its filter's, and its CR 202.3
-- computed bound's. Its own name is not among them -- this is what the slot
-- READS, and CR 601.2c binds it only once it has been answered.
--
-- Its own function because a card declares a target slot in THREE places, and
-- each needs its own reader. Enumerated off the three types that hold one:
--
--   * Mode.targetSlots -- CR 601.2c's ordinary target, declared inside a mode.
--     modeSlots below folds this one, and the corpus lint that pairs a mode's
--     reads with its declarations is what consumes it.
--   * Face.enchant -- CR 303.4a's enchant slot, declared on the face BESIDE the
--     modes (Card.enchantSlotMap), so it is in no mode's declared set.
--   * Modification.GainEnchant -- the same slot GRANTED by a CR 613.1f layer 6
--     effect (Cloudform, the Licids, CR 702.103b's bestow). What answers it is
--     never the granting mode: the grant is a CR 611.2 continuous effect that
--     outlives the resolution that made it, so by the time CR 601.2c chooses for
--     a bestowed spell (Card.modesTargetSlotsGiven) or CR 303.4c's state-based
--     action re-reads CR 702.5a against a permanent, the announcement the grant
--     was written in is gone. Declaring the name would not rescue it.
--
-- The last two are one claim, and Pawl.CardSpec's "an enchant slot reads no slot,
-- printed or granted" sweep is what states it: neither may read anything.
targetSlotSlots :: TargetSlot.TargetSlot -> Map.Map SlotName SlotArity
targetSlotSlots slot =
  joinSlots
    [ poolSlot (TargetSlot.pool slot),
      -- Every slot the slot's own FILTER names -- CR 603.2's "target artifact or
      -- enchantment that player controls".
      maybe Map.empty (Map.fromSet (const SlotArity.One) . Filter.boundSlots) (TargetSlot.filter slot),
      -- And every slot its CR 202.3 computed bound names -- Venerable Warsinger's
      -- "mana value X or less ... where X is the amount of damage this creature
      -- dealt to that player", whose X is the trigger's own event amount
      -- (Pawl.Engine.Binding.eventAmount). Target.slotContext is what answers it,
      -- off the announcement the caller hands over.
      --
      -- What it buys is the pairing -- a card whose bound names an amount its
      -- CONDITION does not supply (Pawl.Engine.Event.eventBindingSlots) is caught
      -- only because the read is reported here. No card in data/cards/ misauthors
      -- that pairing, so the proof is a planted one; see amountSlots below for
      -- which case proves it.
      maybe Map.empty amountSlots (TargetSlot.amount slot)
    ]
  where
    -- The bound's own reads: quantitySlots' -- a Quantity.InSlot naming the
    -- slot's amount -- plus the ones only QuantitySlot.nestedRefs reports, a slot
    -- named through a PlayerRef buried inside the number ("mana value X or less,
    -- where X is the amount of life THAT PLAYER gained this turn") or through CR
    -- 400.7j's Scope.OverBound. Without them a bound naming a slot its carrier
    -- never binds is invisible to the equality above, which is the one defect the
    -- fold exists to catch; Pawl.CardSpec's "the lint itself catches a computed
    -- bound naming a slot through a player" is what proves it.
    --
    -- The arities are playerRefSlots' rather than One across the board, because
    -- the count lint reads these values: PlayerRef.EachInSlot takes every player
    -- a slot names and CR 400.7j's fold every object, so neither is damaged by a
    -- plural slot.
    amountSlots quantity =
      joinSlots
        ( quantitySlots quantity
            : fmap (either playerRefSlots (`Map.singleton` SlotArity.Many)) (Set.toList (QuantitySlot.nestedRefs quantity))
        )

-- Every slot a whole MODE reads: its effects', every payer CR 118.12a's "unless
-- [a player] pays" names, and every slot a target slot's own pool, filter or
-- bound names. A payer or pool slot no effect also reads would otherwise dangle.
modeSlots :: Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Map.Map SlotName SlotArity
modeSlots mode =
  joinSlots
    [ joinSlots (fmap slotsOf (Foldable.toList (Mode.allEffects mode))),
      joinSlots (fmap payerSlot (Foldable.toList (Mode.clauses mode))),
      joinSlots (fmap askerSlot (Foldable.toList (Mode.clauses mode))),
      joinSlots (fmap chooserSlot (Foldable.toList (Mode.clauses mode))),
      joinSlots (fmap targetSlotSlots (Map.elems (Mode.targetSlots mode)))
    ]
  where
    -- Every clause's payer: CR 118.12 scopes a resolution cost to its clause.
    payerSlot = maybe Map.empty (playerRefSlots . PayGate.payer) . Clause.payGate
    -- And every clause's ASKER, for its reason: CR 603.5's "may" is scoped to a
    -- clause too, and Jungle Wayfinder's names the table rather than a slot --
    -- but a card may name one, and an asker slot no effect also reads would
    -- otherwise dangle.
    askerSlot clause = case Clause.optionality clause of
      Optionality.Mandatory -> Map.empty
      Optionality.Optional ref -> playerRefSlots ref
    -- And every clause's branch CHOOSER, for the same reason one rider over: CR
    -- 608.2d's announcement is scoped to a clause pair and its reference may
    -- name a slot.
    chooserSlot = maybe Map.empty (playerRefSlots . OrElse.chooser) . Clause.orElse

-- The slot a target pool draws its candidates from, if it draws them from one
-- (CR 400.1's per-player graveyard), read singly.
poolSlot :: Pool.Pool -> Map.Map SlotName SlotArity
poolSlot pool = case pool of
  Pool.Creatures -> Map.empty
  Pool.Players -> Map.empty
  Pool.AnyTarget -> Map.empty
  Pool.Permanents -> Map.empty
  Pool.Spells -> Map.empty
  Pool.Abilities -> Map.empty
  Pool.SpellsAndPermanents -> Map.empty
  Pool.PlayersAndPlaneswalkers -> Map.empty
  Pool.CardsInGraveyard scope -> case scope of
    ZoneScope.Scoped _ -> Map.empty
    ZoneScope.InSlot slot -> oneSlot slot
  Pool.CardsInExile -> Map.empty
  -- The graveyard half's scope; the battlefield half names no slot.
  Pool.CreaturesAndCardsInGraveyard scope -> case scope of
    ZoneScope.Scoped _ -> Map.empty
    ZoneScope.InSlot slot -> oneSlot slot

-- Both sides of a comparison are a Quantity, and either may read a slot.
conditionSlots :: Condition.Type.Condition -> Map.Map SlotName SlotArity
conditionSlots condition = case condition of
  Condition.Type.Compares c ->
    joinTwo (quantitySlots (Compares.measured c)) (quantitySlots (Compares.threshold c))
  Condition.Type.Any conditions -> joinSlots (fmap conditionSlots conditions)
  Condition.Type.All conditions -> joinSlots (fmap conditionSlots conditions)

-- Everything one waiting ROW can name a slot with: the Filters its pattern and its
-- rewrite describe things with, and the Quantities its rewrite counts with. A
-- Filter.IsBound in any of them, and a Quantity.InSlot in any of them, is a read
-- of the installing resolution's binding environment
-- (Pawl.Types.ActiveReplacement).
--
-- ONE declaration for THREE consumers, which is why the rewrite is not left to a
-- second function: replacementRowSlots below reports it as what the effect reads
-- (CR 603.3b, through slotsOf's Replace arm); installDamageRow and the
-- Effect.Replace resolution arm restrict what the installed row CAPTURES to it;
-- and referredToSources reads that captured map back out as CR 609.7a's "any
-- object referred to by ... a replacement or prevention effect that's waiting to
-- apply". A read missing here is one the row cannot answer at the event, with no
-- -Werror to catch it and no help from the card dataflow lint, whose read side is
-- this same function.
--
-- The two halves are returned TOGETHER rather than by two traversals, so that
-- ownSlotsAreExhaustive's Replace arm and the slot walk cannot come apart about
-- what an arm holds.
--
-- SYNTACTIC rather than per-reader: a slot NAME anywhere in the row's own data is
-- an object the row refers to, whether or not the arm reading it happens to build
-- a slot-aware Filter.Context today (#2141 names the callers that do not). That is
-- what CR 609.7a asks for, and it is the safe direction for the capture.
--
-- CR 614.9's printed DESTINATION is walked with the damage pattern beside it and
-- is not a pattern: it is read in the same
-- Pawl.Engine.Replacement.candidateContext the pattern is, so IsBound means the
-- same thing in both and a slot declared for one is declared for the other.
--
-- No wildcard: an arm added to Pawl.Types.ReplacementEffect must answer here, and
-- the ones carrying neither Filter nor Quantity say so rather than falling
-- through. Not implemented: the nested EFFECTS an EntryR rewrite or a DamageR
-- rider carries read slots of their own and are not walked (gap #1962).
replacementRowReads :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
replacementRowReads re = case re of
  -- The rewrite is a Zone and two Bools (Pawl.Types.ZoneChangeR): nothing that can
  -- name a slot, so the pattern is the whole of it.
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR pat _ _ _) -> ([ZoneChangePattern.whatObject pat], [])
  ReplacementEffect.EntryR (EntryR.MkEntryR pat rewrite) -> addFilter pat (entryRewriteReads rewrite)
  ReplacementEffect.DamageR (DamageR.MkDamageR pat rewrite _) ->
    ( DamagePattern.whatSource pat : (Maybe.maybeToList (DamagePattern.whatRecipient pat) <> damageRewriteFilters rewrite),
      []
    )
  ReplacementEffect.DestructionR _ -> ([], [])
  -- The rewrite is one Scaling, which is a constructor and a Natural.
  ReplacementEffect.CounterR (CounterR.MkCounterR pat _) -> ([CounterPattern.onWhat pat], [])
  -- The pattern's Filter over what the token is; the scaling is a number and
  -- the appended token is card data of its own, so neither reads a slot.
  ReplacementEffect.TokenR (TokenR.MkTokenR pat _ _) -> ([TokenPattern.whatToken pat], [])
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR pat _ rewrite) -> addFilter pat (turnUpRewriteReads rewrite)
  ReplacementEffect.UntapR _ -> ([], [])
  -- A LifeLossPattern is one ControllerRelation and one LifeLossCause, and no arm
  -- of LifeLossRewrite carries a Filter or a Quantity: no read at all.
  ReplacementEffect.LifeLossR {} -> ([], [])
  -- A DrawR is one ControllerRelation and one amount of life. LifeLossR's answer,
  -- and for its reason.
  ReplacementEffect.DrawR {} -> ([], [])
  -- A DrawCountR is one ControllerRelation, one threshold and one nullary rewrite.
  -- DrawR's answer, and for its reason.
  ReplacementEffect.DrawCountR {} -> ([], [])
  ReplacementEffect.PhaseR _ -> ([], [])

-- A row's pattern Filter joined onto what its rewrite reads.
addFilter :: Filter.Type.Filter Keyword.Type.Keyword -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity]) -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
addFilter filter_ (filters, quantities) = (filter_ : filters, quantities)

-- What an ENTRY rewrite reads, beside its row's pattern. Total over
-- Pawl.Types.EntryRewrite and no wildcard, replacementRowReads' discipline: an arm
-- gaining a Filter or a Quantity must answer here rather than have its reads go
-- undeclared. The arms answering nothing carry Naturals, constructors and literal
-- card data, none of which can name a slot.
--
-- Every arm here is a REGRESSION FENCE rather than a proven behaviour: no
-- Effect.Replace in data/cards/ carries an EntryR whose rewrite is anything but
-- Tapped or UnderSourceControl (Gather Specimens), and both answer nothing here,
-- so neutralizing any arm leaves the whole suite green. They are written
-- because the narrowing one caller over is only sound if this list is complete --
-- a rewrite read left out is a slot the installed row does not carry, and the
-- Filter or Quantity that wanted it then answers vacuously at the event.
entryRewriteReads :: EntryRewrite.EntryRewrite (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
entryRewriteReads rewrite = case rewrite of
  EntryRewrite.AsCopy asCopy -> ([AsCopy.eligible asCopy], [])
  EntryRewrite.ChoiceOf _ -> ([], [])
  EntryRewrite.ChoiceByCoinFlip _ -> ([], [])
  EntryRewrite.ChooseColor -> ([], [])
  EntryRewrite.ChooseBasicLandType -> ([], [])
  EntryRewrite.ChoosePlayer -> ([], [])
  EntryRewrite.ChooseCardNames restriction -> ([restriction], [])
  EntryRewrite.ChooseCardName restriction -> ([restriction], [])
  -- CR 614.1c's count per kind, evaluated as the row applies and against the ROW's
  -- Context (Pawl.Engine.Event's WithCounters arm), so a Quantity.InSlot in one
  -- reads the captured map. The KINDS beside them cannot name a slot.
  EntryRewrite.WithCounters counters -> ([], Map.elems (WithCounters.counters counters))
  EntryRewrite.UnderSourceControl -> ([], [])
  EntryRewrite.SacrificeAnyNumber sacrifice -> ([SacrificeAnyNumber.filter sacrifice], [])
  EntryRewrite.Riot -> ([], [])
  EntryRewrite.ReadAhead -> ([], [])
  EntryRewrite.Unleash -> ([], [])
  EntryRewrite.Bloodthirst _ -> ([], [])
  EntryRewrite.Compleated _ -> ([], [])
  EntryRewrite.Tapped -> ([], [])
  EntryRewrite.PayLifeOrTapped _ -> ([], [])
  EntryRewrite.RevealOrTapped filter_ -> ([filter_], [])
  EntryRewrite.EntersTransformed -> ([], [])
  -- Not implemented: the nested effects read slots of their own and neither this
  -- answer nor slotsOf reports them (gap #1962).
  EntryRewrite.RunEffects _ -> ([], [])

-- What a TURN-UP rewrite reads. entryRewriteReads' two shapes and its discipline:
-- CR 702.37b's count is evaluated against the row's Context, and CR 303.4k's host
-- description is a Filter.
turnUpRewriteReads :: TurnUpRewrite.TurnUpRewrite -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
turnUpRewriteReads rewrite = case rewrite of
  TurnUpRewrite.WithCounters counters -> ([], Map.elems (WithCounters.counters counters))
  TurnUpRewrite.MayAttachTo filter_ -> ([filter_], [])

-- replacementRowReads as a slot map. Arity One for every FILTER read, a
-- Filter.IsBound being a membership test rather than a target slot; the QUANTITY
-- half is quantitySlots' answer, so a Quantity.InSlot reports SlotArity.Amount --
-- CR 614.1c's WithCounters and CR 702.37b's are the two rewrites that can carry
-- one. The two capture sites take Map.keysSet, so the arity reaches only slotsOf's
-- Replace arm.
replacementRowSlots :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Map.Map SlotName SlotArity
replacementRowSlots re =
  let (filters, quantities) = replacementRowReads re
   in joinSlots (fmap filterSlotsOf filters <> fmap quantitySlots quantities)

-- The Filters a damage REWRITE holds, which is CR 614.9's printed destination and
-- nothing else. No wildcard, replacementRowSlots' discipline: a later rewrite
-- describing something must answer here rather than have its slot reads go
-- undeclared.
damageRewriteFilters :: DamageRewrite.DamageRewrite -> [Filter.Type.Filter Keyword.Type.Keyword]
damageRewriteFilters rewrite = case rewrite of
  DamageRewrite.RedirectMatching f -> [f]
  DamageRewrite.Redirect _ -> []
  DamageRewrite.RedirectNext _ _ -> []
  DamageRewrite.PreventAll -> []
  DamageRewrite.PreventRemovingShieldCounter -> []
  DamageRewrite.PreventNext _ -> []
  DamageRewrite.PreventAllBut _ -> []
  DamageRewrite.SetAmount _ -> []
  DamageRewrite.Scale _ -> []

-- One Filter's slot reads, at arity One -- the same shape modeSlots folds over a
-- mode's target-slot Filters.
filterSlotsOf :: Filter.Type.Filter Keyword.Type.Keyword -> Map.Map SlotName SlotArity
filterSlotsOf = Map.fromSet (const SlotArity.One) . Filter.boundSlots

-- CR 603.3b: is slotsOf's answer the WHOLE of what applying this effect reads off
-- the resolving object's bindings? A classification of effect SHAPE, never of
-- which effect it is; Engine.orderInert may elide CR 603.3b's ordering prompt
-- only for an ability that reads nothing.
--
-- Four ways this and slotsOf come apart, one per False or guard below:
-- ArmDelayedTrigger captures the whole environment (CR 603.7c); a Duration
-- slotsOf's arm drops can still name a slot (CR 611.2b); a PlayerRef nested in a
-- Quantity is Quantity.slotsAreExhaustive's half; CR 725.2's ControllerOfSource
-- reads the trigger-source slot, which is not a target.
--
-- The Quantities nested in this effect's ObjectRefs are taken from
-- effectObjectRefs here rather than arm by arm, so no arm of the case below
-- names a ref.
slotsAreExhaustive :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
slotsAreExhaustive effect = all (all Quantity.slotsAreExhaustive . objectRefQuantities) (effectObjectRefs effect) && ownSlotsAreExhaustive effect

-- slotsAreExhaustive's half that is not an ObjectRef's: this opcode's own
-- fields, and its nested effects through the recursion back into it.
--
-- No wildcard: a new opcode must answer here as well as in slotsOf. The `{}`
-- arms answer a constant, so a new FIELD on one is not forced.
ownSlotsAreExhaustive :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
ownSlotsAreExhaustive effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> all (Quantity.slotsAreExhaustive . DamagePart.quantity) parts
  Effect.Fight {} -> True
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) ->
    durationSlotsAreExhaustive duration
      && all Quantity.slotsAreExhaustive (Projection.quantitiesOf modification)
  Effect.ChangeText {} -> True
  Effect.AddMana _ -> True
  -- The COUNT's own nested reads. The filter's are exhaustive by construction:
  -- slotsOf reports them through Filter.boundSlots, the one walk that enumerates
  -- what a Filter reads, and no Filter atom carries a Quantity for
  -- Quantity.slotsAreExhaustive to be about.
  Effect.Search (Search.MkSearch _ _ _ quantity _ _ _) -> all Quantity.slotsAreExhaustive quantity
  Effect.ExileAllGraveyards -> True
  Effect.Proliferate -> True
  Effect.ChooseCardName _ -> True
  Effect.FromOutsideTheGame _ -> True
  Effect.ExileThisSpell -> True
  Effect.Bolster quantity -> Quantity.slotsAreExhaustive quantity
  Effect.Amass (Amass.Type.MkAmass quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.TemptWithTheRing -> True
  Effect.Venture {} -> True
  Effect.ExileHandThenDraw -> True
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RestartGame _ -> True
  Effect.ControlPlayerNextTurn _ -> True
  Effect.Destroy {} -> True
  Effect.Sacrifice _ -> True
  Effect.TurnFaceDown _ -> True
  Effect.TurnFaceUp _ -> True
  Effect.RemoveFromCombat _ -> True
  Effect.BecomesBlocked _ -> True
  -- The entry rider nests a Quantity of its own, CR 122.6's count per kind; the
  -- ref's is effectObjectRefs' above.
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> all Quantity.slotsAreExhaustive (riderQuantities riders)
  Effect.Draw (Draw.MkDraw _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.Mill (Mill.MkMill _ quantity _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.Reveal {} -> True
  Effect.LookAt {} -> True
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Explore {} -> True
  Effect.Discard subject -> case subject of
    Discard.Counted (CountedDiscard.MkCountedDiscard _ quantity _) -> Quantity.slotsAreExhaustive quantity
    Discard.These {} -> True
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.ExchangeLifeTotals _ -> True
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RedistributeLifeTotals -> True
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.DecreaseSpeed d -> Quantity.slotsAreExhaustive (SpeedDecrease.quantity d)
  -- CR 111.1's token is minted with empty bindings, so its card is literal text.
  -- Its entry riders are not: CR 122.6's count per kind is the effect speaking,
  -- read in the resolution's own slots.
  Effect.Create (Create.MkCreate quantity _ riders _ _) -> all Quantity.slotsAreExhaustive (quantity : riderQuantities riders)
  -- The conjured card is literal text, Create's token's reason; the COUNT is the
  -- effect speaking, read in the resolution's own slots.
  Effect.Conjure (Conjure.MkConjure quantity _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ riders) -> all Quantity.slotsAreExhaustive (quantity : riderQuantities riders)
  Effect.BecomeCopy {} -> True
  Effect.CopyStackObject {} -> True
  -- The ReplacementEffect's own reads are replacementRowReads', and slotsOf
  -- reports them through replacementRowSlots: its Filters name no target slot, and
  -- the Quantities a counter rewrite counts with are asked here. Not implemented:
  -- the effects a rewrite or a CR 615.5 rider nests under this opcode read slots
  -- of their own and neither this answer nor slotsOf reports them; every
  -- Effect.Replace in data/cards/ nests none (gap #1962).
  Effect.Replace (Replace.MkReplace duration _ _ condition re) ->
    durationSlotsAreExhaustive duration
      && all conditionSlotsAreExhaustive condition
      && all Quantity.slotsAreExhaustive (snd (replacementRowReads re))
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> True
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ _ _ _ _ quantity rider) ->
    durationSlotsAreExhaustive duration && Quantity.slotsAreExhaustive quantity && all slotsAreExhaustive rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ _ _ _ _ _ rider) ->
    durationSlotsAreExhaustive duration && all slotsAreExhaustive rider
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ amount _ _ _ _ _) -> durationSlotsAreExhaustive duration && all Quantity.slotsAreExhaustive amount
  Effect.Counter {} -> True
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  -- No Quantity at all: CR 122.8 names neither a kind nor a count.
  Effect.PutCountersFrom {} -> True
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  -- The count the moved kinds may write. CR 122.5's GIVER carries the other one,
  -- through the ObjectRef it became when the first side was widened to a group,
  -- and it is effectObjectRefs' above -- an arm reading the kinds alone kept
  -- compiling (#2729).
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> all Quantity.slotsAreExhaustive (MovedKinds.quantityOf kinds)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.PayAnyEnergy _ -> True
  Effect.Tap _ -> True
  Effect.Untap _ -> True
  Effect.Detain _ -> True
  Effect.Goad _ -> True
  Effect.MakePlotted _ -> True
  Effect.DoesNotUntapNext _ -> True
  Effect.Transform _ -> True
  Effect.Convert _ -> True
  -- The combined face is interned with EMPTY bindings, CreateEmblem's reason, so
  -- its text is literal.
  Effect.Meld _ -> True
  Effect.PhaseOut _ -> True
  Effect.AddPhases _ -> True
  Effect.EndTurn -> True
  Effect.EndCombatPhase -> True
  -- slotsOf's arm drops this Duration, so the slotless test is made here.
  Effect.GainControl (DurationRef.MkDurationRef duration _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CR 603.7c: the armed ability inherits this object's whole environment.
  Effect.ArmDelayedTrigger {} -> False
  -- GainControl's reason for the Duration.
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- slotsOf drops the Duration, so the slotless test is made here.
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- RequireBlock's reason, one axis narrower.
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CantBeRegenerated's reason again.
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock duration _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- RequireBlock's reason, one axis over.
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CR 114.2's emblem is minted with EMPTY bindings, so its card is literal text.
  Effect.CreateEmblem _ -> True
  Effect.BecomeMonarch MonarchTarget.TheController -> True
  -- The one arm answering NO: CR 725.2 reads Binding.triggerSource.
  Effect.BecomeMonarch MonarchTarget.ControllerOfSource -> False
  Effect.BecomeMonarch (MonarchTarget.InSlot _) -> True
  Effect.TakeTheInitiative InitiativeTarget.TheController -> True
  -- CR 726.2 reads Binding.triggerSource, Effect.BecomeMonarch ControllerOfSource's answer.
  Effect.TakeTheInitiative InitiativeTarget.ControllerOfSource -> False
  Effect.Designate (Designate.MkDesignate _ _) -> True
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> True
  Effect.Unsuspect _ -> True
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> True
  Effect.Evolve _ -> True
  Effect.Mentor _ -> True
  Effect.Train _ -> True
  Effect.ItBecomes _ -> True
  Effect.ExileUntilMonarch _ -> True
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting _ _) -> True
  Effect.Attach _ -> True
  Effect.AttachTarget (AttachTarget.MkAttachTarget _ _) -> True
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget _ _) -> True
  Effect.AttachBound (AttachBound.MkAttachBound _ _) -> True
  -- CR 729.1b: a DEFINITION, and the subgame reads no binding of the outer game.
  Effect.PlaySubgame _ -> True
  -- PlaySubgame's answer: a definition reads no slot.
  Effect.ChooseOpponent _ -> True
  Effect.ChooseOpponentAtRandom _ -> True
  Effect.RollDie rollDie -> all Quantity.slotsAreExhaustive (RollDie.modifier rollDie)
  Effect.FlipCoin flipCoin -> Quantity.slotsAreExhaustive (FlipCoin.count flipCoin)
  Effect.TakeExtraTurn takeExtraTurn -> Quantity.slotsAreExhaustive (TakeExtraTurn.count takeExtraTurn)
  Effect.ShuffleIntoLibrary {} -> True
  Effect.Shuffle {} -> True
  Effect.OfferCast {} -> True
  Effect.GrantPlayFromExile grant -> durationSlotsAreExhaustive (GrantPlayFromExile.duration grant)
  -- PreventNextDamage's answer for the body, plus its own ref's: a PlayerRef
  -- nested in the DEPTH is one slotsOf cannot see.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> all slotsAreExhaustive body

-- CR 611.2b: only ForAsLongAs reads anything, through its Condition.
durationSlotsAreExhaustive :: Duration.Duration -> Bool
durationSlotsAreExhaustive duration = case duration of
  Duration.UntilEndOfTurn -> True
  Duration.Indefinite -> True
  Duration.Perpetual -> True
  Duration.UntilYourNextTurn -> True
  Duration.UntilEndOfYourNextTurn -> True
  Duration.ForAsLongAs condition -> conditionSlotsAreExhaustive condition
  -- durationSlots' answer: a Cost reads no slot, so its enumeration is complete.
  Duration.UntilPaid _ -> True
  Duration.UntilEndOfCombat -> True
  Duration.UntilUsed -> True

-- conditionSlots' mirror: both sides are a Quantity.
conditionSlotsAreExhaustive :: Condition.Type.Condition -> Bool
conditionSlotsAreExhaustive condition = case condition of
  Condition.Type.Compares c ->
    Quantity.slotsAreExhaustive (Compares.measured c) && Quantity.slotsAreExhaustive (Compares.threshold c)
  Condition.Type.Any conditions -> all conditionSlotsAreExhaustive conditions
  Condition.Type.All conditions -> all conditionSlotsAreExhaustive conditions

-- Does any of these effects read X? A card that reads X must declare it in its
-- cost (CR 107.3, CR 107.3a, CR 118.4), the same reads-equal-declares contract
-- slotsOf draws for target slots.
--
-- NOTE: when an opcode gains a Quantity FIELD, add its arm here by hand. A new
-- OPCODE the compiler forces, this case being exhaustive; widening an existing
-- one it does not, since an arm written `{} -> False` keeps compiling. That is
-- how a Quantity nested in an ObjectRef went unread once (#2729), so those are
-- taken from effectObjectRefs ahead of the case and no arm below names one.
readsX :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Bool
readsX = any effectReadsX
  where
    effectReadsX effect = any (any Quantity.readsX . objectRefQuantities) (effectObjectRefs effect) || effectOwnReadsX effect
    -- effectReadsX's half that is not an ObjectRef's: this opcode's own fields,
    -- and its nested effects through the recursion back into readsX.
    effectOwnReadsX effect = case effect of
      Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> any (Quantity.readsX . DamagePart.quantity) parts
      Effect.Fight {} -> False
      -- Untamed Might's "+X/+X" sits inside the Modification, not on the effect.
      Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ modification _) -> any Quantity.readsX (Projection.quantitiesOf modification)
      Effect.ChangeText {} -> False
      Effect.AddMana _ -> False
      Effect.Search (Search.MkSearch _ _ _ quantity _ _ _) -> any Quantity.readsX quantity
      Effect.ExileAllGraveyards -> False
      Effect.Proliferate -> False
      -- No Quantity: rule 201.4 chooses one name and states no count.
      Effect.ChooseCardName _ -> False
      Effect.FromOutsideTheGame _ -> False
      Effect.ExileThisSpell -> False
      Effect.Bolster quantity -> Quantity.readsX quantity
      Effect.Amass (Amass.Type.MkAmass quantity _) -> Quantity.readsX quantity
      Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.TemptWithTheRing -> False
      Effect.Venture {} -> False
      Effect.ExileHandThenDraw -> False
      Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> Quantity.readsX quantity
      Effect.RestartGame _ -> False
      Effect.ControlPlayerNextTurn _ -> False
      Effect.Destroy {} -> False
      Effect.Sacrifice _ -> False
      Effect.TurnFaceDown _ -> False
      Effect.TurnFaceUp _ -> False
      Effect.RemoveFromCombat _ -> False
      Effect.BecomesBlocked _ -> False
      -- The entry rider is a nested position of its own, CR 122.6's count per
      -- kind, and no ObjectRef holds it.
      Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> any Quantity.readsX (riderQuantities riders)
      Effect.Draw (Draw.MkDraw _ quantity _) -> Quantity.readsX quantity
      Effect.Mill (Mill.MkMill _ quantity _ _) -> Quantity.readsX quantity
      Effect.Reveal {} -> False
      Effect.LookAt {} -> False
      Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Explore {} -> False
      Effect.Discard subject -> case subject of
        Discard.Counted (CountedDiscard.MkCountedDiscard _ quantity _) -> Quantity.readsX quantity
        Discard.These {} -> False
      Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.ExchangeLifeTotals _ -> False
      Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.RedistributeLifeTotals -> False
      Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.DecreaseSpeed d -> Quantity.readsX (SpeedDecrease.quantity d)
      Effect.Create (Create.MkCreate quantity _ riders _ _) -> any Quantity.readsX (quantity : riderQuantities riders)
      Effect.Conjure (Conjure.MkConjure quantity _ _) -> Quantity.readsX quantity
      Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ riders) -> any Quantity.readsX (quantity : riderQuantities riders)
      Effect.BecomeCopy {} -> False
      Effect.CopyStackObject {} -> False
      Effect.Replace {} -> False
      Effect.SkipNextPhase {} -> False
      -- CR 601.2b's X reaches the rider too.
      Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ _ quantity rider) -> Quantity.readsX quantity || readsX (Foldable.toList rider)
      Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ _ _ _ rider) -> readsX (Foldable.toList rider)
      Effect.RedirectDamage (RedirectDamage.MkRedirectDamage _ _ amount _ _ _ _ _) -> any Quantity.readsX amount
      Effect.Counter {} -> False
      Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.readsX quantity
      Effect.PutCountersFrom {} -> False
      Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.readsX quantity
      Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> any Quantity.readsX (MovedKinds.quantityOf kinds)
      Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      -- CR 107.14's amount is asked for as the spell resolves, never CR
      -- 601.2b's announced X.
      Effect.PayAnyEnergy _ -> False
      Effect.Tap _ -> False
      Effect.Untap _ -> False
      Effect.Detain _ -> False
      Effect.Goad _ -> False
      Effect.MakePlotted _ -> False
      Effect.DoesNotUntapNext _ -> False
      Effect.Transform _ -> False
      Effect.Convert _ -> False
      Effect.Meld _ -> False
      Effect.PhaseOut _ -> False
      Effect.AddPhases _ -> False
      Effect.EndTurn -> False
      Effect.EndCombatPhase -> False
      Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
      Effect.ArmDelayedTrigger {} -> False
      Effect.AffectPlayers {} -> False
      Effect.RequireBlock {} -> False
      Effect.CantBeRegenerated {} -> False
      Effect.ForbidBlock {} -> False
      Effect.ForbidAttack {} -> False
      Effect.RequireAttack {} -> False
      Effect.CreateEmblem {} -> False
      Effect.BecomeMonarch {} -> False
      Effect.TakeTheInitiative {} -> False
      Effect.Designate (Designate.MkDesignate _ _) -> False
      Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> False
      Effect.Unsuspect _ -> False
      Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> False
      Effect.Evolve _ -> False
      Effect.Mentor _ -> False
      Effect.Train _ -> False
      Effect.ItBecomes _ -> False
      Effect.ExileUntilMonarch _ -> False
      Effect.ExileHaunting {} -> False
      Effect.Attach _ -> False
      Effect.AttachTarget {} -> False
      Effect.AttachTargetToEach {} -> False
      Effect.AttachBound {} -> False
      Effect.PlaySubgame _ -> False
      Effect.ChooseOpponent _ -> False
      Effect.ChooseOpponentAtRandom _ -> False
      -- CR 706.2's modifier is an ordinary Quantity, so it may be the X the
      -- caster announced (CR 601.2b).
      Effect.RollDie rollDie -> any Quantity.readsX (RollDie.modifier rollDie)
      -- The number of coins is an ordinary Quantity, so it may be the X the
      -- caster announced (Flock of Rabid Sheep's "flip X coins").
      Effect.FlipCoin flipCoin -> Quantity.readsX (FlipCoin.count flipCoin)
      -- The number of turns is an ordinary Quantity too.
      Effect.TakeExtraTurn takeExtraTurn -> Quantity.readsX (TakeExtraTurn.count takeExtraTurn)
      Effect.ShuffleIntoLibrary {} -> False
      Effect.Shuffle {} -> False
      Effect.OfferCast {} -> False
      Effect.GrantPlayFromExile {} -> False
      -- CR 608.2f's body is an effect list like any other, so an X inside it counts.
      Effect.ForEach (ForEach.MkForEach _ _ body) -> readsX (Foldable.toList body)

-- CR 603.7: the text an Effect.ArmDelayedTrigger's name resolves to, off the
-- SOURCE's own card.
--
-- The face that is up first, which is what every ordinary arm finds, and then the
-- card's other faces. That fallback is Ratchet, Field Medic's: "you may convert
-- Ratchet. When you do, return target artifact card ..." converts the permanent
-- and arms the reflexive in ONE clause (CR 608.2c's written order), so by the
-- time the arm runs the face that declared the ability is no longer the one up.
-- CR 603.7a makes the delayed ability something the RESOLVING ability creates,
-- and CR 603.7c is the same posture from the other end -- a delayed ability
-- survives its object changing characteristics -- so which face the permanent
-- happens to show as the opcode runs is not what says whether the text exists.
-- Letting a turn earlier in the same resolution blank it would be the wrong
-- reading of the rule as well as a trigger that could never fire.
--
-- Pawl.CardSpec's D4 dataflow lint is per FACE, so a name is declared on the face
-- that arms it and the fallback cannot pick up somebody else's ability: two faces
-- reusing one name would have to be two arms as well, and the lint's equality is
-- what would catch a card writing one.
declaredDelayedAbility :: ObjectId -> AbilityName -> GameState -> Maybe (TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
declaredDelayedAbility source name gs =
  let onFace = Game.faceOfWithLastKnown source gs >>= (Map.lookup name . Face.delayedAbilities)
      onCard = do
        card <- Game.cardOfWithLastKnown source gs
        Maybe.listToMaybe (Maybe.mapMaybe (Map.lookup name . Face.delayedAbilities) (NonEmpty.toList (Card.Type.faces card)))
   in onFace <|> onCard

-- CR 603.7: the delayed abilities an effect list ARMS, by name.
armedAbilities :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- CR 603.7: armedAbilities narrowed to the arms whose firing is gated past the
-- turn that armed them, i.e. not Onset.Immediately.
onsetGatedAbilities :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set AbilityName
onsetGatedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger _ Onset.Immediately _) -> Nothing
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- boundSlots over a whole effect list: the write half of the dataflow lint.
definedSlots :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Set SlotName
definedSlots = foldMap boundSlots

-- definedSlots' other half, one MODE at a time: the slot a CR 118.12 gate binds
-- as it is answered (Binding.gatePlayers, stamped by payGateAdmits). A mode
-- stating no gate binds nothing, so a card reading that name without offering a
-- resolution cost is still caught by the dataflow lint.
gateDefinedSlots :: Mode.Mode card ability -> Set SlotName
gateDefinedSlots mode
  | any (Maybe.isJust . Clause.payGate) (Mode.clauses mode) = Set.singleton Binding.gatePlayers
  | otherwise = Set.empty

-- gateDefinedSlots' twin for CR 603.5's "may": the seats that took it
-- (Binding.mayPlayers, stamped by exercises). A mode printing no "may" binds
-- nothing, so a card reading that name without an optional clause is still
-- caught by the dataflow lint.
mayDefinedSlots :: Mode.Mode card ability -> Set SlotName
mayDefinedSlots mode
  | any (isOptional . Clause.optionality) (Mode.clauses mode) = Set.singleton Binding.mayPlayers
  | otherwise = Set.empty
  where
    isOptional o = case o of
      Optionality.Mandatory -> False
      Optionality.Optional _ -> True

-- slotsOf's mirror for ONE effect: the slots it BINDS rather than reads, which
-- is also the set Pawl.CardSpec's reserved-name sweep ranges over. Exhaustive
-- deliberately: a wildcard would file a new bind position under "binds nothing"
-- in both the dataflow lint and that sweep, with no diagnostic.
boundSlots :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Set SlotName
boundSlots effect = case effect of
  -- CR 400.7: the incarnation minted at the destination.
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ _ mSlot _ _ _) -> foldMap Set.singleton mSlot
  -- The tokens this Create minted, for CR 603.7c's delayed trigger to name.
  Effect.Create (Create.MkCreate _ _ _ mSlot _) -> foldMap Set.singleton mSlot
  -- Not implemented: Pawl.Types.Conjure carries no slot, so a printing that DOES
  -- name the conjured card later in its own instruction list (Kari Zev, Crew of
  -- Two's "if that card is on the battlefield, return it to its owner's hand")
  -- cannot be transcribed and this binds nothing (#2638).
  Effect.Conjure {} -> Set.empty
  Effect.CreateCopy {} -> Set.empty
  -- Binds nothing: no new object comes into existence.
  Effect.BecomeCopy {} -> Set.empty
  Effect.CopyStackObject {} -> Set.empty
  -- CR 729.1b: the subgame's winner, reported rather than chosen.
  Effect.PlaySubgame slot -> Set.singleton slot
  -- CR 608.2d: the opponent this effect chose.
  Effect.ChooseOpponent slot -> Set.singleton slot
  Effect.ChooseOpponentAtRandom slot -> Set.singleton slot
  -- CR 706.4: the number the die came up, for a later effect of this resolution
  -- to read as Quantity.InSlot.
  Effect.RollDie rollDie -> Set.singleton (RollDie.slot rollDie)
  -- CR 705.2: how many of the instruction's flips the flipping player won (or
  -- how many coins came up heads), and, where the card reads it, how many they
  -- lost, for a later effect of this resolution to read as Quantity.InSlot.
  Effect.FlipCoin flipCoin -> Set.singleton (FlipCoin.slot flipCoin) <> foldMap Set.singleton (FlipCoin.misses flipCoin)
  -- Three slots CR 701.8's destruction may define: how many permanents it
  -- ACTUALLY destroyed, for a later "for each ... destroyed this way"; the cards
  -- it put into a graveyard, for a later clause that NAMES them (CR 400.7's
  -- incarnations); and the PERMANENTS it destroyed, for a later clause that
  -- walks them one at a time.
  Effect.Destroy (Destroy.MkDestroy _ _ mSlot mBuried mPermanents) -> foldMap Set.singleton mSlot <> foldMap Set.singleton mBuried <> foldMap Set.singleton mPermanents
  -- How many milled cards matched the tally's filter (CR 728.1), and WHICH cards
  -- the mill put in the graveyard, for a later clause that names them (CR
  -- 701.17c). Two slots and not one: a card may write either without the other.
  Effect.Mill (Mill.MkMill _ _ mTally mSlot) -> foldMap (Set.singleton . MillTally.slot) mTally <> foldMap Set.singleton mSlot
  -- The cards CR 701.20a's reveal showed, where the card named a slot. Optional,
  -- where LookAt's is not: the GameEvent.Revealed in the log is a record already.
  Effect.Reveal (Reveal.MkReveal _ mSlot) -> foldMap Set.singleton mSlot
  -- The cards CR 701.20e's look showed, for a later clause to name.
  Effect.LookAt (LookAt.MkLookAt _ slot) -> Set.singleton slot
  Effect.Scry {} -> Set.empty
  Effect.Surveil {} -> Set.empty
  Effect.Fateseal {} -> Set.empty
  Effect.Explore {} -> Set.empty
  Effect.DealDamage {} -> Set.empty
  Effect.Fight {} -> Set.empty
  Effect.ModifyTarget {} -> Set.empty
  Effect.ChangeText {} -> Set.empty
  Effect.AddMana _ -> Set.empty
  Effect.Search {} -> Set.empty
  Effect.ExileAllGraveyards -> Set.empty
  Effect.Proliferate -> Set.empty
  -- Binds nothing: the name goes on the SOURCE (Object.chosenNames) and is read
  -- back off it by Filter.HasChosenName, so no slot carries it.
  Effect.ChooseCardName _ -> Set.empty
  Effect.FromOutsideTheGame _ -> Set.empty
  Effect.ExileThisSpell -> Set.empty
  Effect.Bolster _ -> Set.empty
  Effect.Amass _ -> Set.empty
  Effect.Blight _ -> Set.empty
  Effect.TemptWithTheRing -> Set.empty
  Effect.Venture {} -> Set.empty
  Effect.ExileHandThenDraw -> Set.empty
  Effect.PlayerSacrifices {} -> Set.empty
  Effect.RestartGame _ -> Set.empty
  Effect.ControlPlayerNextTurn _ -> Set.empty
  Effect.Sacrifice _ -> Set.empty
  Effect.TurnFaceDown _ -> Set.empty
  Effect.TurnFaceUp _ -> Set.empty
  Effect.RemoveFromCombat _ -> Set.empty
  Effect.BecomesBlocked _ -> Set.empty
  -- CR 121.1's cards "drawn this way", as CR 400.7's incarnations in the hand
  -- they arrived in.
  Effect.Draw (Draw.MkDraw _ _ mSlot) -> foldMap Set.singleton mSlot
  -- CR 701.9a's cards "discarded this way", as CR 400.7's incarnations. The
  -- These arm has none, for the reason its type carries.
  --
  -- A REGRESSION FENCE rather than proven behaviour: emptying this arm leaves
  -- the suite green. The only consumer is Pawl.CardSpec's D4 dataflow lint, and
  -- the read it would have to notice is a Filter.IsBound inside a Count inside a
  -- Clause.condition -- which modeSlots does not fold at all, and which
  -- Count.slots would not descend into if it did (#1079).
  Effect.Discard subject -> case subject of
    Discard.Counted (CountedDiscard.MkCountedDiscard _ _ mDiscarded) -> foldMap Set.singleton mDiscarded
    Discard.These _ -> Set.empty
  Effect.LoseLife {} -> Set.empty
  Effect.GainLife {} -> Set.empty
  Effect.ExchangeLifeTotals _ -> Set.empty
  Effect.SetLifeTotal {} -> Set.empty
  Effect.RedistributeLifeTotals -> Set.empty
  Effect.IncreaseSpeed {} -> Set.empty
  Effect.DecreaseSpeed {} -> Set.empty
  Effect.Replace {} -> Set.empty
  Effect.SkipNextPhase {} -> Set.empty
  -- The shield itself binds nothing; CR 615.5's rider is an effect list, so a
  -- name IT authors is a name this card authors. Both shields.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ _ _ rider) -> foldMap boundSlots rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ _ _ _ rider) -> foldMap boundSlots rider
  Effect.RedirectDamage {} -> Set.empty
  -- How many spells this countering ACTUALLY countered, for a "for each spell
  -- countered this way", and the permanents whose abilities were (CR 113.7).
  Effect.Counter (Counter.MkCounter _ mSlot mSources) -> foldMap Set.singleton mSlot <> foldMap Set.singleton mSources
  Effect.PutCounters {} -> Set.empty
  Effect.PutCountersFrom {} -> Set.empty
  Effect.RemoveCounters {} -> Set.empty
  -- How many counters CR 122.5 ACTUALLY moved, for a "that much life".
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ _ mSlot _) -> foldMap Set.singleton mSlot
  Effect.GainPlayerCounters {} -> Set.empty
  Effect.RemovePlayerCounters {} -> Set.empty
  -- CR 107.14: how much {E} the payer paid, for a later effect of the same
  -- resolution to read as Quantity.InSlot.
  Effect.PayAnyEnergy slot -> Set.singleton slot
  Effect.Tap _ -> Set.empty
  Effect.Untap _ -> Set.empty
  Effect.Detain _ -> Set.empty
  Effect.Goad _ -> Set.empty
  Effect.MakePlotted _ -> Set.empty
  Effect.DoesNotUntapNext _ -> Set.empty
  Effect.Transform _ -> Set.empty
  Effect.Convert _ -> Set.empty
  -- CR 701.42a's melded permanent is bound to nothing: no printing names it later
  -- in its own instruction list.
  Effect.Meld _ -> Set.empty
  Effect.PhaseOut _ -> Set.empty
  Effect.AddPhases _ -> Set.empty
  Effect.EndTurn -> Set.empty
  Effect.EndCombatPhase -> Set.empty
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> Set.empty
  Effect.ArmDelayedTrigger {} -> Set.empty
  Effect.AffectPlayers {} -> Set.empty
  Effect.RequireBlock {} -> Set.empty
  Effect.CantBeRegenerated {} -> Set.empty
  Effect.ForbidBlock {} -> Set.empty
  Effect.ForbidAttack {} -> Set.empty
  Effect.RequireAttack {} -> Set.empty
  Effect.CreateEmblem {} -> Set.empty
  Effect.BecomeMonarch {} -> Set.empty
  Effect.TakeTheInitiative {} -> Set.empty
  Effect.Designate (Designate.MkDesignate _ _) -> Set.empty
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> Set.empty
  Effect.Unsuspect _ -> Set.empty
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> Set.empty
  Effect.Evolve _ -> Set.empty
  Effect.Mentor _ -> Set.empty
  Effect.Train _ -> Set.empty
  Effect.ItBecomes _ -> Set.empty
  Effect.ExileUntilMonarch _ -> Set.empty
  Effect.ExileHaunting {} -> Set.empty
  Effect.Attach _ -> Set.empty
  Effect.AttachTarget {} -> Set.empty
  Effect.AttachTargetToEach {} -> Set.empty
  Effect.AttachBound {} -> Set.empty
  Effect.TakeExtraTurn {} -> Set.empty
  Effect.ShuffleIntoLibrary {} -> Set.empty
  Effect.Shuffle {} -> Set.empty
  Effect.OfferCast {} -> Set.empty
  Effect.GrantPlayFromExile {} -> Set.empty
  -- The loop's member slot, plus every name the BODY authors.
  Effect.ForEach (ForEach.MkForEach _ slot body) -> Set.insert slot (foldMap boundSlots body)

-- Does a Create's slot name EVERY token it minted rather than one particular one
-- (CR 603.7c's "it")? CR 111 gives the opcode no way to carry the word, and the
-- count at RESOLUTION cannot tell them apart -- CR 614.16 lets a replacement
-- multiply either -- so the PRINTED quantity decides. Nothing in card data
-- records the word, so the inference cannot be linted.
namesEveryToken :: Quantity.Type.Quantity -> Bool
namesEveryToken quantity = quantity /= Quantity.Type.Literal 1

-- CR 111.3: the values the creating effect defines become the token's text, so a
-- computed power or toughness is settled here and stamped as a literal. Left as a
-- quantity it would be re-read on every projection, and GameState.events is
-- cleared at the turn handoff -- the token would have no P/T next turn and die to
-- a state-based action. P/T is the whole of it: the only printed characteristics
-- a Face holds as a Quantity.
--
-- A box holding CR 208.2's STAR is left standing, so Projection.seedCharacteristicPT
-- still has a star to substitute the token's characteristic-defining ability into
-- at layer 7a. Everything else is settled even when it cannot be evaluated, on CR
-- 208.2a's terms (Quantity.determineWith: an undeterminable number is 0, including
-- inside a calculation) -- Miming Slime with no creatures makes a 0/0 Ooze, which
-- CR 704.5f puts away unless something is raising its toughness. Left standing it
-- would instead be a board-reading box on a permanent, which is not a thing CR
-- 208 allows a token to have.
--
-- The star half is a REGRESSION FENCE, not a proven behaviour: no card in
-- data/cards mints a token whose printed box holds a Star (grepped 2026-08-24),
-- so making containsStar answer False everywhere leaves the suite green. It is
-- kept because CR 208.2 states it.
bakeTokenCharacteristics :: (Quantity.Type.Quantity -> Maybe Integer) -> Card.Type.Card -> Card.Type.Card
bakeTokenCharacteristics eval card = card {Card.Type.faces = fmap bakeFace (Card.Type.faces card)}
  where
    bake quantity =
      if Star.containsStar quantity
        then quantity
        else Quantity.Type.Literal (Quantity.determineWith eval quantity)
    bakeFace face =
      face
        { Face.power = fmap (Power.MkPower . bake . Power.unwrap) (Face.power face),
          Face.toughness = fmap (Toughness.MkToughness . bake . Toughness.unwrap) (Face.toughness face)
        }

-- A resolving spell's PROJECTED modes: only its chosen ones (CR 608.2c/700.2),
-- with every text change affecting it applied (CR 612). Modes rather than a flat
-- effect list because CR 603.5's "may" belongs to a clause within a mode.
--
-- The mode's TARGET SLOTS are rewritten by targetSlotsOf instead: CR 608.2b
-- re-reads them off the printed face, which unions in CR 303.4a's enchant slot.
modesOf :: ObjectId -> GameState -> [(ModeInstance, Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))]
modesOf oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Game.faceOf oid gs of
    Nothing -> []
    Just face ->
      let chosen = Binding.modesOf (Object.bindings obj)
          changes = Projection.textChangesAffecting oid gs
          rewrite = Projection.rewriteEffect changes
          rewriteClause c =
            c
              { Clause.effects = fmap rewrite (Clause.effects c),
                -- A clause gate's Filters are printed words CR 612.1 changes.
                Clause.condition = fmap (Projection.rewriteCondition changes) (Clause.condition c)
              }
          rewriteMode m = m {Mode.clauses = fmap rewriteClause (Mode.clauses m)}
       in fmap (fmap rewriteMode) (Card.chosenModes chosen face)

-- CR 608.2b: a target can stop being legal because an effect changed the spell's
-- text, so the re-check measures the spell's own slots with CR 612.1's changes
-- applied. Off the face rather than off modesOf, which has no room for CR
-- 303.4a's enchant slot. Both readers -- the fizzle test and resolveSpellWith's
-- per-effect skip -- go through this, so they cannot disagree.
targetSlotsOf :: Object.Object -> ObjectId -> GameState -> Face.Face Card.Type.Card -> Map.Map SlotName TargetSlot.TargetSlot
targetSlotsOf obj oid gs face =
  fmap
    (Projection.rewriteTargetSlot (Projection.textChangesAffecting oid gs))
    -- CR 303.4a's slot comes off the PROJECTION and not off `face`, which is the
    -- only reading that sees a granted enchant ability -- CR 702.103b gives a
    -- spell cast bestowed one, and its printed face declares none. A printed
    -- Aura's projection is seeded from that same printed list, so this is the
    -- wider read rather than a different one.
    (Card.modesTargetSlotsGiven (Projection.enchantOf oid gs) (Binding.modesOf (Object.bindings obj)) face)

-- CR 405.4: who controls a SPELL on the stack -- both CR 608.2b's legality
-- perspective and the effects' execution, which must name the same player. The
-- player who CAST it, stamped at CR 601.2a's move, but read THROUGH the
-- projection because CR 613.1b's layer 2 can override it (CR 109.4).
spellController :: Object.Object -> ObjectId -> GameState -> PlayerId
spellController obj oid gs = Maybe.fromMaybe (Projection.defaultControllerOf obj) (Projection.controllerOf oid gs)

-- CR 608.2b: are ALL of this spell's targets illegal? A spell with no target slot
-- never fizzles, and one with several survives if any one is still legal.
-- Reserved slots are not targets and are vacuously legal. Shared with the Aura
-- path in Pawl.Engine.Stack, so the two cannot drift.
targetsAllIllegal :: ObjectId -> GameState -> Bool
targetsAllIllegal oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Game.faceOf oid gs of
    Nothing -> False
    Just face ->
      let slots = targetSlotsOf obj oid gs face
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            Nothing -> recipients
            -- CR 608.2b's perspective is the SPELL's controller (CR 405.4).
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) (Object.bindings obj) oid recipient targetSlot gs) recipients
          legal = Map.mapWithKey legalSlot chosen
          targeted = Map.restrictKeys legal (Map.keysSet slots)
       in -- Measured on the TARGETS chosen, not the slots declared: CR 115.6
          -- makes a spell that chose zero targets untargeted.
          not (Map.null targeted) && all Set.null (Map.elems targeted)

-- CR 608.2b then CR 608.2: re-validate every filled slot; if the spell has slots
-- and ALL are now illegal it fizzles to the graveyard with no effect applied.
-- Otherwise the effects run in order (CR 608.2c), each skipping a slot whose
-- target is illegal, and the spell goes to its owner's graveyard (CR 608.2n).
--
-- Per CR 608.2c the bindings are re-read before EACH effect, so a slot DEFINED
-- mid-resolution is visible to a later one; target-slot legality stays fixed at
-- the start. `runSubgame` is the injected nested-game runner.
resolveSpellWith :: Game Result -> ObjectId -> Game ()
resolveSpellWith runSubgame oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Game.faceOf oid gs of
      Nothing -> pure ()
      Just face ->
        -- CR 608.2b/700.2c: re-validate only the CHOSEN modes' slots.
        let chosenSelection = Binding.modesOf (Object.bindings obj)
            slots = targetSlotsOf obj oid gs face
            -- CR 700.2d: the slots the MODES own -- `slots` minus CR 303.4a's
            -- enchant slot.
            modeOwnedSlots = Modal.modesTargetSlots chosenSelection (Face.spell face)
            legalSlot slot recipients = case Map.lookup slot slots of
              -- CR 608.2b is about TARGETS. A slot declaring none is a RESERVED
              -- binding and was never targeted.
              Nothing -> recipients
              -- Per RECIPIENT and not per slot (CR 608.2b): the slot's surviving
              -- targets are still affected.
              Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) (Object.bindings obj) oid recipient targetSlot gs) recipients
         in if targetsAllIllegal oid gs
              then Event.changeZone oid Zone.Graveyard
              else do
                let effectController = spellController obj oid gs
                Monad.forM_ (modesOf oid gs) $ \(mi, mode) -> do
                  let idx = ModeInstance.index mi
                      applyOne eff = do
                        -- Re-read the live bindings for THIS effect: a prior
                        -- PlaySubgame may have bound its winner slot.
                        bindingsNow <- State.gets (liveBindings obj oid)
                        let chosenNow = Binding.targetsOf bindingsNow
                            legalNow = Map.mapWithKey legalSlot chosenNow
                        applyEffectWith
                          runSubgame
                          oid
                          oid
                          effectController
                          (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) legalNow)
                          (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) chosenNow)
                          eff
                  -- CR 608.2e's clause is the unit all four gates cover, so each
                  -- is asked once per clause. The fold carries this mode
                  -- INSTANCE's CR 118.12 answers and the clauses whose
                  -- instructions ran, per instance because CR 700.2d makes a mode
                  -- chosen twice make its offer twice.
                  Monad.foldM_
                    ( \(answers, picked, ran) (cIdx, clause) -> do
                        -- CR 608.2c's "If you do" first: a clause hanging off one
                        -- the fold has not recorded is skipped entirely, so no
                        -- later gate raises a prompt whose answer cannot matter.
                        let hangs = ifTakenHolds ran clause
                        -- CR 701.46a's printed "if" next, against the LIVE
                        -- bindings (CR 608.2c): a slot an earlier clause DEFINED
                        -- is part of the state this one is read against, and the
                        -- re-read adds only defined slots. A REGRESSION FENCE --
                        -- mutating this half back leaves the suite green.
                        gateBindings <- State.gets (liveBindings obj oid)
                        gated <- if hangs then gateHolds effectController oid (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Binding.targetsOf gateBindings)) gateBindings clause else pure False
                        -- CR 603.5 / 608.2d: then the printed "may", against the
                        -- SAME live bindings CR 608.2b's filter is applied to, so
                        -- a clause whose every read is dead is not asked about.
                        let legalNowForMay = Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                            boundNowForMay = Map.keysSet (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) gateBindings)
                        -- CR 608.2d's "or" next, and BEFORE the "may": Twiddle
                        -- prints one "may" over the pair, so a branch a player
                        -- did not announce has no "may" left to offer THEM.
                        (announced, picked') <- if gated then chosenBranch oid effectController idx cIdx legalNowForMay picked clause else pure (Just Set.empty, picked)
                        let branch = maybe True (not . Set.null) announced
                        taken <- if branch then exercises oid effectController idx cIdx boundNowForMay legalNowForMay announced clause else pure False
                        -- CR 118.12: then the cost paid on resolution, against the
                        -- START-of-resolution targets to match CR 608.2b's single
                        -- re-validation. Both maps are projected into THIS
                        -- instance's view (CR 700.2d) after legality is decided,
                        -- since deciding it after the rename would miss in `slots`.
                        (admitted, answers') <-
                          if taken
                            then
                              let chosenAtStart = Binding.targetsOf (Object.bindings obj)
                               in payGateAdmits
                                    oid
                                    oid
                                    effectController
                                    idx
                                    cIdx
                                    (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Map.mapWithKey legalSlot chosenAtStart))
                                    announced
                                    answers
                                    clause
                            else pure (False, answers)
                        Monad.when admitted (applyClauseEffects oid applyOne (Foldable.toList (Clause.effects clause)))
                        pure (answers', picked', recordTaken admitted cIdx ran)
                    )
                    (Map.empty, Map.empty, Set.empty)
                    (zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode)))
                finishSpell oid face effectController

-- CR 608.2n / 715.3d / 720.3d: where the spell goes as the last part of its
-- resolution -- its owner's graveyard, unless it was cast as an Adventure, when
-- its controller exiles it and CR 715.3d's permission to play it goes onto the
-- exiled card, or as an Omen, when its controller shuffles it into its OWNER's
-- library instead.
--
-- Reached only from the RESOLVING path: a fizzled spell does not resolve (CR
-- 608.2b), so CR 715.3d's "as it resolves" never applies to it. Written onto the
-- id the move RETURNS, since CR 400.7 mints a fresh incarnation in exile.
--
-- Both riders are keyed on the CHOSEN FACE's spell type (CR 205.3k) rather than
-- on the card's layout, because the question is which set of characteristics is
-- resolving rather than which card printed them -- a classification either way,
-- never an effect's identity.
finishSpell :: ObjectId -> Face.Face Card.Type.Card -> PlayerId -> Game ()
finishSpell oid face controller
  -- CR 720.3d: "As an Omen spell resolves, its controller shuffles it into its
  -- owner's library instead of putting it into its owner's graveyard as it
  -- resolves." CR 108.3 makes the library the OWNER's, read BEFORE the move for
  -- Effect.ShuffleIntoLibrary's reason -- CR 400.7 mints a fresh incarnation, so
  -- the owner has to be in hand whether or not the move produced one. CR 701.24a
  -- is the randomisation, through the same Event.shuffleLibrary that opcode uses.
  | Card.isOmen face = do
      owner <- State.gets (fmap Object.owner . Game.lookupObject oid)
      Event.changeZone oid Zone.Library
      Monad.forM_ owner Event.shuffleLibrary
  | not (Card.isAdventure face) = Event.changeZone oid Zone.Graveyard
  | otherwise = do
      exiled <- Event.changeZoneReturning oid Zone.Exile
      Monad.forM_ exiled $ \newId ->
        State.modify' $ \gs ->
          gs
            { GameState.objects =
                Map.adjust (\o -> o {Object.playableFromExile = Just (permission newId)}) newId (GameState.objects gs)
            }
  where
    -- Never per CR 611.2a: CR 715.3d states no duration. What ends it is CR
    -- 400.7 -- leaving exile mints a new incarnation, and newIncarnation clears
    -- the field.
    permission newId =
      ExilePlayPermission.MkExilePlayPermission
        { ExilePlayPermission.player = controller,
          ExilePlayPermission.source = newId,
          ExilePlayPermission.expiry = Expiry.Type.Never,
          -- CR 715.3d says nothing about mana.
          ExilePlayPermission.spending = ManaSpending.AsProduced,
          -- This IS rule 715.3d's permission, and so the one its next sentence
          -- excludes the Adventure half from.
          ExilePlayPermission.origin = PlayPermissionOrigin.Adventure
        }

-- The no-subgame spell resolver (Stack's default path and every direct caller).
resolveSpell :: ObjectId -> Game ()
resolveSpell = resolveSpellWith noSubgame

-- CR 608.2: the executor shared by an activated and a triggered ability on the
-- stack. Re-validates filled slots (CR 608.2b), walks the CHOSEN modes in order
-- (CR 608.2c/700.2c) applying each one's effects with `srcId` as the effect source
-- (CR 113.7) and asking about any printed "may" (CR 603.5), then the ability
-- ceases (CR 608.2n). `stackId` is the ability object's own id, and the slots ARE
-- the union of the chosen modes' own (CR 700.2c).
--
-- The bindings are re-read before EACH effect (CR 608.2c), but CR 608.2b's
-- question is asked ONCE off the pre-fold snapshot: re-deriving the fizzle
-- mid-fold would let a token a Create just minted rescue an ability whose every
-- target is gone.
--
-- `runSubgame` is the injected nested-game runner, the same one resolveSpellWith
-- takes: CR 729.1a says "the spell or ability that created the subgame", so an
-- ability's PlaySubgame plays one exactly as a spell's does (see #137).
resolveModesWith :: Game Result -> ObjectId -> ObjectId -> [(ModeInstance, Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))] -> Game ()
resolveModesWith runSubgame stackId srcId modes = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      -- CR 700.2d: instance-named, not printed-named -- two instances of one
      -- repeated mode fill two slots this union would otherwise collapse.
      -- Modal.modeInstanceTargetSlots rather than a rename of its own: the slot
      -- names a pool and a filter carry are rewritten along with the key, which
      -- is what makes this the same map CR 601.2c was answered against. CR
      -- 608.2b re-judges each against the SAME declaration CR 603.3d offered, so
      -- the "that player controls" atoms are baked here too; an ability whose
      -- environment binds no player leaves them standing, admitting nothing.
      --
      -- The ORDER is deliberately not load-bearing: Engine.placeBorne bakes the
      -- whole modal and then renames, this renames and then bakes, and CR 700.2d's
      -- rename touches only the names a mode DECLARES (Modal.ownSlot), which the
      -- bake never reads. Pawl.ModalSpec's "CR 700.2d a repeated mode on a trigger
      -- keeps reading the trigger's own bound player" is what proves the two paths
      -- agree.
      let slots = Target.bakeSlots (Binding.playerSlots (Object.bindings obj)) (Map.unions (fmap (uncurry Modal.modeInstanceTargetSlots) modes))
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            -- CR 608.2b is about TARGETS. A slot declaring none is a RESERVED
            -- binding and can never have become an illegal target.
            Nothing -> recipients
            -- CR 608.2b: the perspective is the ABILITY's controller. `srcId`
            -- stays the source (CR 113.7) and may well be gone -- exactly the
            -- case this rule is about. Judged per RECIPIENT.
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just effectController) (Object.bindings obj) srcId recipient targetSlot gs) recipients
          legal = Map.mapWithKey legalSlot chosen
          -- CR 608.2b's fizzle asks about the TARGETED slots only, measured on
          -- the slots FILLED rather than declared (CR 115.6).
          targeted = Map.restrictKeys legal (Map.keysSet slots)
          fizzles = not (Map.null targeted) && all Set.null (Map.elems targeted)
          -- CR 113.8 / 603.3a: stamped as Object.owner at the ability's creation
          -- and never revisited, so a stolen permanent's later controller must
          -- not override it.
          effectController = Object.owner obj
          resolveOne (mi, mode) =
            let idx = ModeInstance.index mi
                -- CR 700.2d: this instance's slots under the names its mode
                -- prints, applied to both maps so they cannot disagree.
                instanceView = Modal.instanceView slots mi (Mode.targetSlots mode)
                applyOne eff = do
                  -- Re-read the LIVE bindings for THIS effect (CR 608.2c). Both
                  -- maps come from the SAME bindings: `legalNow` is `chosenNow`
                  -- with CR 608.2b's illegal recipients dropped, so re-reading one
                  -- without the other would lose the bindings it just gained.
                  bindingsNow <- State.gets (liveBindings obj stackId)
                  let chosenNow = Binding.targetsOf bindingsNow
                      legalNow = Map.mapWithKey legalSlot chosenNow
                  applyEffectWith runSubgame stackId srcId effectController (instanceView legalNow) (instanceView chosenNow) eff
             in -- CR 608.2e's clause is what each gate covers. Run only when
                -- `fizzles` is False.
                Monad.foldM_
                  ( \(answers, picked, ran) (cIdx, clause) -> do
                      -- CR 608.2c's "If you do" first, off the same fold the
                      -- spell path keeps. Proved on this path, not merely
                      -- fenced: Aetherplasm's second clause hangs on its first,
                      -- and Pawl.CombatEffectSpec's "declining to return
                      -- Aetherplasm skips the clause its 'If you do' hangs on"
                      -- reddens when this conjunct is defeated. What #1887 still
                      -- covers on this loop is the OTHER gate -- an observable
                      -- MANDATORY clause standing before a printed "may".
                      let hangs = ifTakenHolds ran clause
                      -- CR 701.46a's printed "if" next, read against `srcId` --
                      -- the rule says "this permanent", which is also why
                      -- `payGatePaid` is given `srcId`. Off the LIVE bindings of
                      -- the STACK object (CR 608.2c), where this resolution's
                      -- slots are bound (see bindSlot).
                      gateBindings <- State.gets (liveBindings obj stackId)
                      gated <- if hangs then gateHolds effectController srcId (instanceView (Binding.targetsOf gateBindings)) gateBindings clause else pure False
                      -- CR 603.5 / 608.2d: then the printed "may", against the
                      -- SAME live bindings CR 608.2b's filter is applied to, so a
                      -- clause whose every read is dead is not asked about.
                      let legalNowForMay = instanceView (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                          boundNowForMay = Map.keysSet (instanceView gateBindings)
                      -- CR 608.2d's "or" next, and BEFORE the "may", off the same
                      -- helper the spell path uses. Proved on THIS loop and not
                      -- merely on the spell's twin: Teardrop Kami's "sacrifice
                      -- this creature: you may tap or untap target creature" is
                      -- Pawl.ResolveSpec's "CR 608.2d announcing Teardrop Kami's
                      -- tap taps the untapped Piker", which reddens when this
                      -- conjunct is defeated.
                      (announced, picked') <- if gated then chosenBranch stackId effectController idx cIdx legalNowForMay picked clause else pure (Just Set.empty, picked)
                      let branch = maybe True (not . Set.null) announced
                      taken <- if branch then exercises stackId effectController idx cIdx boundNowForMay legalNowForMay announced clause else pure False
                      -- CR 118.12: then the cost paid on resolution, against the
                      -- START-of-resolution slots.
                      (admitted, answers') <- if taken then payGateAdmits stackId srcId effectController idx cIdx (instanceView legal) announced answers clause else pure (False, answers)
                      Monad.when admitted (applyClauseEffects srcId applyOne (Foldable.toList (Clause.effects clause)))
                      pure (answers', picked', recordTaken admitted cIdx ran)
                  )
                  (Map.empty, Map.empty, Set.empty)
                  (zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode)))
       in do
            Monad.unless fizzles (Monad.forM_ modes resolveOne)
            State.modify' (Game.cease stackId)

-- CR 608.2c: one clause's instructions, in written order, carrying the one thing
-- a later instruction can ask about an earlier one -- whether it HAPPENED. CR
-- 701.28e is what makes that question observable: an instruction to convert a
-- permanent that has already converted "is ignored", so a CR 603.12 reflexive
-- ability armed after it hangs off a trigger event that never occurred, and rule
-- 603.12 creates it only "based on whether the trigger event or events occurred".
--
-- WHAT ANSWERS IT is the event log rather than the opcode: an instruction that
-- took place recorded a game event, an ignored one recorded none. A board
-- difference, the posture applyEffectWith's CR 607.2a filing already takes, so
-- the rules core reads the ACT rather than which effect it was. Ratchet, Field
-- Medic's two trigger instances are Pawl.TransformSpec's "CR 701.28e / 603.12 an
-- ignored convert arms no reflexive", the case that proves it.
--
-- Effect.ForEach's body (below) runs its instructions through this SAME fold,
-- reset per member rather than carried across members. Synthetic Communal Toll
-- (data/cards/synthetic-communal-toll.json) stands in for Nihiloor's "for each
-- opponent, tap up to one untapped creature you control. When you do, ..."
-- (#3166); Pawl.ResolveSpec's "CR 608.2f / 603.12 a reflexive armed inside a
-- ForEach reads only that member's own instruction" is the case that proves it.
--
-- Not implemented: only the instruction IMMEDIATELY before the arm is asked
-- about, not whichever earlier instruction the reflexive's own wording names
-- ("do A. do B. when you do A" would read B's outcome instead); no card in
-- `data/cards/` writes an arm that is not second in its clause or its ForEach
-- body (#3057). "Happened" is read as "recorded a game event", which misses an
-- effect that mutates the board without one -- Effect.Detain and Effect.Goad
-- are two -- and would wrongly suppress an arm that follows it (#3165).
applyClauseEffects ::
  ObjectId ->
  (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()) ->
  [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] ->
  Game ()
applyClauseEffects source applyOne = Monad.foldM_ step True
  where
    step happened effect = do
      skipped <- if happened then pure False else State.gets (armsReflexive source effect)
      before <- State.gets (Seq.length . GameState.events)
      Monad.unless skipped (applyOne effect)
      after <- State.gets (Seq.length . GameState.events)
      pure (after > before)

-- CR 603.12: does this instruction create a REFLEXIVE triggered ability? The name
-- is resolved exactly as the arm itself resolves it (declaredDelayedAbility, then
-- rule 702's minted roster), and the answer is the created ability's own
-- CLASSIFICATION -- its trigger condition -- never which card armed it.
armsReflexive :: ObjectId -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> GameState -> Bool
armsReflexive source effect gs = case effect of
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) ->
    maybe
      False
      ((== TriggerCondition.Reflexive) . TriggeredAbility.condition)
      (declaredDelayedAbility source name gs <|> Keyword.mintedDelayedAbility name)
  _ -> False

-- CR 608.2c: does this clause's printed "If you do" hold? A clause naming no
-- earlier one always happens; one that names an earlier clause of this mode
-- instance happens only if that clause's instructions ran -- Tweeze's "you may
-- discard a card. If you do, draw a card".
--
-- Off the fold's record of what ran, so the answer is the one the named clause's
-- own riders gave (Clause.ifTaken says why that rather than the board), and a
-- name the fold has not reached -- a later clause, or one that does not exist --
-- is False. ANY of the names is enough, which is what Worms of the Earth's "if a
-- player does either" prints over the two halves of an either-or pair. Asked
-- BEFORE the other three, so a skipped clause raises no prompt.
--
-- A pure function rather than a Game action: it reads nothing but the fold.
ifTakenHolds :: Set ClauseIndex -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
ifTakenHolds ran clause = maybe True (any (`Set.member` ran)) (Clause.ifTaken clause)

-- The other end of the same fold: a clause's ordinal is recorded exactly when
-- its instructions ran, which is what CR 608.2c's "If you do" asks about. One
-- writer for both resolution paths, so the spell loop and the ability loop
-- cannot disagree about what "you did" means.
recordTaken :: Bool -> ClauseIndex -> Set ClauseIndex -> Set ClauseIndex
recordTaken admitted cIdx ran = if admitted then Set.insert cIdx ran else ran

-- CR 701.46a: does this clause's printed "if" hold? CR 701.37a prints the same
-- gate on a proper prefix of a longer ability, which is why the rider is on CR
-- 608.2e's clause rather than on the mode. A clause stating no condition always
-- happens. Asked as the clause is REACHED (CR 608.2c) and BEFORE `exercises`, so
-- no CR 603.5 prompt is raised whose answer cannot matter.
--
-- `controller` is CR 109.5's "you"; `source` is the source PERMANENT rather than
-- the ability on the stack (CR 701.46a's "this permanent", CR 113.7a).
--
-- CR 608.2h's view, not the live one: a gate asked BETWEEN clauses may read an
-- object an earlier clause already moved. The CHOSEN slots rather than CR
-- 608.2b's surviving ones -- a target THIS resolution moved is not one that
-- became illegal before it.
--
-- The resolving object's WHOLE binding map comes in beside them, from the same
-- live read the caller takes the chosen slots off, so a gate can ask after a
-- batch an earlier clause named (CR 115.10a) and after an amount an earlier
-- clause stamped. Under the printed slot names, which is how every other live
-- read is written (slotBindings) and unlike the chosen map, which the caller has
-- projected into CR 700.2d's mode instance.
gateHolds :: PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName Binding.Type.Binding -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game Bool
gateHolds controller source chosen bindings clause = case Clause.condition clause of
  Nothing -> pure True
  Just condition -> do
    gs <- State.get
    pure (Condition.holds (Projection.viewWithLastKnownAnywhere gs) (effectContext gs controller source chosen bindings) gs source condition)

-- CR 608.2d: which branch of an either-or clause pair happens -- Twiddle's "you
-- may tap or untap target artifact, creature, or land". A clause naming no
-- sibling always happens; one that names an earlier or later clause of this mode
-- instance happens only if the controller announced IT.
--
-- Asked ONCE per pair, at whichever branch the fold reaches first, and the
-- answer carried in `picked` under the pair's LOWEST ordinal -- the key both
-- branches compute, so the loser's arrival raises no second prompt. A separate
-- fold component and not `ifTakenHolds`' `ran`: that set records which clauses'
-- instructions RAN, which `condition` and `payGate` can pull apart from which
-- branch was CHOSEN (Clause.ifTaken says why it is keyed that way), and an
-- either-or must exclude its sibling even when the winner then does nothing.
--
-- PER PLAYER, the way CR 118.12's own offer is: OrElse.chooser is a reference
-- and a card may name the table, so the answers are a map and CR 101.4's order
-- runs over them. Worms of the Earth's "any player may sacrifice two lands of
-- their choice or have this enchantment deal 5 damage to that player" is the
-- card; Twiddle's chooser is the resolving controller, one seat and one answer.
--
-- What comes back is the set of players who announced THIS branch, or Nothing
-- for a clause naming no sibling -- the caller hands it to `exercises` and
-- `payGateAdmits`, which offer their own questions to nobody else. NOT a bound
-- slot: both of those read bindings captured before this question was asked, so
-- a slot bound here would be invisible to them.
--
-- The branches are offered in CR 608.2c's printed order and the answer is
-- FILTERED back through them rather than trusted, the posture every choose-don't-
-- target prompt takes.
--
-- Not implemented: CR 608.2d's "can't choose an option that's illegal or
-- impossible" -- a branch whose own `condition` has already failed is offered
-- anyway, as is one whose instruction has nothing legal to act on (Keys to the
-- House offers its lock over a Room with every door already shut), and choosing
-- either leaves the pair doing nothing (#2167).
chosenBranch :: ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Map.Map ClauseIndex (Map.Map PlayerId ClauseIndex) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game (Maybe (Set PlayerId), Map.Map ClauseIndex (Map.Map PlayerId ClauseIndex))
chosenBranch resolving controller idx cIdx legal picked clause = case Clause.orElse clause of
  Nothing -> pure (Nothing, picked)
  Just orElse ->
    let branches = NonEmpty.nub (NonEmpty.sort (cIdx NonEmpty.:| [OrElse.sibling orElse]))
        key = NonEmpty.head branches
        won answers = Just (Map.keysSet (Map.filter (== cIdx) answers))
     in case Map.lookup key picked of
          Just answers -> pure (won answers, picked)
          Nothing -> do
            gs <- State.get
            answers <-
              Monad.foldM
                ( \acc chooser -> do
                    gs1 <- State.get
                    answered <- Game.choose (Prompt.ChooseClause (Decide.deciderFor chooser gs1) chooser resolving idx branches)
                    pure (Map.insert chooser (if elem answered branches then answered else key) acc)
                )
                Map.empty
                (apnapPlayersOf (OrElse.chooser orElse) legal controller gs)
            pure (won answers, Map.insert key answers picked)

-- CR 603.5 / 608.2d: does this clause's instruction list happen at all? A
-- mandatory clause always does; an optional one is its controller's call, made
-- HERE as the effect is applied. The unit is CR 608.2e's clause and not the whole
-- mode, so a "may" printed on one sentence leaves its neighbours alone.
--
-- WHO is asked is the Optionality's own PlayerRef, resolved like every other
-- (playerRefPlayers for the membership) and ordered by CR 101.4 through
-- apnapPlayersOf. Every printed "you may" names the resolving controller -- CR
-- 405.4 for a spell, CR 113.8 for an ability -- and Jungle Wayfinder's "each
-- player may" names the whole table. Each of them is asked through
-- Decide.deciderFor, so a player controlled under CR 723.1 has their controller
-- answer.
--
-- ALL the asks BEFORE any effect runs, which is CR 608.2e: the choices for an
-- action are made in APNAP order and then the action is taken. That is what
-- forbids the ask-and-act-per-seat shape, and rule 101.4b is why each seat is
-- asked against the live board rather than a snapshot.
--
-- The seats that ACCEPTED are bound under Binding.mayPlayers, which is how the
-- clause's own instructions say "they" (PlayerRef.EachInSlot), and the clause
-- happens when anybody accepted -- payGateAdmits' shape one question over. A
-- reference naming nobody therefore accepts nobody and the clause does nothing.
--
-- `announced` narrows the asked seats to the ones that announced THIS branch of
-- a CR 608.2d pair (chosenBranch), Nothing for a clause naming no sibling: the
-- "may" over a branch is offered to the players who took it and to nobody else,
-- which is what stops a player from taking both halves of Worms of the Earth's
-- "sacrifice two lands of their choice or have this enchantment deal 5 damage to
-- that player".
--
-- `bound` is every slot the live bindings hold and `legal` is CR 608.2b's
-- surviving recipients, both under the names this mode instance prints (CR
-- 700.2d); an inert clause is not asked about at all -- see clauseIsInert.
-- Binding.mayPlayers is added to `bound` for that test alone: the slot this very
-- "may" is about to define is not dead, and without that a clause whose only
-- read is its own accepters would be judged inert and decline with no prompt
-- raised.
--
-- Not implemented: CR 608.2d's other half, that the player cannot choose an
-- option that is illegal or impossible -- an inert clause is a slot question,
-- and "you may discard a card" on an empty hand is not (#2167).
exercises :: ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Set SlotName -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game Bool
exercises resolving controller idx cIdx bound legal announced clause = case Clause.optionality clause of
  Optionality.Mandatory -> pure True
  Optionality.Optional asker
    | clauseIsInert (Set.insert Binding.mayPlayers bound) legal clause -> pure False
    | otherwise -> do
        gs <- State.get
        accepted <-
          Monad.foldM
            ( \acc pid -> do
                gs1 <- State.get
                decision <- Game.choose (Prompt.ChooseOptional (Decide.deciderFor pid gs1) pid resolving idx cIdx)
                pure $ case decision of
                  OptionalDecision.Exercises -> Set.insert pid acc
                  OptionalDecision.Declines -> acc
            )
            Set.empty
            (announcedOnly announced (apnapPlayersOf asker legal controller gs))
        State.modify' (bindPlayersSlot resolving Binding.mayPlayers accepted)
        pure (not (Set.null accepted))

-- CR 608.2d: the seats a branch's own questions are offered to. A clause naming
-- no sibling keeps every seat its reference named; one that names a sibling
-- keeps only the seats that announced it, in the APNAP order the caller already
-- imposed.
announcedOnly :: Maybe (Set PlayerId) -> [PlayerId] -> [PlayerId]
announcedOnly = maybe id (\winners -> filter (`Set.member` winners))

-- CR 608.2b / 603.5: can this clause's answer not matter? Only when every one of
-- its effects reads a slot and every slot it reads is illegal or unfilled, since
-- each opcode's slot reads then name nothing and the clause does nothing either
-- way. The engine never makes a player's choice, so this is the one elision the
-- prompt admits and it is deliberately conservative: an effect reading NO slot,
-- or reading one surviving recipient among several, keeps the prompt.
--
-- A CLASSIFICATION and never an identity check: what an effect reads comes from
-- slotsOf, and slotsAreExhaustive is what says slotsOf is the WHOLE of it -- an
-- opcode that reads more than its slots (ArmDelayedTrigger, CR 725.2's
-- ControllerOfSource) answers False there and so is never called inert. That
-- conjunct is a REGRESSION FENCE rather than a proven behaviour: no optional
-- clause in data/cards/ holds such an opcode, so dropping it leaves the suite
-- green.
--
-- "Dead" is per SLOT and takes both maps, because a slot's binding need not be a
-- target at all: a TARGET slot is dead once CR 608.2b has emptied it, and any
-- other slot -- a group an earlier clause revealed, X, a reserved binding -- is
-- dead only when nothing has bound it. Reading `legal` alone would call a
-- revealed-cards slot dead and silently decline Midnight Tilling's return.
--
-- An EMPTY clause is not inert: it has no effect to read a slot, so `all` would
-- hold vacuously. Nothing in data/cards/ prints one, and reaching this ahead of
-- CR 608.2b's fizzle needs a modal payload mixing a live mode with a dead one
-- (Deadly Complication).
clauseIsInert :: Set SlotName -> Map.Map SlotName (Set Recipient) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
clauseIsInert bound legal clause =
  let effects = Foldable.toList (Clause.effects clause)
      dead name = case Map.lookup name legal of
        Just recipients -> Set.null recipients
        Nothing -> not (Set.member name bound)
      inert effect =
        let names = Map.keysSet (slotsOf effect)
         in slotsAreExhaustive effect && not (Set.null names) && all dead names
   in not (null effects) && all inert effects

-- CR 118.12: does this clause's instruction list happen, given the cost paid on
-- resolution it may state? A clause stating none always does; one that states one
-- offers it to the players its `payer` reference names, and the instructions are
-- whichever branch PayGate.branch says.
--
-- The branch is keyed on the ANSWER and never on the board afterwards, which is
-- CR 118.12 in as many words: it checks whether the player chose to pay
-- "regardless of what events actually occurred".
--
-- PER PLAYER, because CR 118.12a's rewriting is: "[Do something] unless [a
-- player does something else]" means "[A player may do something else]. If
-- [that player doesn't], [do something]", so Rishadan Cutpurse's "each opponent
-- sacrifices a permanent of their choice unless they pay {1}" is one offer per
-- opponent gating that opponent's own edict. The seats the branch SELECTS are
-- bound under Binding.gatePlayers, which is how the clause's own instructions
-- say "they", and the clause happens when the branch selected anybody.
--
-- A gate whose reference names NOBODY therefore selects nobody and its clause is
-- skipped, where a single-payer gate used to take the IfNotPaid branch and run
-- its instructions against an unfilled slot. Unobservable across the pool as it
-- stands: only an IfNotPaid clause diverges (an IfPaid one was skipped either
-- way), only the slot-reading references can name nobody, and every IfNotPaid
-- clause in the pool whose payer is one of those aims its own instructions at
-- that same slot -- Mana Leak's Counter, Amulet of Safekeeping's. The rest read
-- `you`, which is stamped for every carrier (Binding.you).
--
-- FOUR ways one player's answer comes out, of which exactly one is "paid": the
-- reference names them but they CANNOT pay (CR 118.3), asked on neither limb;
-- they decline, which only an OPTIONAL cost reaches; they chose to pay -- the
-- one place the answer is not the raw choice, since Pawl.Engine.Cost.pay
-- restores the payments an incomplete attempt made and an Unpaid result buys
-- nothing, though it is not a no-op on the BOARD: Cost.reverseIllegal asks
-- before reversing a mana ability, so a payer who declines keeps CR 605.3a's
-- window -- the mana floating and the sources tapped; or the reference never
-- named them at all, which is not an answer and leaves them out of both
-- branches.
--
-- The cost is paid AGAINST `source` rather than the resolving stack object (CR
-- 113.7a); the two are the same object for a spell.
--
-- ONE offer per payment (CR 118.12): a second clause hanging off the same cost
-- names the first (PayGate.offeredAt) and reuses the recorded answers, `answers`
-- being keyed on the offering clause's ordinal. A clause naming an offer never
-- made falls through and makes it, the named clause having failed its own CR
-- 701.46a "if" or CR 603.5 "may".
payGateAdmits :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> Map.Map ClauseIndex (Map.Map PlayerId Bool) -> Clause.Clause Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game (Bool, Map.Map ClauseIndex (Map.Map PlayerId Bool))
payGateAdmits resolving source controller idx cIdx legal announced answers clause = case Clause.payGate clause of
  Nothing -> pure (True, answers)
  Just gate -> do
    let offerAt = Maybe.fromMaybe cIdx (PayGate.offeredAt gate)
    (asked, answers') <- case Map.lookup offerAt answers of
      Just recorded -> pure (recorded, answers)
      Nothing -> do
        recorded <- payGatePaid resolving source controller idx cIdx legal announced gate
        pure (recorded, Map.insert offerAt recorded answers)
    let selected = Map.keysSet (Map.filter (branchTaken (PayGate.branch gate)) asked)
    State.modify' (bindPlayersSlot resolving Binding.gatePlayers selected)
    pure (not (Set.null selected), answers')

-- Which branch of CR 118.12 a payment outcome selects, off the classification a
-- card states -- never off what the payment DID.
branchTaken :: PayBranch.PayBranch -> Bool -> Bool
branchTaken branch wasPaid = case branch of
  PayBranch.IfPaid -> wasPaid
  PayBranch.IfNotPaid -> not wasPaid

-- The offer itself: who was offered this gate's cost, and which of them paid?
-- CR 118.12's MANDATORY limb is not offered, and that is the rule rather than an
-- elision -- it asks whether the player "started to pay", so a mandatory cost the
-- payer can afford leaves nothing to choose, and CR 118.3 is asked first so an
-- unpayable one takes the "can't" branch with no prompt either.
--
-- Narrowed by `announcedOnly` to the seats that announced this branch of a CR
-- 608.2d pair, where the clause is one: the offer belongs to the players who
-- took it, so Worms of the Earth's sacrifice is offered to nobody who announced
-- its damage instead.
--
-- CR 101.4's APNAP order over the players the reference names, which is what
-- `apnapPlayersOf` imposes: rule 101.4b lets a later payer answer knowing what an
-- earlier one did. The board is re-read for each of them (payGatePaidBy's own
-- State.get) rather than measured once, so a cost that changes the board -- CR
-- 118.12's own "sacrifice this enchantment" -- is affordable to the next payer
-- against the board it left. Each payer spends only their own resources, so the
-- sequencing is not observable as an ordering of the ACTIONS.
payGatePaid :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Maybe (Set PlayerId) -> PayGate.PayGate -> Game (Map.Map PlayerId Bool)
payGatePaid resolving source controller idx cIdx legal announced gate = do
  gs <- State.get
  Monad.foldM
    ( \acc payer -> do
        paid <- payGatePaidBy resolving source idx cIdx legal payer gate
        pure (Map.insert payer paid acc)
    )
    Map.empty
    (announcedOnly announced (apnapPlayersOf (PayGate.payer gate) legal controller gs))

-- One player's answer to one gate. The cost is the PRINTED one with CR 107.3's X
-- resolved (`announcedXOn`) and then multiplied by CR 702.24a's "for each"
-- (PayGate.perCounter), and that pair of rewrites is what every reader below
-- sees -- CR 118.3's affordability test, the prompt the payer is shown, and the
-- payment itself -- so none of them can disagree about what is owed.
--
-- The multiplier is read HERE rather than once for the whole gate, which is the
-- posture payGatePaid's own comment states: rule 101.4b lets an earlier payer's
-- answer move the board, and CR 118.12's cost is measured against the board each
-- payer faces. It counts the counters on the ability's SOURCE through
-- `effectViewOf`, so CR 113.7a's last known record answers for a source that has
-- already left -- the same read Quantity.ObjectCounters makes. Whether such an
-- ability should be offering anything at all is its own text's business: rule
-- 702.24a's intervening "if" is what stops it (CR 603.4), proved at
-- Pawl.KeywordTriggerSpec's "a Unicorn murdered in response".
payGatePaidBy :: ObjectId -> ObjectId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> PlayerId -> PayGate.PayGate -> Game Bool
payGatePaidBy resolving source idx cIdx legal payer gate = do
  gs <- State.get
  let multiplier = case PayGate.perCounter gate of
        Nothing -> 1
        Just kind -> maybe 0 (Map.findWithDefault 0 kind . Filter.counters) (effectViewOf source legal gs source)
      cost = Cost.repeated multiplier (Cost.substituteX (announcedXOn resolving gs) (PayGate.cost gate))
  if not (Cost.canPay payer source cost gs)
    then pure False
    else do
      decision <- case PayGate.obligation gate of
        PayObligation.Mandatory -> pure PaymentDecision.Pays
        PayObligation.Optional -> Game.choose (Prompt.ChooseToPay (Decide.deciderFor payer gs) payer resolving idx cIdx cost)
      case decision of
        PaymentDecision.Declines -> pure False
        PaymentDecision.Pays -> do
          -- CR 118.13b: a symbol payable in multiple ways is announced by the
          -- PAYER "immediately before they pay that cost" -- after CR 118.12's
          -- "may" above, since what is announced is how to pay a cost already
          -- chosen, and before the mana window Cost.pay opens.
          --
          -- CR 601.2f's totalling is `pure`, which is the identity in the list
          -- applicative `Cost.announce` measures through: pawl gathers cost
          -- adjustments for a SPELL (Cast.castSpell) and for an ACTIVATION
          -- (Activate.activateAbility) and nowhere else, and `Cost.pay` below
          -- applies none of its own, so the announced cost IS the cost that will
          -- be paid. A card that reduced a CR 118.12 cost would be the one to
          -- refute that, and `data/cards/` prints none. That also keeps the offer
          -- exactly as permissive as `Cost.canPay` above, which enumerates the
          -- same CR 601.2b nonhybrid equivalents through Mana.resolutions --
          -- so no route Mana.announce offers is one the gate refused, and its
          -- no-payable-route fallback stays unreachable from here.
          -- Discarded, Activate's reason: rule 702.150a asks about a spell's own
          -- cost, not about a cost paid during a resolution (CR 118.13b).
          (announced, _) <- Cost.announce PaymentSubject.ForNeither ManaSpending.AsProduced payer source pure cost
          -- DuringResolution: rule 118.12's cost is paid as the spell or ability
          -- resolves, which is CR 609.1's effect, so a blight paid here is CR
          -- 614.16's subject where Soul Immolation's additional cost is not.
          outcome <- Cost.pay performManaAbility PaymentMoment.DuringResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced payer source announced
          -- Not implemented: the slots this payment bound are dropped, so a
          -- CR 118.12 cost that sacrifices a permanent cannot be read by a
          -- later clause of the same resolution (#1872).
          pure (case outcome of Payment.Paid _ -> True; Payment.Unpaid -> False)

-- CR 118.4 / CR 107.3a: the value of X in a cost paid during resolution. NOT a
-- choice the payer makes -- CR 107.3a fixes it at the value the object's own
-- controller announced at CR 601.2b, and CR 107.3i gives every instance of X on
-- that object that one value -- so Clash of Wills' "unless its controller pays
-- {X}" charges the X its caster paid for, and nobody is prompted here.
--
-- Read off the object CR 601.2b announced ON: the SPELL (Cast.castSpell stamps
-- the new incarnation) or the ABILITY (Activate.activateAbility stamps the
-- ability object), which is `resolving` in both cases and never `source` --
-- Quantity.evaluateFor's InSlot arm says the same about the same binding.
--
-- Zero when nothing was announced. A face whose gate prints {X} always declares
-- an {X} of its own (Pawl.CardSpec's "every printing that reads X declares X"),
-- so this is a totality guard for a CARD-authored gate. A gate MINTED by a
-- keyword resolves on a triggered ability that announced nothing, and reads 0
-- here.
--
-- Not implemented: CR 702.21b's ward {X}, whose value is determined as the
-- ability resolves rather than announced, needs a cost whose amount is a
-- Quantity and has no spelling at all (#1526).
announcedXOn :: ObjectId -> GameState -> Natural
announcedXOn oid gs =
  Maybe.fromMaybe
    0
    (Game.lookupObject oid gs >>= Binding.amountOf Binding.variableX . Object.bindings)

-- The players a PlayerRef names, in CR 101.4's APNAP order -- playerRefPlayers
-- answers in PlayerId order and says so, leaving the ordering rule to its
-- caller. Two callers ask, and for the same reason: the seats a resolution cost
-- is offered to (CR 118.12a) and the seats CR 111.2 has creating tokens. Mana
-- Leak's reference names one and Rishadan Cutpurse's names every opponent; the
-- order is only observable for the second.
apnapPlayersOf :: PlayerRef -> Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> [PlayerId]
apnapPlayersOf ref legal controller gs =
  let named = playerRefPlayers legal controller gs ref
   in filter (\pid -> List.elem pid named) (Game.apnapOrder gs)

-- The no-subgame mode executor: every direct caller, and any path that cannot
-- reach a PlaySubgame.
resolveModes :: ObjectId -> ObjectId -> [(ModeInstance, Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))] -> Game ()
resolveModes = resolveModesWith noSubgame

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (CR 113.7a), not the ability object, and only the CHOSEN modes are read (CR
-- 700.2c). The ability then ceases (CR 608.2n) rather than being buried.
--
-- `runSubgame` rides through to the effects for the reason resolveModesWith
-- gives (CR 729.1a).
resolveAbilityWith :: Game Result -> ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
resolveAbilityWith runSubgame abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.modesOf (Object.bindings obj)
       in resolveModesWith runSubgame abilId srcId (Modal.chosenModes chosen (ActivatedAbility.modal ability))

-- The no-subgame activated-ability resolver.
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
resolveAbility = resolveAbilityWith noSubgame

-- CR 701.27a and CR 701.28a: turn each named permanent over. ONE function for
-- both opcodes, which is CR 701.28a said as code -- "this follows rules
-- 701.27a-f, 712.9-10, and 712.18", so a convert cannot pick up a gate a
-- transform lacks, nor miss one it has.
turnPermanentsOver ::
  Map.Map SlotName (Set Recipient) ->
  ObjectId ->
  PlayerId ->
  ObjectId ->
  ObjectRef.ObjectRef ->
  Game ()
turnPermanentsOver legal resolving controller source ref = do
  -- WHICH permanents turn over, gathered BEFORE the turn: the one ref here that
  -- is a CR 608.2d question rather than a read has to be answered in the Game
  -- monad, and every other is objectRefObjects' pure sweep.
  victims <- case ref of
    -- CR 608.2d: "any number of Human Werewolves you control" is a choice made
    -- while the effect is applied, so it is asked HERE. The candidates are
    -- EachMatching's sweep of the same Filter, read live off the pre-turn board
    -- (CR 608.2c) -- which for Tovolar is a board CR 702.145c has already turned
    -- him over on, so his own back face is no longer a Human Werewolf to offer.
    --
    -- ONE ask, of the resolving controller (CR 608.2c). Skipped at no candidate,
    -- where the empty set is the only answer (CR 101.3, CR 609.3), and asked at
    -- ONE, unlike the counted choices: "any number" leaves two distinguishable
    -- answers there.
    --
    -- FILTERED, not trusted (#222): an answer naming a permanent that was never
    -- offered would otherwise turn it over. Filtering rather than taking the
    -- answer also keeps CR 608.2f's APNAP order, which the candidate list
    -- already carries and a Set does not.
    ObjectRef.AnyNumberMatching filter_ -> do
      gs <- State.get
      let candidates = battlefieldMatching legal resolving controller source gs filter_
      if null candidates
        then pure []
        else do
          answer <- Game.choose (Prompt.ChooseAnyNumberOfPermanents (Decide.deciderFor controller gs) controller source candidates)
          pure (filter (`Set.member` answer) candidates)
    _ -> do
      gs <- State.get
      pure (objectRefObjects legal resolving controller source gs ref)
  -- CR 613.7m: which of the swept victims will ACTUALLY take a stamp, ordered by
  -- their controllers. Asked BEFORE the write and off the pre-turn board, the
  -- gather's own reason (CR 608.2f), and `turnsOver` below is the whole
  -- membership question, so a permanent this instruction cannot turn over is no
  -- part of anybody's choice.
  gs0 <- State.get
  ordered <- Restamp.order (filter (turnsOver (Projection.projectAll gs0) resolving gs0) victims)
  State.modify' $ \gs ->
    -- CR 701.27a: turn each victim over -- one assignment to Object.face, which
    -- is all a turn IS here, every characteristic read already resolving
    -- through Game.faceOf. The victims were enumerated ONCE above (CR 608.2f),
    -- ahead of this write so no member of the batch is judged against a board a
    -- sibling has already turned over on. WHICH face is
    -- Pawl.Engine.Card.turnedOver's answer, off the card's layout, which
    -- withholds a turn from a permanent that is not double-faced (CR 701.27c)
    -- or whose other face is an instant or sorcery (CR 701.27d).
    --
    -- CR 701.27b: turning over is its own game action, so it records its own
    -- event -- Event.recordTransformed, over the victims that ACTUALLY turned
    -- rather than the ones this instruction named, since the four ways a turn
    -- is withheld leave nothing for CR 603.2 to fire on.
    --
    -- AFTER the fold, and the order is load-bearing: Pawl.Engine.Card gives a
    -- transforming permanent only the SHOWN face's abilities, so a trigger
    -- printed on the face just turned to is among recordEvent's candidates only
    -- because the write has already happened. Pawl.TransformSpec's CR 701.27e
    -- group is what proves it.
    --
    -- ONE fresh timestamp for the whole instruction (CR 701.27f), minted even
    -- when nothing turns over, and ONE whole-board projection, CR 702.145b's
    -- restriction being read off the layer fold. CR 613.7g's per-permanent stamp
    -- is a second thing, minted inside Game.turnFaceOver.
    --
    -- LEFT fold, where this was a right one before the stamp: the fold hands out
    -- CR 613.7g's timestamps, so it must run `ordered` forwards for the earlier
    -- stamp to go to the permanent CR 613.7m put first.
    let (now, g1) = Game.freshTimestamp gs
        g2 = List.foldl' (flip (Game.turnFaceOver now)) g1 ordered
     in Event.recordTransformed
          (Game.facesTurned (GameState.objects g1) (GameState.objects g2) ordered)
          g2

-- CR 701.27a over ONE object, asked rather than performed: whether this
-- instruction turns it over. The act itself is Game.turnFaceOver, shared with
-- Pawl.Engine.Daytime's CR 702.145c/f sweep, and Game.turnsTo is the act's own
-- half of this question. What this adds is the two gates that belong to an
-- INSTRUCTION rather than to the act, and a static ability's turn has neither: CR
-- 701.27f's already-turned check, and CR 702.145b/e's "can't transform except due
-- to its daybound/nightbound ability" (Pawl.Engine.Daytime.restrictsTransform).
--
-- Asked of the PRE-TURN board (CR 608.2f's simultaneous processing), so no member
-- of the batch is judged against a board a sibling has already turned over on,
-- and `pcs` is that board's projection.
turnsOver :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> ObjectId -> Bool
turnsOver pcs resolving frozen oid =
  not (alreadyTurnedFor resolving oid frozen)
    && not (Daytime.restrictsTransform pcs oid)
    && Maybe.isJust (Game.turnsTo oid frozen)

-- CR 701.27f. True when this resolution must be ignored: the resolving object is
-- an ability whose SOURCE is the very permanent being turned over (CR 113.7a),
-- and that permanent's last turn is later than the moment the rule measures that
-- ability from.
--
-- WHICH moment is the rule's two sentences, and they differ observably. An
-- ordinary ability is measured from when it was put onto the stack, which is its
-- own CR 613.7d timestamp. A DELAYED triggered ability is measured from when it
-- was CREATED, which is earlier and can sit on the far side of a turn-over its
-- placement stamp is on the near side of -- Pawl.TransformSpec's Aang pair is
-- the board that tells the two apart. Every stamp comes from
-- GameState.nextTimestamp, so one `>` decides it and equality cannot arise.
alreadyTurnedFor :: ObjectId -> ObjectId -> GameState -> Bool
alreadyTurnedFor resolving victim gs = case Game.lookupObject resolving gs of
  Nothing -> False
  Just ability -> case startedAt ability of
    Nothing -> False
    Just started -> maybe False (> started) (Game.lookupObject victim gs >>= Object.turnedOverAt)
  where
    -- Both of the rule's narrowings at once, and a CLASSIFICATION of the
    -- resolving object throughout, never which card it is: Nothing unless the
    -- object is an ability OF the victim, and otherwise the moment to measure
    -- from. CR 725.2's sourceless inherent trigger has no permanent to be an
    -- ability of, so it falls out with the spells.
    startedAt ability = case Object.source ability of
      Source.OfAbility activated
        | ActivatedAbilitySource.source activated == victim -> Just (Object.timestamp ability)
        | otherwise -> Nothing
      Source.OfTrigger triggered
        | TriggeredAbilitySource.source triggered == victim ->
            -- CR 603.7a's creation moment when there is one, and the placement
            -- stamp otherwise: an ability the source itself has carries none.
            Just (Maybe.fromMaybe (Object.timestamp ability) (TriggeredAbilitySource.createdAt triggered))
        | otherwise -> Nothing
      Source.OfCard _ -> Nothing
      Source.OfMeld _ -> Nothing
      Source.OfToken _ -> Nothing
      Source.OfEmblem _ -> Nothing
      Source.OfSpellCopy _ -> Nothing
      Source.OfInherentTrigger _ -> Nothing

-- CR 608.2b: the ONE recipient still legal in `slot`, for a reader that can take
-- only one -- nothing when the slot named none, its target became illegal, or it
-- names SEVERAL. Pawl.CardSpec's plural-slot lint keeps a card from aiming one of
-- those at such a reader.
legalOne :: SlotName -> Map.Map SlotName (Set Recipient) -> Maybe Recipient
legalOne slot legal = Binding.onlyOne (Map.findWithDefault Set.empty slot legal)

-- The same read for a reader that takes them ALL, CR 608.2b's illegal ones
-- already dropped.
legalMany :: SlotName -> Map.Map SlotName (Set Recipient) -> [Recipient]
legalMany slot legal = Set.toList (Map.findWithDefault Set.empty slot legal)

-- The players a PlayerRef names DURING a resolution, read from the slots this
-- resolution filled rather than the source's bindings. A slot naming SEVERAL
-- names nobody (`legalOne`).
--
-- CR 102.1: a departed player keeps their row in GameState.players, so `everyone`
-- is Game.stillPlaying rather than the map's keys; whether a departed player can
-- be named from elsewhere is CR 800.4d/800.4i's question (#181). In PlayerId
-- order, a PlayerRef naming an unordered SET, so a caller with an ordering rule
-- imposes it.
playerRefPlayers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> PlayerRef -> [PlayerId]
playerRefPlayers legal controller gs ref = case ref of
  PlayerRef.InSlot slot -> case legalOne slot legal of
    Just (Recipient.ToPlayer pid) -> [pid]
    _ -> [] -- an unfilled, illegal, or non-player slot: no-op
    -- Every player the slot names, InSlot's read without Binding.onlyOne's
    -- collapse -- Binding.mayPlayers, the seats a CR 603.5 "may" selected.
    -- Non-player recipients are dropped, as the arm above drops them.
    --
    -- Binding.gatePlayers is the same shape one question over, and Bellowing
    -- Mauler's "each player loses 4 life unless they sacrifice a nontoken
    -- creature of their choice" reads THAT slot plurally through the arm below.
  PlayerRef.EachInSlot slot -> Maybe.mapMaybe Recipient.playerOf (legalMany slot legal)
  PlayerRef.Relative PlayerRelation.You -> [controller]
  PlayerRef.Relative PlayerRelation.Opponent -> filter (PlayerRelation.holds (Game.teams gs) PlayerRelation.Opponent controller) everyone
  -- CR 102.1's whole table, off the roster rather than by consing the controller
  -- onto the Opponent set, so a departed seat stays out.
  PlayerRef.Relative PlayerRelation.AnyPlayer -> everyone
  PlayerRef.EachPlayer -> everyone
  -- EachPlayer minus the seat the slot names. A slot that is unfilled, illegal,
  -- names several, or names an object excludes NOBODY.
  PlayerRef.EachPlayerExcept slot ->
    let excluded = legalOne slot legal >>= Recipient.playerOf
     in filter (\pid -> Just pid /= excluded) everyone
  -- The baked seat, unreachable from card data. Not filtered against the roster:
  -- it names one specific player who arrived from elsewhere.
  PlayerRef.Specific pid -> [pid]
  -- NOBODY, and not a hole: the reference names whichever player a fold has
  -- reached, and this function is handed no fold. The two positions that DO
  -- answer it never route through here -- Pawl.Engine.Quantity's playersOf reads
  -- it off the view a Count's fold supplies, and the Effect.Search arm's
  -- ownersFor substitutes the searcher for a search whose owner is its own
  -- searcher -- so what reaches this arm is a reference in a position with no
  -- candidate at all, and the opcode is a no-op.
  PlayerRef.Candidate -> []
  -- CR 608.2h: the controller of the object the slot names, through last known
  -- information -- the clause naming the player generally MOVED it first, and CR
  -- 108.4 leaves a card in a hand with no controller at all.
  PlayerRef.ControllerOfBound slot -> case legalOne slot legal of
    Just recipient -> case Recipient.objectOf recipient of
      Just oid -> Maybe.maybeToList (Projection.controllerWithLastKnown oid gs)
      Nothing -> []
    Nothing -> []
  -- CR 508.6: the players controlling a creature that is attacking the player the
  -- slot names, narrowed by the relation the card printed -- Curse of Vitality's
  -- "each opponent attacking that player".
  --
  -- The LIVE combat record, read as this effect applies (CR 608.2c): the sentence
  -- is present tense, so a creature removed from combat (CR 506.4) since the
  -- declaration has taken its controller out of the set. Not the event log, which
  -- is Pawl.Engine.Turn.attackedThisStep's historical reading of the same rule.
  --
  -- AttackTarget.OfPlayer alone, CR 508.1b listing player, planeswalker and
  -- battle separately: a creature attacking a planeswalker that player controls
  -- is not attacking that player.
  --
  -- Filtered out of `everyone` rather than collected from the record, so the
  -- roster order and the CR 102.1 exclusion of a departed seat are the ones every
  -- other arm gives.
  PlayerRef.Attacking (AttackingPlayers.MkAttackingPlayers relation slot) ->
    case legalOne slot legal >>= Recipient.playerOf of
      Nothing -> []
      Just attacked ->
        let sentAt = Map.keys (Map.filter (== AttackTarget.OfPlayer attacked) (Combat.attackers (GameState.combat gs)))
            attackers = Maybe.mapMaybe (\oid -> Projection.controllerOf oid gs) sentAt
         in filter (\pid -> PlayerRelation.holds (Game.teams gs) relation controller pid && pid `elem` attackers) everyone
  where
    everyone = Game.stillPlaying gs

-- CR 109.2's battlefield, narrowed by an effect-borne Filter and sorted into CR
-- 608.2f's APNAP order. ObjectRef.EachMatching's whole answer, and the
-- CANDIDATES ObjectRef.AnyNumberMatching offers -- shared so a card cannot find
-- the sweep and the offer disagreeing about what matches.
--
-- CR 303.4b's host is supplied here and nowhere else in this module: this sweep
-- is the one effect-borne Filter position naming what the SOURCE enchants. Read
-- live, so an Aura moved between the trigger and its resolution acts on the host
-- it has now.
--
-- Through effectContext, so the resolution's own slot bindings ride along and a
-- sweep can exclude what another slot already named: Showstopping Surprise's
-- "each OTHER creature" is `Not (IsBound "target")`. Filter.IsBound answers False
-- for every candidate against an empty slot map, so a bare contextFor here would
-- leave such a card silently sweeping in its own target. It is also what answers
-- a CONTROLLER-relative conjunct -- Tovolar's "Human Werewolves you control" is
-- `ControlledBy You`, read against CR 109.5's perspective.
battlefieldMatching :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
battlefieldMatching legal resolving controller source gs filter_ =
  let context = (effectContext gs controller source legal (slotBindings resolving gs)) {Filter.sourceAttachedTo = Projection.hostOf source gs}
      viewOf = Projection.viewsOf gs
      matching =
        filter
          (\oid -> Filter.matches context (viewOf oid) filter_)
          (Set.toList (GameState.battlefield gs))
      order = Game.apnapOrder gs
      last_ = length order
      seat oid = case Projection.controllerOf oid gs of
        Nothing -> last_
        Just pid -> Maybe.fromMaybe last_ (List.elemIndex pid order)
   in List.sortOn (\oid -> (seat oid, oid)) matching

-- The objects an ObjectRef names DURING a resolution, for every arm whose answer
-- is a READ. The arms that are a CR 608.2d QUESTION answer [] here and are
-- carried out by the opcode arms that reach the Game monad. InSlot takes every
-- recipient CR 608.2b left legal
-- (CR 601.2c); a slot bound to a GROUP is answered before that question, a group
-- being a definition rather than a target (CR 115.10a).
--
-- EachMatching folds the battlefield (CR 109.2) against the projection, so a
-- permanent that is a creature only by a layer-4 effect is in the set -- through
-- battlefieldMatching above, which is that fold and is shared with the
-- AnyNumberMatching offer. The filter context is this effect's own -- CR 109.5's
-- "you" is the ability's controller -- because the filter IS the ability's card
-- text. EachCardInGraveyard is the same
-- fold over CR 400.1's per-player graveyards (CR 109.2a).
--
-- WHEN: at the moment the caller runs (CR 608.2c), and the list is then FIXED --
-- one half of CR 608.2f's simultaneous processing. The other half is the
-- caller's: it hands the whole list to its funnel as one batch rather than
-- calling it per element (Event.destroy's haddock).
--
-- The two callers that store a CONTINUOUS effect -- ModifyTarget and GainControl
-- -- owe CR 611.2c: the set is determined when the effect begins, so those arms
-- freeze this answer as Affected.TheseObjects. Nothing enforces it; a third
-- storing caller must not reach for Affected.Matching, a STATIC ability's set.
--
-- ORDER, for the arms folding over CR 400.1's per-player zones: APNAP (CR
-- 608.2f), then ascending ObjectId. The arms over a SHARED zone keep that zone's
-- own order (CR 101.4). `forEachOrder` is where CR 608.2f's secondary sentence
-- and CR 701.44d's per-seat choice open, and both ask rather than reading this
-- order.
objectRefObjects :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [ObjectId]
objectRefObjects legal resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- The slot names every object bound there as a group, all at once. Ahead of
    -- the target read and not subject to `legal`: a group binding is a definition,
    -- never a target (CR 115.10a). slotGroup says why being ahead is safe.
    Just group -> Foldable.toList group
    Nothing -> Maybe.mapMaybe Recipient.objectOf (legalMany slot legal)
  ObjectRef.EachMatching filter_ -> battlefieldMatching legal resolving controller source gs filter_
  -- A CR 608.2d question, so this pure sweep answers nothing for it: the
  -- candidates are battlefieldMatching's, but WHICH of them the instruction names
  -- is the chooser's, and two gathers reach the Game monad to ask --
  -- turnPermanentsOver, the body Effect.Transform and Effect.Convert share, and
  -- the Effect.MoveToZone gather. Under any other opcode this empty answer is an
  -- inert card-data error, which Pawl.CardSpec's inertChoosers rejects at load
  -- time -- ChosenCardInGraveyard's note below is the shape.
  ObjectRef.AnyNumberMatching _ -> []
  -- The arm above's answer, for its reason: a CR 608.2d question, so this pure
  -- sweep answers nothing for it. The Effect.MoveToZone gather is the one arm
  -- that reaches the Game monad to ask it; under any other opcode this empty
  -- answer is an inert card-data error, which Pawl.CardSpec's inertChoosers
  -- rejects at load time.
  ObjectRef.ChosenPermanent _ -> []
  -- The arm above's answer, for its reason: a CR 608.2d question, so this pure
  -- sweep answers nothing for it -- the source half included, which no reader may
  -- take without the counterpart the one instruction names alongside it.
  ObjectRef.SourceAndChosenPermanent _ -> []
  -- EachMatching's sweep with CR 109.2's battlefield default switched off by the
  -- card's own words (CR 109.2a), over CR 400.1's per-player zone. Whose
  -- graveyards is zoneScopePlayers below -- either the perspective's own
  -- reading of CR 109.5 or the players another slot of this announcement targets
  -- -- and what matches within each is graveyardCardsOf.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard scope filter_) ->
    concatMap (\pid -> graveyardCardsOf (effectContext gs controller source legal (slotBindings resolving gs)) gs pid filter_) (zoneScopePlayers legal controller gs scope)
  -- CR 400.1's per-player zone again, but only the RESOLVING CONTROLLER's, so no
  -- scope to fold over and no APNAP order to impose. In the zone's own order,
  -- which no rule reads: CR 402.3 leaves a hand's arrangement to its owner.
  ObjectRef.EachCardInYourHand -> Game.zoneMembers Zone.Hand controller gs
  -- The arm above's zone under EachCardInGraveyard's scope and filter: CR
  -- 109.2a's reading again, over the hands zoneScopePlayers names rather
  -- than the resolving controller's alone. In APNAP order (CR 608.2f) across
  -- seats, and within a seat in the hand's own order, which no rule reads (CR
  -- 402.3) -- the arm above's answer.
  --
  -- effectContext and NOT Filter.contextFor, so the resolution's own slot
  -- bindings ride along and a filter reading a slot (Filter.IsBound) is not
  -- vacuously False here. Amnesia's "nonland" reads no slot, so no test on this
  -- module can tell the two apart at this site (#2075); the sibling sweeps are
  -- written the same way for the reason spelled out at EachMatching above.
  --
  -- No sourceAttachedTo override, unlike EachMatching: no card in a hand names
  -- what its Aura's host is.
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand scope mFilter) ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        held pid = case mFilter of
          Nothing -> Game.zoneMembers Zone.Hand pid gs
          Just filter_ -> handCardsOf context gs pid filter_
     in concatMap held (zoneScopePlayers legal controller gs scope)
  -- CR 400.1's other hidden per-player zone, and only the RESOLVING
  -- CONTROLLER's, so no scope to fold over and no APNAP order to impose --
  -- EachCardInYourHand's answer above. CR 400.12 is what makes "from your
  -- library" name every card in it, and CR 109.2a is what a stated Filter reads
  -- -- Caldera Breaker's "all Mountain cards". In the library's own
  -- order, top card first, which CR 401.2 keeps players from looking at or
  -- changing; the narrowing does not reorder, and the cards that did not match
  -- stay where they were.
  --
  -- NOT a search (CR 701.23a) and so no shuffle (CR 701.24): neither producer's
  -- text says "search" or "find", so CR 701.23b's "isn't required to find" and
  -- CR 701.23f's search triggers have nothing to reach. A stated characteristic
  -- does not make it one -- rule 701.23b governs a player who is SEARCHING, and
  -- nothing here asks anyone to.
  ObjectRef.EachCardInYourLibrary mFilter ->
    let inLibrary = Game.zoneMembers Zone.Library controller gs
     in case mFilter of
          Nothing -> inLibrary
          Just filter_ ->
            let context = effectContext gs controller source legal (slotBindings resolving gs)
                viewOf = Projection.viewsOf gs
             in filter (\oid -> Filter.matches context (viewOf oid) filter_) inLibrary
  -- CR 607.2a's linked set: the cards GameState.exiledWith files against this
  -- effect's SOURCE. The relation, not a zone sweep, is the membership test, so a
  -- card exiled by a second copy of the same printing is not named; a stated
  -- Filter then narrows it. `source` and not `resolving`, since rule 607.2a links
  -- two abilities of one OBJECT and for a dies trigger the two ids differ. Read
  -- off GameState.exile directly because CR 400.1 makes exile one SHARED zone --
  -- no player to ask, and no APNAP sort, so ascending id and thus arrival order.
  --
  -- Rule 607.2a's wording ALONE: an ability referring back to what its own
  -- earlier instruction exiled is CR 400.7j in CR 608.2c's written order, and
  -- names the slot that instruction bound instead -- Hanweir Battlements' "exile
  -- them, then meld them into Hanweir, the Writhing Township" is that printing,
  -- and its Meld reads an InSlot.
  ObjectRef.EachCardExiledWithSource mFilter ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        viewOf = Projection.viewsOf gs
        stated oid = case mFilter of
          Nothing -> True
          Just filter_ -> Filter.matches context (viewOf oid) filter_
     in filter
          (\oid -> Map.lookup oid (GameState.exiledWith gs) == Just source && stated oid)
          (Set.toList (GameState.exile gs))
  -- CR 109.2b's reading of a description carrying the word "spell" -- the stack,
  -- not the battlefield. Game.isSpell keeps the abilities sharing the zone out
  -- (CR 112.1), a classification of the object's kind and not of its identity. In
  -- the STACK's own order, top first (CR 405.2), not APNAP: one shared zone has an
  -- order the rules already read. Read LIVE (CR 608.2c).
  ObjectRef.EachSpell filter_ ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        viewOf = Projection.viewsOf gs
     in filter
          (\oid -> Game.isSpell oid gs && Filter.matches context (viewOf oid) filter_)
          (GameState.stack gs)
  -- CR 405.1's whole zone: the arm above without Game.isSpell, since a sentence
  -- naming spells AND abilities names everything the stack holds. Same order,
  -- top first (CR 405.2), and read LIVE (CR 608.2c).
  ObjectRef.EachOnStack filter_ ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        viewOf = Projection.viewsOf gs
     in filter
          (\oid -> Filter.matches context (viewOf oid) filter_)
          (GameState.stack gs)
  -- Names players and so no objects at all.
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  ObjectRef.ChosenPlayer -> []
  -- CR 401.2's ordered pile, whose head is the top (CR 121.1). The depth is taken
  -- from EACH named library, top first, and a shorter library gives what it has
  -- (CR 609.3). Restricted to the players still in the turn order and delivered in
  -- it (CR 608.2f, CR 101.4), which also drops a player CR 800.4 removed.
  --
  -- The depth is a Quantity evaluated HERE (CR 608.2c), off the announcement CR
  -- 601.2b left on the resolving object -- which is why `resolving` rather than
  -- `source` is the announcedOn id -- and ONCE for the whole ref, no printing
  -- writing a per-library depth. A depth that will not evaluate, or evaluates
  -- negative, is ZERO cards (CR 107.1b): a REGRESSION FENCE, since making the
  -- fallback 5 leaves the suite green.
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player count) ->
    let named = playerRefPlayers legal controller gs player
        viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        depth = maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source count)
     in concatMap
          (\pid -> List.genericTake depth (Game.zoneMembers Zone.Library pid gs))
          (filter (`elem` named) (Game.apnapOrder gs))
  -- The arm above's walk with its Quantity counting MATCHES rather than cards:
  -- the same prefix of CR 401.2's ordered pile taken from its head (CR 121.1),
  -- ended by the card whose match brings the tally up to the count instead of by
  -- a counted depth. That card is IN the prefix, which is what Treasure Hunt's
  -- "until you reveal a nonland card" and Open the Way's "until you reveal X land
  -- cards" both say -- the walk stops having reached it, not before it.
  --
  -- A library holding fewer matches than the count is given up whole (CR 609.3),
  -- which `walkDown` does by running out of cards; the rest of the instruction is
  -- then performed on all of it (CR 101.3). An empty library names nothing, for
  -- the same reason, and so does a count of zero -- an unevaluable or negative
  -- one being clamped to zero here (CR 107.1b), the arm above's clamp.
  --
  -- Per named library and in APNAP order, the arm above's fold, so "each
  -- player's" walks each pile separately rather than one across the table. The
  -- Filter is matched against each card's own projection as the walk reaches it
  -- (CR 608.2c) -- the context is this resolution's, so a filter reading a slot
  -- this resolution bound sees it.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil player filter_ count) ->
    let named = playerRefPlayers legal controller gs player
        viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        wanted = maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source count)
        viewOfCard = Projection.viewsOf gs
        matches oid = Filter.matches context (viewOfCard oid) filter_
        walkDown remaining oids =
          if remaining <= (0 :: Natural)
            then []
            else case oids of
              [] -> []
              oid : rest -> oid : walkDown (if matches oid then remaining - 1 else remaining) rest
        walk pid = walkDown wanted (Game.zoneMembers Zone.Library pid gs)
     in concatMap walk (filter (`elem` named) (Game.apnapOrder gs))
  -- A card somebody CHOOSES is a QUESTION, and this function cannot ask one; the
  -- MoveToZone arm's own gather does. Under any other opcode this empty answer is
  -- an inert card-data error.
  ObjectRef.ChosenCardInGraveyard {} -> []
  ObjectRef.ChosenCardInHand {} -> []
  ObjectRef.ChosenCardFromAmong {} -> []
  -- The arm above's plural, and answered HERE rather than deferred, which is the
  -- whole difference between them: "all land cards revealed this way" asks
  -- nobody anything, so every opcode reading this function gets the set. The
  -- members are InSlot's own read of the slot -- the arm at the head of this
  -- case, so the sentence naming the matches and a later one naming the rest
  -- cannot see different groups -- narrowed by the ref's Filter against each
  -- member's CR 613 projection, matched when the effect executes (CR 608.2c) in
  -- this effect's own context (CR 109.5). The group's mint order survives, which
  -- CR 608.2f leaves standing.
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong slot filter_) ->
    matchingFromAmong legal resolving controller source gs filter_ $
      objectRefObjects legal resolving controller source gs (ObjectRef.InSlot slot)
  -- Answered for real by the REVEAL arm, over the seats handChoosers names.
  ObjectRef.RandomCardInHand _ -> []

-- The players a ZoneScope names, in APNAP order -- whose graveyards is
-- Target.zoneScopePlayers, the same answer a target pool over CR 400.1's
-- per-player zone gets, and the order imposed on it here is APNAP (CR 608.2f, CR
-- 101.4) restricted to the players still in the game. The seat half of both
-- graveyardCards and ObjectRef.ChosenCardInGraveyard's EachInScope chooser,
-- which asks each seat separately.
--
-- The bindings are the ones the CALLER holds, which is CR 608.2b's re-checked set
-- at resolution: an InSlot scope naming a slot whose target went illegal names
-- nobody, and CR 101.3 ignores that share of the effect.
zoneScopePlayers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> ZoneScope.ZoneScope -> [PlayerId]
zoneScopePlayers bindings controller gs scope =
  let named = Target.zoneScopePlayers (Just controller) bindings scope gs
   in filter (`elem` named) (Game.apnapOrder gs)

-- The cards in ONE player's graveyard matching the filter, in ascending
-- ObjectId. The filter is matched in THIS EFFECT's context -- the caller's, so
-- CR 109.5's "you" is the resolving controller rather than whoever is choosing,
-- and the resolution's own slots ride along: Midnight Tilling's "from among
-- them" is `IsBound` over the slot its own mill defined, which a bare contextFor
-- would answer False for on every candidate.
graveyardCardsOf :: Filter.Context -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
graveyardCardsOf context gs pid filter_ =
  let viewOf = Projection.viewsOf gs
   in List.sort
        ( filter
            (\oid -> Filter.matches context (viewOf oid) filter_)
            (Game.zoneMembers Zone.Graveyard pid gs)
        )

-- The cards in the named graveyards matching the filter, for
-- ChosenCardInGraveyard's TheController chooser -- ObjectRef.EachCardInGraveyard
-- is this same fold, spelled out at its own arm. A card in a
-- graveyard has no controller, so Filter.ControlledBy is vacuously False. APNAP
-- (CR 101.4) then ascending ObjectId, not the graveyard's own pile order (CR
-- 404.2), which no rule makes a batch's processing order.
graveyardCards :: Filter.Context -> Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> ZoneScope.ZoneScope -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
graveyardCards context bindings controller gs scope filter_ =
  concatMap (\pid -> graveyardCardsOf context gs pid filter_) (zoneScopePlayers bindings controller gs scope)

-- The seats an ObjectRef.ChosenCardInHand asks -- and an
-- ObjectRef.RandomCardInHand reads -- in APNAP order. One list, not a chooser
-- plus a scope: CR 402.3 lets a player look only at their own hand. Through
-- playerRefPlayers, so an unfilled, illegal or non-player slot names nobody and
-- CR 101.3 ignores that share (CR 608.2b).
handChoosers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> PlayerRef -> [PlayerId]
handChoosers legal controller gs player =
  let named = playerRefPlayers legal controller gs player
   in filter (`elem` named) (Game.apnapOrder gs)

-- The cards in ONE player's hand matching the filter: graveyardCardsOf one zone
-- over. The filter is matched in THIS EFFECT's context, so `controller` is CR
-- 109.5's "you" rather than whoever is choosing.
--
-- NOT sorted, where the graveyard sibling sorts: the candidates keep the zone's
-- own order, which no rule reads (CR 402.3). Narrowing must not reorder.
handCardsOf :: Filter.Context -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
handCardsOf context gs pid filter_ =
  let viewOf = Projection.viewsOf gs
   in filter
        (\oid -> Filter.matches context (viewOf oid) filter_)
        (Game.zoneMembers Zone.Hand pid gs)

-- CR 401.2 and CR 401.4: turn the effect's LibraryPlacement into the END each
-- moving object arrives at, and hand back the batch in the order the moves must
-- then be PERFORMED in. Both questions are asked before anything moves (CR
-- 608.2f): CR 401.2 once per object, asked of the OWNER in the sweep's APNAP
-- order (CR 101.4); CR 401.4 once per (owner, end) group of two or more, whose
-- decider is the owner rather than CR 608.2f's resolving controller -- except
-- where the effect states a RANDOM order, which takes that arrangement back from
-- the owner and randomises the group instead (CR 701.24a's standard).
--
-- The arrangement answer names the cards from the chosen end INWARD, and
-- Game.insertIntoZone puts every arrival AT that end, so the batch is performed
-- in REVERSE of the arranged order -- one rule for both ends. A move the CR
-- 616.1 loop cancels (CR 614.6) simply does not arrive, and the rest keep their
-- owner's relative order.
settleArrivals :: Zone.Zone -> LibraryPlacement.LibraryPlacement -> [ObjectId] -> Game [(ObjectId, LibraryPosition.LibraryPosition)]
settleArrivals zone placement targets = case zone of
  Zone.Library -> do
    settled <- Monad.mapM settleEnd targets
    fmap concat (Monad.mapM (arrange settled) (List.nub (fmap fst settled)))
  -- No other destination has ends, so nothing to settle and the funnel ignores
  -- the position it is handed.
  _ -> pure (fmap (\oid -> (oid, LibraryPosition.defaultValue)) targets)
  where
    settleEnd oid = do
      gs <- State.get
      case fmap Object.owner (Game.lookupObject oid gs) of
        -- Already gone (CR 603.7c). moveOne is a no-op, so nobody to ask.
        Nothing -> pure ((Nothing, LibraryPosition.defaultValue), oid)
        Just owner -> do
          position <- case placement of
            LibraryPlacement.Stated stated -> pure stated
            LibraryPlacement.RandomOrder stated -> pure stated
            LibraryPlacement.OwnerChooses ->
              Game.choose (Prompt.ChooseLibraryEnd (Decide.deciderFor owner gs) owner oid)
          pure ((Just owner, position), oid)
    arrange settled key = do
      let batch = [oid | (k, oid) <- settled, k == key]
          (mOwner, position) = key
      case (mOwner, batch) of
        (Just owner, _ : _ : _) -> do
          ordered <- case placement of
            -- The effect states the order, so CR 401.4's owner is not asked.
            -- Prompt.Shuffle is the randomness channel the mulligan and CR
            -- 701.24a's own shuffle already go through, so the engine still
            -- rolls nothing; Game.honourShuffle refuses an answer that is not a
            -- permutation of the batch, which is what keeps a random order from
            -- inventing or destroying cards.
            LibraryPlacement.RandomOrder _ -> do
              answer <- Game.ask (Prompt.Shuffle batch)
              pure (Game.honourShuffle batch answer)
            _ -> do
              gs <- State.get
              answer <- Game.choose (Prompt.ArrangeLibraryArrivals (Decide.deciderFor owner gs) owner position batch)
              pure (Game.permute batch answer)
          pure (fmap (\oid -> (oid, position)) (reverse ordered))
        -- One card is one order, which is CR 401.4's own "two or more".
        _ -> pure (fmap (\oid -> (oid, position)) batch)

-- The same sweep as objectRefObjects, one step earlier: what an ObjectRef names
-- as RECIPIENTS. It exists because CR 115.4's "any target" includes a player and
-- CR 120.1 lets damage go to one, so DealDamage's InSlot arm must name something
-- no ObjectId can.
objectRefRecipients :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [Recipient]
objectRefRecipients legal resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- A group is objects; a player is something only a TARGET slot can hold.
    Just group -> fmap Recipient.ToObject (Foldable.toList group)
    Nothing -> legalMany slot legal
  ObjectRef.EachMatching _ -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachCardInGraveyard {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachCardInYourHand -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachCardInHand {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachCardInYourLibrary _ -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachCardExiledWithSource {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.TopOfLibrary {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.TopOfLibraryUntil {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachSpell _ -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachOnStack _ -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- CR 120.3a: a player is a damage recipient. APNAP (CR 608.2f) via
  -- Game.apnapOrder.
  ObjectRef.EachPlayer -> fmap Recipient.ToPlayer (Game.apnapOrder gs)
  -- CR 120.3a again, over CR 102.1's opponents alone -- the arm above filtered
  -- by PlayerRelation.holds against CR 109.5's "you", which is the resolving
  -- controller. APNAP order survives the filter (CR 608.2f).
  ObjectRef.EachOpponent -> fmap Recipient.ToPlayer (filter (PlayerRelation.holds (Game.teams gs) PlayerRelation.Opponent controller) (Game.apnapOrder gs))
  -- CR 120.3a, one seat wide: the player the SOURCE chose as it entered (CR
  -- 614.12a). Read off `source` (CR 113.7a), not `resolving`, which for a
  -- triggered ability is the ability object and never carries the choice.
  -- Nothing where the source has left or never chose, which CR 101.3 ignores.
  ObjectRef.ChosenPlayer ->
    Maybe.maybeToList (fmap Recipient.ToPlayer (Game.lookupObject source gs >>= Object.chosenPlayer))
  -- No recipients: the answer needs the chooser asked, and only the MoveToZone
  -- gather can ask.
  ObjectRef.ChosenCardInGraveyard {} -> []
  ObjectRef.ChosenCardInHand {} -> []
  ObjectRef.ChosenCardFromAmong {} -> []
  -- A read rather than a question, so the sweep above answers it -- and objects
  -- only, a group never holding a player.
  ObjectRef.EachCardFromAmong {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- No recipients: only the Reveal arm can ask the interpreter.
  ObjectRef.RandomCardInHand _ -> []
  -- No recipients: the answer needs the chooser asked, and only
  -- turnPermanentsOver's gather and the Effect.MoveToZone gather can ask.
  ObjectRef.AnyNumberMatching _ -> []
  -- No recipients: the arm above's answer, for its reason -- only a gather that
  -- reaches the Game monad can ask the chooser.
  ObjectRef.ChosenPermanent _ -> []
  ObjectRef.SourceAndChosenPermanent _ -> []

-- The order for a per-object batch -- CR 608.2f's, and CR 701.44d's, which say
-- the same thing about the PRIMARY key: APNAP first, reading a player recipient
-- as that seat and an object as its controller's. Imposed here rather
-- than in objectRefRecipients, whose InSlot arm answers in Recipient (set)
-- order; this loop is the first reader that takes recipients one at a time.
--
-- The second key is a CHOICE, and which player makes it is the caller's, via
-- `chooserFor`: it is handed the group's own seat and answers who is asked, or
-- Nothing for nobody. CR 608.2f's secondary sentence gives it to the RESOLVING
-- CONTROLLER (`const (Just controller)`); CR 701.44d gives it to the player who
-- controls the permanents themselves (`id`), which is the only difference
-- between the two rules' orderings and the reason this takes a function.
--
-- Each seat's group is handed to its chooser as a Prompt.OrderForEach. Asked
-- once per group, in APNAP order of the groups; a group of one is not asked.
-- Game.permute keeps the engine's order for a non-permutation answer.
--
-- The GROUPING is what CR 701.44d's repeat clause describes as well: the first
-- APNAP seat holding any of the permanents keeps being first while it holds one,
-- so a whole seat's instructions run before the next seat's rather than
-- interleaving.
--
-- A recipient the board no longer holds has no controller and sorts last, which
-- is reachable rather than defensive (CR 400.7); two such share that bucket.
forEachOrder :: ObjectId -> (Maybe PlayerId -> Maybe PlayerId) -> [Recipient] -> Game [Recipient]
forEachOrder resolving chooserFor recipients = do
  gs <- State.get
  let order = Game.apnapOrder gs
      last_ = length order
      seatOf = recipientSeat gs
      rank recipient = maybe last_ (\pid -> Maybe.fromMaybe last_ (List.elemIndex pid order)) (seatOf recipient)
      groups = List.groupBy (\a b -> rank a == rank b) (List.sortOn (\recipient -> (rank recipient, recipient)) recipients)
      pick group = case group of
        first_ : _ : _ | Just chooser <- chooserFor (seatOf first_) -> do
          answer <- Game.choose (Prompt.OrderForEach (Decide.deciderFor chooser gs) chooser resolving group)
          pure (Game.permute group answer)
        _ -> pure group
  fmap concat (traverse pick groups)

-- WHOSE a recipient is: a player recipient is that seat, an object's is its
-- controller (CR 110.2) -- and CR 608.2h's last known information for an object
-- the board no longer holds, which is reachable rather than defensive (CR
-- 400.7): Effect.ForEach walks the permanents a destruction already removed.
-- CR 701.44c asks for the same read by name, and reading it here is what keeps
-- this and exploreOne agreeing about whose a departed permanent is.
-- Nothing only where neither answer exists; each caller says what it does with
-- that.
recipientSeat :: GameState -> Recipient -> Maybe PlayerId
recipientSeat gs recipient = case recipient of
  Recipient.ToPlayer pid -> Just pid
  _ -> Recipient.objectOf recipient >>= \oid -> Projection.controllerWithLastKnown oid gs

-- CR 701.21a: which player one Effect.Sacrifice instructs, as the function that
-- performs it. The rule's second sentence -- "a player can't sacrifice ...
-- something that's a permanent they don't control" -- is what makes the two arms
-- observably different rather than two spellings of one thing.
--
-- EffectController hands Event.sacrifice this effect's controller and lets that
-- rule REFUSE, which is the printed "sacrifice it": Ray of Command stealing a
-- Thatcher Revolt token before its delayed "sacrifice those tokens" turns on
-- leaves the token on the battlefield, and Pawl.ResolveSpec's "CR 701.21a a
-- stolen token is not sacrificed by the player who was told to sacrifice it" is
-- the proof.
--
-- PermanentController reads the controller off the permanent instead, live (CR
-- 613.1b's layer 2), which is the printed "[that permanent]'s controller
-- sacrifices it" -- CR 701.54c's three-temptation tier, whose blocker its own
-- controller sacrifices. Rule 701.21a can never refuse this arm, by construction.
-- Nothing to do for an object that is gone or has no controller (CR 400.7), which
-- is also how the funnel answers.
sacrificerFor :: Sacrificer.Sacrificer -> PlayerId -> ObjectId -> Game ()
sacrificerFor sacrificer controller oid = case sacrificer of
  Sacrificer.EffectController -> Event.sacrifice controller oid
  Sacrificer.PermanentController -> do
    gs <- State.get
    Monad.forM_ (Projection.controllerOf oid gs) (\pid -> Event.sacrifice pid oid)

-- The objects a Create bound into `slot` as a GROUP, read off the RESOLVING
-- stack object's live bindings rather than out of `chosen`, which projects CR
-- 601.2c's targets only. Live is what lets a later effect of the same resolution
-- name what an earlier Create minted.
--
-- A Just here wins over the slot's target, which would skip a CR 608.2b
-- re-validation. One Binding can carry both fields, but a Pawl.CardSpec lint
-- ("no delayed ability declares a target slot under a name its card defines")
-- rules the case out, so this arm never actually chooses.
slotGroup :: SlotName -> ObjectId -> GameState -> Maybe (Seq.Seq ObjectId)
slotGroup slot resolving gs = Binding.objectsOf slot (maybe Map.empty Object.bindings (Game.lookupObject resolving gs))

-- slotGroup's singular: the ONE object bound at a slot, read live off the
-- resolving object rather than out of `chosen`, which is a PROJECTION -- CR
-- 601.2c's targets under CR 700.2d's names, gated on CR 608.2b legality. A
-- binding an earlier effect DEFINED is neither a target nor renameable. See
-- bindObjectsSlot's note, and CR 603.7c for why the binding exists.
--
-- Nothing when the slot is unbound, holds a group, names several targets, or
-- names a player: both callers want one object or none.
slotOne :: SlotName -> ObjectId -> GameState -> Maybe ObjectId
slotOne slot resolving gs = do
  obj <- Game.lookupObject resolving gs
  Recipient.objectOf =<< Binding.onlyOne =<< Map.lookup slot (Binding.targetsOf (Object.bindings obj))

-- CR 608.2g: make the offer Effect.OfferCast carries, and cast if it is taken.
--
-- The four questions, in the order the rules ask them:
--
--   1. IS THERE ANYTHING TO OFFER -- the slot's id (CR 400.7) may no longer
--      resolve to an object (CR 603.7c).
--   2. WHICH FACE: CR 712.11a for the `transformed` rider, otherwise
--      Card.castableFaces (CR 709.3, CR 712.11b, CR 715.3, CR 720.3), less the face CR
--      702.162a's alternative cost is the only road to when this offer states an
--      alternative cost of its own (CR 118.9a).
--   3. WHAT IT COSTS (CR 118.9): `withoutPayingManaCost` or a stated
--      `payingInstead` (CR 702.94a); otherwise CR 601.2b's own candidates.
--   4. MAY IT BE CAST AT ALL -- Cast.castableWhenOffered, asked BEFORE the
--      prompt so no cast is offered that the announcement would reverse.
--
-- Questions 3 and 4 are asked of EACH half separately (CR 709.3a, CR 712.11c);
-- where more than one survives, CR 709.3's choice is put to the caster before
-- the "may" below, since CR 118.8c's excuse is a property of the spell being
-- cast. At CastObligation.Mandatory the cast is not a decision, so
-- Prompt.OfferedCast is elided; question 4 is what a printed "if able" comes to
-- (CR 601.3, CR 609.3). CR 118.8c is the exception: `excused` turns the
-- mandatory branch back into a may, classified by Cost.statesHiddenQuality.
--
-- The caster is a parameter and not the resolving controller: CR 608.2g says "a
-- player". Everything above is a CLASSIFICATION carried by the opcode's
-- CastOffer and its CastObligation; nothing here asks which card is offered.
offerCast :: ObjectId -> PlayerId -> SlotName -> CastObligation.CastObligation -> CastOffer.CastOffer -> Game ()
offerCast resolving caster slot optionality offer = do
  gs <- State.get
  let -- Whether this offer states CR 118.9's alternative cost, in either of the
      -- two wordings `applied` below reads. NOT `transformed`, which is CR
      -- 712.11a's rider about which face goes on the stack and says nothing about
      -- payment.
      alternative = CastOffer.withoutPayingManaCost offer || Maybe.isJust (CastOffer.payingInstead offer)
      -- CR 712.11a for the transformed rider; CR 709.3, CR 712.11b, CR 715.3 and
      -- CR 720.3 otherwise, via Card.castableFaces. Nothing for a card with no back face
      -- (CR 712.14a): an offer that cannot be made is not made.
      --
      -- Less Card.convertedFace under an alternative, which is CR 118.9a: a spell
      -- takes one alternative cost, and that face is on the list only because CR
      -- 702.162a's alternative cost put it there, so an offer spending the
      -- spell's one alternative on something else cannot also reach it. CR
      -- 712.11's default -- the front face -- is then the whole answer.
      --
      -- Card.castableFaces carries the HALVES and never Card.fusedFace, so an
      -- offer naming a card in a hand offers each half and no fused cast. Not
      -- implemented: CR 702.102a's fused cast under a permission an effect grants
      -- (#2787). The direction is the safe one -- this list only ever offers less
      -- than the rules allow, never a cast for free.
      faces card
        | CastOffer.transformed offer = fmap pure (Card.backFace card)
        | alternative = Just (filter (\face -> fmap Face.name (Card.convertedFace card) /= Just (Face.name face)) (Card.castableFaces card))
        | otherwise = Just (Card.castableFaces card)
      -- One proposal per half, gated on its own (CR 709.3a, CR 712.11c), which
      -- is why the whole tuple is built per face rather than once per card.
      --
      -- The excuse being the half's is unobserved: no printing pairs a
      -- multi-half layout with a hidden-zone additional cost (gap #1814).
      proposal oid face =
        let name = Face.name face
            -- CR 118.9a: at most ONE alternative cost, so the applied one
            -- replaces the candidates rather than joining them; the two riders
            -- are asked in order, free first. CR 118.9d in both cases -- an
            -- alternative replaces only the MANA cost, so the face's additional
            -- costs ride along.
            applied
              | CastOffer.withoutPayingManaCost offer = Just (Cost.withoutPayingManaCost face)
              | otherwise = fmap (\c -> c {Cost.Type.components = Cost.Type.components c <> Face.additionalCosts face}) (CastOffer.payingInstead offer)
            -- Face up: CR 708.4's face-down cast is a morph permission (CR
            -- 702.37d), and an OfferCast opcode carries no such rider.
            proposed = Cast.asProposed oid name Facing.FaceUp gs
            candidates = maybe (Cost.candidateCostsFor name oid proposed) (pure . Cost.untagged) applied
         in if Cast.castableWhenOffered caster oid name candidates proposed
              then
                -- CR 118.8c, read off the same candidates the cast will be
                -- announced with: CR 118.9d keeps the face's additional costs on
                -- an alternative, so every candidate already carries them.
                --
                -- Not implemented: a cost APPLIED from another effect (CR 118.8)
                -- arrives as CostAdjustments.components and is not read here
                -- (#1834).
                Just (oid, name, applied, any (Cost.statesHiddenQuality . CandidateCost.cost) candidates)
              else Nothing
      offers = Maybe.fromMaybe [] $ do
        oid <- slotOne slot resolving gs
        card <- Game.cardOf oid gs
        fmap (Maybe.mapMaybe (proposal oid)) (faces card)
  -- No survivor is no offer; one survivor is one outcome, so CR 709.3's choice
  -- is elided there rather than asked.
  chosen <- case offers of
    [] -> pure Nothing
    [sole] -> pure (Just sole)
    first : rest -> do
      let decider = Decide.deciderFor caster gs
          nameOf (_, name, _, _) = name
          oidOf (oid, _, _, _) = oid
      picked <- Game.choose (Prompt.ChooseOfferedCastFace decider caster (oidOf first) (fmap nameOf (first NonEmpty.:| rest)))
      -- Reject-not-repair: a name the offer did not include is no cast at all.
      pure (List.find ((== picked) . nameOf) offers)
  case chosen of
    Nothing -> pure ()
    Just (oid, name, applied, excused) -> do
      let cast = Cast.castSpellWith performManaAbility applied caster oid name Facing.FaceUp
          -- The SAME prompt on both paths: CR 118.8c creates no new decision.
          mayCast = do
            let decider = Decide.deciderFor caster gs
            decision <- Game.choose (Prompt.OfferedCast decider caster oid name)
            case decision of
              OptionalDecision.Declines -> pure ()
              OptionalDecision.Exercises -> cast
      case optionality of
        CastObligation.Mandatory | not excused -> cast
        CastObligation.Mandatory -> mayCast
        CastObligation.Optional -> mayCast

-- CR 615.3: install one floating damage row, for a duration. Shared by
-- Effect.PreventNextDamage, Effect.PreventAllDamage and Effect.RedirectDamage,
-- which differ only in the DamageRewrite -- CR 615.7's countdown, CR 615.1's
-- unbounded shield, or CR 614.9's redirection.
--
-- One shield PER RECIPIENT the caller's ObjectRef NAMES, which is why the callers
-- fold this over that set: those recipients were fixed by the resolution (CR
-- 615.11 for an untargeted "each", CR 601.2c for a targeted one), so a shield
-- each is what the rules describe.
--
-- A recipient side the card DESCRIBES instead is the other shape and is not
-- folded: `describedRecipient` below is written onto ONE row, which CR 615.7's
-- "such effects count only the amount of damage; the number of events or sources
-- dealing it doesn't matter" makes a single shared pool over every recipient it
-- admits. Divine Deflection is the countdown's producer, and Pawl.Engine.Replacement's
-- rewriteRemaining is where one row spends per point rather than per recipient.
-- CR 615.1's UNBOUNDED shield (Pack Leader) has no amount for that rule to be
-- about, so one row there is simply the row a description needs: with nothing to
-- count down, a row per admitted recipient would prevent exactly the same damage.
--
-- The `rider` is CR 615.5's additional effect, Nothing for a row that has none;
-- a redirection is not a prevention, so RedirectDamage never passes one.
--
-- The folded argument is the row's TWO baked halves, since CR 615.1's shield can
-- be pinned on either side of the damage event: the recipient it covers, and the
-- source it watches. Nothing on a side means EVERY recipient or every source,
-- which is DamagePattern's own reading of the field. Dovin, Hand of Control's
-- "-1: prevent all damage that would be dealt to and dealt BY target permanent"
-- is one row of each, so no row today names both -- nothing here forbids one.
--
-- The source half carries both halves of CR 615.9 -- the printed properties and
-- the id -- so a row cannot be given an id without the recheck that goes with
-- it. That recheck is CR 609.7a's, and a TARGETED source has no properties to
-- recheck (CR 601.2c declared the object itself), so it passes the trivial
-- predicate rather than being exempted from the pairing.
--
-- `printed` is the OTHER way a row can name its source: CR 609.7b's properties,
-- written on the card and asking nobody to choose (Scarecrow's "by creatures
-- with flying"). It reaches the same DamagePattern field as the pair's filter
-- above, so a caller writing both gets their conjunction; `And []` on either
-- side drops out rather than nesting. No card in data/cards/ writes both, and
-- only Effect.PreventAllDamage passes a non-trivial one at all: no printing
-- pairs CR 609.7b's properties with CR 615.7's countdown or CR 614.9's
-- redirection -- Scryfall `o:"prevent the next" o:"sources"`, 2026-08-27, no
-- hit -- so those two opcodes carry no such field.
installDamageRow :: Map.Map SlotName PlayerId -> Map.Map SlotName (Set ObjectId) -> PlayerId -> ObjectId -> Duration.Duration -> Maybe DamageKind.DamageKind -> DamageRewrite.DamageRewrite -> Maybe PreventionRider.PreventionRider -> Filter.Type.Filter Keyword.Type.Keyword -> (Maybe (Filter.Type.Filter Keyword.Type.Keyword), Maybe PlayerRelation.PlayerRelation) -> GameState -> (Maybe Recipient, Maybe (Filter.Type.Filter Keyword.Type.Keyword, ObjectId)) -> GameState
installDamageRow players slots controller source duration kind rewrite rider printed describedRecipient g (recipient, sourceChoice) = case Expiry.arm players controller source duration g of
  -- CR 611.2b: the duration never started, so no shield is installed.
  Nothing -> g
  Just expiry ->
    let (ts, g1) = Game.freshTimestamp g
        re =
          ReplacementEffect.DamageR
            ( DamageR.MkDamageR
                DamagePattern.MkDamagePattern
                  { -- PRINTED, not assumed: Nothing takes combat and
                    -- noncombat alike, Just Combat only the former.
                    DamagePattern.whichKind = kind,
                    -- A caller that names no source gets the trivial
                    -- predicate, which is CR 615.7's own "the number of
                    -- events or sources dealing it doesn't matter"; one
                    -- that named a CHOSEN source (CR 609.7a) gets that
                    -- source's printed properties, for CR 615.9's recheck
                    -- at the damage event. A caller naming CR 609.7b's
                    -- PRINTED properties instead contributes them here
                    -- too, and the trivial predicate drops out of the
                    -- conjunction rather than nesting inside it.
                    DamagePattern.whatSource = case filter (/= Filter.Type.And []) (printed : Maybe.maybeToList (fmap fst sourceChoice)) of
                      [only] -> only
                      named -> Filter.Type.And named,
                    -- CR 611.2c's DESCRIBED recipient side, disjoined on the
                    -- pattern: a shield covering "you and/or permanents you
                    -- control" is read live at each damage event, since a
                    -- prevention effect changes no characteristic and no
                    -- controller and so reaches objects it did not reach
                    -- when it began. Both halves are Nothing for a row
                    -- whose recipient the resolution BAKED below instead.
                    DamagePattern.whatRecipient = fst describedRecipient,
                    DamagePattern.whoRecipient = snd describedRecipient,
                    DamagePattern.whichRecipient = recipient,
                    -- CR 609.7a's chosen source or CR 601.2c's targeted
                    -- one, baked for the recipient's reason -- both were
                    -- fixed when this effect was created and card data can
                    -- name neither.
                    DamagePattern.whichSource = fmap snd sourceChoice
                  }
                rewrite
                -- CR 615.5's rider on this carrier is the snapshotted one
                -- on the row below; the authored field here stays empty.
                Seq.empty
            )
        active =
          ActiveReplacement.MkActiveReplacement
            { ActiveReplacement.effect = re,
              ActiveReplacement.source = source,
              -- CR 109.5, baked as Replace's is.
              ActiveReplacement.controller = controller,
              ActiveReplacement.timestamp = ts,
              ActiveReplacement.expiry = expiry,
              -- CR 615.7's shield is spent in DAMAGE, not in applications, so
              -- the use count is not what ends it (see Pawl.Types.Uses).
              ActiveReplacement.uses = Uses.Unlimited,
              -- CR 614.15: a self-replacement is one that replaces the damage its
              -- OWN resolution deals, which none of these rows does -- not even
              -- one whose CR 609.7a choice happened to land on this very source.
              ActiveReplacement.origin = ReplacementOrigin.Other,
              -- No clause: the prevention opcodes carry no printed "if" (see
              -- Pawl.Types.ActiveReplacement).
              ActiveReplacement.condition = Nothing,
              ActiveReplacement.rider = rider,
              -- The installing resolution's slot bindings, captured here for the
              -- reason Effect.Replace's row captures them (CR 603.7c): this
              -- resolution is about to end, and DamagePattern.whatSource and
              -- DamagePattern.whatRecipient are re-asked at every damage event
              -- (Pawl.Engine.Replacement.candidateContext), so a Filter.IsBound in
              -- either would have nothing live to read. The two carriers of a
              -- DamageR row therefore answer a bound slot the same way.
              --
              -- NARROWED to the slots THIS ROW names, not the whole resolution's
              -- map, because the map is also read as CR 609.7a's third class:
              -- referredToSources offers "any object referred to by ... a
              -- replacement or prevention effect that's waiting to apply", and a
              -- slot an unrelated earlier effect of the same resolution bound is
              -- not something this row refers to. The redirection carries no
              -- rider and no clause, but not an empty row: its CR 609.7a
              -- chosen-source predicate is the card's own Filter and folds into
              -- DamagePattern.whatSource, which Synthetic Turn the Blade writes,
              -- and Harm's Way's recipient description rides it as Divine
              -- Deflection's does.
              ActiveReplacement.slots = Map.restrictKeys slots namedSlots
            }
        -- Every slot name the row above can name: replacementRowReads' answer for
        -- the ReplacementEffect just built, which is the whole of what
        -- Replacement.candidateContext re-asks it against, plus CR 615.5's rider,
        -- whose objects the effect still refers to even though
        -- Resolve.runPreventionRider reads them off Pawl.Types.PreventionRider
        -- rather than off the row. `condition` is Nothing on every one of these
        -- carriers, so it contributes no third half.
        namedSlots = Map.keysSet (replacementRowSlots re) <> foldMap (Map.keysSet . PreventionRider.targets) rider
     in g1 {GameState.replacements = active : GameState.replacements g1}

-- CR 609.7a: choose the SOURCE a prevention or redirection effect names, and
-- answer the pair installDamageRow bakes -- the printed properties beside the id
-- the choice landed on. Nothing for a row naming no source, and Nothing again where the
-- rule's pool held nothing the card's properties admit, which the caller tells
-- apart by whether it passed a Filter at all.
--
-- CHOOSE, not target: rule 609.7a says "a source of your choice", so nothing was
-- declared on the stack (CR 601.2c) and there is no CR 608.2b legality to
-- re-check at the damage event -- CR 615.9 rechecks the source's PROPERTIES
-- instead, which is DamagePattern.whatSource's job and not this one's.
--
-- FILTERED, NOT TRUSTED, the ChooseBolster posture: an answer naming something
-- never offered falls back to the first candidate. The prompt is raised only for
-- TWO OR MORE candidates, one candidate being the whole of the rule's set.
chooseDamageSource :: PlayerId -> ObjectId -> Filter.Context -> GameState -> Maybe (Filter.Type.Filter Keyword.Type.Keyword) -> Game (Maybe (Filter.Type.Filter Keyword.Type.Keyword, ObjectId))
chooseDamageSource controller resolving context gs filter_ = case filter_ of
  Nothing -> pure Nothing
  Just f -> case damageSourceCandidates context gs f of
    [] -> pure Nothing
    first : rest -> do
      picked <- case rest of
        [] -> pure first
        second : more -> do
          let offered = first NonEmpty.:| (second : more)
          answer <- Game.choose (Prompt.ChooseDamageSource (Decide.deciderFor controller gs) controller resolving offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      pure (Just (f, picked))

-- CR 609.7a's candidate set, its four classes in the rule's own order: "a
-- permanent; a spell on the stack (including a permanent spell); any object
-- referred to by an object on the stack, by a replacement or prevention effect
-- that's waiting to apply, or by a delayed triggered ability that's waiting to
-- trigger (even if that object is no longer in the zone it used to be in); or a
-- face-up object in the command zone", narrowed to the properties the card
-- printed.
--
-- SPELLS on the stack, not the whole zone: the rule says "a spell", and an
-- activated or triggered ability sharing that zone falls under none of the four
-- classes -- it is not a permanent, not a spell, not in the command zone, and
-- nothing refers to it. Game.isSpell asks the object's zone and its KIND, never which
-- card it is -- the same reader ObjectRef.EachSpell above narrows with under CR
-- 109.2b, where the CR 405.1 arm beside it takes the whole zone instead.
--
-- Read off the PROJECTION, so a permanent that is a red source only by a
-- continuous effect is a candidate -- with CR 608.2h's fallback onto last known
-- information, since the third class admits an object "no longer in the zone it
-- used to be in" and a departed id projects blank. Exactly the pair
-- Pawl.Engine.Replacement.matchesDamageSource rechecks with under CR 609.7b, so
-- the choice and the recheck cannot read one source two ways.
--
-- The fallback is what Pawl.ReplacementSpec's "a departed source is narrowed by
-- its last known information" proves: Synthetic Turn the Blade narrows to Humans
-- and the Ghitu Fire-Eater its controller names has already paid itself as a
-- cost, so under the bare viewOfObject it satisfies no subtype filter, is never
-- offered, and the redirection watches a source nobody chose.
--
-- DEDUPED, the classes overlapping wherever a spell's living target is also a
-- permanent, and ASCENDING out of the same Set, so both the single-candidate
-- shortcut and a transcript are deterministic -- Pawl.Engine.Blight's posture.
damageSourceCandidates :: Filter.Context -> GameState -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
damageSourceCandidates context gs filter_ =
  let faceUp oid = fmap Object.facing (Game.lookupObject oid gs) == Just Facing.FaceUp
      viewOf oid = Maybe.fromMaybe (Projection.viewOfObject oid gs) (Projection.viewWithLastKnownAnywhere gs oid)
      pool =
        Set.fromList
          ( Set.toList (GameState.battlefield gs)
              <> filter (\oid -> Game.isSpell oid gs) (GameState.stack gs)
              <> referredToSources gs
              <> filter faceUp (Set.toList (GameState.command gs))
          )
   in filter (\oid -> Filter.matches context (viewOf oid) filter_) (Set.toAscList pool)

-- CR 609.7a's third class, off the three carriers the sentence names: every
-- object on the stack, every row of GameState.replacements waiting to apply, and
-- every entry of GameState.delayedTriggers waiting to trigger. What each REFERS
-- TO is the object it names as its source plus the objects its bindings or slots
-- hold -- Ghitu Fire-Eater's ability naming the creature its own cost
-- sacrificed, a shield naming "that creature", a delayed trigger naming "it".
--
-- The SOURCE counts as a referent for all three: CR 113.7a says an ability that
-- causes a source to do something checks that source's information when it
-- resolves, and a waiting row's own condition and pattern are evaluated in a
-- Filter.Context built on its source (Pawl.Engine.Replacement.collect), so each
-- of the three can genuinely name it.
--
-- NO ZONE TEST anywhere in here, which is the clause's point: an id that names
-- nothing is exactly the case the parenthesis admits, and damageSourceCandidates
-- reads it through CR 608.2h's last known information rather than through the
-- blank view a live-only projection gives.
--
-- A waiting ROW refers to three things, and the fold below is over all three
-- rather than over a whole resolution's bindings: its source, the SLOTS its own
-- pattern and rewrite name -- which is all its captured map now holds, since
-- installDamageRow and the Effect.Replace resolution arm restrict what they
-- capture to replacementRowSlots, and Pawl.ReplacementSpec's Synthetic Parting
-- Ward and Galvanic Blast cases read that narrowing -- and the ids a resolution
-- BAKED into it, which no Filter and no slot map holds (referentsOfReplacement).
--
-- Every carrier is proven in the OFFERING direction, each by one
-- Pawl.ReplacementSpec case: the STACK's by "a source only a waiting ability
-- still refers to is offered" (Ghitu Fire-Eater under Auriok Replica), the ROW's
-- baked ids by the Healing Grace case beside it, the row's captured SLOTS by
-- Synthetic Communal Bulwark's under Healing Grace, and the DELAYED TRIGGER's
-- bindings by Come Back Wrong's under Auriok Replica.
--
-- The two binding-reading carriers -- the stack's and the delayed trigger's -- also
-- share one EXCLUSION, stated once at referentsOfBindings below rather than at each
-- of them: an activated ability's own id is not something it refers to.
referredToSources :: GameState -> [ObjectId]
referredToSources gs =
  foldMap (\oid -> foldMap referentsOfObject (Game.lookupObject oid gs)) (GameState.stack gs)
    <> foldMap (\row -> ActiveReplacement.source row : (referentsOfReplacement (ActiveReplacement.effect row) <> foldMap Set.toList (ActiveReplacement.slots row))) (GameState.replacements gs)
    <> foldMap (\entry -> DelayedTrigger.source entry : referentsOfBindings (DelayedTrigger.bindings entry)) (GameState.delayedTriggers gs)

-- The objects a waiting row names BY ID: what card data cannot write, so what no
-- Filter and no captured slot holds, and what CR 609.7a's "any object referred to
-- by ... a replacement or prevention effect that's waiting to apply" reaches only
-- through here. Only the damage arm has any -- CR 609.7a's chosen source or CR
-- 601.2c's targeted one, CR 611.2c's named recipient, and CR 614.9's baked
-- destination -- and each is written as a resolution installs the row
-- (installDamageRow), a card's own redirection naming its destination by
-- description instead (DamageRewrite.RedirectMatching, Pariah). Each is exactly
-- the rule's parenthetical case, an object that may no longer be in the zone it
-- used to be in.
--
-- PLAYER recipients drop out, referentsOfBindings' reason: the rule's four classes
-- are all objects.
--
-- No wildcard: an arm of Pawl.Types.ReplacementEffect that later bakes an id must
-- answer here, or its object silently leaves the pool. Every arm answering []
-- names things by Filter and by slot alone, which replacementRowSlots reports.
referentsOfReplacement :: ReplacementEffect.ReplacementEffect card effect -> [ObjectId]
referentsOfReplacement re = case re of
  ReplacementEffect.ZoneChangeR _ -> []
  ReplacementEffect.EntryR _ -> []
  ReplacementEffect.DamageR (DamageR.MkDamageR pat rewrite _) ->
    Maybe.maybeToList (DamagePattern.whichSource pat)
      <> Maybe.mapMaybe Recipient.objectOf (Maybe.maybeToList (DamagePattern.whichRecipient pat) <> damageRewriteRecipients rewrite)
  ReplacementEffect.DestructionR _ -> []
  ReplacementEffect.CounterR _ -> []
  ReplacementEffect.TokenR _ -> []
  ReplacementEffect.TurnUpR _ -> []
  ReplacementEffect.UntapR _ -> []
  ReplacementEffect.LifeLossR _ -> []
  ReplacementEffect.DrawR _ -> []
  ReplacementEffect.DrawCountR _ -> []
  ReplacementEffect.PhaseR _ -> []

-- The recipients a damage REWRITE bakes, which is CR 614.9's redirect destination
-- and nothing else. damageRewriteFilters' discipline: no wildcard, so a later
-- rewrite naming a recipient must answer here.
damageRewriteRecipients :: DamageRewrite.DamageRewrite -> [Recipient]
damageRewriteRecipients rewrite = case rewrite of
  DamageRewrite.RedirectMatching _ -> []
  DamageRewrite.Redirect recipient -> [recipient]
  DamageRewrite.RedirectNext _ recipient -> [recipient]
  DamageRewrite.PreventAll -> []
  DamageRewrite.PreventRemovingShieldCounter -> []
  DamageRewrite.PreventNext _ -> []
  DamageRewrite.PreventAllBut _ -> []
  DamageRewrite.SetAmount _ -> []
  DamageRewrite.Scale _ -> []

-- What one object on the stack refers to: its CR 113.7 source object, and every
-- object its bindings name.
referentsOfObject :: Object.Object -> [ObjectId]
referentsOfObject obj = sourceObjectOf (Object.source obj) <> referentsOfBindings (Object.bindings obj)

-- The object a Source names, and TOTAL over every arm rather than two and a
-- wildcard: a future arm that names an object owes this list a line, and `_`
-- would swallow it. Only the two on-stack ability arms name one -- CR 113.7's
-- "the object whose ability was activated" and "the object whose ability
-- triggered". The card-shaped arms ARE the object rather than naming another --
-- a melded permanent included, CR 701.42a making it one object however many
-- cards represent it -- and CR 725.2's inherent trigger has no object source at
-- all, which is the whole of what distinguishes it from OfTrigger.
sourceObjectOf :: Source.Source -> [ObjectId]
sourceObjectOf src = case src of
  Source.OfCard _ -> []
  Source.OfMeld _ -> []
  Source.OfToken _ -> []
  Source.OfAbility a -> [ActivatedAbilitySource.source a]
  Source.OfTrigger t -> [TriggeredAbilitySource.source t]
  Source.OfEmblem _ -> []
  Source.OfSpellCopy _ -> []
  Source.OfInherentTrigger _ -> []

-- Every object a binding environment names, both shapes: the one object a target
-- slot holds (CR 601.2c) and every member of a group a clause defines without
-- targeting it (CR 115.10a). Not Pawl.Engine.Binding.slotObjects, which narrows a
-- multi-target slot away through `onlyOne` -- a spell that targets two creatures
-- refers to both of them, and CR 609.7a asks for every object referred to.
-- Player recipients drop out, the rule's classes all being objects.
--
-- ONE SLOT IS DROPPED, and by NAME rather than by comparing ids: Binding.thisAbility
-- holds the activated ability's OWN id (CR 602.2a), which pawl stamps so a card can
-- read the record of the mana that paid for the activation -- not because any
-- printed text names the ability as another object. Counting it would undo CR
-- 609.7a's second class, which admits "a spell on the stack" and deliberately stops
-- short of an ability.
--
-- HERE rather than at the carriers, because two of CR 609.7a's three read bindings
-- and both were wrong: `referentsOfObject` for an ability still on the stack, and
-- Effect.ArmDelayedTrigger's captured environment for one that has already ceased,
-- which is unrestricted by design (CR 603.7c). The third carrier reads
-- ActiveReplacement.slots, already narrowed to the row's own slot names, so it
-- never sees this one unless a card writes it -- and then the card really does name
-- it. Pawl.ReplacementSpec proves both binding carriers, one case each.
referentsOfBindings :: Map.Map SlotName Binding.Type.Binding -> [ObjectId]
referentsOfBindings bindings =
  let named = Map.delete Binding.thisAbility bindings
   in foldMap (Maybe.mapMaybe Recipient.objectOf . Set.toList) (Binding.targetsOf named)
        <> foldMap Foldable.toList (Binding.groupsOf named)

-- The context every effect of a resolution evaluates its quantities and its
-- ref-borne filters in: CR 109.5's "you" is the resolving controller, the source
-- frames CR 113.7, and the resolution's slot objects ride along so a
-- Quantity.AgainstSlot can aim at one and a Filter.IsBound can ask whether a
-- candidate is among them.
--
-- Of the TARGET half, only LEGAL recipients and only OBJECT ones, and only where
-- the slot names exactly one (CR 608.2b); all three drop out as an absent key, so
-- a quantity is unanswered rather than answered off the source.
--
-- The GROUP half comes in beside `legal` rather than through it: CR 115.10a makes
-- a group a definition and never a target, so it owes CR 608.2b nothing and is
-- read live off the resolving object (slotBindings) instead. It reaches
-- Filter.IsBound whole, and the singular readers decline it
-- (Filter.slotOneObject).
--
-- The AMOUNTS ride the same live read, which is why the parameter is the whole
-- binding map rather than the groups alone: a number an earlier clause stamped
-- (bindAmountSlot) is on the resolving object exactly as a group is.
--
-- The NAMES of those same objects ride along too, which is why the parameter is a
-- GameState rather than Teams alone: this module can read a board and
-- Pawl.Engine.Filter cannot, so CR 709.4a's SameNameAsBound is answerable at a
-- resolution's positions exactly as it is at a target slot's
-- (Pawl.Engine.Target.slotContext). A THUNK, as it is there: one projection per
-- bound object, paid for only by a filter naming the atom.
effectContext :: GameState -> PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName Binding.Type.Binding -> Filter.Context
effectContext gs controller source legal bindings =
  let objects = Binding.withGroups (effectSlotObjects legal) (Binding.groupsOf bindings)
   in (Filter.contextWithSlots (Game.teams gs) (Just controller) (Just source) objects)
        { -- CR 608.2c: the numbers earlier clauses of THIS resolution stamped on
          -- slots, for the one Filter atom that compares a candidate against one
          -- (Filter.PowerIsAmountInSlot) -- Localized Destruction's "power equal to
          -- the amount of {E} paid this way". Live off the resolving object, the
          -- group half's own read, so a clause reads what the clause before it bound.
          Filter.boundAmounts = Map.mapMaybe Binding.Type.amount bindings,
          -- CR 709.4a's names off the same objects, through CR 608.2h's
          -- last-known reader for slotContext's reason: Bifurcate's bound
          -- creature may have left by the time the search runs, and "that
          -- creature" is a reference the spell already made rather than a target
          -- CR 608.2b re-checks.
          Filter.slotNames = fmap (foldMap (foldMap Filter.names . Projection.viewWithLastKnownAnywhere gs)) objects
        }

-- CR 608.2h for an entry rider's counts: the card writes a Quantity per counter
-- kind (CR 122.6, CR 107.3c -- Printlifter Ooze's "X +1/+1 counters on it, where
-- X is the number of other creatures you control"), and the funnel takes settled
-- numbers. This is the ONE bridge between the two, so the answer is determined
-- once, when the effect is applied, and nothing downstream can re-read the board.
--
-- Against the CALLER'S OWN context, which is the resolution's. Evaluating here
-- rather than at the funnel is what keeps that available: Event.createTokens and
-- Event.changeZoneEntering have no context and no view to hand, so one built
-- there would be a Filter.contextFor with an empty slot map and no source --
-- Filter.IsBound false, Filter.IsSource false and a Quantity.AgainstSlot
-- unanswered, none of which raises anything.
--
-- CR 107.2 for an unevaluable count and CR 107.1b for a negative one, both zero;
-- a zero-count kind is DROPPED rather than passed on, so CR 614.16's replacements
-- are never offered an event that places no counter.
freezeRiders ::
  (ObjectId -> Maybe Filter.View) ->
  Filter.Context ->
  GameState ->
  ObjectId ->
  ObjectId ->
  EntryRiders.EntryRiders Quantity.Type.Quantity ->
  EntryRiders.EntryRiders Natural
freezeRiders viewOf context gs resolving source riders =
  riders
    { EntryRiders.counters = Map.mapMaybe frozen (EntryRiders.counters riders)
    }
  where
    frozen quantity = case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Just n | n > 0 -> Just (Integer.toNaturalSaturating n)
      _ -> Nothing

-- The ONE object each of a resolution's TARGET slots names, shared by
-- effectContext above and effectViewOf below so the two cannot disagree about
-- which object a slot is.
effectSlotObjects :: Map.Map SlotName (Set Recipient) -> Map.Map SlotName ObjectId
effectSlotObjects = Map.mapMaybe Recipient.objectOf . Map.mapMaybe Binding.onlyOne

-- The GROUP bindings a resolution has made so far, read LIVE off the resolving
-- object (CR 608.2c): a slot an earlier clause of this same resolution defined is
-- part of the state a later one is read against, which is exactly what "from
-- among them" needs. By the name the effect wrote, as slotGroup above reads it.
slotBindings :: ObjectId -> GameState -> Map.Map SlotName Binding.Type.Binding
slotBindings resolving gs = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)

-- CR 608.2h's reader for one resolution: Projection.viewWithLastKnown, which
-- answers the SOURCE off its last known information, widened to two reserved
-- slots whose object is gone by construction.
--
-- Binding.sacrificedPermanent is the first -- CR 601.2h paid the cost before the
-- ability was on the stack at all, and CR 701.21a put the permanent in a
-- graveyard as a new object (CR 400.7) -- so viewWithLastKnown's blank answer for
-- a non-source object would leave Jarad, Golgari Lich Lord's "the sacrificed
-- creature's power" permanently unanswerable.
--
-- Binding.departedPermanent is the second, and CR 603.10a is why: what a
-- leaves-the-battlefield trigger says "it" about is the permanent as it last
-- existed on the battlefield, which CR 400.7 has already deleted by the time the
-- ability resolves. Resourceful Defense's "put those counters" reads its whole CR
-- 122.8 tally through here.
--
-- The blank is still right for every OTHER non-source id, and that is why this
-- names two slots rather than lifting the scope: those ids are TARGETS, and CR
-- 608.2b wants a target that has left to answer with nothing. Neither slot here
-- was ever a target (CR 115.10a).
effectViewOf :: ObjectId -> Map.Map SlotName (Set Recipient) -> GameState -> ObjectId -> Maybe Filter.View
effectViewOf source legal gs oid =
  let slots = effectSlotObjects legal
      lookBack slot = Map.lookup slot slots == Just oid
   in if lookBack Binding.sacrificedPermanent || lookBack Binding.departedPermanent
        then Projection.viewWithLastKnownAnywhere gs oid
        else Projection.viewWithLastKnown source gs oid

-- The amount ONE RECIPIENT of a per-player instruction reads, which need not be
-- the amount the rest of the table reads (Stronghold Discipline). Every opcode
-- naming a set of players and an amount evaluates through here, once per
-- recipient. For Effect.DealDamage the set may hold objects too, and
-- recipientSeat is what says whose an object's amount is.
--
-- Two spellings, because a card asks two different questions: Filter.Context's
-- `recipient`, which Filter.ControlledByRecipient reads (see #161); and
-- Quantity.forCandidate, which substitutes PlayerRef.Candidate. Both are no-ops
-- for a quantity naming neither, so this is no departure from CR 608.2f's single
-- determination -- every amount is read off the same pre-effect GameState.
evaluateForRecipient ::
  (ObjectId -> Maybe Filter.View) ->
  Filter.Context ->
  GameState ->
  ObjectId ->
  ObjectId ->
  PlayerId ->
  Quantity.Type.Quantity ->
  Maybe Integer
evaluateForRecipient viewOf context gs announcedOn source pid quantity =
  Quantity.evaluateFor
    viewOf
    (context {Filter.recipient = Just pid})
    gs
    announcedOn
    source
    (Quantity.forCandidate pid quantity)

-- The objects a slot holds as a BINDING: the whole GROUP an earlier effect of
-- this resolution bound, or the single object bound in its place -- the two
-- shapes every binder dispatches between on how many objects it named. Nothing
-- where the slot holds neither, which the MoveToZone InSlot gather reads as
-- "then look at the targets".
--
-- Read AHEAD of the caller's legal-target map: a group binding is a definition,
-- never a target (CR 115.10a), so it owes CR 608.2b nothing. `slotOne` and not
-- objectRefObjects, since slotOne is what lets a slot an EARLIER EFFECT OF THIS
-- SAME RESOLUTION bound name its object here; a slot the `chosen` map mentions
-- was targeted, so it is left to the target path behind CR 608.2b's
-- re-validation.
slotBoundObjects :: ObjectId -> Map.Map SlotName (Set Recipient) -> SlotName -> Game (Maybe [ObjectId])
slotBoundObjects resolving chosen slot = do
  group <- State.gets (slotGroup slot resolving)
  case group of
    -- Mint order, which CR 608.2f leaves standing.
    Just objects -> pure (Just (Foldable.toList objects))
    Nothing -> do
      bound <- if Map.member slot chosen then pure Nothing else State.gets (slotOne slot resolving)
      pure (fmap (: []) bound)

-- The members a "from among them" slot offers: the GROUP an earlier effect of
-- this resolution bound, the SINGLE object bound in its place, or -- where the
-- slot was targeted rather than defined -- every recipient CR 608.2b left legal.
-- slotBoundObjects with the last of those three added, which is the InSlot
-- gather's own read; the three callers share it so the matched half, the chosen
-- card and "the rest" cannot see different groups.
--
-- The third case is not decoration: a reveal or a look that named exactly ONE
-- card binds the singular shape, which slotBoundObjects reports only while the
-- slot is absent from `chosen` -- and the per-effect re-read of Object.bindings
-- (CR 608.2c) puts it there. Without the fallback, Commune with the Gods over a
-- one-card library would offer nothing and bury the card.
fromAmongMembers :: Map.Map SlotName (Set Recipient) -> ObjectId -> Map.Map SlotName (Set Recipient) -> SlotName -> Game [ObjectId]
fromAmongMembers legal resolving chosen slot = do
  bound <- slotBoundObjects resolving chosen slot
  pure $ case bound of
    Just objects -> objects
    Nothing -> Maybe.mapMaybe Recipient.objectOf (legalMany slot legal)

-- The members of a group that a ref's own Filter matches: the shared half of "a
-- card from among them" and "all cards from among them", so the choice one makes
-- and the sweep the other takes cannot come apart.
--
-- Matched in THIS EFFECT's context -- CR 109.5's "you" is the resolving
-- controller and not whoever is choosing, and the resolution's own slot bindings
-- ride along -- against the CR 613 projection, so a card a continuous effect
-- made a creature is a creature card here. The caller's order survives, which
-- for a group is mint order (CR 608.2f).
matchingFromAmong :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId] -> [ObjectId]
matchingFromAmong legal resolving controller source gs filter_ members =
  let context = effectContext gs controller source legal (slotBindings resolving gs)
      viewOf = Projection.viewsOf gs
   in filter (\oid -> Filter.matches context (viewOf oid) filter_) members

-- The printed "from among them", a CR 608.2d choice: the candidates are the
-- members of a GROUP an earlier clause of this resolution bound rather than a
-- zone's contents, which is the whole difference from the zone-keyed choices --
-- and the reason a batch CR 701.20a's reveal or CR 701.20e's look left in the
-- LIBRARY is reachable at all (CR 701.20b), a library's filtered sweep naming
-- every match in the whole zone rather than that handful.
--
-- ONE function rather than one per opcode, because it is ONE choice: Carth the
-- Lion's "you may reveal a planeswalker card from among them and put it into
-- your hand" reveals and moves the SAME card, so the reveal asks this and binds
-- what it showed, and the move reads that binding. Two opcodes each asking their
-- own would be two independent choices, which the printed "and" forbids.
--
-- The candidates are read from the state the instruction is reached in (CR
-- 608.2c) through slotBoundObjects, so every reader of the group sees the same
-- members. Narrowed by the ref's own Filter, matched in THIS EFFECT's context (CR
-- 109.5's "you" is the resolving controller), against the CR 613 projection -- so
-- a card a continuous effect made a creature is a creature card here.
--
-- WHO is asked is the ref's own chooser, ONE seat: CR 608.2c's resolving
-- controller by default, or the seat a PlayerRef names -- Animal Magnetism's "an
-- opponent chooses a creature card from among them", read out of the slot a
-- ChooseOpponent filled earlier in this resolution. Read through playerRefPlayers
-- so the slot is read as every other is (CR 608.2b): a reference naming nobody,
-- or naming several where no printing writes one, asks nobody and so names no
-- card (CR 101.3).
--
-- HOW MANY is the ref's Quantity, evaluated HERE (CR 608.2c) off the announcement
-- CR 601.2b left on the resolving object -- TopOfLibrary's reading, and its clamp:
-- a count that will not evaluate, or evaluates negative, is zero cards (CR
-- 107.1b). A group holding fewer matches than the count gives what it has (CR
-- 609.3) and the rest of the instruction is performed on that (CR 101.3).
--
-- ONE ask per card, each over the candidates the earlier asks have not taken --
-- CR 608.2d cannot choose the same card twice, "put two of them into your hand"
-- naming two cards. One seat answering several asks in sequence is the same
-- decision as one simultaneous choice of that many, CR 101.4c leaving the order
-- of a player's own simultaneous choices to that player. Elided at one candidate
-- and skipped at none (CR 101.3, CR 609.3). Filtered, not trusted: an answer
-- naming a card never offered falls back to the first candidate.
chooseCardFromAmong ::
  ObjectId ->
  ObjectId ->
  PlayerId ->
  Map.Map SlotName (Set Recipient) ->
  Map.Map SlotName (Set Recipient) ->
  ChosenCardFromAmong.ChosenCardFromAmong ->
  Game [ObjectId]
chooseCardFromAmong resolving source controller legal chosen (ChosenCardFromAmong.MkChosenCardFromAmong slot filter_ count chooser) = do
  members <- fromAmongMembers legal resolving chosen slot
  gs <- State.get
  let viewOf = effectViewOf source legal gs
      context = effectContext gs controller source legal (slotBindings resolving gs)
      wanted = maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source count)
      candidates = matchingFromAmong legal resolving controller source gs filter_ members
      pick asked n available
        | n <= (0 :: Natural) = pure []
        | otherwise = case available of
            [] -> pure []
            [only] -> pure [only]
            first : second : more -> do
              let offered = first NonEmpty.:| (second : more)
              answer <- Game.choose (Prompt.ChooseCardFromAmong (Decide.deciderFor asked gs) asked source offered)
              let taken = if List.elem answer (NonEmpty.toList offered) then answer else first
              rest <- pick asked (n - 1) (List.delete taken available)
              pure (taken : rest)
  case playerRefPlayers legal controller gs chooser of
    [asked] -> pick asked wanted candidates
    _ -> pure []

-- One effect, applied, wrapped in the window CR 607.2a's link is filed from:
-- what was in exile before, and what is in it after.
applyEffectWith :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
applyEffectWith runSubgame resolving source controller legal chosen effect = do
  before <- State.gets GameState.exile
  applyOneEffect runSubgame resolving source controller legal chosen effect
  State.modify' (recordExiledWith source before)
  State.modify' (recordExilePile before)

-- CR 607.2a's link, filed as the instruction that made it finishes: every card
-- that ARRIVED in exile while the effect ran is filed against that effect's
-- source.
--
-- A DIFFERENCE over GameState.exile rather than a case over the opcode: CR
-- 607.2a asks whether an ability's instruction exiled the card, never which
-- instruction, so the rules core stays off effect identity. New IDS and not new
-- cards, so an arrival cannot be confused with a card already in exile (CR
-- 400.7). The INNERMOST filing wins, which is what insertWith keeps, since
-- applyEffectWith recurses. Then RESTRICTED to what is still in exile, so the
-- map cannot grow over a game.
--
-- What insertWith ALSO keeps is CR 607.2b's link, which Pawl.Engine.Event files
-- at the move itself: a card this window saw arrive because somebody else's
-- replacement effect redirected it there was not put in exile by an instruction
-- in the resolving ability, so 607.2a does not claim it and this diff must not
-- overwrite the funnel's entry with `source`. Load-bearing, not incidental --
-- Pawl.ZoneTriggerSpec's "CR 607.2b the card Rest in Peace's replacement exiles
-- is linked to IT" is what proves it.
--
-- Filed for a SPELL's effects too, where CR 607.2a scopes the link to an
-- activated or triggered ability -- unreadable rather than wrong, since CR
-- 608.2n puts the spell into its graveyard as part of its own resolution.
recordExiledWith :: ObjectId -> Set ObjectId -> GameState -> GameState
recordExiledWith source before gs =
  let arrived = Set.difference (GameState.exile gs) before
      file oid = Map.insertWith (\_ inner -> inner) oid source
   in gs {GameState.exiledWith = Map.restrictKeys (foldr file (GameState.exiledWith gs) arrived) (GameState.exile gs)}

-- CR 406.4's separate piles, filed off the same window: "face-down cards in exile
-- should be kept in separate piles based on when they were exiled and how they
-- were exiled", so every card that arrived in exile FACE DOWN while one effect
-- ran shares one stamp, and the next effect's arrivals get another.
--
-- That window is the rule's two criteria at once, and is why the stamp is taken
-- here rather than in Pawl.Engine.Event's zone-change funnel: the funnel moves
-- one card, so a stamp minted there would put every card in a pile of its own --
-- and a pile of one is a card the chooser has effectively named, which is the
-- direction rule 406.4 exists to forbid. One EXECUTION of one instruction is
-- "when", and its being that instruction is "how".
--
-- A difference over GameState.exile and not a case over the opcode,
-- recordExiledWith's road above and for its reason: the rules core stays off
-- effect identity.
--
-- Only cards NOT already stamped, which is what makes a nested application's
-- filing stand -- applyEffectWith recurses, so the innermost window is the one
-- that ran the instruction, and the outer one must not re-pile what it saw
-- arrive. That gate is also what keeps the timestamp supply still: no stamp is
-- drawn by an effect that exiled nothing face down.
--
-- The nesting half of it is a regression fence rather than proven behaviour: no
-- card in `data/cards/` exiles face down from inside another effect, so dropping
-- the gate leaves the suite green. The supply half is proved by every board that
-- exiles nothing face down.
--
-- Then RESTRICTED to what is still in exile, recordExiledWith's sweep: CR 400.7
-- gives a card that left a new id, so the old key can never be named again.
recordExilePile :: Set ObjectId -> GameState -> GameState
recordExilePile before gs =
  let kept = Map.restrictKeys (GameState.exilePiles gs) (GameState.exile gs)
      unstamped =
        filter
          (\oid -> not (Map.member oid kept) && maybe False Object.exiledFaceDown (Game.lookupObject oid gs))
          (Set.toList (Set.difference (GameState.exile gs) before))
   in case unstamped of
        [] -> gs {GameState.exilePiles = kept}
        _ ->
          let (stamp, gs1) = Game.freshTimestamp gs
           in gs1 {GameState.exilePiles = foldr (\oid -> Map.insert oid stamp) kept unstamped}

-- CR 724.1b: how one object leaves the stack when an effect ends the turn. A card
-- or a token is EXILED, which is a zone change like any other; an ability is not
-- represented by a card, so it ceases to exist instead (Game.cease, CR 608.2n's
-- own mechanism) rather than being filed into exile as a phantom that rule
-- 724.1b's second sentence expects the next check to remove and that
-- Pawl.Engine.Sba's cease pass would not.
--
-- A COPY OF A SPELL is exiled with the cards, and rule 724.1b's second sentence
-- is why it may be: CR 704.5e removes it at the next check, which is the same
-- pass CR 704.5d makes for a token. Ceasing it here instead would be the same end
-- state reached without the zone change the rule asks for.
exileOrCease :: ObjectId -> Game ()
exileOrCease oid = do
  gs <- State.get
  case fmap Object.source (Game.lookupObject oid gs) of
    Just (Source.OfCard _) -> Event.changeZone oid Zone.Exile
    Just (Source.OfToken _) -> Event.changeZone oid Zone.Exile
    Just (Source.OfSpellCopy _) -> Event.changeZone oid Zone.Exile
    _ -> State.modify' (Game.cease oid)

-- CR 707.10: what a copy of this object's Source would be, and which of CR
-- 707.10's nouns the copy is, or Nothing when the object is neither a spell nor
-- an ability. A CLASSIFICATION -- read off the same Source that Game.isSpell and
-- Game.isAbility read -- never which card it is.
--
-- A copy of a SPELL gets a Source of its own (CR 112.1a's spell with no card),
-- while a copy of an ABILITY keeps the original's Source unchanged: CR 707.10b's
-- "a copy of an ability has the same source as the original ability", which that
-- payload already carries (the source object and the ability itself). So the
-- rule's second sentence -- an ability referring to its source by name refers to
-- that same object -- and its third -- the copy counts as the same ability for
-- effects counting resolutions -- both hold by construction rather than by
-- anything written here. Pawl.CopySpec's Longtusk Cub case proves the second.
--
-- Not implemented: the count the third sentence is about, so nothing observes
-- that half (gap #3135).
--
-- A copy of a copy answers with the copy's own printing: CR 707.2's copiable
-- values are the ones the copied object reports, and its snapshot already carries
-- them.
copyOnStackOf :: Source.Source -> Maybe (Source.Source, StackObjectKind.StackObjectKind)
copyOnStackOf source = case source of
  Source.OfCard pid -> Just (Source.OfSpellCopy pid, StackObjectKind.Spell)
  Source.OfSpellCopy pid -> Just (Source.OfSpellCopy pid, StackObjectKind.Spell)
  Source.OfAbility a -> Just (Source.OfAbility a, StackObjectKind.Ability)
  Source.OfTrigger t -> Just (Source.OfTrigger t, StackObjectKind.Ability)
  -- CR 725.2's sourceless triggered ability is a triggered ability all the same,
  -- and Pawl.Engine.Target.abilityRecipients offers it, so it copies like one.
  Source.OfInherentTrigger t -> Just (Source.OfInherentTrigger t, StackObjectKind.Ability)
  -- CR 707.10 copies what is ON THE STACK, and none of these three ever is: a
  -- melded permanent and a token are put onto the battlefield (CR 701.42a, CR
  -- 111.1) and an emblem into the command zone (CR 114.1). CR 202.3c's copy of a
  -- melded permanent is a permanent copy and does not come through here.
  Source.OfMeld _ -> Nothing
  Source.OfToken _ -> Nothing
  Source.OfEmblem _ -> Nothing

-- The TARGETS a spell or ability on the stack currently has, per slot. Every
-- recipient-valued binding restricted to the slots the object actually declares
-- a target in: Binding.targetsOf reports the reserved ones too -- CR 109.5's
-- `you` above all -- and CR 115.10b says outright that "you" indicates no
-- target. Restricted for legalSlot's own reason as well: a slot declaring no
-- target was never targeted, so CR 608.2b has nothing to re-check there.
--
-- Read off the LIVE bindings rather than off what an announcement chose, which
-- is what makes it answer for a copy whose targets CR 707.10c has just changed.
targetsOnStack :: ObjectId -> GameState -> Map.Map SlotName (Set Recipient)
targetsOnStack oid gs =
  Maybe.fromMaybe Map.empty $ do
    obj <- Game.lookupObject oid gs
    pure (Map.restrictKeys (Binding.targetsOf (Object.bindings obj)) (Map.keysSet (stackTargetSlots obj oid gs)))

-- CR 601.2c: the target slots an object on the stack declares, for whichever of
-- CR 707.10's three nouns it is. A CLASSIFICATION off the object's Source, like
-- copyOnStackOf above and never which card it is.
--
-- A card-backed object reads its printed face (targetSlotsOf). An ability has no
-- card behind it -- CR 113.7a makes it an object in its own right, and
-- Game.cardOf answers Nothing for one -- so its slots come off the modal its
-- Source carries, which is where Pawl.Engine.Activate announced them against and
-- where Pawl.Engine.Stack's two ability arms read its modes at resolution.
--
-- CR 612.1's rewrite (Projection.rewriteTargetSlot) is applied on the card-backed
-- half alone, and that is agreement rather than an omission this reader makes:
-- the two roads an ability's slots already travel -- Pawl.Engine.Activate's
-- announcement and Pawl.Engine.Stack's resolution -- read the modal unrewritten,
-- so a rewrite here would describe a slot neither of them announced. Whenever CR
-- 612 does have to reach an ability on the stack, all three move together.
stackTargetSlots :: Object.Object -> ObjectId -> GameState -> Map.Map SlotName TargetSlot.TargetSlot
stackTargetSlots obj oid gs =
  let chosen = Binding.modesOf (Object.bindings obj)
      fromFace = maybe Map.empty (targetSlotsOf obj oid gs) (Game.faceOf oid gs)
      -- CR 603.2's player slots baked in, off the object's OWN bindings -- the
      -- map Pawl.Engine.Engine.placeBorne baked with as the ability went on the
      -- stack, and which CR 707.10 copied onto a copy verbatim. Not optional:
      -- Filter.ControlledByBound answers False wherever `matches` reaches it, so
      -- an unbaked slot admits nobody, and CR 707.10c's prompt would then be
      -- elided as "settled" while the copy silently kept the original's target
      -- (Questing Beast's "that player" under Lithoform Engine).
      baked = Target.bakeModal (Binding.playerSlots (Object.bindings obj))
   in case Object.source obj of
        Source.OfAbility a -> Modal.modesTargetSlots chosen (baked (ActivatedAbility.modal (ActivatedAbilitySource.ability a)))
        Source.OfTrigger t -> Modal.modesTargetSlots chosen (baked (TriggeredAbility.modal (TriggeredAbilitySource.ability t)))
        Source.OfInherentTrigger t -> Modal.modesTargetSlots chosen (baked (TriggeredAbility.modal (InherentTriggerSource.ability t)))
        Source.OfCard _ -> fromFace
        Source.OfSpellCopy _ -> fromFace
        Source.OfMeld _ -> fromFace
        Source.OfToken _ -> fromFace
        Source.OfEmblem _ -> fromFace

-- CR 707.10d: the copies' targets, one map per candidate, in the order their
-- controller chose. Answers the empty list where nothing is copied at all.
--
-- The candidates are the card's own description ("each other creature you
-- control"), narrowed by the rule's "could target": a candidate is kept only
-- where it is a legal target for EVERY instance of the word "target" and the
-- whole assignment survives CR 601.2c's joint check, which is rule 707.10d's
-- last sentence -- "if that player or object isn't a legal target for each
-- instance of the word 'target', a copy isn't created for that player or
-- object".
--
-- A slot the original filled with TWO targets keeps nobody, and that is the
-- rule rather than a shortcut: every one of the copy's targets must be the same
-- object, and one object cannot fill two instances of "target" in one slot.
--
-- Legality is measured against the ORIGINAL, which is what "it could target"
-- names -- the copy does not exist yet -- and from the copying effect's
-- controller's seat, whose copy it will be. Rule 707.10c's own derivation, with
-- the same seed: the slots being written are dropped from the environment, so a
-- sibling read is not answered off the target it is about to replace.
--
-- The ORDER is the whole of what CR 707.10d leaves to a player, and
-- Prompt.OrderForEach is the question; the rule states no primary key, so the
-- one prompt covers the whole list rather than forEachOrder's APNAP groups.
-- Elided for fewer than two, which is one order.
copyForEachTargets :: PlayerId -> ObjectId -> ObjectId -> Map.Map SlotName (Set Recipient) -> ObjectId -> ObjectRef -> Game [Map.Map SlotName (Set Recipient)]
copyForEachTargets controller resolving source legal original candidateRef = do
  gs <- State.get
  let candidates = objectRefObjects legal resolving controller source gs candidateRef
      picks = Maybe.fromMaybe [] $ do
        obj <- Game.lookupObject original gs
        let slots = stackTargetSlots obj original gs
            current = targetsOnStack original gs
            seed = Map.withoutKeys (Object.bindings obj) (Map.keysSet slots)
            fresh = Target.legalSets (Just controller) False seed original slots gs
            takes oid slot = Set.filter ((== Just oid) . Recipient.objectOf) (Map.findWithDefault Set.empty slot fresh)
            pick oid =
              let chosen = Map.mapWithKey (\slot _ -> takes oid slot) current
               in if not (Map.null current)
                    && and (Map.elems (Map.map ((== 1) . Set.size) current))
                    && and (Map.elems (Map.map ((== 1) . Set.size) chosen))
                    && Target.jointlyCoherent (Just controller) seed original slots chosen gs
                    then Just (oid, chosen)
                    else Nothing
        pure (Maybe.mapMaybe pick candidates)
  ordered <- case picks of
    _ : _ : _ -> do
      answer <- Game.choose (Prompt.OrderForEach (Decide.deciderFor controller gs) controller resolving (fmap (Recipient.ToObject . fst) picks))
      pure (Game.permute picks answer)
    _ -> pure picks
  pure (fmap snd ordered)

-- CR 707.10c: "the player may leave any number of the targets unchanged, even if
-- those targets would be illegal. If the player chooses to change some or all of
-- the targets, the new targets must be legal."
--
-- ONE prompt, not a "may" followed by a choice: leaving every target where it is
-- is an answer to the same question, so a second prompt would ask nothing the
-- first cannot say. Prompt.ChooseTargets is the shape -- per slot, how many
-- targets and which recipients -- and this is the second place it is raised;
-- Pawl.Engine.Target.chooseTargets raises it for CR 601.2c's announcement.
--
-- The OFFERED set per slot is the recipients legal on the board NOW, unioned
-- with what the copy already targets, which is the rule's two halves: a changed
-- target must be legal, an unchanged one need not be. The count is what the
-- original announced, CR 707.10 having copied the decision and CR 707.10c
-- offering no chance to change it.
--
-- Not raised when no slot can be answered any other way -- the offered set is
-- exactly what is already chosen -- because then the options are
-- indistinguishable.
--
-- Reject-not-repair, as every other announcement is: an answer that names a slot
-- it was not offered, the wrong number of targets, or a recipient outside the
-- offered set leaves the copied targets standing rather than being patched into
-- something the player did not choose.
--
-- CR 601.2c's joint check reaches this answer as it reaches a cast's: the offer
-- above is the UNION over what a sibling slot could take, so two slots that
-- exclude each other pass `wellFormed` and Target.jointlyCoherent is what judges
-- them together. Asked on the DRAWN answer rather than the raw one, so what the
-- re-derivation reads is the card CR 406.4 picked out of a pile -- the order
-- Pawl.Engine.Cast.castProposed takes, chooseTargets drawing before it asks.
--
-- CR 733.1 is what a rejection falls to, and it is a REVERSAL rather than a
-- remedy of its own: the choice is undone, and the copy is left holding the
-- targets CR 707.10 already gave it. Not rule 707.10c's "may leave any number of
-- the targets unchanged", which is the player's option and not the rules'
-- handling of an illegal one -- the two coincide here only because undoing this
-- particular choice restores exactly that state. CR 733.2's redo has no analogue
-- inside a resolution: nobody holds priority, and a pure prompt-to-answer
-- decider re-asked would loop on a stubborn answer.
chooseNewTargetsFor :: PlayerId -> ObjectId -> Game ()
chooseNewTargetsFor controller copyId = do
  gs <- State.get
  Monad.forM_ (Game.lookupObject copyId gs) $ \copy -> do
    let slots = stackTargetSlots copy copyId gs
        -- TARGET slots only, which targetsOnStack is: the reserved slots are
        -- not targets and are not CR 707.10c's to change.
        current = targetsOnStack copyId gs
        -- CR 608.2b's own derivation, made against the CURRENT board: this is
        -- a fresh choice of targets rather than a re-check of the old one, so
        -- it reads what the board can supply now.
        --
        -- Seeded with the copy's own bindings, which is where CR 707.10 put the
        -- original's decisions: a slot's CR 202.3 computed bound reading the
        -- announced X (Stir the Grave) is answered off the SAME number the
        -- original was, because CR 707.10c changes targets and nothing else.
        --
        -- A REGRESSION FENCE rather than a proven line, and the honest reason is
        -- that the announced X reaches the bound a second way: Quantity.InSlot
        -- asks the object the evaluation names before it asks the context, and
        -- for a copy that object IS the announcement's holder. So the behaviour
        -- Pawl.CopySpec's stirCopySpec proves stands with this seed empty too,
        -- and what the seed adds is every OTHER slot atom -- Filter's
        -- SameNameAsBound and IsBound, a ZoneScope.InSlot pool -- reading a
        -- binding of the copy's that is not one of the re-chosen slots. Nothing
        -- drives that half: stirCopySpec is the only case that copies a spell
        -- whose slot reads the announcement at all, and its slot reads the X.
        --
        -- The slots being re-chosen are dropped from it. They are not decisions
        -- the copy keeps -- this call is choosing them -- so a sibling read must
        -- not be answered off the target it is about to replace, and CR 601.2c's
        -- own road seeds no target either (Pawl.Engine.Cast.castProposed). The
        -- second pass below is what relates one re-chosen slot to another.
        seed = Map.withoutKeys (Object.bindings copy) (Map.keysSet slots)
        fresh = Target.legalSets (Just controller) False seed copyId slots gs
        -- CR 406.4: what this player may not name specifically is offered as
        -- the pile it sits in, exactly as at CR 601.2c. The targets already
        -- CHOSEN are offered unchanged whatever they are, rule 707.10c letting
        -- one stand even when it is now illegal.
        offer slot recipients = (Natural.length recipients, Set.union recipients (Target.piledOffer (Just controller) gs (Map.findWithDefault Set.empty slot fresh)))
        asked = Map.mapWithKey offer current
        -- Every slot answerable only one way means the options are
        -- indistinguishable, and CR 707.10c's offer is elided.
        settled slot = Set.isSubsetOf (Target.piledOffer (Just controller) gs (Map.findWithDefault Set.empty slot fresh))
    Monad.unless (and (Map.elems (Map.mapWithKey settled current))) $ do
      answer <- Game.choose (Prompt.ChooseTargets (Decide.deciderFor controller gs) controller copyId asked)
      let admits (n, offered) picked = Natural.length picked == n && Set.isSubsetOf picked offered
          wellFormed =
            Map.keysSet answer == Map.keysSet asked
              && and (Map.elems (Map.intersectionWith admits asked answer))
      Monad.when wellFormed $ do
        -- CR 406.4's draw, run on the ANSWER: a pile the player named becomes
        -- the card randomness picked out of it before any target is recorded.
        drawn <- traverse (Target.drawFromPiles (Just controller)) answer
        -- CR 707.10c: "if the player chooses to change some or all of the
        -- targets, the new targets must be legal". Asked of the DRAWN answer
        -- and not of the raw one, because rule 406.4 draws from the whole pile
        -- -- the card it names can be one this slot refuses, and a pile is
        -- never a target that was left unchanged. An unchanged target is
        -- admitted whatever it is, which is the rule's own first sentence.
        --
        -- Pawl.ExileSpec's "CR 707.10c a copy's re-target keeps its old target
        -- when the draw names a card the slot refuses" is what proves this
        -- line: without it the copy records the illegal card and CR 608.2b
        -- counters it, where the rule leaves it resolving on its old target.
        let stands slot picked = Set.isSubsetOf picked (Set.union (Map.findWithDefault Set.empty slot current) (Map.findWithDefault Set.empty slot fresh))
        Monad.when (and (Map.elems (Map.mapWithKey stands drawn)) && Target.jointlyCoherent (Just controller) seed copyId slots drawn gs) $ do
          let write o = o {Object.bindings = Map.union (fmap Binding.toRecipients drawn) (Object.bindings o)}
          State.modify' (\g -> g {GameState.objects = Map.adjust write copyId (GameState.objects g)})

-- One effect, applied. `runSubgame` is the injected nested-game runner; only
-- the PlaySubgame arm consults it.
--
-- `controller` is the controller of the resolving spell or ability, never the
-- effect `source`, which for an ability may already have been sacrificed as a
-- cost. Every arm evaluating a Quantity views through
-- `effectViewOf source legal gs` (CR 608.2h).
--
-- `resolving` is the object ON THE STACK -- the spell, or the ABILITY object --
-- where every slot this fold defines is bound, where CR 603.7c's captured
-- environment is read back, and where CR 601.2b's announced X lives. Not
-- `source`: for an ability the two differ (CR 113.7a), and that permanent can be
-- gone before a later effect of the same list runs.
applyOneEffect :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
applyOneEffect runSubgame resolving source controller legal chosen effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts dealer excess) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- CR 120.1a: damage only to a battle, creature, or planeswalker, so every
        -- object an ObjectRef names goes through Damage.damageRecipient and none
        -- is trusted. A player recipient survives untouched (CR 115.4, CR 120.3a).
        recipientsOf ref = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
        -- WHO DEALS IT (CR 120.1): `source` is CR 113.7's default, and a `dealer`
        -- slot is CR 120.2b's exception. Resolved here and carried on the
        -- DamageEvent, so every later reader of "what dealt this" sees the
        -- redirected source. CR 608.2b applies to a dealer as to a recipient: a
        -- dealer whose target went illegal names no object and nothing is dealt.
        dealerId = case dealer of
          Nothing -> Just source
          Just slot -> Maybe.listToMaybe (objectRefObjects legal resolving controller source gs (ObjectRef.InSlot slot))
    case dealerId of
      -- No source, no damage: CR 608.2b's illegal dealer, above.
      Nothing -> pure ()
      Just dealt -> do
        -- HOW MUCH, read ONCE PER RECIPIENT off the clause that named it: Acidic
        -- Soil's "damage to each player equal to the number of lands they
        -- control" is one amount per seat, and Char's "4 damage to any target and
        -- 2 damage to you" is one amount per clause. Still one action under CR
        -- 608.2f, since every read is against the same pre-effect state. Both
        -- kinds of recipient get it, keyed to recipientSeat. An unevaluable
        -- amount drops that recipient, and so does a zero (CR 120.8).
        let amountFor quantity recipient = case recipientSeat gs recipient of
              Nothing -> Quantity.evaluateFor viewOf context gs resolving source quantity
              Just pid -> evaluateForRecipient viewOf context gs resolving source pid quantity
            -- ONE event list from EVERY clause the instruction names, so a
            -- sentence naming objects and players at once -- Molten Disaster's
            -- "each creature without flying and each player" -- reaches
            -- applyDamage as a single CR 608.2f batch rather than one per clause.
            --
            -- And one EVENT per recipient within that batch (CR 120.4), which is
            -- Damage.oneEventPerRecipient's business: two clauses can name the
            -- same recipient -- Char aimed at its own caster -- and one source
            -- dealing damage to one recipient at one moment deals one event.
            aimed =
              concatMap
                ( \part ->
                    Maybe.mapMaybe
                      ( \recipient -> do
                          n <- amountFor (DamagePart.quantity part) recipient
                          Monad.guard (n > 0)
                          pure (Damage.damageEvent gs DamageKind.Noncombat dealt recipient (Integer.toNaturalSaturating n))
                      )
                      (recipientsOf (DamagePart.ref part))
                )
                parts
            events = Damage.oneEventPerRecipient aimed
            -- CR 120.4a, and BEFORE applyDamage's CR 120.4b, against the same
            -- pre-effect state every amount was read against.
            rewritten = Damage.redirectExcess gs excess events
        Monad.unless (null rewritten) $ do
          -- ONE batch, not one call per recipient: CR 608.2f's "each such action
          -- is processed simultaneously".
          Damage.applyDamage rewritten
          -- CR 615.5's "immediately afterward": a shield this damage spent runs
          -- its additional effect inside this resolution.
          runPreventionRiders
  -- CR 701.14. Every clause of the rule is one line here, and none of them is a
  -- read of what the effect IS: the amounts come off the projection, the kind is
  -- data, and the pair guard is arithmetic on two Maybes.
  Effect.Fight (Fight.MkFight firstSlot secondSlot) -> do
    gs <- State.get
    -- CR 701.14b, and it is a guard on the PAIR: "if one or both creatures
    -- instructed to fight are no longer on the battlefield or are no longer
    -- creatures, NEITHER of them fights or deals damage". Its second sentence --
    -- an illegal target -- is legalOne's Nothing, CR 608.2b having already
    -- narrowed the slot.
    --
    -- Both powers read off the SAME pre-effect state, which is CR 701.14a's
    -- "each of those creatures deals damage equal to its power": a fight whose
    -- first blow shrank the second creature would read the wrong number.
    let fighter slot = do
          recipient <- legalOne slot legal
          oid <- Recipient.objectOf recipient
          Monad.guard (Set.member oid (GameState.battlefield gs))
          Monad.guard (Projection.isCreatureOf oid gs)
          power <- Projection.powerOf oid gs
          pure (oid, power)
    case (fighter firstSlot, fighter secondSlot) of
      (Just (oneId, onePower), Just (twoId, twoPower)) -> do
        -- CR 701.14d: "the damage dealt when a creature fights ISN'T COMBAT
        -- DAMAGE", so DamageKind.Noncombat and never the combat damage path.
        -- Each creature is its own blow's source (CR 120.2b), which is what makes
        -- an ordinary fight two dealers where an Effect.DealDamage has one -- and
        -- a self-fight one dealer again.
        --
        -- Zero is dropped rather than dealt (CR 120.8), the same guard the
        -- DealDamage arm above writes.
        let blow dealer victim amount =
              [ Damage.damageEvent gs DamageKind.Noncombat dealer (Recipient.ToCreature victim) (Integer.toNaturalSaturating amount)
              | amount > 0
              ]
            events
              -- CR 701.14c: "if a creature fights itself, it deals damage to
              -- itself equal to TWICE its power" -- ONE event, not two of its
              -- power. Every observer that sums (marked damage, CR 120.4a's
              -- excess, lifelink, CR 615.7's amount-based shield) agrees either
              -- way; one that counts events (CR 122.1c's shield counters, a
              -- damage trigger) does not, which is why the rule bothers to say
              -- it.
              --
              -- Both reads of the same power off the same pre-effect state, so
              -- doubling one is the same number as summing the two; written as
              -- the rule words it.
              | oneId == twoId = blow oneId oneId (2 * onePower)
              | otherwise = blow oneId twoId onePower <> blow twoId oneId twoPower
        -- ONE batch: CR 701.14a's "each of those creatures deals damage" is one
        -- action, so the two blows land simultaneously and a creature that dies
        -- to the first still dealt the second.
        Monad.unless (null events) (Damage.applyDamage events)
      _ -> pure ()
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    State.modify' $ \gs ->
      -- The affected objects are enumerated once, by the same sweep every
      -- ObjectRef-taking opcode uses. Nothing to affect (an illegal slot per CR
      -- 608.2b, a set that matched nothing) arrives as the empty list.
      case objectRefObjects legal resolving controller source gs ref of
        [] -> gs
        targets -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
          -- CR 611.2b: the duration never started, so nothing is stored.
          Nothing -> gs
          Just expiry ->
            -- CR 611.2c: the swept ids are frozen into the stored effect, so the
            -- set never changes afterwards. ONE effect over the whole set, which
            -- is one timestamp for CR 613.7 to order.
            --
            -- CR 608.2h / 611.2d: the VALUE is locked here too, against the
            -- SOURCE (CR 113.7a) and its controller, never an affected object.
            -- CR 601.2b's announced X is read off `resolving` instead, since
            -- only the ability object holds it. A quantity that cannot be
            -- evaluated now is undetermined for good, so nothing is stored.
            --
            -- Through effectContext and NOT Filter.contextFor, so the resolution's
            -- own slot bindings ride along: Rush of Blood's X is the power of the
            -- creature in its own target slot, and a slotless context answers
            -- Nothing and stores nothing at all.
            case Projection.freezeQuantities gs resolving source (effectContext gs controller source legal (slotBindings resolving gs)) modification of
              Nothing -> gs
              Just frozen ->
                let (ts, gs1) = Game.freshTimestamp gs
                    eff =
                      ContinuousEffect.MkContinuousEffect
                        { ContinuousEffect.source = source,
                          ContinuousEffect.timestamp = ts,
                          ContinuousEffect.expiry = expiry,
                          ContinuousEffect.modification = frozen,
                          ContinuousEffect.affected = Affected.TheseObjects (Set.fromList targets)
                        }
                 in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
  -- CR 608.2d: a subtype word swap is not among CR 601.2b-d's announcements, so
  -- the choice is made here, as the effect applies. Observable: a countered
  -- Magical Hack is never asked.
  Effect.ChangeText (ChangeText.MkChangeText family forbidden slot) -> case legalOne slot legal of
    Just recipient -> case Recipient.objectOf recipient of
      Nothing -> pure ()
      Just target -> do
        gs0 <- State.get
        let decider = Decide.deciderFor controller gs0
            -- CR 612.2: which words are offered is a CLASSIFICATION of the
            -- effect (which family), never its identity; the "can't be Wall"
            -- restriction rides in from the data as `forbidden`.
            question = case family of
              SubtypeFamily.BasicLandType -> Prompt.ChooseLandTypeSwap decider controller resolving slot forbidden
              SubtypeFamily.CreatureType -> Prompt.ChooseCreatureTypeSwap decider controller resolving slot forbidden
        (from, to) <- Game.choose question
        State.modify' $ \gs ->
          -- CR 611.2a: no stated duration, so Duration.Indefinite, armed through
          -- Expiry like the other storing arms. Indefinite always arms; the
          -- Nothing branch is written out only because arm is total.
          case Expiry.arm (Binding.playersIn legal) controller source Duration.Indefinite gs of
            Nothing -> gs
            Just expiry ->
              -- CR 611 / 612: a continuous effect over the one target, with the
              -- announced (from, to) baked in. Resolve constructs the
              -- Modification but never cases on one.
              let (ts, gs1) = Game.freshTimestamp gs
                  eff =
                    ContinuousEffect.MkContinuousEffect
                      { ContinuousEffect.source = source,
                        ContinuousEffect.timestamp = ts,
                        ContinuousEffect.expiry = expiry,
                        ContinuousEffect.modification = Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord from to),
                        ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                      }
               in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
    -- CR 608.2b: an illegal target is not affected by this part, and CR 608.2d's
    -- announcement belongs to an effect that IS applied, so nothing is asked.
    _ -> pure ()
  -- The second place mana reaches a pool (CR 106.3). A mana ability is applied by
  -- Cost.tapForMana and never resolves (CR 605.3b), but CR 605.1a/605.1b leave a
  -- triggered producer like Burning-Tree Emissary out, so it adds its mana here;
  -- see #1572.
  --
  -- CR 106.4: into the pool of the player the effect names, read through
  -- playerRefPlayers like every other slot read (CR 608.2b). The type and the CR
  -- 106.3 tags come from the ability's SOURCE, the payment path's own readers.
  -- The RETENTION (CR 106.4) and CR 106.6's two clauses -- the restriction and
  -- the rider -- are what come off the INSTRUCTION instead, stamped onto every
  -- unit it adds (CR 106.6a).
  --
  -- The RIDER's stamp here is a regression fence rather than a proven behaviour:
  -- both printings that write one (Boseiju, Who Shelters All and Delighted
  -- Halfling) are mana abilities and take the inline CR 605.3b road instead, so
  -- neutralising this line leaves the whole suite green. CR 106.6a states it
  -- anyway, which is why the line is here.
  Effect.AddMana (ManaAddition.MkManaAddition ref production retention restriction rider) -> do
    gs0 <- State.get
    case Mana.producedTypes source gs0 production of
      -- One settled type is one mana; a clause adding two writes two effects,
      -- run in printed order (CR 608.2c).
      [manaType] ->
        let unit =
              ManaUnit.MkManaUnit
                { ManaUnit.manaType = manaType,
                  ManaUnit.tags = Mana.productionTagsGiven Map.empty source gs0,
                  ManaUnit.retention = retention,
                  ManaUnit.restriction = restriction,
                  ManaUnit.rider = rider
                }
         in State.modify' (\gs -> foldr (\pid -> Mana.addMana pid [unit]) gs (playerRefPlayers legal controller gs0 ref))
      -- No type at all is CR 607.2d's "the chosen color" with nothing chosen:
      -- adding nothing is the honest answer.
      [] -> pure ()
      -- Several types is CR 105.4's choice, and it is the RECIPIENT's: CR 106.3
      -- has the effect instruct a player to add the mana, and CR 106.4 puts it in
      -- that player's pool. CR 101.4: several recipients are asked in APNAP
      -- order, with apnapOrder supplying the ORDER and the ref the MEMBERSHIP; a
      -- recipient apnapOrder does not name keeps its place at the end rather than
      -- losing the mana CR 106.4 puts in their pool.
      first : second : more ->
        let offered = first NonEmpty.:| (second : more)
            named = playerRefPlayers legal controller gs0 ref
            ordered = filter (\pid -> List.elem pid named) (Game.apnapOrder gs0)
            recipients = ordered <> filter (\pid -> List.notElem pid ordered) named
         in Monad.forM_ recipients $ \pid -> do
              gs1 <- State.get
              answer <- Game.choose (Prompt.ChooseManaType (Decide.deciderFor pid gs1) pid resolving offered)
              -- Filtered, not trusted: an answer naming a type never offered
              -- falls back to the first candidate, since the instruction is
              -- mandatory and must put mana in a pool.
              let manaType = if List.elem answer (NonEmpty.toList offered) then answer else first
                  unit =
                    ManaUnit.MkManaUnit
                      { ManaUnit.manaType = manaType,
                        ManaUnit.tags = Mana.productionTagsGiven Map.empty source gs0,
                        ManaUnit.retention = retention,
                        ManaUnit.restriction = restriction,
                        ManaUnit.rider = rider
                      }
              State.modify' (Mana.addMana pid [unit])
  Effect.Search (Search.MkSearch searcherRef ownerRef zones quantity filter_ upTo destination) ->
    -- CR 701.23a: match each candidate through its own CR 613 projection --
    -- rule 613.1 names no zone, so a card in any of the searched zones is folded
    -- exactly as a permanent is, and CR 208.2a's characteristic-defining power
    -- rides along at layer 7a.
    --
    -- Through effectContext and NOT Filter.contextFor, so the resolution's own
    -- slots ride along and a search filter naming one is answerable: Bifurcate's
    -- "with the same name as target nontoken creature" is CR 709.4a's
    -- Filter.SameNameAsBound over the slot the spell targeted, which a bare
    -- context would have answered False for on every card in the library.
    -- Pawl.ResolveSpec's Bifurcate cases are what prove it.
    --
    -- A FUNCTION of the board rather than a value, sourceChosenNames' reason
    -- below, and the slots come with it: CR 608.2c has the clauses carried out in
    -- order, so a slot an earlier clause of this same resolution bound is read
    -- here rather than as it stood when the arm was entered.
    --
    -- CR 109.5's "you" is the resolving controller, which effectContext supplies
    -- as the perspective; a search filter in the pool names no player, so no
    -- candidate's answer turns on it.
    --
    -- CR 701.3a: one candidate's VIEW carries the field no projection can fill --
    -- whether the CANDIDATE could legally be attached to the object this
    -- instruction fixes (Auratouched Mage's "an Aura card that could enchant
    -- it"), which for a search is the searching ability's own SOURCE. Answered by
    -- Attach.attachableWithLastKnown, whose live half is the same function the
    -- move goes through, so the offer and the move cannot disagree -- and whose
    -- other half is CR 608.2h: a source killed while its own trigger was on the
    -- stack is read as it MOST RECENTLY existed, so the search still finds the
    -- card that could have enchanted it, and putFound's own CR 608.2h branch is
    -- where that card then goes. Lazy, so a filter that never names
    -- Filter.CanAttachToSubject pays nothing for it.
    --
    -- No recursion to bound: this is called from a resolution rather than from
    -- inside a CR 613 fold, so the projections that question reaches start fresh.
    --
    -- CR 201.4: the ONE field the context does fill is the SOURCE's chosen names,
    -- which is what Filter.HasChosenName reads (Ancient Vendetta's "cards with
    -- that name"). A function of the board rather than a value, so the read is
    -- LIVE: CR 608.2c has the controller follow the instructions in order, and the
    -- clause that chose the name is an earlier one of this same resolution. A
    -- context built once when the arm was entered would have been the stale read.
    -- Pawl.CardSpec's "CR 201.4 no card asks HasChosenName outside a search's
    -- filter" is what keeps the atom to this position, the one that answers.
    let searchContext g = (effectContext g controller source legal (slotBindings resolving g)) {Filter.sourceChosenNames = PlayerEffect.chosenNamesOf (Just source) g}
        viewOfCandidate g oid =
          (Projection.viewOfObject oid g)
            { Filter.canAttachToSubject = Attach.attachableWithLastKnown oid source g
            }
        matches1 g oid = Filter.matches (searchContext g) (viewOfCandidate g oid) filter_
        -- CR 400.2: the library and the hand are the hidden zones. CR 701.23b,
        -- CR 701.23c and CR 701.23d are each scoped to a hidden zone, so this is
        -- what decides, PER ZONE, whether a find may be declined.
        isHidden zone = zone == Zone.Library || zone == Zone.Hand
     in do
          gs0 <- State.get
          -- Search.searcher names who searches; Search.owner names whose zones
          -- are read and whose library is shuffled. The searcher is prompted and
          -- offered CR 601.3's cast. Neither is `controller` except where a ref
          -- says so. CR 701.23i supplies the order: apnapOrder supplies the ORDER
          -- and the ref the MEMBERSHIP, for searchers and owners alike.
          --
          -- Not implemented: CR 701.23i's SIMULTANEOUS look, each searcher seeing
          -- the zones before any of them decides (#1319).
          let inApnapOrder r =
                let named = playerRefPlayers legal controller gs0 r
                 in filter (\pid -> List.elem pid named) (Game.apnapOrder gs0)
              searchers = inApnapOrder searcherRef
              -- "Each player searches THEIR library" (Jungle Wayfinder) is ONE
              -- instruction applied per player, not the cross product of two
              -- folds: the zones read are whichever searcher this pass has
              -- reached. Rule 701.23a says only how to look, so which player's
              -- zones those are comes from the card's own sentence rather than
              -- from the rule. That is Pawl.Types.PlayerRef.Candidate's reading,
              -- and the substitution is the same move Pawl.Engine.Quantity
              -- .forCandidate makes for a per-player amount. Every other ref
              -- names a set of its own, so Extract's You/InSlot pair still
              -- crosses -- one searcher over one owner.
              ownersFor searcher = case ownerRef of
                PlayerRef.Candidate -> [searcher]
                _ -> inApnapOrder ownerRef
              -- How many cards this search may find (CR 701.23a), evaluated ONCE
              -- before the loop: the card prints one instruction with one count
              -- however many zones it names, so a two-zone search caps the two
              -- together. An unevaluable or non-positive quantity comes
              -- out as 0. A search that states no count at all -- Mana Severance's
              -- "any number of land cards" -- comes out as Nothing, and is bounded
              -- by what the searched zones hold, which is CR 701.23a's "all cards
              -- in that zone".
              cap = fmap evaluateCap quantity
              evaluateCap q = case Quantity.evaluateFor (effectViewOf source legal gs0) (effectContext gs0 controller source legal (slotBindings resolving gs0)) gs0 resolving source q of
                Just n | n > 0 -> Integer.toNaturalSaturating n
                _ -> 0
          Monad.forM_ searchers $ \searcher -> Monad.forM_ (ownersFor searcher) $ \owner -> do
            -- CR 101.2: a player who can't search libraries does not, and finds
            -- nothing there. Asked BEFORE CR 601.3's offer below, which is made
            -- WHILE SEARCHING. The rest of the instruction still happens -- CR
            -- 701.23 says only how to look, so the shuffle is the card's own.
            --
            -- The prohibition names LIBRARIES (Leonin Arbiter), so it removes
            -- that one zone rather than the whole instruction: a prohibited
            -- searcher still looks through the graveyard the same card named.
            --
            -- Asked per (searcher, owner) pair and handed this resolution's own
            -- controller, because a prohibition narrows on both axes: whose
            -- library is looked through, and who controls the spell or ability
            -- causing the look (Ashiok, Dream Render). `controller` is CR
            -- 405.4's controller of the object whose instructions these are.
            prohibited <- State.gets (PlayerEffect.prohibitsSearching searcher owner controller)
            -- The printed "and/or": the searcher picks which of the zones the
            -- card names to look through -- Delivery Moogle's "your library
            -- and/or graveyard" -- and the rest of the instruction, the shuffle
            -- included, follows the CHOICE rather than the printing.
            --
            -- Filtered, not trusted, and never empty: an answer naming a zone
            -- the card did not, or naming none at all, takes every named zone.
            -- Prompt.ChooseSearchZones says why the empty answer is not the
            -- searcher's to give. Raised only where two or more zones make it a
            -- real choice.
            --
            -- Offered over the card's own zones rather than over the
            -- prohibition-filtered set just below, since CR 101.2 stops the
            -- looking and not the card's other instructions -- the shuffle among
            -- them. Read LIVE rather than off gs0: CR 601.3's cast and the finds
            -- both happen during an earlier searcher's pass, so a later pass asks
            -- over a board this resolution has already moved.
            chosenZones <-
              if Set.size zones < 2
                then pure zones
                else do
                  gsZones <- State.get
                  answer <- Game.choose (Prompt.ChooseSearchZones (Decide.deciderFor searcher gsZones) searcher zones)
                  let kept = Set.intersection answer zones
                  pure (if Set.null kept then zones else kept)
            let searchedZones = if prohibited then Set.delete Zone.Library chosenZones else chosenZones
            -- A cap of zero asks nothing and finds nothing: one legal answer is
            -- no choice to put to a player. An unbounded search has no such
            -- shortcut -- its cap is not known until the zones are read.
            found <-
              if Set.null searchedZones || cap == Just 0
                then pure []
                else do
                  -- CR 601.3 (Panglacial Wurm): the chance to cast is offered AT
                  -- THE SEARCH, not when the resolution began, so earlier
                  -- effects of the resolution have already happened. Both spells
                  -- and abilities reach here. The Wurm's "while you're searching
                  -- your library" makes the offer the SEARCHER's, only where the
                  -- library being searched is their own, and only where a LIBRARY
                  -- is among the zones being looked through.
                  --
                  -- That last conjunct is a REGRESSION FENCE rather than a
                  -- proven behaviour. It is now REACHABLE -- a searcher may take
                  -- the graveyard half of an "and/or" alone -- but observing it
                  -- needs a castable library card on the board at the same time,
                  -- which no case builds; removing it reddens nothing.
                  Monad.when (searcher == owner && Set.member Zone.Library searchedZones) (Cast.castWhileSearching performManaAbility searcher)
                  gs <- State.get
                  -- ONE prompt over the union, not one per zone: the card prints
                  -- one instruction with one count, and asking per zone would cap
                  -- per zone. The zone is kept alongside its candidates because
                  -- CR 701.23b's permission is a property of the ZONE.
                  let byZone = fmap (\zone -> (zone, filter (matches1 gs) (Game.zoneMembers zone owner gs))) (Set.toAscList searchedZones)
                      matches = concatMap snd byZone
                      -- CR 701.23a bounds an unbounded search by the zones: every
                      -- card the filter admits is findable, and no more.
                      capHere = Maybe.fromMaybe (List.genericLength matches) cap
                      decider = Decide.deciderFor searcher gs
                  answer <- Game.choose (Prompt.Search decider searcher matches capHere)
                  -- CR 701.23a: every card found is one the filter admits.
                  -- Filtered, not trusted, deduplicated, and truncated to the cap.
                  -- What a SHORT answer leaves is the difference between CR
                  -- 701.23b and CR 701.23d, and it is settled PER ZONE: CR
                  -- 701.23b, CR 701.23c and CR 701.23d each speak of a HIDDEN
                  -- zone, and CR 400.2 makes the graveyard public, so a search of
                  -- a graveyard must find what it can even where the same search
                  -- of a library need not. CR 701.23b's own Splinter example says
                  -- exactly that. So a short answer is COMPLETED from the matches
                  -- the searcher passed over in the zones that may NOT be
                  -- declined.
                  --
                  -- Only the FIRST of the three ways to decline is the rule's
                  -- and so scoped to a hidden zone: CR 701.23b's "isn't required
                  -- to find" for a filter stating a quality. Search.upTo -- a
                  -- card's own "up to" over a quality-free filter -- and a search
                  -- stating no quantity are the CARD's own permissions, printed
                  -- over the whole instruction, so they hold in a public zone
                  -- too; CR 701.23d cannot force the second either, a search
                  -- stating no quantity not being one "simply for a quantity of
                  -- cards".
                  --
                  -- Those two are REGRESSION FENCES rather than proven behaviour,
                  -- twice over. No card in the pool prints "up to" or "any number
                  -- of" over a zone that is not a library, so their zone scoping
                  -- is unobservable; and every printing of "for any number of
                  -- cards" also states a quality, so the first disjunct answers
                  -- for all of them and removing the third reddens nothing
                  -- (Scryfall o:"for any number of cards", 2026-08-23, 25 hits,
                  -- every one of them qualified -- "named X", "with the chosen
                  -- name", "that have mana value 9"). A card that said "search
                  -- your library and graveyard for any number of cards" would
                  -- refute both.
                  let mayDecline zone = upTo || Maybe.isNothing cap || (isHidden zone && Filter.statesAQuality filter_)
                      picked = List.genericTake capHere . List.nub $ filter (\oid -> List.elem oid matches) answer
                      forced = concatMap snd (filter (not . mayDecline . fst) byZone)
                      filler = filter (\oid -> List.notElem oid picked) forced
                  pure (List.genericTake capHere (picked <> filler))
            -- Where the cards go is the CARD's instruction, not rule 701.23's;
            -- CR 701.23e says the same of the reveal. The searcher is the
            -- revealer (CR 701.20a), and the cards go in the order the searcher
            -- named them.
            Monad.mapM_ (putFound searcher source destination) found
            -- The shuffle is the CARD's instruction too (CR 701.23h, CR 701.24b).
            -- The library shuffled is the one that was READ, so this seat is the
            -- owner -- and only where a LIBRARY is among the zones the searcher
            -- CHOSE. Delivery Moogle's "if you search your library this way,
            -- shuffle" is that condition printed: a searcher who took the
            -- graveyard half alone shuffles nothing.
            --
            -- Read off chosenZones rather than searchedZones, since CR 101.2's
            -- prohibition stops the looking and not the card's other
            -- instructions.
            Monad.when (Set.member Zone.Library chosenZones) $ do
              lib <- State.gets (Game.zoneMembers Zone.Library owner)
              shuffleAnswer <- Game.ask (Prompt.Shuffle lib)
              State.modify' (reorderLibrary owner (Game.honourShuffle lib shuffleAnswer))
  -- Exile every card in every graveyard (CR 400.7: each move funnels through
  -- changeZone). "Every graveyard" is CR 102.1's players still in the game, not
  -- the keys of GameState.players, which keep a departed seat's row.
  Effect.ExileAllGraveyards -> do
    gs <- State.get
    let gyCards = concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) (Game.stillPlaying gs)
    Monad.mapM_ (\c -> Event.changeZone c Zone.Exile) gyCards
  -- CR 103.5b (Serum Powder): the count is the hand size BEFORE the exile, which
  -- is why this is one opcode rather than an exile followed by a Draw. Both
  -- halves go through the usual funnels, so a short deck still loses at the first
  -- upkeep exactly as the mulligan redraw already does.
  Effect.ExileHandThenDraw -> do
    gs <- State.get
    let handIds = Game.zoneMembers Zone.Hand controller gs
    Monad.mapM_ (\oid -> Event.changeZone oid Zone.Exile) handIds
    Monad.replicateM_ (length handIds) (Event.drawCard controller)
  -- CR 727.1/727.1a: restart the game, with this ability's controller as the new
  -- starting player; the rebuild lives in Setup, reached through a generic opcode
  -- rather than Karn's identity. CR 727.4: this resolves several frames deep, and
  -- the rebuild replaces the game those frames are running, so
  -- GameState.restartSignal is how they unwind. CR 727.5: the exempted cards are
  -- swept BEFORE the rebuild, the only state in which the exemption can be read;
  -- putting them back is a separate effect of the same ability.
  --
  -- Not implemented: CR 727.6's restarted SUBGAME (#1628).
  Effect.RestartGame exempt -> do
    gs <- State.get
    let exempted = case exempt of
          Nothing -> Set.empty
          Just ref -> Set.fromList (objectRefObjects legal resolving controller source gs ref)
    Setup.restartGame performHandAction exempted controller
  -- CR 729.1/729.5: run the nested game to completion, then bind its outcome.
  --
  -- CR 729.1b: what the main game may read is the subgame's WINNER, so the slot
  -- holds a winner and a Drawn subgame binds nothing -- Shahrazad's "each player
  -- who doesn't win" is PlayerRef.EachPlayerExcept over that slot, and gets the
  -- drawn case for free. The roster the complement is taken against is
  -- Game.stillPlaying's, the set Setup.subgameStateFrom seated.
  --
  -- The winner is bound onto `resolving` -- the object ON THE STACK -- and not
  -- onto `source`, because that is what the next effect's re-read looks at: both
  -- resolveSpellWith and resolveModesWith re-read the stack object's bindings
  -- before each effect. The two coincide for a spell, which resolveSpellWith
  -- passes as both, and differ for an ability, whose source is the permanent it
  -- came from (CR 113.7) -- so binding onto `source` left an ability's follow-on
  -- reading an unbound slot; see #137.
  --
  -- That object may not survive the subgame it started: CR 729.4 offers the main
  -- game's stack to a wish cast inside, so the subgame can take this very spell's
  -- card. bindPlayerSlot files the winner off-object when that has happened and
  -- liveBindings reads it back, which is CR 729.5's "finishes resolving, even if
  -- it was created by a spell card that's no longer on the stack".
  Effect.PlaySubgame slot -> do
    result <- runSubgame
    case result of
      Result.Won winner -> State.modify' (bindPlayerSlot resolving slot winner)
      Result.Drawn -> pure ()
  -- CR 608.2d: "choose an opponent", announced as this effect is applied and
  -- bound so the sentence after it can say "that player". NOT A TARGET (CR
  -- 115.10a), so nothing was announced at CR 601.2c and the pick is made here,
  -- against the board as this effect runs.
  --
  -- The opponents are Game.stillPlaying's, so a seat that has left (CR 104.3a) is
  -- not offered; CR 102.2 leaves a two-player game nothing to decide. An answer
  -- naming somebody never offered falls back to the first candidate, since the
  -- instruction is mandatory. No opponent at all binds nothing, so the following
  -- sentence names no player and does nothing (CR 101.3).
  Effect.ChooseOpponent slot -> do
    gs <- State.get
    let opponents = Game.opponentsOf controller gs
    chosenOpponent <- case opponents of
      [] -> pure Nothing
      [sole] -> pure (Just sole)
      first : second : rest -> do
        let offered = first NonEmpty.:| (second : rest)
        answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor controller gs) controller source offered)
        pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
    Monad.forM_ chosenOpponent $ \pid -> State.modify' (bindPlayerSlot resolving slot pid)
  -- ChooseOpponent's twin with the decision replaced by randomness (Ruhan of the
  -- Fomori): the same offer, the same filter, the same bind, and the same CR
  -- 608.2d moment -- only the question changes. CR 701.9b's distinction between
  -- "at random" and "the player chooses" is why it is a separate opcode and a
  -- separate prompt.
  --
  -- Game.ask and not Game.choose, since randomness is not CR 104.4b's optional
  -- action. The question goes to the INTERPRETER: the engine does not roll and no
  -- player picks. Filtered rather than trusted, so an answer naming somebody
  -- never offered falls back to the first candidate, the instruction being
  -- mandatory.
  Effect.ChooseOpponentAtRandom slot -> do
    gs <- State.get
    let opponents = Game.opponentsOf controller gs
    chosenOpponent <- case opponents of
      [] -> pure Nothing
      [sole] -> pure (Just sole)
      first : second : rest -> do
        let offered = first NonEmpty.:| (second : rest)
        answer <- Game.ask (Prompt.RandomOpponent offered)
        pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
    Monad.forM_ chosenOpponent $ \pid -> State.modify' (bindPlayerSlot resolving slot pid)
  -- CR 706.1: roll a die of the stated kind, and bind CR 706.4's result at the
  -- slot for a later effect of this same resolution to read (Ancient Copper
  -- Dragon's "roll a d20. You create a number of Treasure tokens equal to the
  -- result").
  --
  -- Bound on `source`, which is bindAmountSlot's own contract (CR 608.2h aims an
  -- amount read at the effect's source) and the holder Destroy's "destroyed this
  -- way" tally already uses. Unobservable on THIS producer, and the DiceSpec case
  -- below does not prove it: Quantity.InSlot reads the source first and then the
  -- object CR 603.3 put on the stack, which for a triggered ability is
  -- `resolving`, so either holder answers here. Consistency, not a behaviour.
  --
  -- Game.ask and not Game.choose, since a die result is not CR 104.4b's optional
  -- action: nobody is deciding, so there is nothing to usurp. The question goes
  -- to the INTERPRETER -- the engine never rolls. Filtered rather than trusted,
  -- against CR 706.1a's outcomes "numbered from 1 to N", BOTH ends included, so
  -- an answer outside the range leaves the floor standing rather than a value no
  -- die could show; the instruction is mandatory, so there is no third option.
  --
  -- CR 706.2: the natural result is the face, and the instruction's own modifier
  -- is added to it to give the RESULT, which is what this binds (Diviner's
  -- Portent). ORDER IS LOAD-BEARING: CR 706.1a bounds the face at 1..N and no
  -- rule bounds the sum, so the filter runs on the face and the modifier is added
  -- after -- a d20 answered 20 with a modifier of 5 is a result of 25, past the
  -- die's own top face. CR 107.1b for a sum a negative modifier drove below zero.
  --
  -- Not implemented: a binding for the natural result, CR 706.2a's costed
  -- modifier and CR 706.2b's ordering among competing modifiers (#2083); with one
  -- mandatory, free modifier there is nothing to order and no second reader.
  --
  -- CR 706.1's roll is also the event TriggerCondition.PlayerRollsDice watches
  -- (Feywild Trickster). Recorded under `controller`, not `source`: rule 706.1's
  -- instruction is aimed at a PLAYER, RollDie names none of its own, and an
  -- unnamed player on a resolving object is CR 109.5's "you" -- its controller. A
  -- card telling ANOTHER player to roll would put the seat on Pawl.Types.RollDie.
  --
  -- One writer, one road: Prompt.RollDie is asked from this arm and from no
  -- other place in the engine, so there is no second road to record on.
  --
  -- Recorded AFTER the binding, so a trigger placed by CR 603.3 sees the same
  -- state a later effect of this resolution would. Nothing observes the order --
  -- the trigger goes on the stack only once this resolution finishes.
  --
  -- Not implemented: CR 706.1's other half, how MANY dice (#2085), so one entry
  -- here is one roll instruction.
  Effect.RollDie rollDie -> do
    let sides = RollDie.sides rollDie
    rolled <- Game.ask (Prompt.RollDie sides)
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- CR 706.2, read AFTER the roll as the rule words it. CR 107.2's posture
        -- for a modifier that cannot be evaluated: no modifier at all.
        modifier = case RollDie.modifier rollDie of
          Nothing -> 0
          Just quantity -> Maybe.fromMaybe 0 (Quantity.evaluateFor viewOf context gs resolving source quantity)
        natural = if rolled >= 1 && rolled <= sides then rolled else 1
        result = Integer.toNaturalSaturating (toInteger natural + modifier)
    State.modify' (bindAmountSlot source (RollDie.slot rollDie) result)
    State.modify' (Event.recordEvent (GameEvent.DiceRolled controller))
  -- CR 705.1's flip, in RollDie's holder and for its reason: bindAmountSlot's
  -- `source` is the resolving object, and on a SPELL -- which Winter Sky is --
  -- `source` and `resolving` are the same object, so the ambiguity the arm above
  -- documents does not arise there. A triggered ability that flipped would land
  -- on that arm's reasoning instead, which says either holder answers.
  --
  -- ONE INSTRUCTION, however many coins (CR 705.1, CR 705.2): neither rule scopes
  -- itself to a single coin, and Flock of Rabid Sheep's "flip X coins" is one
  -- instruction. The slot binds a TALLY over them -- how many flips the flipper
  -- won, or how many coins came up heads, per Pawl.Types.CoinReading -- which on
  -- the one-coin instruction Winter Sky prints is the 1 or 0 it always was.
  --
  -- No filtering back, unlike RollDie: CR 705.1's coin has exactly two sides and
  -- Pawl.Types.CoinFace has exactly two constructors, so every answer is in
  -- range.
  --
  -- The flipper is `controller`, CR 109.5's "you" on a resolving object, and CR
  -- 705.2's last sentence keeps everyone else out of it -- so the PlayerId on
  -- the call is the same seat and no opponent is ever asked.
  --
  -- CR 705.1's flip is also the event TriggerCondition.PlayerWinsCoinFlip
  -- watches (Tavern Scoundrel), recorded under `controller` for the reason the
  -- roll above gives: the instruction is aimed at a player, Pawl.Types.FlipCoin
  -- names none of its own, and CR 705.2's last sentence keeps every other seat
  -- out of it. EVERY coin is recorded, won or lost -- Pawl.Types.CoinFlipped
  -- says why the outcome is a field rather than the presence of an entry -- and
  -- one entry per coin, since the event is the log of flips rather than of
  -- instructions.
  --
  -- TWO WRITERS, two roads. Pawl.Engine.Coin is also called by
  -- Pawl.Engine.Event's ChoiceByCoinFlip arm, the flip made as a permanent
  -- enters (Molten Sentry), which records its own CoinFlipped there. Every road
  -- that flips records, which is what keeps this event the log of CR 705.1 flips
  -- rather than the log of one opcode.
  --
  -- Recorded per coin and so BEFORE the binding, where the roll above records
  -- after it: the tally is not settled until the last coin, and nothing observes
  -- the order -- a trigger CR 603.3 places goes on the stack only once this
  -- resolution finishes.
  --
  -- CR 705.3's second clause is the `stated` half: an effect may state that this
  -- player WINS the flip, and then the call and the face are both ignored
  -- ("ignore the actual results of that flip and use the indicated results
  -- instead"). Its first clause -- the stated FACE -- is applied inside
  -- Pawl.Engine.Coin, before the comparison here sees the face at all, which is
  -- why a statement of heads alone still loses this flip against a call of
  -- tails.
  Effect.FlipCoin flipCoin -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- How many coins the instruction flips, read ONCE before the first of
        -- them: the number is part of the instruction, and re-reading it per coin
        -- would let a count over the board flip a different number than the
        -- instruction named. CR 107.2's posture for a quantity that cannot be
        -- evaluated: no coins at all.
        coins = Integer.toNaturalSaturating (Maybe.fromMaybe 0 (Quantity.evaluateFor viewOf context gs resolving source (FlipCoin.count flipCoin)))
    -- CR 705.3's statements, read once for the whole instruction rather than per
    -- coin -- Pawl.Engine.Coin.statementsFor says why Edgar's "the first time you
    -- flip one or more coins each turn" makes that the difference.
    statements <- Coin.statementsFor (Just controller)
    let flipOnce acc _ = do
          hit <- case FlipCoin.reading flipCoin of
            -- CR 705.2's win/lose flip. TWO questions, in the rule's own order.
            -- The CALL comes first and through Game.choose, because it is a
            -- choice: the flipping player weighs heads against tails, and CR 723
            -- lets a controller make it for them. The FACE comes second and
            -- through Game.ask, because it is not: nobody decides how a coin
            -- lands, so there is nothing to usurp and the question goes to the
            -- INTERPRETER. Asking in the other order would let the call be made
            -- with the face already known, which is a different game. No board
            -- reaches the difference: both orders leave the same slot bound, so
            -- Pawl.CoinSpec's "only the flipping player calls" case proves the
            -- order by what the engine ASKED rather than by anything the board
            -- shows.
            --
            -- The decider is re-read per coin rather than off the snapshot above,
            -- since CR 723.1's control could in principle change between two
            -- coins of one instruction.
            CoinReading.Wins -> do
              gsNow <- State.get
              called <- Game.choose (Prompt.CallCoin (Decide.deciderFor controller gsNow) controller)
              (face, stated) <- Coin.flipOne statements
              let matched = stated || face == called
              State.modify'
                ( Event.recordEvent
                    ( GameEvent.CoinFlipped
                        CoinFlipped.MkCoinFlipped
                          { CoinFlipped.flipper = controller,
                            -- CR 705.2's win or loss, which this kind of flip
                            -- always has -- the caller made the call.
                            CoinFlipped.won = Just matched
                          }
                    )
                )
              pure matched
            -- CR 705.2's first sentence: the effect cares only about the face, so
            -- no Prompt.CallCoin is asked -- "no player wins or loses a coin flip
            -- for this kind of effect", so there is no call to make and nothing
            -- for a CR 723 controller to usurp. The flip is recorded with no
            -- outcome, which is what keeps rule 705.2's first sentence honest
            -- against TriggerCondition.PlayerWinsCoinFlip; see
            -- Pawl.Types.CoinFlipped. CR 705.3's second clause is the exception
            -- the rule itself names, Pawl.Engine.Event's entry road verbatim: an
            -- effect may state that a player WINS a flip that would ordinarily
            -- have no winner.
            CoinReading.Heads -> do
              (face, stated) <- Coin.flipOne statements
              State.modify'
                ( Event.recordEvent
                    ( GameEvent.CoinFlipped
                        CoinFlipped.MkCoinFlipped
                          { CoinFlipped.flipper = controller,
                            CoinFlipped.won = if stated then Just True else Nothing
                          }
                    )
                )
              pure (face == CoinFace.Heads)
          pure (if hit then acc + 1 else acc)
    -- Bound AFTER every coin, since CR 705.2 asks how many of the flips matched
    -- and one instruction's flips are all of them.
    tally <- Foldable.foldlM flipOnce (0 :: Natural) [1 .. coins]
    State.modify' (bindAmountSlot source (FlipCoin.slot flipCoin) tally)
    -- The flips that did NOT match, off the same coins. WHICH sentence of CR
    -- 705.2 that is depends on the reading: for a win/lose flip it is
    -- "otherwise, the player loses the flip", and for one that cares only about
    -- the face it is the first sentence's tails, where no player wins or loses
    -- at all. Either way it is the complement of the tally above, so the two are
    -- bound from one set of flips rather than one being re-derived from the
    -- count. Pawl.CoinSpec's Mutalith Vortex Beast group proves a lost flip
    -- reaches the loser and a won one does not.
    Foldable.for_ (FlipCoin.misses flipCoin) $ \misses ->
      State.modify' (bindAmountSlot source misses (coins - tally))
  Effect.ControlPlayerNextTurn slot ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just (Recipient.ToPlayer target) ->
          -- CR 723.1: schedule control of `target` by this ability's controller
          -- (CR 723.5). Map.insert overwrites a prior pending control (CR 723.1a).
          gs {GameState.pendingControl = Map.insert target (Decider.MkDecider controller) (GameState.pendingControl gs)}
        -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
        _ -> gs
  Effect.Destroy (Destroy.MkDestroy ref regenerability mSlot mBuried mPermanents) -> do
    gs <- State.get
    -- CR 701.8: destroy them through the single funnel -- indestructible (CR
    -- 702.12b) and regeneration (CR 701.19a) are Event.destroy's to decide, and
    -- the card's own CR 701.19c rider rides along. ONE batch, not one call per
    -- victim (CR 608.2f). An illegal slot (CR 608.2b), a non-object recipient, or
    -- a set that matched nothing all arrive here as the empty list.
    destroyed <- Event.destroyReturning regenerability (objectRefObjects legal resolving controller source gs ref)
    -- CR 701.8b: "destroyed this way" is what the funnel DESTROYED, never what
    -- the sweep named. Bound onto this effect's SOURCE so a later effect of the
    -- same resolution reads it as Quantity.InSlot, through live GameState rather
    -- than through `chosen`, which carries recipients rather than amounts. Bound
    -- even when nothing was destroyed: zero is an answer, where an unbound slot
    -- would make the rider's quantity unevaluable instead.
    Monad.forM_ mSlot $ \slot ->
      State.modify' (bindAmountSlot source slot (Natural.length destroyed))
    -- The other reading of the same printed phrase: the CARDS "put into a
    -- graveyard this way", for Come Back Wrong's "return it to the battlefield"
    -- to name. Bound onto `resolving` and as a GROUP, which is where and how
    -- MoveToZone binds its own arrivals (CR 400.7j) and the only shape every
    -- reader of a slot a destruction defines can see: slotGroup reads live
    -- GameState unconditionally, where a single binding would have to survive
    -- `chosen`.
    --
    -- Two filters, both read off the board AFTER the funnel rather than off the
    -- funnel's answer, because the answer names the incarnation and not where it
    -- ended up:
    --
    --   * in a GRAVEYARD. A CR 614 replacement may send the destroyed permanent
    --     to exile instead (Rest in Peace), and then nothing was put into a
    --     graveyard this way. A cancelled move (CR 616.1) has no incarnation at
    --     all and is already Nothing.
    --   * a CARD. CR 111.6 says a token is not a card, and every printed reader
    --     of this slot says "card" -- "if a creature CARD is put into a graveyard
    --     this way", "return each CARD put into a graveyard this way". A second
    --     rule would refuse the same token one step later -- CR 111.8, which
    --     Pawl.Engine.Event's zone-change funnel states -- so this filter is what
    --     the CARD says rather than the only thing standing in the way.
    --
    -- Nothing is bound when nothing qualifies, MoveToZone's rule: no slot names
    -- an empty set, so the later clause finds an unbound slot and does nothing --
    -- which is exactly what "IF a creature card is put into a graveyard this way"
    -- asks for.
    --
    -- EVERY arrival of each destruction, not just the first: CR 712.21c gives an
    -- effect that finds what a melded permanent becomes both cards, and CR
    -- 712.21e counts a melded permanent as two CARDS put into a graveyard where
    -- the amount slot above counts it as one OBJECT destroyed.
    Monad.forM_ mBuried $ \slot -> do
      after <- State.get
      let buriedCards = concatMap (filter (\oid -> isCardInAGraveyard oid after) . Foldable.toList . snd) destroyed
      Monad.unless (null buriedCards) (State.modify' (bindObjectsSlot resolving slot (Seq.fromList buriedCards)))
    -- The THIRD reading, and the only one that keeps a controller: the PERMANENTS
    -- CR 701.8 destroyed, under the ids they held on the battlefield, for
    -- Effect.ForEach to walk one at a time -- Rampage of the Clans' "for each
    -- permanent destroyed this way, its controller creates a 3/3 green Centaur
    -- creature token". `fst`, not `snd`: the incarnation the graveyard move
    -- minted is a different object (CR 400.7), and a card in a graveyard has no
    -- controller (CR 108.4) so CR 108.4a would answer with its OWNER -- a
    -- different player for anything that was stolen. Only these ids answer "its
    -- controller", through CR 608.2h's last known information.
    --
    -- The funnel's own answer and not the sweep (CR 701.8b): an indestructible
    -- permanent (CR 702.12b) and a regenerated one (CR 701.8c) were not
    -- destroyed, so neither is walked. No board filter of `buried`'s kind: what
    -- the rider asks about is the destruction, not where the remains landed, so a
    -- CR 614 replacement sending the card to exile changes nothing here.
    --
    -- Bound onto `resolving` as a GROUP, `buried`'s shape and for its reason:
    -- slotGroup reads live GameState unconditionally. Nothing is bound when
    -- nothing was destroyed, so the loop finds an unbound slot and runs no
    -- iterations (CR 101.3).
    Monad.forM_ mPermanents $ \slot ->
      Monad.unless (null destroyed) (State.modify' (bindObjectsSlot resolving slot (Seq.fromList (fmap fst destroyed))))
  Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect ref sacrificer) -> do
    -- CR 701.21 through the single funnel, which is NOT Event.destroy (CR
    -- 701.21a): a sacrifice is not a destruction, so indestructible (CR 702.12b)
    -- and a regeneration shield leave it alone.
    --
    -- The victims come from objectRefObjects like every other ObjectRef reader,
    -- which is what lets Golgothian Sylex's EachMatching sweep the battlefield.
    -- Its InSlot arm keeps what a bare SlotName did: a slot a Create bound to a
    -- GROUP names every token at once, in mint order, ahead of the target read
    -- and owing CR 608.2b nothing; an illegal slot (CR 608.2b) and a player
    -- recipient both arrive here as the empty list. CR 603.7c's zone check
    -- applies per MEMBER rather than to the whole word: a member that is gone is
    -- simply not affected, and the rest still are.
    --
    -- One at a time rather than as one event (#757).
    gs <- State.get
    Monad.mapM_ (sacrificerFor sacrificer controller) (objectRefObjects legal resolving controller source gs ref)
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown ref listed) -> do
    -- CR 708.2: ONE assignment to Object.facing per victim is the whole effect.
    -- What each permanent becomes is the list the effect carries (CR 708.2a's
    -- 2/2 when it lists nothing); the rule calls those copiable values, so this
    -- is a copiable swap rather than a CR 613 layer, performed by Game.faceOf.
    -- FaceDownReason.TurnedFaceDown is CR 708.6's other half: it closes CR
    -- 701.40b's turn-face-up procedure and leaves CR 702.37e's open. No CR 400.7
    -- incarnation is minted, so the object id, marked damage, counters,
    -- attachments and statuses all ride through -- the mirror of
    -- FaceDown.performTurnFaceUp.
    --
    -- The TIMESTAMP does NOT ride through, and it is the one thing on that list
    -- that does not: CR 613.7f gives a permanent a new one each time it turns
    -- face down. Written by Game.turnFacing, the primitive the turn-face-up road
    -- shares, so one rule has one writer.
    --
    -- CR 708.2b is the guard below: an effect that LISTS its own values would
    -- otherwise overwrite the list already there. No event is recorded, so
    -- nothing triggers on the turning-over (#984).
    --
    -- The victims are enumerated ONCE (CR 608.2f), as RemoveFromCombat's fold
    -- below does; an illegal slot (CR 608.2b), a player recipient and an empty
    -- match all turn nothing over. CR 608.2f's own secondary sentence never
    -- engages -- turning A face down cannot change whether B may be, so the
    -- turnings are processed simultaneously -- and what IS asked below is a
    -- different question: which of the simultaneous CR 613.7f stamps is earlier.
    --
    -- A permanent already face down is turned no further, read off the gather's
    -- board rather than the fold's; the two agree, turning one permanent face
    -- down leaving another's facing alone.
    --
    -- LEFT fold, where RemoveFromCombat's is a right one, and the stamp above is
    -- why: the fold hands out timestamps, so it must run `ordered` forwards for
    -- the earlier stamp to go to the permanent CR 613.7m put first.
    gs0 <- State.get
    let alreadyDown target = maybe False (Facing.isFaceDown . Object.facing) (Map.lookup target (GameState.objects gs0))
        faceUp = filter (not . alreadyDown) (objectRefObjects legal resolving controller source gs0 ref)
    ordered <- Restamp.order faceUp
    State.modify'
      ( \gs ->
          List.foldl'
            (flip (Game.turnFacing (Facing.FaceDown FaceDownState.MkFaceDownState {FaceDownState.reason = FaceDownReason.TurnedFaceDown, FaceDownState.listed = listed})))
            gs
            ordered
      )
  -- CR 708 through FaceDown.turnFaceUpByEffect, the funnel CR 116.2b's special
  -- action shares: this arm decides only WHICH permanent, never what turning it
  -- over does. CR 701.40g lives inside that funnel and so applies here without
  -- this arm knowing the rule exists.
  Effect.TurnFaceUp slot -> case legalOne slot legal of
    Just recipient -> case Recipient.objectOf recipient of
      Nothing -> pure () -- a player is not a permanent and has no face
      Just target -> FaceDown.turnFaceUpByEffect target
    -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
    _ -> pure ()
  Effect.RemoveFromCombat ref ->
    State.modify' $ \gs ->
      -- CR 506.4: through Game.removeFromCombat, the one performer of every
      -- clause of that rule, so CR 509.1h's asymmetry comes along for free.
      -- Unprompted and undirected: the rule leaves nothing to ask.
      --
      -- The victims are enumerated ONCE (CR 608.2f), as Untap's fold above does;
      -- an illegal slot (CR 608.2b), a player recipient and an empty match all
      -- remove nothing, and a permanent already out of combat needs no guard.
      foldr Game.removeFromCombat gs (objectRefObjects legal resolving controller source gs ref)
  Effect.BecomesBlocked slot ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> gs -- a player recipient is not in combat
          -- CR 509.1h: through Combat.becomeBlocked, which owns every write of
          -- the blocked status and carries CR 509.3c's event. Unprompted and
          -- undirected: no creature blocks, so there is nothing to ask.
          Just target -> Combat.becomeBlocked target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone entry mSlot _ placement duration) ->
    -- ONE object through CR 400.7's funnel, shared by the two arms below.
    let -- CR 400.7: the funnel mints a new incarnation in `zone`, owner-relative
        -- (CR 400.3). The riders (CR 110.5b), the library position (CR 401.2) and
        -- the controller (CR 110.2a) are handed to it rather than written
        -- afterward, so CR 614.1c's entry loop and the Moved snapshot read the
        -- settled values. No CR 800.4b guard: a departed player controls no
        -- resolving spell or ability (CR 800.4a, CR 800.4d). Answers the fold's
        -- state: the ids that have arrived so far, and the incarnations in
        -- reverse -- Nothing for a member that did not arrive.
        --
        -- CR 608.2f makes the whole sweep ONE event, so each member is judged
        -- against `before` -- the board the batch began on -- and against
        -- `sofar`, the members that have already arrived out of it. Threading a
        -- fold rather than a mapM is what carries those two: the funnel processes
        -- one member at a time, which is an implementation order CR 608.2f gives
        -- nobody the right to observe, and the two values are what keep it
        -- unobservable. `sofar` ACCUMULATES rather than naming every target up
        -- front because a member still in its old zone is not on the battlefield
        -- for a sweep to find anyway; only an arrived one needs excluding.
        --
        -- `sofar` is the half a test separates: Pawl.AuraSpec's returned Aura and
        -- Pawl.CopySpec's reanimated Clone both fail without it. `before` is
        -- threaded because CR 614.4 asks which effects existed before the BATCH,
        -- but nothing in data/cards separates it from Nothing. Every
        -- ReplacementEffect.ZoneChangeR there names a `whenDestination` of
        -- Graveyard or Stack (rest-in-peace, leyline-of-the-void,
        -- anafenza-the-foremost, yawgmoths-will, nexus-of-fate,
        -- dire-fleet-daredevil, synthetic-stack-interdiction),
        -- which no member of a batch moving ONTO THE BATTLEFIELD goes to, or the
        -- Battlefield under a Filter.IsInZone Stack conjunct
        -- (synthetic-entry-interdiction), which no batch member arriving from a
        -- graveyard or a hand satisfies.
        -- The engine installs one row naming NO destination -- CR 702.34a's
        -- exile, Pawl.Engine.Keyword.castFromGraveyardExile -- which admits the
        -- battlefield too. It separates nothing either: the three rules that
        -- install it arm it onto an INSTANT OR SORCERY spell alone (CR 702.34a
        -- and CR 702.133a say so outright; CR 702.127a's aftermath is printed on
        -- a split card's instant and sorcery halves), and no such spell is a
        -- member of a batch entering the battlefield. A card whose ZoneChangeR
        -- watched the battlefield and could match a member of such a batch would
        -- separate them.
        moveOne mBlocked frozen before (sofar, acc) (target, position) = do
          mNew <- Event.changeZoneEnteringIn (Just before) sofar target zone position frozen (Just controller)
          -- CR 614.6: the move was cancelled, or the id was already gone (CR
          -- 603.7c). Nothing entered, so there is nothing to bind.
          Monad.forM_ mNew $ \newId -> do
            -- CR 508.4, via Pawl.Engine.Combat -- which is also what keeps this
            -- from looking like a declaration, so CR 508.3a's attack triggers see
            -- nothing. CR 506.3b refuses a controller who is not the active
            -- player, which the funnel above has already settled.
            Monad.when (EntryRiders.attacking entry) (Combat.putOntoBattlefieldAttacking newId)
            -- CR 509.4, the blocking twin one rule over, through the same
            -- Pawl.Engine.Combat function the Create arm hands its tokens to, so
            -- CR 506.3e and CR 509.4a's two no-op conditions are decided in one
            -- place whichever opcode put the creature there. The attacker was
            -- read ONCE ahead of this fold (see mBlocked below), so CR 608.2f's
            -- single event cannot see it move between members.
            Monad.forM_ mBlocked (Combat.putOntoBattlefieldBlocking newId)
            -- CR 610.3: a move with a duration is only half of a pair, so the
            -- incarnation that arrived is registered against the source whose
            -- leaving the battlefield ends it, and against the zone it came from
            -- (rule 610.3's "its previous zone"). Pawl.Engine.MoveDuration is where
            -- the second one-shot effect that reads this happens; nothing a card
            -- prints performs it. Per ARRIVAL, which is CR 712.21c: a melded
            -- permanent leaves as two cards and both come back.
            let watched = do
                  Monad.guard (duration == Just MoveDuration.Type.UntilSourceLeavesTheBattlefield)
                  fmap Object.zone (Game.lookupObject target before)
            Monad.forM_ watched $ \from ->
              State.modify' (\g -> g {GameState.movedUntilSourceLeaves = Map.insert newId (ReturnWatch.MkReturnWatch {ReturnWatch.source = source, ReturnWatch.zone = from}) (GameState.movedUntilSourceLeaves g)})
          pure (foldr Set.insert sofar mNew, mNew : acc)
        -- The context a CHOICE's candidates are filtered in, off the board the
        -- choice is being made on: the resolution's own slots ride along, so a
        -- card offering "a permanent card from among them" (Midnight Tilling)
        -- offers only what an earlier clause of this resolution named.
        chooseContext g = effectContext g controller source legal (slotBindings resolving g)
        -- CR 608.2d's singular choice, shared by the two arms that make it, so a
        -- card cannot find "a creature named Hanweir Garrison" offered one way
        -- when the source rides along and another way when it does not.
        chosenPermanent filter_ = do
          gs <- State.get
          case battlefieldMatching legal resolving controller source gs filter_ of
            [] -> pure []
            [only] -> pure [only]
            first : second : more -> do
              let offered = first NonEmpty.:| (second : more)
              answer <- Game.choose (Prompt.ChoosePermanent (Decide.deciderFor controller gs) controller source offered)
              pure [if List.elem answer (NonEmpty.toList offered) then answer else first]
        -- CR 400.7j: bind what arrived into the resolving object's live bindings,
        -- where a later effect of this resolution or a delayed ability it arms
        -- (CR 603.7c) can name it. The shape follows how many arrived: one takes
        -- the single binding, which every reader sees; several take the group,
        -- which the ObjectRef readers and Filter.IsBound see and slotOne does
        -- not; none binds nothing, so no slot names an empty set.
        bindArrivals slot arrived = case arrived of
          [] -> pure ()
          [only] -> State.modify' (bindSlot resolving slot only)
          _ -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList arrived))
     in do
          -- CR 610.3b: the source has already left the battlefield since this
          -- ability triggered, so the object doesn't move. CR 610.3a says the
          -- same of a spell or activated ability put onto the stack, and one
          -- test answers both -- CR 400.7 makes the source one incarnation, so
          -- "has it left" is the same question whichever rule asks it. Glorious
          -- Protector's ruling is the printed statement of it: if it leaves the
          -- battlefield before its triggered ability resolves, no creatures are
          -- exiled.
          --
          -- Ahead of the GATHER, so the CR 608.2d choice is not put to anybody
          -- either: with nothing able to move, that question has no board behind
          -- it.
          declined <- State.gets (\gs -> duration == Just MoveDuration.Type.UntilSourceLeavesTheBattlefield && MoveDuration.hasLeftTheBattlefield source gs)
          Monad.unless declined $ do
            -- WHICH objects move, gathered first and moved second, so the CR 401.2
            -- and CR 401.4 questions between the two steps are asked of the whole
            -- batch.
            targets <- case ref of
              -- NOT routed through objectRefObjects: that function reads `slotGroup`
              -- then `chosen`, never `slotOne`, and slotOne is what lets a slot an
              -- EARLIER EFFECT OF THIS SAME RESOLUTION bound name its object here.
              -- A declared target is read out of `chosen` behind CR 608.2b's
              -- re-validation; a slot `chosen` does not mention was never targeted
              -- and is read live. Membership rather than preference keeps a target
              -- from losing its re-validation to a binding sharing its name.
              -- A slot bound to a GROUP names every member, a slot bound to one
              -- names that one, and a TARGETED slot names EVERY still-legal
              -- recipient -- not one, which is the SlotArity.Many objectRefSlots
              -- declares; legalOne declines a slot naming several. An empty list is
              -- an unbound slot, every target gone illegal, or CR 115.6's zero
              -- targets chosen. fromAmongMembers is where those three and their
              -- rules live, shared with the two "from among them" refs so they
              -- cannot read the slot differently.
              ObjectRef.InSlot slot -> fromAmongMembers legal resolving chosen slot
              -- Swept ONCE from the PRE-MOVE state (CR 608.2c, CR 608.2f), in APNAP
              -- order, then moved one at a time, each judged against the board the
              -- batch began on and against the siblings that have already arrived
              -- (see moveOne). CR 400.3 files a hand arrival under Object.owner.
              ObjectRef.EachMatching _ -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              -- Swept once from the PRE-MOVE state (CR 608.2c, CR 608.2f). "Under
              -- your control" needs nothing here: CR 110.2a hands a battlefield
              -- arrival to the player the effect instructed.
              ObjectRef.EachCardInGraveyard {} -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              -- Swept once from the PRE-MOVE state (CR 608.2c, CR 608.2f). A
              -- resolving spell is on the stack (CR 608.1), not in the hand.
              ObjectRef.EachCardInYourHand -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              -- Swept once from the PRE-MOVE state (CR 608.2c, CR 608.2f), the arm
              -- above's answer over the wider scope.
              ObjectRef.EachCardInHand {} -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              -- Swept once from the PRE-MOVE state (CR 608.2c, CR 608.2f), the two
              -- arms above's answer over CR 400.1's other hidden per-player zone.
              -- A resolving spell is on the stack (CR 608.1), not in the library,
              -- so Paradigm Shift does not sweep itself.
              ObjectRef.EachCardInYourLibrary _ -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              -- CR 607.2a, swept once from the PRE-MOVE state (CR 608.2c, CR
              -- 608.2f). CR 400.3 files a hand arrival under Object.owner.
              ObjectRef.EachCardExiledWithSource {} -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              ObjectRef.EachSpell _ -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              ObjectRef.EachOnStack _ -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              ObjectRef.EachPlayer -> pure []
              ObjectRef.EachOpponent -> pure []
              ObjectRef.ChosenPlayer -> pure []
              -- Read from the pre-move state like the sweeps above: the whole batch
              -- comes off one look at each library (CR 608.2c, CR 608.2f).
              ObjectRef.TopOfLibrary {} -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              -- The walked prefix, read from the PRE-MOVE state for the arm above's
              -- reason: the whole batch comes off one walk of each library (CR
              -- 608.2c, CR 608.2f).
              ObjectRef.TopOfLibraryUntil {} -> do
                gs <- State.get
                pure (objectRefObjects legal resolving controller source gs ref)
              -- One card per chooser, and the only ref whose gather asks a question
              -- rather than reading the board, which is why it is answered here in
              -- the Game monad. Candidates come from the pre-move state (CR 608.2c),
              -- so an earlier effect of this resolution -- Port of Karfell's own mill
              -- -- has already put its cards in the graveyard.
              --
              -- WHO is asked is the ref's Chooser: the resolving controller (CR
              -- 608.2d), or each player the scope names, asked about their own
              -- graveyard alone. The asks run in APNAP order and all before any card
              -- moves (CR 608.2e, CR 101.4). Elided at one candidate and skipped at
              -- none (CR 101.3, CR 609.3), per player under EachInScope. Filtered,
              -- not trusted: an answer naming a card never offered falls back to the
              -- first candidate.
              ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser scope filter_) -> do
                gs <- State.get
                let ask asked candidates = case candidates of
                      [] -> pure []
                      [only] -> pure [only]
                      first : second : more -> do
                        let offered = first NonEmpty.:| (second : more)
                        answer <- Game.choose (Prompt.ChooseCardInGraveyard (Decide.deciderFor asked gs) asked source offered)
                        pure [if List.elem answer (NonEmpty.toList offered) then answer else first]
                case chooser of
                  Chooser.TheController -> ask controller (graveyardCards (chooseContext gs) legal controller gs scope filter_)
                  Chooser.EachInScope ->
                    fmap concat . Monad.mapM (\pid -> ask pid (graveyardCardsOf (chooseContext gs) gs pid filter_)) $
                      zoneScopePlayers legal controller gs scope
                  -- ONE chooser, read out of the slot a ChooseOpponent bound,
                  -- choosing out of their own graveyard. Through playerRefPlayers so
                  -- the slot is read as every other is (CR 608.2b): an unfilled,
                  -- illegal, non-player or many-valued slot names nobody, and nobody
                  -- asked is nothing moved (CR 101.3). Intersected with the scope, so
                  -- a chooser the scope does not name is offered nothing.
                  Chooser.BoundInSlot slot ->
                    case playerRefPlayers legal controller gs (PlayerRef.InSlot slot) of
                      [pid] | List.elem pid (zoneScopePlayers legal controller gs scope) -> ask pid (graveyardCardsOf (chooseContext gs) gs pid filter_)
                      _ -> pure []
              -- The arm above over the hidden zone CR 400.2 makes a hand: what it
              -- says about when the candidates are read (CR 608.2c), about the asks
              -- running in APNAP order (CR 608.2e, CR 101.4) and about the answer
              -- being filtered rather than trusted holds unchanged.
              --
              -- What the hidden zone changes is WHO may be asked: CR 402.3 gives a
              -- hand's cards to its owner alone, so each seat is offered its OWN hand
              -- and no other. Narrowing the offer by filter is the card's own words
              -- saying which cards were ever legal answers (CR 608.2d). Elided at one
              -- card and skipped at none (CR 101.3, CR 609.3).
              ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand player filter_) -> do
                gs <- State.get
                let ask asked candidates = case candidates of
                      [] -> pure []
                      [only] -> pure [only]
                      first : second : more -> do
                        let offered = first NonEmpty.:| (second : more)
                        answer <- Game.choose (Prompt.ChooseCardInHand (Decide.deciderFor asked gs) asked source offered)
                        pure [if List.elem answer (NonEmpty.toList offered) then answer else first]
                fmap concat . Monad.mapM (\pid -> ask pid (handCardsOf (chooseContext gs) gs pid filter_)) $
                  handChoosers legal controller gs player
              -- The printed "from among them", a CR 608.2d choice, asked by
              -- chooseCardFromAmong -- which is where the rule lives, this opcode
              -- and CR 701.20a's reveal being the two that ask it. The candidates
              -- come off the pre-move state (CR 608.2c) through the same
              -- slotBoundObjects the InSlot gather reads, so the choice and "the
              -- rest" cannot see different groups.
              --
              -- What the group has left over is not named here at all. "The rest"
              -- is the same slot read by ObjectRef.InSlot in a LATER clause, which
              -- finds every chosen card gone -- CR 400.7 minted a new object for
              -- each on the way to its new zone, and the ids the group still holds
              -- resolve to nothing, so moveOne passes over them.
              ObjectRef.ChosenCardFromAmong from -> chooseCardFromAmong resolving source controller legal chosen from
              -- Mulch's "all land cards revealed this way", the arm above's plural.
              -- NOT routed through objectRefObjects, for the InSlot arm's reason:
              -- the members come off slotBoundObjects, which reads the single
              -- binding an earlier effect of this resolution left as well as the
              -- group. The same read chooseCardFromAmong makes, off the pre-move
              -- state (CR 608.2c), so the matched half and "the rest" -- a LATER
              -- clause naming the same slot with InSlot, which finds these cards
              -- gone (CR 400.7) -- cannot see different groups.
              ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong slot filter_) -> do
                members <- fromAmongMembers legal resolving chosen slot
                gs <- State.get
                pure (matchingFromAmong legal resolving controller source gs filter_ members)
              -- Not implemented: a card moved at random out of a hand, CR 701.9b's
              -- random discard. Nothing moves it here, so a card writing the ref
              -- under this opcode names no object; the count and that rule's other
              -- exception need a design call (#1733).
              ObjectRef.RandomCardInHand _ -> pure []
              -- CR 608.2d: Glorious Protector's "any number of non-Angel creatures
              -- you control", announced while the effect is applied and so asked
              -- HERE rather than read by objectRefObjects. turnPermanentsOver asks
              -- the same question for CR 701.27a's turn, and this gather owes it the
              -- same posture: candidates are battlefieldMatching's sweep of the same
              -- Filter, read off the pre-move board (CR 608.2c) so the sweep and the
              -- offer cannot disagree, ONE ask of the resolving controller, skipped
              -- at no candidate where the empty set is the only answer (CR 101.3, CR
              -- 609.3), and asked at ONE, where "any number" still leaves two
              -- distinguishable answers.
              --
              -- FILTERED, not trusted (#222), the sibling arms' reason: an answer
              -- naming a permanent that was never offered would otherwise be moved.
              -- Filtering rather than taking the answer also keeps CR 608.2f's APNAP
              -- order, which the candidate list carries and a Set does not.
              ObjectRef.AnyNumberMatching filter_ -> do
                gs <- State.get
                case battlefieldMatching legal resolving controller source gs filter_ of
                  [] -> pure []
                  candidates -> do
                    answer <- Game.choose (Prompt.ChooseAnyNumberOfPermanents (Decide.deciderFor controller gs) controller source candidates)
                    pure (filter (`Set.member` answer) candidates)
              -- CR 608.2d's singular of the arm above: "a creature named Hanweir
              -- Garrison" in Hanweir Battlements' "exile them, then meld them",
              -- announced while the effect is applied. The candidates are
              -- EachMatching's sweep of the same Filter, read live off the pre-move
              -- board (CR 608.2c), so the sweep and the offer cannot disagree about
              -- what matches.
              --
              -- Asked only at TWO or more candidates, which is the whole of what
              -- parts this from the arm above: CR 608.2d admits only a legal
              -- option, so one candidate leaves one legal announcement and no
              -- decision to put to anybody, and none makes the instruction
              -- impossible -- CR 101.3 ignores that part and CR 609.3 leaves the
              -- rest of the effect to do as much as it can.
              -- Pawl.Types.Prompt.ChoosePermanent is where that posture is written.
              --
              -- FILTERED, not trusted (#222), the hand arm's reason: an answer
              -- naming a permanent that was never offered would otherwise be moved.
              ObjectRef.ChosenPermanent filter_ -> chosenPermanent filter_
              -- The arm above's choice with the SOURCE named alongside it: Hanweir
              -- Battlements' "exile them", where "them" is this land and the
              -- Garrison the Filter admits. One instruction over two objects, so
              -- the two are gathered here and moved in the single batch CR 608.2f
              -- makes them -- what two MoveToZone effects could not be, whichever
              -- order they were written in.
              --
              -- The source is read off the PRE-MOVE board (CR 608.2c) through the
              -- same battlefieldMatching sweep the counterpart comes from, filtered
              -- by Filter.IsSource, which is what the card's "this land" is. So a
              -- source that has left the battlefield is simply not named -- CR
              -- 101.3 ignoring that much of the instruction, CR 609.3 leaving the
              -- rest to do as much as it can. It is not offered to anybody: the printed sentence names it
              -- outright, and the one choice here is WHICH counterpart.
              --
              -- The counterpart comes FIRST and the source second, which is the
              -- order the two separate moves had. Nothing rests on it: CR 608.2f
              -- makes the primary order APNAP, and both objects are the resolving
              -- controller's, so its secondary sentence is reached only when the
              -- action cannot be processed simultaneously -- and this one is.
              ObjectRef.SourceAndChosenPermanent filter_ -> do
                counterpart <- chosenPermanent filter_
                gs <- State.get
                pure (counterpart <> battlefieldMatching legal resolving controller source gs Filter.Type.IsSource)
            arrivals <- settleArrivals zone placement targets
            -- The batch's own board, read after CR 401.4's arrangement asks (which
            -- move nothing) and before any member does.
            before <- State.get
            -- CR 701.40e's own three conditions, read off that same board; the
            -- `arrived` bind below is where the rule is applied and where the case
            -- for reading a rider here is written out.
            let manifested = fmap FaceDownState.reason (EntryRiders.faceDown entry) == Just FaceDownReason.Manifested
                fromOwnLibrary target = case Game.lookupObject target before of
                  Just obj -> Object.zone obj == Zone.Library && Object.owner obj == controller
                  Nothing -> False
                oneAtATime =
                  zone == Zone.Battlefield
                    && manifested
                    && length arrivals > 1
                    && all (fromOwnLibrary . fst) arrivals
            -- CR 608.2h: the counts the riders carry, settled ONCE off that same
            -- board and before the fold, so no member of the batch can see how many
            -- counters an earlier member arrived with (CR 608.2f).
            let frozen = freezeRiders (effectViewOf source legal before) (chooseContext before) before resolving source entry
            -- CR 509.4's parenthetical: the attacking creature the effect
            -- SPECIFIED, named by slot. Read ONCE, ahead of the fold and off the
            -- same pre-move board the riders' counts were settled on, so CR 608.2f's
            -- single event cannot see it move; through fromAmongMembers, the reader
            -- every "the object this slot names" site shares, so a targeted slot
            -- and one a trigger bound (Aetherplasm's "that creature") read the same
            -- way.
            --
            -- Exactly one object or nothing: CR 509.4 names ONE attacking creature,
            -- and no printing names several. Nothing here is Combat's own no-op
            -- case anyway.
            mBlocked <- case EntryRiders.blocking entry of
              Nothing -> pure Nothing
              Just slot -> do
                named <- fromAmongMembers legal resolving chosen slot
                pure $ case named of
                  [attacker] -> Just attacker
                  _ -> Nothing
            -- ONE event, which is what Event.simultaneously stamps on everything the
            -- fold records: CR 608.2f processes an action taken on multiple objects
            -- simultaneously, and one opcode is one such action however many objects
            -- the ref swept. The fold's own comments already rest on that sentence --
            -- `before` and the frozen riders are there because no member may observe
            -- another -- so the bracket adds no claim they do not, and CR 603.2c's
            -- batch conditions are what a board can finally read it with.
            --
            -- The per-member CR 616.1 loop inside Event.changeZoneEnteringIn is no
            -- argument for N groups: Event.destroyIn runs exactly such a loop inside
            -- one bracket, and Pawl.Engine.Sba's CR 704.3 pass nests deeper still.
            -- CR 616.1g is where the rules say events nest at all -- a replacement
            -- may apply to "an event contained within the first event" -- so a
            -- replacement opportunity per member is not a second event.
            --
            -- Around the FOLD alone. The gathers above ask CR 608.2d choices and CR
            -- 401.4 arrangements, which move nothing and record nothing; a bracket
            -- reaching over them would only widen what the group means.
            --
            -- CR 701.40e is the one exception the rules write to that reading: "if
            -- an effect instructs a player to manifest multiple cards from their
            -- library, those cards are manifested one at a time." Then the opcode
            -- is not one action taken on several objects but several actions, so
            -- each card gets its own event -- its own board, its own frozen riders,
            -- and no sibling to exclude, which is what lets the second card's CR
            -- 614.12 determination count the first one (Pawl.FaceDownSpec's
            -- Ethereal Ambush under Synthetic Encircling Net).
            --
            -- The gate reads the RIDER (CR 708.2's manifest reason), the
            -- destination and where the cards are, never the effect's identity:
            -- CR 701.40's keyword action is what a face-down battlefield arrival
            -- out of a library IS. Both of the rule's own restrictions are in it --
            -- MULTIPLE, so a single card takes the ordinary path, and THEIR
            -- LIBRARY, which CR 108.3 makes the owner's, so a manifest out of
            -- another player's library or out of exile (Ghastly Conscription) stays
            -- one event.
            arrived <-
              if oneAtATime
                then fmap concat . Monad.forM arrivals $ \arrival -> do
                  now <- State.get
                  let riders = freezeRiders (effectViewOf source legal now) (chooseContext now) now resolving source entry
                  fmap (reverse . snd) (Event.simultaneously (moveOne mBlocked riders now (Set.empty, []) arrival))
                else fmap (reverse . snd) (Event.simultaneously (Monad.foldM (moveOne mBlocked frozen before) (Set.empty, []) arrivals))
            Monad.mapM_ (\slot -> bindArrivals slot (concatMap Foldable.toList arrived)) mSlot
  -- CR 701.24: shuffle the objects the ref names into their OWNERS' libraries. Two
  -- steps: CR 400.7's move through the same changeZone funnel every destination
  -- uses, so a library-entry replacement gets its CR 616.1 opportunity (CR 400.3
  -- files the arrival under Object.owner), then CR 701.24a's randomisation.
  --
  -- The owners are read before either, because the shuffle runs whether or not the
  -- move did (CR 701.24c): a cancelled move leaves no incarnation and no owner, and
  -- the effect's own PlayerRef is what covers that and CR 701.24d's empty set. The
  -- UNION of the two, since rule 701.24's objects go to their owners' libraries and
  -- the named player need not be one of them. Each library is shuffled ONCE (CR
  -- 608.2f), and CR 701.24a makes WHO shuffles unobservable.
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named ref) -> do
    gs <- State.get
    let targets = objectRefObjects legal resolving controller source gs ref
        -- The owners are read from the PRE-MOVE objects (CR 701.24c); an id that
        -- no longer resolves contributes no owner.
        owners =
          Set.fromList (Maybe.mapMaybe (\target -> fmap Object.owner (Game.lookupObject target gs)) targets)
            <> Set.fromList (foldMap (playerRefPlayers legal controller gs) named)
    Monad.forM_ targets $ \target -> Monad.void (Event.changeZoneReturning target Zone.Library)
    -- APNAP (CR 608.2f), which is what makes the ORDER of the Prompt.Shuffle calls
    -- a fact about the rules rather than about PlayerId's Ord.
    Monad.forM_ (filter (`Set.member` owners) (Game.apnapOrder gs)) Event.shuffleLibrary
  -- CR 701.24a alone: randomize the named libraries so no player knows their
  -- order. Nothing moves, so there is no changeZone call and no CR 616.1
  -- opportunity -- the cards a "then shuffle" follows are still the objects they
  -- were, with the ids they had.
  --
  -- APNAP (CR 608.2f) and once per library, the arm above's two reasons: a
  -- PlayerRef naming a player twice still shuffles their library once, and the
  -- ORDER of the Prompt.Shuffle calls is a fact about the rules rather than about
  -- PlayerId's Ord.
  Effect.Shuffle ref -> do
    gs <- State.get
    let named = Set.fromList (playerRefPlayers legal controller gs ref)
    Monad.forM_ (filter (`Set.member` named) (Game.apnapOrder gs)) Event.shuffleLibrary
  Effect.OfferCast (OfferCast.MkOfferCast slot caster optionality offer) -> do
    gs <- State.get
    -- CR 608.2g names "a player", and a reference resolving to nobody offers the
    -- cast to nobody.
    Monad.forM_ (playerRefPlayers legal controller gs caster) $ \pid ->
      offerCast resolving pid slot optionality offer
  -- CR 601.3: write the standing permission onto every object the ObjectRef names,
  -- as CR 109.5's "you" and the stated duration.
  --
  -- NOT gated on the object being in exile: CR 601.3's permissions are not
  -- zone-scoped, so a zone test would be the rules core reading the effect.
  Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile duration ref spending) ->
    State.modify' $ \gs ->
      -- The sweep every ObjectRef-taking opcode shares: a player recipient, an
      -- illegal slot (CR 608.2b) and a set that matched nothing all arrive empty.
      case objectRefObjects legal resolving controller source gs ref of
        [] -> gs
        targets -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
          -- CR 611.2b: the duration never started, so no permission is stored.
          Nothing -> gs
          Just expiry ->
            let permission =
                  ExilePlayPermission.MkExilePlayPermission
                    { ExilePlayPermission.player = controller,
                      ExilePlayPermission.source = source,
                      ExilePlayPermission.expiry = expiry,
                      -- CR 118.14, carried from the opcode unread;
                      -- Pawl.Engine.Mana is the only thing that acts on it.
                      ExilePlayPermission.spending = spending,
                      -- CR 715.3d's "other effects that allow a player to cast
                      -- it": a card said this, not rule 715.3d, so the Adventure
                      -- exclusion does not reach it.
                      ExilePlayPermission.origin = PlayPermissionOrigin.Granted
                    }
                grant o = o {Object.playableFromExile = Just permission}
             in gs {GameState.objects = foldr (Map.adjust grant) (GameState.objects gs) targets}
  -- CR 702.170c: each named card becomes plotted -- the stamp CR 702.170d reads
  -- and the GameEvent.Plotted entry a "when this card becomes plotted" trigger
  -- reads, both through Pawl.Engine.Plot.becomePlotted, so this route and CR
  -- 116.2k's special action say the same thing by construction. The victims are
  -- enumerated ONCE (CR 608.2f).
  --
  -- NOT gated on the object being in exile, Effect.GrantPlayFromExile's reason one
  -- rule over: every producer supplies the exile in the same clause, so a zone
  -- test would gate a branch nothing can reach. A stamp that landed elsewhere is
  -- inert anyway -- Cast.permitsCastPlotted is asked only of exile's members, and
  -- CR 400.7's new incarnation carries no stamp across a zone change.
  Effect.MakePlotted ref ->
    State.modify' $ \gs ->
      foldr Plot.becomePlotted gs (objectRefObjects legal resolving controller source gs ref)
  Effect.ForEach (ForEach.MkForEach ref slot body) -> do
    gs0 <- State.get
    -- CR 608.2f: WHICH members, swept ONCE from the pre-loop board and then fixed,
    -- so the body can neither shorten the batch nor add to it. Recipients rather
    -- than objects, since rule 608.2f is about "players and/or objects" and
    -- Soulfire Eruption's targets are both.
    members <- forEachOrder resolving (const (Just controller)) (objectRefRecipients legal resolving controller source gs0 ref)
    let -- The slots the BODY defines, computed off the instruction rather than the
        -- board: a body effect binds into the resolving object's live bindings and
        -- the next body effect must see it. Restricted to those names so a target
        -- slot cannot return under the instance name CR 700.2d renamed it from.
        bodyDefined = foldMap boundSlots body
        -- The member is bound HERE, in the map handed down, never onto the
        -- resolving object, which scopes it to this iteration. OUTERMOST, so the
        -- loop's own name wins; `m` beats `defined`, since `m` is the CR 608.2b
        -- re-validated map and shadowing it would skip a re-validation.
        withMember member defined m = Map.insert slot (Set.singleton member) (Map.union m defined)
        bindingsOf gs = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)
        -- Whatever those names held BEFORE the loop, to be put back at each
        -- iteration's start and once at the end.
        beforeLoop = Map.restrictKeys (bindingsOf gs0) bodyDefined
        -- The other half of the scoping, not tidiness: an iteration whose
        -- MoveToZone found an empty library binds nothing, and without this its
        -- DealDamage would read the card the PREVIOUS iteration exiled. Restoring
        -- rather than deleting leaves the rest of the resolution its environment.
        rescope gs =
          gs
            { GameState.objects =
                Map.adjust
                  (\o -> o {Object.bindings = Map.union beforeLoop (Map.withoutKeys (Object.bindings o) bodyDefined)})
                  resolving
                  (GameState.objects gs)
            }
    Monad.forM_ members $ \member -> do
      State.modify' rescope
      -- CR 608.2c: the body's instructions in written order, per member, through
      -- the SAME fold a clause's own instructions run through -- CR 603.12's
      -- "happened" is a question about THIS member's iteration, so it resets at
      -- each member rather than carrying over from the previous one. Nihiloor's
      -- "for each opponent, tap up to one untapped creature you control. When you
      -- do, ..." is the shape: the reflexive is that opponent's own tap, not the
      -- previous opponent's.
      applyClauseEffects
        source
        ( \eff -> do
            defined <- State.gets (\gs -> Map.restrictKeys (Binding.targetsOf (bindingsOf gs)) bodyDefined)
            applyEffectWith runSubgame resolving source controller (withMember member defined legal) (withMember member defined chosen) eff
        )
        (Foldable.toList body)
    State.modify' rescope
  Effect.Draw (Draw.MkDraw ref quantity mSlot) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        named = playerRefPlayers legal controller gs ref
        -- CR 121.2c: the active player draws first, then each other player in turn
        -- order. Observable rather than cosmetic: each draw records a zone change
        -- the trigger scan reads (CR 603.2). Not implemented: CR 121.2d's order for
        -- the shared team turns option (#2848). An intersection: apnapOrder supplies the ORDER and `named`
        -- the MEMBERSHIP, which matters for a seat apnapOrder names and `named`
        -- does not -- a departure leaves the roster (CR 800.4k) but takes the
        -- library (CR 800.4a), so drawing would write drewFromEmpty.
        drawers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    -- PER DRAWER (evaluateForRecipient), every amount off the same pre-effect `gs`,
    -- so a seat drawing first cannot change what a later seat draws.
    drawn <- fmap concat . Monad.forM drawers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 -> do
              -- CR 121.2a: the INSTRUCTION is its own replaceable event, and rule
              -- 616.1g settles it before any of the individual draws below -- so
              -- the count those draws run on is the one this loop leaves standing,
              -- and a row that replaced the instruction outright leaves none.
              outcome <- Event.applyReplacements (ProposedEvent.WouldDrawCards pid (Integer.toNaturalSaturating n))
              case outcome >>= Replacement.asDrawCount of
                Nothing -> pure []
                -- CR 121.2: draw the settled count one at a time, so each draw
                -- re-reads the library top and the CR 104.3c empty-library loss is
                -- preserved.
                Just (drawer, settled) ->
                  Maybe.catMaybes <$> Monad.replicateM (Natural.toIntSaturating settled) (Event.drawCardReturning drawer)
        _ -> pure []
    -- CR 121.1's "and reveal IT": the cards the draw put into a hand, for a later
    -- clause of this resolution to name (#1899). The ids are the ones the CR 400.7
    -- funnel ANSWERED -- the incarnation in the hand -- so a reader finds the card
    -- where the rule says it is; a draw a replacement diverted, or one off an
    -- empty library, contributes nothing and binds nothing. Pawl.Types.Draw says
    -- why CR 701.20a rather than CR 400.7j is what makes the card findable in a
    -- hidden zone.
    --
    -- ACROSS drawers and across CR 121.2's individual draws, the tally's posture
    -- in the Mill arm below: the slot is one name and no reader is per-player.
    -- The one/many split is every other group binder's -- one card takes the
    -- SINGLE binding, which slotOne and every singular reader can see, and
    -- several take the group.
    Monad.forM_ mSlot $ \slot -> case drawn of
      [] -> pure ()
      [only] -> State.modify' (bindSlot resolving slot only)
      several -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList several))
  Effect.Mill (Mill.MkMill ref quantity mTally mSlot) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- An illegal slot (CR 608.2b) or a reference naming nobody mills nothing.
        millers = playerRefPlayers legal controller gs ref
        -- CR 701.17/701.17b: top min(n, library) of each miller's library, which is
        -- why the tally below counts THESE cards rather than the number asked for.
        -- PER MILLER (evaluateForRecipient), off the one pre-effect `gs`.
        milledBy =
          Maybe.mapMaybe
            ( \pid -> case evaluateForRecipient viewOf context gs resolving source pid quantity of
                Just n | n > 0 -> Just (pid, List.genericTake n (Game.zoneMembers Zone.Library pid gs))
                _ -> Nothing
            )
            millers
        milled = concatMap snd milledBy
    -- Funnelled so each move mints a new incarnation, then recorded as the mill it
    -- was (CR 701.17a). GameEvent.Milled carries the ids the funnel ANSWERED, not
    -- the ones in the library (CR 400.7, CR 701.17c); a cancelled move is no card
    -- milled. ONE entry per miller, holding that player's whole batch, since rule
    -- 701.17a mills them at once -- and recorded even for a card a replacement
    -- diverted elsewhere, since it was milled wherever it ended up.
    arrivals <- fmap concat . Monad.forM milledBy $ \(pid, cards) -> do
      arrived <- concatMap Foldable.toList <$> Monad.mapM (\c -> Event.changeZoneReturning c Zone.Graveyard) cards
      Monad.unless (null arrived) (State.modify' (Event.recordEvent (GameEvent.Milled (Milled.MkMilled pid (Seq.fromList arrived)))))
      pure arrived
    -- CR 701.17c: a later clause naming the milled cards -- Midnight Tilling's
    -- "from among them" -- finds them in the graveyard they moved to, so the
    -- binding holds the ids the funnel ANSWERED rather than the library ids the
    -- tally counts (CR 400.7). A card a replacement diverted elsewhere is bound
    -- too, and rule 701.17c is then what decides whether a reader can find it:
    -- an exile is public and a hand is not, which is a question about the READER
    -- rather than about this binding.
    --
    -- ACROSS millers, since the slot is one name and no reader is per-player --
    -- the tally's own posture. The one/many split is every other group binder's:
    -- one milled card takes the SINGLE binding, which slotOne and every singular
    -- reader can see, and several take the group.
    Monad.forM_ mSlot $ \slot -> case arrivals of
      [] -> pure ()
      [only] -> State.modify' (bindSlot resolving slot only)
      several -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList several))
    -- The tally, read from the pre-move state because CR 400.7 has since minted
    -- new ids. Each milled card is judged by its own CR 613 projection: rule
    -- 613.1 starts from the actual object and names no zone, so a library card is
    -- folded exactly as a permanent is, and a layer-4 type change (CR 613.1d)
    -- decides rule 728.1's "nonland card" there. Every milled id names a card in
    -- a library, so there is no faceless object for the projection to answer
    -- blankly about.
    --
    -- Bound onto this effect's SOURCE, so a later effect reads it as
    -- Quantity.InSlot; bound even at zero, since zero is an answer. ONE number
    -- across every miller, as no Quantity has a per-player reader.
    -- Not implemented: the tally's filter is matched in a bare Filter.contextFor,
    -- so an atom naming a slot this same resolution bound -- the mill's own, a
    -- clause above -- answers as though nothing were bound (#2141). No card in
    -- the pool writes one there.
    Monad.forM_ mTally $ \tally ->
      let tallyContext = Filter.contextFor (Game.teams gs) Nothing Nothing
          viewOfMilled = Projection.viewsOf gs
          counted oid = Filter.matches tallyContext (viewOfMilled oid) (MillTally.filter tally)
       in State.modify' (bindAmountSlot source (MillTally.slot tally) (Natural.length (filter counted milled)))
  -- CR 701.20a: show the named cards to every player. CR 701.20b keeps them where
  -- they are, so the GameEvent.Revealed the funnel appends IS the whole effect.
  --
  -- The SHOWER is the player CARRYING OUT the instruction (CR 701.20a): for
  -- RandomCardInHand that is the seat whose hand it is, not the controller.
  -- RevealCause.Ordinary, since rule 702.94a's "this way" is the miracle window's
  -- alone. One reveal per named card, which is all GameEvent.Revealed's single
  -- ObjectId allows and all rule 701.20a asks for.
  Effect.Reveal (Reveal.MkReveal ref mSlot) -> do
    gs <- State.get
    -- Show one card, and name it if the card asked for a name. bindSlot and NOT
    -- bindObjectsSlot: this arm names one card per seat, so there is no group to
    -- bind, and the single shape is the one every reader sees -- slotOne included,
    -- where Filter.IsBound reads either.
    let showOne pid oid = do
          Event.reveal RevealCause.Ordinary pid oid
          Monad.forM_ mSlot $ \slot -> State.modify' (bindSlot resolving slot oid)
    case ref of
      -- The one ref whose objects are a QUESTION rather than a read, so it is
      -- answered here -- and the question goes to the INTERPRETER: the engine does
      -- not roll and no player picks. Filtered rather than trusted, so an answer
      -- naming a card never offered falls back to the head of the offer. Game.ask
      -- and not Game.choose, since randomness is not CR 104.4b's optional action.
      --
      -- Elided at one card and skipped at none (CR 101.3, CR 609.3). Candidates are
      -- the hand as CR 608.2c reaches it in the zone's own order (CR 402.3), seats
      -- from handChoosers so the asks run in CR 608.2e's APNAP order. ONE card per
      -- SEAT, so a ref naming several writes the slot once each.
      ObjectRef.RandomCardInHand player ->
        Monad.forM_ (handChoosers legal controller gs player) $ \pid ->
          case Game.zoneMembers Zone.Hand pid gs of
            [] -> pure ()
            [only] -> showOne pid only
            first : second : more -> do
              let offered = first NonEmpty.:| (second : more)
              answer <- Game.ask (Prompt.RandomObject offered)
              showOne pid $
                if List.elem answer (NonEmpty.toList offered) then answer else first
      -- CR 608.2d's "from among them", the ONE choice the printed "reveal ... and
      -- put it into your hand" makes: chooseCardFromAmong asks it, this arm shows
      -- what it named (CR 701.20a), and mSlot below is what a later clause moves
      -- -- so the card revealed and the card moved cannot come apart. Asked here
      -- rather than read, which is why it is not among the refs objectRefObjects
      -- answers.
      --
      -- Not implemented: a reveal whose ref names SEVERAL cards. showOne binds one
      -- card at a time, so the last write wins and a later clause reading the slot
      -- would find one card rather than the group (#2859). Every reveal of a group
      -- member in the pool has a count of one, Carth the Lion's included.
      ObjectRef.ChosenCardFromAmong from -> do
        picked <- chooseCardFromAmong resolving source controller legal chosen from
        Monad.mapM_ (showOne controller) picked
      _ -> do
        let named = objectRefObjects legal resolving controller source gs ref
        Monad.mapM_ (Event.reveal RevealCause.Ordinary controller) named
        -- LookAt's one-versus-many line: a lone card takes the SINGLE binding,
        -- which every reader sees, and several take the group binding, which the
        -- ObjectRef readers and Filter.IsBound see and slotOne does not.
        Monad.forM_ mSlot $ \slot -> case named of
          [] -> pure ()
          [only] -> State.modify' (bindSlot resolving slot only)
          several -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList several))
  Effect.LookAt (LookAt.MkLookAt ref slot) -> do
    gs <- State.get
    -- CR 608.2c: the cards are named as this instruction is reached, and CR 701.20b
    -- (via rule 701.20e) leaves every one where it is -- so this whole arm is the
    -- binding, and an empty library binds nothing. No prompt and no event: rule
    -- 701.20e shows the cards to one player, which pawl has no way to do (#1412),
    -- and a public GameEvent.Revealed would be a different rule (CR 701.20a).
    case objectRefObjects legal resolving controller source gs ref of
      [] -> pure ()
      -- One card takes the SINGLE binding, which every reader sees; several take
      -- the group, which Filter.IsBound reads as CR 701.20e's "among them".
      [only] -> State.modify' (bindSlot resolving slot only)
      several -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList several))
  Effect.Scry (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- An illegal slot (CR 608.2b) or a reference naming nobody scries nothing.
        named = playerRefPlayers legal controller gs ref
        -- CR 701.22c: players scrying at once decide in APNAP order -- apnapOrder
        -- supplies the ORDER, `named` the MEMBERSHIP. Each scryer's cards move
        -- before the next is asked, rather than all together (#1340).
        scryers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    Monad.forM_ scryers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        -- CR 701.22b: scry 0 is not a scry, so zero raises no prompt.
        Just n | n > 0 -> scryOne n pid
        _ -> pure ()
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- Scry's arm in every respect, APNAP included (CR 101.4, rule 701.25
        -- stating no order of its own).
        named = playerRefPlayers legal controller gs ref
        surveillers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    Monad.forM_ surveillers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        -- CR 701.25c: surveil 0 is not a surveil at all.
        Just n | n > 0 -> surveilOne n pid
        _ -> pure ()
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- The players who FATESEAL, not the ones fatesealed (CR 701.29a); whose
        -- library is looked at is fatesealOne's separate choice.
        named = playerRefPlayers legal controller gs ref
        fatesealers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    Monad.forM_ fatesealers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        -- Zero reaches no library, so there is nothing to look at and nobody to
        -- ask. Rule 701.29 states no zero case of its own, unlike CR 701.22b.
        Just n | n > 0 -> fatesealOne source n pid
        _ -> pure ()
  Effect.Explore ref -> do
    gs <- State.get
    -- CR 608.2c: the set is swept as this instruction is reached; an illegal slot
    -- (CR 608.2b) or a player recipient answers with nobody -- Recipient.objectOf
    -- drops the latter, which is why the sweep can be the RECIPIENT one and still
    -- reach only permanents.
    --
    -- CR 701.44d's order, through forEachOrder: APNAP across seats, then that
    -- seat's OWN choice within it -- `id`, not the resolving controller CR 608.2f
    -- names one rule up. The seat comes from recipientSeat, which reads
    -- Projection.controllerWithLastKnown, so it agrees with exploreOne about a
    -- permanent that has left (CR 701.44c).
    ordered <- forEachOrder resolving id (objectRefRecipients legal resolving controller source gs ref)
    Monad.mapM_ exploreOne (Maybe.mapMaybe Recipient.objectOf ordered)
  -- The card names the set, so CR 701.9b's default choice does not arise and
  -- nobody is prompted. Swept ONCE as this instruction is reached (CR 608.2c) and
  -- then fixed (CR 608.2f), and buried through the same funnel the counted branch
  -- below uses, so CR 701.9a's move is RECORDED as a discard -- writing it as a
  -- zone change instead would land every card in the right graveyard and leave
  -- TriggerCondition.SelfDiscarded silent.
  --
  -- The discarding player is PER CARD: rule 701.9a moves a card from its OWNER's
  -- hand, and a ref over CR 400.1's per-player zone can name several owners'
  -- cards at once. A card whose owner cannot be read is skipped rather than
  -- filed under the controller.
  Effect.Discard (Discard.These ref) -> do
    gs <- State.get
    let owned oid = fmap ((,) oid . Object.owner) (Game.lookupObject oid gs)
    Monad.mapM_
      (\(oid, owner) -> Event.discard DiscardCause.Ordinary owner oid)
      (Maybe.mapMaybe owned (objectRefObjects legal resolving controller source gs ref))
  Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard slot quantity mDiscarded)) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
    let -- Every player recipient the slot holds, in APNAP order (CR 101.4),
        -- PlayerSacrifices' own intersection: a slot that is unfilled, illegal
        -- (CR 608.2b) or names an object contributes nobody, and apnapOrder
        -- supplies the ORDER while `named` supplies the MEMBERSHIP.
        named = Maybe.mapMaybe Recipient.playerOf (legalMany slot legal)
        victims = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
        -- Read against the VICTIM, not the controller, so "cards equal to the
        -- number of creatures they control" is answered per discarding player.
        pickFor victim =
          case evaluateForRecipient viewOf context gs resolving source victim quantity of
            Just n
              | n > 0 -> do
                  let held = Game.zoneMembers Zone.Hand victim gs
                      -- `n > 0` above, so the clamp never decides anything here.
                      count = Integer.toNaturalSaturating n
                  if count >= Natural.length held
                    -- CR 609.3: discarding the whole hand is "as much as possible," so
                    -- it is forced -- no choice, so no prompt.
                    then pure (victim, held)
                    else do
                      -- CR 701.9b: the discarding player chooses which cards.
                      let decider = Decide.deciderFor victim gs
                      choices <- Game.choose (Prompt.ChooseDiscard decider victim held count)
                      -- FILTERED AND COMPLETED, PlayerSacrifices' posture. This
                      -- branch is reached only when the hand is LARGER than the
                      -- count, so CR 609.3 does no work and every omitted card is one
                      -- the player could have discarded. Deduplicated too, since the
                      -- answer is a LIST and a card named twice would fill two of the
                      -- n slots; `valid <> filler` permutes `held`, so the take is n.
                      let valid = List.nub (filter (\c -> elem c held) choices)
                          filler = filter (\c -> List.notElem c valid) held
                      pure (victim, List.genericTake count (valid <> filler))
            _ -> pure (victim, [])
    -- CR 101.4: every player chooses in APNAP order, and only THEN do the
    -- actions happen -- PlayerSacrifices' two phases, whose edict is 101.4's own
    -- worked example, so no seat's discard can change what a later seat is
    -- offered. CR 101.4a is why a hand is no exception. The candidate hands are read off the
    -- one `gs` above for the same reason (CR 608.2f).
    --
    -- The SPLIT is a regression fence rather than a proven behaviour: interleaving
    -- the burials with the picks left the suite green, because `held` is read off
    -- the frozen `gs` either way and no printing lets one player's discard reach
    -- another player's hand. The ORDER is proven -- ZoneChangeSpec's "CR 101.4:
    -- asked in turn order from the active player" reads the sequence of prompts.
    doomed <- traverse pickFor victims
    -- CR 701.9a's move, through the shared discard funnel, so the discard is
    -- recorded for a trigger to read. The funnel's own answers come back for the
    -- binding below; a move that did not complete answers Nothing and is dropped.
    moved <-
      fmap concat . Monad.forM doomed $ \(victim, oids) ->
        fmap (concatMap Foldable.toList) (Monad.mapM (Event.discardReturning DiscardCause.Ordinary victim) oids)
    -- The cards "discarded this way", for a later effect of the same resolution
    -- to look back at -- Psychic Miasma's "if a land card is discarded this way".
    -- The CR 400.7 incarnations the funnel MINTED, never the hand ids it was
    -- handed: the hand incarnation no longer exists, so a reader of the
    -- destination zone could match none of them. Destroy's `buried` slot binds
    -- on exactly that argument.
    --
    -- The UNION across seats, since the slot is already a GROUP binding and a
    -- multi-seat discard is one event batch (CR 608.2f); at one seat that is the
    -- singleton it always was, so Psychic Miasma is unchanged.
    --
    -- No board filter of `buried`'s kind: CR 701.9c takes a card put somewhere
    -- other than its owner's graveyard to have been discarded all the same, so
    -- what the funnel moved is what was discarded wherever it landed. CR 400.7j
    -- then decides whether a later part of the effect can FIND it -- a public
    -- destination yes, a hidden one only if the card was revealed on the way
    -- (CR 701.9c) -- which is a question about the reader.
    --
    -- Bound onto `resolving` and as a GROUP, `buried`'s shape and for its reason:
    -- slotBindings reads live GameState unconditionally, which is what puts the
    -- slot in front of Filter.IsBound when the NEXT clause's gate is evaluated. A
    -- single binding would land in the target half of the context instead, where
    -- gateHolds does not look. Nothing is bound when nothing moved, so the gate
    -- finds an unbound slot and the "if" is false -- which is what the rider asks
    -- for.
    Monad.forM_ mDiscarded $ \bound ->
      Monad.unless (null moved) (State.modify' (bindObjectsSlot resolving bound (Seq.fromList moved)))
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- Whoever the PlayerRef names loses the life. Unordered: there is no CR
        -- 121.2c for life, and CR 704.3 checks state-based actions only as a player
        -- would get priority, so no total is observable in between.
        losers = playerRefPlayers legal controller gs ref
    -- PER PAYER (evaluateForRecipient): Shahrazad's "half THEIR life" reads each
    -- payer's own total, and every number is read off the SAME `gs`.
    --
    -- CR 608.2f's bracket, the GainLife arm's and for its reason: one instruction
    -- naming several players is one event, so every seat's loss shares a
    -- Pawl.Types.EventGroup.
    --
    -- A FENCE rather than a proved behaviour, and the same goes for the
    -- SetLifeTotal, ExchangeLifeTotals and RedistributeLifeTotals brackets below:
    -- only a life GAIN has a CR 603.2c batch condition watching it
    -- (Pawl.Types.TriggerCondition.PlayersGainLife), so dropping this bracket
    -- leaves the suite green. It is here because the rule says so.
    Event.simultaneously . Monad.forM_ losers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 -> do
              -- CR 119.3: the life total is simply adjusted. Not through
              -- Pawl.Engine.Damage: CR 119.2 makes damage a CAUSE of life loss,
              -- not a synonym. CR 704.5a's state-based action is the existing one
              -- in Pawl.Engine.Sba.
              --
              -- Through Event.resolveLifeLoss, CR 614.1's funnel for the class,
              -- carrying LifeLossCause.ByEffect: a row scoped to damage does not
              -- reach this road, which is Worship's own ruling ("Worship does not
              -- prevent loss of life, so loss of life bypasses Worship") and what
              -- Pawl.ReplacementSpec's Worship group proves on a board where the
              -- same player at the same life survives 3 damage and dies to
              -- Stronghold Discipline's 3.
              settled <- Event.resolveLifeLoss LifeLossCause.ByEffect pid (Integer.toNaturalSaturating n)
              Event.changeLife pid (negate (toInteger settled))
        _ -> pure ()
  -- CR 119.3's other half, LoseLife's mirror but for the sign. The `n > 0` guard
  -- is CR 119.9: a gain of 0 is no life gain event to trigger on.
  --
  -- CR 608.2f's bracket: "each player gains 4 life" is ONE action taken on
  -- several players and is processed simultaneously, so every seat's gain shares
  -- one Pawl.Types.EventGroup and a CR 603.2c batch condition watching life gain
  -- across players sees one trigger event rather than one per seat. Nothing about
  -- the WRITES moves: the amounts already come off the one pre-effect `gs`, and
  -- what the bracket changes is the group the log stamps.
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        gainers = playerRefPlayers legal controller gs ref
    Event.simultaneously . Monad.forM_ gainers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 -> Event.changeLife pid n
        _ -> pure ()
  -- CR 701.12c: both sides reach each other's PREVIOUS total, so both deltas are
  -- read off the same game state before either is written. Written as a gain and
  -- a loss rather than two assignments, which is what puts a LifeGained and a
  -- LifeLost in the log.
  --
  -- The LOWERED side goes through changeLifeByDelta, so it is proposed as a life
  -- loss and CR 701.12c's "replacement effects may modify these gains and losses"
  -- is reachable; ReplacementSpec's Bloodletter group proves it.
  --
  -- Not implemented: CR 701.12c's deferral to CR 119.7-8, under which an
  -- exchange that would raise a player who can't gain life doesn't happen.
  -- Vacuous: Pawl.Types.PlayerEffect has no such arm to consult (#3078).
  Effect.ExchangeLifeTotals sides -> do
    gs <- State.get
    let twoSides = case sides of
          ExchangeSides.WithController slot -> case legalOne slot legal of
            Just (Recipient.ToPlayer other) -> Just (controller, other)
            _ -> Nothing
          ExchangeSides.BetweenTargets slot -> case legalMany slot legal of
            [Recipient.ToPlayer one, Recipient.ToPlayer two] -> Just (one, two)
            _ -> Nothing
    case twoSides of
      Just (this, that) -> do
        let lifeOf pid = maybe 0 Player.life (Map.lookup pid (GameState.players gs))
            thisLife = lifeOf this
            thatLife = lifeOf that
        -- CR 608.2f's bracket: an exchange is ONE action taken on two players --
        -- CR 701.12a's "the entire exchange", which either happens or does not --
        -- so the gain and the loss it decomposes into share one
        -- Pawl.Types.EventGroup rather than reading as two events in sequence.
        Event.simultaneously $ do
          changeLifeByDelta this (thatLife - thisLife)
          changeLifeByDelta that (thisLife - thatLife)
      -- CR 701.12a: if the entire exchange can't be completed, no part of it
      -- occurs.
      Nothing -> pure ()
  -- CR 119.5: a DELTA per player against that player's own current total, so one
  -- seat may gain while another loses. Through Event.changeLife rather than a raw
  -- write to Player.life, for the sake of the log the rule describes.
  --
  -- Evaluated ONCE PER RECIPIENT, a card being able to name a number that is each
  -- recipient's own (Biorhythm). Every evaluation and delta is read off `gs`, the
  -- state before any life moves (CR 608.2f).
  --
  -- Each delta goes through changeLifeByDelta, which proposes a DOWNWARD one as a
  -- life loss, since rule 119.5 spells a lower total as the player losing "the
  -- necessary amount of life".
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        recipients = playerRefPlayers legal controller gs ref
    -- CR 608.2f's bracket, the GainLife arm's: one instruction setting several
    -- players' totals is one event, however the per-seat deltas fall out.
    Event.simultaneously . Monad.forM_ recipients $ \pid ->
      -- A player with no row is nobody to move.
      Monad.forM_ (Map.lookup pid (GameState.players gs)) $ \player ->
        -- An undeterminable total is no instruction, asked per recipient: one
        -- seat's unanswerable count says nothing about the others.
        Monad.forM_ (evaluateForRecipient viewOf context gs resolving source pid quantity) $ \total ->
          changeLifeByDelta pid (total - Player.life player)
  -- CR 119.7 / 119.8: redistribute life totals, each new total being CR 119.5's
  -- gain or loss of the necessary amount. The roster is CR 102.1's players IN the
  -- game, not the keys of GameState.players, which keep a departed seat's row.
  -- Every total is read ONCE, before the prompt and before any life moves (CR
  -- 608.2h); a rotation would otherwise leave two seats on one number.
  --
  -- FILTERED, NOT TRUSTED, all-or-nothing: only a whole permutation is a
  -- legal answer, so a bad one falls back to redistributing among nobody.
  --
  -- A LOWERED total goes through changeLifeByDelta, the ExchangeLifeTotals arm's
  -- road: rule 119.5's loss, proposed so a replacement reaches it (CR 614.1).
  --
  -- Not implemented: CR 119.7-8's own restrictions on a player who can't gain or
  -- lose life (vacuous, as for ExchangeLifeTotals) (#3078), nor CR 810.9f's "not
  -- more than one member of each team", which is a Two-Headed Giant rule (#2849).
  Effect.RedistributeLifeTotals -> do
    gs <- State.get
    let candidates = Game.stillPlaying gs
        lifeOf pid = maybe 0 Player.life (Map.lookup pid (GameState.players gs))
        offered = fmap (\pid -> (pid, lifeOf pid)) candidates
    -- With one candidate or none every assignment is the same, so there is
    -- nothing to ask.
    Monad.when (length candidates > 1) $ do
      assignment <- Game.choose (Prompt.ChooseRedistribution (Decide.deciderFor controller gs) controller offered)
      let takers = Map.keysSet assignment
          givers = Set.fromList (Map.elems assignment)
          -- Set equality also settles injectivity: a repeated giver makes
          -- `givers` smaller than `takers`.
          isPermutation = Set.isSubsetOf takers (Set.fromList candidates) && takers == givers
      -- CR 608.2f's bracket, the ExchangeLifeTotals arm's: one redistribution is
      -- one action taken on every seat it names, so the whole permutation's gains
      -- and losses share one Pawl.Types.EventGroup.
      Monad.when isPermutation . Event.simultaneously . Monad.forM_ (Map.toList assignment) $ \(taker, giver) ->
        changeLifeByDelta taker (lifeOf giver - lifeOf taker)
  -- CR 702.179c: each named player's speed increases by this much. Its two
  -- readings -- a player who HAS speed goes up, a player with NONE has their
  -- speed BECOME the value -- are spelled separately, since DecreaseSpeed below
  -- must not create a speed out of nothing. No cap, which is a PROJECT DECISION
  -- and not a rule: nothing in rule 702.179 bounds speed from above, and nothing
  -- permits exceeding 4 either, so pawl rules that an effect may push past it.
  -- Pawl.Engine.Speed.maxSpeed carries the decision and CR 702.178a's side of it.
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        revving = playerRefPlayers legal controller gs ref
    Monad.forM_ revving $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 ->
              let by :: Natural
                  by = Integer.toNaturalSaturating n
                  faster p = p {Player.speed = Just (maybe by (+ by) (Player.speed p))}
               in State.modify' (\g -> g {GameState.players = Map.adjust faster pid (GameState.players g)})
        _ -> pure ()
  -- CR 702.179: speed drops by this much, never below the floor the CARD prints
  -- (rule 702.179 states none). Not the mirror arm's negation: a player with NO
  -- SPEED (CR 702.179b) stays that way, so the Maybe is traversed rather than
  -- defaulted.
  --
  -- The REFERENCE is resolved against the CHOSEN slots where every sibling arm
  -- reads the legal ones: this effect names the controller of a permanent an
  -- earlier clause has already moved, which CR 608.2h rather than CR 608.2b
  -- governs. The AMOUNT's context stays on the legal slots.
  --
  -- Not implemented: no other opcode passes the chosen slots, so a card writing
  -- PlayerRef.ControllerOfBound in one of their references would lose the player
  -- once the object moved (#3059).
  Effect.DecreaseSpeed d -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        slowing = playerRefPlayers chosen controller gs (SpeedDecrease.player d)
        atLeast = toInteger (SpeedDecrease.floor d)
    Monad.forM_ slowing $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid (SpeedDecrease.quantity d) of
        Just n
          | n > 0 ->
              let slower p = p {Player.speed = fmap (\was -> Integer.toNaturalSaturating (max atLeast (toInteger was - n))) (Player.speed p)}
               in State.modify' (\g -> g {GameState.players = Map.adjust slower pid (GameState.players g)})
        _ -> pure ()
  -- CR 701.21a: the players the slot names each sacrifice `quantity` permanents
  -- matching the filter, and EACH OF THEM chooses which -- the whole difference
  -- between this and Sacrifice above. CR 609.3: only a genuine surplus prompts.
  --
  -- CR 101.4's example is this instruction verbatim -- "Each player sacrifices a
  -- creature. First, the active player chooses a creature they control. Then each
  -- of the nonactive players, in turn order, chooses ... Then all creatures
  -- chosen this way are sacrificed simultaneously" -- so every pick is taken
  -- first, in APNAP order, and only then does anything leave the battlefield.
  -- The candidate lists are read off ONE `gs` for the same reason.
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot filter_ quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- Every player recipient the slot holds, in APNAP order. A slot that is
        -- unfilled, illegal (CR 608.2b) or names an object contributes nobody.
        named = Maybe.mapMaybe Recipient.playerOf (legalMany slot legal)
        victims = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
        pickFor victim =
          -- Read against the VICTIM: "half the permanents they control" is a
          -- number of the sacrificing player's own.
          case evaluateForRecipient viewOf context gs resolving source victim quantity of
            Just n
              | n > 0 -> do
                  -- Candidates are what the VICTIM controls, ascending, so both the
                  -- elision and a short transcript are deterministic. Through
                  -- Replacement.sacrificeCandidates, which is what puts CR 101.2's
                  -- "can't be sacrificed" on this path: a prohibited permanent is
                  -- never the pick that satisfies the edict.
                  let candidates = Replacement.sacrificeCandidates (Filter.slotObjects context) victim Nothing filter_ gs
                      decider = Decide.deciderFor victim gs
                      -- `n > 0` above, so the clamp never decides anything here.
                      count = Integer.toNaturalSaturating n
                  picked <-
                    if Natural.length candidates <= count
                      then pure (Set.fromList candidates)
                      else Game.choose (Prompt.ChooseSacrifices decider victim source candidates count)
                  -- FILTERED AND COMPLETED, not merely filtered: an edict is not
                  -- "may", so an answer naming too few would cheat it, and CR 609.3
                  -- caps it at "as much as possible". Valid picks are honoured
                  -- first, the rest made up from the remaining candidates in the
                  -- order offered.
                  let wanted = min count (Natural.length candidates)
                      valid = filter (\oid -> Set.member oid picked) candidates
                      filler = filter (\oid -> List.notElem oid valid) candidates
                  pure (victim, List.genericTake wanted (valid <> filler))
            _ -> pure (victim, [])
    doomed <- traverse pickFor victims
    -- One at a time rather than as one event, Effect.Sacrifice's fold above
    -- (#757). The reachable caller of the two: All Is Dust lands here.
    Monad.forM_ doomed (\(victim, oids) -> Monad.mapM_ (Event.sacrifice victim) oids)
  Effect.Create (Create.MkCreate quantity card entry mSlot creator) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- CR 608.2h: the counts the tokens enter with, settled ONCE off the
        -- pre-effect board and outside the per-creator loop below, so every seat's
        -- tokens carry the same answer (CR 608.2f).
        frozen = freezeRiders viewOf context gs resolving source entry
        -- CR 111.2: WHOSE tokens. CR 109.5's "you" is the default reference, and
        -- every other one is somebody the sentence identified -- Rampage of the
        -- Clans' "its controller", read off the loop's member through CR 608.2h.
        -- APNAP (CR 608.2f) for a reference naming several, apnapPlayersOf's own
        -- intersection so a reference naming a departed seat mints nothing.
        creators = apnapPlayersOf creator legal controller gs
    -- CR 509.4's parenthetical: the attacking creature the effect SPECIFIED,
    -- named by slot. Read ONCE, ahead of the minting loop, so CR 608.2f's single
    -- event cannot see it move; through fromAmongMembers, the reader every "the
    -- object this slot names" site shares, so a targeted slot (Flash Foliage) and
    -- one an earlier effect or a trigger bound read the same way.
    --
    -- Exactly one object or nothing: CR 509.4 names ONE attacking creature, and
    -- no printing names several. Nothing here is Combat's own no-op case anyway.
    mBlocked <- case EntryRiders.blocking entry of
      Nothing -> pure Nothing
      Just slot -> do
        named <- fromAmongMembers legal resolving chosen slot
        pure $ case named of
          [attacker] -> Just attacker
          _ -> Nothing
    -- PER CREATOR, every amount off the same pre-effect `gs` (CR 608.2f), so one
    -- seat's tokens cannot change how many the next seat gets.
    minted <- fmap concat . Monad.forM creators $ \creating ->
      case evaluateForRecipient viewOf context gs resolving source creating quantity of
        Just n
          | n > 0 -> do
              -- CR 111: create n tokens under that player's control (CR 111.2)
              -- through the single funnel, so CR 614's token replacements get their
              -- opportunity. CR 110.5b: the funnel is handed the entry's tap state.
              -- CR 122.6's counters ride along through that rule's own door, so a
              -- counter replacement reaches them, in the counts freezeRiders
              -- settled above.
              made <- Event.createTokens creating (bakeTokenCharacteristics (Quantity.evaluateFor viewOf context gs resolving source) card) Nothing (Integer.toNaturalSaturating n) (EntryRiders.tapped entry) (EntryRiders.counters frozen)
              -- CR 508.4: a creature put onto the battlefield attacking has its
              -- defending player chosen in Pawl.Engine.Combat, and CR 508.3a's
              -- attack triggers see nothing. After the entry loops rather than
              -- inside them: CR 614.16's replacement settles the COUNT first.
              Monad.when (EntryRiders.attacking entry) (Monad.mapM_ Combat.putOntoBattlefieldAttacking made)
              -- CR 509.4, the blocking twin one rule over, and in the same place
              -- for the same reason: CR 614.16's replacement settles the COUNT
              -- first, and CR 506.3e / CR 509.4a's no-op conditions live in
              -- Pawl.Engine.Combat rather than here.
              Monad.forM_ mBlocked (\attacker -> Monad.mapM_ (\made' -> Combat.putOntoBattlefieldBlocking made' attacker) made)
              pure made
        _ -> pure []
    case (mSlot, namesEveryToken quantity, minted) of
      (Nothing, _, _) -> pure ()
      -- Nothing was minted, so no slot names anything: an unevaluable or
      -- non-positive count, a creator reference naming nobody (CR 101.3), a
      -- creator who has left the game (CR 800.4b), or CR 111.5's prohibited
      -- token, which createTokens refuses to mint at all.
      (Just _, _, []) -> pure ()
      -- The card says "those tokens", so the slot holds EVERY token this
      -- Create minted (CR 111.1) and there is nothing to ask.
      (Just slot, True, _) -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList minted))
      -- CR 603.7c: bind the minted token so a delayed ability this same
      -- resolution arms can name it. One token is the whole candidate
      -- list, so there is nothing to ask.
      (Just slot, False, [only]) -> State.modify' (bindSlot resolving slot only)
      -- CR 614.16 got there first: a replacement multiplied the count, so
      -- several tokens stand where CR 603.7c's "it" names one. CR 707.10e
      -- is the codified analogue, so this asks. FILTERED, NOT TRUSTED: an
      -- answer naming something not minted falls back to the first.
      (Just slot, False, first : second : rest) -> do
        gs1 <- State.get
        let candidates = first NonEmpty.:| (second : rest)
            decider = Decide.deciderFor controller gs1
        answer <- Game.choose (Prompt.ChooseBoundToken decider controller source candidates)
        let named = if List.elem answer (NonEmpty.toList candidates) then answer else first
        State.modify' (bindSlot resolving slot named)
  -- Alchemy's conjure keyword action. Digital-only, so there is no rule to cite;
  -- what the CR settles is that the result is a CARD and not CR 111.1's token,
  -- which is what Pawl.Engine.Event's conjure and conjureOntoBattlefield mint.
  --
  -- The card and the destination are read off no board -- the one is literal
  -- text and the other a constructor -- but the COUNT is a Quantity, so it takes
  -- CR 608.2h's snapshot like every other count: evaluated once off the
  -- pre-effect board, ahead of the minting loop, so a card arriving cannot
  -- change how many arrive.
  --
  -- The conjurer is the resolving CONTROLLER (CR 109.5's "you"). Not implemented:
  -- a printing that states one instead, which is a shape rather than one card --
  -- Pawl.Types.Conjure lists the two forms (#2638).
  Effect.Conjure (Conjure.MkConjure quantity card destination) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- Three of the four arrivals are no zone change -- the card was in no
        -- zone to leave -- so nothing triggers and nothing is revealed. The
        -- BATTLEFIELD is the exception and takes its own road below, because CR
        -- 616.1's entry loop and CR 603.6a's trigger scan both have to see a
        -- permanent enter.
        --
        -- Not implemented: a stated library position. Every arrival takes
        -- LibraryPosition.defaultValue, which is the BOTTOM, and the printings
        -- that state an end all say the TOP -- Pawl.Types.ConjureDestination's
        -- Library arm names them and says why none of them is in data/cards/
        -- (#2638).
        intoZone zone n = Monad.replicateM_ (Integer.toIntSaturating n) (Monad.void (Event.conjure controller card zone LibraryPosition.defaultValue))
    case evaluateForRecipient viewOf context gs resolving source controller quantity of
      Just n
        | n > 0 -> case destination of
            ConjureDestination.Hand -> intoZone Zone.Hand n
            ConjureDestination.Library -> intoZone Zone.Library n
            ConjureDestination.Graveyard -> intoZone Zone.Graveyard n
            -- CR 110.2a: the resolving controller is who the permanent enters
            -- under, which conjureOntoBattlefield stamps.
            ConjureDestination.Battlefield -> Monad.void (Event.conjureOntoBattlefield controller card (Integer.toNaturalSaturating n))
      _ -> pure ()
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref entry) -> do
    gs <- State.get
    -- CR 707.2 / 111.3: this many tokens per named permanent, minted through the
    -- same CR 111.2 funnel, carrying the copied permanent's COPIABLE values
    -- rather than its projection. All of it read off ONE `gs` (CR 608.2f).
    --
    -- The token's own card is the copied permanent's, so a reader going past the
    -- projection to Game.faceOf sees the same text the snapshot does; it is not
    -- where the characteristics come FROM, the snapshot being layer 1 (CR
    -- 613.1a). CR 608.2h: both reads take the last known branch for a permanent
    -- already gone, and the pair has to move together.
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        sources = objectRefObjects legal resolving controller source gs ref
        -- CR 608.2h: the counters the copies enter with, settled ONCE off the
        -- pre-effect board, outside the loop over named permanents below.
        frozen = freezeRiders viewOf context gs resolving source entry
    -- The count is Create's, read the same way and off the same `gs` (CR 707.1).
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Just n
        | n > 0 ->
            Monad.forM_ sources $ \src ->
              Monad.forM_ (Game.cardOfWithLastKnown src gs) $ \card ->
                -- CR 707.2 copies no counters, so what the token arrives with
                -- is what the EFFECT said and nothing the original carried --
                -- Littjara Mirrorlake's "except it enters with an additional
                -- +1/+1 counter on it". Through Event.createTokens' own counter
                -- argument, CR 122.6's door, so CR 614.16's replacements see
                -- them; Create's arm one case up hands over the same value.
                --
                -- Untapped rather than the rider's tap state: only `counters` is
                -- read here, and Pawl.CardSpec lints that no CreateCopy in the
                -- pool sets any other rider (gap #2302).
                --
                -- ONE call per named permanent, with the whole count: CR 614.12's
                -- entry loop is handed the batch, so the copies enter
                -- simultaneously and none may copy a sibling.
                Monad.void (Event.createTokens controller card (Just (Event.copiedSnapshotWithLastKnown src gs)) (Integer.toNaturalSaturating n) TapState.Untapped (EntryRiders.counters frozen))
      _ -> pure ()
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy originalRef subjectRef) ->
    State.modify' $ \gs ->
      -- CR 707.1: each named subject becomes a copy of the named original, in
      -- whatever zone it already sits -- CR 707.4's "while remaining on the
      -- battlefield" for a permanent, and Synthetic Mirror of the Fallen's card
      -- in a graveyard otherwise, no zone change happening either way (CR
      -- 400.7). Both sides are enumerated ONCE off the same
      -- `gs` (CR 608.2f), so an illegal slot, a player recipient and a set that
      -- matched nothing all arrive empty and copy nothing. ONE original: CR 707.2
      -- copies the values of "the original object", singular.
      --
      -- The snapshot is a VALUE, so CR 707.2b holds by construction. Written to
      -- Binding.copyOf per CR 707.3, at layer 1 (CR 613.1a), so layers 2-7
      -- re-apply over the new base -- CR 707.4's "doesn't change any noncopy
      -- effects presently affecting the permanent".
      --
      -- Not implemented: CR 707.9a's "except it has this ability", which every
      -- printed producer carries (#1292); pawl's copy is stricter, losing the
      -- ability that made it. Nor a stated duration (#1753).
      case objectRefObjects legal resolving controller source gs originalRef of
        [original] ->
          let snapshot = Event.copiedSnapshotWithLastKnown original gs
              write o = o {Object.bindings = Binding.setCopy snapshot (Object.bindings o)}
              subjects = objectRefObjects legal resolving controller source gs subjectRef
           in gs {GameState.objects = foldr (Map.adjust write) (GameState.objects gs) subjects}
        _ -> gs
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets) -> do
    gs <- State.get
    -- CR 707.10: one copy per named object, each put onto the stack. The named
    -- objects are enumerated ONCE off this `gs` (CR 608.2f); each copy is then
    -- minted against the live state, since a fresh id and a fresh timestamp are
    -- both counters the previous mint moved.
    Monad.forM_ (objectRefObjects legal resolving controller source gs ref) $ \original ->
      -- CR 707.10's three nouns: a spell (Game.isSpell) and an activated or
      -- triggered ability (Game.isAbility), each classifying off the object's
      -- ZONE and its Source and never off which card it is. Everything else
      -- copies nothing -- an ObjectRef that named a card in a graveyard reaches
      -- CR 707.13's different act (#888), and a permanent on the battlefield is
      -- the CreateCopy and BecomeCopy opcodes' subject rather than this one's.
      Monad.forM_ (if Game.isSpell original gs || Game.isAbility original gs then Game.lookupObject original gs else Nothing) $ \obj ->
        Monad.forM_ (copyOnStackOf (Object.source obj)) $ \(copySource, kind) -> do
          -- CR 707.10's answers, as the target maps to write: one EMPTY map
          -- where the copy keeps the decisions rule 707.10 copied (rule 707.10c
          -- included, its offer below being a separate act), and CR 707.10d's
          -- one map per candidate, in the order its controller chose.
          plan <- case targets of
            CopyTargets.Copied -> pure [Map.empty]
            CopyTargets.ChosenByController -> pure [Map.empty]
            CopyTargets.ForEach candidateRef -> copyForEachTargets controller resolving source legal original candidateRef
          Monad.forM_ plan $ \retarget -> do
            gsNow <- State.get
            let (copyId, gs1) = Game.freshObjectId gsNow
                (ts, gs2) = Game.freshTimestamp gs1
                -- CR 707.10's "all decisions made for it": the modes, the targets,
                -- the value of X and the announced costs are all fields of the
                -- object being copied, so the copy IS that object with the few
                -- things CR 707.10 names overwritten. Enumerating what to carry
                -- would be a list to keep in step with Pawl.Types.Object; this way
                -- a new decision field is copied by construction.
                --
                -- Owner and controller are the COPYING effect's controller, both
                -- stated outright by CR 707.10 and neither inherited: the copy is
                -- "owned by the player under whose control it was put on the
                -- stack". Rule 707.10 gives an OWNER only to a copy of a spell, and
                -- the same write is right for an ability: pawl's Object.owner on an
                -- ability object is that ability's controller, which is what
                -- Pawl.Engine.Activate stamps and what Pawl.Engine.Stack's two
                -- ability arms read.
                --
                -- Zeroed: damage, counters and designations, none of which CR
                -- 707.2 copies. Neither a spell nor an ability on the stack carries
                -- any of the three today, so this is the rule written out rather
                -- than a difference the board can show.
                --
                -- CR 707.2's copiable values are stamped for a SPELL only, where the
                -- other two copy opcodes stamp them so
                -- Projection.copiableCharacteristics answers for all three and CR
                -- 707.3 holds for free. The LIVE reader, not the last-known one: the
                -- object was just looked up, so there is nothing to resurrect. An
                -- ABILITY has no card behind it at all (CR 113.7a; Game.cardOf
                -- answers Nothing for one), so there is nothing to snapshot -- CR
                -- 707.10b's "the same source as the original ability" is the whole of
                -- what its copy carries, and it rides in `copySource` above. That
                -- half is a REGRESSION FENCE rather than a proven line: stamping a
                -- snapshot on an ability copy anyway left the suite green
                -- (2026-09-03), an ability having no characteristic any board can
                -- read it back off.
                stampCopiable = case kind of
                  StackObjectKind.Spell -> Binding.setCopy (Event.copiedSnapshot original gs)
                  StackObjectKind.Ability -> id
                copy =
                  obj
                    { Object.source = copySource,
                      -- CR 707.10 puts the copy ONTO THE STACK, so the zone is the
                      -- opcode's own rather than the copied object's -- which the
                      -- guard above has already established was the stack.
                      Object.zone = Zone.Stack,
                      Object.owner = controller,
                      Object.enteredUnder = Just controller,
                      Object.timestamp = ts,
                      Object.damage = 0,
                      Object.counters = Map.empty,
                      Object.counterTimestamps = Map.empty,
                      Object.designations = Set.empty,
                      -- CR 109.5's "you" is RE-STAMPED, and it is the one binding
                      -- that must be: Pawl.Engine.Cast and Pawl.Engine.Activate
                      -- write the caster or activator into it as the original goes
                      -- on the stack, and CR 707.10 makes the copy's controller the
                      -- copying effect's controller instead. Every other binding is
                      -- a DECISION, which CR 707.10 copies verbatim -- including an
                      -- ability's self slot, so CR 707.10b's "the copy refers to
                      -- that same object" needs no write of its own.
                      -- CR 707.10d's targets, where the effect chose them, over
                      -- the decisions CR 707.10 copied. Empty for the other two
                      -- answers, which leave every one of them standing.
                      Object.bindings = Binding.setYou controller (stampCopiable (Map.union (fmap Binding.toRecipients retarget) (Object.bindings obj)))
                    }
            State.put (Game.insertIntoZone Zone.Stack LibraryPosition.defaultValue controller copyId gs2 {GameState.objects = Map.insert copyId copy (GameState.objects gs2)})
            Monad.when (targets == CopyTargets.ChosenByController) (chooseNewTargetsFor controller copyId)
            -- CR 115.1: "these targets are declared as part of the process of
            -- putting the spell or ability on the stack", and CR 707.10c puts the
            -- copy on the stack once its controller has decided what its targets
            -- will be -- so each of them becomes a target of the copy, and CR
            -- 702.21a's ward fires on it.
            --
            -- EVERY target, not only the ones CR 707.10c changed: the copy "is
            -- itself a spell" -- or itself an ability -- under CR 707.10, so an
            -- object the copy kept is a target of something it was not a target of
            -- before. Read back off the board AFTER the re-target above rather than
            -- from the copied bindings, for the same reason.
            --
            -- The KIND is the copied object's, which copyOnStackOf classified: CR
            -- 707.10's "a copy of a spell is itself a spell" and "a copy of an
            -- ability is itself an ability".
            gsCopied <- State.get
            Event.becameTarget copyId kind controller (targetsOnStack copyId gsCopied)
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name onset duration) -> do
    gs <- State.get
    -- CR 608.2h's last-known fallback, and not belt and braces: the source can
    -- have left an opcode earlier in this same list, CR 400.7 having deleted the
    -- id `source` names. CR 603.7: a rule 702 keyword has no card text to declare
    -- the far end in, so a name a minted ability arms resolves against rule 702's
    -- own roster instead; the two namespaces are kept disjoint by Pawl.CardSpec.
    case declaredDelayedAbility source name gs <|> Keyword.mintedDelayedAbility name of
      -- For a CARD's name the dataflow lint makes a dangling one a failing test,
      -- and this arm only keeps the executor total. A MINTED name has no such
      -- lint, so a forgotten roster row lands here and does nothing.
      Nothing -> pure ()
      Just ability ->
        -- CR 603.7d-f: the controller is the player who controlled the spell or
        -- ability AS IT RESOLVED, baked in now. CR 603.7a: an entry appended here
        -- never fires on an event that already happened.
        let captured = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)
            -- CR 603.7a's creation moment, from the same counter every other
            -- moment comes from, so CR 701.27f can compare it against
            -- Object.turnedOverAt. Minted here rather than reusing the resolving
            -- object's stamp: one resolution can arm several entries, and each is
            -- created as its own opcode runs.
            (createdAt, gs1) = Game.freshTimestamp gs
            entry =
              DelayedTrigger.MkDelayedTrigger
                { DelayedTrigger.ability = ability,
                  DelayedTrigger.source = source,
                  DelayedTrigger.controller = controller,
                  DelayedTrigger.bindings = captured,
                  -- CR 603.7a's other end: the BOUNDARY, not a turn number, for
                  -- one printed "on your next turn". Which turn that names is
                  -- settled as that turn begins (Event.settleOnsets).
                  DelayedTrigger.window = Event.armOnset onset,
                  -- CR 603.7b's stated duration. The OUTER Maybe is the card
                  -- printing no duration; the inner one is Expiry.arm reporting
                  -- that a printed duration never STARTED (CR 611.2b).
                  DelayedTrigger.expiry = duration >>= \d -> Expiry.arm (Binding.playersIn legal) controller source d gs,
                  DelayedTrigger.createdAt = createdAt
                }
         in State.put gs1 {GameState.delayedTriggers = GameState.delayedTriggers gs1 Seq.|> entry}
  Effect.Replace (Replace.MkReplace duration uses origin condition re) ->
    -- CR 614.3 / 615.3: install the floating replacement. Targetless and
    -- unprompted. CR 113.7: the SOURCE is this effect's source, which with the
    -- timestamp is the row's CR 614.5 identity. CR 614.15: the ORIGIN
    -- travels with the row rather than being re-derived.
    State.modify' $ \gs ->
      let context = effectContext gs controller source legal (slotBindings resolving gs)
       in case Expiry.arm (Binding.playersIn legal) controller source duration gs of
            -- CR 611.2b: the duration never started.
            Nothing -> gs
            Just expiry ->
              let (ts, gs1) = Game.freshTimestamp gs
                  active =
                    ActiveReplacement.MkActiveReplacement
                      { ActiveReplacement.effect = re,
                        ActiveReplacement.source = source,
                        -- CR 109.5: the resolution's controller, BAKED now --
                        -- the source is a spell CR 608.2n is about to put in a
                        -- graveyard, so it will have no controller to project
                        -- when the row is consulted.
                        ActiveReplacement.controller = controller,
                        ActiveReplacement.timestamp = ts,
                        ActiveReplacement.expiry = expiry,
                        ActiveReplacement.uses = uses,
                        ActiveReplacement.origin = origin,
                        -- CR 614.1: the clause's printed "if" rides the row and
                        -- is asked as the event would happen, never latched here
                        -- (see Pawl.Types.ActiveReplacement).
                        ActiveReplacement.condition = condition,
                        ActiveReplacement.rider = Nothing,
                        -- The resolution's slot bindings, captured as the row is
                        -- installed for the reason Pawl.Types.DelayedTrigger
                        -- captures them (CR 603.7c): this resolution is about to
                        -- end and its object with it, so a pattern naming a slot
                        -- would have nothing live to read. The row's `condition`
                        -- is asked against these too (Replacement.collect), so
                        -- the clause and the pattern cannot disagree about what a
                        -- slot names.
                        --
                        -- NARROWED to the slots THIS ROW names, installDamageRow's
                        -- restriction and for its reason: referredToSources reads
                        -- this map as CR 609.7a's "any object referred to by ... a
                        -- replacement or prevention effect that's waiting to
                        -- apply", and a slot an unrelated earlier effect of the
                        -- same resolution bound is not something this row refers
                        -- to. The CLAUSE's own reads join the row's, since it is
                        -- asked in a Context built from this very map.
                        ActiveReplacement.slots =
                          Map.restrictKeys
                            (Filter.slotObjects context)
                            (Map.keysSet (replacementRowSlots re) <> foldMap (Map.keysSet . conditionSlots) condition)
                      }
               in gs1 {GameState.replacements = active : GameState.replacements gs1}
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration kind ref whatRecipient whoRecipient sourceFilter quantity riderEffects) -> do
    -- CR 615.3 / 615.7: install a floating prevention shield, consulted at the
    -- damage funnel until it is spent or the duration expires. Its own opcode
    -- rather than an Effect.Replace carrying a DamageR, because the pattern has
    -- to name the shielded permanent or player by id, which card data cannot.
    -- Through Damage.damageRecipient, so the baked recipient is in the same
    -- vocabulary a DamageEvent's target arrives in (CR 120.1a).
    --
    -- ONE ROW PER NAMED RECIPIENT, which is CR 615.11: a shield over "each of a
    -- number of untargeted creatures" is a separate shield per creature, created
    -- as the spell resolves. Only ONE ROW ALTOGETHER for a card that DESCRIBES
    -- its recipients instead (Divine Deflection's "you and/or permanents you
    -- control"), which CR 615.11 does not reach -- it names neither "each" nor an
    -- untargeted creature -- leaving CR 615.7's plain shield: one countdown,
    -- reduced by 1 for each 1 damage it prevents, whatever it is around. A card
    -- writing both spellings is read as the description alone, since the
    -- described row already covers whatever it admits and a second row would be a
    -- second shield the card never printed.
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        describedRecipient = (whatRecipient, whoRecipient)
        described = Maybe.isJust whatRecipient || Maybe.isJust whoRecipient
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (foldMap (objectRefRecipients legal resolving controller source gs) ref)
        -- The recipient each row this resolution installs bakes: one row naming
        -- nobody for a described shield, and one per named recipient otherwise.
        rows = if described then [Nothing] else fmap Just recipients
        -- CR 615.5: the additional effect, BAKED onto the row with this
        -- resolution's chosen targets and CR 109.5's "you".
        rider =
          if Seq.null riderEffects
            then Nothing
            else
              Just
                PreventionRider.MkPreventionRider
                  { PreventionRider.effects = riderEffects,
                    PreventionRider.targets = chosen,
                    PreventionRider.controller = controller,
                    -- CR 113.7's source: the rider needs an id to run against.
                    PreventionRider.source = source
                  }
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      -- An unevaluable quantity is a no-op, DealDamage's posture.
      Nothing -> pure ()
      Just n ->
        -- CR 615.7: a shield of 0 can prevent nothing, so none is installed --
        -- and neither is CR 609.7a's choice raised, a choice existing only to be
        -- baked into a row. The rows are counted for the same reason: CR 608.2b's
        -- gone target leaves a named shield no recipient, so there is nothing to
        -- shield. A DESCRIBED shield always has its one row, its recipients being
        -- read at the damage event rather than settled here.
        Monad.when (n > 0 && not (null rows)) $ do
          -- CR 609.7a: "the source is chosen when the effect is created", so the
          -- choice is made ONCE here and every shield this resolution installs
          -- watches the object it landed on.
          sourceChoice <- chooseDamageSource controller resolving context gs sourceFilter
          case (sourceFilter, sourceChoice) of
            -- The card asked for a source and CR 609.7a's pool was empty, so
            -- there is nothing to shield against and no row is installed --
            -- stricter than printed rather than weaker, which a row watching
            -- every source would be.
            (Just _, Nothing) -> pure ()
            _ ->
              State.modify' $ \g0 ->
                let amount = Integer.toNaturalSaturating n
                 in List.foldl' (installDamageRow (Binding.playersIn legal) (Filter.slotObjects context) controller source duration kind (DamageRewrite.PreventNext amount) rider (Filter.Type.And []) describedRecipient) g0 (fmap (\recipient -> (recipient, sourceChoice)) rows)
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration kind ref whatRecipient direction sourceFilter printedSource riderEffects) -> do
    -- CR 615.1 / 615.3: one floating shield per object the ref names, with no
    -- amount to count down. PreventNextDamage's row but for its rewrite, hence
    -- the shared `installDamageRow`; CR 615.7's "reduced to 0" terminator does
    -- not exist here, so only the duration ends it. The recipient side goes
    -- through Damage.damageRecipient for PreventNextDamage's reason (CR
    -- 120.1a); the source side is an ObjectId and needs no such translation.
    --
    -- ONE ROW ALTOGETHER for a card that DESCRIBES its recipients instead (Pack
    -- Leader's "to Dogs you control"), PreventNextDamage's split exactly: CR
    -- 611.2c leaves that set live, so the row carries the predicate and
    -- Replacement re-asks it at each damage event rather than sweeping it here.
    -- A DealtTo card writing both spellings is read as the description alone;
    -- beside DealtBy the two are not alternatives at all, the ref naming the
    -- SOURCE there and the description the recipients, so both ride the row.
    gs <- State.get
    let named = foldMap (objectRefRecipients legal resolving controller source gs) ref
        -- Through effectContext rather than Filter.contextFor, so CR 609.7a's
        -- candidates are narrowed against this resolution's own slot bindings
        -- and CR 109.5's "you" rather than against an empty slot map.
        context = effectContext gs controller source legal (slotBindings resolving gs)
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) named
        -- The recipient each row this resolution installs bakes: one row naming
        -- nobody for a described shield, and one per named recipient otherwise.
        rows = if Maybe.isJust whatRecipient then [Nothing] else fmap Just recipients
        -- CR 615.5's additional effect. With no amount to count down, "this way"
        -- is what THIS application prevented, which Prevention.amounts carries.
        rider =
          if Seq.null riderEffects
            then Nothing
            else
              Just
                PreventionRider.MkPreventionRider
                  { PreventionRider.effects = riderEffects,
                    PreventionRider.targets = chosen,
                    PreventionRider.controller = controller,
                    -- CR 113.7's source: the rider needs an id to run against.
                    PreventionRider.source = source
                  }
    -- Which SIDE of the damage event the ref's objects sit on, and which
    -- question the source half of the row answers.
    case direction of
      -- A DealtBy row watches CR 120.1's source, which is always an OBJECT, so a
      -- player the ref named drops out. A card describing no recipient names
      -- none on the row either, so the damage Dovin, Hand of Control's shielded
      -- permanent would deal to a PLAYER is prevented as much as the damage it
      -- would deal to a permanent.
      --
      -- The source half here is CR 601.2c's TARGET, and CR 609.7a's player
      -- CHOICE is a different question, so the chosenSource field is not read on
      -- this side: a card writing both would be naming two sources (see
      -- Pawl.Types.PreventAllDamage). It gets the trivial predicate beside the
      -- id, CR 615.9's recheck being of a CHOSEN source's printed properties,
      -- and a target has none -- CR 608.2b already rechecked its legality as the
      -- ability resolved.
      --
      -- CR 609.7b's PRINTED properties are a predicate rather than a second
      -- name, so they are threaded on this side too; every by-direction card in
      -- data/cards/ leaves them trivial.
      --
      -- The DESCRIBED recipient rides this side too: CR 615.1's shield watches a
      -- damage EVENT, and a card may narrow either end of it (Goblin Furrier's
      -- "prevent all damage that this creature would deal to snow creatures"), so
      -- the two halves conjoin on the row rather than the description being
      -- dropped for the source's sake. `rows` is not consulted here -- the fold is
      -- over the ids the ref named, and each row bakes CR 601.2c's source beside
      -- the live predicate.
      DamageDirection.DealtBy ->
        State.modify' $ \g0 -> List.foldl' (installDamageRow (Binding.playersIn legal) (Filter.slotObjects context) controller source duration kind DamageRewrite.PreventAll rider printedSource (whatRecipient, Nothing)) g0 (fmap (\oid -> (Nothing, Just (Filter.Type.And [], oid))) (Maybe.mapMaybe Recipient.objectOf named))
      DamageDirection.DealtTo ->
        -- No recipient is CR 608.2b's gone target, so there is nothing to shield
        -- and CR 609.7a's choice -- a choice existing only to be baked into a
        -- row -- is not raised either. PreventNextDamage's posture. A DESCRIBED
        -- shield always has its one row, its recipients being read at the damage
        -- event rather than settled here.
        Monad.unless (null rows) $ do
          -- CR 609.7a: "the source is chosen when the effect is created", so the
          -- choice is made ONCE here and every shield this resolution installs
          -- watches the object it landed on.
          sourceChoice <- chooseDamageSource controller resolving context gs sourceFilter
          case (sourceFilter, sourceChoice) of
            -- The card asked for a source and CR 609.7a's pool was empty, so
            -- there is nothing to shield against and no row is installed --
            -- stricter than printed rather than weaker, which a row watching
            -- every source would be.
            (Just _, Nothing) -> pure ()
            _ -> State.modify' $ \g0 -> List.foldl' (installDamageRow (Binding.playersIn legal) (Filter.slotObjects context) controller source duration kind DamageRewrite.PreventAll rider printedSource (whatRecipient, Nothing)) g0 (fmap (\recipient -> (recipient, sourceChoice)) rows)
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration kind amount srcRef whatRecipient whoRecipient destRef sourceFilter) -> do
    -- CR 614.9: install a floating redirection effect. BOTH sides are baked here,
    -- both being known only at resolution: the source side into
    -- DamagePattern.whichRecipient, the destination into the rewrite. Both
    -- through Damage.damageRecipient (CR 120.1a). The rule's own guard is re-asked
    -- at redirect time, in Event.apply, the destination being able to leave.
    --
    -- ONE ROW PER NAMED RECIPIENT, and ONE ROW ALTOGETHER for a card that
    -- DESCRIBES its recipients instead -- PreventNextDamage's split, for its
    -- reason: Harm's Way's "to you and/or permanents you control" is Divine
    -- Deflection's shape, one countdown read live at each damage event (CR
    -- 611.2c), where Carom's "target creature" is a recipient the resolution
    -- fixed (CR 601.2c). A card writing both spellings is read as the
    -- description alone.
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        recipientsOf ref = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
        -- Through effectContext rather than Filter.contextFor, for
        -- PreventAllDamage's reason: CR 609.7a's candidates are narrowed against
        -- this resolution's own slot bindings and CR 109.5's "you".
        context = effectContext gs controller source legal (slotBindings resolving gs)
        describedRecipient = (whatRecipient, whoRecipient)
        described = Maybe.isJust whatRecipient || Maybe.isJust whoRecipient
        redirected = foldMap recipientsOf srcRef
        -- The recipient each row this resolution installs bakes: one row naming
        -- nobody for a described redirection, and one per named recipient
        -- otherwise.
        rows = if described then [Nothing] else fmap Just redirected
        -- CR 615.7's counted shape on this rewrite, its amount evaluated ONCE as
        -- the effect is created (Harm's Way's "the next 2 damage"); Nothing is
        -- Turn the Tables' uncounted "all". A redirection of 0 can move nothing,
        -- so none is installed, PreventNextDamage's posture; an unevaluable
        -- amount is a no-op, DealDamage's.
        rewriteTo dest = case amount of
          Nothing -> Just (DamageRewrite.Redirect dest)
          Just quantity -> case Quantity.evaluateFor viewOf context gs resolving source quantity of
            Just n | n > 0 -> Just (DamageRewrite.RedirectNext (Integer.toNaturalSaturating n) dest)
            _ -> Nothing
    -- EXACTLY ONE destination (CR 614.9). None means CR 608.2b's target is
    -- already gone, so no row is installed.
    case fmap rewriteTo (recipientsOf destRef) of
      [Just rewrite] ->
        -- No recipient to redirect FROM is CR 608.2b's gone target, so there is
        -- nothing to install and CR 609.7a's choice -- a choice existing only to
        -- be baked into a row -- is not raised either. The prevention opcodes'
        -- posture. A DESCRIBED redirection always has its one row, its
        -- recipients being read at the damage event rather than settled here.
        Monad.unless (null rows) $ do
          -- CR 609.7a: "the source is chosen when the effect is created", so the
          -- choice is made ONCE here and every row this resolution installs
          -- watches the object it landed on.
          sourceChoice <- chooseDamageSource controller resolving context gs sourceFilter
          case (sourceFilter, sourceChoice) of
            -- The card asked for a source and CR 609.7a's pool was empty, so
            -- there is nothing to redirect and no row is installed -- stricter
            -- than printed rather than weaker, which a row watching every source
            -- would be.
            (Just _, Nothing) -> pure ()
            _ -> State.modify' $ \g0 -> List.foldl' (installDamageRow (Binding.playersIn legal) (Filter.slotObjects context) controller source duration kind rewrite Nothing (Filter.Type.And []) describedRecipient) g0 (fmap (\recipient -> (recipient, sourceChoice)) rows)
      _ -> pure ()
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase ref selector) -> do
    -- CR 614.1b: "skip" is a replacement effect, installed floating because a
    -- sorcery's skip outlives the sorcery (CR 614.3). CR 614.10a: one instance
    -- PER NAMED PLAYER, each with Uses.Once and its own row rather than merged,
    -- so "skip the next two" is two rows and Replacement.consume spends the one
    -- it applied.
    --
    -- Expiry.Never, and no Duration on the opcode: CR 614.3's other terminator
    -- never fires and the skip waits however many turns it must (CR 614.10a). The
    -- PhaseSelector goes in untouched: step or whole phase (CR 500.1) is card
    -- data.
    gs <- State.get
    let named = playerRefPlayers legal controller gs ref
        install pid g =
          let (ts, g1) = Game.freshTimestamp g
              active =
                ActiveReplacement.MkActiveReplacement
                  { ActiveReplacement.effect =
                      ReplacementEffect.PhaseR
                        PhasePattern.MkPhasePattern
                          { PhasePattern.whichPhase = selector,
                            -- The player the resolution named, baked now. Card
                            -- data cannot name one (see
                            -- Pawl.Types.PhasePattern).
                            PhasePattern.whosePhase = Just pid
                          },
                    ActiveReplacement.source = source,
                    -- CR 109.5: the resolution's controller, not `pid` above,
                    -- which the effect NAMED; nothing reads this, a PhaseR
                    -- resolving no ControllerRelation.
                    ActiveReplacement.controller = controller,
                    ActiveReplacement.timestamp = ts,
                    ActiveReplacement.expiry = Expiry.Type.Never,
                    ActiveReplacement.uses = Uses.Once,
                    ActiveReplacement.origin = ReplacementOrigin.Other,
                    -- No clause: CR 614.10a's skip states none (see
                    -- Pawl.Types.ActiveReplacement).
                    ActiveReplacement.condition = Nothing,
                    ActiveReplacement.rider = Nothing,
                    ActiveReplacement.slots = Map.empty
                  }
           in g1 {GameState.replacements = active : GameState.replacements g1}
    State.modify' (\g -> List.foldl' (flip install) g named)
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration affected playerEffect) ->
    -- CR 611.1 / 613.11: install the stored player effect. CR 109.5: the
    -- CONTROLLER is baked in now, the source having none to project once it
    -- leaves the stack. A SCOPED set is not baked, CR 611.2c letting a
    -- rules-modifying effect reach objects it did not begin with, so it is
    -- re-resolved on every read. A NAMED set is baked, the bindings that answer a
    -- target slot (CR 601.2c) being gone once this resolution is over; an
    -- unfilled or illegal slot stores nothing (CR 608.2b).
    State.modify' $ \gs -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let baked = case affected of
              AffectedPlayers.Scoped scope -> [AffectedPlayers.Scoped scope]
              -- Through playerRefPlayers so the slot is read exactly as every
              -- other opcode reads one, CR 608.2b's empty answer included.
              AffectedPlayers.Named slot ->
                fmap AffectedPlayers.Named (playerRefPlayers legal controller gs (PlayerRef.InSlot slot))
            install g scope =
              let (ts, g1) = Game.freshTimestamp g
                  active =
                    ActivePlayerEffect.MkActivePlayerEffect
                      { ActivePlayerEffect.source = source,
                        ActivePlayerEffect.controller = controller,
                        ActivePlayerEffect.timestamp = ts,
                        ActivePlayerEffect.expiry = expiry,
                        ActivePlayerEffect.scope = scope,
                        ActivePlayerEffect.effect = playerEffect
                      }
               in g1 {GameState.playerEffects = active : GameState.playerEffects g1}
         in List.foldl' install gs baked
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration blockerRef attackerRef) ->
    -- CR 509.1c / 613.11: store one requirement per (blocker, attacker) pair the
    -- two refs name, rule 509.1c counting requirements PER CREATURE. Both sets are
    -- enumerated ONCE, for the CR 608.2f simultaneity objectRefObjects buys; an
    -- illegal slot (CR 608.2b) stores nothing, which is provoke's fizzle.
    State.modify' $ \gs -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let blockers = objectRefObjects legal resolving controller source gs blockerRef
            attackers = objectRefObjects legal resolving controller source gs attackerRef
            (ts, gs1) = Game.freshTimestamp gs
            stored =
              [ ActiveBlockRequirement.MkActiveBlockRequirement
                  { ActiveBlockRequirement.source = source,
                    ActiveBlockRequirement.timestamp = ts,
                    ActiveBlockRequirement.expiry = expiry,
                    ActiveBlockRequirement.blocker = blocker,
                    ActiveBlockRequirement.attacker = attacker
                  }
              | blocker <- blockers,
                attacker <- attackers
              ]
         in gs1 {GameState.blockRequirements = stored <> GameState.blockRequirements gs1}
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration ref) ->
    -- CR 701.19c / 611.1: store one prohibition per permanent the ref names.
    -- RequireBlock above is the model and its arguments carry over: the ref is
    -- enumerated ONCE, for the CR 608.2f simultaneity objectRefObjects buys, and
    -- an illegal slot (CR 608.2b) stores nothing, which is Hurr Jackal's fizzle.
    --
    -- Nothing is written onto the permanent itself. CR 701.19c makes this a
    -- property the DESTRUCTION acquires, so the row is read at
    -- Event.resolveDestruction and never by a projection.
    State.modify' $ \gs -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let objects = objectRefObjects legal resolving controller source gs ref
            (ts, gs1) = Game.freshTimestamp gs
            stored =
              [ ActiveUnregeneratable.MkActiveUnregeneratable
                  { ActiveUnregeneratable.source = source,
                    ActiveUnregeneratable.timestamp = ts,
                    ActiveUnregeneratable.expiry = expiry,
                    ActiveUnregeneratable.object = object
                  }
              | object <- objects
              ]
         in gs1 {GameState.unregeneratables = stored <> GameState.unregeneratables gs1}
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock duration ref) ->
    -- CR 509.1b / 611.1: store one restriction per permanent the ref names.
    -- CantBeRegenerated above is the model and its arguments carry over: the ref
    -- is enumerated ONCE, for the CR 608.2f simultaneity objectRefObjects buys,
    -- and an illegal slot (CR 608.2b) stores nothing, which is Zirda's fizzle.
    --
    -- Nothing is written onto the permanent itself, and nothing is projected: CR
    -- 613.11 keeps a restriction on a declaration out of the layers, so the row
    -- is read at Pawl.Engine.CombatRestriction.blockProhibited and never by a
    -- projection.
    State.modify' $ \gs -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let objects = objectRefObjects legal resolving controller source gs ref
            (ts, gs1) = Game.freshTimestamp gs
            stored =
              [ ActiveBlockProhibition.MkActiveBlockProhibition
                  { ActiveBlockProhibition.source = source,
                    ActiveBlockProhibition.timestamp = ts,
                    ActiveBlockProhibition.expiry = expiry,
                    ActiveBlockProhibition.object = object
                  }
              | object <- objects
              ]
         in gs1 {GameState.blockProhibitions = stored <> GameState.blockProhibitions gs1}
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration affected aimedAt) ->
    -- CR 508.1c / 611.1: store one restriction per permanent a Named ref names,
    -- or ONE row for a Matching class. ForbidBlock above is the model for the
    -- first and every one of its arguments carries over: the ref is enumerated
    -- ONCE, for the CR 608.2f simultaneity objectRefObjects buys, and an illegal
    -- slot (CR 608.2b) stores nothing, which is Netter en-Dal's fizzle.
    --
    -- The class is NOT enumerated, CR 611.2c's third sentence: a restriction on a
    -- declaration modifies no characteristic and no controller, so it reaches
    -- creatures that enter after this resolution, and the Filter is stored to be
    -- re-read at each declaration. Its bound player slots are baked now,
    -- Expiry.arm's reason for baking a ForAsLongAs condition -- the bindings that
    -- answer them are gone once this resolution is over.
    --
    -- CR 109.5's controller is baked, AffectPlayers' reason: the source is a
    -- sorcery in a graveyard by the time "you" is asked of the row.
    --
    -- Nothing is written onto the permanent itself, and nothing is projected: CR
    -- 613.11 keeps a restriction on a declaration out of the layers, so the row
    -- is read at Pawl.Engine.CombatRestriction.attackProhibited (or, aimed, at
    -- Pawl.Engine.CombatRestriction.cantAttackPlayer) and never by a projection.
    State.modify' $ \gs -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let subjects = case affected of
              RestrictedCreatures.Named ref -> fmap RestrictedCreatures.Named (objectRefObjects legal resolving controller source gs ref)
              RestrictedCreatures.Matching f -> [RestrictedCreatures.Matching (Filter.bakeBound (Binding.playersIn legal) f)]
            (ts, gs1) = Game.freshTimestamp gs
            stored =
              [ ActiveAttackProhibition.MkActiveAttackProhibition
                  { ActiveAttackProhibition.source = source,
                    ActiveAttackProhibition.controller = controller,
                    ActiveAttackProhibition.timestamp = ts,
                    ActiveAttackProhibition.expiry = expiry,
                    ActiveAttackProhibition.affected = subject,
                    ActiveAttackProhibition.aimedAt = aimedAt
                  }
              | subject <- subjects
              ]
         in gs1 {GameState.attackProhibitions = stored <> GameState.attackProhibitions gs1}
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration attackerRef defenderRef) ->
    -- CR 508.1d / 613.11: store one requirement per (attacker, defender) pair the
    -- two refs name, rule 508.1d counting requirements PER CREATURE. RequireBlock
    -- above is the twin, and its arguments carry over: both sets are enumerated
    -- ONCE for CR 608.2f's simultaneity, and an illegal slot (CR 608.2b) stores
    -- nothing, which is Alluring Siren's fizzle.
    State.modify' $ \gs -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let attackers = objectRefObjects legal resolving controller source gs attackerRef
            -- Through playerRefPlayers so the ref is read exactly as every other
            -- opcode reads one, CR 608.2b's empty answer included.
            defenders = playerRefPlayers legal controller gs defenderRef
            (ts, gs1) = Game.freshTimestamp gs
            stored =
              [ ActiveAttackRequirement.MkActiveAttackRequirement
                  { ActiveAttackRequirement.source = source,
                    ActiveAttackRequirement.timestamp = ts,
                    ActiveAttackRequirement.expiry = expiry,
                    ActiveAttackRequirement.attacker = attacker,
                    ActiveAttackRequirement.defender = defender
                  }
              | attacker <- attackers,
                defender <- defenders
              ]
         in gs1 {GameState.attackRequirements = stored <> GameState.attackRequirements gs1}
  Effect.CreateEmblem card -> do
    -- CR 114.2: the resolving controller gets the emblem, minted by
    -- Event.createEmblem.
    _ <- Event.createEmblem controller card
    pure ()
  Effect.BecomeMonarch target -> do
    gs <- State.get
    let newMonarch = case target of
          MonarchTarget.TheController -> Just controller
          -- CR 725.2: the controller of the ability's bound source, read from the
          -- reserved trigger-source slot.
          --
          -- CR 608.2h's last known information, not the live board: the creature
          -- that stole the crown by connecting is routinely dead by the time this
          -- resolves -- it traded with its blocker in the same damage step -- and
          -- a live read would crown nobody. Same reading as
          -- Event.combatDamagerAgainst, which bound the slot.
          MonarchTarget.ControllerOfSource ->
            Map.lookup Binding.triggerSource chosen
              >>= Binding.onlyOne
              >>= Recipient.objectOf
              >>= (\o -> Projection.controllerWithLastKnown o gs)
          -- CR 601.2c's chosen player, re-checked under CR 608.2b: the slot is a
          -- TARGET, so an illegal one crowns nobody while the rest of the ability
          -- still resolves.
          MonarchTarget.InSlot slot ->
            case legalOne slot legal of
              Just (Recipient.ToPlayer crowned) -> Just crowned
              _ -> Nothing
    case newMonarch of
      Nothing -> pure ()
      -- CR 101.2: a "can't become the monarch" effect outranks this instruction
      -- and the crown does not move. Read here rather than at the three
      -- MonarchTarget arms, so every route this opcode has is stopped at once.
      Just p | PlayerEffect.prohibitsBecomingMonarch p gs -> pure ()
      -- CR 725.3's handoff, the CR 603.2 event and CR 725's exile watches are all
      -- Monarch.crown's, so no caller can move the crown without them.
      Just p -> State.modify' (Monarch.crown p)
  -- CR 726.1: the initiative moves, whichever InitiativeTarget named the taker.
  Effect.TakeTheInitiative target -> do
    gs <- State.get
    let taker = case target of
          InitiativeTarget.TheController -> Just controller
          -- CR 726.2: the controller of the ability's bound source, read from the
          -- reserved trigger-source slot. CR 608.2h's last known information, for
          -- Effect.BecomeMonarch ControllerOfSource's reason one rule over.
          InitiativeTarget.ControllerOfSource ->
            Map.lookup Binding.triggerSource chosen
              >>= Binding.onlyOne
              >>= Recipient.objectOf
              >>= (\o -> Projection.controllerWithLastKnown o gs)
    case taker of
      Nothing -> pure ()
      -- CR 726.3's hand-off, CR 726.5's re-take and the CR 603.2 event are all
      -- Initiative.takeInitiative's, so no caller can move the designation
      -- without them. No eligibility gate, where the crown has one: rule 726
      -- states no "can't take the initiative" effect.
      Just p -> State.modify' (Initiative.takeInitiative p)
  -- The slot's permanent gains the designation (CR 702.112a, CR 701.37a, CR
  -- 701.60a) -- a state write, not a CR 613 modification.
  --
  -- GameEvent.BecameDesignated is emitted only on a TRANSITION (CR 701.60d); a
  -- player recipient, an illegal slot (CR 608.2b) and an id naming no object all
  -- write nothing. CR 701.60c's menace and can't-block are read off
  -- Object.designations live.
  Effect.Designate (Designate.MkDesignate designation slot) ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          gs <- State.get
          Monad.when (maybe False (not . Set.member designation . Object.designations) (Game.lookupObject target gs)) $ do
            State.modify'
              (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.designations = Set.insert designation (Object.designations o)}) target (GameState.objects g)})
            State.modify' (Event.recordEvent (GameEvent.BecameDesignated (BecameDesignated.MkBecameDesignated designation target)))
      _ -> pure ()
  -- CR 716.2a: "This Class's level becomes N." A state write on the slot's
  -- permanent, not a CR 613 modification -- CR 716.2b makes a level a designation,
  -- which is what Object.classLevel holds and what the Designate arm above writes
  -- the boolean marks into.
  --
  -- BECOMES rather than increments, and the level bar's own
  -- ActivatedAbility.condition is the only thing that keeps the ladder in order
  -- (CR 716.2a's "activate only if this Class is level N-1"). Nothing is re-checked
  -- here, which is CR 113.7a: once activated, an ability exists on the stack
  -- independently of its source, so a level that moved in between changes nothing.
  --
  -- A player recipient, an illegal slot (CR 608.2b) and an id naming no object all
  -- write nothing -- Designate's postures.
  --
  -- GameEvent.ClassLevelSet is what TriggerCondition.SelfBecomesClassLevel watches,
  -- and it is emitted only on a TRANSITION -- the Designate arm above's posture.
  -- CR 716.2a's ladder cannot set a level a Class already has, but this opcode is
  -- not the ladder, and a CR 603.2 event for a change that did not happen would
  -- fire "when this Class becomes level N" for a level it never became. The level
  -- BEFORE is read through CR 716.2d's default, so a Class that has never been
  -- levelled crosses from 1.
  --
  -- The WRITE is unguarded, unlike Designate's: writing the designation is not
  -- idempotent where the level was absent, and CR 716.2b makes having one
  -- observable (a copy reads CR 716.2d's default instead). So only the event turns
  -- on the transition.
  --
  -- That guard is a REGRESSION FENCE rather than a proven behaviour: no card in
  -- `data/cards/` carries Effect.SetClassLevel outside a level bar, and a bar
  -- cannot set the level it already has, so removing it leaves the suite green.
  -- It is kept because rule 603.2 states it, not because a test pays for it.
  --
  -- One writer, every road: this is the only place in the engine that CHANGES a
  -- permanent's level. Everywhere else naming Object.classLevel builds a fresh
  -- object at CR 400.7 with no level at all, so there is no second road to record
  -- on.
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel level slot) ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          gs <- State.get
          State.modify'
            (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.classLevel = Just level}) target (GameState.objects g)})
          case fmap (ClassLevel.MkClassLevel . ClassLevel.defaulted . Object.classLevel) (Game.lookupObject target gs) of
            Just before | before /= level -> State.modify' (Event.recordEvent (GameEvent.ClassLevelSet (ClassLevelChange.MkClassLevelChange target before level)))
            _ -> pure ()
      _ -> pure ()
  -- CR 701.60a's other ending: undoes Designate's write for that one designation.
  -- CR 701.60c's menace and can't-block are read off the set live, so nothing
  -- else has to be undone. An illegal slot (CR 608.2b), a player recipient and a
  -- set that matched nothing all arrive as the empty list.
  Effect.Unsuspect ref ->
    State.modify' $ \gs ->
      let unsuspect o = o {Object.designations = Set.delete Designation.Suspected (Object.designations o)}
       in gs
            { GameState.objects =
                foldr (Map.adjust unsuspect) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
  -- CR 709.5f and CR 709.5g, one arm because the two rules are one sentence with
  -- two words swapped: choose a half of the slot's permanent that the setting
  -- admits, and give or take the appropriate unlocked designation.
  --
  -- The CANDIDATES come from Pawl.Engine.Room's CR 709.5c derivation -- rule
  -- 709.5g's "an unlocked half" for a lock, rule 709.5f's "a locked half" for an
  -- unlock -- so the prompt cannot offer a half the instruction forbids, and the
  -- answer is FILTERED back through them rather than trusted. Rule 709.5c's own
  -- scope is why an object that is not a permanent with a shared type line on the
  -- battlefield offers nothing.
  --
  -- No candidate leaves the instruction doing nothing, CR 101.3: a Room with
  -- every door already shut cannot be locked further. A SINGLE candidate is not
  -- asked about, both rules leaving nothing to choose.
  --
  -- A player recipient, an illegal slot (CR 608.2b) and an id naming no object
  -- all write nothing -- Designate's postures.
  --
  -- Casing on `locked` and on `every` is not casing on an effect's identity: both
  -- are payloads of one opcode, which is the argument Effect.Designate's own
  -- haddock makes.
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked every locked slot) ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          gs <- State.get
          let halves = fmap Face.name (if locked then Room.unlockedHalves target gs else Room.lockedHalves target gs)
          case halves of
            [] -> pure ()
            first : rest
              -- "each locked door", which names them rather than choosing among
              -- them, so CR 709.5f's "chooses" has nothing to ask and no prompt
              -- is raised. The unlock is ONE write for CR 709.5i's sake; the
              -- lock is a fold, rule 709.5g having no completion to record.
              | every ->
                  if locked
                    then Monad.mapM_ (Event.lockHalf target) halves
                    else Event.unlockHalves controller target (Set.fromList halves)
              | otherwise -> do
                  half <- case rest of
                    [] -> pure first
                    second : more -> do
                      let offered = first NonEmpty.:| (second : more)
                      answered <- Game.choose (Prompt.ChooseHalf (Decide.deciderFor controller gs) controller target offered)
                      pure (if List.elem answered (NonEmpty.toList offered) then answered else first)
                  if locked then Event.lockHalf target half else Event.unlockHalves controller target (Set.singleton half)
      _ -> pure ()
  -- CR 702.100a's counter and CR 702.100b's marker: the creature evolves only if
  -- the placement actually put one or more counters on it.
  Effect.Evolve slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          placed <- Event.putCounters (CounterCause.ByEffect controller) target CounterKind.PlusOnePlusOne 1
          Monad.when (placed > 0) (State.modify' (Event.recordEvent (GameEvent.Evolved target)))
      _ -> pure ()
  -- CR 702.134a's counter and CR 702.134c's marker: rule 702.134c fires on the
  -- mentor ability RESOLVING, so the event is recorded however many counters CR
  -- 614.16 left to place. The event names the resolving ability's SOURCE and the
  -- slot's creature, in that order.
  Effect.Mentor slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          _ <- Event.putCounters (CounterCause.ByEffect controller) target CounterKind.PlusOnePlusOne 1
          State.modify' (Event.recordEvent (GameEvent.Mentored (Mentored.MkMentored source target)))
      _ -> pure ()
  -- CR 702.149a's counter and CR 702.149c's marker, with Evolve's gate: a
  -- placement CR 614.16 replaced away to nothing trains nobody. The event names
  -- the slot's creature, rule 702.149c's "this creature".
  Effect.Train slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          placed <- Event.putCounters (CounterCause.ByEffect controller) target CounterKind.PlusOnePlusOne 1
          Monad.when (placed > 0) (State.modify' (Event.recordEvent (GameEvent.Trained target)))
      _ -> pure ()
  -- CR 731.1: the GAME gains the designation; what that entails is
  -- Pawl.Engine.Daytime's. Nobody is named and nothing is prompted.
  Effect.ItBecomes designation -> do
    _ <- Daytime.becomes Event.recordTransformed designation
    pure ()
  -- CR 701.3a / 702.6a: the SOURCE moves, relocating it if it is already
  -- attached elsewhere; AttachTarget below moves the slot's TARGET instead.
  -- Event.attach is the one funnel CR 701.3's move goes through, holding rule
  -- 701.3b, CR 701.3c's restamp and the GameEvent.BecameAttached.
  Effect.Attach slot -> Foldable.for_ (legalOne slot legal) (Event.attach source)
  -- CR 701.3a, the other direction: the SLOT's target moves, to a destination
  -- chosen now rather than targeted.
  Effect.AttachTarget (AttachTarget.MkAttachTarget slot filter_) ->
    case legalOne slot legal of
      -- An unfilled slot, or one CR 608.2b has since made illegal: no-op.
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient: nothing on the battlefield moves
        Just subject -> do
          gs <- State.get
          -- The destinations the card's own TEXT admits, narrowed to legal ones
          -- only when the card SAYS so (Filter.CanHostSubject); narrowing
          -- otherwise would answer CR 303.4j on the player's behalf. CR 609.3
          -- when Attach.hostsFor is empty. The elision at one candidate is
          -- Attach.chooseHost's, not re-derived for an optional attach.
          destination <- Attach.chooseHost controller subject (Attach.hostsFor controller source subject filter_ gs)
          -- Proposed as a bare ToObject; Event.attach re-tags it the way the
          -- subject's own enchant slot references it, and applies CR 303.4j's
          -- refusal. Always a different object than the current host, so CR
          -- 701.3c's restamp is always earned.
          Monad.mapM_ (Event.attach subject . Recipient.ToObject) destination
      _ -> pure ()
  -- CR 303.4d / CR 301.5c: the same move, with the destination filter read as
  -- "each" rather than "one of". The candidate list is built exactly as
  -- AttachTarget builds it -- same filter, same Context, so CR 109.5's "you" on
  -- the card's own text stays the ASKING ability's -- and only the arbitration
  -- differs: Attach.arbitrate asks the SUBJECT's controller instead of the
  -- resolving controller. Everything downstream is shared, so CR 303.4j's
  -- refusal, CR 701.3b's no-op and CR 701.3c's restamp cannot diverge between
  -- the two opcodes.
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot filter_) ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just subject -> do
          gs <- State.get
          destination <- Attach.arbitrate subject (Attach.hostsFor controller source subject filter_ gs)
          Monad.mapM_ (Event.attach subject . Recipient.ToObject) destination
      _ -> pure ()
  -- CR 701.3a's third arrangement, and the one the other two cannot spell: the
  -- MOVER is whatever a binding names -- Sigarda's Aid's "it", CR 400.7e's
  -- entrant under Binding.became -- and the DESTINATION is targeted rather than
  -- found now. Nothing is chosen here, which is the observable difference from
  -- AttachTarget above: the destination was fixed at CR 603.3d, so shroud
  -- refused it then (CR 702.18a) and CR 608.2b has already dropped it from
  -- `legal` if it went illegal, leaving this a no-op.
  --
  -- The mover is read through objectRefObjects rather than legalOne so that a
  -- GROUP binding names every member (CR 712.21c's two cards, say); each is
  -- attached to the one destination, which CR 301.5c permits since the limit it
  -- states is on the Equipment and not on the creature.
  Effect.AttachBound (AttachBound.MkAttachBound subject destination) ->
    case legalOne destination legal of
      Just recipient -> do
        gs <- State.get
        Foldable.for_ (objectRefObjects legal resolving controller source gs (ObjectRef.InSlot subject)) $ \mover ->
          Event.attach mover recipient
      -- An unfilled slot, or one CR 608.2b has since made illegal: no-op.
      Nothing -> pure ()
  Effect.ExileUntilMonarch slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          -- CR 400.7: exile through the funnel and register the incarnation for
          -- return when an opponent of `controller` (CR 102.2) BECOMES the
          -- monarch. Armed undischarged whoever holds the crown now, so an
          -- opponent who already holds it does not free the creature.
          --
          -- One watch per ARRIVAL, which is CR 712.21c: an effect that can find
          -- the new object a melded permanent becomes finds both cards, and "the
          -- same actions are taken upon each of them" -- so both come back when
          -- an opponent becomes the monarch.
          mNew <- Event.changeZoneReturning target Zone.Exile
          Monad.forM_ mNew $ \newId -> do
            let watch =
                  MonarchWatch.MkMonarchWatch
                    { MonarchWatch.controller = controller,
                      MonarchWatch.due = False
                    }
            State.modify' (\g -> g {GameState.exiledUntilMonarch = Map.insert newId watch (GameState.exiledUntilMonarch g)})
      _ -> pure ()
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting card slot) ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just haunted -> do
          -- The card is read LIVE off the resolving object, never out of
          -- `chosen`: rule 702.55a's "it" is the graveyard incarnation the death
          -- minted (CR 400.7e), and CR 115.10a makes it no target.
          mCard <- State.gets (slotOne card resolving)
          case mCard of
            Nothing -> pure ()
            Just oid -> do
              -- CR 400.7 mints the exiled incarnation; CR 702.55b's link is filed
              -- against THAT id, which puts the ability in exile for CR 113.6k. A
              -- cancelled move (CR 614.6) leaves no link.
              --
              -- The link names the object the ability TARGETED, so it goes on
              -- matching after that object has stopped being a creature, and is
              -- what TriggerCondition.HauntedCreatureDies compares against.
              mNew <- Event.changeZoneReturning oid Zone.Exile
              Monad.forM_ mNew $ \newId ->
                State.modify' (\g -> g {GameState.haunting = Map.insert newId haunted (GameState.haunting g)})
      _ -> pure ()
  Effect.Counter (Counter.MkCounter ref mSlot mSources) -> do
    gs <- State.get
    let named = objectRefObjects legal resolving controller source gs ref
        -- CR 113.7: each named ability's source, read BEFORE the funnel runs --
        -- CR 608.2n makes a countered ability cease, so afterwards there is no
        -- object left to walk from.
        sourceOf = Map.fromList (Maybe.mapMaybe (\oid -> fmap ((,) oid) (Game.abilitySourceOf oid gs)) named)
    -- CR 701.6a: counter each named object through the single funnel, which picks
    -- that rule's ending from each object's own kind. A player recipient or an
    -- illegal target (CR 608.2b) counters nothing. The whole set goes as ONE
    -- batch (CR 608.2f).
    --
    -- The funnel is handed THIS effect's source and controller, which Baral,
    -- Chief of Compliance reads off the event: by the CR 117.5 trigger scan the
    -- controller can no longer be asked for exactly (see Pawl.Types.Countering).
    countered <- Event.counterReturning source controller named
    -- CR 701.6a's "countered this way" is what the funnel COUNTERED, never what
    -- the sweep named. Bound onto this effect's SOURCE, and bound even at zero.
    Monad.forM_ mSlot $ \slot ->
      State.modify' (bindAmountSlot source slot (Natural.length countered))
    -- CR 113.7: the PERMANENTS whose abilities the funnel countered, for Green
    -- Slime's "if a permanent's ability is countered this way, destroy that
    -- permanent". The funnel's answer walked to its sources, never the sweep's;
    -- a countered spell has no source. Read off the board AFTER the funnel, so a
    -- source that has left the battlefield -- sacrificed to activate the ability
    -- (CR 113.7a) -- is no permanent and is not bound. Bound onto `resolving` as a
    -- GROUP, Destroy's `permanents` shape and for its reason; nothing is bound
    -- when nothing qualifies, so the rider finds an unbound slot and does nothing.
    --
    -- The battlefield conjunct is the rule's word "permanent" and a REGRESSION
    -- FENCE rather than a proved behaviour: Event.destroy refuses a card in a
    -- graveyard on its own, so dropping the conjunct leaves the suite green
    -- (Pawl.CounterspellSpec's Golden Egg case).
    Monad.forM_ mSources $ \slot -> do
      after <- State.get
      let permanents = Set.toList (Set.fromList (filter (\src -> Set.member src (GameState.battlefield after)) (Maybe.mapMaybe (\oid -> Map.lookup oid sourceOf) countered)))
      Monad.unless (null permanents) (State.modify' (bindObjectsSlot resolving slot (Seq.fromList permanents)))
  Effect.PutCounters (PutCounters.MkPutCounters kind quantity ref) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- CR 608.2c: the set is swept as this instruction is reached, and an
        -- illegal slot (CR 608.2b) or a player recipient answers with nobody.
        targets = objectRefObjects legal resolving controller source gs ref
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
      -- ONE evaluation for the whole set (CR 608.2f), then CR 122.6's funnel per
      -- recipient, so CR 614's counter replacements apply against each placement.
      --
      -- ONE event for the PLACEMENTS too, which is the Event.simultaneously
      -- bracket: CR 608.2f processes an action taken on multiple objects
      -- simultaneously, and one written instruction is one such action however
      -- many permanents the ref swept. CR 603.2c's batch conditions are what a
      -- board can read that with -- TriggerCondition.PermanentsGetCounters fires
      -- once for the sweep where its per-permanent twin fires once per recipient,
      -- and Pawl.Engine.Event.batchScoped is that fork.
      --
      -- The per-recipient CR 122.6 funnel below is no argument for N groups: CR
      -- 616.1g is where the rules say events nest at all -- a replacement may
      -- apply to "an event contained within the first event" -- so CR 614's
      -- per-placement replacement opportunity is not a second event. The swept
      -- Effect.MoveToZone arm above runs the same shape, and Event.destroyIn a
      -- whole CR 616.1 loop, inside one bracket.
      --
      -- Inside the `n > 0` guard rather than around it, so an instruction placing
      -- no counters spends no group; and around the FOLD alone, the sweep above it
      -- having taken no action for CR 608.2f to be read over.
      Just n ->
        Monad.when (n > 0) . Event.simultaneously . Monad.forM_ targets $ \target ->
          Event.putCounters (CounterCause.ByEffect controller) target kind (Integer.toNaturalSaturating n)
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom fromSlot kind ref) -> do
    gs <- State.get
    -- CR 122.8: put the counters the `from` object HAD onto every permanent the
    -- ref names -- "the player puts the same number of each kind of counter the
    -- first object had onto the second object".
    --
    -- A PUT and nothing else: rule 122.8's first sentence says the player
    -- "doesn't move counters from one object to the other", so unlike the CR
    -- 122.5 arm below there is no removal, no atomicity to enforce and none of
    -- that rule's four impossibilities to check. CR 122.2 is why -- the first
    -- object's counters ceased to exist as it changed zones, and what rule 122.8
    -- reads is the tally, not the counters.
    --
    -- The tally comes off the VIEW rather than off Object.counters, which is what
    -- makes CR 608.2h answer it: `effectViewOf` hands the resolving SOURCE its
    -- last known information, and Iron Apprentice's "when this creature dies" is
    -- read off the source itself. A `from` slot naming a departing BYSTANDER is
    -- Binding.departedPermanent, the other id effectViewOf looks back for --
    -- Resourceful Defense's "whenever a permanent you control leaves the
    -- battlefield", proved in Pawl.PutCounterSpec.
    let viewOf = effectViewOf source legal gs
        -- CR 608.2c: the set is swept as this instruction is reached, and an
        -- illegal slot (CR 608.2b) or a player recipient answers with nobody.
        targets = objectRefObjects legal resolving controller source gs ref
        -- CR 122.8's second sentence: an ability that "specifies what kind(s) of
        -- counters to place" places "the same number of each of those kinds of
        -- counter the first object had", so a named kind narrows the tally to
        -- itself -- Selfless Police Captain's "put its +1/+1 counters", where
        -- Iron Apprentice's "put those counters" names none and takes all.
        narrow m = maybe m (Map.restrictKeys m . Set.singleton) kind
        -- Ascending (Map.toList), so a transcript is deterministic. A kind
        -- recorded at zero is dropped: it is not a kind the object HAD.
        tally = case legalOne fromSlot legal >>= Recipient.objectOf of
          Nothing -> Map.empty
          Just oid -> narrow (Map.filter (> 0) (maybe Map.empty Filter.counters (viewOf oid)))
    -- ONE event for the placements, the PutCounters arm's Event.simultaneously
    -- bracket and its reasons (CR 608.2f), spent only when something crosses.
    -- ONE call per kind per permanent inside it, which is CR 614.16's own unit.
    Monad.unless (Map.null tally) . Event.simultaneously . Monad.forM_ targets $ \target ->
      Monad.forM_ (Map.toList tally) (uncurry (Event.putCounters (CounterCause.ByEffect controller) target))
  -- CR 201.4 via CR 608.2c: the resolving controller names a card, and the name
  -- is stamped on the SOURCE so a later clause of the same resolution can read it
  -- (Ancient Vendetta). Object.chosenNames is the same store CR 614.1c's
  -- as-enters twin writes (Pawl.Engine.Event's EntryRewrite.ChooseCardNames arm),
  -- and Pawl.Engine.Filter's HasChosenName is the one reader on the match side.
  --
  -- CHOOSE, not target: no CR 608.2b legality to re-check, and the prompt is
  -- raised unconditionally because CR 201.4's offer is every card in the Oracle
  -- card reference -- there is no candidate list to be short, so the "ask only
  -- where two or more make it a choice" shortcut the sibling prompts take cannot
  -- apply here.
  --
  -- The answer is NOT filtered against the restriction, the posture the entry
  -- twin already takes: pawl holds no Oracle card reference, so there is nothing
  -- to check it against (#663).
  --
  -- Set.insert rather than a fresh singleton: CR 201.4g's interchangeable names
  -- aside, nothing in rule 201.4 says a second choice unmakes the first, and a
  -- source that chose as it entered keeps that name too. No printed card chooses
  -- twice, so the two readings agree on the pool.
  --
  -- Written to the SOURCE and not to `resolving`: Pawl.Engine.PlayerEffect
  -- .chosenNamesOf and the search arm's context both ask about a source (CR
  -- 113.7), and for a spell the two ids are the same object anyway.
  Effect.ChooseCardName restriction -> do
    gs <- State.get
    answer <- Game.choose (Prompt.ChooseCardName (Decide.deciderFor controller gs) controller source restriction)
    let stamp o = o {Object.chosenNames = Set.insert answer (Object.chosenNames o)}
    State.modify' $ \g -> g {GameState.objects = Map.adjust stamp source (GameState.objects g)}
  -- CR 400.11c: the resolving controller reveals a card they own from outside the
  -- game matching the filter and puts it into their hand -- Burning Wish.
  --
  -- The CONTROLLER and not the owner of the source, because CR 109.5 makes the
  -- card's "you" the player the effect is applied for; the two differ for a spell
  -- whose control was gained. The SOURCE goes along only for the filter's context
  -- (CR 113.7), the same pair the search arm's own context is built from.
  --
  -- Pawl.Engine.OutsideTheGame is the whole of it, and this arm asks nothing about
  -- which effect the filter came from.
  Effect.FromOutsideTheGame payload -> OutsideTheGame.bringInto payload source controller
  -- CR 608.2n: "Exile Burning Wish" -- the resolving SPELL goes to exile as this
  -- instruction runs rather than to its owner's graveyard when the resolution
  -- ends. finishSpell's move afterwards finds nothing, CR 400.7 having minted a
  -- fresh incarnation in exile.
  --
  -- `resolving` and not `source`: the two coincide for a spell, and this arm is
  -- about the object ON THE STACK. The faceOf gate is what makes an ability inert
  -- here -- CR 113.7a's ability object has no card behind it, so exiling it would
  -- take the ability off the stack in the middle of its own resolution.
  Effect.ExileThisSpell -> do
    gs <- State.get
    Monad.when (Maybe.isJust (Game.faceOf resolving gs)) (Event.changeZone resolving Zone.Exile)
  -- CR 122: PutCounters' mirror, deliberately NOT through a CR 614.16 gate --
  -- nothing in CR 614 replaces a removal.
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters kind quantity slot) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient has no object counters
        Just target -> case Quantity.evaluateFor viewOf context gs resolving source quantity of
          Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
          Just n -> Monad.when (n > 0) (Event.removeCounters target kind (Integer.toNaturalSaturating n))
      _ -> pure () -- illegal slot at resolution (CR 608.2b): no-op
  Effect.MoveCounters (MoveCounters.MkMoveCounters fromRef kinds mSlot toRef) -> do
    -- CR 122.5: move counters off one permanent and onto a second. WHICH kinds
    -- cross is the card's call when it names one (Explorer's Cache's "move a
    -- +1/+1 counter" and Spike Cannibal's "move all +1/+1 counters"), the
    -- player's when it names none (Agent's Toolkit's "move a counter",
    -- Resourceful Defense's "move any number of counters" and Takesies' "up to
    -- one counter", which alone lets the player decline, and Goldberry,
    -- River-Daughter's "one or more counters", which alone puts a floor under
    -- the answer), shared when the card
    -- names the kind and the player supplies the count (Scrounging Bandar's
    -- "move any number of +1/+1 counters"), and neither's when the card takes
    -- them all (Fate Transfer's "move all counters") or reads the
    -- DESTINATION for them (Goldberry, River-Daughter's "a counter of each kind
    -- not on Goldberry"), which is what `kinds` holds. HOW MANY first objects
    -- there are is `from`, an ObjectRef: one for every producer that names a
    -- permanent, a whole battlefield sweep for Spike Cannibal's "all creatures".
    -- HOW MANY SECOND objects there are is `to`, an ObjectRef the same way:
    -- Forgotten Ancient's "onto other creatures" is a whole group, and the
    -- player then says how many counters land on each (distributePair below).
    -- ATOMIC -- "if either of these actions isn't possible, it's not possible to
    -- move a counter, and no counter is removed from or put onto anything" -- so
    -- every impossibility the rule names is checked BEFORE either half runs, and
    -- this arm is not a RemoveCounters followed by a PutCounters however much its
    -- tail looks like one.
    --
    -- All four of the rule's impossibilities are checked here, in its own order.
    -- Its third, "the second object can't have counters put onto it", is a
    -- PROHIBITION (Solemnity, Melira Sylvok Outcast) and not a replacement, so it
    -- is Pawl.Engine.CounterRestriction rather than a CR 614.16 row -- a row that
    -- scales the placement to nothing is NOT that case: the placement was
    -- possible and was replaced, which rule 122.5 does not undo.
    --
    -- That third impossibility is asked PER KIND, which is what makes it part of
    -- the candidate sweep below rather than a gate beside the two zone reads:
    -- Melira names one kind, so a destination that can't take a -1/-1 counter may
    -- still take a +1/+1 one. A kind the destination refuses is not a kind this
    -- move could choose, and rule 122.5's atomicity is why it is dropped from the
    -- candidates rather than chosen and then half-performed -- "no counter is
    -- removed from or put onto anything". Under "all counters" that is per kind
    -- too, and under "a counter of each kind not on Goldberry" likewise: the
    -- refused kind stays where it is and every other kind still crosses, since
    -- each kind is its own pair of actions and the rule's all-or-nothing is
    -- stated about one counter, not about the sentence that moved it.
    --
    -- The two halves go through Event.removeCounters and Event.putCounters, so the
    -- move records both crossings -- a CR 122.7 "when the Nth counter is put on"
    -- trigger sees the arrival, and rule 122.5's own reading is that a move IS
    -- those two actions. Never one pair per counter, CR 614.16's own unit being
    -- "one or more counters" and the call Pawl.Types.PutCounters already made.
    -- The two halves batch DIFFERENTLY, which the sweep below is what forces: one
    -- removal per kind PER FIRST OBJECT, since a removal is off one object and
    -- rule 122.5 pairs it with one; one placement per kind PER DESTINATION, since
    -- CR 608.2f processes an action taken on several objects simultaneously and
    -- every pair's counters land on the destination its own answer named. Where
    -- `to` names ONE object -- every printing but Forgotten Ancient -- that is one
    -- placement per kind for the whole instruction however many first objects the
    -- sweep gathered from; where it names a group the placements are one per
    -- recipient per kind and batch no further, an arrival being on one object.
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- CR 122.5's SECOND side, swept exactly as the first is and by the same
        -- reader: an ObjectRef, so a slot naming one target is one destination
        -- (CR 608.2b's re-read inside objectRefObjects), a slot bound to a group
        -- is every member of it, and Forgotten Ancient's "other creatures" is
        -- ObjectRef.EachMatching's battlefield fold.
        recipients = objectRefObjects legal resolving controller source gs toRef
        -- HOW MANY of the appropriate kind the card asks for -- Black Panther,
        -- Wakandan King's "all +1/+1 counters", written as the count of that kind
        -- on the object the `from` slot names. Zero for CR 107.2's undeterminable
        -- count and CR 107.1b's negative one alike, freezeRiders' posture.
        askedFor quantity = case Quantity.evaluateFor viewOf context gs resolving source quantity of
          Just n | n > 0 -> Integer.toNaturalSaturating n
          _ -> 0
        -- CR 122.5's pair, performed once per object the FIRST side names:
        -- Spike Cannibal's "from all creatures" sweeps a group, and every
        -- member is its own pair against the one destination, with the rule's
        -- four impossibilities asked afresh for each.
        movePair to from = case from of
          _
            -- "This may occur if the first and second objects are the same object".
            | from /= to,
              -- "... or if either object is no longer in the correct zone". CR 122.1
              -- puts a counter on "an object or player", and CR 122.1a/122.1b
              -- contemplate counters on a card in a zone other than the battlefield,
              -- so the battlefield is not the correct zone by the rule alone -- it is
              -- the correct zone for THIS opcode because every producer in
              -- data/cards/ names permanents on both sides, in every combination the
              -- pool writes (Agent's Toolkit binds the artifact itself and the
              -- creature that entered; Explorer's Cache binds the artifact and a
              -- targeted creature; Black Panther, Wakandan King binds a targeted land
              -- and a targeted creature; Fate Transfer binds two targeted creatures;
              -- Goldberry, River-Daughter binds a targeted permanent and herself,
              -- and her second ability the two the other way about; Scrounging
              -- Bandar binds itself and a targeted creature, where Bioshift binds
              -- two targeted ones; Spike Cannibal sweeps every creature on the
              -- battlefield onto itself; Takesies sweeps every permanent onto a
              -- targeted one; Forgotten Ancient sweeps itself onto every other
              -- creature). A slot bound as the ability triggered may
              -- name an object CR 400.7 has since moved, and a targeted one may have
              -- become illegal, which is CR 608.2b's re-read in the legalMany inside
              -- objectRefObjects.
              --
              -- The `from` half answers CR 702.26b as much as CR 400.7, and both are
              -- proven boards. Pawl.Engine.Phasing spells "treated as though it does
              -- not exist" by moving the object OUT of GameState.battlefield, while
              -- CR 702.26d leaves its counters and its Object.zone alone -- so a
              -- phased-out source is off the battlefield still bearing every kind,
              -- and without this read the candidate sweep below would strip one off
              -- it. Pawl.MoveCounterSpec's Reality Ripple case is that board; its
              -- sacrifice case is CR 122.2's.
              Set.member from (GameState.battlefield gs),
              Set.member to (GameState.battlefield gs) ->
                -- "... if the first object doesn't have the appropriate kind of
                -- counter on it". Which kinds are appropriate is the one place the
                -- printed spellings part, and rule 122.5's clause reads differently
                -- under each.
                --
                -- `onFrom` drops the kinds rule 122.5's THIRD impossibility rules out
                -- as well as the ones the first object does not have, so every
                -- spelling answers it in one place: a kind the destination refuses is
                -- not appropriate for this move, whether the card named it, the
                -- player would have, the card took every kind, or the destination's
                -- own tally settled it. Rule 122.5's atomicity is why it is dropped
                -- here rather than half-performed -- "no counter is removed from or
                -- put onto anything".
                let onFrom =
                      Map.filterWithKey
                        (\kind n -> n > 0 && not (CounterRestriction.prohibited to kind gs))
                        (maybe Map.empty Object.counters (Game.lookupObject from gs))
                    -- CR 609.3 for a count larger than the object has: "it does only
                    -- as much as possible". The clamp is load-bearing --
                    -- Event.removeCounters saturates where Event.putCounters does not,
                    -- so an unclamped pair would place more counters than it took off
                    -- -- and PROVEN by the AnyNumber arm below: no card in the corpus
                    -- writes a count the object it reads cannot cover, but a PLAYER's
                    -- answer is under no such constraint, so Pawl.MoveCounterSpec's
                    -- "an answer asking for more counters than the permanent has"
                    -- case is where the two differ. It also subsumes rule 122.5's
                    -- second impossibility, a kind the first object does not have at
                    -- all: `onFrom` answers zero and nothing crosses.
                    --
                    -- The REMOVAL half alone, reporting per kind what it took off
                    -- THIS first object. The placement half is deliberately not here:
                    -- it happens once for the whole instruction, below the sweep, which
                    -- is CR 608.2f's FIRST branch -- "in most cases, each such action is
                    -- processed simultaneously". Nothing about removals off distinct
                    -- objects beside one batch of placements onto a single object
                    -- cannot be processed simultaneously, so the rule's individual
                    -- fallback and its APNAP ordering never engage. A group source is
                    -- exactly where the two branches part: one placement of nine is
                    -- one CR 614.16 opportunity and one CR 122.7 arrival, where three
                    -- placements of four, three and two are three of each.
                    move kind asked =
                      let taken = min asked (Map.findWithDefault 0 kind onFrom)
                       in if taken == 0
                            then pure Map.empty
                            else do
                              Event.removeCounters from kind taken
                              pure (Map.singleton kind taken)
                 in case kinds of
                      -- "Move all counters": every kind the first object has, the whole
                      -- tally of each, with nothing asked and nothing to name. Ascending
                      -- (Map.toList), so a transcript is deterministic, and SUMMED,
                      -- since "moved this way" counts counters and not kinds.
                      MovedKinds.Every -> fmap (Map.unionsWith (+)) (mapM (uncurry move) (Map.toList onFrom))
                      -- The card named the kind, so the appropriate kind is that one
                      -- and NOTHING is asked: a prompt offering the single option the
                      -- card already settled would be the engine putting a decision
                      -- where the rules leave none.
                      MovedKinds.Named wanted quantity -> move wanted (askedFor quantity)
                      -- Spike Cannibal's "move all +1/+1 counters": the card named
                      -- the kind and asks nothing, Named's reason, and the count is
                      -- the whole tally THIS first object bears rather than a number
                      -- the sentence evaluates once. `onFrom` already dropped a kind
                      -- the destination refuses, so a wanted kind missing from it
                      -- answers zero and nothing crosses.
                      MovedKinds.EveryOfKind wanted -> move wanted (Map.findWithDefault 0 wanted onFrom)
                      -- No kind named: the kinds actually on it ARE the candidates, so
                      -- an object bearing none -- or bearing only kinds the
                      -- destination refuses -- leaves nothing to move and nothing to
                      -- ask. Ascending (Map.keys), so a transcript is deterministic.
                      --
                      -- The whole count comes out of the ONE kind picked; the sweep
                      -- behind that is Pawl.Types.MovedKinds' haddock.
                      MovedKinds.Chosen quantity ->
                        -- A count of zero moves nothing and ASKS nothing: the prompt
                        -- below picks which kind crosses, and no kind crossing makes
                        -- that a question whose answer cannot matter. A REGRESSION
                        -- FENCE, not a proven behaviour -- the one producer reaching
                        -- THIS arm has a literal count of one, so a mutation dropping
                        -- this guard leaves the suite green.
                        let asked = askedFor quantity
                         in if asked == 0
                              then pure Map.empty
                              else case Map.keys onFrom of
                                [] -> pure Map.empty
                                first : rest -> do
                                  kind <- case rest of
                                    -- One kind on the object leaves nothing to decide.
                                    [] -> pure first
                                    second : more -> do
                                      let offered = first NonEmpty.:| (second : more)
                                      answer <- Game.choose (Prompt.ChooseMovedCounter (Decide.deciderFor controller gs) controller from to offered)
                                      -- FILTERED, NOT TRUSTED: an answer naming a kind that is
                                      -- not on the object is dropped for the first one offered.
                                      pure (if Foldable.elem answer offered then answer else first)
                                  move kind asked
                      -- Resourceful Defense's "move any number of counters": the card
                      -- settles neither the kind nor the count, so ONE prompt asks for
                      -- both and the answer may spread across kinds -- which is the
                      -- whole of what separates this arm from Chosen above. Ascending
                      -- (Map.toList), so a transcript is deterministic.
                      --
                      -- An object with no movable kind moves nothing and asks nothing.
                      -- Everywhere else the prompt IS raised, a single kind bearing a
                      -- single counter included, because "any number" includes none:
                      -- moving it and leaving it are two different boards, so eliding
                      -- the question would be the engine making the player's choice.
                      MovedKinds.AnyNumber ->
                        if Map.null onFrom
                          then pure Map.empty
                          else do
                            answer <- Game.choose (Prompt.ChooseMovedCounters (Decide.deciderFor controller gs) controller from to onFrom)
                            -- FILTERED, NOT TRUSTED, in `move` rather than here: it
                            -- already reads `onFrom` for the count, so a kind the object
                            -- does not have -- or one the destination refuses, which
                            -- `onFrom` dropped -- answers zero and moves nothing, and a
                            -- count above what the object holds is clamped to it. A
                            -- filter written here as well would have no observer.
                            fmap (Map.unionsWith (+)) (mapM (uncurry move) (Map.toList answer))
                      -- Goldberry, River-Daughter's "move one or more counters":
                      -- AnyNumber's question above with the empty answer struck
                      -- out, which is the whole of what separates the two arms.
                      -- The card states a floor, so a player holding a movable
                      -- counter must move one; the prompt is its own
                      -- (Prompt.ChooseMovedCountersAtLeastOne) because an
                      -- answerer that cannot see the floor cannot answer within
                      -- it.
                      --
                      -- An object with no movable kind moves nothing and asks
                      -- nothing: the floor is what the card can ask for, not
                      -- what rule 122.5 can perform, and its second
                      -- impossibility still empties the batch.
                      MovedKinds.AtLeastOne ->
                        if Map.null onFrom
                          then pure Map.empty
                          else do
                            answer <- Game.choose (Prompt.ChooseMovedCountersAtLeastOne (Decide.deciderFor controller gs) controller from to onFrom)
                            -- FILTERED, NOT TRUSTED, in two steps where AnyNumber
                            -- needs one. A kind the object does not have is dropped
                            -- and a count above what it holds is clamped, which
                            -- Map.intersectionWith does here rather than leaving it
                            -- to `move` -- the floor has to be read off the answer
                            -- AS FILTERED, since an answer naming only kinds that
                            -- are not there moves nothing. An answer that then still
                            -- moves nothing is repaired to one counter of the first
                            -- kind offered (Map.lookupMin, so a transcript is
                            -- deterministic): the card says one or more must cross,
                            -- and rule 122.5 performs the move wherever it is
                            -- possible.
                            let clamped = Map.filter (> 0) (Map.intersectionWith min answer onFrom)
                                floored =
                                  if Map.null clamped
                                    then foldMap (\(kind, _) -> Map.singleton kind 1) (Map.lookupMin onFrom)
                                    else clamped
                            fmap (Map.unionsWith (+)) (mapM (uncurry move) (Map.toList floored))
                      -- Scrounging Bandar's "move any number of +1/+1
                      -- counters": the card names the kind and leaves the count
                      -- to the player, so the prompt above is raised over the ONE
                      -- kind the card named rather than over every kind the object
                      -- bears. That is what keeps this arm from being AnyNumber:
                      -- offering the rest would let the answerer move a counter
                      -- the card never mentioned.
                      --
                      -- The same prompt and not a bespoke one: its Map is what the
                      -- move could really carry, and a card naming its kind narrows
                      -- that to a single key exactly as `onFrom` narrows it to the
                      -- kinds present. The answer is FILTERED the same way too, by
                      -- reading only the named kind out of it.
                      --
                      -- A first object bearing none of that kind moves nothing and
                      -- asks nothing -- rule 122.5's second impossibility with no
                      -- question left behind it. Everywhere else the prompt IS
                      -- raised, a lone counter included, AnyNumber's reason above.
                      MovedKinds.AnyNumberOfKind wanted ->
                        case Map.lookup wanted onFrom of
                          Nothing -> pure Map.empty
                          Just available -> do
                            answer <- Game.choose (Prompt.ChooseMovedCounters (Decide.deciderFor controller gs) controller from to (Map.singleton wanted available))
                            move wanted (Map.findWithDefault 0 wanted answer)
                      -- Goldberry, River-Daughter's "a counter of each kind not on
                      -- Goldberry": one counter of every kind the FIRST object has
                      -- that the SECOND does not, which names no kind, prints no
                      -- count and asks nothing -- the destination's own tally is the
                      -- whole of the decision, so a prompt here would be the engine
                      -- putting a choice where the card leaves none. Ascending
                      -- (Map.keys), so a transcript is deterministic.
                      --
                      -- `onTo` is read from the SAME pre-move snapshot as `onFrom`,
                      -- which is not a stale read but the rule's own reading: "each
                      -- kind not on Goldberry" is settled once for the whole sentence,
                      -- and the kinds it selects are by construction absent from the
                      -- destination, so no kind this arm moves can make a later kind
                      -- ineligible however the batch is ordered.
                      --
                      -- Kinds the destination holds at zero count as absent: CR 122.1
                      -- makes a counter a marker that is on the object or is not, and
                      -- Object.counters keeps a key whose counters have all been
                      -- removed.
                      MovedKinds.EachAbsentKind ->
                        let onTo = Map.filter (> 0) (maybe Map.empty Object.counters (Game.lookupObject to gs))
                            absent = Map.keys (Map.withoutKeys onFrom (Map.keysSet onTo))
                         in fmap (Map.unionsWith (+)) (mapM (`move` 1) absent)
                      -- Takesies' "move up to one counter from each permanent":
                      -- Chosen's question -- which kind, out of the kinds this
                      -- first object actually has -- with declining added, and
                      -- one counter of whichever kind is picked. Ascending
                      -- (Map.keys), so a transcript is deterministic.
                      --
                      -- The prompt is raised for a SINGLE candidate as well,
                      -- AnyNumber's reason rather than Chosen's: "up to one"
                      -- includes none, so moving the one kind and leaving it are
                      -- two different boards and the question is real.
                      MovedKinds.UpToOneChosen ->
                        case Map.keys onFrom of
                          [] -> pure Map.empty
                          first : rest -> do
                            let offered = first NonEmpty.:| rest
                            answer <- Game.choose (Prompt.ChooseMovedCounterOrNone (Decide.deciderFor controller gs) controller from to offered)
                            -- FILTERED, NOT TRUSTED: an answer naming a kind
                            -- that is not on the object is read as declining,
                            -- which is Chosen's filter under a wording where
                            -- declining is itself legal -- there the answer had
                            -- to fall back to a kind, since rule 122.5 moves a
                            -- counter wherever it can.
                            case answer of
                              Just kind | Foldable.elem kind offered -> move kind 1
                              _ -> pure Map.empty
          -- Rule 122.5's impossibilities above: THIS pair moves nothing and the
          -- others in the sweep are untouched, the rule's all-or-nothing being
          -- stated about one pair. A destination that resolves to no object at
          -- all -- an illegal slot at resolution (CR 608.2b), a player recipient
          -- -- never reaches here, `recipients` being empty instead.
          _ -> pure Map.empty
        -- movePair's sibling for a destination naming a GROUP: Forgotten
        -- Ancient's "move any number of +1/+1 counters from this creature onto
        -- other creatures". One first object against many second objects, where
        -- movePair is one against one, and the pairs it makes are rule 122.5's
        -- own -- so its four impossibilities are asked afresh for each recipient
        -- rather than once for the sentence.
        --
        -- WHERE each counter goes is the player's, which is what makes this a
        -- prompt rather than a fold: the card leaves both the count and the
        -- recipient open, and one question answers both (Prompt's
        -- ChooseDistributedMovedCounters).
        --
        -- Only MovedKinds.AnyNumber and MovedKinds.AnyNumberOfKind are asked, and
        -- they are two of the THREE arms whose count is the player's rather than
        -- all of them: MovedKinds.AtLeastOne's count is the player's too and it
        -- is not asked here. An arm the card settles a count on would be a
        -- different question -- where does a fixed batch go -- which this prompt
        -- cannot state, since an answer allocating less than the batch would
        -- leave counters removed with nowhere to land and rule 122.5 forbids the
        -- half-move. The floor is a different question again: an answer moving
        -- nothing is repaired below by taking one counter of the first kind
        -- offered, and over a group that repair would have to choose the
        -- RECIPIENT as well, which the card leaves to the player and no board
        -- forces. Neither pairing is printed; see Pawl.Types.MoveCounters
        -- (#2784).
        distributePair candidates fromOne =
          -- Rule 122.5's first and fourth impossibilities, per recipient: the
          -- first object is not its own destination, and both ends are on the
          -- battlefield. A recipient failing either is not a pair this move can
          -- make and is simply not offered.
          let others = NonEmpty.filter (\to -> to /= fromOne && Set.member to (GameState.battlefield gs)) candidates
           in if not (Set.member fromOne (GameState.battlefield gs))
                then pure Map.empty
                else case others of
                  [] -> pure Map.empty
                  firstTo : moreTo ->
                    -- movePair's `onFrom` read across the whole group: a kind is
                    -- appropriate here if SOME recipient can take it, rule 122.5's
                    -- third impossibility being asked of one pair at a time.
                    -- Which recipient may take which kind is then asked again as
                    -- the answer is filtered below.
                    let onFrom =
                          Map.filterWithKey
                            (\kind n -> n > 0 && any (\to -> not (CounterRestriction.prohibited to kind gs)) others)
                            (maybe Map.empty Object.counters (Game.lookupObject fromOne gs))
                        offered = case kinds of
                          MovedKinds.AnyNumber -> onFrom
                          MovedKinds.AnyNumberOfKind wanted -> Map.restrictKeys onFrom (Set.singleton wanted)
                          -- Not implemented: a floor over a group, whose repair
                          -- would have to name a recipient (#2784). Written out
                          -- rather than left to the fallthrough below, since it
                          -- is the arm a reader of this file would expect here.
                          MovedKinds.AtLeastOne -> Map.empty
                          _ -> Map.empty
                     in if Map.null offered
                          then pure Map.empty
                          else do
                            answer <- Game.choose (Prompt.ChooseDistributedMovedCounters (Decide.deciderFor controller gs) controller fromOne offered (firstTo NonEmpty.:| moreTo))
                            -- FILTERED, NOT TRUSTED: an object that was not
                            -- offered is dropped (the fold is over `others`), a
                            -- kind that was not offered is dropped, a kind the
                            -- recipient itself refuses is dropped, and the
                            -- tallies are clamped in offered order to what the
                            -- first object actually holds (CR 609.3's "only as
                            -- much as possible"). Clamping in an order rather
                            -- than proportionally is what keeps a transcript
                            -- deterministic; an honest answer never reaches it.
                            let step (remaining, acc) to =
                                  let wanted = Map.restrictKeys (Map.findWithDefault Map.empty to answer) (Map.keysSet offered)
                                      granted =
                                        Map.filter (> 0) $
                                          Map.mapMaybeWithKey
                                            (\kind n -> if CounterRestriction.prohibited to kind gs then Nothing else Just (min n (Map.findWithDefault 0 kind remaining)))
                                            wanted
                                   in (Map.differenceWith (\held n -> Just (held - n)) remaining granted, if Map.null granted then acc else Map.insert to granted acc)
                                allocated = snd (List.foldl' step (offered, Map.empty) others)
                            -- The REMOVAL half, once per kind for this first
                            -- object however many recipients share it: movePair's
                            -- batching, CR 608.2f's first branch. The placements
                            -- are below the sweep with the single-destination
                            -- ones, and cannot batch across recipients, an
                            -- arrival being on one object.
                            Monad.forM_ (Map.toList (Map.unionsWith (+) (Map.elems allocated))) $ \(kind, n) ->
                              Monad.when (n > 0) (Event.removeCounters fromOne kind n)
                            pure allocated
    -- The FIRST side is swept as this instruction is reached (CR 608.2c), and the
    -- removals run in the order the sweep hands back -- battlefieldMatching's
    -- APNAP sort for the EachMatching arm, one object for every other arm the
    -- corpus writes. That sort is not this arm's reason for ordering them:
    -- rule 608.2f orders an action that CANNOT be processed simultaneously, and
    -- the placement paragraph below is why this one can. What it buys here is a
    -- deterministic transcript.
    --
    -- objectRefObjects takes EVERY recipient a slot holds, through slotGroup and
    -- then legalMany, where a bare SlotName read goes through legalOne and takes a
    -- slot naming several objects as naming none. So a slot bound to more than one
    -- object is one pair per binding here rather than an instruction that silently
    -- moves nothing -- which is the reading the printed sentences want, "from all
    -- creatures" being a group before it is a slot. That now holds on BOTH sides,
    -- neither of which is a singular read, so neither is Pawl.CardSpec's
    -- singular-reader lint's business. No card in data/cards/ binds either slot
    -- plurally; the groups both sides do carry are ObjectRef.EachMatching sweeps.
    --
    -- Every read inside movePair takes the same pre-sweep snapshot, which is
    -- Effect.PutCounters' posture: one evaluation for the whole instruction. That
    -- is not a stale read, because no sibling pair can move what any of these
    -- reads asks about. The counters on the FIRST object and a prohibition on the
    -- destination are per-pair values to begin with, and so is the destination's
    -- own tally, which only MovedKinds.EachAbsentKind reads: a pair takes counters
    -- off nothing but its own first object, `from /= to` keeps the destination out
    -- of that set, and nothing is placed until the sweep has ended. Each read
    -- therefore answers what a live read would, which is the answer CR 608.2h
    -- wants either way -- it "is determined only once, when the effect is
    -- applied".
    let froms = objectRefObjects legal resolving controller source gs fromRef
    -- CR 608.2f's FIRST branch, and the whole reason the placement is not inside
    -- movePair: "in most cases, each such action is processed simultaneously", so
    -- one written instruction puts ONE batch of each kind onto the destination
    -- however many first objects it took them off. The rule's individual fallback
    -- is for an action that CANNOT be simultaneous -- Soulfire Eruption's example,
    -- where a library can only be exiled off one card at a time -- and nothing
    -- makes a single object's arrivals serial that way.
    --
    -- It is observable, which is why it is not a matter of taste: CR 614.16
    -- replaces "one or more counters" per placement, so Hardened Scales grows one
    -- batch of nine by one and three batches of four, three and two by three; and
    -- CR 122.7's "when the Nth counter is put on" sees one arrival rather than
    -- three. Pawl.MoveCounterSpec's Hardened Scales case is that board.
    --
    -- ANSWERS how many counters made the whole journey, which is neither half on
    -- its own: Event.putCounters reports what landed after CR 614.16, and a row
    -- that GREW the placement added counters that never came off a first object,
    -- so they were not moved. The minimum is the journey's overlap.
    --
    -- ONE destination or a GROUP of them is the branch here, and it is the whole
    -- of what the second side's ObjectRef buys: one destination gathers every
    -- first object's counters into one batch per kind, where a group has the
    -- player say how many of each kind land on each recipient, so the placements
    -- are one per recipient per kind and can batch no further -- an arrival is on
    -- one object. Ascending by recipient and then by kind (Map.toList), so a
    -- transcript is deterministic either way, and SUMMED, since "moved this way"
    -- counts counters and not kinds.
    let place to (kind, n) = fmap (min n) (Event.putCounters (CounterCause.ByEffect controller) to kind n)
    moved <- case recipients of
      [to] -> do
        taken <- fmap (Map.unionsWith (+)) (mapM (movePair to) froms)
        fmap sum (mapM (place to) (Map.toList taken))
      firstTo : secondTo : moreTo -> do
        allocated <- fmap (Map.unionsWith (Map.unionWith (+))) (mapM (distributePair (firstTo NonEmpty.:| (secondTo : moreTo))) froms)
        fmap sum (mapM (\(to, batch) -> fmap sum (mapM (place to) (Map.toList batch))) (Map.toList allocated))
      -- A destination naming no object at all: an illegal slot at resolution (CR
      -- 608.2b), a player recipient, or a filter nothing on the battlefield
      -- matches. Nothing crosses and nothing is asked, and the slot below is
      -- still bound -- to zero.
      [] -> pure 0
    -- "Counters moved this way", for a later effect of the same resolution to read
    -- as Quantity.InSlot -- Destroy's `slot` above in every respect, bound onto
    -- this effect's SOURCE and bound even when nothing moved, since zero is an
    -- answer where an unbound slot would leave the rider's gate unevaluable.
    Monad.forM_ mSlot $ \slot -> State.modify' (bindAmountSlot source slot moved)
  -- CR 701.34a: choose any number of permanents and/or players that have a
  -- counter, then give each one more of every kind it already has. "That have
  -- a counter" is the candidate filter, and "any number" is why a lone
  -- candidate is still asked about.
  --
  -- Candidates and kinds are read BEFORE the prompt and before any counter
  -- lands (CR 608.2h), so a CR 614 replacement cannot widen the kinds.
  -- Targetless: no CR 608.2b legality to re-check.
  --
  -- The roster is Game.stillPlaying, not the keys of GameState.players, whose
  -- departed seats keep counters CR 800.4a does not remove.
  Effect.Proliferate -> do
    gs <- State.get
    let everyone = Game.stillPlaying gs
        kindsOn oid = foldMap (Map.keys . Map.filter (> 0) . Object.counters) (Game.lookupObject oid gs)
        kindsFor pid = foldMap (Map.keys . Map.filter (> 0) . Player.counters) (Map.lookup pid (GameState.players gs))
        -- zoneMembers slices the shared battlefield by OWNER, so the union over
        -- every seat is every permanent in play (CR 701.34a).
        onBattlefield = concatMap (\pid -> Game.zoneMembers Zone.Battlefield pid gs) everyone
        permanents = filter (not . null . kindsOn) onBattlefield
        players = filter (not . null . kindsFor) everyone
    Monad.unless (null permanents && null players) $ do
      (pickedPermanents, pickedPlayers) <-
        Game.choose (Prompt.ChooseProliferate (Decide.deciderFor controller gs) controller permanents players)
      -- FILTERED, NOT TRUSTED: an answer naming something not offered is dropped.
      let keptPermanents = filter (\oid -> Set.member oid pickedPermanents) permanents
          keptPlayers = filter (\pid -> Set.member pid pickedPlayers) players
      -- CR 122.6: object counters through the single funnel, so CR 614's counter
      -- replacements apply to a proliferated counter as to a placed one.
      Monad.forM_ keptPermanents $ \oid ->
        Monad.forM_ (kindsOn oid) $ \kind -> Event.putCounters (CounterCause.ByEffect controller) oid kind 1
      -- CR 122.1: and player counters through their own funnel.
      Monad.forM_ keptPlayers $ \pid ->
        Monad.forM_ (kindsFor pid) $ \kind ->
          Monad.void (Event.putPlayerCounters (CounterCause.ByEffect controller) pid kind 1)
  -- CR 701.39a: "bolster N" -- put N +1/+1 counters on a creature you control
  -- with the least toughness, or tied for least. The prompt is raised only for a
  -- TIE; CR 101.3 ignores the instruction when the pool is empty. Targetless: no
  -- CR 608.2b legality to re-check.
  --
  -- Toughness is the PROJECTED value (CR 613.1g's layer 7); a creature the
  -- projection gives no toughness is dropped rather than sorted as zero, and the
  -- pool is swept before the counters land (CR 608.2h).
  Effect.Bolster quantity -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- Ascending, so the single-candidate shortcut and a transcript are
        -- deterministic.
        creatures = List.sort (filter (\oid -> Projection.isCreatureOf oid gs) (Projection.controls controller gs))
        measured = Maybe.mapMaybe (\oid -> fmap ((,) oid) (Projection.toughnessOf oid gs)) creatures
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
      Just n -> case measured of
        -- CR 101.3: a player controlling no creature bolsters nothing.
        [] -> pure ()
        first : rest -> do
          let least = minimum (fmap snd (first : rest))
              tied = fmap fst (filter ((== least) . snd) (first : rest))
          bolstered <- case tied of
            -- Unreachable by construction, `least` being the minimum OF this
            -- list; keeps the mandatory action mandatory.
            [] -> pure (fst first)
            one : others -> case others of
              -- One creature at the minimum leaves nothing to ask.
              [] -> pure one
              second : more -> do
                let offered = one NonEmpty.:| (second : more)
                answer <- Game.choose (Prompt.ChooseBolster (Decide.deciderFor controller gs) controller resolving offered)
                -- FILTERED, NOT TRUSTED: an answer never offered falls back to
                -- the first candidate, the action being mandatory.
                pure (if List.elem answer (NonEmpty.toList offered) then answer else one)
          -- CR 122.6: through the single funnel, so CR 614.16's counter
          -- replacements get their opportunity.
          Monad.when (n > 0) . Monad.void $
            Event.putCounters (CounterCause.ByEffect controller) bolstered CounterKind.PlusOnePlusOne (Integer.toNaturalSaturating n)
  -- CR 701.47a: the resolving controller amasses; the keyword action is
  -- Pawl.Engine.Amass.amass's, and this arm evaluates only the printed N.
  --
  -- Targetless: no CR 608.2b legality to re-check.
  Effect.Amass (Amass.Type.MkAmass quantity subtype) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
      Just n -> Amass.amass controller source resolving subtype (Integer.toNaturalSaturating n)
  -- CR 701.68a: each player the PlayerRef names puts N -1/-1 counters on a
  -- creature they control; the keyword action is Pawl.Engine.Blight.blight's, and
  -- rule 701.68b's optional reading is a cost (CR 118.12). MANDATORY here, so an
  -- empty pool is CR 101.3's no-op. A reference naming nobody, or an illegal slot
  -- (CR 608.2b), blights nothing. APNAP for the order (CR 101.4). Targetless: no
  -- CR 608.2b legality to re-check.
  --
  -- Rule 101.4's last sentence -- "then the actions happen simultaneously" -- is
  -- the Event.simultaneously bracket: every blighter's counters share one
  -- Pawl.Types.EventGroup, so a CR 603.2c batch condition watching placements
  -- across permanents sees one trigger event and not one per seat.
  --
  -- The bracket does not touch the DECISION order, which stays the APNAP walk
  -- above it: each seat is prompted in turn, from its own pool, knowing what the
  -- seats before it chose (CR 101.4b). Rule 101.4's FIRST sentence -- all choices,
  -- then the actions -- is honored as that order and no further: a seat's counters
  -- are written to the board before the next seat is asked, and what the two
  -- readings share is the event group rather than the moment of writing. The
  -- difference is unobservable -- the quantity is evaluated off the pre-loop `gs`,
  -- rule 701.68a's candidates are the asked seat's OWN creatures, CR 704.3 holds
  -- state-based actions until a player would get priority, and no replacement in
  -- data/cards reads a counter tally across permanents.
  --
  -- Not implemented: nothing records which creature was blighted, so CR 701.68c's
  -- "blighted creature" has nothing to read (#1492); CR 701.68d's trigger on a
  -- player blighting has no condition to match either (#3065).
  Effect.Blight (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        named = playerRefPlayers legal controller gs ref
        blighters = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    Event.simultaneously . Monad.forM_ blighters $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
        Just n -> Monad.void (Blight.blight (CounterCause.ByEffect pid) resolving (Integer.toNaturalSaturating n))
  -- CR 701.54a: the Ring tempts the resolving controller; the keyword action is
  -- Pawl.Engine.Ring.tempt's.
  Effect.TemptWithTheRing -> Ring.tempt controller
  -- CR 701.49: the whole keyword action, which Pawl.Engine.Dungeon owns.
  Effect.Venture quality -> Dungeon.venture controller quality
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters ref kind quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        recipients = playerRefPlayers legal controller gs ref
    -- CR 122 / 107.14: the amount is read per recipient off the one pre-effect
    -- `gs`, then CR 122.6's funnel per recipient, so a counter-scaling
    -- replacement gets its CR 614 opportunity against each player's gain.
    Monad.forM_ recipients $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 -> Monad.void (Event.putPlayerCounters (CounterCause.ByEffect controller) pid kind (Integer.toNaturalSaturating n))
        _ -> pure ()
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters ref kind quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        recipients = playerRefPlayers legal controller gs ref
    Monad.forM_ recipients $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 ->
              -- CR 122: GainPlayerCounters' mirror, through no funnel for
              -- Effect.RemoveCounters' reason. The floor is explicit, since
              -- Natural subtraction would underflow.
              State.modify'
                ( \g ->
                    g
                      { GameState.players =
                          Map.adjust
                            (\p -> p {Player.counters = Map.adjust (\held -> held - min held (Integer.toNaturalSaturating n)) kind (Player.counters p)})
                            pid
                            (GameState.players g)
                      }
                )
        _ -> pure ()
  -- CR 107.14: "you may pay any amount of {E}". The payer is the resolving
  -- controller (CR 109.5's "you"), the amount is theirs to name, and CR 118.3
  -- caps it at the energy they actually have -- so unlike CR 601.2b's
  -- announcement the bound is ENFORCED, and an answer above it is clamped rather
  -- than trusted. Paying 0 is how the printed "may" is declined.
  --
  -- Through Pawl.Engine.Cost's own reader and writer, so CR 107.14 has one
  -- meaning here and in a CostComponent.PayEnergy payment.
  --
  -- Not implemented: skipping the offer when the payer has no energy, where 0 is
  -- the only payable amount (#1920).
  Effect.PayAnyEnergy slot -> do
    gs <- State.get
    let have = Cost.energyOf controller gs
    answer <- Game.choose (Prompt.ChoosePaidEnergy (Decide.deciderFor controller gs) controller resolving have)
    let paid = min answer have
    Cost.spendEnergy controller paid
    -- Bound onto this effect's SOURCE even when nothing was paid, Effect.Destroy's
    -- count for its reason: zero is an answer, where an unbound slot would leave a
    -- later clause's quantity unevaluable instead.
    State.modify' (bindAmountSlot source slot paid)
  -- CR 701.26a: turn each named permanent sideways. The victims are enumerated
  -- ONCE (CR 608.2f) and off the board as it stands before any of them is tapped,
  -- so an illegal slot (CR 608.2b), a player recipient and a set that matched
  -- nothing all tap nothing.
  --
  -- Through Pawl.Engine.Event.tap rather than a direct write, which is what makes
  -- each one a becomes-tapped event. Rule 701.26a's "only untapped permanents can
  -- be tapped" lives in that funnel, and it earns its keep now that an event rides
  -- on the write: the assignment was idempotent and the event is not.
  Effect.Tap ref -> do
    gs <- State.get
    Monad.forM_ (objectRefObjects legal resolving controller source gs ref) Event.tap
  -- CR 701.26b: rotate each named permanent back upright. The victims are
  -- enumerated ONCE (CR 608.2f) and off the board as it stands before any of them
  -- is untapped, so an illegal slot (CR 608.2b), a player recipient and a set
  -- that matched nothing all untap nothing.
  --
  -- Through Pawl.Engine.Event.untap rather than a direct write, Effect.Tap's
  -- reason one rule clause over: rule 701.26b's "only tapped permanents can be
  -- untapped" lives in that funnel, and CR 122.1d's replacement is offered the
  -- event there.
  Effect.Untap ref -> do
    gs <- State.get
    Monad.forM_ (objectRefObjects legal resolving controller source gs ref) Event.untap
  Effect.Detain ref ->
    State.modify' $ \gs ->
      -- CR 701.35a: detain each named permanent until the next turn of this
      -- resolution's `controller` (CR 109.5), sampled once, since the sweep that
      -- ends the detain (Pawl.Engine.Expiry.dropAtTurnOf) has no resolution left
      -- to read it off. The victims are enumerated ONCE (CR 608.2f). Nothing is
      -- stored anywhere but on the victim, so an already-detained permanent is
      -- detained again with no count kept -- see Object.detainedUntil.
      foldr (Detain.detain controller) gs (objectRefObjects legal resolving controller source gs ref)
  Effect.Goad ref ->
    State.modify' $ \gs ->
      -- CR 701.15a: goad each named permanent until the next turn of this
      -- resolution's `controller` (CR 109.5), sampled once, for the reason
      -- Effect.Detain samples it. The victims are enumerated ONCE (CR 608.2f).
      -- An already-goaded permanent goaded again by the same player keeps one
      -- entry, which is CR 701.15d -- see Object.goadedBy.
      foldr (Goad.goad controller) gs (objectRefObjects legal resolving controller source gs ref)
  Effect.DoesNotUntapNext ref ->
    State.modify' $ \gs ->
      -- CR 502.3's untap prohibition, as a one-shot; the victims are enumerated
      -- ONCE (CR 608.2f). No duration is stored: CR 701.43b makes the untap step
      -- the prohibition bites in the step it expires in, and Engine.untapAll
      -- clears the flag there. Marking an already-marked permanent is a no-op,
      -- that rule's non-stacking said as a state assignment.
      let mark o = o {Object.doesNotUntapNext = True}
       in gs
            { GameState.objects =
                foldr (Map.adjust mark) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
  Effect.Transform ref -> turnPermanentsOver legal resolving controller source ref
  -- CR 701.28a routes a convert through rules 701.27a-f unchanged, so it is the
  -- SAME call and not a similar one. The two opcodes stay distinct in the card
  -- data (Pawl.Types.Effect.Convert) and identical in behaviour here.
  Effect.Convert ref -> turnPermanentsOver legal resolving controller source ref
  Effect.Meld (Meld.MkMeld ref resultCard) -> do
    gs <- State.get
    -- An ordinary read of the ref (CR 608.2c), not a question: the cards were
    -- named by the ability that melds -- for the pool's only pair, the slot its
    -- own "exile them" bound (CR 400.7j) -- so there is nothing here to ask. The
    -- one choice this card makes is WHICH counterpart to exile, and it is made an
    -- instruction earlier.
    --
    -- Event.meld is the whole of CR 701.42a, gate included: it refuses (CR
    -- 701.42b) by writing nothing, which is CR 701.42c's "they stay in their
    -- current zone" -- the cards are wherever the exile left them, and this
    -- resolution simply had no effect. Nothing is bound and no slot is filled: no
    -- printing names the melded permanent later in its own instruction list.
    Monad.void (Event.meld controller (objectRefObjects legal resolving controller source gs ref) resultCard)
  Effect.PhaseOut ref ->
    State.modify' $ \gs ->
      -- CR 702.26b: each named permanent phases out; the victims are enumerated
      -- ONCE (CR 608.2f). The whole set goes to Phasing.phaseOutSet in one call,
      -- because CR 702.26g and CR 702.26h both ask whether a permanent's HOST is
      -- leaving in this same event, which per-victim calls could not see.
      --
      -- Nothing about the row is read off `controller`: rule 702.26a schedules
      -- the return by who controlled IT, and `controller` is only phaseOutSet's
      -- fallback for a permanent the projection can no longer place.
      Phasing.phaseOutSet controller (Set.fromList (objectRefObjects legal resolving controller source gs ref)) gs
  -- CR 500.8: add the phases, directly after the phase this is resolving in.
  -- Turn.splicePhases is handed GameState.phase because that is NOT the head of
  -- `remaining` when the resolving phase still has steps to come; CR 511.3 bounds
  -- the phase, and Turn.thisPhase is where that lives.
  Effect.AddPhases extras ->
    State.modify' $ \gs ->
      gs {GameState.remaining = Turn.splicePhases (GameState.phase gs) extras (GameState.remaining gs)}
  -- CR 724.1: end the turn (Time Stop). Rule 724.1's six steps, in its own order,
  -- and deliberately NOT the CR 608 process the fold around this is running.
  Effect.EndTurn -> do
    -- CR 724.1a: an ability that triggered before this process began but is not
    -- yet on the stack ceases to exist. There is no pending queue to flush -- a
    -- pending trigger is DERIVED from GameState.events behind the CR 117.5
    -- watermark -- so this is a watermark bump, which is also what makes rule
    -- 724.1f's exception fall out: everything recorded after this line is an
    -- ability that triggered DURING the process, and stays unscanned until the
    -- cleanup step settles.
    --
    -- UNOBSERVED, and said plainly rather than left to look tested: Time Stop
    -- resolves at a priority boundary, which has just settled, so nothing is
    -- pending here. The pool holds no producer that can leave one -- Day's
    -- Undoing draws seven before its own end-the-turn clause (#2067).
    State.modify' $ \gs ->
      gs
        { GameState.scannedThrough = Natural.length (GameState.events gs),
          GameState.battlefieldWhenTriggered = Map.empty
        }
    -- CR 724.1b: exile every object on the stack, INCLUDING the one that is
    -- resolving -- which is still on GameState.stack, Stack.resolveTopWith
    -- leaving it there until CR 608.2n moves it. A real zone change, so an
    -- ability that triggers off one is CR 724.1f's.
    --
    -- Rule 724.1b's second sentence -- an object not represented by a card ceases
    -- to exist at the next check -- is taken HERE for an ability rather than at
    -- CR 724.1c, and the two are indistinguishable: nothing happens between the
    -- exile and that check, and Game.cease is already what CR 608.2n, CR 603.3c
    -- and CR 701.6a use for an ability, which is not a card and so has no zone to
    -- arrive in (CR 400.7 mints nothing). A token, and a copy of a spell, DO go
    -- through the exile, Pawl.Engine.Sba implementing CR 704.5d and CR 704.5e for
    -- them.
    --
    -- finishSpell still runs when this fold returns, and is a no-op for it: CR
    -- 400.7 minted a fresh incarnation in exile, so changeZone's lookup of the
    -- old id finds nothing and the spell stays exiled rather than reaching a
    -- graveyard.
    onStack <- State.gets GameState.stack
    Foldable.traverse_ exileOrCease onStack
    -- CR 724.1c: check state-based actions, granting no priority and putting no
    -- triggered ability on the stack -- so Pawl.Engine.Sba's own pass and not
    -- Engine.performSettle, which places triggers. (Engine is unreachable from
    -- here in any case: Engine imports Stack imports Resolve.)
    --
    -- Repeated to a fixpoint on CR 704.3's authority -- "if any state-based
    -- actions are performed as a result of a check, the check is repeated" -- and
    -- one pass does not reach it: CR 704.5m's Aura falls off only on the pass
    -- after the creature it enchanted died. Terminates because the flag is False
    -- as soon as a pass performs nothing.
    let checkToFixpoint = do
          performed <- Sba.performStateBasedActions
          Monad.when performed checkToFixpoint
    checkToFixpoint
    -- CR 724.1d: the current phase and/or step ends; creatures and planeswalkers
    -- leave combat if this happened during one; the game skips straight to the
    -- cleanup step, or to a NEW cleanup step if this IS one.
    --
    -- What ends the step is Engine.runStepThatBegan, which resumes after the
    -- signal below stops the priority round and runs CR 500.5's expiries, CR
    -- 703.4q's mana emptying and CR 704.3 for the step exactly as it would for a
    -- step that ended by itself. Nothing of that is duplicated here.
    --
    -- The PHASE-scoped half of CR 500.5 is taken here and not there, on rule
    -- 724.1d's "the current phase and/or step ends": Turn.phaseEndingAt answers
    -- from the last step alone, and the step this ends is not it, so an "until
    -- end of combat" effect would otherwise outlive a turn ended during combat.
    -- The phase does NOT end when the jump stays inside it -- CR 512.1 puts the
    -- cleanup step in the ending phase, so ending the turn during the end step
    -- leaves that phase still under way.
    State.modify' $ \gs ->
      let phase = GameState.phase gs
          cleared = case phase of
            Phase.Combat _ -> Combat.clearCombat gs
            _ -> gs
          -- Both sweeps, for the same reason: a mana unit's "until end of combat"
          -- retention carries no Expiry, so Expiry.dropAtEndOf cannot reach it
          -- (CR 500.5a). The mana itself is taken by the CR 500.5 sweep this
          -- step's own end runs in Engine.runStepThatBegan, once this has made
          -- the units ordinary.
          expired = case phase of
            Phase.Ending _ -> cleared
            _ -> maybe cleared (\ending -> Mana.endRetentionAtEndOf ending (Expiry.dropAtEndOf ending cleared)) (Turn.wholePhaseOf phase)
          jumped = case phase of
            Phase.Ending EndingStep.Cleanup -> Turn.spliceExtraCleanup (GameState.remaining gs)
            _ -> Turn.jumpToCleanup (GameState.remaining gs)
       in expired {GameState.remaining = jumped}
    -- CR 724.1f: no player gets priority during this process. Engine.priorityLoop
    -- reads this and returns without settling and without granting another round,
    -- so the exiles above stay unscanned until the cleanup step's own CR 514.3a
    -- settle puts them on the stack. Engine.runStep lowers it there.
    State.modify' (\gs -> gs {GameState.endTurnSignal = EndTurnSignal.Ended})
  -- CR 724.2: end the combat phase (Mandate of Peace). Rule 724.2's seven steps,
  -- in its own order, and deliberately NOT the CR 608 process around it. The arm
  -- above is its sibling and not its implementation: the two agree on 724.2a-c
  -- and differ on what 724.2d leaves at the head of the schedule.
  Effect.EndCombatPhase -> do
    phase <- State.gets GameState.phase
    case phase of
      -- CR 724.2g: attempted at a time that is not a combat phase, nothing
      -- happens -- not even the exile of the stack, since rule 724.2's steps are
      -- the whole of what "ends the combat phase" does. No CAST reaches it today:
      -- Mandate of Peace's own rider admits only a combat phase, and nothing
      -- between the cast and the resolution can leave one, this being the only
      -- card that ends a combat phase. The guard is rule 724.2g's own sentence
      -- rather than a defence against a board pawl can build, and
      -- Pawl.TurnSpec's CR 724.2g case reaches it by applying the effect
      -- directly.
      Phase.Combat _ -> do
        -- CR 724.2a, exactly as CR 724.1a: a watermark bump rather than a queue
        -- flush, which is also what makes 724.2f's exception fall out. UNOBSERVED
        -- for the same reason the arm above says: resolution follows a settle, so
        -- nothing is pending here.
        State.modify' $ \gs ->
          gs
            { GameState.scannedThrough = Natural.length (GameState.events gs),
              GameState.battlefieldWhenTriggered = Map.empty
            }
        -- CR 724.2b: exile every object on the stack, the resolving one included.
        -- Shared with the arm above down to the CR 400.7 detail that leaves
        -- finishSpell a no-op afterwards.
        onStack <- State.gets GameState.stack
        Foldable.traverse_ exileOrCease onStack
        -- CR 724.2c: check state-based actions, granting no priority and putting
        -- no triggered ability on the stack. To a fixpoint on CR 704.3's
        -- authority, as above.
        let checkToFixpoint = do
              performed <- Sba.performStateBasedActions
              Monad.when performed checkToFixpoint
        checkToFixpoint
        -- CR 724.2d, in the rule's own sentence order: the phase ends, creatures
        -- and planeswalkers leave combat, "until end of combat" effects expire,
        -- and the game skips the steps between here and the next phase.
        --
        -- The expiry is this rule's, not CR 500.5's through
        -- Engine.runStepThatBegan: that sweep asks Turn.phaseEndingAt, which
        -- answers from the phase's last step, and CR 724.2e is precisely the
        -- claim that the end of combat step never runs. EQUALITY on
        -- PhaseSelector.CombatPhase, so an "until end of turn" effect installed
        -- by the same card survives (Expiry.dropAtEndOf says why).
        --
        -- Where the game lands is whatever `remaining` holds after this phase --
        -- CR 724.2d's "usually the postcombat main phase", and a CR 500.8 extra
        -- combat phase later in the turn is untouched, Turn.dropRestOfPhase being
        -- positional.
        State.modify' $ \gs ->
          let cleared = Combat.clearCombat gs
              skipped = cleared {GameState.remaining = Turn.dropRestOfPhase phase (GameState.remaining gs)}
           in -- Both sweeps, the CR 724.1d arm's reason above: an "until end of
              -- combat" mana retention rides the UNIT and carries no Expiry, so
              -- the phase ending here has to end it too. The mana goes with this
              -- step's own CR 500.5 sweep in Engine.runStepThatBegan.
              Mana.endRetentionAtEndOf PhaseSelector.CombatPhase (Expiry.dropAtEndOf PhaseSelector.CombatPhase skipped)
        -- CR 724.2f: no player gets priority during this process. The SAME signal
        -- CR 724.1f raises, because the two rules want the same thing of
        -- Engine.priorityLoop -- return without settling and without granting
        -- another round -- and differ only in which step Engine.runStep lowers it
        -- at. CR 724.1d leaves the cleanup step at the head of the schedule and
        -- CR 724.2d the following phase, so "the abilities that triggered during
        -- this process are put onto the stack there" is one mechanism, not two.
        State.modify' (\gs -> gs {GameState.endTurnSignal = EndTurnSignal.Ended})
      _ -> pure ()
  Effect.GainControl (DurationRef.MkDurationRef duration ref) ->
    State.modify' $ \gs ->
      -- Enumerated ONCE by the shared sweep; a player recipient, an illegal slot
      -- (CR 608.2b) and a set that matched nothing all change nothing.
      case objectRefObjects legal resolving controller source gs ref of
        [] -> gs
        targets
          -- CR 800.4b: an object doesn't change to the control of a player who
          -- has left the game. `controller` is baked at trigger time (CR 113.8),
          -- and CR 800.4a's exile clause is not a state-based action, so it has
          -- already run and does not run again; without this guard the permanent
          -- would sit on the battlefield controlled by a player not in the game.
          | List.notElem controller (Game.stillPlaying gs) -> gs
          -- CR 611.2b's condition is baked against `legal` rather than `chosen`,
          -- so a slot CR 608.2b has emptied never starts the duration.
          | otherwise -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
              -- CR 611.2b: the duration never started, so nothing is stored and
              -- control never changed.
              Nothing -> gs
              Just expiry ->
                -- CR 613.1b / 611.2c: the new controller is `controller`, baked
                -- in now -- derived, never chosen. CR 302.6 re-Sicks it, unless
                -- control does not actually move, which is why the
                -- question is asked PER OBJECT and against the PROJECTED
                -- controller rather than against Object.owner.
                --
                -- CR 611.2c: one stored effect over the frozen id set.
                let (ts, gs1) = Game.freshTimestamp gs
                    eff =
                      ContinuousEffect.MkContinuousEffect
                        { ContinuousEffect.source = source,
                          ContinuousEffect.timestamp = ts,
                          ContinuousEffect.expiry = expiry,
                          ContinuousEffect.modification = Modification.SetController controller,
                          ContinuousEffect.affected = Affected.TheseObjects (Set.fromList targets)
                        }
                    sicken o = o {Object.sickness = Sickness.Sick}
                    moved = filter (\oid -> Projection.controllerOf oid gs /= Just controller) targets
                 in gs1
                      { GameState.continuousEffects = eff : GameState.continuousEffects gs1,
                        GameState.objects = foldr (Map.adjust sicken) (GameState.objects gs1) moved
                      }
  Effect.TakeExtraTurn takeExtraTurn -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        -- How many extra turns each named player is given, read ONCE: Ral
        -- Zarek's "for each coin that comes up heads" is the tally the flip
        -- before it bound. CR 107.2's posture for a quantity that cannot be
        -- evaluated: no turns at all.
        turns = Integer.toNaturalSaturating (Maybe.fromMaybe 0 (Quantity.evaluateFor viewOf context gs resolving source (TakeExtraTurn.count takeExtraTurn)))
        named = playerRefPlayers legal controller gs (TakeExtraTurn.player takeExtraTurn)
        -- CR 500.7: extra turns are added one at a time in APNAP order (CR
        -- 101.4). apnapOrder supplies the ORDER and `named` the MEMBERSHIP, so a
        -- departed player gets no turn; one named through a TARGET slot can still
        -- get an entry, and CR 800.4k catches it at the handoff.
        --
        -- Observable: APNAP order decides which of two players takes their extra
        -- turn first.
        takers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    -- CR 500.7: the most recently created turn is taken first, so each taker is
    -- pushed onto the head and the last pushed is the first popped -- a stack.
    --
    -- CR 500.11 / 113.7: the skips ride ALONG on each entry, naming that turn and
    -- no other; Engine.takeNextTurn installs them as the turn begins (see
    -- Pawl.Types.ExtraTurn).
    --
    -- CR 500.7: "if a player is given multiple extra turns, the extra turns are
    -- added one at a time" -- `turns` rounds, each pushing one turn per taker in
    -- APNAP order. Whether the rounds nest inside the APNAP walk or around it is
    -- observable only for an effect giving SEVERAL players SEVERAL turns each,
    -- which no card in data/cards/ prints; Ral Zarek names one player.
    let entry pid = ExtraTurn.MkExtraTurn {ExtraTurn.taker = pid, ExtraTurn.source = source, ExtraTurn.skipped = TakeExtraTurn.skips takeExtraTurn}
        pushRound ts = List.foldl' (\acc pid -> entry pid : acc) ts takers
    State.modify' (\g -> g {GameState.extraTurns = List.foldl' (\ts _ -> pushRound ts) (GameState.extraTurns g) [1 .. turns]})

-- The no-subgame executor (the ability path and every direct caller): a
-- PlaySubgame resolves as a draw here (see noSubgame).
applyEffect :: ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Game ()
applyEffect = applyEffectWith noSubgame

-- CR 615.5: run the additional effect of every prevention that has just been
-- applied. Drains GameState.pendingPreventionRiders, which Pawl.Engine.Damage
-- filled -- that module is below this one and cannot run a card's effects. Both
-- callers run it before the board can be observed and before the next
-- state-based action check, and that ordering IS the rule: a 1/1 dealt 6 through
-- Test of Faith ends as a 4/4 rather than dying to CR 704.5g first. Emptied
-- BEFORE the riders run, so a rider whose own damage is prevented appends to a
-- fresh queue instead of being re-run here.
runPreventionRiders :: Game ()
runPreventionRiders = do
  queued <- State.gets GameState.pendingPreventionRiders
  State.modify' (\gs -> gs {GameState.pendingPreventionRiders = Seq.empty})
  Foldable.traverse_ runPreventionRider queued

-- One queued prevention's additional effect; nothing runs unless the prevention
-- carries a rider.
--
-- CR 615.5's "amount of damage that was prevented" is the SUM over the
-- recipients this one application covered, rule 615.13 counting the application
-- and not the recipients (Divine Deflection throws one lot of 3, not a 2 and a
-- 1). A Quantity.InSlot read of the reserved Binding.eventAmount slot, published through
-- GameState.ambientAmounts rather than bound onto an object, because the shielded
-- recipient may be a PLAYER. Restored rather than cleared, so this cannot clobber
-- an outer amount.
--
-- `resolving` and `source` are both the rider's own source (CR 113.7). Every slot
-- the rider names is treated as a LEGAL target, CR 608.2b having been applied
-- when the installing spell resolved.
runPreventionRider :: Prevention.Prevention -> Game ()
runPreventionRider prevention = Foldable.for_ (Prevention.rider prevention) $ \rider -> do
  was <- State.gets GameState.ambientAmounts
  State.modify' (\gs -> gs {GameState.ambientAmounts = Map.insert Binding.eventAmount (sum (Prevention.amounts prevention)) was})
  let targets = PreventionRider.targets rider
      src = PreventionRider.source rider
  Foldable.traverse_
    (applyEffect src src (PreventionRider.controller rider) targets targets)
    (PreventionRider.effects rider)
  State.modify' (\gs -> gs {GameState.ambientAmounts = was})

-- CR 614.1c: run the effects of every as-enters rewrite that has applied and not
-- run yet. Drains GameState.pendingEntryEffects, which Pawl.Engine.Event filled
-- -- runPreventionRiders above in every structural respect and for the same
-- reason. Emptied before the effects run.
--
-- Its one caller is Pawl.Engine.Engine.performSettle, which runs it before the
-- SBA pass and before the trigger scan. What that ordering does NOT give is CR
-- 614.1c's own placement, inside the entry; see GameState.pendingEntryEffects.
runEntryEffects :: Game ()
runEntryEffects = do
  queued <- State.gets GameState.pendingEntryEffects
  State.modify' (\gs -> gs {GameState.pendingEntryEffects = Seq.empty})
  Foldable.traverse_ runEntryEffect queued

-- One entered permanent's as-enters effects, in printed order.
--
-- `resolving` and `source` are both the permanent (CR 113.7), runPreventionRider's
-- posture. The slot maps are empty because a static ability targets nothing (CR
-- 115.10a).
runEntryEffect :: PendingEntryEffect.PendingEntryEffect -> Game ()
runEntryEffect pending =
  Foldable.traverse_
    ( applyEffect
        (PendingEntryEffect.object pending)
        (PendingEntryEffect.object pending)
        (PendingEntryEffect.controller pending)
        Map.empty
        Map.empty
    )
    (PendingEntryEffect.effects pending)

-- CR 103.5b / CR 103.6: perform the effects of an action a card grants from a
-- player's hand. Pawl.Engine.Mulligan's window loops reach this through the
-- HandActionPerformer parameter (see Pawl.Types.HandActionPerformer).
--
-- The action does not use the stack, so there is nothing to put on it and no
-- modes to bind. Stands on the noSubgame floor: no hand action starts a subgame.
performHandAction :: HandActionPerformer.HandActionPerformer
performHandAction source player =
  Monad.mapM_
    ( applyEffect
        source
        source
        player
        -- CR 115.1: the reserved self slot is NOT a target, so there is no CR
        -- 608.2b legality question -- the card is in the acting player's hand by
        -- construction. Binding it is how "this card" is expressible with no
        -- self-variant opcode.
        (Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject source)))
        (Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject source)))
    )

-- CR 405.6c: run the non-mana effects of a mana ability, which
-- Pawl.Engine.Cost.tapForManaWith reaches through the
-- Pawl.Types.ManaAbilityPerformer parameter.
--
-- CR 605.3b gives the ability no stack object, so the SOURCE stands in for the
-- resolving one, performHandAction's posture.
--
-- Not implemented: an object of the ability's own to carry slots the bindings
-- below do not. A slot read that misses them falls through to the source
-- PERMANENT's bindings instead. Exact for the pool as it stands -- CR 605.1a
-- leaves a mana ability no targets to have bound, and the other slots
-- Pawl.CardSpec's activatedAbilityOffends admits a read of are ones the payment
-- binds and Cost.tapForManaWith's Paid branch drops, which no mana ability in
-- data/cards/ reads (#3124).
--
-- Stands on the noSubgame floor, performHandAction's reason: no mana ability
-- starts a subgame (#1900).
performManaAbility :: ManaAbilityPerformer.ManaAbilityPerformer
performManaAbility source controller =
  Monad.mapM_
    ( applyEffect
        source
        source
        controller
        -- CR 109.5's "you" is the player who activated the ability, and the
        -- reserved self slot is CR 113.7's source. Both are bound here rather
        -- than read off an object, because there is no ability object carrying
        -- them: Pawl.Engine.Activate.activateAbility stamps them for every
        -- ability that does go on the stack.
        manaAbilityBindings
        manaAbilityBindings
    )
  where
    manaAbilityBindings =
      Map.fromList
        [ (Binding.triggerSource, Set.singleton (Recipient.ToObject source)),
          (Binding.you, Set.singleton (Recipient.ToPlayer controller))
        ]

-- CR 603.7c: bind `target` into `slot` of `holder`'s binding environment, so a
-- delayed ability armed later in the SAME resolution can name the object.
-- `holder` is `resolving` -- the object ON THE STACK -- the same object
-- ArmDelayedTrigger captures from.
--
-- The slot IS visible to a later effect of the same fold: resolveSpellWith and
-- resolveModes each re-read Object.bindings before EACH effect (CR 608.2c), while
-- ArmDelayedTrigger and slotOne read live GameState rather than `chosen`.
bindSlot :: ObjectId -> SlotName -> ObjectId -> GameState -> GameState
bindSlot holder slot target gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObject target) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- bindSlot's plural: bind EVERY object one instruction produced into `slot`, for
-- a card that refers back to all of them at once ("those tokens", "those cards",
-- CR 400.7j). Same holder and same further reason (CR 603.7c).
--
-- Readable mid-fold without either path's per-effect re-read, because every
-- reader goes through slotGroup, which reads live GameState. It has to: this
-- rides the binding's `objects` field, while `chosen` reads only `target`, where
-- bindSlot's SINGLE object lands.
bindObjectsSlot :: ObjectId -> SlotName -> Seq.Seq ObjectId -> GameState -> GameState
bindObjectsSlot holder slot targets gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObjects targets) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- CR 701.8b's "put into a graveyard this way", asked of one CR 400.7 incarnation:
-- is it a card, and is it in a graveyard? Both halves are questions about where
-- the object ended up rather than about the destruction, which is why they are
-- asked of the board after the funnel ran (see Effect.Destroy's arm).
--
-- WHOSE graveyard is not asked: CR 400.3 files the arrival under the owner and
-- no printed reader of this slot distinguishes them. Object.zone is the read
-- because it tracks the zone sets and answers in one lookup; CR 111.6 is the
-- card half.
isCardInAGraveyard :: ObjectId -> GameState -> Bool
isCardInAGraveyard oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> Object.zone obj == Zone.Graveyard && not (Game.isToken oid gs)

-- The default runner for the resolutions the live loop does not drive: a
-- PlaySubgame effect reports a draw and binds nothing. Pawl.Engine.Engine's
-- priority loop passes the real runner to BOTH halves of CR 729.1a's "spell or
-- ability" (resolveSpellWith, resolveModesWith), which is every object that
-- resolves off the stack.
--
-- Not implemented: a subgame started by an instruction that never reaches the
-- stack -- CR 103.5b/103.6's hand actions (performHandAction), CR 615.5's
-- prevention riders and CR 614.1c's as-enters effects all fold the bare
-- applyEffect and land here (#1900).
noSubgame :: Game Result
noSubgame = pure Result.Drawn

-- Bind a PLAYER a resolution named into `slot` on `holder`, bindSlot's mirror
-- with a player recipient (ToPlayer) rather than an object.
--
-- Every caller passes `resolving` -- the object on the stack, whose bindings both
-- resolution loops re-read before each effect: CR 729.1b's subgame winner and CR
-- 608.2d's chosen opponent are each read by the effect after the one that bound
-- it, through that re-read (`legalNow`).
--
-- A holder that no longer EXISTS writes to GameState.detachedBindings instead,
-- which `liveBindings` reads back. Map.adjust is silent on a missing key, so
-- without that branch the binding is simply dropped.
--
-- CR 729.5 is the branch's reason: a wish cast inside a subgame can take the
-- resolving spell's own card, and "the spell or ability that created the subgame
-- finishes resolving, even if it was created by a spell card that's no longer on
-- the stack" is what keeps the resolution going anyway. It is not the only way an
-- id can cease mid-resolution -- Effect.ExileThisSpell mints a fresh incarnation
-- (CR 400.7) and leaves the old id naming nothing too -- and the read side treats
-- both the same.
--
-- The sibling writers below are NOT given the same fallback. bindPlayersSlot's
-- one caller is CR 118.12a's per-player gate, which cannot lose its holder.
-- bindAmountSlot writes onto `source`, which for a SPELL is the same id a subgame
-- can take, but an amount is read back by Pawl.Engine.Quantity's own lookup
-- rather than through liveBindings, so a fallback here would not reach it
-- (#2493).
bindPlayerSlot :: ObjectId -> SlotName -> PlayerId -> GameState -> GameState
bindPlayerSlot holder slot player gs =
  let binding = Binding.toPlayer player
      put obj = obj {Object.bindings = Map.insert slot binding (Object.bindings obj)}
   in if Map.member holder (GameState.objects gs)
        then gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}
        else gs {GameState.detachedBindings = Map.insertWith Map.union holder (Map.singleton slot binding) (GameState.detachedBindings gs)}

-- CR 608.2c: the bindings a resolution reads before each of its own effects --
-- the LIVE ones off the stack object, so a slot an earlier effect DEFINED is
-- visible to a later one. `obj` is the object as resolution began, the fallback
-- for the reads below.
--
-- CR 729.5 is why the fallback is not just that snapshot: "the spell or ability
-- that created the subgame finishes resolving, even if it was created by a spell
-- card that's no longer on the stack". A wish cast INSIDE a subgame may name the
-- very spell that is resolving -- CR 729.4 puts the main game's stack outside the
-- subgame -- and Pawl.Engine.Setup.applyCrossings then deletes that object before
-- the resolution resumes. What it bound meanwhile is in
-- GameState.detachedBindings, and takes precedence over the announced bindings
-- the snapshot carries.
--
-- Not implemented: the readers that look the resolving object up by id rather
-- than coming through here -- Pawl.Engine.Count.playersFor's EachPlayerExcept
-- arm and Pawl.Engine.Quantity's InSlot arm -- stay unanswered on that path
-- (#2493).
liveBindings :: Object.Object -> ObjectId -> GameState -> Map SlotName Binding.Type.Binding
liveBindings obj oid gs = case Game.lookupObject oid gs of
  Just live -> Object.bindings live
  Nothing -> Map.union (Map.findWithDefault Map.empty oid (GameState.detachedBindings gs)) (Object.bindings obj)

-- bindPlayerSlot's plural: bind SEVERAL players a resolution named into `slot` on
-- `holder`. CR 118.12a's per-player gate is the one caller, and the set is
-- written even when it is EMPTY -- Binding.toRecipients turns that into an
-- unbound slot, so a branch nobody took leaves the previous clause's answer
-- unreadable rather than standing.
bindPlayersSlot :: ObjectId -> SlotName -> Set PlayerId -> GameState -> GameState
bindPlayersSlot holder slot players gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toPlayers players) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- CR 701.8b: bind how many permanents a destruction actually destroyed into
-- `slot` on `holder`, readable as Quantity.InSlot. Binds a NUMBER, which rides
-- the binding field CR 601.2b's chosen X rides. Left behind after the resolution,
-- harmless and unreadable: only an effect naming this slot can see it, and a
-- second sweep overwrites the value before reading it.
--
-- `holder` is the effect's `source`, NOT `resolving`, because an amount is read
-- back by Quantity.evaluateFor aimed at `source` (CR 608.2h) while an object
-- binding is read back by ArmDelayedTrigger off the stack object.
bindAmountSlot :: ObjectId -> SlotName -> Natural -> GameState -> GameState
bindAmountSlot holder slot n gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toAmount n) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- CR 119.5 / 701.12c: move a player's life total by a delta. A DOWNWARD delta is
-- a life loss and goes through Event.resolveLifeLoss first, CR 614.1's funnel for
-- the class, so a replacement watching life loss reaches it and the SETTLED loss
-- is what moves the total -- which is why a player may end up somewhere other
-- than the total the effect named. An upward delta is a life GAIN and proposes
-- nothing here (#3086).
--
-- The one road for every arm that arrives at a TOTAL rather than at an amount:
-- Effect.SetLifeTotal, Effect.ExchangeLifeTotals and
-- Effect.RedistributeLifeTotals.
changeLifeByDelta :: PlayerId -> Integer -> Game ()
changeLifeByDelta pid delta =
  if delta < 0
    then do
      settled <- Event.resolveLifeLoss LifeLossCause.ByEffect pid (Integer.toNaturalSaturating (negate delta))
      Event.changeLife pid (negate (toInteger settled))
    else Event.changeLife pid delta

-- CR 701.23: do to a found card what the search said -- a move for every
-- destination and, for one of them, a CR 701.20a reveal first, through the CR
-- 400.7 funnel either way.
putFound :: PlayerId -> ObjectId -> SearchDestination.SearchDestination -> ObjectId -> Game ()
putFound searcher source destination cardId = case destination of
  -- Nature's Lore's "put that card onto the battlefield": the plain move, with
  -- no rider naming how it enters, so CR 110.5b's defaults stand and the card
  -- arrives untapped. That is why this is Event.changeZone rather than putTapped
  -- below -- the difference between the two arms is the card's own sentence.
  SearchDestination.Battlefield -> Event.changeZone cardId Zone.Battlefield
  SearchDestination.BattlefieldTapped -> putTapped cardId
  -- The reveal comes FIRST, in the card's own order, and CR 701.20b makes that an
  -- order rather than decoration: revealing does not move the card. The two lines
  -- do not commute -- swapped, CR 400.7 has already ceased `cardId` and the
  -- reveal shows nothing.
  SearchDestination.RevealThenHand -> do
    Event.reveal RevealCause.Ordinary searcher cardId
    Event.changeZone cardId Zone.Hand
  -- Hoarding Dragon's "exile it": the move alone, with NO Event.reveal ahead of
  -- it (CR 701.23e). Which card this instruction exiled is CR 607.2a's link,
  -- filed by recordExiledWith off the effect that ran rather than here.
  SearchDestination.Exile -> Event.changeZone cardId Zone.Exile
  -- Auratouched Mage's "put that Aura card onto the battlefield attached to it".
  -- The seed is the RECIPIENT Attach.attachmentFor produced rather than a
  -- hand-built ToObject, for the reason Event.changeZoneAttaching's CR 303.4f arm
  -- gives: Sba.stillLegalEnchant compares the (pool, tag) pair, so a mismatched
  -- tag would have CR 704.5m bury the Aura on the next pass.
  --
  -- Supplying a seed at all is what keeps CR 303.4f's host prompt out of this
  -- move: the effect DOES specify what the Aura will enchant, so its controller
  -- chooses nothing.
  --
  -- CR 110.2a: it enters under the SEARCHER, the player whose effect is putting
  -- it there -- not under its owner, which is what the other arms' Event.changeZone
  -- leaves it to, since none of them puts anything onto the battlefield for
  -- someone other than its owner.
  --
  -- Nothing is CR 303.4i's "the Aura remains in its current zone" -- unreachable
  -- from a filter naming Filter.CanAttachToSubject, since that atom is this same
  -- function, and the honest answer for a card whose filter does not.
  --
  -- The OUTER branch is the card's own "If this creature is still on the
  -- battlefield ... Otherwise", asked of the SOURCE's liveness rather than of
  -- attachmentFor's Nothing. The two are different questions and the rules give
  -- them opposite answers: an Aura a LIVE host cannot legally hold stays in the
  -- library (CR 303.4i), while an Aura whose host has gone is revealed and put
  -- into its owner's hand (CR 608.2h; CR 113.7a is what keeps the ability
  -- resolving at all with its source gone). Branching on Nothing alone would
  -- send the first of those to the hand.
  --
  -- The ORDER of those two questions is a REGRESSION FENCE rather than a proven
  -- behaviour: rule 303.4i's Nothing is unreachable for the one card that reaches
  -- this arm, whose filter names Filter.CanAttachToSubject, so both readings
  -- produce the same board and swapping them reddens nothing. A card whose search
  -- filter did NOT ask rule 701.3a would be its observer.
  SearchDestination.BattlefieldAttachedToSource -> do
    gs <- State.get
    if Set.member source (GameState.battlefield gs)
      then case Attach.attachmentFor cardId (Recipient.ToObject source) gs of
        Nothing -> pure ()
        Just seed ->
          Monad.void
            (Event.changeZoneAttaching Nothing Set.empty cardId Zone.Battlefield LibraryPosition.defaultValue (Just seed) TapState.Untapped Map.empty (Just searcher) Nothing Facing.FaceUp False CarryOver.NotCarried)
      else do
        -- CR 701.20b makes the order matter, for RevealThenHand's reason above:
        -- swapped, CR 400.7 has already ceased `cardId` and the reveal shows
        -- nothing. The reveal is the CARD's own instruction (CR 701.23e), which
        -- is why it is written here rather than in the searching rule.
        Event.reveal RevealCause.Ordinary searcher cardId
        Event.changeZone cardId Zone.Hand

-- Put a found card onto the battlefield tapped (CR 701.23's Evolving Wilds
-- shape). changeZone mints a new object; tap it by id after the move.
--
-- A direct write and NOT Pawl.Engine.Event.tap, for CR 603.2e's reason: this is
-- the permanent ENTERING the battlefield tapped, and an ability that triggers when
-- a permanent "becomes tapped" doesn't trigger if the permanent enters in that
-- state. Routing it through the funnel would fire such an ability.
putTapped :: ObjectId -> Game ()
putTapped cardId = do
  before <- State.get
  Event.changeZone cardId Zone.Battlefield
  moved <- State.get
  case newestBattlefieldOf cardId before moved of
    Nothing -> pure ()
    Just newId ->
      State.put moved {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) newId (GameState.objects moved)}

-- The single battlefield id present after a one-object move that was absent
-- before (changeZone mints a fresh id, CR 400.7).
newestBattlefieldOf :: ObjectId -> GameState -> GameState -> Maybe ObjectId
newestBattlefieldOf _ before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    newId : _ -> Just newId
    [] -> Nothing

-- Write a whole new order back to a player's library: the shuffle after a CR
-- 701.23 search, and CR 701.22a's scry, CR 701.25a's surveil and CR 701.29a's
-- fateseal.
reorderLibrary :: PlayerId -> [ObjectId] -> GameState -> GameState
reorderLibrary pid order gs =
  gs {GameState.library = Map.insert pid (Seq.fromList order) (GameState.library gs)}

-- CR 701.22a: one player's scry. A short library is looked at as far as it goes;
-- rule 701.22 states no penalty for scrying more than there is.
--
-- The library is REWRITTEN rather than funnelled through Event.changeZone:
-- nothing crosses a zone boundary, so CR 400.7 mints no new incarnation and the
-- ids the prompt named still move. Looking mints nothing either (CR 701.20b).
scryOne :: Integer -> PlayerId -> Game ()
scryOne n pid = do
  gs <- State.get
  let whole = Game.zoneMembers Zone.Library pid gs
      looked = List.genericTake n whole
      beneath = List.genericDrop n whole
      -- The engine never makes the player's choice, but does not ask a question
      -- with one answer either: nothing to look at, or a single looked-at card
      -- that is the whole library. One card with more beneath it IS a decision.
      decided = case looked of
        [] -> False
        [_] -> not (null beneath)
        _ -> True
  Monad.when decided $ do
    answer <- Game.choose (Prompt.ChooseScry (Decide.deciderFor pid gs) pid looked)
    let (toBottom, onTop) = splitLooked looked answer
    State.modify' (reorderLibrary pid (onTop <> beneath <> toBottom))
  -- CR 701.22d: recorded AFTER rule 701.22a's process and OUTSIDE the guard
  -- above, that rule firing "even if some or all of those actions were
  -- impossible".
  State.modify' (Event.recordEvent (GameEvent.Scried pid))

-- Repair a look-and-split answer into the two groups the effect then moves: the
-- cards going AWAY from the top, and the cards staying on top in reading order.
--
-- Filtered, deduped and COMPLETED, Effect.Discard's arm's posture: a card named
-- in neither list stays on top behind the ones that were named, and a card named
-- in BOTH goes away, the first list winning. One function for all three keyword
-- actions, the repair being a question about the ANSWER.
splitLooked :: [ObjectId] -> ([ObjectId], [ObjectId]) -> ([ObjectId], [ObjectId])
splitLooked looked (away, kept) =
  let named xs = List.nub (filter (\c -> List.elem c looked) xs)
      leaving = named away
      onTop = filter (\c -> List.notElem c leaving) (named kept)
      unnamed = filter (\c -> List.notElem c leaving && List.notElem c onTop) looked
   in (leaving, onTop <> unnamed)

-- CR 701.25a: one player's surveil -- scryOne's shape over a different
-- destination.
--
-- Half of it IS a zone change: the graveyard cards go through Event.changeZone in
-- the order the answer named them, so the first named ends up deepest (CR 404.1),
-- an order that is the player's rather than the engine's (CR 404.3).
--
-- The ELISION is scryOne's minus its one-card case: the graveyard and the top of
-- the library are still two different places.
surveilOne :: Integer -> PlayerId -> Game ()
surveilOne n pid = do
  gs <- State.get
  let whole = Game.zoneMembers Zone.Library pid gs
      looked = List.genericTake n whole
      beneath = List.genericDrop n whole
  Monad.unless (null looked) $ do
    answer <- Game.choose (Prompt.ChooseSurveil (Decide.deciderFor pid gs) pid looked)
    let (toGraveyard, onTop) = splitLooked looked answer
    -- Order-independent: Game.removeFromZones takes each mover out of the library
    -- by identity rather than by position.
    State.modify' (reorderLibrary pid (onTop <> beneath))
    Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard) toGraveyard
  -- CR 701.25d, scryOne's placement and for its rule: outside the guard, so a
  -- surveil of an empty library is still a surveil.
  State.modify' (Event.recordEvent (GameEvent.Surveiled pid))

-- CR 701.29a: one player's fateseal -- scryOne's question over an opponent's
-- library.
--
-- TWO choices, both the fatesealer's and in this order: which opponent, then how
-- to split. The first is elided at one candidate (CR 102.2) and filtered rather
-- than trusted; the library's owner is asked neither. CR 102.1's opponents are
-- Game.stillPlaying's, so a seat that has left (CR 104.3a) is not offered.
fatesealOne :: ObjectId -> Integer -> PlayerId -> Game ()
fatesealOne source n pid = do
  gs <- State.get
  let opponents = Game.opponentsOf pid gs
  victim <- case opponents of
    [] -> pure Nothing
    [sole] -> pure (Just sole)
    first : second : rest -> do
      let offered = first NonEmpty.:| (second : rest)
      answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor pid gs) pid source offered)
      pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
  Monad.forM_ victim $ \owner -> do
    -- Re-read rather than reusing the state the opponent choice was made
    -- against: a prompt is the one place this function yields.
    chosen <- State.get
    let whole = Game.zoneMembers Zone.Library owner chosen
        looked = List.genericTake n whole
        beneath = List.genericDrop n whole
        decided = case looked of
          [] -> False
          [_] -> not (null beneath)
          _ -> True
    Monad.when decided $ do
      answer <- Game.choose (Prompt.ChooseFateseal (Decide.deciderFor pid chosen) pid owner looked)
      let (toBottom, onTop) = splitLooked looked answer
      State.modify' (reorderLibrary owner (onTop <> beneath <> toBottom))

-- CR 701.44a: one permanent's explore.
--
-- The controller comes from LAST KNOWN INFORMATION (CR 701.44c), so a permanent
-- killed while the instruction was on the stack still explores; its counter then
-- lands on nothing while the reveal and the choice still happen, which is rule
-- 701.44b's "even if some or all of those actions were impossible".
--
-- The reveal is PUBLIC (CR 701.20a) and so rides Event.reveal, unlike scry's
-- private look at the same position.
--
-- "If a land card is revealed" is asked of the revealed card's own CR 613
-- projection: rule 613.1 starts from the actual object and names no zone, so a
-- library card is folded exactly as a permanent is, and a layer-4 type change
-- (CR 613.1d) reaches it there. An id nothing is filed under projects no card
-- types, so it is no land card -- the answer the printed face gave.
exploreOne :: ObjectId -> Game ()
exploreOne oid = do
  gs <- State.get
  case Projection.controllerWithLastKnown oid gs of
    -- An id nothing was ever filed under: nobody explores and nothing happens.
    Nothing -> pure ()
    Just pid -> do
      case Game.zoneMembers Zone.Library pid gs of
        [] -> grow pid
        top : _ -> do
          Event.reveal RevealCause.Ordinary pid top
          after <- State.get
          let isLand = Set.member CardType.Land (Filter.cardTypes (Projection.viewOfObject top after))
          if isLand
            then Event.changeZone top Zone.Hand
            else do
              grow pid
              asked <- State.get
              decision <- Game.choose (Prompt.ChooseExplore (Decide.deciderFor pid asked) pid oid top)
              Monad.when (decision == OptionalDecision.Exercises) (Event.changeZone top Zone.Graveyard)
      -- CR 701.44b: the permanent explores once the whole of rule 701.44a is
      -- done, so this comes after every branch above. Inside the Just, since an
      -- id nobody ever controlled explores nothing; but not inside the library
      -- case, that rule firing even when the actions were impossible.
      State.modify' (Event.recordEvent (GameEvent.Explored oid))
  where
    -- CR 122.6 through the one counter funnel, so a CR 614.1 counter replacement
    -- (Hardened Scales) gets its opportunity against this placement too.
    grow pid = Monad.void (Event.putCounters (CounterCause.ByEffect pid) oid CounterKind.PlusOnePlusOne 1)
