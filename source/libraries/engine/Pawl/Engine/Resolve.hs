module Pawl.Engine.Resolve where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
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
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Daytime as Daytime
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.Dungeon as Dungeon
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Ring as Ring
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Amass as Amass.Type
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.Clause as Clause
import Pawl.Types.ClauseIndex (ClauseIndex)
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamageKind as DamageKind
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
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.ForEach as ForEach
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.HandActionPerformer as HandActionPerformer
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaUnit as ManaUnit
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
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.ObjectRef (ObjectRef)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import Pawl.Types.PlayerRef (PlayerRef)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity.Type
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.RequireBlock as RequireBlock
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import Pawl.Types.SlotArity (SlotArity)
import qualified Pawl.Types.SlotArity as SlotArity
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- The read side of the D4 dataflow lint answers WHICH slots and HOW MANY
-- recipients apiece (Pawl.Types.SlotArity): a slot read through an ObjectRef can
-- hold CR 601.2c's "up to two", and one read any other way cannot. These
-- combinators are the join, which takes the conservative arity wherever two reads
-- disagree -- and are written out rather than left to Map's own Monoid, whose
-- union is left-biased and would keep whichever arity it saw first.
joinTwo :: Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity
joinTwo = Map.unionWith min

joinSlots :: [Map.Map SlotName SlotArity] -> Map.Map SlotName SlotArity
joinSlots = foldr joinTwo Map.empty

oneSlot :: SlotName -> Map.Map SlotName SlotArity
oneSlot slot = Map.singleton slot SlotArity.One

insertOne :: SlotName -> Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity
insertOne slot = joinTwo (oneSlot slot)

-- A Quantity reads a slot to evaluate a number against ONE object
-- (Quantity.AgainstSlot), so every slot it names is read singly.
quantitySlots :: Quantity.Type.Quantity -> Map.Map SlotName SlotArity
quantitySlots = Map.fromSet (const SlotArity.One) . Quantity.slots

-- The slots a PlayerRef reads. Only InSlot names one; the others are answered
-- from the evaluation context alone. Factored out of slotsOf below so the
-- recursion into PlayerRef is stated once.
playerRefSlots :: PlayerRef -> Map.Map SlotName SlotArity
playerRefSlots ref = case ref of
  PlayerRef.EachPlayer -> Map.empty
  -- One slot, read singly: the excluded seat is one player, and a slot naming
  -- several excludes nobody rather than several (playerRefPlayers' legalOne).
  PlayerRef.EachPlayerExcept slot -> Map.singleton slot SlotArity.One
  PlayerRef.Relative _ -> Map.empty
  PlayerRef.InSlot slot -> Map.singleton slot SlotArity.One
  -- InSlot's baked half names a seat, not a slot. Unreachable from card data,
  -- which this lint's whole input is (Pawl.CardSpec sweeps the pool for one).
  PlayerRef.Specific _ -> Map.empty
  -- A fold's candidate is not a slot either: it comes from the member being
  -- read, and a card writes it (Malignus), so this arm is reachable and empty.
  PlayerRef.Candidate -> Map.empty
  -- A TARGET slot like InSlot's, read at arity one: the reference asks who
  -- controls the object the slot names, and a slot naming several objects names
  -- no one controller. Spikeshell Harrier is what writes it.
  PlayerRef.ControllerOfBound slot -> Map.singleton slot SlotArity.One

-- The slots an AffectedPlayers reads. Only the Named arm does, and only ever one
-- player: a card writes AffectedPlayers SlotName, whose Named payload is a slot
-- name rather than the seat Pawl.Engine.Resolve bakes it into.
affectedPlayersSlots :: AffectedPlayers.AffectedPlayers SlotName -> Map.Map SlotName SlotArity
affectedPlayersSlots affected = case affected of
  AffectedPlayers.Scoped _ -> Map.empty
  AffectedPlayers.Named slot -> Map.singleton slot SlotArity.One

-- The slots a Chooser reads. Only the slot-named chooser does, and only ever one
-- player -- Pawl.Types.PlayerRef's InSlot arity, for its reason.
chooserSlots :: Chooser.Chooser -> Map.Map SlotName SlotArity
chooserSlots chooser = case chooser of
  Chooser.TheController -> Map.empty
  Chooser.EachInScope -> Map.empty
  Chooser.BoundInSlot slot -> Map.singleton slot SlotArity.One

-- The slots an ObjectRef reads. Only InSlot names one directly; the sweeping
-- arms are swept at resolution and name nothing at cast, so a card whose only
-- object reference is a set declares no target slot and CR 608.2b has nothing to
-- fizzle (CR 115.10a). TopOfLibrary names no object slot either -- it reads a
-- POSITION -- but the PlayerRef saying whose library may name a player slot, and
-- that read is playerRefSlots' already.
objectRefSlots :: ObjectRef -> Map.Map SlotName SlotArity
objectRefSlots ref = case ref of
  ObjectRef.InSlot slot -> Map.singleton slot SlotArity.Many
  ObjectRef.EachMatching _ -> Map.empty
  ObjectRef.EachCardInGraveyard {} -> Map.empty
  ObjectRef.EachCardInYourHand -> Map.empty
  ObjectRef.EachCardExiledWithSource {} -> Map.empty
  ObjectRef.EachPlayer -> Map.empty
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player _) -> playerRefSlots player
  -- A PlayerScope names players by their relation to the effect's controller (CR
  -- 109.5) rather than out of a slot, so whose graveyards are drawn from names
  -- none. WHO CHOOSES may: Skullwinder's chosen opponent is read out of the slot
  -- an earlier effect bound.
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser _ _) -> chooserSlots chooser
  -- The choosers, who are also the hands' owners (CR 402.3), so the one
  -- PlayerRef is the whole read -- Karn Liberated's "+4: TARGET player exiles a
  -- card from their hand" names its seat with a target slot.
  ObjectRef.ChosenCardInHand player -> playerRefSlots player

-- The slots a MonarchTarget reads: only the targeted arm names one. Written out
-- rather than routed through playerRefSlots because MonarchTarget is its own
-- three-arm type -- CR 725.2's ControllerOfSource has no PlayerRef spelling.
monarchTargetSlots :: MonarchTarget.MonarchTarget -> Map.Map SlotName SlotArity
monarchTargetSlots target = case target of
  MonarchTarget.TheController -> Map.empty
  MonarchTarget.ControllerOfSource -> Map.empty
  MonarchTarget.InSlot slot -> Map.singleton slot SlotArity.One

-- The slots an ExchangeSides reads, and how many recipients out of each: the
-- controller's own side comes from nowhere, so WithController reads one target,
-- while BetweenTargets takes BOTH sides out of one slot (CR 601.2c) and so must
-- see the whole set.
exchangeSidesSlots :: ExchangeSides.ExchangeSides -> Map.Map SlotName SlotArity
exchangeSidesSlots sides = case sides of
  ExchangeSides.WithController slot -> Map.singleton slot SlotArity.One
  ExchangeSides.BetweenTargets slot -> Map.singleton slot SlotArity.Many

-- The one legitimate home of `case effect of`: this module is the VM's opcode
-- semantics (design.md section 1), and everything else asks classifications.
-- slotsOf is the read half of the dataflow lint.
--
-- Every arm carrying a Quantity joins quantitySlots over it, a Quantity.InSlot
-- being a slot read like any other. X is not one of those reads; readsX below is
-- X's own half of the contract.
slotsOf :: Effect Card.Type.Card -> Map.Map SlotName SlotArity
slotsOf effect = case effect of
  -- The dealer is a slot READ like any other (CR 120.2b), and one object rather
  -- than a set: a damage event has one source (CR 120.1).
  Effect.DealDamage (DealDamage.MkDealDamage ref quantity dealer) ->
    joinTwo
      (joinTwo (objectRefSlots ref) (quantitySlots quantity))
      (maybe Map.empty oneSlot dealer)
  -- The modification's own quantities read slots too, through
  -- Projection.quantitiesOf. No card in the pool reads a slot there, but a
  -- dangling one would otherwise slip past the lint entirely.
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    joinSlots [objectRefSlots ref, joinSlots (fmap quantitySlots (Projection.quantitiesOf modification)), durationSlots duration]
  Effect.ChangeText (ChangeText.MkChangeText _ _ slot) -> oneSlot slot
  Effect.AddMana _ -> Map.empty
  -- BOTH refs: Extract's library owner is the slot it targets, and a slot read
  -- only by that ref would otherwise look dangling to the dataflow lint.
  Effect.Search (Search.MkSearch searcher owner quantity _ _) ->
    joinSlots [playerRefSlots searcher, playerRefSlots owner, quantitySlots quantity]
  Effect.ExileAllGraveyards -> Map.empty
  Effect.Proliferate -> Map.empty
  Effect.Bolster quantity -> quantitySlots quantity
  Effect.Amass (Amass.Type.MkAmass quantity _) -> quantitySlots quantity
  Effect.Blight (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.TemptWithTheRing -> Map.empty
  Effect.Venture -> Map.empty
  Effect.ExileHandThenDraw -> Map.empty
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot _ quantity) -> insertOne slot (quantitySlots quantity)
  -- CR 727.5's exemption is an ObjectRef like any other, so a card that named
  -- its exempted cards out of a slot would read that slot here.
  Effect.RestartGame exempt -> foldMap objectRefSlots exempt
  Effect.ControlPlayerNextTurn slot -> oneSlot slot
  -- The third field is a DEFINITION (how many this sweep destroyed), not a read,
  -- so it belongs to boundSlots below and must not appear here -- Create's and
  -- PlaySubgame's slots take the same posture.
  Effect.Destroy (Destroy.MkDestroy ref _ _) -> objectRefSlots ref
  Effect.Sacrifice slot -> oneSlot slot
  Effect.TurnFaceDown slot -> oneSlot slot
  Effect.RemoveFromCombat slot -> oneSlot slot
  Effect.BecomesBlocked slot -> oneSlot slot
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ _ _) -> objectRefSlots ref
  Effect.Draw (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  -- The tally's slot is a DEFINITION (how many of them counted), not a read, so
  -- it belongs to boundSlots below -- Destroy's third field takes the same
  -- posture, for the same reason.
  Effect.Mill (Mill.MkMill ref quantity _) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.Reveal ref -> objectRefSlots ref
  -- The bound slot is a DEFINITION and not a read, the posture Mill's tally
  -- above takes; boundSlots below is where it is reported.
  Effect.LookAt (LookAt.MkLookAt ref _) -> objectRefSlots ref
  Effect.Scry (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.Explore ref -> objectRefSlots ref
  Effect.Discard (Discard.MkDiscard slot quantity) -> insertOne slot (quantitySlots quantity)
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.ExchangeLifeTotals sides -> exchangeSidesSlots sides
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.RedistributeLifeTotals -> Map.empty
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  -- The floor beside them names no slot: it is a printed literal.
  Effect.DecreaseSpeed d -> joinTwo (playerRefSlots (SpeedDecrease.player d)) (quantitySlots (SpeedDecrease.quantity d))
  -- Create's slot is a DEFINITION, not a read: it is not a target, so the D4
  -- lint must not see it here. Its Quantity is a read like every other.
  Effect.Create (Create.MkCreate quantity _ _ _) -> quantitySlots quantity
  -- A READ, unlike Create's slot: the ref names the permanent being copied,
  -- which is a target on Cackling Counterpart and the reserved self slot on
  -- Watchful Radstag. Both are reads; only the first is a target (CR 115.10a).
  -- Its Quantity is a read like Create's.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref) -> joinTwo (quantitySlots quantity) (objectRefSlots ref)
  -- The ReplacementEffect carries no Quantity, but the Duration and Condition each
  -- carry two, and a Quantity.InSlot inside either is a slot read. No card writes
  -- one, but a dangling one would slip past the lint as ModifyTarget's would.
  Effect.Replace (Replace.MkReplace duration _ _ condition _) -> joinTwo (durationSlots duration) (joinSlots (fmap conditionSlots (Maybe.maybeToList condition)))
  -- The PlayerRef may name a target slot -- Fatigue's "target player".
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase ref _) -> playerRefSlots ref
  -- The ObjectRef names the shielded recipient and the Quantity the shield's size.
  -- CR 615.5's rider reads slots of its own -- Test of Faith's "that creature"
  -- is the spell's own target slot -- so its reads join this effect's, LESS the
  -- reserved amount slot. That one is not a dangling read: the prevention itself
  -- binds it, exactly as a trigger condition binds one for its ability's effects
  -- (Event.eventBindingSlots), and Resolve.runPreventionRiders is the writer.
  -- Subtracted here rather than added to `boundSlots` below, because the
  -- Pawl.CardSpec sweep that forbids a CARD binding a reserved name reads that
  -- one.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration ref quantity rider) ->
    joinSlots
      [ durationSlots duration,
        objectRefSlots ref,
        quantitySlots quantity,
        Map.delete Binding.eventAmount (joinSlots (fmap slotsOf (Foldable.toList rider)))
      ]
  -- The same two reads, minus the shield size this opcode does not carry.
  Effect.PreventAllDamage (DurationRef.MkDurationRef duration ref) -> joinTwo (durationSlots duration) (objectRefSlots ref)
  -- BOTH ObjectRefs. Turn the Tables reads its target slot through the
  -- DESTINATION ref, so naming only the source side would leave a declared
  -- target unread and pass the reads-equal-declares lint on a card that fizzles.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ srcRef destRef) ->
    joinSlots [durationSlots duration, objectRefSlots srcRef, objectRefSlots destRef]
  Effect.Counter slot -> oneSlot slot
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity ref) -> joinTwo (objectRefSlots ref) (quantitySlots quantity)
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity slot) -> insertOne slot (quantitySlots quantity)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters ref _ quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters ref _ quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.Tap ref -> objectRefSlots ref
  Effect.Untap ref -> objectRefSlots ref
  Effect.Detain ref -> objectRefSlots ref
  Effect.DoesNotUntapNext ref -> objectRefSlots ref
  Effect.Transform ref -> objectRefSlots ref
  Effect.AddPhases _ -> Map.empty
  Effect.GainControl (DurationRef.MkDurationRef _ ref) -> objectRefSlots ref
  Effect.ArmDelayedTrigger {} -> Map.empty
  -- The AffectedPlayers may name a target slot -- Cease-Fire's "target player".
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers _ affected _) -> affectedPlayersSlots affected
  -- Both refs, for Tap and Untap's reason: either may name a slot.
  Effect.RequireBlock (RequireBlock.MkRequireBlock _ blocker attacker) -> joinTwo (objectRefSlots blocker) (objectRefSlots attacker)
  Effect.CreateEmblem {} -> Map.empty
  -- CR 725.1's crown names a target slot only in the InSlot arm (Denethor's
  -- "target player"); the other two derive their player and read nothing.
  Effect.BecomeMonarch target -> monarchTargetSlots target
  -- A READ: the slot names the permanent gaining the designation. Reserved rather
  -- than a target on renown and monstrosity (rule 702.112a's "it") and a CR 115.6
  -- target on Rune-Brand Juggler's suspect, but the lint's question is which slots
  -- the effect names, not which are targets.
  Effect.Designate (Designate.MkDesignate _ slot) -> oneSlot slot
  -- A READ of whatever slot the ref names, Tap's arm: rule 701.60a's ending can
  -- reach a set instead.
  Effect.Unsuspect ref -> objectRefSlots ref
  -- A READ, Designate's: the slot names the permanent rule 702.100a's
  -- counter goes on.
  Effect.Evolve slot -> oneSlot slot
  Effect.Mentor slot -> oneSlot slot
  Effect.Train slot -> oneSlot slot
  Effect.ItBecomes _ -> Map.empty
  Effect.ExileUntilMonarch slot -> oneSlot slot
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting card slot) -> joinSlots [oneSlot card, oneSlot slot]
  Effect.Attach slot -> oneSlot slot
  Effect.AttachTarget (AttachTarget.MkAttachTarget slot _) -> oneSlot slot
  -- CR 729.1/729.1b: PlaySubgame's slot is a DEFINITION (the subgame's winner,
  -- bound once it ends), not a read -- same shape as Create's slot.
  Effect.PlaySubgame _ -> Map.empty
  -- A DEFINITION too, PlaySubgame's exactly: the opponent is chosen as this
  -- effect is applied (CR 608.2d) and bound, never read.
  Effect.ChooseOpponent _ -> Map.empty
  -- The PlayerRef may name a target slot -- Time Warp's "target player".
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn ref _) -> playerRefSlots ref
  -- Both halves may name a slot: the ObjectRef names what is shuffled, and the
  -- PlayerRef whose library (Dwell on the Past's "target player").
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named ref) -> joinTwo (maybe Map.empty playerRefSlots named) (objectRefSlots ref)
  -- OfferCast's slot is a READ: it names the object being offered, bound by an
  -- earlier effect of the same list (CR 400.7).
  Effect.OfferCast (OfferCast.MkOfferCast slot _) -> oneSlot slot
  -- Both a READ: the ObjectRef names the object being permitted, normally bound
  -- by a MoveToZone earlier in the same list (CR 400.7) exactly as OfferCast's
  -- slot is, and the Duration's Condition may read a slot as a Quantity.
  Effect.GrantPlayFromExile grant -> joinTwo (durationSlots (GrantPlayFromExile.duration grant)) (objectRefSlots (GrantPlayFromExile.ref grant))
  -- Both the swept ref and everything the BODY reads, the shape
  -- PreventNextDamage's rider takes. The loop's own slot is NOT subtracted, as
  -- the rider's reserved amount slot is: this one is authored by the card and
  -- reported by boundSlots below, so the D4 lint pairs the body's read of it
  -- with this opcode's definition of it rather than seeing a dangling name.
  Effect.ForEach (ForEach.MkForEach ref _ body) ->
    joinTwo (objectRefSlots ref) (joinSlots (fmap slotsOf (Foldable.toList body)))

-- CR 611.2b: the only Duration carrying a Quantity is ForAsLongAs, through its
-- Condition.
durationSlots :: Duration.Duration -> Map.Map SlotName SlotArity
durationSlots duration = case duration of
  Duration.UntilEndOfTurn -> Map.empty
  Duration.Indefinite -> Map.empty
  Duration.UntilYourNextTurn -> Map.empty
  Duration.UntilEndOfYourNextTurn -> Map.empty
  Duration.ForAsLongAs condition -> conditionSlots condition
  Duration.UntilEndOfCombat -> Map.empty

-- Every slot a whole MODE reads: every clause's effects', plus every payer CR
-- 118.12a's "unless [a player] pays" names, plus every slot named by a TARGET
-- SLOT's own pool or filter. What the D4 dataflow lint asks, since a payer slot no
-- effect ALSO reads would otherwise dangle unnoticed. Mana Leak's Counter happens to read
-- the very slot its "unless" names, so the lint's answer is the same either way
-- for the one card in the pool; a card whose payer and target differ is what
-- this exists for.
--
-- The third source is Dwell on the Past's: its "target player" slot is read by
-- no effect at all, only by the other slot's GraveyardScope. That is a read
-- like any other -- CR 601.2c's announcement is what fills it, and the pool
-- would be empty without it -- so the lint counts it and the card declares no
-- unused slot.
modeSlots :: Mode.Mode Card.Type.Card -> Map.Map SlotName SlotArity
modeSlots mode =
  joinSlots
    [ joinSlots (fmap slotsOf (Foldable.toList (Mode.allEffects mode))),
      joinSlots (fmap payerSlot (Foldable.toList (Mode.clauses mode))),
      joinSlots (fmap (poolSlot . TargetSlot.pool) (Map.elems (Mode.targetSlots mode))),
      -- And every slot a target slot's own FILTER names -- CR 603.2's "target
      -- artifact or enchantment that player controls", where the player is read
      -- from the trigger's bindings rather than from an effect's operand.
      -- Without this the lint would never see that read, and a card naming "that
      -- player" under a condition that binds no player would quietly admit
      -- nothing.
      joinSlots (fmap filterSlots (Map.elems (Mode.targetSlots mode)))
    ]
  where
    filterSlots =
      maybe Map.empty (Map.fromSet (const SlotArity.One) . Filter.boundSlots)
        . TargetSlot.filter
    -- Every clause's payer, not just one: CR 118.12 scopes a resolution cost to
    -- the clause it is printed on, so a mode may state more than one and each
    -- names a slot the card owes a declaration for.
    payerSlot = maybe Map.empty (oneSlot . PayGate.payer) . Clause.payGate

-- The slot a target pool draws its candidates from, if it draws them from one
-- (CR 400.1's per-player graveyard). SlotArity.One: "their graveyard" is one
-- player's, and Target.graveyardRecipients folding several would still be one
-- slot read singly per player named.
poolSlot :: Pool.Pool -> Map.Map SlotName SlotArity
poolSlot pool = case pool of
  Pool.Creatures -> Map.empty
  Pool.Players -> Map.empty
  Pool.AnyTarget -> Map.empty
  Pool.Permanents -> Map.empty
  Pool.Spells -> Map.empty
  Pool.Abilities -> Map.empty
  Pool.SpellsAndPermanents -> Map.empty
  Pool.CardsInGraveyard scope -> case scope of
    GraveyardScope.Scoped _ -> Map.empty
    GraveyardScope.InSlot slot -> oneSlot slot
  Pool.CardsInExile -> Map.empty
  -- The graveyard half's scope, read exactly as CardsInGraveyard's is; the
  -- battlefield half names no slot.
  Pool.CreaturesAndCardsInGraveyard scope -> case scope of
    GraveyardScope.Scoped _ -> Map.empty
    GraveyardScope.InSlot slot -> oneSlot slot

-- Both sides of a comparison are a Quantity, and either may read a slot.
conditionSlots :: Condition.Type.Condition -> Map.Map SlotName SlotArity
conditionSlots condition = case condition of
  Condition.Type.Compares c ->
    joinTwo (quantitySlots (Compares.measured c)) (quantitySlots (Compares.threshold c))
  Condition.Type.Any conditions -> joinSlots (fmap conditionSlots conditions)
  Condition.Type.All conditions -> joinSlots (fmap conditionSlots conditions)

-- CR 603.3b: is slotsOf's answer for this effect the WHOLE of what APPLYING it
-- reads off the resolving object's bindings? A CLASSIFICATION of effects, in the
-- genre of Replacement.readsApplier: what SHAPE an effect has, never which
-- effect it is. Engine.orderInert is the sole caller, and may elide CR 603.3b's
-- ordering prompt only for an ability that reads nothing.
--
-- slotsOf answers the dataflow LINT's question -- which TARGET slots does this
-- effect name -- so a Set.empty there is not the same claim. Four ways the two
-- come apart, one per False or guard below:
--
--   * ArmDelayedTrigger captures the resolving object's WHOLE environment (CR
--     603.7c), which the ability it arms then reads.
--   * a Duration slotsOf's own arm does not union in (GainControl,
--     AffectPlayers) can still name a slot through CR 611.2b's condition.
--   * a PlayerRef nested in a Quantity is a TARGET slot Quantity.slots leaves to
--     this module, which cannot see it -- Quantity.slotsAreExhaustive is that half.
--   * CR 725.2's ControllerOfSource reads the reserved trigger-source slot, which
--     monarchTargetSlots must not report because a source is not a target.
--
-- One arm per constructor, no wildcard, and BecomeMonarch split per target: a
-- new opcode must answer HERE as well as in slotsOf. A wildcard defaulting to
-- True would hand a future nested payload an unsound elision instead of a build
-- failure. Fields are spelled out wherever hlint's record-pattern rule allows
-- it; the four `{}` arms below all answer a constant, so a new FIELD on one of
-- those is the one change this case will not force.
slotsAreExhaustive :: Effect Card.Type.Card -> Bool
slotsAreExhaustive effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) ->
    durationSlotsAreExhaustive duration
      && all Quantity.slotsAreExhaustive (Projection.quantitiesOf modification)
  Effect.ChangeText {} -> True
  Effect.AddMana _ -> True
  Effect.Search (Search.MkSearch _ _ quantity _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.ExileAllGraveyards -> True
  Effect.Proliferate -> True
  Effect.Bolster quantity -> Quantity.slotsAreExhaustive quantity
  Effect.Amass (Amass.Type.MkAmass quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.TemptWithTheRing -> True
  Effect.Venture -> True
  Effect.ExileHandThenDraw -> True
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RestartGame _ -> True
  Effect.ControlPlayerNextTurn _ -> True
  Effect.Destroy {} -> True
  Effect.Sacrifice _ -> True
  Effect.TurnFaceDown _ -> True
  Effect.RemoveFromCombat _ -> True
  Effect.BecomesBlocked _ -> True
  Effect.MoveToZone {} -> True
  Effect.Draw (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Mill (Mill.MkMill _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.Reveal {} -> True
  Effect.LookAt {} -> True
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Explore {} -> True
  Effect.Discard (Discard.MkDiscard _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.ExchangeLifeTotals _ -> True
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RedistributeLifeTotals -> True
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.DecreaseSpeed d -> Quantity.slotsAreExhaustive (SpeedDecrease.quantity d)
  -- The embedded card is literal text, not a read: CR 111.1's token is minted
  -- with its own empty bindings, so nothing in it sees this environment.
  Effect.Create (Create.MkCreate quantity _ _ _) -> Quantity.slotsAreExhaustive quantity
  -- The ObjectRef is reported by slotsOf, so only the count can hide a slot.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _) -> Quantity.slotsAreExhaustive quantity
  -- The ReplacementEffect holds no Quantity and no reference, so slotsOf's two
  -- unions are the whole of it once their quantities check out.
  Effect.Replace (Replace.MkReplace duration _ _ condition _) ->
    durationSlotsAreExhaustive duration && all conditionSlotsAreExhaustive condition
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> True
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ quantity rider) ->
    durationSlotsAreExhaustive duration && Quantity.slotsAreExhaustive quantity && all slotsAreExhaustive rider
  Effect.PreventAllDamage (DurationRef.MkDurationRef duration _) -> durationSlotsAreExhaustive duration
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ _ _) -> durationSlotsAreExhaustive duration
  Effect.Counter _ -> True
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Tap _ -> True
  Effect.Untap _ -> True
  Effect.Detain _ -> True
  Effect.DoesNotUntapNext _ -> True
  Effect.Transform _ -> True
  Effect.AddPhases _ -> True
  -- slotsOf's arm drops this Duration, so the slotless test is made here.
  Effect.GainControl (DurationRef.MkDurationRef duration _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CR 603.7c: the armed ability inherits this object's whole environment, so
  -- what it reads is not stated here at all.
  Effect.ArmDelayedTrigger {} -> False
  -- GainControl's reason for the Duration; the AffectedPlayers is reported by
  -- slotsOf above and the PlayerEffect beside it holds no reference of any sort.
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- slotsOf's arm reports both refs but drops the Duration, so the slotless
  -- test is made here -- AffectPlayers' reason.
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CR 114.2's emblem is minted with EMPTY bindings (Event.createEmblem), so its
  -- embedded card is literal text for Create's reason.
  Effect.CreateEmblem _ -> True
  Effect.BecomeMonarch MonarchTarget.TheController -> True
  -- The one arm that answers NO here: CR 725.2 reads Binding.triggerSource,
  -- which monarchTargetSlots reports as nothing.
  Effect.BecomeMonarch MonarchTarget.ControllerOfSource -> False
  Effect.BecomeMonarch (MonarchTarget.InSlot _) -> True
  Effect.Designate (Designate.MkDesignate _ _) -> True
  Effect.Unsuspect _ -> True
  Effect.Evolve _ -> True
  Effect.Mentor _ -> True
  Effect.Train _ -> True
  Effect.ItBecomes _ -> True
  Effect.ExileUntilMonarch _ -> True
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting _ _) -> True
  Effect.Attach _ -> True
  Effect.AttachTarget (AttachTarget.MkAttachTarget _ _) -> True
  -- CR 729.1b: the slot is a DEFINITION, and the subgame itself reads no binding
  -- of the game that spawned it.
  Effect.PlaySubgame _ -> True
  -- PlaySubgame's answer: a definition reads no slot, so slotsOf's empty map is
  -- exhaustive.
  Effect.ChooseOpponent _ -> True
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn _ _) -> True
  Effect.ShuffleIntoLibrary {} -> True
  Effect.OfferCast {} -> True
  Effect.GrantPlayFromExile grant -> durationSlotsAreExhaustive (GrantPlayFromExile.duration grant)
  -- PreventNextDamage's answer: the ref is reported by slotsOf, so only the
  -- body can hide a read, and each of its effects answers for itself.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> all slotsAreExhaustive body

-- CR 611.2b: only ForAsLongAs reads anything, through its Condition.
durationSlotsAreExhaustive :: Duration.Duration -> Bool
durationSlotsAreExhaustive duration = case duration of
  Duration.UntilEndOfTurn -> True
  Duration.Indefinite -> True
  Duration.UntilYourNextTurn -> True
  Duration.UntilEndOfYourNextTurn -> True
  Duration.ForAsLongAs condition -> conditionSlotsAreExhaustive condition
  Duration.UntilEndOfCombat -> True

-- conditionSlots' mirror: both sides are a Quantity.
conditionSlotsAreExhaustive :: Condition.Type.Condition -> Bool
conditionSlotsAreExhaustive condition = case condition of
  Condition.Type.Compares c ->
    Quantity.slotsAreExhaustive (Compares.measured c) && Quantity.slotsAreExhaustive (Compares.threshold c)
  Condition.Type.Any conditions -> all conditionSlotsAreExhaustive conditions
  Condition.Type.All conditions -> all conditionSlotsAreExhaustive conditions

-- Does any of these effects read X? A card that reads X must declare it in its
-- cost -- in the mana cost as an {X}, or in an additional cost as Hatred's "pay X
-- life" (CR 107.3, CR 107.3a, CR 118.4) -- the same reads-equal-declares contract
-- slotsOf draws for target slots. Quantity.readsX does the looking, so a nested
-- X -- Vitalizing Cascade's "X plus 3", or an X inside a Count -- is seen here
-- exactly as slotsOf sees a nested slot through Quantity.slots.
--
-- NOTE: when an opcode gains a Quantity FIELD, add its arm here by hand. A new
-- OPCODE the compiler does force, this case being exhaustive over constructors;
-- widening an existing one it does not, since an arm already written `{} ->
-- False` keeps compiling and keeps answering False about a quantity it now
-- carries.
readsX :: [Effect Card.Type.Card] -> Bool
readsX = any effectReadsX
  where
    effectReadsX effect = case effect of
      Effect.DealDamage (DealDamage.MkDealDamage _ quantity _) -> Quantity.readsX quantity
      -- Untamed Might's "+X/+X" is an X the effect itself does not carry: it sits
      -- inside the Modification, reached through Projection.quantitiesOf.
      Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ modification _) -> any Quantity.readsX (Projection.quantitiesOf modification)
      Effect.ChangeText {} -> False
      Effect.AddMana _ -> False
      Effect.Search (Search.MkSearch _ _ quantity _ _) -> Quantity.readsX quantity
      Effect.ExileAllGraveyards -> False
      Effect.Proliferate -> False
      Effect.Bolster quantity -> Quantity.readsX quantity
      Effect.Amass (Amass.Type.MkAmass quantity _) -> Quantity.readsX quantity
      Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.TemptWithTheRing -> False
      Effect.Venture -> False
      Effect.ExileHandThenDraw -> False
      Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> Quantity.readsX quantity
      Effect.RestartGame _ -> False
      Effect.ControlPlayerNextTurn _ -> False
      Effect.Destroy {} -> False
      Effect.Sacrifice _ -> False
      Effect.TurnFaceDown _ -> False
      Effect.RemoveFromCombat _ -> False
      Effect.BecomesBlocked _ -> False
      Effect.MoveToZone {} -> False
      Effect.Draw (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Mill (Mill.MkMill _ quantity _) -> Quantity.readsX quantity
      Effect.Reveal {} -> False
      Effect.LookAt {} -> False
      Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Explore {} -> False
      Effect.Discard (Discard.MkDiscard _ quantity) -> Quantity.readsX quantity
      Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.ExchangeLifeTotals _ -> False
      Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.RedistributeLifeTotals -> False
      Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.DecreaseSpeed d -> Quantity.readsX (SpeedDecrease.quantity d)
      Effect.Create (Create.MkCreate quantity _ _ _) -> Quantity.readsX quantity
      Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _) -> Quantity.readsX quantity
      Effect.Replace {} -> False
      Effect.SkipNextPhase {} -> False
      -- CR 601.2b's X reaches the rider too, an X-cost shield's rider being
      -- free to name the same X. No card in the pool does, but this half of the
      -- contract is a lint the rider must not slip past.
      Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ quantity rider) -> Quantity.readsX quantity || readsX (Foldable.toList rider)
      Effect.PreventAllDamage {} -> False
      Effect.RedirectDamage {} -> False
      Effect.Counter _ -> False
      Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.readsX quantity
      Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.readsX quantity
      Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      Effect.Tap _ -> False
      Effect.Untap _ -> False
      Effect.Detain _ -> False
      Effect.DoesNotUntapNext _ -> False
      Effect.Transform _ -> False
      Effect.AddPhases _ -> False
      Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
      Effect.ArmDelayedTrigger {} -> False
      Effect.AffectPlayers {} -> False
      Effect.RequireBlock {} -> False
      Effect.CreateEmblem {} -> False
      Effect.BecomeMonarch {} -> False
      Effect.Designate (Designate.MkDesignate _ _) -> False
      Effect.Unsuspect _ -> False
      Effect.Evolve _ -> False
      Effect.Mentor _ -> False
      Effect.Train _ -> False
      Effect.ItBecomes _ -> False
      Effect.ExileUntilMonarch _ -> False
      Effect.ExileHaunting {} -> False
      Effect.Attach _ -> False
      Effect.AttachTarget {} -> False
      Effect.PlaySubgame _ -> False
      Effect.ChooseOpponent _ -> False
      Effect.TakeExtraTurn {} -> False
      Effect.ShuffleIntoLibrary {} -> False
      Effect.OfferCast {} -> False
      Effect.GrantPlayFromExile {} -> False
      -- CR 608.2f's body is an effect list like any other, so an X inside it is
      -- an X this card reads -- PreventNextDamage's rider, one opcode over.
      Effect.ForEach (ForEach.MkForEach _ _ body) -> readsX (Foldable.toList body)

-- CR 601.3 (Panglacial): does this effect search a library? The classification
-- Stack asks before resolving, to offer the cast-while-searching opportunity.
-- Search reads the library of whoever its SECOND PlayerRef names, whatever its
-- count; every other effect reads none.
searchesLibrary :: Effect Card.Type.Card -> Bool
searchesLibrary effect = case effect of
  Effect.Search {} -> True
  Effect.Proliferate -> False
  Effect.Bolster _ -> False
  Effect.Amass _ -> False
  Effect.Blight _ -> False
  Effect.TemptWithTheRing -> False
  Effect.Venture -> False
  Effect.PlayerSacrifices {} -> False
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText {} -> False
  Effect.AddMana _ -> False
  Effect.ExileAllGraveyards -> False
  Effect.ExileHandThenDraw -> False
  Effect.RestartGame _ -> False
  Effect.ControlPlayerNextTurn _ -> False
  Effect.Destroy {} -> False
  Effect.Sacrifice _ -> False
  Effect.TurnFaceDown _ -> False
  Effect.RemoveFromCombat _ -> False
  Effect.BecomesBlocked _ -> False
  Effect.MoveToZone {} -> False
  Effect.Draw {} -> False
  Effect.Mill {} -> False
  -- CR 701.20a's reveal, CR 701.20e's look and the three look-and-split actions
  -- each show NAMED cards, where CR 701.23a's search looks through ALL of a
  -- zone; Panglacial Wurm's window is the latter's alone.
  Effect.Reveal {} -> False
  Effect.LookAt {} -> False
  Effect.Scry {} -> False
  Effect.Surveil {} -> False
  Effect.Fateseal {} -> False
  -- CR 701.44a reveals the top card, where CR 701.23a's search looks through a
  -- whole zone; Panglacial Wurm's window is the latter's alone.
  Effect.Explore {} -> False
  Effect.Discard {} -> False
  Effect.LoseLife {} -> False
  Effect.GainLife {} -> False
  Effect.ExchangeLifeTotals _ -> False
  Effect.SetLifeTotal {} -> False
  Effect.RedistributeLifeTotals -> False
  Effect.IncreaseSpeed {} -> False
  Effect.DecreaseSpeed {} -> False
  Effect.Create {} -> False
  Effect.CreateCopy {} -> False
  Effect.Replace {} -> False
  Effect.SkipNextPhase {} -> False
  -- Not descended into for CR 615.5's rider, because this classification is
  -- asked of the RESOLUTION that is about to run: the rider runs later, from
  -- Resolve.runPreventionRiders and outside any resolution, so a Search there
  -- would need its window opened where it runs rather than here. No rider in
  -- the pool searches.
  Effect.PreventNextDamage {} -> False
  Effect.PreventAllDamage {} -> False
  Effect.RedirectDamage {} -> False
  Effect.Counter _ -> False
  Effect.PutCounters {} -> False
  Effect.RemoveCounters {} -> False
  Effect.GainPlayerCounters {} -> False
  Effect.RemovePlayerCounters {} -> False
  Effect.Tap _ -> False
  Effect.Untap _ -> False
  Effect.Detain _ -> False
  Effect.DoesNotUntapNext _ -> False
  Effect.Transform _ -> False
  Effect.AddPhases _ -> False
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
  Effect.ArmDelayedTrigger {} -> False
  Effect.AffectPlayers {} -> False
  Effect.RequireBlock {} -> False
  Effect.CreateEmblem {} -> False
  Effect.BecomeMonarch {} -> False
  Effect.Designate (Designate.MkDesignate _ _) -> False
  Effect.Unsuspect _ -> False
  Effect.Evolve _ -> False
  Effect.Mentor _ -> False
  Effect.Train _ -> False
  Effect.ItBecomes _ -> False
  Effect.ExileUntilMonarch _ -> False
  Effect.ExileHaunting {} -> False
  Effect.Attach _ -> False
  Effect.AttachTarget {} -> False
  Effect.PlaySubgame _ -> False
  Effect.ChooseOpponent _ -> False
  -- CR 701.24 shuffles a library but never LOOKS at one, which is what CR 701.23a
  -- makes a search, so this opens no window.
  Effect.ShuffleIntoLibrary {} -> False
  -- CR 608.2g's other producer, and not a search: the offered cast names one
  -- object the effect already has, so no library is looked at and the
  -- Panglacial window does not open on top of it.
  Effect.OfferCast {} -> False
  Effect.GrantPlayFromExile {} -> False
  Effect.TakeExtraTurn {} -> False
  -- Descended into, unlike PreventNextDamage's rider: this body runs INSIDE the
  -- resolution being asked about (CR 608.2f considers each member as the
  -- instruction is followed), so a Search in it wants CR 601.3's window opened
  -- here. No body in the pool searches.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> any searchesLibrary body

-- CR 603.7: the delayed abilities an effect list ARMS, by name. The read half of
-- the AbilityName dataflow lint, exactly as slotsOf is for target slots.
armedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- CR 603.7: the delayed abilities an effect list arms with an ONSET -- the ones
-- whose firing is gated past the turn that armed them. armedAbilities' sibling,
-- narrowed to the arms that are not Onset.Immediately.
--
-- Its customer is the card lint, which joins these names against the card's own
-- delayedAbilities and asks Event.controllerTurnScoped of each condition.
onsetGatedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
onsetGatedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger _ Onset.Immediately _) -> Nothing
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- boundSlots over a whole effect list: the write half of the dataflow lint,
-- against which Quantity.slots and slotsOf are the read half.
definedSlots :: [Effect Card.Type.Card] -> Set SlotName
definedSlots = foldMap boundSlots

-- slotsOf's mirror for ONE effect: the slots it BINDS rather than reads -- a
-- Create that names what it mints, one token or every one of them, a MoveToZone
-- that names the incarnation CR 400.7 minted at the destination, a PlaySubgame
-- that names its winner, a Destroy that names how many it destroyed. Every
-- position in the DSL at which a card AUTHORS a name the engine will later
-- write a binding to, which is what makes it the set Pawl.CardSpec's
-- reserved-name sweep ranges over.
--
-- EXHAUSTIVE, and deliberately so, where this was a four-arm case under a
-- wildcard. The wildcard filed every opcode it had not been told about under
-- "binds nothing", which is silent in both directions: a new bind position
-- would drop out of the dataflow lint's write half AND out of that sweep, the
-- second being how a card could name CR 615.13's `thatMuch` in a Destroy and
-- shadow the amount the event supplied. Spelling every arm makes adding an
-- opcode a compile error until an author decides which half it belongs to.
--
-- An arm that both READS and BINDS appears in both functions with different
-- fields -- MoveToZone reads what it moves and binds what arrived. slotsOf's
-- comments at those arms say which field is which.
boundSlots :: Effect Card.Type.Card -> Set SlotName
boundSlots effect = case effect of
  -- CR 400.7: the incarnation minted at the destination, named so a later
  -- effect can reach the object the move created rather than the one it ended.
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ _ mSlot _ _) -> foldMap Set.singleton mSlot
  -- The token or tokens this Create minted, named so a later effect can refer
  -- back to them -- CR 603.7c's delayed trigger "that refers to a particular
  -- object" is the case that needs the name to survive the resolution.
  Effect.Create (Create.MkCreate _ _ _ mSlot) -> foldMap Set.singleton mSlot
  -- Binds nothing: no card in the pool names the token copy it minted.
  Effect.CreateCopy {} -> Set.empty
  -- CR 729.1b: the subgame's winner, reported by the nested game rather than chosen.
  Effect.PlaySubgame slot -> Set.singleton slot
  -- CR 608.2d: the opponent this effect chose, so a later effect naming the slot
  -- passes the dataflow lint -- PlaySubgame's winner, chosen rather than reported.
  Effect.ChooseOpponent slot -> Set.singleton slot
  -- How many permanents this destruction ACTUALLY destroyed, for a later "for
  -- each ... destroyed this way" to read as a Quantity.
  Effect.Destroy (Destroy.MkDestroy _ _ mSlot) -> foldMap Set.singleton mSlot
  -- How many of the cards this mill put in the graveyard matched the tally's
  -- filter, for CR 728.1's "for each nonland card milled this way" to read.
  Effect.Mill (Mill.MkMill _ _ mTally) -> foldMap (Set.singleton . MillTally.slot) mTally
  -- CR 701.20a's reveal binds nothing: it is public, so what it leaves behind
  -- is the GameEvent.Revealed in the log rather than a name for a later clause.
  Effect.Reveal {} -> Set.empty
  -- The cards CR 701.20e's look showed, for a later clause of the same
  -- resolution to name -- Into the Wilds' "if it's a land card ... put it onto
  -- the battlefield", which reads the slot twice over (Filter.IsBound, then
  -- ObjectRef.InSlot).
  Effect.LookAt (LookAt.MkLookAt _ slot) -> Set.singleton slot
  Effect.Scry {} -> Set.empty
  Effect.Surveil {} -> Set.empty
  Effect.Fateseal {} -> Set.empty
  Effect.Explore {} -> Set.empty
  Effect.DealDamage {} -> Set.empty
  Effect.ModifyTarget {} -> Set.empty
  Effect.ChangeText {} -> Set.empty
  Effect.AddMana _ -> Set.empty
  Effect.Search {} -> Set.empty
  Effect.ExileAllGraveyards -> Set.empty
  Effect.Proliferate -> Set.empty
  Effect.Bolster _ -> Set.empty
  Effect.Amass _ -> Set.empty
  Effect.Blight _ -> Set.empty
  Effect.TemptWithTheRing -> Set.empty
  Effect.Venture -> Set.empty
  Effect.ExileHandThenDraw -> Set.empty
  Effect.PlayerSacrifices {} -> Set.empty
  Effect.RestartGame _ -> Set.empty
  Effect.ControlPlayerNextTurn _ -> Set.empty
  Effect.Sacrifice _ -> Set.empty
  Effect.TurnFaceDown _ -> Set.empty
  Effect.RemoveFromCombat _ -> Set.empty
  Effect.BecomesBlocked _ -> Set.empty
  Effect.Draw {} -> Set.empty
  Effect.Discard {} -> Set.empty
  Effect.LoseLife {} -> Set.empty
  Effect.GainLife {} -> Set.empty
  Effect.ExchangeLifeTotals _ -> Set.empty
  Effect.SetLifeTotal {} -> Set.empty
  Effect.RedistributeLifeTotals -> Set.empty
  Effect.IncreaseSpeed {} -> Set.empty
  Effect.DecreaseSpeed {} -> Set.empty
  Effect.Replace {} -> Set.empty
  Effect.SkipNextPhase {} -> Set.empty
  -- The shield itself binds nothing; CR 615.5's rider is an effect list like any
  -- other, so a name IT authors is a name this card authors -- which is what
  -- keeps Pawl.CardSpec's reserved-name sweep over the rider.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ rider) -> foldMap boundSlots rider
  Effect.PreventAllDamage {} -> Set.empty
  Effect.RedirectDamage {} -> Set.empty
  Effect.Counter _ -> Set.empty
  Effect.PutCounters {} -> Set.empty
  Effect.RemoveCounters {} -> Set.empty
  Effect.GainPlayerCounters {} -> Set.empty
  Effect.RemovePlayerCounters {} -> Set.empty
  Effect.Tap _ -> Set.empty
  Effect.Untap _ -> Set.empty
  Effect.Detain _ -> Set.empty
  Effect.DoesNotUntapNext _ -> Set.empty
  Effect.Transform _ -> Set.empty
  Effect.AddPhases _ -> Set.empty
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> Set.empty
  Effect.ArmDelayedTrigger {} -> Set.empty
  Effect.AffectPlayers {} -> Set.empty
  Effect.RequireBlock {} -> Set.empty
  Effect.CreateEmblem {} -> Set.empty
  Effect.BecomeMonarch {} -> Set.empty
  Effect.Designate (Designate.MkDesignate _ _) -> Set.empty
  Effect.Unsuspect _ -> Set.empty
  Effect.Evolve _ -> Set.empty
  Effect.Mentor _ -> Set.empty
  Effect.Train _ -> Set.empty
  Effect.ItBecomes _ -> Set.empty
  Effect.ExileUntilMonarch _ -> Set.empty
  Effect.ExileHaunting {} -> Set.empty
  Effect.Attach _ -> Set.empty
  Effect.AttachTarget {} -> Set.empty
  Effect.TakeExtraTurn {} -> Set.empty
  Effect.ShuffleIntoLibrary {} -> Set.empty
  Effect.OfferCast {} -> Set.empty
  -- Reads a slot, binds none: the object it permits already exists and already
  -- has a name.
  Effect.GrantPlayFromExile {} -> Set.empty
  -- The loop's member slot, plus every name the BODY authors -- the body is an
  -- effect list like any other, which is what keeps Pawl.CardSpec's
  -- reserved-name sweep over it (PreventNextDamage's rider, for its reason).
  Effect.ForEach (ForEach.MkForEach _ slot body) -> Set.insert slot (foldMap boundSlots body)

-- Does a Create's slot name EVERY token it minted ("those tokens") rather than
-- one particular one (CR 603.7c's "it")? Which of the two a card means is its own
-- text, and CR 111 gives the opcode no way to carry the word; the PRINTED quantity
-- is the only thing left that can tell them apart, and the count at RESOLUTION
-- cannot -- CR 614.16 lets a replacement multiply either one.
--
-- Literal 1 is "it": the card printed one token, so a doubled create leaves
-- several candidates for a singular word and the Create arm asks which. Anything
-- else is "them", on the reading that a card printing several tokens and
-- referring back to them has nothing on it to pick ONE by -- Thatcher Revolt (in
-- the pool), Chandra Acolyte of Flame and Orthion (not) all say it in the plural,
-- and Orthion carries both forms with the singular on its ONE-token ability.
--
-- An inference from the quantity, and unlike the rejection it replaces it cannot
-- be linted: nothing in card data records the word, so a future card printing
-- several while naming one would bind them all with no diagnostic. No such card
-- is known.
namesEveryToken :: Quantity.Type.Quantity -> Bool
namesEveryToken quantity = quantity /= Quantity.Type.Literal 1

-- CR 111.3: the values the creating spell or ability defines "become the token's
-- text", and they are functionally equivalent to values PRINTED on a card. So a
-- power or toughness a card wrote as a computed quantity -- Rootha, Mastering the
-- Moment's "an X/X ... where X is the greatest mana value among instant and
-- sorcery spells you've cast this turn" -- is settled here, as the effect
-- resolves, and stamped as a literal. Left as a quantity it would be a rule the
-- token carries rather than a number the effect defined, re-read on every
-- projection: GameState.events is cleared at the turn handoff, so that token
-- would have no power or toughness at all on the next turn, and a creature with
-- none is destroyed as a state-based action.
--
-- An UNDETERMINABLE quantity is left exactly as it stands, which is what keeps CR
-- 208.2's star working: a face with a characteristic-defining P/T prints Star in
-- its power box, evaluate answers Nothing for it, and
-- Projection.seedCharacteristicPT still finds the star where it expects it.
--
-- Power and toughness are the whole of it because they are the only printed
-- characteristics a Face holds as a Quantity -- loyalty and defense are plain
-- numbers.
--
-- Only the faces of the token being created, never the cards embedded in its own
-- effects: a Create the token itself carries is a different effect and settles
-- its own quantities when it resolves.
bakeTokenCharacteristics :: (Quantity.Type.Quantity -> Maybe Integer) -> Card.Type.Card -> Card.Type.Card
bakeTokenCharacteristics eval card = card {Card.Type.faces = fmap bakeFace (Card.Type.faces card)}
  where
    bake quantity = Maybe.maybe quantity Quantity.Type.Literal (eval quantity)
    bakeFace face =
      face
        { Face.power = fmap (Power.MkPower . bake . Power.unwrap) (Face.power face),
          Face.toughness = fmap (Toughness.MkToughness . bake . Toughness.unwrap) (Face.toughness face)
        }

-- A resolving spell's PROJECTED modes: only its chosen ones (CR 608.2c/700.2),
-- with every text change affecting it applied to each effect (CR 612), so the
-- resolver honors a spell hacked on the stack. A non-modal card has one mode,
-- always chosen.
--
-- Modes rather than a flat effect list because CR 603.5's "may" is a property of
-- a clause within a mode, not of the effect list. A text change rewrites the
-- printed words -- a clause's effects and its CR 701.46a gate -- so optionality
-- passes through untouched.
--
-- Not implemented: a SPELL's mode target slots are left unrewritten, so CR
-- 608.2b's re-validation in targetsAllIllegal below measures the printed clause
-- (#635). An ACTIVATED ability has no such gap -- Projection.rewriteModal rewrites
-- its slots and CR 602.2a's stack object carries them.
modesOf :: ObjectId -> GameState -> [(ModeInstance, Mode.Mode Card.Type.Card)]
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
                -- Projection.rewriteModal's reason: a clause gate's Filters are
                -- printed words CR 612.1 changes.
                Clause.condition = fmap (Projection.rewriteCondition changes) (Clause.condition c)
              }
          rewriteMode m = m {Mode.clauses = fmap rewriteClause (Mode.clauses m)}
       in fmap (fmap rewriteMode) (Card.chosenModes chosen face)

-- CR 405.4: who controls a SPELL on the stack -- both CR 608.2b's legality
-- perspective and the effects' own execution. One function because those two
-- must name the same player, not because they disagree today.
--
-- The player who CAST it, fixed by CR 601.2a's move and stamped on the object
-- (Object.enteredUnder), never re-derived from the board -- which is what
-- Projection.defaultControllerOf reads under the fold below. Casting a card
-- somebody else owns is what makes the distinction visible, and Dire Fleet
-- Daredevil is the pool's producer.
--
-- Still read THROUGH the projection rather than off the object, and that is the
-- rules' own shape rather than a leftover: CR 613.1b's layer 2 overrides a
-- default controller wherever an object has one (CR 109.4), and a continuous
-- effect that changed a spell's control would end by that fold stopping to say
-- so -- not by anything rewriting the stamp. Nothing in the pool installs a
-- SetController naming a stack object, so today the fold is the identity on this
-- read (#83).
spellController :: Object.Object -> ObjectId -> GameState -> PlayerId
spellController obj oid gs = Maybe.fromMaybe (Projection.defaultControllerOf obj) (Projection.controllerOf oid gs)

-- CR 608.2b: are ALL of this spell's targets illegal? A spell with no target slot
-- never fizzles, and one with several survives if any one is still legal. Reserved
-- slots are not targets and are vacuously legal.
--
-- Shared by the ordinary spell path and the Aura path in Pawl.Engine.Stack, which
-- is the point of it being a function: an Aura spell is the first permanent spell
-- that can be countered on resolution, and a second copy would drift.
targetsAllIllegal :: ObjectId -> GameState -> Bool
targetsAllIllegal oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Game.faceOf oid gs of
    Nothing -> False
    Just face ->
      let slots = Card.modesTargetSlots (Binding.modesOf (Object.bindings obj)) face
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            Nothing -> recipients
            -- CR 608.2b's perspective is the SPELL's controller (CR 405.4), read
            -- through the same function resolveSpellWith uses for execution.
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) chosen oid recipient targetSlot gs) recipients
          legal = Map.mapWithKey legalSlot chosen
          targeted = Map.restrictKeys legal (Map.keysSet slots)
       in -- Measured on the TARGETS actually chosen, not on the slots declared: CR
          -- 115.6 makes a spell that chose zero targets untargeted, so CR 608.2b's
          -- "all its targets, for every instance of the word 'target,' are now
          -- illegal" has nothing to be true of -- and one surviving target of one
          -- slot keeps the whole spell resolving. For a card whose every slot takes
          -- exactly one target the two readings agree, every declared slot having
          -- been filled at CR 601.2c.
          not (Map.null targeted) && all Set.null (Map.elems targeted)

-- CR 608.2b then CR 608.2: re-validate every filled slot against what it
-- declares; if the spell has slots and ALL are now illegal it fizzles, moving to
-- the graveyard with no effect applied. Otherwise the effects run in order (CR
-- 608.2c), each skipping a slot whose target is illegal, and the spell goes to
-- its owner's graveyard as the final part of resolution (CR 608.2n).
--
-- Extended for CR 729.1b: the resolving object's bindings are re-read before EACH
-- effect, so a slot DEFINED mid-resolution is visible to a later one. Target-slot
-- legality stays fixed at the start of resolution; only newly-defined reserved
-- slots appear, and those are vacuously legal. `runSubgame` is the injected
-- nested-game runner.
resolveSpellWith :: Game Result -> ObjectId -> Game ()
resolveSpellWith runSubgame oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Game.faceOf oid gs of
      Nothing -> pure ()
      Just face ->
        -- CR 608.2b/700.2c: re-validate only the CHOSEN modes' slots -- an
        -- unchosen mode's slot was never filled and is not part of this
        -- resolution's legality question.
        let chosenSelection = Binding.modesOf (Object.bindings obj)
            slots = Card.modesTargetSlots chosenSelection face
            -- The slots as FILLED, which a slot-scoped pool is re-derived
            -- against (Target.stillLegal).
            chosen = Binding.targetsOf (Object.bindings obj)
            -- CR 700.2d: the slots the MODES own, which is `slots` minus CR
            -- 303.4a's enchant slot -- the one the card itself declares, and so
            -- the one every chosen instance can still read.
            modeOwnedSlots = Modal.modesTargetSlots chosenSelection (Face.spell face)
            legalSlot slot recipients = case Map.lookup slot slots of
              -- CR 608.2b is about TARGETS. A slot that declares no target is
              -- a RESERVED binding -- a trigger's source, a token this resolution
              -- minted -- and was never targeted.
              Nothing -> recipients
              -- Per RECIPIENT and not per slot: CR 608.2b's "illegal targets, if
              -- any, won't be affected" leaves the slot's surviving targets to be
              -- affected as usual.
              Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) chosen oid recipient targetSlot gs) recipients
         in if targetsAllIllegal oid gs
              then Event.changeZone oid Zone.Graveyard
              else do
                let effectController = spellController obj oid gs
                Monad.forM_ (modesOf oid gs) $ \(mi, mode) -> do
                  let idx = ModeInstance.index mi
                      applyOne eff = do
                        -- Re-read the live bindings for THIS effect: a prior
                        -- PlaySubgame may have bound its winner slot.
                        bindingsNow <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject oid)
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
                  -- CR 608.2e's clause is the unit all three gates cover, so they
                  -- are asked once per clause rather than once per mode --
                  -- between the preceding clause's instructions and this one's.
                  Monad.forM_ (zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode))) $ \(cIdx, clause) -> do
                    -- CR 701.46a's printed "if" first: it precedes the
                    -- instructions in written order (CR 608.2c), and a clause
                    -- that cannot happen is no question to ask.
                    -- The LIVE bindings, not the start-of-resolution ones, for the
                    -- reason the gate is asked here at all (CR 608.2c): a slot an
                    -- earlier clause of this resolution DEFINED is part of the
                    -- state this clause is read against -- Into the Wilds' "if
                    -- it's a land card" over the card its first clause looked at.
                    -- Same re-read `applyOne` above makes, and it adds only
                    -- defined slots: CR 608.2b's re-validation is still the one
                    -- made once, as the resolution began.
                    --
                    -- A REGRESSION FENCE on this path rather than a proved
                    -- behaviour: every card in the pool whose gate reads a
                    -- mid-resolution slot is a triggered ability, so mutating
                    -- this half back to the start-of-resolution map leaves the
                    -- suite green. It is here because the two paths must read
                    -- CR 608.2c the same way.
                    gateBindings <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject oid)
                    gated <- gateHolds effectController oid (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Binding.targetsOf gateBindings)) clause
                    -- CR 603.5 / 608.2d: then the printed "may".
                    taken <- if gated then exercises oid effectController idx cIdx clause else pure False
                    -- CR 118.12: and then this clause's cost paid on resolution,
                    -- offered only for a clause that is happening at all.
                    -- The legal targets are the START-of-resolution ones,
                    -- matching CR 608.2b's single re-validation; the
                    -- per-effect re-read above adds only slots this resolution
                    -- DEFINES, and the cost is offered before any of THIS
                    -- clause's effects have run. A later clause's gate is asked
                    -- after an earlier clause's effects, which no card reaches:
                    -- the pool's payGate cards are all one-clause.
                    --
                    -- Both maps are projected into THIS instance's view (CR 700.2d):
                    -- the legality is decided against the instance-named slot, then
                    -- renamed to the printed one the mode's own text reads. Deciding
                    -- it after the rename would look every slot up in `slots` and
                    -- miss, and CR 608.2b's re-validation would silently pass.
                    admitted <-
                      if taken
                        then
                          let chosenAtStart = Binding.targetsOf (Object.bindings obj)
                           in payGateAdmits
                                oid
                                oid
                                idx
                                cIdx
                                (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Map.mapWithKey legalSlot chosenAtStart))
                                clause
                        else pure False
                    Monad.when admitted (Monad.mapM_ applyOne (Clause.effects clause))
                finishSpell oid face effectController

-- CR 608.2n / 715.3d: where the spell goes as the last part of its resolution.
-- Its owner's graveyard, unless it was cast as an Adventure -- then its
-- controller exiles it instead, and CR 715.3d's permission to play it goes onto
-- the exiled card.
--
-- Reached only from the RESOLVING path above. The fizzle a few lines up keeps
-- the graveyard, and must: a spell whose targets have all become illegal does
-- not resolve at all (CR 608.2b), so CR 715.3d's "as it resolves" never applies
-- to it. The mechanic's own ruling says so outright -- an Adventure spell that
-- leaves the stack any other way, "most likely by being countered or by failing
-- to resolve because its targets have all become illegal", is not exiled.
--
-- Written onto the id the move RETURNS and never onto `oid`: CR 400.7 mints a
-- fresh incarnation in exile and deletes the one that was on the stack, so the
-- permission belongs to the new object. Nothing comes back when the move was
-- cancelled, and then there is no exiled card to permit anything about.
finishSpell :: ObjectId -> Face.Face Card.Type.Card -> PlayerId -> Game ()
finishSpell oid face controller =
  if not (Card.isAdventure face)
    then Event.changeZone oid Zone.Graveyard
    else do
      exiled <- Event.changeZoneReturning oid Zone.Exile
      Monad.forM_ exiled $ \newId ->
        State.modify' $ \gs ->
          gs
            { GameState.objects =
                Map.adjust (\o -> o {Object.playableFromExile = Just (permission newId)}) newId (GameState.objects gs)
            }
  where
    -- Expiry.Type.Never, and that is CR 611.2a read literally: CR 715.3d states
    -- no duration, so the permission "lasts until the end of the game" and no
    -- sweep ever ends it. What ends it is CR 400.7 -- the card leaving exile
    -- mints a new incarnation, and newIncarnation clears the field.
    --
    -- The exiled card is its own `source`, which costs nothing and is honest:
    -- Never never consults it, since only Expiry.Type.While evaluates a
    -- Condition against a source.
    permission newId =
      ExilePlayPermission.MkExilePlayPermission
        { ExilePlayPermission.player = controller,
          ExilePlayPermission.source = newId,
          ExilePlayPermission.expiry = Expiry.Type.Never,
          -- CR 715.3d grants permission and says nothing about mana, so the
          -- Adventure is paid for in the colours it prints.
          ExilePlayPermission.spending = ManaSpending.AsProduced
        }

-- The no-subgame spell resolver (Stack's default path and every direct caller).
resolveSpell :: ObjectId -> Game ()
resolveSpell = resolveSpellWith noSubgame

-- CR 608.2: the executor shared by an activated and a triggered ability on the
-- stack. Re-validates filled slots (CR 608.2b), walks the CHOSEN modes in order
-- (CR 608.2c/700.2c) applying each one's effects with `srcId` as the effect source
-- (CR 113.7) and asking about any printed "may" (CR 603.5), then the ability
-- ceases (CR 608.2n). `stackId` is the ability object's own id.
--
-- Takes the modes rather than a flat effect list plus a slot map: the slots ARE
-- the union of those modes' own (CR 700.2c).
--
-- The resolving object's bindings are re-read before EACH effect, exactly as
-- resolveSpellWith does it, so a slot an earlier effect of this same list DEFINED
-- is visible to a later one -- Harried Dronesmith's "create ... token. It gains
-- haste until end of turn", which is CR 608.2c's "instructions in the order
-- written" applied to a sentence that names what the sentence before it made.
--
-- CR 608.2b's question is asked ONCE, off the pre-fold snapshot: `fizzles` below
-- is decided before any effect runs, and the per-effect re-read only ever adds
-- RESERVED slots, which are vacuously legal. Re-deriving the fizzle mid-fold
-- would let a token a Create just minted rescue an ability whose every target is
-- gone.
resolveModes :: ObjectId -> ObjectId -> [(ModeInstance, Mode.Mode Card.Type.Card)] -> Game ()
resolveModes stackId srcId modes = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      -- CR 700.2d: instance-named, not printed-named -- two instances of one
      -- repeated mode declare one printed slot and fill two, and this union would
      -- otherwise collapse them.
      -- CR 608.2b re-judges the slot against the SAME declaration CR 603.3d
      -- offered, so the "that player controls" atoms are baked here too, off the
      -- bindings the placement stamped on this very object
      -- (Pawl.Engine.Target.bakeSlots). An ability whose environment binds no
      -- player leaves them standing, which admits nothing -- see
      -- Pawl.Engine.Filter.bakeBound.
      let slots = Target.bakeSlots (Binding.playerSlots (Object.bindings obj)) (Map.unions (fmap (\(mi, mode) -> Map.mapKeys (Modal.instanceSlot mi) (Mode.targetSlots mode)) modes))
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            -- CR 608.2b is about TARGETS. A slot that declares no target is a
            -- RESERVED binding -- the trigger's source (Pawl.Engine.Binding.triggerSource),
            -- a token this resolution minted -- and was never targeted, so it can
            -- never have become an illegal target.
            Nothing -> recipients
            -- CR 608.2b: the perspective is the ABILITY's controller, the
            -- `effectController` bound below. `srcId` stays the source (CR 113.7)
            -- and may well be gone -- exactly the case this rule is about, and why
            -- the perspective is not read from it. Judged per RECIPIENT, the spell
            -- path's reason.
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just effectController) chosen srcId recipient targetSlot gs) recipients
          legal = Map.mapWithKey legalSlot chosen
          -- CR 608.2b's fizzle asks about the TARGETED slots only, so the
          -- reserved slots above cannot rescue an ability whose every target is
          -- gone. Measured on the slots FILLED rather than declared, for the
          -- reason targetsAllIllegal above gives (CR 115.6).
          targeted = Map.restrictKeys legal (Map.keysSet slots)
          fizzles = not (Map.null targeted) && all Set.null (Map.elems targeted)
          -- CR 113.8 / 603.3a: an activated ability's controller is who activated
          -- it, a triggered ability's is whoever controlled its source when it
          -- triggered. Both are stamped as Object.owner at the ability's creation
          -- and never revisited, and `obj` is the ability object itself, so a
          -- stolen permanent's later controller must not override it.
          effectController = Object.owner obj
          resolveOne (mi, mode) =
            let idx = ModeInstance.index mi
                -- CR 700.2d: this instance's own slots under the names its mode
                -- prints, with every other instance's removed -- the spell path's
                -- projection, applied to both maps so they cannot disagree.
                instanceView = Modal.instanceView slots mi (Mode.targetSlots mode)
                applyOne eff = do
                  -- Re-read the LIVE bindings for THIS effect (CR 608.2c), the
                  -- same shape resolveSpellWith's applyOne has: a Create's single
                  -- token (CR 111.1) reaches the sentence after the one that
                  -- minted it. Both maps are recomputed from the SAME bindings:
                  -- `legalNow` is `chosenNow` with CR 608.2b's illegal recipients
                  -- filtered out, so re-reading one without the other would drop
                  -- exactly the bindings it just gained.
                  bindingsNow <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject stackId)
                  let chosenNow = Binding.targetsOf bindingsNow
                      legalNow = Map.mapWithKey legalSlot chosenNow
                  applyEffect stackId srcId effectController (instanceView legalNow) (instanceView chosenNow) eff
             in -- CR 608.2e's clause is what each gate covers, so all three are
                -- asked once per clause. Run only when `fizzles` is False, so no
                -- question is asked about an ability that never resolves.
                Monad.forM_ (zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode))) $ \(cIdx, clause) -> do
                  -- CR 701.46a: the printed "if" first, for the spell path's
                  -- reason. Read against `srcId`, the source permanent, not the
                  -- ability object -- CR 701.46a says "this permanent", which is
                  -- also why `paid` is given `srcId`.
                  -- The LIVE bindings off the STACK object, the spell path's own
                  -- re-read and for its reason (CR 608.2c) -- and off that object
                  -- rather than off `srcId`, because that is where this
                  -- resolution's slots are bound (see bindSlot). Proved by
                  -- Pawl.ResolveSpec's LookAt group, whose card is Into the
                  -- Wilds.
                  gateBindings <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject stackId)
                  gated <- gateHolds effectController srcId (instanceView (Binding.targetsOf gateBindings)) clause
                  -- CR 603.5 / 608.2d: then the printed "may", answered as this
                  -- clause's instructions are applied.
                  taken <- if gated then exercises stackId effectController idx cIdx clause else pure False
                  -- CR 118.12: then the cost paid on resolution, against the
                  -- START-of-resolution slots -- the spell path's own note says
                  -- why it follows the "may", and why the gate is asked before
                  -- this clause's effects have defined anything.
                  admitted <- if taken then payGateAdmits stackId srcId idx cIdx (instanceView legal) clause else pure False
                  Monad.when admitted (Monad.mapM_ applyOne (Clause.effects clause))
       in do
            Monad.unless fizzles (Monad.forM_ modes resolveOne)
            State.modify' (Game.cease stackId)

-- CR 701.46a: does this clause's printed "if" hold? Adapt's "if this permanent
-- has no +1/+1 counters on it" is the shape in the pool; CR 701.37a's
-- monstrosity prints the same gate on a proper prefix of a longer ability, which
-- is why the rider is on CR 608.2e's clause rather than on the mode. A clause
-- stating no condition always happens.
--
-- Asked as this clause is REACHED (CR 608.2c's written order), so an earlier
-- clause's effects are already on the board -- not once at the start of
-- resolution. Asked BEFORE `exercises`, because the engine must not raise a
-- CR 603.5 prompt whose answer cannot matter.
--
-- `controller` is CR 109.5's "you". `source` is the object both Quantities read,
-- and it is the source PERMANENT rather than the ability on the stack -- CR
-- 701.46a says "this permanent", and CR 113.7a's separation of the two is why
-- `paid` takes `source` apart from `resolving` for its own reason. The two are
-- the same object for a spell.
--
-- CR 608.2h's view, not the live one, and the rule states the case outright:
-- "if the effect has moved it from a public zone to a hidden zone, the effect
-- uses the object's last known information". A clause gate is asked BETWEEN this
-- resolution's clauses, so the object it reads may be one an earlier clause has
-- already moved -- Spikeshell Harrier bounces the permanent whose controller the
-- next sentence then asks about. Same view CR 603.4's intervening "if" is read
-- with (Pawl.Engine.Condition's own account of the three answers), and it differs
-- from the live one only for an id the board no longer holds.
--
-- The CHOSEN slots rather than CR 608.2b's surviving ones, which is the other
-- half of the same rule. That check is made once, as the ability begins to
-- resolve (targetsAllIllegal above), and a target THIS resolution moved is not a
-- target that became illegal before it -- filtering it out here would answer
-- "which player?" with nobody for the very sentence the rule above exists to
-- answer.
gateHolds :: PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Clause.Clause Card.Type.Card -> Game Bool
gateHolds controller source chosen clause = case Clause.condition clause of
  Nothing -> pure True
  Just condition -> do
    gs <- State.get
    pure (Condition.holds (Projection.viewWithLastKnownAnywhere gs) (effectContext controller source chosen) gs source condition)

-- CR 603.5 / 608.2d: does this clause's instruction list happen at all? A
-- mandatory clause always does; an optional one is its controller's call, made
-- HERE as the effect is applied -- CR 603.5 puts an optional ability on the
-- stack regardless and defers the choice to resolution.
--
-- The unit is CR 608.2e's clause and not the whole mode, so a "may" printed on
-- one sentence leaves its neighbours to happen either way (#335).
--
-- `resolving` is the object on the stack, which is what the prompt names.
-- `controller` is who "you" means (CR 405.4 for a spell, CR 113.8 for an ability)
-- and therefore who is asked, through Decide.deciderFor so a player controlled
-- under CR 723.1 has their controller answer.
exercises :: ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Clause.Clause Card.Type.Card -> Game Bool
exercises resolving controller idx cIdx clause = case Clause.optionality clause of
  Optionality.Mandatory -> pure True
  Optionality.Optional -> do
    gs <- State.get
    let decider = Decide.deciderFor controller gs
    decision <- Game.choose (Prompt.ChooseOptional decider controller resolving idx cIdx)
    pure $ case decision of
      OptionalDecision.Exercises -> True
      OptionalDecision.Declines -> False

-- CR 118.12: does this clause's instruction list happen, given the cost paid on
-- resolution it may state? A clause stating none always does. One that states
-- one offers the cost to the player its `payer` slot names, and the instructions
-- are whichever branch PayGate.branch says: Mana Leak's counter is IfNotPaid,
-- reached through CR 118.12a's rewriting ("'[Do something] unless [a player does
-- something else]' ... means the same thing as '[A player may do something
-- else]. If [that player doesn't], [do something]'"), and Merfolk Seer's draw is
-- IfPaid. Either way a refusal is not a failure and the resolution continues.
--
-- The branch is keyed on the ANSWER and never on the board afterwards, which is
-- CR 118.12 in as many words: the clause "checks whether the player chose to pay
-- an optional cost or started to pay a mandatory cost, regardless of what events
-- actually occurred". That rule's Dermoplasm example is what a re-check of the
-- world would get wrong -- the payment's intended consequence was replaced and
-- the branch still ran. Nothing here looks at the target, at the payer's board or
-- at the mana that moved; it reads Prompt.ChooseToPay's answer.
--
-- FOUR ways the answer comes out, of which exactly one is "paid":
--
--   * no payer. The slot is unfilled, has become an illegal target (CR 608.2b),
--     or names an object that is gone, so there is nobody to offer the cost to
--     and nobody has paid. Unreachable for Mana Leak, whose single target slot
--     sends the whole spell through CR 608.2b's fizzle before this is asked.
--   * the payer CANNOT pay. The cost is a "may" -- CR 118.12a's rewriting makes
--     it one, and CR 118.12's other half prints it -- and CR 118.3 says "a
--     player can't pay a cost without having the necessary resources to pay it
--     fully", so declining is the only possible answer and there is nothing to
--     ask. Not asked; see Prompt.ChooseToPay.
--   * the payer declines.
--   * the payer chose to pay. Whether the payment then went THROUGH is the one
--     place the answer is not the raw choice, and it is the reading that leaves
--     the game where it started: Pawl.Engine.Cost.pay restores the entry state,
--     so an Unpaid result is a complete no-op and no cost was paid to read a
--     choice off. Nothing in the pool reaches it -- Mana Leak's {3} is GENERIC,
--     so every tap pays it, and Merfolk Seer's {1}{U} is asked of a player who
--     has just been found able to pay it. A cost reachable that way is the one
--     Pawl.Engine.Cost.payMana's own haddock describes: a player who taps their
--     only Birds of Paradise for green cannot then pay {B} (#417, #56).
--
-- The cost is paid AGAINST `source` rather than the resolving stack object: CR
-- 113.7a keeps the source on the permanent, which is what a component naming
-- "this" must reach. The two are the same object for a spell.
--
-- CR 118.13b's announcement -- how a symbol payable in multiple ways is being
-- paid, chosen immediately before this payment -- is not made (#702).
payGateAdmits :: ObjectId -> ObjectId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Clause.Clause Card.Type.Card -> Game Bool
payGateAdmits resolving source idx cIdx legal clause = case Clause.payGate clause of
  Nothing -> pure True
  Just gate -> fmap (branchTaken (PayGate.branch gate)) (payGatePaid resolving source idx cIdx legal gate)

-- Which branch of CR 118.12 a payment outcome selects. The whole of the rule's
-- polarity, in one comparison and off the classification a card states -- never
-- off what the payment DID.
branchTaken :: PayBranch.PayBranch -> Bool -> Bool
branchTaken branch wasPaid = case branch of
  PayBranch.IfPaid -> wasPaid
  PayBranch.IfNotPaid -> not wasPaid

-- The offer itself: was this gate's cost paid? The four outcomes payGateAdmits
-- above lists, with the branch left to it.
payGatePaid :: ObjectId -> ObjectId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> PayGate.PayGate -> Game Bool
payGatePaid resolving source idx cIdx legal gate = do
  gs <- State.get
  let cost = PayGate.cost gate
  case payerOf (PayGate.payer gate) legal gs of
    Nothing -> pure False
    Just payer ->
      if not (Cost.canPay payer source cost gs)
        then pure False
        else do
          decision <- Game.choose (Prompt.ChooseToPay (Decide.deciderFor payer gs) payer resolving idx cIdx cost)
          case decision of
            PaymentDecision.Declines -> pure False
            PaymentDecision.Pays -> do
              outcome <- Cost.pay ManaSpending.AsProduced payer source cost
              pure (outcome == Payment.Paid)

-- Which player a resolution cost is offered to. ONE slot read answering in two
-- ways, since a slot may hold either kind of recipient: a slot bound to a PLAYER
-- names that player, and one bound to an OBJECT names whoever controls it -- CR
-- 109.4, "only objects on the stack or on the battlefield have a controller",
-- and CR 405.4 for a spell in particular. Mana Leak's "its controller", read off
-- the targeted spell. NOT CR 109.5, which is the rule for the words "you" and
-- "your" on an object; this card says "its controller" instead.
--
-- Legality is asked as every other slot read asks it (CR 608.2b): a target that
-- has since become illegal is already out of `legal`, and a reserved slot
-- declares no target, so legalSlot dropped nothing from it. A slot naming
-- SEVERAL pays nothing -- an "unless [a player] pays" names one payer
-- (`legalOne`).
payerOf :: SlotName -> Map.Map SlotName (Set Recipient) -> GameState -> Maybe PlayerId
payerOf slot legal gs = case legalOne slot legal of
  Just (Recipient.ToPlayer pid) -> Just pid
  Just recipient -> Recipient.objectOf recipient >>= \oid -> Projection.controllerOf oid gs
  _ -> Nothing

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (CR 113.7a), not the ability object. Reads only the ability's CHOSEN modes (CR
-- 700.2c), stamped at activation, and reuses applyEffect with the same per-slot
-- legality and CR 608.2b fizzle as a spell. The ability then ceases (CR 608.2n)
-- rather than being buried -- an ability is not a card.
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Game ()
resolveAbility abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.modesOf (Object.bindings obj)
       in resolveModes abilId srcId (Modal.chosenModes chosen (ActivatedAbility.modal ability))

-- CR 701.27a over ONE object: turn it over, or leave the map exactly as it was.
-- The Transform arm of applyOneEffect folds this over its victims.
--
-- The turn itself is Game.turnFaceOver, shared with the CR 702.145c/f sweep in
-- Pawl.Engine.Daytime, and that is where the account of what a turn writes and of
-- the ways it declines (CR 701.27c, CR 701.27d, a card in another zone) lives.
-- What this adds is the TWO gates that belong to an instruction rather than to
-- the act, and a static ability's turn has neither, which is why the split is
-- here:
--
--   * CR 701.27f's already-turned check, alreadyTurnedFor below.
--   * CR 702.145b's third static ability and CR 702.145e's second -- "this
--     permanent can't transform except due to its daybound/nightbound ability" --
--     Pawl.Engine.Daytime.restrictsTransform. The transform those rules DO permit
--     is CR 702.145c's and CR 702.145f's sweep, which is Daytime's own and calls
--     Game.turnFaceOver without passing through here.
--
-- Reading a KEYWORD is reading the rulebook, the licence Pawl.Types.Keyword's own
-- comment states: rule 702 is as much a part of the comprehensive rules as rule
-- 701 is. Nothing here asks which EFFECT ordered the turn.
--
-- `now` is minted ONCE for the whole instruction by the caller rather than per
-- victim, because CR 608.2f processes a swept set simultaneously: two Humans
-- transformed by one Moonmist turned over at the same moment, and a later CR
-- 701.27f comparison must not be able to tell them apart. `pcs` is hoisted for
-- the same reason it is elsewhere -- ONE whole-board projection per instruction
-- rather than one per victim.
turnOver :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> Timestamp.Timestamp -> GameState -> ObjectId -> Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
turnOver pcs resolving now gs oid objects
  | alreadyTurnedFor resolving oid gs = objects
  | Daytime.restrictsTransform pcs oid = objects
  | otherwise = Game.turnFaceOver now gs oid objects

-- CR 701.27f, first sentence: "If an activated or triggered ability of a
-- permanent that isn't a delayed triggered ability of that permanent tries to
-- transform it, the permanent does so only if it hasn't transformed or converted
-- since the ability was put onto the stack. ... if the permanent has already
-- transformed or converted, an instruction to do either is ignored."
--
-- True when this resolution must be ignored. Two conditions, and BOTH narrow:
--
--   * the resolving object is an ability whose SOURCE is the very permanent
--     being turned over (CR 113.7a). A spell is not one -- Moonmist transforms a
--     permanent that already transformed this turn, because the rule names only
--     abilities -- and neither is an ability of some OTHER permanent, because the
--     rule says such an ability "tries to transform IT".
--   * the permanent's last turn is later than the moment the ability was put
--     onto the stack, which is that ability object's own CR 613.7d timestamp.
--     Both come from GameState.nextTimestamp, so one `>` decides it and equality
--     cannot arise -- a fresh stamp is never reissued.
--
-- The rule's SECOND sentence measures a delayed triggered ability from when it
-- was CREATED rather than from when it reached the stack, and that half is not
-- implemented: a fired delayed ability arrives as the same Source.OfTrigger an
-- ordinary one does (Engine.placeBorne), and Pawl.Types.DelayedTrigger records
-- no creation moment to compare against, so this measures it from the stack like
-- any other trigger (#694).
alreadyTurnedFor :: ObjectId -> ObjectId -> GameState -> Bool
alreadyTurnedFor resolving victim gs = case Game.lookupObject resolving gs of
  Nothing -> False
  Just ability ->
    abilityOf (Object.source ability)
      && maybe False (> Object.timestamp ability) (Game.lookupObject victim gs >>= Object.turnedOverAt)
  where
    -- A CLASSIFICATION of the resolving object -- which kind of object it is,
    -- never which card it is. CR 725.2's sourceless inherent trigger has no
    -- permanent to be an ability OF, so it falls out with the spells.
    abilityOf source = case source of
      Source.OfAbility srcId _ -> srcId == victim
      Source.OfTrigger srcId _ -> srcId == victim
      Source.OfCard _ -> False
      Source.OfToken _ -> False
      Source.OfEmblem _ -> False
      Source.OfInherentTrigger _ _ -> False

-- CR 608.2b: the ONE recipient still legal in `slot`, for a reader that can take
-- only one. Nothing when the slot named none, when its target has become illegal,
-- or when it names SEVERAL -- a reader that cannot take a group must not silently
-- take one member of one, and Pawl.CardSpec's plural-slot lint is what keeps a
-- card from aiming a multi-target slot at one of these readers.
legalOne :: SlotName -> Map.Map SlotName (Set Recipient) -> Maybe Recipient
legalOne slot legal = Binding.onlyOne (Map.findWithDefault Set.empty slot legal)

-- The same read for a reader that takes them ALL -- "up to two target creatures"
-- -- with CR 608.2b's illegal ones already dropped.
legalMany :: SlotName -> Map.Map SlotName (Set Recipient) -> [Recipient]
legalMany slot legal = Set.toList (Map.findWithDefault Set.empty slot legal)

-- The players a PlayerRef names DURING a resolution, read from the slots this
-- resolution filled rather than the source's bindings (which is what
-- Count.playersFor reads for a static count).
--
-- A slot's legality is asked as every other slot read asks it (CR 608.2b): a
-- target that has since become illegal is already out of `legal`, and a reserved
-- slot declares no target, so legalSlot dropped nothing from it. A slot naming
-- SEVERAL names nobody here -- a PlayerRef points at one player (`legalOne`).
--
-- CR 102.1: a player who has left keeps their row in GameState.players with a
-- Departed status, so `everyone` is Game.stillPlaying rather than the map's keys.
-- Only the two enumerating arms read the roster -- `Relative You` and `InSlot`
-- name one specific player who arrived from elsewhere, and whether a departed
-- player can still be one of those is CR 800.4d/800.4i's question (#181).
--
-- `everyone` is in PlayerId order rather than turn order: a PlayerRef names an
-- unordered SET, and a caller with an ordering rule imposes it on this answer, as
-- the Draw arm does for CR 121.2c.
playerRefPlayers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> PlayerRef -> [PlayerId]
playerRefPlayers legal controller gs ref = case ref of
  PlayerRef.InSlot slot -> case legalOne slot legal of
    Just (Recipient.ToPlayer pid) -> [pid]
    _ -> [] -- an unfilled, illegal, or non-player slot: no-op
  PlayerRef.Relative PlayerRelation.You -> [controller]
  PlayerRef.Relative PlayerRelation.Opponent -> filter (/= controller) everyone
  PlayerRef.EachPlayer -> everyone
  -- EachPlayer minus the seat the slot names. A slot that is unfilled, illegal,
  -- names several, or names an object excludes NOBODY -- see the type, where the
  -- widening is the producer's reading rather than a fallback.
  PlayerRef.EachPlayerExcept slot ->
    let excluded = legalOne slot legal >>= Recipient.playerOf
     in filter (\pid -> Just pid /= excluded) everyone
  -- The baked seat, named outright -- InSlot's answer with the lookup already
  -- done, and unreachable from card data. Not filtered against the roster, the
  -- reason InSlot is not: it names one specific player who arrived from
  -- elsewhere.
  PlayerRef.Specific pid -> [pid]
  -- NOBODY, and not a hole: an effect names the players it acts on, and there is
  -- no fold running over a resolution's opcodes for a candidate to come from.
  -- The reference is only ever answerable inside a Count (Pawl.Engine.Quantity's
  -- playersOf), so here it names an empty set and the opcode is a no-op.
  PlayerRef.Candidate -> []
  -- CR 608.2h: the controller of the object the slot names, through last known
  -- information -- the clause that names the player generally MOVED it first
  -- (Spikeshell Harrier bounces it), and CR 108.4 leaves a card in a hand with no
  -- controller at all, so the live reading alone would answer nobody. Nothing to
  -- act on when the slot is unfilled, names several objects, names a player, or
  -- nothing was filed for a gone one.
  PlayerRef.ControllerOfBound slot -> case legalOne slot legal of
    Just recipient -> case Recipient.objectOf recipient of
      Just oid -> Maybe.maybeToList (Projection.controllerWithLastKnown oid gs)
      Nothing -> []
    Nothing -> []
  where
    everyone = Game.stillPlaying gs

-- The objects an ObjectRef names DURING a resolution -- the object-side twin of
-- playerRefPlayers above, and the ONE place a filter-selected set is swept, so
-- every opcode that takes an ObjectRef gets the same answer.
--
-- InSlot takes every recipient CR 608.2b left legal -- one for "target
-- creature", up to the announced count for "up to two target creatures" (CR
-- 601.2c) -- and a player recipient among them names no object. It asks that of a
-- TARGET; a slot bound to a GROUP -- a Create's minted tokens, a MoveToZone's
-- arrivals -- is answered before the question is put, since a group is a
-- definition rather than a target (CR 115.10a) and CR 608.2b has nothing to
-- re-validate about one.
--
-- EachMatching folds the battlefield (CR 109.2: a description with a card type
-- and no zone "means a permanent of that card type ... on the battlefield")
-- against the projection, so a permanent that is a creature only because of a
-- layer-4 effect is in the set and one whose printed line says Creature but is
-- currently not is out. The filter context is this effect's own -- CR 109.5's
-- "you" is the ability's controller and IsSource is its source -- because the
-- filter IS the ability's card text. That is the footing AttachTarget's
-- destination filter is on, and not PlayerSacrifices', whose filter is read
-- against the victim instead. EachCardInGraveyard is the same fold over CR
-- 400.1's per-player graveyards instead of the shared battlefield (CR 109.2a).
--
-- WHEN: at the moment the caller runs, which is when this instruction is
-- reached, CR 608.2c ("follows its instructions in the order written"). The list
-- is then FIXED -- the caller iterates over this answer, so nothing a later
-- element's fate does can add to or remove from it. That is one half of CR
-- 608.2f's "each such action is processed simultaneously": WHICH objects the
-- instruction names.
--
-- The other half is not this function's to keep. Whether each named object is
-- actually AFFECTED has to be judged before any of them is, or "destroy all
-- creatures" degrades into destroying them one at a time with a fresh look in
-- between -- so a caller hands the whole list to its funnel as one batch rather
-- than calling it once per element. Event.destroy's haddock has that half.
--
-- The two callers that store a CONTINUOUS effect -- ModifyTarget and GainControl
-- -- take a further obligation from CR 611.2c: "the set of objects it affects is
-- determined when that continuous effect begins. After that point, the set won't
-- change." This answer is that determination, and those arms freeze it into the
-- stored effect as Affected.TheseObjects rather than keeping the Filter around.
-- Nothing here enforces that; it is stated so a third storing caller does not
-- reach for Affected.Matching, which is a STATIC ability's dynamic set.
--
-- ORDER: APNAP (CR 608.2f's "APNAP order is used to make the primary
-- determination of the order of those actions"), then ascending ObjectId within
-- a controller. That second key is the ENGINE's, and rule 608.2f's secondary
-- sentence does not take it away: that sentence is guarded by "if the action
-- can't be processed simultaneously", and every reader of this function hands its
-- whole answer to a funnel as ONE simultaneous batch. `forEachOrder` is where the
-- guard opens, and it asks rather than reading this order. The no-controller
-- fallback is unreachable: Projection.controllerOf answers Nothing only for an
-- object that does not exist, and every id here came out of the battlefield.
objectRefObjects :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [ObjectId]
objectRefObjects legal resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- The slot names every object bound there as a group -- a Create's tokens
    -- ("they"), a MoveToZone's arrivals ("those cards") -- so all of them are
    -- named at once. Ahead of the target read and not subject to `legal`:
    -- a group binding is a definition, never a target (CR 115.10a), so CR 608.2b
    -- has nothing to re-validate. See slotGroup for why being ahead is safe.
    Just group -> Foldable.toList group
    Nothing -> Maybe.mapMaybe Recipient.objectOf (legalMany slot legal)
  ObjectRef.EachMatching filter_ ->
    let context = Filter.contextFor (Just controller) (Just source)
        matching =
          filter
            (\oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_)
            (Set.toList (GameState.battlefield gs))
        order = Game.apnapOrder gs
        last_ = length order
        seat oid = case Projection.controllerOf oid gs of
          Nothing -> last_
          Just pid -> Maybe.fromMaybe last_ (List.elemIndex pid order)
     in List.sortOn (\oid -> (seat oid, oid)) matching
  -- Rise of the Dark Realms' "all creature cards from all graveyards": the same
  -- sweep as EachMatching with CR 109.2's battlefield default switched off by the
  -- card's own words (CR 109.2a), over the per-player zone CR 400.1 gives each
  -- player instead of one shared one. Whose graveyards, which cards match and in
  -- what order are all graveyardCards below, shared with the chosen-card arm.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard scope filter_) -> graveyardCards controller source gs scope filter_
  -- Ignorant Bliss' "all cards from your hand": CR 400.1's per-player zone
  -- again, but only ever the RESOLVING CONTROLLER's, so there is no scope to
  -- fold over and no APNAP order to impose -- one seat cannot be out of order
  -- with itself. In the zone's own order, which is the order Game.zoneMembers
  -- keeps and no rule reads: CR 400.5 leaves a hand's arrangement to its owner,
  -- so nothing observes it and nothing may depend on it.
  ObjectRef.EachCardInYourHand -> Game.zoneMembers Zone.Hand controller gs
  -- CR 607.2a's linked set: the cards in exile GameState.exiledWith files against
  -- this effect's SOURCE. The relation, not a zone sweep, is the membership test,
  -- so a card exiled by a second copy of the same printing is a different entry
  -- and is not named here. A stated Filter then narrows that set -- Karn
  -- Liberated's "all non-Aura permanent cards exiled with Karn" -- read exactly
  -- as the EachMatching arm above reads its own, against each linked card's
  -- projection wherever it sits.
  --
  -- `source` and not `resolving`, which is the whole point: rule 607.2a links two
  -- abilities of one OBJECT, and for a dies trigger the two ids differ (the
  -- ability object on the stack is not the permanent that exiled anything).
  --
  -- Read off GameState.exile directly, where every sibling arm goes through
  -- Game.zoneMembers: CR 400.1 makes exile one SHARED zone, so there is no player
  -- to ask it about.
  --
  -- In exile-set order, which is ascending id and so arrival order, since
  -- Pawl.Engine.Game mints ids increasing. No APNAP sort, unlike the battlefield
  -- and graveyard arms: one shared zone has no seats to interleave, and CR 101.4
  -- asks for an order only where a per-player question is put.
  ObjectRef.EachCardExiledWithSource mFilter ->
    let context = Filter.contextFor (Just controller) (Just source)
        stated oid = case mFilter of
          Nothing -> True
          Just filter_ -> Filter.matches context (Projection.viewOfObject oid gs) filter_
     in filter
          (\oid -> Map.lookup oid (GameState.exiledWith gs) == Just source && stated oid)
          (Set.toList (GameState.exile gs))
  -- Names players and so no objects at all. Empty rather than an error: every
  -- ObjectRef-taking opcode but DealDamage reads objects only, and the same
  -- empty answer is what a slot holding a player already gives them.
  ObjectRef.EachPlayer -> []
  -- CR 401.2's ordered pile, whose head Pawl.Engine.Game.insertIntoZone and
  -- Pawl.Engine.Event.drawCard both already treat as the top (CR 121.1). The
  -- depth is taken from EACH named library, top first, and a library holding
  -- fewer cards than that gives up what it has -- CR 609.3's "does only as much
  -- as possible", which `genericTake` is: it is also what makes "exile the top
  -- card of your library" with an empty library a no-op rather than a failure.
  --
  -- Restricted to the players still in the turn order, and delivered in it, for
  -- the reason the EachMatching arm sorts: CR 608.2f processes the batch at once
  -- and CR 101.4 fixes the order any per-object question is then asked in. That
  -- also drops a player CR 800.4 has taken out of the game, whose library
  -- playerRefPlayers would otherwise still be able to name.
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player depth) ->
    let named = playerRefPlayers legal controller gs player
     in concatMap
          (\pid -> List.genericTake depth (Game.zoneMembers Zone.Library pid gs))
          (filter (`elem` named) (Game.apnapOrder gs))
  -- A card somebody CHOOSES, which is a QUESTION rather than a read -- and this
  -- function has no way to ask one. Empty here, and answered for real by the
  -- MoveToZone arm's own gather, which runs in the Game monad and raises
  -- Prompt.ChooseCardInGraveyard over the candidates graveyardCards and
  -- graveyardCardsOf below give it. A card that writes the ref under any other
  -- opcode gets this empty answer, which is the inert card-data error
  -- Pawl.Types.ObjectRef's own note describes.
  ObjectRef.ChosenCardInGraveyard {} -> []
  -- A card somebody chooses out of their own hand: the graveyard arm's answer
  -- above, for the graveyard arm's reason, and answered for real by the
  -- MoveToZone arm over the seats handChoosers below names.
  ObjectRef.ChosenCardInHand {} -> []

-- The players a graveyard scope names, in APNAP order: the seat half of
-- graveyardCards, shared with ObjectRef.ChosenCardInGraveyard's EachInScope
-- chooser, which asks each of them separately and so needs the seats rather
-- than the flattened cards. Nothing -- an absent perspective -- is empty, and
-- cannot arise from these callers, which always have the resolving controller
-- to offer.
graveyardPlayers :: PlayerId -> GameState -> PlayerScope.PlayerScope -> [PlayerId]
graveyardPlayers controller gs scope =
  let named = Maybe.fromMaybe [] (PlayerEffect.playersInScope (Just controller) gs scope)
   in filter (`elem` named) (Game.apnapOrder gs)

-- The cards in ONE player's graveyard matching the filter, in ascending
-- ObjectId: the per-seat half of graveyardCards, shared for graveyardPlayers'
-- reason.
--
-- The filter is matched against the projection exactly as the battlefield sweep
-- does, in this effect's own context -- so `controller` here is CR 109.5's "you"
-- rather than whoever is choosing, which the EachInScope chooser leaves alone: a
-- card's characteristics do not depend on who is being asked about them.
graveyardCardsOf :: PlayerId -> ObjectId -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
graveyardCardsOf controller source gs pid filter_ =
  let context = Filter.contextFor (Just controller) (Just source)
   in List.sort
        ( filter
            (\oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_)
            (Game.zoneMembers Zone.Graveyard pid gs)
        )

-- The cards in the named graveyards matching the filter: the shared body of
-- ObjectRef.EachCardInGraveyard, which takes all of them, and of
-- ObjectRef.ChosenCardInGraveyard's TheController chooser, which offers them as
-- the candidates for one choice. Written once so "which cards are in scope"
-- cannot mean two things.
--
-- Whose is graveyardPlayers above, which is PlayerEffect.playersInScope -- the
-- reading Pawl.Engine.Target.graveyardRecipients already takes for a target
-- pool.
--
-- A card in a graveyard has no controller, so Filter.ControlledBy is vacuously
-- False for every candidate -- the posture Pawl.Types.Pool.CardsInGraveyard's
-- own note records.
--
-- APNAP and then ascending ObjectId, for the reasons the battlefield arm gives:
-- the seat order is the fold over Game.apnapOrder, which also drops a player CR
-- 800.4 took out of the game, and the second key is the engine's for the reason
-- that arm gives -- these cards are returned as one simultaneous batch, so CR
-- 608.2f's secondary sentence never engages. Not the graveyard's own pile order, which CR
-- 404.2 fixes for other purposes and which no rule makes the order a batch is
-- processed in.
graveyardCards :: PlayerId -> ObjectId -> GameState -> PlayerScope.PlayerScope -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
graveyardCards controller source gs scope filter_ =
  concatMap (\pid -> graveyardCardsOf controller source gs pid filter_) (graveyardPlayers controller gs scope)

-- The seats an ObjectRef.ChosenCardInHand asks, in APNAP order: graveyardPlayers
-- one type over, with the PlayerScope replaced by the ref's own PlayerRef.
--
-- ONE list, where the graveyard arm needs a chooser and a scope: CR 402.3 lets a
-- player look at their own hand and no other, so these seats are the choosers
-- AND the hands, and there is no second question for a scope to answer.
--
-- Through playerRefPlayers, so a slot is read exactly as every other slot read
-- is (CR 608.2b): an unfilled, illegal or non-player slot names nobody and CR
-- 101.3 ignores that share of the instruction. Ordered by Game.apnapOrder for
-- graveyardPlayers' reason -- CR 608.2e puts the choices in CR 101.4's order --
-- which also drops a player CR 800.4 has taken out of the game.
handChoosers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> PlayerRef -> [PlayerId]
handChoosers legal controller gs player =
  let named = playerRefPlayers legal controller gs player
   in filter (`elem` named) (Game.apnapOrder gs)

-- CR 401.2 and CR 401.4: turn the effect's LibraryPlacement into the END each
-- moving object arrives at, and hand back the batch in the order the moves must
-- then be PERFORMED in.
--
-- Two questions, both asked before anything moves, so every answer is given
-- against the board the effect swept (CR 608.2f, "processed simultaneously"):
--
--   * CR 401.2, once per object: Aetherspouts' "its owner puts it on their
--     choice of the top or bottom of their library". Asked of the OWNER, read
--     off the PRE-MOVE object, in the sweep's own order -- which
--     objectRefObjects already delivers in APNAP order (CR 101.4), the order
--     WotC's Aetherspouts ruling spells out.
--   * CR 401.4, once per (owner, end) group of two or more: "the owner of those
--     cards may arrange them in any order". A DIFFERENT decider from CR 608.2f's
--     secondary sentence, which hands the relative order of same-controller
--     actions to the resolving spell's controller; for a library destination CR
--     401.4 takes it back and gives it to the cards' owner.
--
-- The arrangement answer names the cards from the chosen end INWARD, which is
-- how a player reads "arrange them". Game.insertIntoZone puts every arrival AT
-- that end -- prepending for Top, appending for Bottom -- so whichever end it
-- is, the card moved LAST finishes outermost. The batch is therefore performed
-- in REVERSE of the arranged order, one rule for both ends rather than two,
-- precisely because the sequence grows from opposite ends for them.
--
-- A move the CR 616.1 loop cancels (CR 614.6) simply does not arrive, and the
-- rest keep the relative order their owner chose. That is CR 401.4 as written --
-- it arranges the cards an effect PUTS into a library -- where re-asking after a
-- cancellation would ask a question the rule does not.
--
-- Not CR 608.2f's secondary sentence, which `forEachOrder` asks about: that one
-- orders an action the CR gives no rule of its own, where this is CR 401.4's
-- library case, and it SCREENS the sweep order off rather than exposing it.
settleArrivals :: Zone.Zone -> LibraryPlacement.LibraryPlacement -> [ObjectId] -> Game [(ObjectId, LibraryPosition.LibraryPosition)]
settleArrivals zone placement targets = case zone of
  Zone.Library -> do
    settled <- Monad.mapM settleEnd targets
    fmap concat (Monad.mapM (arrange settled) (List.nub (fmap fst settled)))
  -- No other destination has ends: the battlefield, exile and the command zone
  -- are unordered, and the hand, graveyard and stack have arrival rules of their
  -- own. Nothing to settle and nothing to arrange, and the funnel ignores the
  -- position it is handed.
  _ -> pure (fmap (\oid -> (oid, LibraryPosition.defaultValue)) targets)
  where
    settleEnd oid = do
      gs <- State.get
      case fmap Object.owner (Game.lookupObject oid gs) of
        -- Already gone (CR 603.7c's "no longer in the zone it's expected to be
        -- in"). moveOne is a no-op for it, so there is nobody to ask.
        Nothing -> pure ((Nothing, LibraryPosition.defaultValue), oid)
        Just owner -> do
          position <- case placement of
            LibraryPlacement.Stated stated -> pure stated
            LibraryPlacement.OwnerChooses ->
              Game.choose (Prompt.ChooseLibraryEnd (Decide.deciderFor owner gs) owner oid)
          pure ((Just owner, position), oid)
    arrange settled key = do
      let batch = [oid | (k, oid) <- settled, k == key]
          (mOwner, position) = key
      case (mOwner, batch) of
        (Just owner, _ : _ : _) -> do
          gs <- State.get
          answer <- Game.choose (Prompt.ArrangeLibraryArrivals (Decide.deciderFor owner gs) owner position batch)
          pure (fmap (\oid -> (oid, position)) (reverse (Game.permute batch answer)))
        -- One card is one order, which is CR 401.4's own "two or more" rather
        -- than an elision this engine invented.
        _ -> pure (fmap (\oid -> (oid, position)) batch)

-- The same sweep as objectRefObjects, one step earlier: what an ObjectRef names as
-- RECIPIENTS, before the objects are picked out of them.
--
-- It exists because CR 115.4's "any target" includes a player and CR 120.1 lets
-- damage go to one, so DealDamage's InSlot arm must be able to name something no
-- ObjectId can. Every other ObjectRef-taking opcode affects permanents only and
-- reads objectRefObjects, which is this answer with the players dropped.
--
-- EachMatching names permanents, so its members arrive as Recipient.ToObject: CR
-- 109.2 draws the set from the battlefield with no claim about card types beyond
-- the Filter's own, and classifying each one is the OPCODE's job.
objectRefRecipients :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [Recipient]
objectRefRecipients legal resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- A group is objects, so its members arrive as Recipient.ToObject exactly as
    -- EachMatching's do -- a player is something only a TARGET slot can hold, and
    -- CR 111.1's tokens are never players.
    Just group -> fmap Recipient.ToObject (Foldable.toList group)
    Nothing -> legalMany slot legal
  ObjectRef.EachMatching _ -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- Cards, so they arrive as Recipient.ToObject for EachMatching's reason: CR
  -- 109.2a draws the set from the graveyards named and says nothing about card
  -- types beyond the Filter's own.
  ObjectRef.EachCardInGraveyard {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- Cards again, so Recipient.ToObject again, for the graveyard arm's reason:
  -- CR 109.2a draws the set from the hand the card's own words name, and what
  -- kind of object each one is stays the OPCODE's question.
  ObjectRef.EachCardInYourHand -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- Cards a third time, so Recipient.ToObject a third time: CR 607.2a's set is
  -- cards in exile, and which kind each one is stays the OPCODE's question.
  ObjectRef.EachCardExiledWithSource {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- A card, so it arrives as Recipient.ToObject for EachMatching's reason: what
  -- kind of object a library's top card is, is the OPCODE's question.
  ObjectRef.TopOfLibrary {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- CR 120.3a: a player is a damage recipient, and this is the arm
  -- objectRefObjects has nothing to say about. APNAP (CR 608.2f) for
  -- objectRefObjects' reason, and Game.apnapOrder is the turn order rotated to
  -- the active player -- so a player CR 800.4 has taken out of the turn order is
  -- already not in it.
  ObjectRef.EachPlayer -> fmap Recipient.ToPlayer (Game.apnapOrder gs)
  -- No recipients, because there is no answer to give without asking the chooser
  -- and this function cannot: objectRefObjects' own note explains where the real
  -- answer is made, and why an opcode that is not MoveToZone reading this ref is
  -- an inert card-data error.
  ObjectRef.ChosenCardInGraveyard {} -> []
  -- No recipients either, and for the arm above's reason: the answer needs the
  -- chooser asked, and only the MoveToZone gather can ask.
  ObjectRef.ChosenCardInHand {} -> []

-- CR 608.2f's order for the per-object loop: APNAP first ("APNAP order is used
-- to make the primary determination of the order of those actions"), reading a
-- player recipient as that seat and an object as its controller's.
--
-- Imposed HERE rather than left to objectRefRecipients, whose sweeping arms
-- already sort this way but whose InSlot arm answers in Recipient order -- a
-- set's own, and an announcement of targets has no order of its own to keep
-- (Binding.targets is a Set for that reason). Every other ObjectRef reader
-- hands its whole answer to a funnel as ONE simultaneous batch, so the order
-- never showed; this loop is the first reader that takes them one at a time.
--
-- The second key is CR 608.2f's secondary sentence, and it is the RESOLVING
-- CONTROLLER's -- "the player who controls the resolving spell or ability chooses
-- the relative order of those actions" -- so each seat's group is handed to them
-- as a Prompt.OrderForEach rather than settled by ascending ObjectId. Observable
-- through this opcode for the first time, since a body drawing on a depleting
-- resource answers differently per position (Soulfire Eruption's exiled card).
--
-- Asked once per group, in APNAP order of the groups: the between-group order is
-- the rule's primary determination and nobody's choice, and every question of one
-- loop goes to the same player, whose own order for them CR 101.4c leaves open.
-- A group of one is not asked -- there is no relative order to choose.
--
-- FILTERED, NOT TRUSTED, payComponents' posture: Game.permute keeps the engine's
-- order for an answer that is not a permutation of the offered indices.
--
-- Ascending ObjectId is still what a group is OFFERED in, so the prompt and a
-- transcript are deterministic. A recipient the board no longer holds has no
-- controller and sorts last -- CR 608.2b already dropped an illegal TARGET, but a
-- group binding names ids that may since have moved (CR 400.7), so the fallback
-- is reachable rather than defensive. Two such recipients share that bucket and
-- are offered as though they were one seat's, which asks a question the rule does
-- not -- harmlessly, since a body finds nothing to do to either of them.
forEachOrder :: ObjectId -> PlayerId -> [Recipient] -> Game [Recipient]
forEachOrder resolving controller recipients = do
  gs <- State.get
  let order = Game.apnapOrder gs
      last_ = length order
      playerOf recipient = case recipient of
        Recipient.ToPlayer pid -> Just pid
        _ -> Recipient.objectOf recipient >>= \oid -> Projection.controllerOf oid gs
      seat recipient = maybe last_ (\pid -> Maybe.fromMaybe last_ (List.elemIndex pid order)) (playerOf recipient)
      groups = List.groupBy (\a b -> seat a == seat b) (List.sortOn (\recipient -> (seat recipient, recipient)) recipients)
      pick group = case group of
        _ : _ : _ -> do
          answer <- Game.choose (Prompt.OrderForEach (Decide.deciderFor controller gs) controller resolving group)
          pure (Game.permute group answer)
        _ -> pure group
  fmap concat (traverse pick groups)

-- The objects a Create bound into `slot` as a GROUP, read off the RESOLVING stack
-- object's live bindings -- the same place Effect.Sacrifice and ArmDelayedTrigger
-- read it, and not out of `chosen`, which projects CR 601.2c's targets only.
--
-- Live rather than snapshotted, which is what lets a later effect of the same
-- resolution name what an earlier Create minted: Salt Road Skirmish's "they gain
-- haste" is the sentence after the one that made them.
--
-- PRECEDENCE, and why it is not a coin toss: a Just here wins over the slot's
-- target, which would skip a CR 608.2b re-validation the target was owed. One
-- Binding really can carry both fields -- Pawl.Engine.Engine.placeOne merges a
-- delayed ability's placement-time choices with its captured environment PER
-- FIELD, precisely so neither is lost -- so the case is not ruled out by the
-- types.
--
-- It is ruled out by a lint instead. A card can only reach it by declaring a
-- delayed ability's target slot under a name its own Create defines, which is
-- the card saying two different things with one word, and the Pawl.CardSpec lint
-- "no delayed ability declares a target slot under a name its card defines"
-- rejects it. So this arm never actually chooses, and the ordering is which way
-- to fail if that lint were ever removed.
slotGroup :: SlotName -> ObjectId -> GameState -> Maybe (Seq.Seq ObjectId)
slotGroup slot resolving gs = Binding.objectsOf slot (maybe Map.empty Object.bindings (Game.lookupObject resolving gs))

-- slotGroup's singular: the ONE object bound at a slot, read live off the
-- resolving object rather than out of `chosen`.
--
-- LIVE rather than through `chosen`, and not because `chosen` is stale -- both
-- resolution paths re-read it per effect (CR 608.2c) -- but because it is a
-- PROJECTION: `chosen` carries CR 601.2c's targets under the printed names CR
-- 700.2d renames them to, and gates every read on CR 608.2b legality. A binding
-- an earlier effect of the same list DEFINED (Effect.MoveToZone's CR 400.7 slot)
-- is neither a target nor renameable, so reading it off the object is the
-- shorter true answer. See bindObjectsSlot's note, and CR 603.7c for why the
-- binding exists at all.
--
-- Nothing when the slot is unbound, holds a group, names several targets, or
-- names a player: both callers want one object or none. A cast offer (CR 608.2g)
-- has nothing else to be; a zone move asks slotGroup FIRST and reaches here only
-- once the slot is known not to hold a group.
slotOne :: SlotName -> ObjectId -> GameState -> Maybe ObjectId
slotOne slot resolving gs = do
  obj <- Game.lookupObject resolving gs
  Recipient.objectOf =<< Binding.onlyOne =<< Map.lookup slot (Binding.targetsOf (Object.bindings obj))

-- CR 608.2g: make the offer Effect.OfferCast carries, and cast if it is taken.
--
-- The four questions, in the order the rules ask them:
--
--   1. IS THERE ANYTHING TO OFFER. The slot names an incarnation an earlier
--      effect bound (CR 400.7), and CR 603.7c's "if the object ... is no longer
--      in the zone it's expected to be in" is answered here by the id simply not
--      resolving to an object any more.
--   2. WHICH FACE (CR 712.11a). `transformed` puts the back face on the stack and
--      CR 712.8c gives the resulting spell that face's characteristics; without
--      the rider it is CR 712.11's default, the card's front face.
--   3. WHAT IT COSTS (CR 118.9). `withoutPayingManaCost` applies the alternative
--      cost the rule names by that very phrase, and `payingInstead` applies a
--      STATED one (CR 702.94a); without either rider the candidates are CR
--      601.2b's own.
--   4. MAY IT BE CAST AT ALL, which is Cast.castableWhenOffered -- the prohibit
--      half of CR 601.3, an affordable cost, and a fillable target set. Asked
--      BEFORE the prompt, so the player is never offered a cast the announcement
--      would only reverse.
--
-- Then, and only then, the "may" (CR 601.2b's decisions are the player's, and so
-- is this one). Declining leaves the card exactly where the earlier effect put
-- it, which for CR 310.12b is exile.
--
-- THE INVARIANT: everything above is a CLASSIFICATION carried by the opcode's
-- CastOffer -- which face, which cost -- and nothing here asks which effect is
-- offering or which card is being offered.
offerCast :: ObjectId -> PlayerId -> SlotName -> CastOffer.CastOffer -> Game ()
offerCast resolving controller slot offer = do
  gs <- State.get
  let offered = do
        oid <- slotOne slot resolving gs
        card <- Game.cardOf oid gs
        -- CR 712.11a for the transformed rider; CR 712.11's default otherwise.
        -- Nothing for a card with no back face at all, which is CR 712.14a's
        -- answer to the same instruction one zone over: an offer that cannot be
        -- made is not made.
        --
        -- CR 709.3's half-choice is not offered on the untransformed branch: the
        -- front face is taken, which for every layout but Split is the only
        -- castable face there is (#904).
        face <- if CastOffer.transformed offer then Card.backFace card else Just (Card.frontFace card)
        let name = Face.name face
            -- CR 118.9a: at most ONE alternative cost, so the applied one
            -- replaces the candidates rather than joining them.
            --
            -- Two riders can state one, and rule 118.9a is why they are asked in
            -- order rather than combined: `withoutPayingManaCost` is CR 118.9's
            -- "without paying its mana cost" and `payingInstead` is its "rather
            -- than pay this spell's mana cost" (CR 702.94a's miracle). No producer
            -- sets both, and if one did the free cast would be the one applied.
            --
            -- CR 118.9d in both cases: an alternative replaces only the MANA
            -- cost, so the face's own additional costs ride along -- which is what
            -- Cost.withoutPayingManaCost does for the free branch and what this
            -- one does explicitly for the stated branch.
            applied
              | CastOffer.withoutPayingManaCost offer = Just (Cost.withoutPayingManaCost face)
              | otherwise = fmap (\c -> c {Cost.Type.components = Cost.Type.components c <> Face.additionalCosts face}) (CastOffer.payingInstead offer)
            -- Face up: CR 708.4's face-down cast is a permission a MORPH
            -- ability gives (CR 702.37d), and an OfferCast opcode carries no
            -- such rider -- CR 310.12b's offer names a face and a cost and
            -- nothing about turning the card over.
            proposed = Cast.asProposed oid name Facing.FaceUp gs
            candidates = maybe (Cost.costsFor name oid proposed) pure applied
        Monad.guard (Cast.castableWhenOffered controller oid name candidates proposed)
        pure (oid, name, applied)
  case offered of
    Nothing -> pure ()
    Just (oid, name, applied) -> do
      let decider = Decide.deciderFor controller gs
      decision <- Game.choose (Prompt.OfferedCast decider controller oid name)
      case decision of
        OptionalDecision.Declines -> pure ()
        OptionalDecision.Exercises -> Cast.castSpellWith applied controller oid name Facing.FaceUp

-- CR 615.3: install one floating damage row over `recipient`, for a duration.
-- The shared body of Effect.PreventNextDamage's, Effect.PreventAllDamage's and
-- Effect.RedirectDamage's arms, which differ only in the DamageRewrite they hand
-- it -- CR 615.7's countdown, CR 615.1's unbounded shield, or CR 614.9's
-- redirection.
--
-- The row's other fields are Replace's: CR 113.7's source, CR 109.5's controller
-- baked at installation, and a fresh timestamp for its CR 614.5 identity.
--
-- One shield PER RECIPIENT, which is why both arms fold this over the set their
-- ObjectRef names rather than installing a single row: CR 615.11 says an effect
-- covering several recipients "creates a prevention shield for each applicable
-- creature when the spell or ability that generates that effect resolves". Every
-- producer in the pool names exactly one, so the fold is over a singleton today.
--
-- The `rider` is CR 615.5's additional effect, Nothing for a row that has none.
-- Only Effect.PreventNextDamage's arm ever passes one: the unbounded shield has
-- no producer that wants one (#1107) and a redirection is not a prevention.
installDamageRow :: Map.Map SlotName PlayerId -> PlayerId -> ObjectId -> Duration.Duration -> Maybe DamageKind.DamageKind -> DamageRewrite.DamageRewrite -> Maybe PreventionRider.PreventionRider -> GameState -> Recipient -> GameState
installDamageRow players controller source duration kind rewrite rider g recipient = case Expiry.arm players controller source duration g of
  -- CR 611.2b: the duration never started, so no shield is installed.
  Nothing -> g
  Just expiry ->
    let (ts, g1) = Game.freshTimestamp g
        active =
          ActiveReplacement.MkActiveReplacement
            { ActiveReplacement.effect =
                ReplacementEffect.DamageR
                  ( DamageR.MkDamageR
                      DamagePattern.MkDamagePattern
                        { -- PRINTED, not assumed. Both prevention producers say
                          -- "damage that would be dealt", naming no kind, and pass
                          -- Nothing so the shield takes combat and noncombat alike;
                          -- Turn the Tables says "all COMBAT damage" and passes
                          -- Just Combat.
                          DamagePattern.whichKind = kind,
                          -- Nor does either name a source, which is CR 615.7's own
                          -- "the number of events or sources dealing it doesn't
                          -- matter" -- so the trivial predicate, admitting every
                          -- source.
                          --
                          -- Not implemented: CR 615.9's shield against a source of a
                          -- player's CHOICE, which CR 609.7a has chosen when the
                          -- effect is created and which this arm would have to bake
                          -- the way it bakes the recipient (#1327).
                          DamagePattern.whatSource = Filter.Type.And [],
                          -- The recipient is BAKED as an id below rather than
                          -- described: this row is installed by a resolution
                          -- that has already chosen the permanent or player it
                          -- shields, so there is nothing for a filter to say.
                          DamagePattern.whatRecipient = Nothing,
                          DamagePattern.whichRecipient = Just recipient
                        }
                      rewrite
                      -- CR 615.5's rider on THIS carrier is the snapshotted one
                      -- on the row below, since the installing spell's targets
                      -- and its "you" cannot be re-derived later; the authored
                      -- field here stays empty.
                      Seq.empty
                  ),
              ActiveReplacement.source = source,
              -- CR 109.5, baked as Replace's is: nothing reads it on one of
              -- these rows today (the pattern names its recipient outright and
              -- has no ControllerRelation to resolve), but the row cannot be
              -- built without one.
              ActiveReplacement.controller = controller,
              ActiveReplacement.timestamp = ts,
              ActiveReplacement.expiry = expiry,
              -- CR 615.7's shield is spent in DAMAGE, not in applications, so
              -- the use count is not what ends it (see Pawl.Types.Uses); a
              -- PreventAll shield and a CR 614.9 redirection have nothing to
              -- spend at all.
              ActiveReplacement.uses = Uses.Unlimited,
              -- CR 614.15: these rows replace damage from any source, including
              -- one this resolution is not itself dealing, so none of them is a
              -- self-replacement.
              ActiveReplacement.origin = ReplacementOrigin.Other,
              ActiveReplacement.rider = rider
            }
     in g1 {GameState.replacements = active : GameState.replacements g1}

-- The context every effect of a resolution evaluates its quantities in: CR
-- 109.5's "you" is the resolving controller, the source frames CR 113.7, and
-- the resolution's slot objects ride along so a Quantity.AgainstSlot can aim at
-- one -- Soul's Majesty reading the power of the creature it targets.
--
-- Only LEGAL recipients, only OBJECT ones, and only where the slot names exactly
-- one. CR 608.2b's last sentences say a part of an effect requiring information
-- about an illegal target fails to determine it, and a player recipient has no
-- characteristics to read at all; a slot naming SEVERAL supplies no one object to
-- evaluate a Quantity.AgainstSlot against, which is why Pawl.CardSpec rejects a
-- card that reads a multi-target slot that way. All three drop out as an absent
-- key, so the quantity is unanswered rather than answered off the source. Nothing
-- in the pool observes the legality half: a one-target spell with an illegal
-- target does not resolve at all.
effectContext :: PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Filter.Context
effectContext controller source legal =
  Filter.contextWithSlots (Just controller) (Just source)
    . Map.mapMaybe Recipient.objectOf
    $ Map.mapMaybe Binding.onlyOne legal

-- The amount ONE RECIPIENT of a per-player instruction reads, which need not be
-- the amount the rest of the table reads: Stronghold Discipline's "each player
-- loses 1 life for each creature they control" gives three seats three answers,
-- where Vision Skeins' literal gives one number to the whole table. Every opcode
-- naming a set of players and an amount evaluates through here, once per
-- recipient, so what "their own" means is stated once rather than opted into arm
-- by arm.
--
-- Two spellings, because a card asks two different questions:
--
--   * Filter.Context's `recipient`, which Filter.ControlledByRecipient reads --
--     CR 110.2's control over the SHARED battlefield, which no per-seat scope can
--     express (#161).
--   * Quantity.forCandidate, which substitutes PlayerRef.Candidate -- a scalar of
--     the recipient's own (Shahrazad's "half their life") and the per-player zone
--     a count folds (Nature's Resurgence's "their graveyard", CR 400.1).
--
-- Both are no-ops for a quantity naming neither, which is every other per-player
-- amount in the pool. So the loop is not a departure from CR 608.2f's single
-- determination: every recipient's amount is read off the SAME pre-effect
-- GameState the caller took, and only a quantity that names the recipient can
-- tell the readings apart.
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

-- One effect, applied, wrapped in the window CR 607.2a's link is filed from: what
-- was in exile before, and what is in it after. The effect itself is
-- applyOneEffect below, whose comment documents every parameter.
applyEffectWith :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card -> Game ()
applyEffectWith runSubgame resolving source controller legal chosen effect = do
  before <- State.gets GameState.exile
  applyOneEffect runSubgame resolving source controller legal chosen effect
  State.modify' (recordExiledWith source before)

-- CR 607.2a's link, filed as the instruction that made it finishes: every card
-- that ARRIVED in exile while the effect ran is filed against that effect's
-- source, which is the object whose ability the instruction was in.
--
-- A DIFFERENCE over GameState.exile rather than a case over the opcode, and that
-- is the design call: rule 607.2a asks whether an ability's instruction exiled
-- the card, never which instruction it was, so a Search with an exile
-- destination, a MoveToZone naming exile and a keyword that exiles are all one
-- road and the rules core stays off effect identity.
--
-- New IDS and not new cards, so the set difference cannot mistake a card already
-- in exile for an arrival: CR 400.7 mints a fresh incarnation for every move, and
-- a card that leaves exile takes its old id with it.
--
-- The INNERMOST filing wins, which is what insertWith keeps: applyEffectWith
-- recurses (Effect.ForEach's body, Effect.PreventNextDamage's payload), so an
-- outer window sees what an inner one already filed. Both recursive callers pass
-- the same `source` today, so nothing observes the choice; keeping the inner one
-- is the answer that stays right if one ever does not.
--
-- Then RESTRICTED to what is still in exile, which is what keeps the map from
-- growing over a game: an entry whose card has left exile can never be named
-- again (the reader intersects with GameState.exile, and CR 400.7 mints a new id
-- for the card that left), so dropping it changes no answer. Pawl.Engine.Departure
-- deletes on the other axis, by owner, when CR 800.4a takes a player's cards out
-- of the game.
--
-- Filed for a SPELL's effects too, where rule 607.2a scopes the link to an
-- activated or triggered ability. Unreadable rather than wrong: the only reader
-- is ObjectRef.EachCardExiledWithSource, which matches against a source id, and
-- CR 608.2n puts the spell into its owner's graveyard as the last part of its own
-- resolution -- so the id it filed under names nothing any later ability can be
-- the source of.
recordExiledWith :: ObjectId -> Set ObjectId -> GameState -> GameState
recordExiledWith source before gs =
  let arrived = Set.difference (GameState.exile gs) before
      file oid = Map.insertWith (\_ inner -> inner) oid source
   in gs {GameState.exiledWith = Map.restrictKeys (foldr file (GameState.exiledWith gs) arrived) (GameState.exile gs)}

-- One effect, applied. The case on the constructor is this module's charter.
-- `runSubgame` is the injected nested-game runner; only the PlaySubgame arm
-- consults it, and the bare applyEffect below passes noSubgame.
--
-- `controller` is the controller of the resolving spell or ability -- who searches
-- their own library (CR 701.23) -- never the effect `source`, which for an ability
-- may already have been sacrificed as a cost. That is why every arm evaluating a
-- Quantity binds its view as `Projection.viewWithLastKnown source gs` rather than
-- `fullView`: CR 608.2h covers information from a specific object including the
-- ability's own source. Uniform across the arms rather than special-cased, the
-- rule being about the source rather than which effect is asking.
--
-- `resolving` is the object ON THE STACK whose resolution this is -- the spell
-- itself, or the ABILITY object -- and is where every slot this fold DEFINES is
-- bound and where ArmDelayedTrigger reads CR 603.7c's captured environment back
-- out. NOT `source`: for an ability the two differ (CR 113.7a keeps `source` on
-- the source permanent, which is where a DealDamage's damage comes from unless
-- its own `dealer` slot names another object under CR 120.2b), and that
-- permanent can be gone before a later effect of the same list runs. The stack
-- object outlives its own effect list by construction (CR 608.2n).
--
-- It is where CR 601.2b's announced X is read from for the same reason, which is
-- why every Quantity goes through Quantity.evaluateFor with both ids rather than
-- Quantity.evaluate with `source` alone: an X-cost ability that sacrifices its
-- source to pay leaves the announced value only on the ability object (#544).
applyOneEffect :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card -> Game ()
applyOneEffect runSubgame resolving source controller legal chosen effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage ref quantity dealer) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- CR 120.1a: "Damage can't be dealt to an object that's not a battle, a
        -- creature, or a planeswalker." Both arms of the ObjectRef can name
        -- something that is not one of those, so both go through
        -- Damage.damageRecipient and neither is trusted:
        --
        --   * a SLOT bound by Pawl.Engine.Event.eventBindings names a permanent
        --     GENERICALLY -- Aether Flash's entrant under
        --     Pawl.Engine.Binding.became, tagged from a trigger condition that
        --     said nothing about card types -- and an entrant that is no longer
        --     a creature (gone by resolution, CR 608.2h) drops out here rather
        --     than becoming a damage event nothing can apply.
        --   * a SET's members are permanents matching the Filter, which is card
        --     text and need not mention a card type at all.
        --
        -- A player recipient survives it untouched: CR 115.4's "any target"
        -- includes one and CR 120.3a is what damage to it does.
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
        -- WHO DEALS IT (CR 120.1). `source` is CR 113.7's default -- the object
        -- the resolving spell or ability came from -- and a `dealer` slot is CR
        -- 120.2b's exception, a sentence naming some other object as the source:
        -- Rabid Bite's "target creature you control deals damage equal to its
        -- power".
        --
        -- Resolved HERE, as the effect applies, and then carried on the
        -- DamageEvent rather than beside it. DamageEvent.source is the one field
        -- every later reader of "what dealt this" already asks -- the CR 704.5h
        -- deathtouch SBA, CR 120.3f's lifelink payee, Replacement's CR 615.1
        -- damage patterns, and the damage triggers' bindings -- and
        -- Damage.damageEvent reads the dealer's own riders off it, so a
        -- redirected source reaches all of them without any of them learning
        -- about redirection.
        --
        -- Through the same InSlot sweep every other slot read takes, so CR
        -- 608.2b applies to a dealer exactly as it does to a recipient: a dealer
        -- whose target went illegal names no object, and the instruction then
        -- has no source and deals nothing. Not damage from `source` instead,
        -- which would be a different card.
        dealerId = case dealer of
          Nothing -> Just source
          Just slot -> Maybe.listToMaybe (objectRefObjects legal resolving controller source gs (ObjectRef.InSlot slot))
    case (dealerId, Quantity.evaluateFor viewOf context gs resolving source quantity) of
      -- No source, no damage: CR 608.2b's illegal dealer, above.
      (Nothing, _) -> pure ()
      -- An unevaluable quantity is a no-op, the powerOf posture.
      (_, Nothing) -> pure ()
      (Just dealt, Just n) ->
        Monad.when (n > 0) $ do
          -- The applied effect IS the event (the M3a spec, section 4):
          -- constructing these DamageEvents and funneling them is the whole
          -- application. CR 120.3e / 120.3a live in applyDamage.
          --
          -- ONE batch, not one call per recipient: CR 608.2f's "each such action
          -- is processed simultaneously" is what Corrosive Gale's "each creature
          -- with flying" needs, and applyDamage is the funnel that keeps it (its
          -- haddock carries the CR 615/616 reading of a batch).
          Damage.applyDamage (fmap (\recipient -> Damage.damageEvent gs DamageKind.Noncombat dealt recipient (Integer.toNaturalSaturating n)) recipients)
          -- CR 615.5's "immediately afterward", for damage a resolution deals:
          -- a shield this damage spent runs its additional effect here, inside
          -- the same resolution, rather than waiting for the next SBA check.
          runPreventionRiders
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    State.modify' $ \gs ->
      -- The affected objects are enumerated ONCE, here, by the same sweep every
      -- ObjectRef-taking opcode uses. Giant Growth's slot and Trumpet Blast's
      -- "attacking creatures" arrive as the same list, so there is one path
      -- rather than two -- and a modification that cannot land at all (a player
      -- recipient, an illegal slot per CR 608.2b, a set that matched nothing)
      -- arrives as the empty one and stores nothing.
      case objectRefObjects legal resolving controller source gs ref of
        [] -> gs
        targets -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
          -- CR 611.2b: the duration never started, so the effect does nothing
          -- and is never stored.
          Nothing -> gs
          Just expiry ->
            -- CR 611.2c: "the set of objects it affects is determined when that
            -- continuous effect begins. After that point, the set won't change."
            -- THIS is that moment: the swept ids are frozen into the stored
            -- effect as Affected.TheseObjects, so a creature that becomes
            -- attacking later is not in it and one that leaves combat is still
            -- in it. Storing the Filter instead would re-derive the set at every
            -- projection and get both wrong.
            --
            -- ONE effect over the whole set rather than one per object: CR 611.2c
            -- describes a single continuous effect with a single set, and one
            -- effect is one timestamp for CR 613.7 to order.
            --
            -- CR 608.2h / 611.2d: the VALUE is locked here too -- "the answer is
            -- determined only once, when the effect is applied". The quantities
            -- are frozen to Literals against the SOURCE (which holds a chosen X)
            -- and the source's CONTROLLER (whose hand a player-scoped count
            -- counts), never against an affected object. See the P3b spec,
            -- section 2.4.
            --
            -- Nothing when a quantity cannot be evaluated at THIS moment: 608.2h
            -- gives the effect exactly one moment to determine its answer, so a
            -- value undetermined here is undetermined for good, and storing the
            -- raw quantity would only move the read to the wrong object at the
            -- wrong time. Nothing is stored instead -- the same shape the Expiry
            -- arm above takes for CR 611.2b's duration that never starts.
            case Projection.freezeQuantities gs source (Just controller) modification of
              Nothing -> gs
              Just frozen ->
                let (ts, gs1) = Game.freshTimestamp gs
                    eff =
                      ContinuousEffect.MkContinuousEffect
                        { ContinuousEffect.source = source,
                          ContinuousEffect.timestamp = ts,
                          ContinuousEffect.expiry = expiry,
                          -- CR 611.2: what a resolution stores is the narrow
                          -- (grantless) modification, widened to the type the
                          -- projection reads. See Projection.widenModification.
                          ContinuousEffect.modification = Projection.widenModification frozen,
                          ContinuousEffect.affected = Affected.TheseObjects (Set.fromList targets)
                        }
                 in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
  -- CR 608.2d: "if an effect of a spell or ability offers any choices other than
  -- choices already made as part of casting the spell ... the player announces
  -- these while applying the effect." Magical Hack's two basic land types, and
  -- Artificial Evolution's two creature types, are such a choice. CR 601.2b-d is
  -- the exhaustive list of what casting announces -- the modes, spliced cards,
  -- the alternative and additional costs, X, the hybrid and Phyrexian
  -- equivalents, the targets, and a division -- and a subtype word swap is none
  -- of them. So the ask is HERE, at the moment the effect is applied, and not at
  -- cast; the difference is observable, because a countered Magical Hack is then
  -- never asked at all (Pawl.ResolveSpec's MagicalHackTiming group proves both
  -- directions).
  Effect.ChangeText (ChangeText.MkChangeText family forbidden slot) -> case legalOne slot legal of
    Just recipient -> case Recipient.objectOf recipient of
      Nothing -> pure ()
      Just target -> do
        gs0 <- State.get
        let decider = Decide.deciderFor controller gs0
            -- CR 612.2: which words the player is offered is which words the
            -- card's own text names. A CLASSIFICATION of the effect (which
            -- family) and not its identity -- there is nothing here that
            -- belongs to Artificial Evolution as opposed to Magical Hack, and
            -- the "can't be Wall" restriction rides in from the data as
            -- `forbidden`.
            question = case family of
              SubtypeFamily.BasicLandType -> Prompt.ChooseLandTypeSwap decider controller resolving slot forbidden
              SubtypeFamily.CreatureType -> Prompt.ChooseCreatureTypeSwap decider controller resolving slot forbidden
        (from, to) <- Game.choose question
        State.modify' $ \gs ->
          -- CR 611.2a: the opcode states no duration, so the effect "lasts
          -- until the end of the game" -- Duration.Indefinite, armed through
          -- Pawl.Engine.Expiry like the other three storing arms rather than naming a
          -- stored Expiry here. Indefinite always arms, so the Nothing branch
          -- is unreachable; it is written out because arm is total over
          -- Duration and CR 611.2b's "never starts" is its general answer.
          case Expiry.arm (Binding.playersIn legal) controller source Duration.Indefinite gs of
            Nothing -> gs
            Just expiry ->
              -- CR 611 / 612: a continuous effect over the one target (CR 611.2c
              -- fixed set). The (from, to) is the answer just announced, baked in
              -- here; Projection rewrites both the target's type line and, at
              -- gather, any static-ability words. Resolve CONSTRUCTS the
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
    -- CR 608.2b: an illegal target "won't be affected by parts of a resolving
    -- spell's effect for which they're illegal", so this part does not happen --
    -- and CR 608.2d's announcement belongs to an effect that IS applied, so
    -- nothing is asked either. Unreachable for both text-changers in the pool,
    -- whose only target is this slot: with it illegal, CR 608.2b's fizzle stops
    -- the resolution before any effect is applied.
    _ -> pure ()
  -- The SECOND place mana reaches a pool, and the one CR 106.3 names outright:
  -- mana "may also be produced by ... the effects of abilities that aren't mana
  -- abilities". A mana ability is applied by Cost.tapForMana at payment and never
  -- resolves (CR 605.3b), but an ability that adds mana is not automatically one
  -- -- CR 605.1a classifies only ACTIVATED abilities, and CR 605.1b makes a
  -- triggered ability a mana ability only where it triggered from a mana ability's
  -- activation or resolution or from mana being added. Burning-Tree Emissary's
  -- enters trigger is neither, so it uses the stack (CR 603.3) and adds its {R}{G}
  -- here; Pawl.ManaSpec's Burning-Tree Emissary group is the proof. No trigger
  -- pawl can express meets CR 605.1b at all, so every triggered producer arrives
  -- here; see #1572.
  --
  -- CR 106.4 / 109.5: into the pool of the player the effect's unnamed "you" is,
  -- which for a triggered ability is its controller -- `controller` here, and not
  -- the active player. The type is decided by Mana.producedTypes and the CR 106.3
  -- tags by Mana.productionTagsGiven off this ability's SOURCE, both of them the
  -- payment path's own readers, so the two routes put identical units in a pool.
  Effect.AddMana production -> do
    gs0 <- State.get
    case Mana.producedTypes source gs0 production of
      -- One settled type is one mana. A clause adding two writes two of these
      -- effects, which CR 608.2c runs in printed order.
      [manaType] ->
        State.modify'
          ( Mana.addMana
              controller
              [ ManaUnit.MkManaUnit
                  { ManaUnit.manaType = manaType,
                    ManaUnit.tags = Mana.productionTagsGiven Map.empty source gs0
                  }
              ]
          )
      -- No type at all, which is CR 607.2d's "the chosen color" with nothing
      -- chosen: producedTypes offers none rather than inventing one, and adding
      -- nothing is the honest answer here for the same reason.
      [] -> pure ()
      -- Not implemented: a resolving ability adding mana whose type is the
      -- player's own choice (CR 105.4's five colours). Picking one would be the
      -- engine making that choice, and the existing prompt -- Cost.chooseManaYield
      -- -- asks about an activation's cost and yield together, which a resolution
      -- has neither of. No triggered ability in the pool adds mana of any colour
      -- (#1571).
      _ : _ : _ -> pure ()
  Effect.Search (Search.MkSearch searcherRef ownerRef quantity filter_ destination) ->
    -- CR 701.23a: match each library card against "the given description",
    -- through the card's own CR 613 projection. Rule 613.1 starts from the actual
    -- object and names no zone, so a library card is folded exactly as a permanent
    -- is: Maskwood Nexus makes every creature card its controller owns every
    -- creature type (CR 613.1d), and Goblin Matron's "a Goblin card" then finds
    -- one printed as something else. CR 208.2a's characteristic-defining power
    -- rides along at layer 7a, so Imperial Recruiter's "creature card with power 2
    -- or less" still sees a Tarmogoyf's real power -- Pawl.ProjectionSpec proves
    -- both readings through this same site.
    --
    -- The context has no perspective (CR 109.5): a search filter never references
    -- a player, so ControlledBy is vacuously False. No source in scope at this
    -- site.
    let searchContext = Filter.contextFor Nothing Nothing
        matches1 g oid = Filter.matches searchContext (Projection.viewOfObject oid g) filter_
     in do
          gs0 <- State.get
          -- Whoever Search.searcher names searches -- the controller for
          -- Evolving Wilds' and Extract's `Relative You`, the targeted player for
          -- Fertilid's Favor's `InSlot`. Whoever Search.owner names owns the
          -- library read: the same seat for the first two cards, the TARGET for Extract's
          -- "target player's library". The searcher is prompted and offered CR
          -- 601.3's cast; the owner's library is read and shuffled. Neither is
          -- `controller` except where a ref says so.
          --
          -- CR 701.23i supplies the order, as CR 121.2c does for Draw, and the
          -- intersection with apnapOrder is that arm's: apnapOrder supplies the
          -- ORDER and the ref the MEMBERSHIP. The owners are ordered the same way,
          -- so a search naming several of either is at least deterministic.
          --
          -- Not implemented: rule 701.23i's SIMULTANEOUS look, each searcher
          -- seeing the libraries before any of them decides. This loop lets each
          -- searcher finish before the next begins, which is the same game for
          -- the one-player refs the pool uses and not for a several-player one
          -- (#1319). A ref naming several OWNERS is the same gap read down the
          -- other axis, and the pool has no such card either.
          let inApnapOrder r =
                let named = playerRefPlayers legal controller gs0 r
                 in filter (\pid -> List.elem pid named) (Game.apnapOrder gs0)
              searchers = inApnapOrder searcherRef
              owners = inApnapOrder ownerRef
              -- How many cards this search may find (CR 701.23a): Explosive
              -- Vegetation's "up to two" is Literal 2 and Rampant Growth's "a basic
              -- land card" is Literal 1. Evaluated ONCE, before the loop -- one
              -- instruction names one count, so several searchers each search for
              -- that many rather than for a number re-read per seat.
              --
              -- An unevaluable quantity and a non-positive one both come out as 0,
              -- the posture Draw and Mill take, and 0 finds nothing.
              cap = case Quantity.evaluateFor (Projection.viewWithLastKnown source gs0) (effectContext controller source legal) gs0 resolving source quantity of
                Just n | n > 0 -> Integer.toNaturalSaturating n
                _ -> 0
          Monad.forM_ searchers $ \searcher -> Monad.forM_ owners $ \owner -> do
            -- CR 101.2 / Leonin Arbiter: a player who can't search libraries does
            -- not, and finds nothing. Asked BEFORE CR 601.3's offer below, because
            -- that offer is made WHILE SEARCHING and this player never begins to.
            --
            -- The rest of the instruction still happens: CR 701.23 says only how
            -- to look, so the shuffle at the end is the CARD's separate
            -- instruction and a prohibition on searching is no reason to skip it.
            prohibited <- State.gets (PlayerEffect.prohibitsSearching searcher)
            -- A cap of zero asks nothing and finds nothing: there is one legal
            -- answer, so there is no choice to put to a player. No card in the
            -- pool can reach it -- every printed count is a positive literal --
            -- and it is written rather than assumed away because a Quantity that
            -- reads a slot could evaluate to it.
            found <-
              if prohibited || cap == 0
                then pure []
                else do
                  -- CR 601.3 (Panglacial Wurm): the chance to cast a
                  -- castable-while-searching card is offered AT THE SEARCH, not
                  -- when the resolution began (#57). Three things follow, and all
                  -- are the rule rather than conveniences:
                  --
                  --   * everything this resolution sequences BEFORE the search has
                  --     already happened, so the offer is made in the game state
                  --     the player is actually searching from -- Scapeshift's
                  --     sacrificed lands are gone before the Wurm's affordability
                  --     is judged;
                  --   * CR 601.3's subject is "a spell or ability", and this site
                  --     is reached by both. The old site was Stack's
                  --     Source.OfAbility arm alone, so a searching SPELL was never
                  --     offered the cast at all;
                  --   * the Wurm's own words are "while you're searching your
                  --     library", so the offer goes to the SEARCHER and is made
                  --     only when the library being searched is the searcher's
                  --     OWN. Extract's controller searching someone else's
                  --     library is not searching theirs, and neither is the owner
                  --     of the library being read, who is not searching at all.
                  Monad.when (searcher == owner) (Cast.castWhileSearching searcher)
                  gs <- State.get
                  let matches = filter (matches1 gs) (Game.zoneMembers Zone.Library owner gs)
                      decider = Decide.deciderFor searcher gs
                  answer <- Game.choose (Prompt.SearchLibrary decider searcher matches cap)
                  -- CR 701.23a: every card found is one the search's own filter
                  -- admits. Filtered, not trusted (#222): naming a card the filter
                  -- excluded, or one that is not in the library at all, finds
                  -- nothing rather than fetching it.
                  --
                  -- Deduplicated as well as filtered, for ChooseDiscard's reason:
                  -- the answer is a LIST, and one card named twice would otherwise
                  -- be fetched twice off a single library card. TRUNCATED too: an
                  -- answer longer than the count is out of the count's range and
                  -- its tail is dropped.
                  --
                  -- What a SHORT answer leaves is the difference between CR 701.23b
                  -- and CR 701.23d, and Filter.statesAQuality is which rule this
                  -- search is under. Stating a quality, it may find "some or all of
                  -- those cards even if they're present" -- down to none -- so the
                  -- omission is honoured and needs no filler. Searching for a bare
                  -- quantity, Extract's "a card", it must find that many or as many
                  -- as the library holds, so the answer is COMPLETED from the
                  -- remaining matches: Effect.Discard's posture below, for the same
                  -- reason -- the rule leaves the player no way to decline, so a
                  -- declining answer cannot be obeyed. `filler` is disjoint from
                  -- `picked` and both are drawn from `matches`, so the take yields
                  -- the cap or the whole of what matched, whichever is smaller.
                  let picked = List.genericTake cap . List.nub $ filter (\oid -> List.elem oid matches) answer
                      filler = filter (\oid -> List.notElem oid picked) matches
                  pure $
                    if Filter.statesAQuality filter_
                      then picked
                      else List.genericTake cap (picked <> filler)
            -- Where the cards go is the CARD's instruction, not rule 701.23's --
            -- that rule says only how to look, and CR 701.23e says the same of the
            -- reveal ("if the effect that contains the search instruction doesn't
            -- also contain instructions to reveal the found card(s), then they're
            -- not revealed"). Evolving Wilds puts it onto the battlefield tapped;
            -- CR 702.29e's typecycling reveals it and puts it into the hand.
            --
            -- The searcher is the revealer: CR 701.20a's "show that card to all
            -- players" is done by the player following the instruction, which is
            -- this seat rather than the resolution's controller.
            -- In the order the searcher named them, so a several-card find enters
            -- the battlefield in a chosen order rather than the library's.
            Monad.mapM_ (putFound searcher destination) found
            -- Shuffle the (possibly reduced) library afterward. The CARD's
            -- instruction, not rule 701.23's -- as just above, that rule says only
            -- how to look. CR 701.23h and CR 701.24b both describe the
            -- shuffle as something an effect instructs, never as part of a
            -- search. The library shuffled is the one that was READ, so this seat
            -- is the owner: Extract's "then that player shuffles" names its
            -- target, and every other shuffling search in the pool says "your
            -- library" of a seat that searches its own.
            lib <- State.gets (Game.zoneMembers Zone.Library owner)
            shuffleAnswer <- Game.ask (Prompt.Shuffle lib)
            State.modify' (reorderLibrary owner (Game.honourShuffle lib shuffleAnswer))
  -- Rest in Peace's ETB: exile every card in every graveyard (CR 400.7 each move
  -- funnels through changeZone). A graveyard->exile move matches no M3f
  -- replacement or trigger, so no cascade.
  --
  -- "Every graveyard" is every player's, and CR 102.1 makes that the players
  -- still in the game -- Game.stillPlaying, not the keys of GameState.players,
  -- which keep a departed seat's row. Unobservable, and written anyway for the
  -- reason Count.playersFor gives: CR 800.4a took every object a departing
  -- player owned out of the game, so Game.zoneMembers finds nothing in their
  -- graveyard and the extra iteration moved nothing either way.
  Effect.ExileAllGraveyards -> do
    gs <- State.get
    let gyCards = concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) (Game.stillPlaying gs)
    Monad.mapM_ (\c -> Event.changeZone c Zone.Exile) gyCards
  -- CR 103.5b (Serum Powder): "exile all the cards from your hand, then draw
  -- that many cards." The count is the hand size BEFORE the exile, which is why
  -- this is one opcode and not an exile followed by a Draw.
  --
  -- Both halves go through the usual funnels: Event.changeZone mints each exiled
  -- card a fresh incarnation (CR 400.7), and Event.drawCard flags a draw from an
  -- empty library, so a short deck still loses at the first upkeep (CR 727.3 /
  -- 729.3) exactly as the mulligan redraw already does.
  Effect.ExileHandThenDraw -> do
    gs <- State.get
    let handIds = Game.zoneMembers Zone.Hand controller gs
    Monad.mapM_ (\oid -> Event.changeZone oid Zone.Exile) handIds
    Monad.replicateM_ (length handIds) (Event.drawCard controller)
  -- CR 727.1/727.1a: restart the game. The starting player of the new game is
  -- this ability's controller (CR 727.1a), which applyEffect already holds as
  -- `controller`; the rebuild lives in Setup (game construction). The engine
  -- reaches it through a generic opcode, never Karn's identity.
  -- CR 727.4: this resolves several frames deep -- inside the priority loop,
  -- inside a step -- and the rebuild replaces the game those frames are running.
  -- GameState.restartSignal is how they unwind to the rebuilt turn 1; see
  -- Pawl.Types.RestartSignal.
  -- CR 727.5: the exempted cards are swept BEFORE the rebuild, out of the game
  -- that is ending -- which is the only state in which the exemption can be read
  -- at all, since the rebuild is what clears exile and the CR 607.2a links.
  -- Karn's "then put those cards onto the battlefield" is a SEPARATE effect of
  -- the same ability, reading the same linked set back off the exile the rebuild
  -- left standing (CR 727.4's additional instructions).
  --
  -- Not implemented: CR 727.5a's exempted commander, which needs a Commander game
  -- to observe (#1627), and CR 727.6's restarted SUBGAME (#1628).
  Effect.RestartGame exempt -> do
    gs <- State.get
    let exempted = case exempt of
          Nothing -> Set.empty
          Just ref -> Set.fromList (objectRefObjects legal resolving controller source gs ref)
    Setup.restartGame performHandAction exempted controller
  -- CR 729.1/729.5: run the nested game to completion (the runner does the
  -- construction, play, funnel-back, and reshuffle); then bind its outcome.
  --
  -- CR 729.1b: the outcome the main game may read is the subgame's WINNER, which
  -- is the whole of what Pawl.Types.Result carries -- so the slot holds a winner
  -- and a Drawn subgame binds nothing. Shahrazad's rider then says "each player
  -- who doesn't win" as PlayerRef.EachPlayerExcept over that slot, and gets the
  -- drawn case for free: no winner is bound, so nobody is excluded and the whole
  -- table pays. Pawl.GameSpec's two Shahrazad cases prove both halves; they are
  -- one board differing only in alice's library size, which is what decides
  -- whether the subgame has a winner.
  --
  -- Binding the winner rather than deriving a loser is what makes the set
  -- available at all. A derived loser is one seat, and CR 729.1b's customer asks
  -- about the complement of one seat -- a set the roster answers and a binding
  -- cannot, since a slot holds one recipient. The roster the complement is taken
  -- against is Game.stillPlaying's (playerRefPlayers), which is the set
  -- Setup.subgameStateFrom seated: a player who left the main game before this
  -- resolved never played the subgame, and GameState.turnOrder would still name
  -- them.
  --
  -- Not implemented: an ability-driven subgame -- this arm runs only on the SPELL
  -- path (#137).
  Effect.PlaySubgame slot -> do
    result <- runSubgame
    case result of
      Result.Won winner -> State.modify' (bindPlayerSlot source slot winner)
      Result.Drawn -> pure ()
  -- CR 608.2d: "choose an opponent", announced by the resolving controller as
  -- this effect is applied, and bound so the sentence AFTER it can say "that
  -- player". Skullwinder is the producer; Infernal Offering prints the same
  -- words.
  --
  -- NOT A TARGET (CR 115.10a: the card does not say the word), so no slot was
  -- announced at CR 601.2c and CR 608.2b has nothing to re-validate. The pick is
  -- therefore made HERE, against the board as this effect runs -- which is what
  -- lets an earlier effect of the same resolution change who the opponents are.
  --
  -- The opponents are Game.stillPlaying's, not GameState.turnOrder's, so a
  -- seat that has left (CR 104.3a) is not offered -- fatesealOne's rule, and the
  -- same three-case shape, elision included: CR 102.2 leaves a two-player game
  -- exactly one opponent and nothing to decide. An answer naming somebody never
  -- offered falls back to the first candidate (Pawl.Engine.Ring.tempt's posture),
  -- since the instruction is mandatory.
  --
  -- NO OPPONENT AT ALL binds nothing, so the following sentence names no player
  -- and does nothing -- CR 101.3 applied to the impossible half rather than to
  -- the whole effect.
  Effect.ChooseOpponent slot -> do
    gs <- State.get
    let opponents = filter (/= controller) (Game.stillPlaying gs)
    chosenOpponent <- case opponents of
      [] -> pure Nothing
      [sole] -> pure (Just sole)
      first : second : rest -> do
        let offered = first NonEmpty.:| (second : rest)
        answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor controller gs) controller source offered)
        pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
    Monad.forM_ chosenOpponent $ \pid -> State.modify' (bindPlayerSlot resolving slot pid)
  Effect.ControlPlayerNextTurn slot ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just (Recipient.ToPlayer target) ->
          -- CR 723.1: schedule control of `target` by this ability's controller
          -- (CR 723.5). Map.insert overwrites a prior pending control (CR 723.1a).
          gs {GameState.pendingControl = Map.insert target (Decider.MkDecider controller) (GameState.pendingControl gs)}
        -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
        _ -> gs
  Effect.Destroy (Destroy.MkDestroy ref regenerability mSlot) -> do
    gs <- State.get
    -- CR 701.8: destroy them through the single funnel -- indestructible (CR
    -- 702.12b) and regeneration (CR 701.19a) are Event.destroy's to decide. The
    -- card's own CR 701.19c rider rides along, because whether a shield may
    -- apply is a fact about THIS destruction (Terror's), not the victim.
    --
    -- The whole set goes to the funnel as ONE batch rather than one call per
    -- victim: CR 608.2f's "each such action is processed simultaneously" governs
    -- which permanents are named (objectRefObjects) and when each one's CR
    -- 702.12b gate is judged (Event.destroy) alike. An illegal slot (CR 608.2b),
    -- a non-object recipient, or a set that matched nothing all arrive here as
    -- the empty list and destroy nothing -- one path, not three.
    destroyed <- Event.destroyReturning regenerability (objectRefObjects legal resolving controller source gs ref)
    -- CR 701.8b: "destroyed this way" is what the funnel DESTROYED, never what
    -- the sweep named -- an indestructible permanent (CR 702.12b) and one a
    -- regeneration shield saved (CR 701.8c) were both named and neither was
    -- destroyed, and the rule's last sentence is explicit that a permanent that
    -- reached a graveyard any other way "hasn't been 'destroyed'". Bound onto
    -- this effect's SOURCE so a later effect of the same resolution reads it as
    -- Quantity.InSlot, which is Bane of Progress' rider; the read goes through
    -- live GameState rather than through `chosen`, which carries recipients
    -- rather than amounts.
    --
    -- Bound even when nothing was destroyed. Zero is an answer -- "for each
    -- permanent destroyed this way" of nothing is no counters -- where leaving
    -- the slot unbound would make the rider's quantity UNEVALUABLE, which is a
    -- different thing that Resolve's arms happen to treat the same way today.
    Monad.forM_ mSlot $ \slot ->
      State.modify' (bindAmountSlot source slot (Natural.length destroyed))
  Effect.Sacrifice slot -> do
    -- A slot a Create bound to a GROUP names every token at once ("those
    -- tokens"), so all of them are sacrificed, in mint order. Read off the
    -- resolving object's LIVE bindings, as ArmDelayedTrigger is, rather than out
    -- of `chosen`: `chosen` projects CR 601.2c's targets and a group binding is
    -- never a target, so it is not there and owes CR 608.2b nothing.
    --
    -- CR 603.7c's zone check applies per MEMBER rather than to the whole word: a
    -- member that is gone is simply not affected, and the rest still are. For a
    -- token that is Event.sacrifice finding no such object at all, since CR
    -- 111.7's state-based action has already made it cease to exist; for a card
    -- permanent it would be that funnel's CR 701.21a battlefield guard.
    bound <- State.gets (slotGroup slot resolving)
    case bound of
      -- One at a time rather than as one event (#757).
      Just victims -> Monad.mapM_ (Event.sacrifice controller) victims
      Nothing -> case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> pure () -- a player recipient cannot be sacrificed
          -- CR 701.21: through the single funnel, which is NOT Event.destroy --
          -- CR 701.21a: sacrificing is not destroying. The sacrificing player is
          -- this effect's controller, which for the "this creature" shape this
          -- opcode serves is the permanent's own controller; the funnel's CR 701.21a
          -- guard turns any other case into a no-op rather than a wrong sacrifice.
          Just target -> Event.sacrifice controller target
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> pure ()
  Effect.TurnFaceDown slot ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> gs -- a player is not a permanent and has no face
          -- CR 708.2a: ONE assignment to Object.facing, and that is the whole
          -- effect. The rule fixes what the permanent becomes -- "a 2/2 face-down
          -- creature with no text, no name, no subtypes, and no mana cost" -- and
          -- calls those "the COPIABLE values of that object's characteristics",
          -- so this is a copiable swap rather than a CR 613 layer. The swap
          -- already exists: Pawl.Engine.Game.faceOf answers with
          -- Pawl.Engine.Card.faceDownFace for any object whose facing says
          -- FaceDown, and every characteristic read in the engine starts there.
          --
          -- What is NOT written is everything CR 708.2a does not list. No CR
          -- 400.7 incarnation is minted -- the permanent keeps its object id --
          -- so marked damage, counters, attachments, the tapped and attacking
          -- statuses and the CR 613.7d timestamp all ride through untouched. The
          -- exact mirror of Pawl.Engine.FaceDown.turnFaceUp, which reverts the
          -- same field for CR 708.8.
          --
          -- CR 708.2b needs no branch HERE: "a face-down permanent can't be
          -- turned face down ... nothing happens and that effect doesn't change
          -- any of its characteristics or their copiable values", and writing
          -- FaceDown onto a permanent that is already FaceDown leaves the map
          -- equal to what it was. It is not reachable in any case -- a face-down
          -- permanent has no keywords (CR 708.2a), so no "with a morph ability"
          -- filter admits it as a target. An effect that LISTS characteristics
          -- would change them and does owe the guard; no such opcode exists
          -- (#957).
          --
          -- No event is recorded, so nothing triggers on the turning-over --
          -- the mirror of FaceDown.turnFaceUp's GameEvent.TurnedFaceUp is
          -- absent (#984). CR 701.27b is what keeps it from being borrowed from
          -- Transform: turning a permanent face down is its own game action.
          Just target ->
            gs
              { GameState.objects =
                  Map.adjust (\o -> o {Object.facing = Facing.FaceDown}) target (GameState.objects gs)
              }
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
  Effect.RemoveFromCombat slot ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> gs -- a player recipient is not in combat
          -- CR 506.4: through Game.removeFromCombat, the one performer of every
          -- clause of that rule -- so this clause takes CR 509.1h's asymmetry
          -- with it for free, an attacker losing its whole entry while a blocker
          -- leaves only the set inside a surviving one. Argued in full there.
          --
          -- Unprompted and undirected: CR 506.4's second sentence says what
          -- removal does, and leaves nothing to ask or to choose.
          Just target -> Game.removeFromCombat target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op. A target
        -- that has already left combat needs no guard either -- removing a
        -- creature that is not in the record is what Game.removeFromCombat
        -- already does to it, which is nothing.
        _ -> gs
  Effect.BecomesBlocked slot ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> gs -- a player recipient is not in combat
          -- CR 509.1h: through Combat.becomeBlocked, which owns every write of
          -- the blocked status and carries the rule's two conditions and CR
          -- 509.3c's event with it. Argued in full there.
          --
          -- Unprompted and undirected, RemoveFromCombat's posture: the rule says
          -- what becoming blocked does, and an effect that says it leaves nothing
          -- to ask -- least of all which creature blocks, since none does.
          Just target -> Combat.becomeBlocked target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone entry mSlot _ placement) ->
    -- ONE object through CR 400.7's funnel, shared by the two arms below so that
    -- what a move DOES is written once and only WHICH objects move differs.
    let -- CR 400.7: the funnel mints a new incarnation in `zone`, owner-relative
        -- (CR 400.3 for a library, graveyard or hand destination).
        -- CR 110.5b: the funnel is handed the riders, so a permanent an effect
        -- says enters tapped is never untapped for an instant -- and, by CR
        -- 712.14a, a double-faced card an effect returns "transformed" never
        -- shows its front face for one either. WHICH face that is stays the
        -- funnel's question, since only Pawl.Engine.Card can read a layout.
        --
        -- CR 401.2: the library position goes to the funnel for the tap state's
        -- reason -- Griptide's "on top of its owner's library" is where the card
        -- ARRIVES, so it has to be settled by the move rather than by a second
        -- write afterward, which would leave the incarnation on the bottom while
        -- the Moved event and any CR 616.1 watcher looked at it. `settleArrivals`
        -- below is what turns the effect's LibraryPlacement into the per-object
        -- end each mover is handed here.
        --
        -- CR 110.2a: "If an effect instructs a player to put an object onto the
        -- battlefield, that object enters the battlefield under THAT PLAYER's
        -- control unless the effect states otherwise." This effect is exactly
        -- such an instruction, so its controller (CR 109.5) is who the permanent
        -- enters under -- Meandering Towershell's "return it to the battlefield
        -- under your control" is that rule restated on the card rather than an
        -- exception to it. The EXCEPTION rule 110.2a allows -- "unless the effect
        -- states otherwise" -- is the riders' `underOwner`, which undying and
        -- persist set and changeZoneEntering reads; this arm hands the funnel
        -- the ability's controller either way.
        --
        -- Handed to the FUNNEL, not applied after it returns,
        -- for the reason the tap state is: control is settled on the entering
        -- incarnation before CR 614.1c's entry loop and the Moved snapshot can
        -- read it, so a CR 616.1b entry replacement that filters on control
        -- (Gather Specimens' "under an opponent's control") sees the controller
        -- the permanent actually enters under. No card in the pool observes that
        -- ordering -- Gather Specimens' row is Uses.Unlimited, so applying it to
        -- a creature already entering under its own controller changes nothing
        -- and consumes nothing -- so it buys the ordering rather than a passing
        -- test. The funnel ignores the player entirely for a non-battlefield
        -- destination, which is CR 110.2's own scope.
        --
        -- No CR 800.4b guard, unlike Effect.GainControl's arm: `controller` here
        -- is whoever controls the resolving spell or ability, and a departed
        -- player controls neither -- CR 800.4d keeps their triggered ability off
        -- the stack and Departure.nonCardStackObjectsCease removes one already
        -- on it, while clause 1 of CR 800.4a took their spells out of the game.
        -- Answers the incarnation CR 400.7 minted, or Nothing when nothing
        -- arrived, so the caller can bind the whole batch once it is complete.
        moveOne (target, position) = do
          mNew <- Event.changeZoneEntering target zone position entry (Just controller)
          -- CR 614.6: the CR 616.1 loop cancelled the move, or the id was already
          -- gone (CR 603.7c's "no longer in the zone it's expected to be in ...
          -- the ability won't affect it"). Nothing entered, so there is nothing
          -- to join to combat and nothing to bind.
          Monad.forM_ mNew $ \newId ->
            -- CR 508.4: "if a creature is put onto the battlefield attacking,
            -- its controller chooses which defending player ... it's
            -- attacking". The rules for that live in Pawl.Engine.Combat, which
            -- is also what keeps this from looking like a declaration -- CR
            -- 508.3a's attack triggers see nothing, INCLUDING the returning
            -- creature's own (Meandering Towershell's ruling says so in as
            -- many words). The Create arm calls the same function for the same
            -- rule.
            --
            -- It reads the new permanent's controller, and CR 506.3b refuses
            -- one who is not the active player -- which the funnel above has
            -- already settled. A Towershell its attacker does not own would
            -- otherwise return to its owner and then fail to be attacking at
            -- all (Pawl.CombatSpec pins both halves).
            Monad.when (EntryRiders.attacking entry) (Combat.putOntoBattlefieldAttacking newId)
          pure mNew
        -- CR 400.7j: "if an effect causes an object to move to a public zone,
        -- other parts of that effect can find that object" -- so bind what
        -- arrived into the resolving object's live bindings, where a LATER
        -- EFFECT of this same resolution or a delayed ability it arms (CR
        -- 603.7c) can name it. Meandering Towershell's "exile it. Return IT to
        -- the battlefield" is the singular reader; Act on Impulse's "you may
        -- play THOSE CARDS" is the plural one.
        --
        -- WHICH SHAPE is decided by how many actually arrived rather than by
        -- which ObjectRef named them, because that is the number the rule is
        -- about: a depth-three exile off a two-card library moved two cards, and
        -- a cancelled move (CR 614.6) left none. One arrival takes the single
        -- binding, which is the only shape a singular reader can see (slotOne
        -- reads Binding.targets) and which an ObjectRef.InSlot still reaches
        -- through each resolution path's per-effect re-read of the bindings;
        -- several take the group, which only the ObjectRef readers see, through
        -- slotGroup. Nothing arrived binds nothing at all, which keeps a slot
        -- from naming an empty set.
        bindArrivals slot arrived = case arrived of
          [] -> pure ()
          [only] -> State.modify' (bindSlot resolving slot only)
          _ -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList arrived))
     in do
          -- WHICH objects move, gathered first and moved second, so that the CR
          -- 401.2 and CR 401.4 questions between the two steps are asked of the
          -- whole batch. BOTH branches go through it: a stated placement is
          -- settled the same way, and an InSlot move is a batch of at most one.
          targets <- case ref of
            -- NOT routed through objectRefObjects, and that is the whole reason
            -- this arm branches by hand rather than sweeping both cases. That
            -- function reads `slotGroup` and then `chosen`, never `slotOne` -- and
            -- slotOne is what lets a slot an EARLIER EFFECT OF THIS SAME RESOLUTION
            -- bound name its object here. Befriending the Moths' chapter III is the
            -- producer: "Exile this Saga, then return IT to the battlefield
            -- transformed", two moves in one resolution where the second names what
            -- the first minted. Sending InSlot through the shared sweep would find
            -- nothing for it.
            --
            -- The slot therefore names a target this ability declared, an
            -- incarnation such an earlier effect bound (the mSlot above, CR 400.7),
            -- or a whole GROUP one bound (a Create's tokens, an earlier move's
            -- arrivals).
            -- A declared target is read out of `chosen` behind CR 608.2b's
            -- re-validation, which is what a target is owed; a slot `chosen` does
            -- not mention was never targeted and owes it nothing, so it is read LIVE
            -- off the resolving object (slotOne, whose own note explains why
            -- `chosen` cannot see it). Testing membership rather than preferring one
            -- answer is what keeps the two apart: a slot cannot be both, and a
            -- target never loses its re-validation to a binding that happens to
            -- share its name. No card observes the difference -- the CardSpec lint
            -- "no delayed ability declares a target slot under a name its card
            -- defines" is what rules the collision out -- so the membership test
            -- buys the ordering rather than a passing test.
            ObjectRef.InSlot slot -> do
              -- A slot bound to a GROUP names every member, which is Feral
              -- Lightning's "exile them" over the three tokens the sentence
              -- before it made. Read FIRST and not subject to `legal`, the
              -- ordering objectRefObjects takes for its own InSlot arm and for
              -- the same reason: a group binding is a definition, never a target
              -- (CR 115.10a), so CR 608.2b has nothing to re-validate. The batch
              -- is in mint order: CR 608.2f's APNAP primary key has nothing to
              -- separate, since one Create's tokens all enter under one player,
              -- and its secondary sentence is guarded by an action that can't be
              -- processed simultaneously -- a whole group's zone change is one
              -- batch, so the order they were made in stands.
              group <- State.gets (slotGroup slot resolving)
              case group of
                Just objects -> pure (Foldable.toList objects)
                Nothing -> do
                  bound <- if Map.member slot chosen then pure Nothing else State.gets (slotOne slot resolving)
                  pure $ case bound of
                    Just oid -> [oid]
                    Nothing -> case legalOne slot legal of
                      Just recipient -> Maybe.maybeToList (Recipient.objectOf recipient)
                      -- Illegal slot (CR 608.2b), a non-object recipient, or a slot
                      -- nothing ever bound.
                      _ -> []
            -- Evacuation's "return all creatures to their owners' hands". Swept
            -- ONCE from the PRE-MOVE state, which is CR 608.2c's "in the order
            -- written" read together with CR 608.2f's "each such action is
            -- processed simultaneously": nothing an earlier move does can add to or
            -- remove from the list, so a creature that stops being one because
            -- another creature left still moves. objectRefObjects already answers in
            -- APNAP order.
            --
            -- Each mover then goes through the same funnel one at a time, so every
            -- arrival gets its own CR 616.1 opportunity. CR 400.3 files each hand
            -- arrival under Object.owner, which is what makes "their OWNERS' hands"
            -- need nothing here: the funnel is handed `Just controller`, and CR
            -- 110.2a's control question is asked only of a battlefield destination.
            ObjectRef.EachMatching _ -> do
              gs <- State.get
              pure (objectRefObjects legal resolving controller source gs ref)
            -- Rise of the Dark Realms' "put all creature cards from all
            -- graveyards onto the battlefield under your control", swept once
            -- from the PRE-MOVE state for the reason the battlefield sweep above
            -- is (CR 608.2c, CR 608.2f). "Under your control" needs nothing here:
            -- the funnel is handed `Just controller` and CR 110.2a hands a
            -- battlefield arrival to the player the effect instructed, which is
            -- what EntryRiders.underOwner would have to override.
            ObjectRef.EachCardInGraveyard {} -> do
              gs <- State.get
              pure (objectRefObjects legal resolving controller source gs ref)
            -- Ignorant Bliss' "exile all cards from your hand face down", swept
            -- once from the PRE-MOVE state for the two sweeps above's reason (CR
            -- 608.2c, CR 608.2f). The spell itself is on the stack while it
            -- resolves (CR 608.1), so it is not among what it exiles.
            ObjectRef.EachCardInYourHand -> do
              gs <- State.get
              pure (objectRefObjects legal resolving controller source gs ref)
            -- Hoarding Dragon's "put the exiled card into its owner's hand" (CR
            -- 607.2a), swept once from the PRE-MOVE state for the three sweeps
            -- above's reason (CR 608.2c, CR 608.2f). "Its owner's hand" needs
            -- nothing here for EachMatching's reason: CR 400.3 files a hand
            -- arrival under Object.owner.
            ObjectRef.EachCardExiledWithSource {} -> do
              gs <- State.get
              pure (objectRefObjects legal resolving controller source gs ref)
            -- Players, and no card moves one to a zone. objectRefObjects' empty
            -- answer, so the move is a no-op rather than a rejected card.
            ObjectRef.EachPlayer -> pure []
            -- Count on Luck's "exile the top card of your library" and Act on
            -- Impulse's "exile the top three cards of your library", read from
            -- the pre-move state exactly as the swept set above is: the whole
            -- batch comes off one look at each library, so nothing an earlier
            -- move of this same resolution did can change what the next card off
            -- the top is (CR 608.2c, CR 608.2f).
            ObjectRef.TopOfLibrary {} -> do
              gs <- State.get
              pure (objectRefObjects legal resolving controller source gs ref)
            -- Port of Karfell's "return a creature card from your graveyard to
            -- the battlefield tapped": one card per chooser, and the only ref
            -- whose gather asks a question rather than reading the board --
            -- which is why it is answered here, in the Game monad, and nowhere
            -- else.
            --
            -- The candidates are read from the pre-move state exactly as the
            -- sweeps above are (CR 608.2c), so an earlier effect of this same
            -- resolution -- Port of Karfell's own mill -- has already put its
            -- cards in the graveyard and they are on offer, which is what "mill
            -- four cards, THEN return" says.
            --
            -- WHO is asked is the ref's Pawl.Types.Chooser: the resolving
            -- CONTROLLER (CR 608.2c, CR 608.2d) whatever graveyards the scope
            -- draws the candidates from, or EACH PLAYER the scope names, asked
            -- about their own graveyard alone -- Exhume's "each player puts a
            -- creature card from their graveyard onto the battlefield".
            --
            -- The per-player asks run in APNAP order and all of them before any
            -- card moves, which is CR 608.2e read with CR 101.4: the choices for
            -- an action involving several players are made in that order and the
            -- action is then processed. `gs` above is the pre-move state and
            -- asking changes nothing, so every chooser sees the same board and
            -- none of them sees another's answer -- and they cannot collide in
            -- any case, since no card is in two graveyards.
            --
            -- Elided at one candidate and skipped at none, which is
            -- Prompt.ChooseCardInGraveyard's documented rule: one matching card
            -- is the whole of "a creature card in your graveyard" and leaves
            -- nothing to decide, and none makes the instruction impossible and
            -- so ignored (CR 101.3, CR 609.3). Under EachInScope that is per
            -- player: an empty graveyard drops one player out of the batch
            -- rather than the instruction out of the effect.
            --
            -- FILTERED, NOT TRUSTED -- Pawl.Engine.Ring.tempt's posture: an
            -- answer naming a card never offered falls back to the first
            -- candidate, since the instruction is mandatory and something must
            -- move.
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
                Chooser.TheController -> ask controller (graveyardCards controller source gs scope filter_)
                Chooser.EachInScope ->
                  fmap concat . Monad.mapM (\pid -> ask pid (graveyardCardsOf controller source gs pid filter_)) $
                    graveyardPlayers controller gs scope
                -- Skullwinder's "that player returns a card from their graveyard
                -- to their hand": ONE chooser, read out of the slot the
                -- ChooseOpponent before it bound, choosing out of their own
                -- graveyard -- EachInScope's per-player ask over a single seat.
                --
                -- Through playerRefPlayers so the slot is read exactly as every
                -- other slot read is (CR 608.2b): an unfilled, illegal or
                -- non-player slot names nobody, and so does a slot naming
                -- several. Nobody asked is nothing moved, CR 101.3's answer for
                -- this share of the instruction.
                --
                -- Intersected with the scope, which is the only thing left for it
                -- to say once the chooser fixes whose graveyard: a chooser the
                -- scope does not name is offered nothing. Skullwinder's sentence
                -- restricts nothing and so writes EachPlayer.
                Chooser.BoundInSlot slot ->
                  case playerRefPlayers legal controller gs (PlayerRef.InSlot slot) of
                    [pid] | List.elem pid (graveyardPlayers controller gs scope) -> ask pid (graveyardCardsOf controller source gs pid filter_)
                    _ -> pure []
            -- Karn Liberated's "+4: target player exiles a card from their
            -- hand": the arm above over the hidden zone CR 400.2 makes a hand,
            -- and everything that arm says about WHEN the candidates are read
            -- (CR 608.2c, from the pre-move state), about the per-player asks
            -- running in APNAP order before anything moves (CR 608.2e, CR
            -- 101.4), and about the answer being FILTERED rather than TRUSTED
            -- holds here unchanged.
            --
            -- What the hidden zone changes is WHO may be asked: CR 402.3 gives a
            -- hand's cards to its owner alone to look at, so each seat is offered
            -- its OWN hand and no other, and the ref carries one PlayerRef rather
            -- than a chooser beside a scope. Karn's is a target slot, so the
            -- opponent it names does the choosing -- the controller never sees
            -- the hand, and pawl's engine never picks for them.
            --
            -- Unfiltered, so the candidates are the whole hand: Game.zoneMembers
            -- in the zone's own order, which is the order the EachCardInYourHand
            -- sweep takes and which no rule reads (CR 400.5).
            --
            -- Elided at one card and skipped at none, ChooseCardInGraveyard's
            -- rule (CR 101.3, CR 609.3): a one-card hand leaves nothing to decide
            -- and an empty one nothing to do. Neither elision leaks anything --
            -- the card was going to a public zone either way.
            ObjectRef.ChosenCardInHand player -> do
              gs <- State.get
              let ask asked candidates = case candidates of
                    [] -> pure []
                    [only] -> pure [only]
                    first : second : more -> do
                      let offered = first NonEmpty.:| (second : more)
                      answer <- Game.choose (Prompt.ChooseCardInHand (Decide.deciderFor asked gs) asked source offered)
                      pure [if List.elem answer (NonEmpty.toList offered) then answer else first]
              fmap concat . Monad.mapM (\pid -> ask pid (Game.zoneMembers Zone.Hand pid gs)) $
                handChoosers legal controller gs player
          arrived <- Monad.mapM moveOne =<< settleArrivals zone placement targets
          Monad.mapM_ (\slot -> bindArrivals slot (Maybe.catMaybes arrived)) mSlot
  -- CR 701.24: shuffle the objects the ref names into their OWNERS' libraries.
  -- Two steps, in this order and with the owners read before either:
  --
  --   * CR 400.7's move, through the same changeZone funnel every other
  --     destination uses, so a replacement watching a library entry gets its CR
  --     616.1 opportunity. Game.insertIntoZone files a library arrival under
  --     Object.owner per CR 400.3, so the card lands in the owner's library
  --     without this arm naming a player.
  --   * CR 701.24a's randomisation, through Mulligan.shuffleLibrary -- the same
  --     call CR 103.3's opening shuffle makes.
  --
  -- The shuffle runs whether or not the move did, which is CR 701.24c, and is the
  -- whole reason the owner is read from the PRE-MOVE object rather than the
  -- incarnation the funnel mints: a cancelled move leaves no incarnation and the
  -- library still has to be shuffled.
  --
  -- CR 701.24c's OTHER half -- "even if none of those objects are in the zone
  -- they're expected to be in" -- is the effect's own PlayerRef, and not the
  -- owners: an id that no longer resolves has no owner left to read, so a
  -- resolution that finds every named object gone shuffles the named library and
  -- nothing else. Dwell on the Past and Gaea's Blessing are what reach it, their
  -- player slot keeping CR 608.2b from fizzling the spell once every targeted
  -- card has left the graveyard, where Riftsweeper's single target made the
  -- ability fizzle first. Riftsweeper names no library and is unchanged.
  --
  -- The same reading covers CR 701.24d's empty SET -- "shuffled even if there
  -- are no objects in that set" -- which is Gaea's Blessing's trigger over a
  -- graveyard something else emptied first.
  --
  -- The UNION of the two, rather than the PlayerRef alone when it is there:
  -- rule 701.24's objects go to their OWNERS' libraries (CR 400.3), and the named
  -- player need not be one of those owners, so every library that receives an
  -- object is shuffled beside the one the effect names. For every card in the
  -- pool the two answers coincide, since a card in a player's graveyard is that
  -- player's; the union is what stays correct for one where they do not.
  --
  -- CR 608.2f: the objects are moved as ONE action and each library is then
  -- shuffled ONCE, however many of the objects it received. Rule 701.24's own
  -- words are plural ("one or more specific objects"), so a card naming four is
  -- one shuffle per library and not four.
  --
  -- CR 701.24a's "so that no player knows their order" makes WHO shuffles
  -- unobservable, which is why the card's "its owner shuffles" needs nothing
  -- here: Prompt.Shuffle deliberately carries no Decider (randomness is not a
  -- choice), so there is no player for it to be asked of.
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named ref) -> do
    gs <- State.get
    let targets = objectRefObjects legal resolving controller source gs ref
        -- The owners are read from the PRE-MOVE objects, for rule 701.24c's
        -- reason above; an id that no longer resolves contributes no owner, and
        -- the named library is what covers that case.
        owners =
          Set.fromList (Maybe.mapMaybe (\target -> fmap Object.owner (Game.lookupObject target gs)) targets)
            <> Set.fromList (foldMap (playerRefPlayers legal controller gs) named)
    Monad.forM_ targets $ \target -> Monad.void (Event.changeZoneReturning target Zone.Library)
    -- APNAP (CR 608.2f), which is what makes the ORDER of the Prompt.Shuffle
    -- calls a transcript can replay a fact about the rules rather than about
    -- PlayerId's Ord.
    Monad.forM_ (filter (`Set.member` owners) (Game.apnapOrder gs)) Mulligan.shuffleLibrary
  Effect.OfferCast (OfferCast.MkOfferCast slot offer) -> offerCast resolving controller slot offer
  -- CR 601.3: write the standing permission onto every object the ObjectRef
  -- names, as CR 109.5's "you" and the stated duration.
  --
  -- NOT gated on the object being in exile. CR 601.3's permissions are not
  -- zone-scoped, and Cast.castableZones consults this field only on its exile
  -- arm, so a zone test here would be the rules core deciding what the effect
  -- means rather than the effect saying it.
  Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile duration ref spending) ->
    State.modify' $ \gs ->
      -- The same sweep every ObjectRef-taking opcode shares: a player recipient,
      -- an illegal slot (CR 608.2b) and a set that matched nothing all arrive as
      -- the empty list and change nothing.
      case objectRefObjects legal resolving controller source gs ref of
        [] -> gs
        targets -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
          -- CR 611.2b: the duration never started, so the effect does nothing and
          -- no permission is stored -- not one a later sweep would take away.
          Nothing -> gs
          Just expiry ->
            let permission =
                  ExilePlayPermission.MkExilePlayPermission
                    { ExilePlayPermission.player = controller,
                      ExilePlayPermission.source = source,
                      ExilePlayPermission.expiry = expiry,
                      -- CR 118.14, carried from the opcode unread: this module
                      -- stores what the card said and Pawl.Engine.Mana is the
                      -- only thing that acts on it.
                      ExilePlayPermission.spending = spending
                    }
                grant o = o {Object.playableFromExile = Just permission}
             in gs {GameState.objects = foldr (Map.adjust grant) (GameState.objects gs) targets}
  Effect.ForEach (ForEach.MkForEach ref slot body) -> do
    gs0 <- State.get
    -- CR 608.2f's first half, and the same read every set-naming opcode makes:
    -- WHICH members, swept ONCE from the pre-loop board and then fixed. A
    -- library the body empties therefore cannot shorten the batch, and a
    -- permanent the body makes matching cannot join it.
    --
    -- Recipients rather than objects, DealDamage's ref reader: rule 608.2f's own
    -- sentence is about "players and/or objects", and Soulfire Eruption's
    -- targets are both.
    members <- forEachOrder resolving controller (objectRefRecipients legal resolving controller source gs0 ref)
    let -- The slots the BODY defines, computed off the instruction rather than
        -- read off the board: a body effect binds into the RESOLVING object's
        -- live bindings (MoveToZone's CR 400.7 arrival), and the next effect of
        -- the same body has to see it. Restricted to those names so that a
        -- target slot cannot come back in under the instance name CR 700.2d
        -- renamed it away from.
        bodyDefined = foldMap boundSlots body
        -- The member is bound HERE, in the map handed down, and never onto the
        -- resolving object -- which is what scopes it to this iteration without
        -- anything to undo afterwards. The insert is OUTERMOST, so the loop's
        -- own name wins over both other sources.
        --
        -- `m` beats `defined` where the two collide: `m` is the CR 608.2b
        -- re-validated map, and a body definition shadowing a target slot would
        -- skip a re-validation that slot was owed. The collision is ruled out by
        -- Pawl.CardSpec rather than by the types, exactly as slotGroup's is, so
        -- this is which way to fail rather than a live choice.
        withMember member defined m = Map.insert slot (Set.singleton member) (Map.union m defined)
        bindingsOf gs = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)
        -- Whatever those names held BEFORE the loop, to be put back at each
        -- iteration's start and once at the end.
        beforeLoop = Map.restrictKeys (bindingsOf gs0) bodyDefined
        -- The other half of the scoping, and it is not tidiness: an iteration
        -- whose MoveToZone found an empty library binds nothing, and without
        -- this its DealDamage would read the card the PREVIOUS member's
        -- iteration exiled -- "that card" naming a card this pass never
        -- produced. Restoring rather than merely deleting keeps the rest of the
        -- resolution reading the environment it had.
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
      -- CR 608.2c: the body's own instructions, in written order, once for this
      -- member before the next member is considered at all.
      Monad.forM_ body $ \eff -> do
        defined <- State.gets (\gs -> Map.restrictKeys (Binding.targetsOf (bindingsOf gs)) bodyDefined)
        applyEffectWith runSubgame resolving source controller (withMember member defined legal) (withMember member defined chosen) eff
    State.modify' rescope
  Effect.Draw (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- Whoever the PlayerRef names draws -- the controller for Divination's
        -- `Relative You`, the targeted player for Ancestral Recall's `InSlot`,
        -- each opponent for Master of the Feast's `Relative Opponent`, the whole
        -- table for Vision Skeins' `EachPlayer`.
        named = playerRefPlayers legal controller gs ref
        -- CR 121.2c: the active player draws first, then each other player in turn
        -- order. The reorder lives here rather than in playerRefPlayers because
        -- that is a rule about DRAWING, and the helper's other caller has no
        -- ordering rule to obey. Observable rather than cosmetic: each draw
        -- records a zone change in the log the trigger scan reads (CR 603.2). CR
        -- 121.2d has no reader -- pawl has no teams (#175).
        --
        -- An intersection: apnapOrder supplies the ORDER and `named` the
        -- MEMBERSHIP. The direction that bites is a seat apnapOrder names and
        -- `named` does not -- a departure does not shorten the seating roster (CR
        -- 800.4k/800.4m), while playerRefPlayers stops naming them at CR 102.1.
        -- Drawing for one was never a no-op: CR 800.4a took their library with
        -- them, so drawCard would write drewFromEmpty for a player not in the game.
        drawers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    -- PER DRAWER (evaluateForRecipient): Nature's Resurgence's "each player draws
    -- a card for each creature card in their graveyard" is a different number for
    -- each of them, and the draws do not disturb it -- every amount is read off
    -- the same pre-effect `gs`, so a seat drawing first cannot change what a later
    -- seat draws.
    Monad.forM_ drawers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 ->
              -- CR 121.2: draw n one at a time, folding the shared primitive so each
              -- draw re-reads the library top and the CR 104.3c empty-library loss is
              -- preserved.
              Monad.replicateM_ (Integer.toIntSaturating n) (Event.drawCard pid)
        _ -> pure ()
  Effect.Mill (Mill.MkMill ref quantity mTally) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- An illegal slot (CR 608.2b) or a reference naming nobody arrives here
        -- as the empty list and mills nothing, the posture every PlayerRef arm
        -- takes.
        millers = playerRefPlayers legal controller gs ref
        -- CR 701.17/701.17b: top min(n, library) of each miller's library. A
        -- short library mills what there is, which is why the tally below counts
        -- THESE cards rather than the number the quantity asked for.
        --
        -- The count is PER MILLER (evaluateForRecipient), off the one pre-effect
        -- `gs` every batch is taken from, so "half their library" would read each
        -- miller's own.
        milledBy =
          Maybe.mapMaybe
            ( \pid -> case evaluateForRecipient viewOf context gs resolving source pid quantity of
                Just n | n > 0 -> Just (pid, List.genericTake n (Game.zoneMembers Zone.Library pid gs))
                _ -> Nothing
            )
            millers
        milled = concatMap snd milledBy
    -- Funnelled so each move mints a new incarnation, and then recorded as the
    -- mill it was (CR 701.17a). The GameEvent.Milled entry is what a later
    -- effect's Filter.MilledThisTurn reads, and it carries the ids the funnel
    -- ANSWERED rather than the ones the cards had in the library: CR 400.7 has
    -- minted new ones, and CR 701.17c is the rule that sends a reader after the
    -- card in the zone it moved to. A move the CR 616.1 loop cancelled answers
    -- Nothing and is no card milled.
    --
    -- ONE entry per miller, holding that player's whole batch, because rule
    -- 701.17a mills them at once -- so a reader cannot mistake one instruction
    -- for several. Recorded even for a card a replacement diverted to another
    -- zone, which is that rule's own reading: the card was milled wherever it
    -- ended up.
    Monad.forM_ milledBy $ \(pid, cards) -> do
      arrived <- Maybe.catMaybes <$> Monad.mapM (\c -> Event.changeZoneReturning c Zone.Graveyard) cards
      Monad.unless (null arrived) (State.modify' (Event.recordEvent (GameEvent.Milled (Milled.MkMilled pid (Seq.fromList arrived)))))
    -- The tally, counted off the PRINTED card and read from the pre-move state
    -- because CR 400.7 has since minted new ids. Rule 728.1's "nonland" is a
    -- card-type question, which the printed face answers.
    --
    -- Not implemented: the milled card is an object and has a CR 613 projection
    -- of its own, so a tally keyed on an axis some effect changed reads the wrong
    -- number (#160). This reader's own choice rather than the zone's --
    -- Effect.Search's CR 701.23a filter, once the same reader, now takes
    -- Projection.viewOfObject.
    --
    -- Bound onto this effect's SOURCE, so a later effect of the same resolution
    -- reads it as Quantity.InSlot -- Destroy's "destroyed this way" binding
    -- exactly. Bound even at zero, for that arm's reason: zero is an answer,
    -- where an unbound slot leaves the reader unevaluable.
    --
    -- ONE number across every miller. Rule 728.1's mill names one player, and a
    -- per-player tally would need a per-player reader, which no Quantity has.
    Monad.forM_ mTally $ \tally ->
      let tallyContext = Filter.contextFor Nothing Nothing
          counted oid = case Game.faceOf oid gs of
            Nothing -> False
            Just face -> Filter.matches tallyContext (Projection.viewOfCardIn gs oid face) (MillTally.filter tally)
       in State.modify' (bindAmountSlot source (MillTally.slot tally) (Natural.length (filter counted milled)))
  -- CR 701.20a: show the named cards to every player. CR 701.20b keeps them
  -- where they are, so the GameEvent.Revealed the funnel appends IS the whole
  -- effect -- the rule, not a shortcut (Event.reveal's own haddock).
  --
  -- The SHOWER is this effect's controller, not each card's owner: rule 701.20a
  -- says "show that card", and the player carrying out the instruction is the
  -- one doing the showing. Every printing in the pool reveals a card the
  -- controller already holds, so the two readings coincide today.
  --
  -- RevealCause.Ordinary: rule 702.94a's "this way" is the miracle window's
  -- alone, and no rule asks again about a reveal a card's own sentence caused.
  --
  -- One reveal per named card rather than one for the batch, which is what
  -- GameEvent.Revealed's shape allows -- it carries a single ObjectId. Nothing
  -- in rule 701.20a makes a simultaneous reveal differ from a sequence of them,
  -- since nothing moves and nothing is decided in between.
  Effect.Reveal ref -> do
    gs <- State.get
    Monad.mapM_ (Event.reveal RevealCause.Ordinary controller) (objectRefObjects legal resolving controller source gs ref)
  Effect.LookAt (LookAt.MkLookAt ref slot) -> do
    gs <- State.get
    -- CR 608.2c: the cards are named as this instruction is reached, and CR
    -- 701.20b (reached by rule 701.20e) leaves every one of them where it is --
    -- so this whole arm is the binding, and an empty library binds nothing.
    --
    -- No prompt and no event. Rule 701.20e shows the cards to one player, which
    -- pawl has no way to do (#1412); what reaches the seat is the later clause's
    -- own question, and a public GameEvent.Revealed would be a different rule
    -- (CR 701.20a).
    case objectRefObjects legal resolving controller source gs ref of
      [] -> pure ()
      -- bindArrivals' one-versus-many line, minus its prompt: one card takes the
      -- SINGLE binding, which is the only one a Filter.IsBound can see, and is
      -- what every printing in the corpus looks at (#1532).
      [only] -> State.modify' (bindSlot resolving slot only)
      several -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList several))
  Effect.Scry (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- An illegal slot (CR 608.2b) or a reference naming nobody arrives here
        -- as the empty list and scries nothing, the posture every PlayerRef arm
        -- takes.
        named = playerRefPlayers legal controller gs ref
        -- CR 701.22c: players scrying at once decide in APNAP order. Draw's
        -- intersection and for that arm's reason -- apnapOrder supplies the
        -- ORDER, `named` the MEMBERSHIP. Each scryer's cards then move before
        -- the next is asked, rather than all of them moving together (#1340).
        scryers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    -- Per scryer, Draw's posture and for its reason: a card naming a number of
    -- the scryer's own would read each one's, and nothing else can tell the loop
    -- from a single evaluation.
    Monad.forM_ scryers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        -- CR 701.22b: scry 0 is not a scry at all, so a quantity of zero reaches
        -- no player and raises no prompt.
        Just n | n > 0 -> scryOne n pid
        _ -> pure ()
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- Scry's arm in every respect: the empty-list posture for a reference
        -- naming nobody, and APNAP for the order several surveillers are asked
        -- in (CR 101.4, rule 701.25 stating no order of its own).
        named = playerRefPlayers legal controller gs ref
        surveillers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    Monad.forM_ surveillers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        -- CR 701.25c: surveil 0 is not a surveil at all.
        Just n | n > 0 -> surveilOne n pid
        _ -> pure ()
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- The players who FATESEAL, not the ones fatesealed: rule 701.29a's
        -- subject is the looker, and whose library is looked at is the separate
        -- choice fatesealOne makes below.
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
    -- CR 608.2c: the set is swept as this instruction is reached, and an illegal
    -- slot (CR 608.2b) or a player recipient answers with nobody. CR 701.44d's
    -- APNAP half is objectRefObjects' own order; its second key is the engine's
    -- where that rule gives the choice to the controller (#1345).
    Monad.mapM_ exploreOne (objectRefObjects legal resolving controller source gs ref)
  Effect.Discard (Discard.MkDiscard slot quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
    case legalOne slot legal of
      Just (Recipient.ToPlayer target) ->
        -- One recipient, so the loop above is the identity here -- but the
        -- READING is the same one, so "discards cards equal to the number of
        -- creatures they control" would be answered against the discarding player
        -- rather than against the controller.
        case evaluateForRecipient viewOf context gs resolving source target quantity of
          Just n
            | n > 0 -> do
                let held = Game.zoneMembers Zone.Hand target gs
                    -- CR 701.9a's move, through the shared discard funnel, so
                    -- the discard is recorded for a rule 701.9a trigger to read
                    -- and not merely performed.
                    bury :: [ObjectId] -> Game ()
                    bury = Monad.mapM_ (Event.discard DiscardCause.Ordinary target)
                    -- The quantity as the count it is. `n > 0` above, so the
                    -- clamp never decides anything here.
                    count = Integer.toNaturalSaturating n
                if count >= Natural.length held
                  -- CR 609.3: discarding the whole hand is "as much as possible," so
                  -- it is forced -- no choice, so no prompt.
                  then bury held
                  else do
                    -- CR 701.9b: the discarding player chooses which cards.
                    let decider = Decide.deciderFor target gs
                    choices <- Game.choose (Prompt.ChooseDiscard decider target held count)
                    -- FILTERED AND COMPLETED, the posture PlayerSacrifices takes
                    -- below. Dropping the invalid picks is not enough: this branch
                    -- is reached only when the hand is LARGER than the count, so CR
                    -- 609.3's "as much as possible" does no work here and every
                    -- card the answer omits is one the player could have discarded.
                    -- Reject-not-repair is the COST path's option, available there
                    -- only because a cost may go unpaid.
                    --
                    -- Deduplicated as well as filtered, which PlayerSacrifices
                    -- gets for free from its Set-shaped answer: ChooseDiscard is
                    -- answered with a LIST, so a card named twice would otherwise
                    -- fill two of the n slots and discard one card too few.
                    --
                    -- `held` is longer than n and `valid <> filler` is a
                    -- permutation of it, so the take always yields exactly n.
                    let valid = List.nub (filter (\c -> elem c held) choices)
                        filler = filter (\c -> List.notElem c valid) held
                    bury (List.genericTake count (valid <> filler))
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- Whoever the PlayerRef names loses the life -- the targeted player for
        -- Sign in Blood's `InSlot`, the controller for a `Relative You`
        -- drawback. Unordered, on the footing GainPlayerCounters is on rather
        -- than Draw's: there is no CR 121.2c for life, and CR 704.3 checks
        -- state-based actions only as a player would get priority, so no life
        -- total is observable between one adjustment and the next.
        losers = playerRefPlayers legal controller gs ref
    -- PER PAYER, not once (evaluateForRecipient): Shahrazad's "each player who
    -- doesn't win the subgame loses half THEIR life, rounded up" reads each
    -- payer's own life total, and Stronghold Discipline's "1 life for each
    -- creature they control" each payer's own board.
    --
    -- Every payer's number is read off the SAME `gs`, so the amounts are the life
    -- totals as the effect began rather than as the previous payer left them --
    -- which is the unordered footing the comment above already rests on.
    Monad.forM_ losers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 ->
              -- CR 119.3: the life total is simply adjusted, directly on the
              -- player record. Not through Pawl.Engine.Damage: CR 119.2 makes damage a
              -- CAUSE of life loss, not a synonym for it (the opcode's own
              -- comment has the consequences). A direct subtraction is what
              -- CostComponent.PayLife does for CR 119.4's cost side, and the CR
              -- 704.5a state-based action that may follow is the existing one in
              -- Pawl.Engine.Sba.
              changeLife pid (negate n)
        _ -> pure ()
  -- CR 119.3's other half, LoseLife's mirror in every respect but the sign. The
  -- comments above apply verbatim: same `viewWithLastKnown` reading, same
  -- unordered adjustment, same direct write to the player record, the same CR
  -- 608.2i record appended alongside it, and the same per-gainer reading through
  -- evaluateForRecipient. One difference: nothing in CR 704.5 follows a gain --
  -- CR 704.5a fires on "0 or less life", which a gain cannot reach.
  --
  -- No card in the pool writes a gain whose amount is each gainer's own, so the
  -- per-gainer reading here is a regression fence rather than proven behaviour.
  -- Stronghold Discipline proves it one arm over, through the same funnel.
  --
  -- The `n > 0` guard is CR 119.9's in as many words: "if a player gains 0 life,
  -- no life gain event has occurred". Here it is load-bearing rather than tidy --
  -- a Quantity that evaluates to 0 (an X of nothing, a count of an empty board)
  -- must leave the log silent, or "whenever you gain life" would fire on it.
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        gainers = playerRefPlayers legal controller gs ref
    Monad.forM_ gainers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 -> changeLife pid n
        _ -> pure ()
  -- CR 701.12c: the two sides reach each other's PREVIOUS total, so both deltas
  -- are read off the same game state before either is written -- a sequential
  -- "set this one to that one's, then that one to this one's" would leave both on
  -- one total.
  --
  -- Written as a gain and a loss rather than as two assignments, which is the
  -- rule's own wording ("each player gains or loses the amount of life
  -- necessary"): that is what puts a LifeGained and a LifeLost in the log for a
  -- "whenever you gain life" trigger to read. Equal totals move nobody, and
  -- changeLife's zero case is what keeps the log silent then.
  --
  -- Which two players are the sides comes from the ExchangeSides: the controller
  -- and one target (Mirror Universe), or the two players one instance of the word
  -- "target" named (Soul Conduit, CR 601.2c -- which is also what makes them
  -- distinct, so nothing here has to check that). Order does not matter, the
  -- exchange being symmetric, which is why an unordered SET of recipients is
  -- enough to name both sides.
  --
  -- Not implemented: CR 701.12c's deferral to CR 119.7-8, under which an
  -- exchange that would raise a player who can't gain life (or lower one who
  -- can't lose life) doesn't happen. Vacuous rather than elided: no effect in
  -- the pool stops a player gaining or losing life, Pawl.Types.PlayerEffect
  -- having no such arm to consult.
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
        changeLife this (thatLife - thisLife)
        changeLife that (thisLife - thatLife)
      -- Not a player recipient, an illegal slot (CR 608.2b), or a BetweenTargets
      -- left with anything but its two sides: no-op, and CR 701.12a agrees --
      -- if the entire exchange can't be completed, no part of it occurs.
      Nothing -> pure ()
  -- CR 119.5: each named player "gains or loses the necessary amount of life to
  -- end up with the new total". So this is a DELTA per player, computed against
  -- that player's own current total -- one seat may gain while another loses on
  -- the same resolution, which is why a card cannot spell this with a GainLife or
  -- a LoseLife of its own: neither can name an amount it has to subtract a live
  -- life total to find.
  --
  -- Written through changeLife exactly as ExchangeLifeTotals is, and for CR
  -- 119.5's sake rather than for tidiness: a raw write to Player.life would leave
  -- the log silent, and "whenever you gain life" is supposed to see this. The
  -- rule spends its whole sentence saying so.
  --
  -- The total is evaluated ONCE PER RECIPIENT, because a card may name a number
  -- that is each recipient's own: Biorhythm's "each player's life total becomes
  -- the number of creatures THEY control" gives three seats three answers, where
  -- Magister Sphinx's literal and Arbiter of Knollridge's fold give one number to
  -- the whole table. Which recipient the evaluation has reached rides in
  -- Filter.Context's `recipient`, and Filter.ControlledByRecipient is the one atom
  -- that reads it; a quantity naming no such atom cannot tell the loop from a
  -- single evaluation. Through evaluateForRecipient, which is where that reading
  -- now lives -- this arm was the only one that had it.
  --
  -- Every evaluation and every delta is read off `gs`, the state BEFORE any life
  -- moves (CR 608.2f) -- ExchangeLifeTotals' posture -- so the deltas cannot see
  -- each other and a fold over life totals answers the same for every seat. Still
  -- a REGRESSION FENCE rather than proven behaviour, as it was before the loop
  -- went per-recipient: Biorhythm counts creatures, which this effect does not
  -- move, and Arbiter's maximum is where it was after the first seat reaches it.
  -- A card folding a MINIMUM over the same players is what would tell them apart.
  --
  -- changeLife's zero case is CR 119.9's, and here it is the load-bearing one:
  -- Arbiter's own highest-life seat is already at the new total, so it gains
  -- nothing and no life-gain trigger may fire for it.
  --
  -- CR 104.3b's state-based action follows a total driven to 0 or less, and it is
  -- the existing one in Pawl.Engine.Sba -- reached through changeLife's ordinary
  -- subtraction, with nothing here to arrange. Biorhythm's seat controlling no
  -- creature is the producer that gets there.
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        recipients = playerRefPlayers legal controller gs ref
    Monad.forM_ recipients $ \pid ->
      -- A player with no row is nobody to move, which is the same no-op an
      -- unfilled slot already gave; the lookup is what makes the delta a delta, so
      -- there is nothing to fall back to.
      Monad.forM_ (Map.lookup pid (GameState.players gs)) $ \player ->
        -- An undeterminable total is no instruction at all, LoseLife's and
        -- GainLife's posture for the same reason: there is no number to reach.
        -- Asked per recipient rather than for the effect as a whole, which is what
        -- a per-recipient number means -- one seat's unanswerable count leaves
        -- that seat alone and says nothing about the others.
        Monad.forM_ (evaluateForRecipient viewOf context gs resolving source pid quantity) $ \total ->
          changeLife pid (total - Player.life player)
  -- Reverse the Sands: "Redistribute any number of players' life totals. (Each of
  -- those players gets one life total back.)" CR 119.7 and CR 119.8 name the
  -- action, and every seat's new total is CR 119.5's gain or loss of the
  -- necessary amount -- SetLifeTotal's arm above, once per recipient, through the
  -- same changeLife.
  --
  -- The roster is CR 102.1's players IN the game (Game.stillPlaying), not the keys
  -- of GameState.players, which keep a departed seat's row -- Proliferate's arm
  -- makes the same distinction. Offering a seat that left would hand a live
  -- player a dead one's total.
  --
  -- Every total is read ONCE, before the prompt and before any life moves (CR
  -- 608.2h), and every delta is computed against that snapshot. Reading a total
  -- this effect had already overwritten is the bug a permutation makes
  -- unmissable: a rotation would otherwise leave two seats on one number.
  --
  -- FILTERED, NOT TRUSTED (#222), but all-or-nothing rather than per entry: only
  -- a whole permutation is a legal answer, so there is no honest way to keep part
  -- of a bad one. An answer that names a non-candidate, drops a player it hands a
  -- total to, or gives two players the same total is refused entire, and the
  -- fallback is the answer that is always legal -- redistribute among nobody.
  -- Refusing whole is what makes "you can't split up a life total" hold:
  -- honouring {alice -> carol} alone would leave carol's total on two seats.
  --
  -- Not implemented: CR 119.7-8's own restrictions, under which a player who
  -- can't gain life may not be given a higher total (and the mirror for losing).
  -- Vacuous rather than elided, ExchangeLifeTotals' position for the same reason:
  -- nothing in the pool stops a player gaining or losing life, Pawl.Types.
  -- PlayerEffect having no such arm to consult.
  --
  -- CR 810.9f's "not more than one member of each team" is not expressed either,
  -- pawl having no teams (#175).
  Effect.RedistributeLifeTotals -> do
    gs <- State.get
    let candidates = Game.stillPlaying gs
        lifeOf pid = maybe 0 Player.life (Map.lookup pid (GameState.players gs))
        offered = fmap (\pid -> (pid, lifeOf pid)) candidates
    -- One candidate leaves only the identity, and no candidates leave not even
    -- that: either way every assignment is the same assignment, so there is
    -- nothing to ask.
    Monad.when (length candidates > 1) $ do
      assignment <- Game.choose (Prompt.ChooseRedistribution (Decide.deciderFor controller gs) controller offered)
      let takers = Map.keysSet assignment
          givers = Set.fromList (Map.elems assignment)
          -- A permutation of the chosen subset: the keys are candidates, and the
          -- totals handed out are exactly the takers' own. Set equality also
          -- settles injectivity, a Map's keys being distinct -- a repeated giver
          -- would make `givers` smaller than `takers` and fail here.
          isPermutation = Set.isSubsetOf takers (Set.fromList candidates) && takers == givers
      Monad.when isPermutation . Monad.forM_ (Map.toList assignment) $ \(taker, giver) ->
        changeLife taker (lifeOf giver - lifeOf taker)
  -- CR 702.179c: each named player's speed increases by this much. Pawl.Engine.
  -- Speed's inherent triggered ability (CR 702.179d) is one producer and card data
  -- is the other, so the PlayerRef is genuinely read rather than known --
  -- Pawl.SpeedSpec's CardIncrease group moves a player who is not the effect's
  -- controller, which is what proves it.
  --
  -- The two readings CR 702.179c distinguishes -- a player who HAS speed, whose
  -- speed goes up by the value, and a player who has NONE, whose speed BECOMES
  -- the value -- are spelled separately here even though they coincide
  -- arithmetically against a stand-in zero. They are separate in the rule, and
  -- the DecreaseSpeed arm below is what a stand-in zero here would have got
  -- wrong: a decrease must leave a player with no speed still without one. Only a
  -- card can reach the second reading at all -- rule 702.179d's ability exists
  -- only for a player who already has 1 or more speed.
  --
  -- No cap is applied. Nothing in rule 702.179 bounds speed from above, and a cap
  -- here would be a rule pawl invented; what keeps rule 702.179d's own climb at 4
  -- is its "if your speed is less than 4", checked when it triggers (CR 603.4) and
  -- again as it resolves (CR 608.2a) -- Pawl.Engine.Stack's inherent-trigger arm.
  -- Whether an EFFECT may push past 4, having no such gate, is unsettled (#809).
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
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
  -- CR 702.179 / Spikeshell Harrier: each named player's speed drops by this
  -- much, never below the floor the CARD prints. The mirror of the arm above, and
  -- deliberately NOT its negation:
  --
  --   * A player with NO SPEED (CR 702.179b) stays that way. CR 702.179c gives an
  --     INCREASE the power to create a speed out of nothing ("their speed becomes
  --     that value") and no rule says the same of a decrease, so the Maybe is
  --     traversed rather than defaulted. CR 702.179f's stand-in 0 is a READING an
  --     effect makes of such a player, not a value they have.
  --   * The floor is the card's own sentence, so it comes off the payload. Rule
  --     702.179 states no floor of its own, and a player at the floor already is
  --     left exactly where they are rather than moved to it.
  --
  -- The REFERENCE is resolved against the CHOSEN slots where every sibling arm
  -- reads the legal ones, and gateHolds carries the argument: this effect names
  -- the controller of a permanent the preceding clause has already returned to a
  -- hand, which CR 608.2b's filter drops, and CR 608.2h rather than CR 608.2b is
  -- the rule about a target THIS resolution moved. The AMOUNT's context stays on
  -- the legal slots, which is every other arm's posture and what CR 608.2b does
  -- govern.
  --
  -- Not implemented: no OTHER opcode passes the chosen slots, so a card writing
  -- PlayerRef.ControllerOfBound in one of their references would lose the player
  -- once the object moved (#1441).
  Effect.DecreaseSpeed d -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        slowing = playerRefPlayers chosen controller gs (SpeedDecrease.player d)
        atLeast = toInteger (SpeedDecrease.floor d)
    Monad.forM_ slowing $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid (SpeedDecrease.quantity d) of
        Just n
          | n > 0 ->
              let slower p = p {Player.speed = fmap (\was -> Integer.toNaturalSaturating (max atLeast (toInteger was - n))) (Player.speed p)}
               in State.modify' (\g -> g {GameState.players = Map.adjust slower pid (GameState.players g)})
        _ -> pure ()
  -- CR 701.21a: the slot's target player sacrifices `quantity` permanents
  -- matching the filter, and THAT PLAYER chooses which -- the whole difference
  -- between this and Sacrifice above.
  --
  -- CR 609.3: with no more candidates than the count, every one of them goes and
  -- there is nothing to ask; with none, nothing happens. Only a genuine surplus
  -- raises the prompt, which is the same shape Cost's Sacrifice component takes.
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot filter_ quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
    case legalOne slot legal of
      Just (Recipient.ToPlayer victim) ->
        -- Read against the VICTIM (evaluateForRecipient), Discard's posture and
        -- for its reason: one recipient makes the loop the identity, but "half the
        -- permanents they control" is still a number of the sacrificing player's
        -- own rather than the controller's.
        case evaluateForRecipient viewOf context gs resolving source victim quantity of
          Just n
            | n > 0 -> do
                -- Candidates are what the VICTIM controls, ascending, so both the
                -- elision and a short transcript are deterministic. No perspective
                -- and no source on the filter context: an edict's filter names a
                -- quality, never a player, and CR 601.2c's "another" is not a word
                -- an edict prints.
                --
                -- Through Replacement.sacrificeCandidates rather than an inline
                -- match, which is what puts CR 101.2's "can't be sacrificed" on
                -- this path too (Garland, Royal Kidnapper): a prohibited permanent
                -- is not merely unsacrificeable but is never the pick that
                -- satisfies the edict, so a victim controlling one prohibited and
                -- one ordinary creature loses the ordinary one. One home for CR
                -- 701.21a's candidate question is what that function's header
                -- already asks for (#111).
                let candidates = Replacement.sacrificeCandidates victim Nothing filter_ gs
                    decider = Decide.deciderFor victim gs
                    -- The quantity as the count it is. `n > 0` above, so the
                    -- clamp never decides anything here.
                    count = Integer.toNaturalSaturating n
                picked <-
                  if Natural.length candidates <= count
                    then pure (Set.fromList candidates)
                    else Game.choose (Prompt.ChooseSacrifices decider victim source candidates count)
                -- FILTERED AND COMPLETED, not merely filtered. Dropping the
                -- invalid picks is not enough: Diabolic Edict is not "may", so an
                -- interpreter answering with too few -- or with nothing -- would
                -- otherwise sacrifice fewer permanents than the effect demands and
                -- cheat the edict. CR 609.3 caps this at "as much as possible",
                -- which is every candidate, not however many the answer named.
                --
                -- So the valid picks are honoured first and the rest is made up
                -- deterministically from the remaining candidates, in the order
                -- they were offered. That differs from the cost path's
                -- reject-not-repair on purpose: a cost may simply go unpaid, and
                -- an effect has no such out.
                let wanted = min count (Natural.length candidates)
                    valid = filter (\oid -> Set.member oid picked) candidates
                    filler = filter (\oid -> List.notElem oid valid) candidates
                Monad.mapM_ (Event.sacrifice victim) (List.genericTake wanted (valid <> filler))
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.Create (Create.MkCreate quantity card entry mSlot) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Just n
        | n > 0 -> do
            -- CR 111: create n tokens with these characteristics under the
            -- effect's controller (CR 111.2), through the single funnel -- so CR
            -- 614's token replacements (Doubling Season) get their opportunity.
            -- CR 110.5b: the funnel is handed the entry's tap state, so a token
            -- the effect says is tapped is never untapped for an instant. CR
            -- 122.6a's counters ride along for the same reason -- incubate's token
            -- is never on the battlefield without them (CR 701.53a) -- and the
            -- funnel places them through CR 122.6's own door, so a counter
            -- replacement reaches them (Pawl.ReplacementSpec's Eyes of Gitaxias
            -- group is the proof).
            minted <- Event.createTokens controller (bakeTokenCharacteristics (Quantity.evaluateFor viewOf context gs resolving source) card) Nothing (Integer.toNaturalSaturating n) (EntryRiders.tapped entry) (EntryRiders.counters entry)
            -- CR 508.4: "if a creature is put onto the battlefield attacking, its
            -- controller chooses which defending player ... it's attacking". The
            -- rules for that live in Pawl.Engine.Combat, which is also what keeps this
            -- from looking like a declaration -- CR 508.3a's attack triggers see
            -- nothing (Pawl.Engine.Combat.putOntoBattlefieldAttacking's own comment).
            --
            -- After the entry loops rather than inside them: CR 614.16's token
            -- replacement settles the COUNT first, so this joins the tokens that
            -- actually entered, however many that turned out to be.
            Monad.when (EntryRiders.attacking entry) (Monad.mapM_ Combat.putOntoBattlefieldAttacking minted)
            case (mSlot, namesEveryToken quantity, minted) of
              (Nothing, _, _) -> pure ()
              -- Unreachable: createTokens places every token onto the battlefield
              -- (CR 111.2). Total rather than partial: nothing bound matches "the
              -- token was never named" instead of crashing.
              (Just _, _, []) -> pure ()
              -- The card says "those tokens", so the slot holds EVERY token
              -- this Create minted (CR 111.1: each is its own object). Nothing
              -- to ask, whatever CR 614.16 did to the count -- a replacement
              -- that doubled the create just makes the group bigger, and the
              -- plural word still names all of it. CR 603.7c is what the binding
              -- is FOR: a delayed ability armed by this same resolution refers
              -- to these particular objects. Thatcher Revolt is the producer.
              (Just slot, True, _) -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList minted))
              -- CR 603.7c: bind the minted token into live Object.bindings so a
              -- delayed ability THIS SAME resolution arms can name it. One token
              -- is the whole candidate list, so there is nothing to ask -- and
              -- where the rules leave nothing to ask, don't prompt.
              (Just slot, False, [only]) -> State.modify' (bindSlot resolving slot only)
              -- CR 614.16 got there first: a token replacement (Doubling Season)
              -- multiplied the count at RESOLUTION, so several tokens now stand
              -- where CR 603.7c's "it" names one PARTICULAR object. CR 707.10e is
              -- the codified analogue -- when a replacement makes a copy target
              -- more than one object, "the copy's controller chooses one of them"
              -- -- so this asks rather than picking the first, which would be the
              -- engine choosing. The plural arm above never reaches here: a card
              -- that says "them" wants all of them and asks nothing.
              --
              -- FILTERED, NOT TRUSTED, the same posture Sba.chooseLegendVictims
              -- takes: an answer naming something that was not minted falls back
              -- to the first, since the slot must end up bound either way.
              (Just slot, False, first : second : rest) -> do
                gs1 <- State.get
                let candidates = first NonEmpty.:| (second : rest)
                    decider = Decide.deciderFor controller gs1
                answer <- Game.choose (Prompt.ChooseBoundToken decider controller source candidates)
                let named = if List.elem answer (NonEmpty.toList candidates) then answer else first
                State.modify' (bindSlot resolving slot named)
      _ -> pure ()
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref) -> do
    gs <- State.get
    -- CR 707.2 / 111.3: this many tokens per named permanent, minted through the same
    -- CR 111.2 funnel a given-text token uses, carrying the copied permanent's
    -- COPIABLE values (Event.copiedSnapshot) rather than its projection -- CR
    -- 707.2 copies neither counters nor other effects. All of it read off ONE `gs`,
    -- which is CR 608.2f: whichever permanents the ref names are snapshotted
    -- before any token exists to disturb them.
    --
    -- The token's own card is the copied permanent's, so that a reader going
    -- past the projection to Game.faceOf sees the same text the snapshot does.
    -- It is not what the token's characteristics come FROM: the snapshot is
    -- layer 1 (CR 613.1a), and Projection.copiableCharacteristics stops there.
    --
    -- CR 608.2h: both reads take the LAST KNOWN branch for a named permanent
    -- that is already gone, so the token is a copy of what it last was --
    -- Watchful Radstag killed in response to its own "create a token that's a
    -- copy of it". The pair has to move together: the card without the snapshot
    -- would mint the printed card rather than the copy, and the snapshot without
    -- the card would mint nothing to stamp it on.
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        sources = objectRefObjects legal resolving controller source gs ref
    -- The count is Create's, read the same way and off the same `gs`: kicked
    -- Rite of Replication's five (CR 707.1). An unevaluable or non-positive
    -- count mints nothing, which is Create's arm's posture one rule over.
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Just n
        | n > 0 ->
            Monad.forM_ sources $ \src ->
              Monad.forM_ (Game.cardOfWithLastKnown src gs) $ \card ->
                -- No riders: CR 707.2 copies no counters, and Effect.CreateCopy (CreateCopy.MkCreateCopy
                -- carries) nothing for an effect to add any with.
                --
                -- Not implemented: a copy token an effect says enters with counters on it
                -- (Ochre Jelly, Littjara Mirrorlake) arrives bare (#1255).
                --
                -- ONE call per named permanent, with the whole count: CR 614.12's
                -- entry loop is handed the batch, so five copies of one creature
                -- enter simultaneously and none of them may copy a sibling.
                Monad.void (Event.createTokens controller card (Just (Event.copiedSnapshotWithLastKnown src gs)) (Integer.toNaturalSaturating n) TapState.Untapped Map.empty)
      _ -> pure ()
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name onset duration) -> do
    gs <- State.get
    -- CR 608.2h's last-known fallback, and NOT belt and braces: the source can
    -- have left the battlefield an opcode earlier in this same list -- Meandering
    -- Towershell's "exile it. Return it ..." -- and CR 400.7 has already deleted
    -- the id `source` names by the time this runs.
    -- CR 603.7 again, from the other kind of arming ability: a rule 702 keyword
    -- has no card text to declare the far end in, so a name a minted ability arms
    -- is on no face and resolves against rule 702's own roster instead
    -- (Keyword.mintedDelayedAbilities, decayed). Card data first, and the two
    -- namespaces are kept disjoint by Pawl.CardSpec, so the order is immaterial.
    case (Game.faceOfWithLastKnown source gs >>= (Map.lookup name . Face.delayedAbilities)) <|> Keyword.mintedDelayedAbility name of
      -- For a CARD's name the dataflow lint makes a dangling one a failing test,
      -- never a silent no-op, and this arm only keeps the executor total. A
      -- MINTED name has no such lint -- nothing enumerates rule 702's arms -- so
      -- a keyword whose roster row was forgotten lands here and does nothing,
      -- which is what that keyword's own gameplay test is for.
      Nothing -> pure ()
      Just ability ->
        -- CR 603.7d-f: the controller is the player who controlled the spell or
        -- ability AS IT RESOLVED -- `controller`, baked in now. CR 603.7a: an
        -- entry appended here can only ever match events at or after the current
        -- watermark, so it never fires on an event that already happened.
        let captured = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)
            entry =
              DelayedTrigger.MkDelayedTrigger
                { DelayedTrigger.ability = ability,
                  DelayedTrigger.source = source,
                  DelayedTrigger.controller = controller,
                  DelayedTrigger.bindings = captured,
                  -- CR 603.7a's other end, from Pawl.Types.Onset: no turn
                  -- restriction for an ability armed the moment it is created
                  -- (every card but one), and the BOUNDARY -- not a turn number
                  -- -- for one printed "on your next turn". Nothing about the
                  -- live board is read: which turn that phrase names is settled
                  -- as that turn begins (Event.settleOnsets), because an
                  -- intervening extra or skipped turn can still move it.
                  DelayedTrigger.window = Event.armOnset onset,
                  -- CR 603.7b's stated duration, armed the way a continuous
                  -- effect's is. The two Maybes meet here and mean different
                  -- things: the OUTER one is the card printing no duration at
                  -- all, and the inner one is Expiry.arm reporting that a
                  -- printed duration never STARTED (CR 611.2b's "if the 'for as
                  -- long as' duration never starts, the effect does nothing").
                  -- Both collapse to Nothing, and both should: an ability whose
                  -- stated duration never began has no duration to outlive its
                  -- first firing, so CR 603.7b's default is exactly right for it.
                  DelayedTrigger.expiry = duration >>= \d -> Expiry.arm (Binding.playersIn legal) controller source d gs
                }
         in State.put gs {GameState.delayedTriggers = GameState.delayedTriggers gs Seq.|> entry}
  Effect.Replace (Replace.MkReplace duration uses origin condition re) ->
    -- CR 614.3 / 615.3: install the floating replacement; Pawl.Engine.Replacement
    -- consults it at every funnel until cleanup drops it (CR 514.2) or its use is
    -- spent. Targetless and unprompted. CR 113.7: the SOURCE is this effect's
    -- source, which together with the timestamp is the row's CR 614.5 identity --
    -- what a "prevented this way" trigger would have to be keyed on (#687).
    --
    -- CR 614.15: the ORIGIN travels with the row rather than being re-derived,
    -- because it is a fact about the ability that wrote it and nothing on the
    -- board still says so once the resolution is over.
    State.modify' $ \gs ->
      -- The clause's own "if" -- Galvanic Blast's "if you control three or more
      -- artifacts". Read with the resolution's controller as CR 109.5's "you"
      -- and this effect's source as the object, the same pair
      -- Pawl.Engine.Expiry.arm reads a CR 611.2b duration with. The full view,
      -- not viewWithLastKnown: a spell creating a self-replacement is on the
      -- stack and the board is live, unlike CR 603.4's intervening "if" on a
      -- leaves-the-battlefield trigger.
      let met = maybe True (Condition.holds (Projection.fullView gs) (effectContext controller source legal) gs source) condition
       in case (met, Expiry.arm (Binding.playersIn legal) controller source duration gs) of
            -- The stated condition is false, so the clause creates nothing.
            (False, _) -> gs
            -- CR 611.2b: the duration never started, so no floating replacement
            -- is installed.
            (_, Nothing) -> gs
            (True, Just expiry) ->
              let (ts, gs1) = Game.freshTimestamp gs
                  active =
                    ActiveReplacement.MkActiveReplacement
                      { ActiveReplacement.effect = re,
                        ActiveReplacement.source = source,
                        -- CR 109.5: the resolution's controller, BAKED now --
                        -- the source is a spell CR 608.2n is about to put in a
                        -- graveyard, so it will have no controller to project
                        -- by the time the row is consulted. Gather Specimens'
                        -- "an opponent's" and "your" are both read off this.
                        ActiveReplacement.controller = controller,
                        ActiveReplacement.timestamp = ts,
                        ActiveReplacement.expiry = expiry,
                        ActiveReplacement.uses = uses,
                        ActiveReplacement.origin = origin,
                        ActiveReplacement.rider = Nothing
                      }
               in gs1 {GameState.replacements = active : GameState.replacements gs1}
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration ref quantity riderEffects) -> do
    -- CR 615.3 / 615.7: install one floating prevention shield per recipient the
    -- ref names -- "Prevent the next 4 damage that would be dealt to any target
    -- this turn" (Mending Hands). Pawl.Engine.Replacement consults it at the damage
    -- funnel until the shield is spent (CR 615.7's "reduced to 0") or the
    -- duration expires (CR 615.3's other terminator).
    --
    -- Its own opcode rather than an Effect.Replace carrying a DamageR, for the
    -- reason Effect.PreventNextDamage's own comment gives: the pattern has to
    -- name the shielded permanent or player by id, which card data cannot. Everything
    -- ELSE about the row is Replace's -- Resolve bakes the source (CR 113.7),
    -- CR 109.5's controller and a fresh timestamp the same way.
    --
    -- Through Damage.damageRecipient, so the baked recipient is in the same
    -- vocabulary a DamageEvent's target arrives in: CR 120.1a's "damage can't be
    -- dealt to an object that's not a battle, a creature, or a planeswalker"
    -- means a generically-named permanent that is neither is not shieldable
    -- either, and a shield installed over it could never match anything.
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
        -- CR 615.5: the additional effect, BAKED onto the row with the
        -- environment it will need -- this resolution's chosen targets, which is
        -- what lets "that creature" still name something once CR 400.7 has
        -- replaced the spell, and CR 109.5's "you". Nothing when the card wrote
        -- no such clause, so an ordinary shield carries no payload at all.
        rider =
          if Seq.null riderEffects
            then Nothing
            else
              Just
                PreventionRider.MkPreventionRider
                  { PreventionRider.effects = riderEffects,
                    PreventionRider.targets = chosen,
                    PreventionRider.controller = controller
                  }
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      -- An unevaluable quantity is a no-op, the powerOf posture DealDamage takes.
      Nothing -> pure ()
      Just n ->
        -- CR 615.7: a shield of 0 can prevent nothing, so none is installed --
        -- the same state `setShield` leaves a spent one in.
        Monad.when (n > 0) . State.modify' $ \g0 ->
          let amount = Integer.toNaturalSaturating n
           in List.foldl' (installDamageRow (Binding.playersIn legal) controller source duration Nothing (DamageRewrite.PreventNext amount) rider) g0 recipients
  Effect.PreventAllDamage (DurationRef.MkDurationRef duration ref) -> do
    -- CR 615.1 / 615.3: install one floating prevention shield per recipient the
    -- ref names, with no amount to count down -- "prevent all damage that would
    -- be dealt to you this turn" (Selfless Squire). The row is
    -- PreventNextDamage's above in every respect but its rewrite, which is why
    -- both go through `installDamageRow`; what differs is that CR 615.7's "reduced
    -- to 0" terminator does not exist here, so only the duration ends it.
    --
    -- Through Damage.damageRecipient for PreventNextDamage's reason (CR 120.1a).
    gs <- State.get
    let recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
    State.modify' $ \g0 -> List.foldl' (installDamageRow (Binding.playersIn legal) controller source duration Nothing DamageRewrite.PreventAll Nothing) g0 recipients
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration kind srcRef destRef) -> do
    -- CR 614.9: install a floating redirection effect -- Turn the Tables' "all
    -- combat damage that would be dealt to you this turn is dealt to target
    -- attacking creature instead". BOTH sides are baked here, because both are
    -- known only at resolution and card data can name neither: the source side
    -- into DamagePattern.whichRecipient, the destination into the rewrite.
    --
    -- Both through Damage.damageRecipient for PreventNextDamage's reason (CR
    -- 120.1a). The rule's own guard is re-asked at redirect time, in
    -- Event.apply, because the destination may leave between now and then.
    gs <- State.get
    let recipientsOf ref = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
    -- EXACTLY ONE destination: CR 614.9 replaces damage to one thing with the
    -- same damage to ANOTHER one thing, and no printed redirect names more than
    -- one. None means CR 608.2b's target is already gone, so the effect does
    -- nothing and no row is installed.
    case recipientsOf destRef of
      [dest] ->
        State.modify' $ \g0 -> List.foldl' (installDamageRow (Binding.playersIn legal) controller source duration kind (DamageRewrite.Redirect dest) Nothing) g0 (recipientsOf srcRef)
      _ -> pure ()
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase ref selector) -> do
    -- CR 614.1b: "effects that use the word 'skip' are replacement effects", so
    -- this installs one -- floating, because a sorcery's skip outlives the
    -- sorcery (CR 614.3: floating replacements "last until they're used up").
    --
    -- CR 614.10a: one instance PER NAMED PLAYER, each with Uses.Once and each
    -- prepended as its own row rather than merged. That is where "skip the next
    -- two" comes from -- two rows with two timestamps, and Replacement.consume
    -- spends exactly the one it applied.
    --
    -- Expiry.Never, and no Duration on the opcode to derive anything else from:
    -- Fatigue states no duration, so CR 614.3's other terminator ("or their
    -- duration has expired") never fires and the skip waits however many turns it
    -- must. That is CR 614.10a's own answer for a "next" that has not come round
    -- yet -- "the other will remain until another occurrence can be skipped".
    --
    -- CR 113.7: the SOURCE is this effect's source, as Replace's is.
    --
    -- The PhaseSelector goes in untouched: whether it names one step or a whole
    -- phase (CR 500.1) is card data, and only Pawl.Engine.Replacement's comparison and
    -- Pawl.Engine.Engine's boundary question read it.
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
                    -- CR 109.5: the resolution's controller. Not the same
                    -- player as `pid` above, which the effect NAMED (Fatigue's
                    -- "target player"); nothing reads this, since a PhaseR
                    -- resolves no ControllerRelation.
                    ActiveReplacement.controller = controller,
                    ActiveReplacement.timestamp = ts,
                    ActiveReplacement.expiry = Expiry.Type.Never,
                    ActiveReplacement.uses = Uses.Once,
                    ActiveReplacement.origin = ReplacementOrigin.Other,
                    ActiveReplacement.rider = Nothing
                  }
           in g1 {GameState.replacements = active : GameState.replacements g1}
    State.modify' (\g -> List.foldl' (flip install) g named)
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration affected playerEffect) ->
    -- CR 611.1 / 613.11: install the stored player effect. Unprompted, and
    -- targetless except through the AffectedPlayers below. CR 109.5: the
    -- CONTROLLER is baked in now, because the source will not have one to project
    -- once it leaves the stack (Silence is an instant). A SCOPED set is not: CR
    -- 611.2c makes a rules-modifying effect one that "can affect objects that
    -- weren't affected when that continuous effect began", so it is re-resolved
    -- on every read.
    --
    -- A NAMED set is baked here instead, because the seat came from a target slot
    -- (CR 601.2c) and the bindings that answer it are gone once this resolution
    -- is over -- the move Expiry.arm already makes for CR 611.2b's condition. One
    -- stored row per player the slot names, RequireBlock's shape: an unfilled or
    -- illegal slot names none and stores nothing, which is CR 608.2b's "if the
    -- spell or ability creates any continuous effects that affect game rules
    -- (see rule 613.11), those effects don't apply to illegal targets".
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
    -- two refs name. Rule 509.1c counts requirements PER CREATURE, so the pairs
    -- are what has to be stored -- Pawl.Engine.BlockRequirement.instances mints
    -- the printed carrier's pairs the same way.
    --
    -- Both sets are enumerated ONCE, for the CR 608.2f simultaneity
    -- objectRefObjects buys; an illegal slot (CR 608.2b) arrives as the empty
    -- list and stores nothing, which is provoke's fizzle.
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
  Effect.CreateEmblem card -> do
    -- CR 114.2: the resolving controller gets the emblem. The whole minting is
    -- Event.createEmblem's, shared with CR 701.54c's Ring emblem.
    _ <- Event.createEmblem controller card
    pure ()
  Effect.BecomeMonarch target -> do
    gs <- State.get
    let newMonarch = case target of
          -- "you become the monarch."
          MonarchTarget.TheController -> Just controller
          -- CR 725.2: the controller of the ability's bound source (the damaging
          -- creature), read from the reserved trigger-source slot.
          MonarchTarget.ControllerOfSource ->
            Map.lookup Binding.triggerSource chosen
              >>= Binding.onlyOne
              >>= Recipient.objectOf
              >>= (\o -> Projection.controllerOf o gs)
          -- CR 601.2c's chosen player, re-checked under CR 608.2b: the slot is a
          -- TARGET, so an illegal one crowns nobody while the rest of the ability
          -- still resolves. Recipient has no player accessor -- CR 115.1 makes a
          -- player a recipient in its own right rather than an object -- so the
          -- tag is matched inline, the way every other player-slot read here does
          -- it.
          MonarchTarget.InSlot slot ->
            case legalOne slot legal of
              Just (Recipient.ToPlayer crowned) -> Just crowned
              _ -> Nothing
    case newMonarch of
      Nothing -> pure ()
      -- CR 101.2: CR 725.1 and CR 725.3 gate nobody -- they ALLOW a crowning -- so
      -- Jared Carthalion's "You can't become the monarch this turn" outranks this
      -- instruction and the crown does not move. Read here rather than at the
      -- three MonarchTarget arms above, so every route this opcode has is stopped
      -- at once, CR 725.2's sourceless steal included (that ability still triggers
      -- and still resolves; it just crowns nobody).
      Just p | PlayerEffect.prohibitsBecomingMonarch p gs -> pure ()
      Just p -> do
        -- CR 725.3: the previous monarch ceases simply because `monarch` is
        -- overwritten (at most one at a time).
        State.modify' (\g -> g {GameState.monarch = Just p})
        State.modify' (Event.recordEvent (GameEvent.BecameMonarch p))
  -- The slot's permanent gains the designation -- CR 702.112a's "and it becomes
  -- renowned", CR 701.37a's "and it becomes monstrous", CR 701.60a's suspect -- and
  -- a state write rather than a CR 613 modification, every one of those rules making
  -- it "neither an ability nor part of the permanent's copiable values".
  --
  -- PutCounters' slot read without the quantity: a player recipient takes no
  -- designation (rule 702.112b: "only permanents can be or become renowned", and
  -- rules 701.37b and 701.60b say the same of the other two), an illegal slot at
  -- resolution is CR 608.2b's no-op, and an id naming no object -- the permanent has
  -- left the battlefield (CR 400.7) -- writes nothing and emits nothing, the lookup
  -- below answering Nothing.
  --
  -- GameEvent.BecameDesignated is what "whenever a creature you control becomes
  -- renowned" (Valeron Wardens) and "when this creature becomes monstrous" (Arbor
  -- Colossus) trigger on, the designation in the event telling the two apart.
  -- Emitted only on a TRANSITION, the same gate Event.unlockHalf applies to CR
  -- 709.5c's designation: a permanent that is already renowned does not become
  -- renowned again, which is also CR 701.60d's "a suspected permanent can't become
  -- suspected again".
  --
  -- That gate is a FENCE rather than a tested branch on every board in the pool, and
  -- deleting it leaves the suite green: each producer is stopped before it reaches
  -- here. Renown's intervening "if" (CR 603.4) removes the second ability from the
  -- stack (CR 608.2a); monstrosity's CR 701.37a "if this permanent isn't monstrous"
  -- is the CLAUSE's condition, read as Quantity.HasDesignation Monstrous before
  -- either of its effects runs, which is also what keeps a second monstrosity from
  -- putting counters on. Suspect CAN reach it -- Rune-Brand Juggler's "up to one
  -- target creature you control" narrows by nothing else -- but the only thing the
  -- gate suppresses is the event, and no card reads a permanent becoming suspected
  -- (#1215), so there is nothing to assert on either side.
  --
  -- CR 701.60c's menace and "this creature can't block" are not written here. They
  -- are read off Object.designations by Pawl.Engine.Projection and
  -- Pawl.Engine.CombatRestriction, which is rule 701.60c's "for as long as it's
  -- suspected".
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
  -- CR 701.60a's other ending, "until a spell or ability causes it to no longer be
  -- suspected". The write Designate above makes, undone for that one designation;
  -- nothing else has to be, because CR 701.60c's menace and can't-block are read off
  -- the set live rather than stamped when it was written.
  --
  -- Tap's arm structurally: the victims are enumerated ONCE through
  -- objectRefObjects for CR 608.2f's simultaneity, and an illegal slot (CR
  -- 608.2b), a player recipient and a set that matched nothing all arrive as the
  -- empty list. Idempotent for Designate's reason -- clearing a designation nothing
  -- has leaves the permanent as it was.
  Effect.Unsuspect ref ->
    State.modify' $ \gs ->
      let unsuspect o = o {Object.designations = Set.delete Designation.Suspected (Object.designations o)}
       in gs
            { GameState.objects =
                foldr (Map.adjust unsuspect) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
  -- CR 702.100a's counter and CR 702.100b's marker, in that order and in one arm
  -- because the rule ties them: the creature evolves only if the placement
  -- actually put one or more counters on it, which is what Event.putCounters
  -- answers.
  --
  -- The gate is a FENCE rather than a tested branch: no pooled board reaches a
  -- resolution here with nothing to place. Rule 702.100a's intervening "if" (CR
  -- 608.2a) already removes the ability when the bearer has left or stopped being
  -- a creature, and every CR 614.16 replacement in the pool only increases a
  -- placement.
  Effect.Evolve slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          placed <- Event.putCounters (CounterCause.ByEffect controller) target CounterKind.PlusOnePlusOne 1
          Monad.when (placed > 0) (State.modify' (Event.recordEvent (GameEvent.Evolved target)))
      _ -> pure ()
  -- CR 702.134a's counter and CR 702.134c's marker, Evolve's arm above with the
  -- rule's own gate rather than that one's: rule 702.134c fires on the mentor
  -- ability RESOLVING, so the event is recorded however many counters CR 614.16 left
  -- to place. That ungated emission is a FENCE and not a tested branch: re-gating it
  -- on `placed` leaves the suite green, because the only pooled replacement that can
  -- reduce a placement to nothing (an opponent's Vorinclex, Monstrous Raider) halves
  -- the shield counter this trigger would put on to nothing as well, so no board can
  -- see the difference.
  --
  -- The pair the event names is the resolving ability's SOURCE and the slot's
  -- creature, in rule 702.134c's order. An illegal slot never arrives here at all
  -- (CR 608.2b removes the ability), and an id naming no object writes nothing and
  -- emits nothing, both Evolve's postures.
  Effect.Mentor slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          _ <- Event.putCounters (CounterCause.ByEffect controller) target CounterKind.PlusOnePlusOne 1
          State.modify' (Event.recordEvent (GameEvent.Mentored (Mentored.MkMentored source target)))
      _ -> pure ()
  -- CR 702.149a's counter and CR 702.149c's marker, and this one takes EVOLVE's
  -- gate rather than the arm above's: rule 702.149c fires when a resolving training
  -- ability "puts one or more +1/+1 counters on this creature", so a placement CR
  -- 614.16 replaced away to nothing trains nobody.
  --
  -- That gate is a FENCE and not a tested branch, for Evolve's reason: nothing in
  -- the pool reduces a +1/+1 placement to nothing on a board where the difference
  -- could be read.
  --
  -- The event names the slot's creature and not `source`, which are the same object
  -- for every ability Pawl.Engine.Keyword.training mints (the slot is
  -- Binding.triggerSource) -- the id that is written is the one rule 702.149c asks
  -- about, "this creature". An id naming no object writes nothing and emits
  -- nothing, Evolve's posture.
  Effect.Train slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          placed <- Event.putCounters (CounterCause.ByEffect controller) target CounterKind.PlusOnePlusOne 1
          Monad.when (placed > 0) (State.modify' (Event.recordEvent (GameEvent.Trained target)))
      _ -> pure ()
  -- CR 731.1: the GAME gains the designation. Everything about what that entails
  -- -- CR 731.1's at-most-one, and the CR 702.145c/f transforms it causes
  -- immediately -- is Pawl.Engine.Daytime's, so this arm names no field and asks
  -- nothing about which effect it came from.
  --
  -- Nobody is named and nothing is prompted: rule 731.1 puts the designation on
  -- the game rather than on a player, unlike BecomeMonarch just above.
  Effect.ItBecomes designation -> do
    _ <- Daytime.becomes designation
    pure ()
  -- CR 701.3a / 702.6a: "Attach this permanent to target creature you control."
  -- CR 701.3a is the move itself -- "take it from where it currently is and put
  -- it onto that object" -- so this relocates a source that is already attached
  -- elsewhere. The SOURCE moves here; AttachTarget below is the sibling that
  -- moves the slot's TARGET instead.
  Effect.Attach slot ->
    case legalOne slot legal of
      Just recipient -> do
        gs <- State.get
        -- The slot's recipient is a PROPOSED destination; what gets stored is the
        -- recipient the moving permanent's own rules name it by (attachmentFor),
        -- and Nothing there is CR 701.3b's illegal attach.
        case Attach.attachmentFor source recipient gs of
          Nothing -> pure ()
          Just attachment -> do
            let alreadyThere = case Game.lookupObject source gs of
                  Nothing -> False
                  Just obj -> Object.attachedTo obj == Just attachment
            -- CR 701.3b, both sentences: an attach that cannot legally be
            -- performed does not move the permanent at all (it stays where it was
            -- rather than becoming unattached), and attaching it to what it is
            -- ALREADY attached to "does nothing" -- which matters because of the
            -- restamp below.
            Monad.unless alreadyThere $ do
              gs1 <- State.get
              -- CR 701.3c: attaching to a DIFFERENT object or player gives it a
              -- new timestamp. Not cosmetic -- CR 613.7 orders layer effects by
              -- it, so two things modifying one creature apply in attach order.
              let (ts, gs2) = Game.freshTimestamp gs1
                  move o = o {Object.attachedTo = Just attachment, Object.timestamp = ts}
              State.put gs2 {GameState.objects = Map.adjust move source (GameState.objects gs2)}
      _ -> pure ()
  -- CR 701.3a, in the other direction from Attach above: the SLOT's target is what
  -- moves, and the destination is chosen now rather than targeted.
  Effect.AttachTarget (AttachTarget.MkAttachTarget slot filter_) ->
    case legalOne slot legal of
      -- An unfilled slot, or one CR 608.2b has since made illegal: no-op.
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient: nothing on the battlefield moves
        Just subject -> do
          gs <- State.get
          -- The destinations the card's own TEXT admits, and NOTHING MORE.
          -- Narrowed to destinations the move would be LEGAL for only when the
          -- card SAYS so, which is Filter.CanHostSubject's whole job: Aura
          -- Graft's "another permanent IT CAN ENCHANT" narrows, and Crown of the
          -- Ages' bare "another creature" does not. Narrowing a filter that does
          -- not carry the atom would answer CR 303.4j's question on the player's
          -- behalf, and CR 303.4j exists precisely because the choice can land
          -- somewhere the subject may not go. That is the contrast with CR
          -- 303.4k at Attach.turnUpHosts, where the rule itself does the
          -- narrowing and leaves no such backstop open.
          --
          -- CR 609.3 when Attach.hostsFor is empty: the effect does as much as it
          -- can, and with no destination the text admits that is nothing. The
          -- elision at one candidate is Attach.chooseHost's, and it is not
          -- re-derived for an optional attach (#359).
          destination <- Attach.chooseHost controller subject (Attach.hostsFor controller source subject filter_ gs)
          -- The destination was chosen as an object off the battlefield, so it is
          -- proposed as a bare ToObject and Attach.attach re-tags it the way the
          -- subject's own enchant slot references it. Always a DIFFERENT object
          -- than the current host, which hostsFor never offers, so CR 701.3c's
          -- restamp is always earned -- and CR 303.4j's refusal, for a
          -- destination the subject may not go to, happens inside Attach.attach.
          Monad.mapM_ (Attach.attach subject . Recipient.ToObject) destination
      _ -> pure ()
  Effect.ExileUntilMonarch slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          -- CR 400.7: exile the target through the funnel; register the resulting
          -- incarnation for return when an opponent of `controller` (CR 102.2)
          -- BECOMES the monarch. The monarch as of right now is stamped into the
          -- watch, so an opponent who already holds the crown at this moment does
          -- not discharge it -- Palace Jailer's ruling is explicit that the
          -- creature "won't immediately return just because an opponent is the
          -- monarch" (#171).
          mNew <- Event.changeZoneReturning target Zone.Exile
          case mNew of
            Nothing -> pure ()
            Just newId -> do
              monarchNow <- State.gets GameState.monarch
              let watch =
                    MonarchWatch.MkMonarchWatch
                      { MonarchWatch.controller = controller,
                        MonarchWatch.lastMonarch = monarchNow
                      }
              State.modify' (\g -> g {GameState.exiledUntilMonarch = Map.insert newId watch (GameState.exiledUntilMonarch g)})
      _ -> pure ()
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting card slot) ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just haunted -> do
          -- The card this ability exiles is read LIVE off the resolving object,
          -- never out of `chosen`: rule 702.55a's "it" is the graveyard
          -- incarnation the death minted, which CR 400.7e lets the trigger find
          -- and Event.eventBindings bound under Binding.became, and CR 115.10a
          -- makes it no target. Nothing there is a card that left the graveyard
          -- before this resolved -- CR 400.7 minted a new object for it and the
          -- bound id names nothing -- and the ability exiles nothing.
          mCard <- State.gets (slotOne card resolving)
          case mCard of
            Nothing -> pure ()
            Just oid -> do
              -- CR 400.7 mints the exiled incarnation; CR 702.55b's link is filed
              -- against THAT id, which is what puts the ability in exile for CR
              -- 113.6k. A cancelled move (CR 614.6) leaves no id and so no link,
              -- which is right: nothing is haunting anything.
              --
              -- The link names the object the ability TARGETED, the permanent as
              -- it was on the battlefield -- rule 702.55b's "the object targeted
              -- by the haunt ability" -- so it goes on matching after that object
              -- has stopped being a creature, and is what
              -- TriggerCondition.HauntedCreatureDies compares ZoneChange.departed
              -- against.
              mNew <- Event.changeZoneReturning oid Zone.Exile
              Monad.forM_ mNew $ \newId ->
                State.modify' (\g -> g {GameState.haunting = Map.insert newId haunted (GameState.haunting g)})
      _ -> pure ()
  Effect.Counter slot ->
    case legalOne slot legal of
      -- CR 701.6a: the slot's target is a spell or an ability on the stack;
      -- counter it through the single funnel, which picks that rule's ending
      -- from the object's own kind (the graveyard for a spell, CR 608.2n's cease
      -- for an ability). A player recipient / illegal slot (CR 608.2b): no-op.
      --
      -- The funnel is handed THIS effect's source and controller, which is what
      -- Baral, Chief of Compliance's "whenever a spell or ability you control
      -- counters a spell" reads off the event it records: the countering object,
      -- and CR 405.4's controller of it, compared against CR 109.5's "you".
      -- Passed rather than left to be re-derived, because by the time the CR
      -- 117.5 trigger scan runs the controller can no longer be asked for
      -- exactly -- see Pawl.Types.Countering, which sets out the two cases.
      Just recipient -> mapM_ (Event.counter source controller) $ Recipient.objectOf recipient
      _ -> pure ()
  Effect.PutCounters (PutCounters.MkPutCounters kind quantity ref) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- CR 608.2c: the set is swept as this instruction is reached, and an
        -- illegal slot (CR 608.2b) or a player recipient answers with nobody.
        targets = objectRefObjects legal resolving controller source gs ref
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
      -- ONE evaluation for the whole set (CR 608.2f), then CR 122.6's single
      -- funnel per recipient, so CR 614's counter replacements (Hardened Scales,
      -- Doubling Season) get their opportunity against each placement.
      Just n ->
        Monad.when (n > 0) . Monad.forM_ targets $ \target ->
          Event.putCounters (CounterCause.ByEffect controller) target kind (Integer.toNaturalSaturating n)
  -- CR 122: PutCounters' mirror, and deliberately NOT through a CR 614.16 gate
  -- -- that rule replaces a placement, and nothing in CR 614 replaces a removal,
  -- so there is no loop for this to enter.
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters kind quantity slot) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient has no object counters
        Just target -> case Quantity.evaluateFor viewOf context gs resolving source quantity of
          Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
          Just n -> Monad.when (n > 0) (Event.removeCounters target kind (Integer.toNaturalSaturating n))
      _ -> pure () -- illegal slot at resolution (CR 608.2b): no-op
      -- CR 701.34a: "choose any number of permanents and/or players that have a
      -- counter, then give each one additional counter of each kind that permanent or
      -- player already has."
      --
      -- Every clause of that sentence is load-bearing. "That have a counter" is the
      -- candidate filter, so proliferate never starts anything on its first counter.
      -- "Any number" means the chosen set may be empty, which is why a lone candidate
      -- is still asked about. "Each kind ... already has" means one more of every kind
      -- present, not a doubling and not a kind of the chooser's choosing.
      --
      -- Both the candidates and their kinds are read from the state BEFORE the prompt
      -- and before any counter lands. That keeps the answer to "which kinds does this
      -- have" fixed for the whole action (CR 608.2h's posture), so a CR 614
      -- replacement that scales one placement cannot feed back and widen the set of
      -- kinds still being walked.
      --
      -- Targetless: nothing was targeted, so unlike every slot-reading opcode here
      -- there is no CR 608.2b legality to re-check.
      --
      -- The "players" of CR 701.34a are CR 102.1's -- the people IN the game --
      -- so the roster is Game.stillPlaying, not the keys of GameState.players,
      -- which keep a departed seat's row. This is the observable one, and the
      -- reason #279 was worth closing rather than deferring again: a departed
      -- player's counters stay on their record, because CR 800.4a removes their
      -- OBJECTS and CR 109.1's list of what an object is has no room for a
      -- counter on a player. Their poison is still there for kindsFor to find,
      -- so the map's keys would offer someone who is not in the game as a
      -- choice, and honouring that answer puts a fresh counter on a non-player.
  Effect.Proliferate -> do
    gs <- State.get
    let everyone = Game.stillPlaying gs
        kindsOn oid = foldMap (Map.keys . Map.filter (> 0) . Object.counters) (Game.lookupObject oid gs)
        kindsFor pid = foldMap (Map.keys . Map.filter (> 0) . Player.counters) (Map.lookup pid (GameState.players gs))
        -- The battlefield is shared, and zoneMembers slices it by OWNER, so the
        -- union over every seat is every permanent in play -- not just this
        -- player's. CR 701.34a lets a proliferating player choose anyone's.
        onBattlefield = concatMap (\pid -> Game.zoneMembers Zone.Battlefield pid gs) everyone
        permanents = filter (not . null . kindsOn) onBattlefield
        players = filter (not . null . kindsFor) everyone
    Monad.unless (null permanents && null players) $ do
      (pickedPermanents, pickedPlayers) <-
        Game.choose (Prompt.ChooseProliferate (Decide.deciderFor controller gs) controller permanents players)
      -- FILTERED, NOT TRUSTED, the posture every other prompt reader takes: an
      -- answer naming something that was not offered is dropped rather than
      -- honoured, so a bogus id cannot mint a counter on a permanent that had
      -- none -- which is precisely what the candidate filter exists to prevent.
      let keptPermanents = filter (\oid -> Set.member oid pickedPermanents) permanents
          keptPlayers = filter (\pid -> Set.member pid pickedPlayers) players
      -- CR 122.6: object counters go through the single funnel, so CR 614's
      -- counter replacements (Hardened Scales, Doubling Season) apply to a
      -- proliferated counter exactly as they do to a placed one.
      Monad.forM_ keptPermanents $ \oid ->
        Monad.forM_ (kindsOn oid) $ \kind -> Event.putCounters (CounterCause.ByEffect controller) oid kind 1
      -- CR 122.1: and player counters through their own funnel, so the same CR
      -- 614 opportunity reaches a proliferated poison counter.
      Monad.forM_ keptPlayers $ \pid ->
        Monad.forM_ (kindsFor pid) $ \kind ->
          Monad.void (Event.putPlayerCounters (CounterCause.ByEffect controller) pid kind 1)
  -- CR 701.39a: "bolster N" -- choose a creature you control with the least
  -- toughness, or tied for least, among creatures you control, and put N +1/+1
  -- counters on it.
  --
  -- Every clause is load-bearing. "Creatures you control" is the candidate pool,
  -- so an opponent's smaller creature is never offered however low its toughness.
  -- "The least toughness ... or tied for least" narrows that pool to the minimum
  -- and everything equal to it, which is why the prompt is only raised for a TIE
  -- -- one creature at the minimum leaves nothing to ask, and CR 101.3 ignores the
  -- instruction when the pool is empty.
  --
  -- Toughness is the PROJECTED value (CR 613.1g's layer 7), so an Aura or a
  -- counter already on a creature moves it in and out of the tie. A creature the
  -- projection gives no toughness at all cannot be compared and is dropped from
  -- the pool rather than sorted as if it were zero.
  --
  -- The pool is swept and the minimum taken BEFORE the counters land, which is CR
  -- 608.2h's posture: bolster places its counters on one creature, so nothing here
  -- re-reads a toughness the placement itself changed.
  --
  -- Targetless: nothing was targeted, so unlike every slot-reading opcode here
  -- there is no CR 608.2b legality to re-check.
  Effect.Bolster quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        -- Ascending, so both the single-candidate shortcut and a transcript are
        -- deterministic -- Ring.tempt's posture.
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
            -- Unreachable by construction, since `least` is the minimum OF this
            -- very list: the first creature measured keeps the mandatory action
            -- mandatory rather than turning an impossible case into a no-op.
            [] -> pure (fst first)
            one : others -> case others of
              -- One creature at the minimum is the whole of rule 701.39a's
              -- candidate set, and the instruction is mandatory -- where the rules
              -- leave nothing to ask, don't prompt.
              [] -> pure one
              second : more -> do
                let offered = one NonEmpty.:| (second : more)
                answer <- Game.choose (Prompt.ChooseBolster (Decide.deciderFor controller gs) controller resolving offered)
                -- FILTERED, NOT TRUSTED, the ChooseRingBearer posture: an answer
                -- naming something never offered falls back to the first
                -- candidate, since the action is mandatory and must put its
                -- counters on someone.
                pure (if List.elem answer (NonEmpty.toList offered) then answer else one)
          -- CR 122.6: through the single funnel, so CR 614.16's counter
          -- replacements (Hardened Scales, Doubling Season) get their opportunity.
          Monad.when (n > 0) . Monad.void $
            Event.putCounters (CounterCause.ByEffect controller) bolstered CounterKind.PlusOnePlusOne (Integer.toNaturalSaturating n)
  -- CR 701.47a: the resolving controller amasses. The whole keyword action is
  -- Pawl.Engine.Amass.amass's, which is where rule 701.47's text and its token
  -- live -- this arm evaluates the printed N and knows nothing else about it,
  -- exactly as the TemptWithTheRing arm below knows only that some effect asked.
  --
  -- Targetless: nothing was targeted, so unlike every slot-reading opcode here
  -- there is no CR 608.2b legality to re-check.
  Effect.Amass (Amass.Type.MkAmass quantity subtype) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
      Just n -> Amass.amass controller source resolving subtype (Integer.toNaturalSaturating n)
  -- CR 701.68a: "blight N" -- each player the PlayerRef names puts N -1/-1
  -- counters on a creature THEY control. The whole keyword action is
  -- Pawl.Engine.Blight.blight's, which is where rule 701.68's text lives -- this
  -- arm knows only that some effect asked for it, exactly as the Amass arm above
  -- knows only that some effect asked for that. Rule 701.68b's "they can't choose
  -- to blight" reaches the same procedure through Pawl.Engine.Cost, where CR
  -- 118.12 makes the optional reading a cost.
  --
  -- The PlayerRef is passed on to `blight` per player, so each blighter's pool is
  -- their own creatures and each is asked separately -- rule 701.68a's "you" is
  -- whoever the instruction addresses. A reference naming nobody, or an illegal
  -- slot (CR 608.2b), arrives as the empty list and blights nothing, the posture
  -- every PlayerRef arm here takes.
  --
  -- APNAP for the order several blighters are asked in (CR 101.4), Scry's and
  -- Surveil's reading and for their reason: rule 701.68 states no order of its
  -- own.
  --
  -- Not implemented: rule 101.4's actions then happen SIMULTANEOUSLY, and each
  -- blighter's counters are placed before the next is asked instead (#1651).
  --
  -- MANDATORY here, so the empty pool is CR 101.3's no-op rather than rule
  -- 701.68b's refusal, and the answer is discarded.
  --
  -- Not implemented: nothing records which creature was blighted, so CR 701.68c's
  -- "blighted creature" cannot be named by a later effect and CR 701.68d's trigger
  -- has nothing to fire on (#1492).
  --
  -- Targetless: nothing was targeted, so unlike every slot-reading opcode here
  -- there is no CR 608.2b legality to re-check.
  Effect.Blight (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        named = playerRefPlayers legal controller gs ref
        blighters = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    Monad.forM_ blighters $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
        Just n -> Monad.void (Blight.blight pid resolving (Integer.toNaturalSaturating n))
  -- CR 701.54a: the Ring tempts the resolving controller. The whole keyword
  -- action is Pawl.Engine.Ring.tempt's, which is where rule 701.54's text lives --
  -- this arm knows only that some effect asked for it, exactly as the arms around
  -- it know only that some effect asked for a counter or a card.
  Effect.TemptWithTheRing -> Ring.tempt controller
  -- CR 701.49: the whole keyword action, which Pawl.Engine.Dungeon owns.
  Effect.Venture -> Dungeon.venture controller
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters ref kind quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        recipients = playerRefPlayers legal controller gs ref
    -- CR 122 / 107.14: the amount per recipient (evaluateForRecipient, off the one
    -- pre-effect `gs`), then CR 122.6's funnel per recipient -- PutCounters'
    -- posture above, so a counter-scaling replacement (Vorinclex, Monstrous
    -- Raider) gets its CR 614 opportunity against each player's gain.
    Monad.forM_ recipients $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 -> Monad.void (Event.putPlayerCounters (CounterCause.ByEffect controller) pid kind (Integer.toNaturalSaturating n))
        _ -> pure ()
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters ref kind quantity) -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = effectContext controller source legal
        recipients = playerRefPlayers legal controller gs ref
    Monad.forM_ recipients $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 ->
              -- CR 122: the mirror of GainPlayerCounters above, off the player
              -- record directly and through no funnel, for the reason
              -- Effect.RemoveCounters gives: CR 614 replaces a placement, and
              -- nothing in it replaces a removal. Natural subtraction would
              -- underflow, so the floor is explicit -- a player with fewer counters
              -- than the effect removes ends at none, which is what "removes one rad
              -- counter from themselves" of a player who has none means.
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
  Effect.Tap ref ->
    State.modify' $ \gs ->
      -- CR 701.26a: turn each named permanent sideways. The exact mirror of the
      -- Untap arm below, enumerating its victims ONCE through the same
      -- objectRefObjects for the same CR 608.2f simultaneity, so an illegal slot
      -- (CR 608.2b), a player recipient and a set that matched nothing all
      -- arrive as the empty list and tap nothing.
      --
      -- Rule 701.26a's "only untapped permanents can be tapped" needs no guard:
      -- assigning Tapped to something already tapped leaves it exactly as it
      -- was, which is what "nothing happens" means for a state this coarse.
      let tap o = o {Object.tapped = TapState.Tapped}
       in gs
            { GameState.objects =
                foldr (Map.adjust tap) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
  Effect.Untap ref ->
    State.modify' $ \gs ->
      -- CR 701.26b: rotate each named permanent back to the upright position.
      -- The victims are enumerated ONCE, for the CR 608.2f simultaneity
      -- objectRefObjects buys; an illegal slot (CR 608.2b), a player recipient
      -- and a set that matched nothing all arrive as the empty list and untap
      -- nothing, so there is one path rather than three.
      let untap o = o {Object.tapped = TapState.Untapped}
       in gs
            { GameState.objects =
                foldr (Map.adjust untap) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
  Effect.Detain ref ->
    State.modify' $ \gs ->
      -- CR 701.35a: detain each named permanent until the next turn of THIS
      -- resolution's controller -- rule 109.5's "you", which is `controller`
      -- here and is sampled once, since the sweep that ends the detain
      -- (Pawl.Engine.Expiry.dropAtTurnOf) has no resolution left to read it off.
      -- The victims are enumerated ONCE through the same objectRefObjects the Tap
      -- and Untap arms above use, for the same CR 608.2f simultaneity, so an
      -- illegal slot (CR 608.2b), a player recipient and a set that matched
      -- nothing all arrive as the empty list and detain nothing.
      --
      -- Nothing is stored anywhere but on the victim, so a permanent already
      -- detained is detained again with no count kept and no row to reconcile --
      -- see Object.detainedUntil.
      foldr (Detain.detain controller) gs (objectRefObjects legal resolving controller source gs ref)
  Effect.DoesNotUntapNext ref ->
    State.modify' $ \gs ->
      -- CR 502.3's "effects can keep one or more of a player's permanents from
      -- untapping", as a one-shot. The victims are enumerated ONCE through the
      -- same objectRefObjects Tap and Untap above share, so an illegal slot (CR
      -- 608.2b), a player recipient and a set that matched nothing all arrive as
      -- the empty list and prohibit nothing.
      --
      -- No duration is stored and none is owed: CR 701.43b makes the untap step
      -- the prohibition bites in the step it expires in, and
      -- Engine.untapAll clears the flag there. That is also why marking a
      -- permanent already carrying the flag is a no-op rather than a stack --
      -- assigning True to True -- which is that rule's non-stacking said as a
      -- state assignment, the shape Tap's arm takes toward an already-tapped
      -- permanent.
      let mark o = o {Object.doesNotUntapNext = True}
       in gs
            { GameState.objects =
                foldr (Map.adjust mark) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
  Effect.Transform ref ->
    State.modify' $ \gs ->
      -- CR 701.27a: "To transform a permanent, turn it over so that its other
      -- face is up." One assignment to Object.face per victim, which is all a
      -- turn IS here -- every characteristic read already resolves through it
      -- (Game.faceOf), so the permanent's power, type line, keywords and
      -- abilities all change together and none of them has to be told.
      --
      -- The victims are enumerated ONCE, the sweep Tap and Untap above share, so
      -- an illegal slot (CR 608.2b), a player recipient and a set that matched
      -- nothing all arrive as the empty list and turn nothing over.
      --
      -- WHICH face is Pawl.Engine.Card.turnedOver's answer, off the card's
      -- layout: it is what withholds a turn from a permanent that is not
      -- double-faced (CR 701.27c) and from one whose other face is an instant or
      -- sorcery (CR 701.27d), so this arm never learns which card it is holding.
      --
      -- CR 701.27b is why nothing else fires: turning over is its own game
      -- action, distinct from turning a permanent face up or face down, and a
      -- card that triggers ON it needs a trigger condition pawl does not have
      -- (#695).
      --
      -- ONE fresh timestamp for the whole instruction, threaded through so CR
      -- 701.27f can later ask when this happened. Minted even when the sweep
      -- turns nothing over, which costs a number nobody reads rather than a
      -- branch: GameState.nextTimestamp is a counter with no meaning beyond its
      -- order (CR 613.7).
      --
      -- ONE whole-board projection for the whole instruction too, hoisted here
      -- and handed to every victim: CR 702.145b's restriction is read off the
      -- layer fold, and projecting per victim would refold the board once per
      -- Human the sweep named.
      let (now, g1) = Game.freshTimestamp gs
          pcs = Projection.projectAll g1
       in g1
            { GameState.objects =
                foldr (turnOver pcs resolving now g1) (GameState.objects g1) (objectRefObjects legal resolving controller source g1 ref)
            }
  -- CR 500.8: add the phases, directly after the phase this is resolving in.
  --
  -- Turn.splicePhases is handed GameState.phase because "directly after this
  -- phase" is NOT the head of `remaining` when the resolving phase still has
  -- steps to come: Aurelia, the Warleader's trigger resolves in the declare
  -- attackers step, where this combat phase's own declare blockers, combat
  -- damage and end of combat steps are all still ahead. CR 511.3 is what bounds
  -- the phase, and Turn.thisPhase is where that lives.
  Effect.AddPhases extras ->
    State.modify' $ \gs ->
      gs {GameState.remaining = Turn.splicePhases (GameState.phase gs) extras (GameState.remaining gs)}
  Effect.GainControl (DurationRef.MkDurationRef duration ref) ->
    State.modify' $ \gs ->
      -- Enumerated ONCE, by the sweep every ObjectRef-taking opcode shares: Act
      -- of Treason's slot and Aura Thief's "all enchantments" arrive as the same
      -- list, and a player recipient, an illegal slot (CR 608.2b) and a set that
      -- matched nothing all arrive as the empty one and change nothing.
      case objectRefObjects legal resolving controller source gs ref of
        [] -> gs
        targets
          -- CR 800.4b: "If an object would change to the control of a player
          -- who has left the game, it doesn't." `controller` is baked at
          -- trigger time (CR 113.8), so a resolution can name a player who has
          -- since left. Nothing would clean up after the change: CR 800.4a's
          -- fourth clause ("Then, if there are any objects still controlled by
          -- that player, those objects are exiled") is not a state-based action
          -- and "happens as soon as the player leaves the game", so it has
          -- already run and does not run again. Without this guard the
          -- permanent would simply sit on the battlefield controlled by a
          -- player who is not in the game.
          | List.notElem controller (Game.stillPlaying gs) -> gs
          -- CR 611.2b's condition is baked against `legal` rather than `chosen`,
          -- the map every other player reference in this resolution is read
          -- through (playerRefPlayers): a slot CR 608.2b has emptied names nobody
          -- here too, so the duration never starts rather than starting on a
          -- target that is no longer legal. For a slot the EVENT bound -- Garland,
          -- Royal Kidnapper's "they", never a target -- the two maps agree.
          | otherwise -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
              -- CR 611.2b: the duration never started -- no control effect is
              -- stored, and nothing is re-Sicked, because control never changed.
              Nothing -> gs
              Just expiry ->
                -- CR 613.1b / 611.2c: the new controller is `controller` (this
                -- effect's source's controller), baked in now -- derived, never
                -- chosen. CR 302.6: the new controller has not controlled the
                -- permanent continuously, so it is re-Sicked.
                --
                -- Unless control does not actually move. CR 302.6 asks whether
                -- control was CONTINUOUS, and gaining control of a permanent you
                -- already control interrupts nothing, so the clock must not
                -- reset (#206). Act of Treason may legally target your own
                -- creature -- untapping it is the reason to; Aura Thief's "all
                -- enchantments" sweeps its own controller's as well. So the
                -- question is asked PER OBJECT, not once for the whole set.
                --
                -- Compared against the PROJECTED controller, read before the new
                -- effect is stored, not against Object.owner: you may already
                -- control a permanent you do not own (Control Magic), and
                -- re-gaining that one interrupts nothing either.
                --
                -- CR 611.2c: one stored effect over the frozen id set, exactly as
                -- ModifyTarget stores one -- "the set of objects it affects is
                -- determined when that continuous effect begins", so an
                -- enchantment that enters after this is not in it.
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
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn ref skips) -> do
    gs <- State.get
    let named = playerRefPlayers legal controller gs ref
        -- CR 500.7: "If multiple players are given extra turns, the extra turns
        -- are added one at a time, in APNAP order (see rule 101.4)." The
        -- intersection is Draw's, for Draw's reasons: apnapOrder supplies the
        -- ORDER and `named` the MEMBERSHIP, so a seat the rotation still names
        -- but playerRefPlayers does not -- a departed player, who stopped being
        -- one at CR 102.1 while keeping their seat -- gets no turn. A departed
        -- player named through a TARGET slot can still get an entry, since that
        -- arm reads the slot rather than the roster; CR 800.4k catches it at the
        -- handoff, where the turn simply does not begin.
        --
        -- Observable, not cosmetic: the pushes below are what CR 500.7's last
        -- sentence then reverses, so APNAP order is what decides which of two
        -- players takes their extra turn first.
        takers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    -- CR 500.7: "the extra turns are added ONE AT A TIME ... the MOST RECENTLY
    -- CREATED turn will be taken first." So each taker is pushed onto the head
    -- in turn, and the last one pushed is the first one Engine.handoffTurn pops
    -- -- a stack, not a queue. A second TakeExtraTurn resolving later in the
    -- same turn lands in front of this one's entries for the same reason, which
    -- is the half of the rule that two Time Warps exercise.
    --
    -- CR 500.11 / 113.7: the skips ride ALONG on each entry, naming that turn
    -- and no other, with this effect's source as theirs. Nothing is installed
    -- now -- Engine.takeNextTurn does that as the turn it belongs to actually
    -- begins (see Pawl.Types.ExtraTurn for why the skip travels with the turn
    -- rather than being a "next occurrence" replacement installed here).
    let entry pid = ExtraTurn.MkExtraTurn {ExtraTurn.taker = pid, ExtraTurn.source = source, ExtraTurn.skipped = skips}
    State.modify' (\g -> g {GameState.extraTurns = List.foldl' (\ts pid -> entry pid : ts) (GameState.extraTurns g) takers})

-- CR 119.3: move one player's life total by this much, and record the CR 608.2i
-- event of the matching sign. The write LoseLife, GainLife and ExchangeLifeTotals
-- share, so a life total moves in exactly one place; a signed delta is the shape
-- the exchange needs, and the sign is what picks the event, so the two kinds of
-- event stay distinct for a trigger to read.
--
-- A zero delta writes nothing at all: CR 119.9 says so for the gain side ("if a
-- player gains 0 life, no life gain event has occurred"), and the loss side takes
-- the same posture, no rule making a 0 subtraction an event either. The opcodes
-- above guard on their quantity as well; an exchange between equal totals reaches
-- this guard alone.
changeLife :: PlayerId -> Integer -> Game ()
changeLife pid delta =
  Monad.when (delta /= 0) . State.modify' $
    Event.recordEvent
      ( if delta > 0
          then GameEvent.LifeGained (LifeChange.MkLifeChange pid (Integer.toNaturalSaturating delta))
          else GameEvent.LifeLost (LifeChange.MkLifeChange pid (Integer.toNaturalSaturating (negate delta)))
      )
      . (\g -> g {GameState.players = Map.adjust (\p -> p {Player.life = Player.life p + delta}) pid (GameState.players g)})

-- The no-subgame executor (the ability path and every direct caller): a
-- PlaySubgame resolves as a draw here (see noSubgame).
applyEffect :: ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card -> Game ()
applyEffect = applyEffectWith noSubgame

-- CR 615.5: run the additional effect of every prevention that has just been
-- applied -- "the prevention takes place at the time the original event would
-- have happened; the rest of the effect takes place immediately afterward".
--
-- Drains GameState.pendingPreventionRiders, which Pawl.Engine.Damage filled. The
-- queue exists because that module is BELOW this one and cannot run a card's
-- effects; this is the seam, and its two callers (this module's DealDamage arm
-- and Pawl.Engine.Engine's combat damage step) both run it before the board can
-- be observed and, decisively, before the next state-based action check. That
-- ordering IS the rule: Test of Faith's 2004-12-01 ruling puts the counters on
-- "at the same time the damage is prevented", so a 1/1 dealt 6 ends as a 4/4
-- with 3 marked rather than dying to CR 704.5g first.
--
-- Emptied BEFORE the riders run, so a rider whose own damage is prevented by a
-- second shield appends to a fresh queue instead of being re-run here.
runPreventionRiders :: Game ()
runPreventionRiders = do
  queued <- State.gets GameState.pendingPreventionRiders
  State.modify' (\gs -> gs {GameState.pendingPreventionRiders = Seq.empty})
  Foldable.traverse_ runPreventionRider queued

-- One queued prevention's additional effect. Nothing to run unless the
-- prevention carries a rider AND its recipient is an object.
--
-- The recipient's OBJECT is where the prevented amount is stamped, and it is the
-- only place it can be: CR 615.5's clause "may refer to the amount of damage
-- that was prevented" is a Quantity.InSlot read of the reserved
-- Binding.eventAmount slot, which Quantity.evaluateFor answers off a live
-- object, and the spell that installed the shield went to its owner's graveyard
-- as a new object long ago (CR 400.7). The shielded permanent is live by
-- construction -- it is what the damage was addressed to. A shield over a PLAYER
-- has no such object, so its rider does not run (#1104).
--
-- The stamp is UNDONE afterwards, restoring whatever was there. It is the one
-- writer that puts Binding.eventAmount on a BATTLEFIELD permanent -- every other
-- writer of that slot is Pawl.Engine.Event.eventBindings, stamping a resolving
-- object's own bindings as a trigger is gathered -- and Quantity.evaluateFor
-- asks an effect's SOURCE before the object on the stack, so a value left behind
-- here would shadow the amount a later CR 615.13 trigger of that same permanent
-- supplied. No board in the pool reaches that collision, so this is a fence
-- rather than a fix: it has no test, and mutating it away leaves the suite
-- green.
--
-- `resolving` and `source` are both the recipient: the rider is not resolving
-- from the stack, and the recipient is the one object it can read anything off.
-- Every slot the rider names is treated as a LEGAL target, because CR 608.2b was
-- applied when the installing spell resolved -- a shield exists only because its
-- target was legal then -- and the rider re-targets nothing.
runPreventionRider :: Prevention.Prevention -> Game ()
runPreventionRider prevention = case (Prevention.rider prevention, Recipient.objectOf (Prevention.recipient prevention)) of
  (Just rider, Just oid) -> do
    was <- State.gets (Map.lookup Binding.eventAmount . maybe Map.empty Object.bindings . Game.lookupObject oid)
    State.modify' (bindAmountSlot oid Binding.eventAmount (Prevention.amount prevention))
    let targets = PreventionRider.targets rider
    Foldable.traverse_
      (applyEffect oid oid (PreventionRider.controller rider) targets targets)
      (PreventionRider.effects rider)
    State.modify' $ \gs ->
      let restore obj = obj {Object.bindings = Map.alter (const was) Binding.eventAmount (Object.bindings obj)}
       in gs {GameState.objects = Map.adjust restore oid (GameState.objects gs)}
  _ -> pure ()

-- CR 614.1c: run the effects of every as-enters rewrite that has applied and not
-- run yet -- Monstrous War-Leech's "as this creature enters, if it was kicked,
-- mill four cards".
--
-- Drains GameState.pendingEntryEffects, which Pawl.Engine.Event filled.
-- runPreventionRiders above in every structural respect, and for the same reason:
-- the module that applies the replacement is below this one and cannot run a
-- card's effects. Emptied before the effects run, so an entry one of them causes
-- appends to a fresh queue instead of being re-run here.
--
-- Its one caller is Pawl.Engine.Engine.performSettle, which runs it before the
-- SBA pass and before the trigger scan -- so the Leech's graveyard is four cards
-- deeper before CR 704.5f reads the power and toughness that graveyard defines.
-- What that ordering does NOT give is CR 614.1c's own placement, inside the
-- entry; see GameState.pendingEntryEffects.
runEntryEffects :: Game ()
runEntryEffects = do
  queued <- State.gets GameState.pendingEntryEffects
  State.modify' (\gs -> gs {GameState.pendingEntryEffects = Seq.empty})
  Foldable.traverse_ runEntryEffect queued

-- One entered permanent's as-enters effects, in printed order.
--
-- `resolving` and `source` are both the permanent, runPreventionRider's posture:
-- nothing is resolving from the stack, and the permanent is the object every
-- Filter.IsSource in the effects resolves against. The slot maps are empty
-- because a static ability targets nothing (CR 115.10a), so there is no chosen
-- target for an effect to name.
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
-- player's hand. Pawl.Engine.Mulligan's two window loops reach this through the
-- HandActionPerformer parameter they are handed (see
-- Pawl.Types.HandActionPerformer for why it is a parameter).
--
-- The action does not use the stack -- both rules say the player PERFORMS it,
-- not that they cast or activate anything -- so there is nothing to put on the
-- stack and no modes to bind. The CHOICE of whether to act was already routed
-- through Decide.deciderFor at the prompt (CR 723).
--
-- Stands on the noSubgame floor (applyEffect), exactly as the RestartGame arm
-- does: no hand action starts a subgame.
performHandAction :: HandActionPerformer.HandActionPerformer
performHandAction source player =
  Monad.mapM_
    ( applyEffect
        source
        source
        player
        -- CR 115.1: the reserved self slot is NOT a target, so there is no CR
        -- 608.2b legality question to answer -- the card is in the acting
        -- player's hand by construction. Binding it is how "this card" is
        -- expressible with no self-variant opcode (see Effect.Sacrifice's
        -- comment, and Engine.placeOne, which binds a trigger's source the same
        -- way). CR 103.6a's "puts that card onto the battlefield" is then just
        -- MoveToZone on this slot.
        (Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject source)))
        (Map.singleton Binding.triggerSource (Set.singleton (Recipient.ToObject source)))
    )

-- CR 603.7c: bind `target` into `slot` of `holder`'s binding environment, so a
-- delayed ability armed later in the SAME resolution can name the object.
-- `holder` is `resolving` -- the object ON THE STACK, which is the spell itself
-- for a spell and the ABILITY object for an activated or triggered one -- the
-- same object ArmDelayedTrigger captures from, so the two always agree. See
-- applyEffectWith for why the stack object and not the effect's `source`.
--
-- The slot IS visible to a later effect of the same fold, on both paths and by
-- the same mechanism: resolveSpellWith and resolveModes each re-read
-- Object.bindings before EACH effect (CR 608.2c), so a later
-- Sacrifice/Destroy/ModifyTarget reading this slot sees the mid-fold value. That
-- is what lets PlaySubgame's reported winner reach a follow-on LoseLife on the
-- spell path, and Harried Dronesmith's "It gains haste until end of turn" reach
-- the token its own trigger just minted on the ability path (proved by
-- Pawl.TriggerSpec's "CR 702.10b \"it gains haste\" reaches the one token").
-- ArmDelayedTrigger and slotOne see it without either re-read, since both go to
-- LIVE GameState rather than to `chosen`.
bindSlot :: ObjectId -> SlotName -> ObjectId -> GameState -> GameState
bindSlot holder slot target gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObject target) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- bindSlot's plural: bind EVERY object one instruction produced into `slot`, for
-- a card that refers back to all of them at once -- every token a Create minted
-- ("those tokens"), every incarnation a MoveToZone left in a public zone ("those
-- cards", CR 400.7j). Same holder and same further reason (CR 603.7c) --
-- ArmDelayedTrigger captures this object's whole environment, which is how
-- "those tokens" outlives the resolution that minted them.
--
-- Readable mid-fold without either resolution path's per-effect re-read, because
-- every reader goes through slotGroup, which reads live GameState. It has to:
-- this rides the binding's `objects` field, and `chosen` is Binding.targetsOf,
-- which reads only the `target` field -- so a group is invisible there however
-- often the map is rebuilt. bindSlot's SINGLE object is the one that lands in
-- `target`, and so the one both a live read (slotOne) and `chosen` can see.
bindObjectsSlot :: ObjectId -> SlotName -> Seq.Seq ObjectId -> GameState -> GameState
bindObjectsSlot holder slot targets gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObjects targets) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- The default runner for every resolution that is NOT a subgame-bearing spell:
-- there is no nested game, so a PlaySubgame effect reports a draw and binds
-- nothing. The ability path (resolveEffects) and every direct test caller take
-- this.
--
-- Not implemented: a subgame played from an ABILITY (#137). Until that lands the
-- default is unreachable from card data -- no card in the pool puts PlaySubgame
-- in an ability -- which matters more than it used to: a follow-on reading the
-- slot through PlayerRef.EachPlayerExcept now charges the WHOLE TABLE for a
-- draw, so a stand-in draw here would be a live wrong answer rather than the
-- silent no-op it once was.
noSubgame :: Game Result
noSubgame = pure Result.Drawn

-- Bind a PLAYER a resolution named into `slot` on `holder`, so a later effect of
-- the same resolution can read it. Mirrors bindSlot, but the recipient is a
-- player (ToPlayer), not an object.
--
-- TWO CALLERS, and each passes the holder its own READER looks at:
--
--   * CR 729.1b's subgame winner, held on the effect's `source`. The
--     follow-on reads it through resolveSpellWith's re-read of the resolving
--     SPELL's bindings, and only a spell can play a subgame (an ability-driven
--     one is deferred, see noSubgame), so for every producer that can exist the
--     two ids are the same object.
--   * CR 608.2d's chosen opponent, held on `resolving` -- Skullwinder's is a
--     TRIGGERED ability, where the two ids differ, and the reader is the next
--     effect's `legalNow`, which resolveModes recomputes off the STACK object's
--     bindings. Binding it to `source` would leave the permanent holding a slot
--     nothing reads.
bindPlayerSlot :: ObjectId -> SlotName -> PlayerId -> GameState -> GameState
bindPlayerSlot holder slot player gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toPlayer player) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- CR 701.8b: bind how many permanents a destruction actually destroyed into
-- `slot` on `holder`, so a later effect of the same resolution can read it as
-- Quantity.InSlot. The third of the same shape, after bindSlot (an object) and
-- bindPlayerSlot (a player); this one binds a NUMBER, which rides the binding
-- field CR 601.2b's chosen X rides.
--
-- Left behind on the holder after the resolution ends, exactly as the other two
-- are. Harmless and unreadable: only an effect naming this slot can see it, the
-- D4 lint makes every such read live under an effect list that also BINDS it,
-- and a second sweep on the same holder overwrites the value before reading it.
--
-- `holder` is the effect's `source`, NOT `resolving`, and that asymmetry with
-- bindSlot above is about where each is READ rather than about what each means:
-- an amount is read back by Quantity.evaluate, which every arm calls aimed at
-- `source` (CR 608.2h's "information from a specific object ... including the
-- source of the ability itself"), while an object binding is read back by
-- ArmDelayedTrigger off the stack object. Bane of Progress binds and reads one
-- inside a TRIGGERED ability, where the two ids differ, so this is load-bearing
-- rather than a convention.
bindAmountSlot :: ObjectId -> SlotName -> Natural -> GameState -> GameState
bindAmountSlot holder slot n gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toAmount n) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- CR 701.23: do to a found card what the search said -- a move for every
-- destination and, for one of them, a CR 701.20a reveal first. The move goes
-- through the CR 400.7 funnel either way, so a replacement watching the
-- destination composes and the card that lands is a new object.
putFound :: PlayerId -> SearchDestination.SearchDestination -> ObjectId -> Game ()
putFound searcher destination cardId = case destination of
  SearchDestination.BattlefieldTapped -> putTapped cardId
  -- No tapped-ness to set afterwards and so no need for the found id: a card in
  -- a hand has no tap state to speak of (CR 110.5 gives a status only to a
  -- permanent).
  --
  -- The reveal comes FIRST, in the card's own order ("reveal that card, put it
  -- into your hand"), and CR 701.20b is what makes that an order rather than
  -- decoration: revealing does not move the card, so it happens while the card
  -- is still in the library.
  --
  -- Not a stylistic preference -- these two lines do not commute, in two
  -- different ways. Swapped as written, the reveal shows NOTHING: CR 400.7 has
  -- already ceased `cardId`, so Event.reveal finds no object and no-ops.
  -- Revealing the incarnation the move mints instead would record something,
  -- with the same characteristics today, and would still be the wrong act --
  -- what was shown was the card in the library, not the card in the hand.
  SearchDestination.RevealThenHand -> do
    Event.reveal RevealCause.Ordinary searcher cardId
    Event.changeZone cardId Zone.Hand
  -- Hoarding Dragon's "exile it": the move alone, with NO Event.reveal ahead of
  -- it. CR 701.23e is what makes that right rather than an omission -- the card
  -- says only "exile it", so nothing is revealed, and the exiled card being
  -- visible afterwards is CR 400.2 making exile a public zone. Which card this
  -- instruction exiled is CR 607.2a's link, filed by recordExiledWith off the
  -- effect that ran rather than here, which is what the Dragon's dies trigger
  -- reads back through ObjectRef.EachCardExiledWithSource.
  SearchDestination.Exile -> Event.changeZone cardId Zone.Exile

-- Put a library card onto the battlefield tapped (CR 701.23's Evolving Wilds
-- shape). changeZone mints a new object; tap it by id after the move.
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

-- Write a whole new order back to a player's library. Callers: the shuffle after
-- a CR 701.23 search, and the three look-and-split keyword actions -- CR
-- 701.22a's scry, CR 701.25a's surveil and CR 701.29a's fateseal -- which
-- reorder one library rather than moving anything between zones (surveil's
-- graveyard half excepted, which is a real move).
reorderLibrary :: PlayerId -> [ObjectId] -> GameState -> GameState
reorderLibrary pid order gs =
  gs {GameState.library = Map.insert pid (Seq.fromList order) (GameState.library gs)}

-- CR 701.22a: one player's scry -- look at the top n cards of their library,
-- then put any number of them on the bottom in any order and the rest back on
-- top in any order. A short library is looked at as far as it goes; rule 701.22
-- states no penalty for scrying more than there is, unlike CR 104.3c's draw.
--
-- The library is REWRITTEN rather than funnelled through Event.changeZone,
-- which every other library-facing opcode uses: nothing crosses a zone boundary
-- here, so CR 400.7 mints no new incarnation and the ids the prompt named are
-- still the ids that move. Looking mints nothing either (CR 701.20b, reached by
-- CR 701.20e).
scryOne :: Integer -> PlayerId -> Game ()
scryOne n pid = do
  gs <- State.get
  let whole = Game.zoneMembers Zone.Library pid gs
      looked = List.genericTake n whole
      beneath = List.genericDrop n whole
      -- The engine never makes the player's choice, but it does not ask a
      -- question with one answer either. Nothing to LOOK at: an empty library.
      -- Nothing to DECIDE: a single looked-at card that is the whole library,
      -- whose top and bottom are the same position. One card with more beneath
      -- it IS a decision, "any number" reaching both none and all of one.
      decided = case looked of
        [] -> False
        [_] -> not (null beneath)
        _ -> True
  Monad.when decided $ do
    answer <- Game.choose (Prompt.ChooseScry (Decide.deciderFor pid gs) pid looked)
    let (toBottom, onTop) = splitLooked looked answer
    State.modify' (reorderLibrary pid (onTop <> beneath <> toBottom))

-- Repair a look-and-split answer into the two groups the effect then moves: the
-- cards going AWAY from the top (to the bottom of a library, or to a graveyard),
-- and the cards staying on top, in the order they end up in reading down.
--
-- Filtered, deduped and COMPLETED, Effect.Discard's arm's posture and for its
-- reasons: an effect has no way to reject an answer, the answer is a list rather
-- than a set so it can repeat itself, and every looked-at card has to end up
-- somewhere. A card named in neither list stays on top behind the ones that were
-- named, which is Replay.defaultAnswer's do-nothing reading of "any number" --
-- and a card named in BOTH goes away, the first list winning so that one rule
-- settles it.
--
-- One function for all three keyword actions, since the repair is a question
-- about the ANSWER rather than about the destination: scry, surveil and fateseal
-- differ in where the first group goes and in nothing this does.
splitLooked :: [ObjectId] -> ([ObjectId], [ObjectId]) -> ([ObjectId], [ObjectId])
splitLooked looked (away, kept) =
  let named xs = List.nub (filter (\c -> List.elem c looked) xs)
      leaving = named away
      onTop = filter (\c -> List.notElem c leaving) (named kept)
      unnamed = filter (\c -> List.notElem c leaving && List.notElem c onTop) looked
   in (leaving, onTop <> unnamed)

-- CR 701.25a: one player's surveil -- look at the top n cards of their library,
-- then put any number of them into their graveyard and the rest back on top in
-- any order. scryOne's shape over a different destination, and a short library
-- is looked at as far as it goes for that function's reason.
--
-- Half of it IS a zone change, unlike scry: the graveyard cards go through
-- Event.changeZone so each mints a CR 400.7 incarnation, in the order the answer
-- named them, so the first named ends up deepest in the pile (CR 404.1 puts each
-- arrival on top). That order is the player's to pick and not the engine's,
-- which is CR 404.3 -- cards put into one graveyard at once are arranged by
-- their owner, who here is the surveilling player. The kept cards never leave
-- the library and are written back by reorderLibrary.
--
-- The ELISION is scryOne's minus its one-card case: with a lone card that is the
-- whole library, the graveyard and the top of the library are still two
-- different places, so the player is asked.
surveilOne :: Integer -> PlayerId -> Game ()
surveilOne n pid = do
  gs <- State.get
  let whole = Game.zoneMembers Zone.Library pid gs
      looked = List.genericTake n whole
      beneath = List.genericDrop n whole
  Monad.unless (null looked) $ do
    answer <- Game.choose (Prompt.ChooseSurveil (Decide.deciderFor pid gs) pid looked)
    let (toGraveyard, onTop) = splitLooked looked answer
    -- Order-independent: Game.removeFromZones takes each mover out of the
    -- library by identity rather than by position, so rewriting the kept order
    -- before or after the moves lands the same board. Written first because the
    -- kept cards are what this function decided.
    State.modify' (reorderLibrary pid (onTop <> beneath))
    Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard) toGraveyard

-- CR 701.29a: one player's fateseal -- look at the top n cards of AN OPPONENT'S
-- library, then put any number of them on the bottom of that library in any
-- order and the rest on top in any order.
--
-- TWO choices, both the fatesealer's and in this order: which opponent, then how
-- to split. The first is Prompt.ChooseOpponent, elided at one candidate the way
-- Pawl.Engine.Event's Null Chamber arm elides it (CR 102.2 leaves a two-player
-- game one opponent), and the answer is filtered rather than trusted for that
-- arm's reason. The second is scryOne's question over somebody else's library,
-- with scryOne's elisions -- and the library's owner is asked neither.
--
-- CR 102.1's opponents are Game.stillPlaying's, not GameState.turnOrder's, so a
-- seat that has left (CR 104.3a) is not offered.
fatesealOne :: ObjectId -> Integer -> PlayerId -> Game ()
fatesealOne source n pid = do
  gs <- State.get
  let opponents = filter (/= pid) (Game.stillPlaying gs)
  victim <- case opponents of
    [] -> pure Nothing
    [sole] -> pure (Just sole)
    first : second : rest -> do
      let offered = first NonEmpty.:| (second : rest)
      answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor pid gs) pid source offered)
      pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
  Monad.forM_ victim $ \owner -> do
    -- Re-read rather than reusing the state the opponent choice was made
    -- against: nothing between the two asks moves a card, but a prompt is the
    -- one place this function yields.
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

-- CR 701.44a: one permanent's explore -- its controller reveals the top card of
-- their library, puts a land card into their hand, and otherwise puts a +1/+1
-- counter on the permanent and may bin the revealed card.
--
-- The controller comes from LAST KNOWN INFORMATION (CR 701.44c), so a permanent
-- killed while the instruction was on the stack still explores under the player
-- who controlled it. Its counter then lands on nothing -- Event.putCounters
-- writes nothing for an id that is not there -- while the reveal and the choice
-- still happen, which is exactly what rule 701.44b's "even if some or all of
-- those actions were impossible" asks for.
--
-- The reveal is PUBLIC (CR 701.20a) and so rides Event.reveal, unlike scry's
-- private look at the same position. Nothing to reveal is not a land card, so an
-- empty library takes the "otherwise" branch: the counter goes on and no
-- question is put, there being no card to bin.
--
-- The land test reads the PRINTED face through Projection.viewOfCardIn, which is
-- what Effect.Mill's tally and Effect.Search's filter do.
--
-- Not implemented: those three readers all miss a continuous effect that changed
-- the card they read, the projection reaching a library card too (#160).
exploreOne :: ObjectId -> Game ()
exploreOne oid = do
  gs <- State.get
  case Projection.controllerWithLastKnown oid gs of
    -- An id nothing was ever filed under: nobody explores and nothing happens.
    Nothing -> pure ()
    Just pid -> case Game.zoneMembers Zone.Library pid gs of
      [] -> grow pid
      top : _ -> do
        Event.reveal RevealCause.Ordinary pid top
        after <- State.get
        let isLand = case Game.faceOf top after of
              Nothing -> False
              Just face -> Set.member CardType.Land (Filter.cardTypes (Projection.viewOfCardIn after top face))
        if isLand
          then Event.changeZone top Zone.Hand
          else do
            grow pid
            asked <- State.get
            decision <- Game.choose (Prompt.ChooseExplore (Decide.deciderFor pid asked) pid oid top)
            Monad.when (decision == OptionalDecision.Exercises) (Event.changeZone top Zone.Graveyard)
  where
    -- CR 122.6 through the one counter funnel, so a CR 614.16 replacement
    -- (Hardened Scales) gets its opportunity against this placement too.
    grow pid = Monad.void (Event.putCounters (CounterCause.ByEffect pid) oid CounterKind.PlusOnePlusOne 1)
