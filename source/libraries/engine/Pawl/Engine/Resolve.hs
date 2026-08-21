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
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Goad as Goad
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Phasing as Phasing
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
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Amass as Amass.Type
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.Clause as Clause
import Pawl.Types.ClauseIndex (ClauseIndex)
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamageDirection as DamageDirection
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
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Fight as Fight
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
import qualified Pawl.Types.ManaAddition as ManaAddition
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
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
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
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
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
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SetClassLevel as SetClassLevel
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
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

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

-- A Quantity evaluates against one object, so its slots are read singly.
quantitySlots :: Quantity.Type.Quantity -> Map.Map SlotName SlotArity
quantitySlots = Map.fromSet (const SlotArity.One) . Quantity.slots

-- The slots a PlayerRef reads. Only InSlot names one.
playerRefSlots :: PlayerRef -> Map.Map SlotName SlotArity
playerRefSlots ref = case ref of
  PlayerRef.EachPlayer -> Map.empty
  -- The excluded seat is one player, so one slot read singly.
  PlayerRef.EachPlayerExcept slot -> Map.singleton slot SlotArity.One
  PlayerRef.Relative _ -> Map.empty
  PlayerRef.InSlot slot -> Map.singleton slot SlotArity.One
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

-- The slots a GraveyardScope reads. Only InSlot names one, and at arity Many: the
-- reader takes the whole recipient set, so "each of up to two target players'
-- graveyards" would be seen whole.
graveyardScopeSlots :: GraveyardScope.GraveyardScope -> Map.Map SlotName SlotArity
graveyardScopeSlots scope = case scope of
  GraveyardScope.Scoped _ -> Map.empty
  GraveyardScope.InSlot slot -> Map.singleton slot SlotArity.Many

-- The slots an ObjectRef reads. InSlot names one directly and
-- EachCardInGraveyard names one through its scope; the other sweeping arms name
-- none. Reporting a scope's slot does not make the SWEPT CARDS targets -- CR
-- 115.10a needs the word "target" against them, and a graveyard scope says it
-- against the PLAYER -- so what CR 608.2b judges is still the card's own target
-- slot, holding that player, and not this read.
objectRefSlots :: ObjectRef -> Map.Map SlotName SlotArity
objectRefSlots ref = case ref of
  ObjectRef.InSlot slot -> Map.singleton slot SlotArity.Many
  ObjectRef.EachMatching _ -> Map.empty
  -- The one sweeping arm that DOES name a slot: CR 400.1's per-player graveyards
  -- leave "whose" to be said, and Angel of Finality says it by pointing at the
  -- player another slot of the same announcement targets. Reported, not dropped,
  -- because the D4 dataflow lint reads this: a card whose ONLY use of that slot
  -- is the scope would otherwise declare a target nothing reads.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard scope _) -> graveyardScopeSlots scope
  ObjectRef.EachCardInYourHand -> Map.empty
  ObjectRef.EachCardExiledWithSource {} -> Map.empty
  ObjectRef.EachSpell _ -> Map.empty
  ObjectRef.EachPlayer -> Map.empty
  ObjectRef.EachOpponent -> Map.empty
  -- The seat comes from the source's own entry choice (CR 614.12a), not a slot.
  ObjectRef.ChosenPlayer -> Map.empty
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player count) -> joinTwo (playerRefSlots player) (quantitySlots count)
  -- The arm above's two reads, and no more: the seat and the count. What a
  -- MATCH is is a Filter, and no arm here reports the slots a Filter reads --
  -- EachMatching's and the two chosen arms' each answer for their PLAYER side
  -- alone, for the reason the header states.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil player _ count) -> joinTwo (playerRefSlots player) (quantitySlots count)
  -- CR 109.5: whose graveyards names no slot; who CHOOSES may.
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser _ _) -> chooserSlots chooser
  -- CR 402.3: the choosers own the hands, so the PlayerRef is the whole read.
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand player _) -> playerRefSlots player
  -- The first of the two arms whose CANDIDATES come from a slot -- the plural is
  -- below -- where the two chosen arms above name a slot only through their
  -- chooser. Reported for EachCardInGraveyard's
  -- reason -- the D4 dataflow lint reads this, and a group one clause binds and a
  -- later one reads is exactly the dataflow that lint checks. Many, not One,
  -- which is the arity InSlot reports of the same binding: the ref reads every
  -- member of the group to offer them.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong slot _) -> Map.singleton slot SlotArity.Many
  -- The arm above's read, for its reasons: the candidates come from a slot, and
  -- the ref reads every member of the group to match them.
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong slot _) -> Map.singleton slot SlotArity.Many
  -- The seats whose hands randomness reads: the arm above's read.
  ObjectRef.RandomCardInHand player -> playerRefSlots player

-- The Quantities an ObjectRef carries: the two library walks' counts.
-- Exhaustive, no wildcard: slotsAreExhaustive, readsX and Pawl.CardSpec's Count traversal all
-- reach a nested Quantity through this, and their own ObjectRef-taking arms are
-- written `{}` and answer a constant.
objectRefQuantities :: ObjectRef -> [Quantity.Type.Quantity]
objectRefQuantities ref = case ref of
  ObjectRef.InSlot _ -> []
  ObjectRef.EachMatching _ -> []
  ObjectRef.EachCardInGraveyard {} -> []
  ObjectRef.EachCardInYourHand -> []
  ObjectRef.EachCardExiledWithSource {} -> []
  ObjectRef.EachSpell _ -> []
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  ObjectRef.ChosenPlayer -> []
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary _ count) -> [count]
  -- The arm above's count, measured in MATCHES rather than in cards.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil _ _ count) -> [count]
  ObjectRef.ChosenCardInGraveyard {} -> []
  ObjectRef.ChosenCardInHand {} -> []
  ObjectRef.ChosenCardFromAmong {} -> []
  ObjectRef.EachCardFromAmong {} -> []
  ObjectRef.RandomCardInHand _ -> []

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
slotsOf :: Effect Card.Type.Card -> Map.Map SlotName SlotArity
slotsOf effect = case effect of
  -- The dealer is a read like any other (CR 120.2b), and one object (CR 120.1).
  Effect.DealDamage (DealDamage.MkDealDamage ref quantity dealer _) ->
    joinTwo
      (joinTwo (objectRefSlots ref) (quantitySlots quantity))
      (maybe Map.empty oneSlot dealer)
  -- BOTH fighters: CR 701.14a reads each one's power against the other, so a
  -- slot named by only one half would still look dangling.
  Effect.Fight (Fight.MkFight first second) -> joinTwo (oneSlot first) (oneSlot second)
  -- The modification's own quantities read slots too, through
  -- Projection.quantitiesOf.
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    joinSlots [objectRefSlots ref, joinSlots (fmap quantitySlots (Projection.quantitiesOf modification)), durationSlots duration]
  Effect.ChangeText (ChangeText.MkChangeText _ _ slot) -> oneSlot slot
  Effect.AddMana (ManaAddition.MkManaAddition ref _ _ _) -> playerRefSlots ref
  -- BOTH refs: a slot read only by the owner ref would otherwise look dangling.
  Effect.Search (Search.MkSearch searcher owner quantity _ _ _) ->
    joinSlots [playerRefSlots searcher, playerRefSlots owner, quantitySlots quantity]
  Effect.ExileAllGraveyards -> Map.empty
  Effect.Proliferate -> Map.empty
  Effect.Bolster quantity -> quantitySlots quantity
  Effect.Amass (Amass.Type.MkAmass quantity _) -> quantitySlots quantity
  Effect.Blight (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.TemptWithTheRing -> Map.empty
  Effect.Venture -> Map.empty
  Effect.ExileHandThenDraw -> Map.empty
  -- CR 101.4's "each player sacrifices": the arm takes every player recipient
  -- the slot holds, so the read is Many.
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot _ quantity) -> joinTwo (Map.singleton slot SlotArity.Many) (quantitySlots quantity)
  -- CR 727.5's exemption is an ObjectRef like any other.
  Effect.RestartGame exempt -> foldMap objectRefSlots exempt
  Effect.ControlPlayerNextTurn slot -> oneSlot slot
  -- The three slot fields are DEFINITIONS, not reads; they belong to boundSlots
  -- below.
  Effect.Destroy (Destroy.MkDestroy ref _ _ _ _) -> objectRefSlots ref
  Effect.Sacrifice slot -> oneSlot slot
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown slot _) -> oneSlot slot
  Effect.TurnFaceUp slot -> oneSlot slot
  Effect.RemoveFromCombat slot -> oneSlot slot
  Effect.BecomesBlocked slot -> oneSlot slot
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ _ _) -> objectRefSlots ref
  Effect.Draw (PlayerQuantity.MkPlayerQuantity ref quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  -- The tally's slot and CR 701.17c's are DEFINITIONS, not reads: see boundSlots
  -- below.
  Effect.Mill (Mill.MkMill ref quantity _ _) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  -- The bound slot is a DEFINITION, not a read.
  Effect.Reveal (Reveal.MkReveal ref _) -> objectRefSlots ref
  -- The bound slot is a DEFINITION, not a read.
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
  Effect.DecreaseSpeed d -> joinTwo (playerRefSlots (SpeedDecrease.player d)) (quantitySlots (SpeedDecrease.quantity d))
  -- Create's slot is a DEFINITION, not a read, so the lint must not see it here.
  -- CR 111.2's creator is a READ: Rampage of the Clans names the controller of
  -- the permanent the loop around it bound.
  Effect.Create (Create.MkCreate quantity _ _ _ creator) -> joinTwo (quantitySlots quantity) (playerRefSlots creator)
  -- A READ, unlike Create's slot: the ref names the permanent being copied.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref) -> joinTwo (quantitySlots quantity) (objectRefSlots ref)
  -- Both refs: either may name a slot.
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject) -> joinTwo (objectRefSlots original) (objectRefSlots subject)
  -- The Duration and Condition each carry Quantities; a Quantity.InSlot is a read.
  -- The pattern's Filter is a READ too: Filter.IsBound in one names an object an
  -- earlier effect of this same resolution defined (Dire Fleet Daredevil's "that
  -- spell"), which is what the row's captured environment answers at CR 616.1.
  Effect.Replace (Replace.MkReplace duration _ _ condition re) ->
    joinSlots [durationSlots duration, joinSlots (fmap conditionSlots (Maybe.maybeToList condition)), replacementPatternSlots re]
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase ref _) -> playerRefSlots ref
  -- CR 615.5's rider reads slots of its own, so its reads join this effect's,
  -- LESS the reserved amount slot: the prevention binds that one itself
  -- (Event.eventBindingSlots), and Resolve.runPreventionRider is the writer.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ ref _ quantity rider) ->
    joinSlots
      [ durationSlots duration,
        objectRefSlots ref,
        quantitySlots quantity,
        Map.delete Binding.eventAmount (joinSlots (fmap slotsOf (Foldable.toList rider)))
      ]
  -- The same reads, minus the shield size this opcode does not carry.
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ ref _ rider) ->
    joinSlots
      [ durationSlots duration,
        objectRefSlots ref,
        Map.delete Binding.eventAmount (joinSlots (fmap slotsOf (Foldable.toList rider)))
      ]
  -- BOTH ObjectRefs: a target slot may be read through the destination ref.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ srcRef destRef) ->
    joinSlots [durationSlots duration, objectRefSlots srcRef, objectRefSlots destRef]
  -- The bound slot is a DEFINITION, not a read: see boundSlots below.
  Effect.Counter (Counter.MkCounter ref _) -> objectRefSlots ref
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity ref) -> joinTwo (objectRefSlots ref) (quantitySlots quantity)
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity slot) -> insertOne slot (quantitySlots quantity)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters ref _ quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters ref _ quantity) -> joinTwo (playerRefSlots ref) (quantitySlots quantity)
  -- The SlotName is a DEFINITION, not a read; it belongs to boundSlots below.
  Effect.PayAnyEnergy _ -> Map.empty
  Effect.Tap ref -> objectRefSlots ref
  Effect.Untap ref -> objectRefSlots ref
  Effect.Detain ref -> objectRefSlots ref
  Effect.Goad ref -> objectRefSlots ref
  Effect.DoesNotUntapNext ref -> objectRefSlots ref
  Effect.Transform ref -> objectRefSlots ref
  Effect.PhaseOut ref -> objectRefSlots ref
  Effect.AddPhases _ -> Map.empty
  Effect.GainControl (DurationRef.MkDurationRef _ ref) -> objectRefSlots ref
  Effect.ArmDelayedTrigger {} -> Map.empty
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers _ affected _) -> affectedPlayersSlots affected
  Effect.RequireBlock (RequireBlock.MkRequireBlock _ blocker attacker) -> joinTwo (objectRefSlots blocker) (objectRefSlots attacker)
  Effect.RequireAttack (RequireAttack.MkRequireAttack _ attacker defender) -> joinTwo (objectRefSlots attacker) (playerRefSlots defender)
  Effect.CreateEmblem {} -> Map.empty
  -- CR 725.1's crown names a target slot only in the InSlot arm.
  Effect.BecomeMonarch target -> monarchTargetSlots target
  -- A READ: the slot names the permanent gaining the designation.
  Effect.Designate (Designate.MkDesignate _ slot) -> oneSlot slot
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ slot) -> oneSlot slot
  -- A READ of whatever slot the ref names; CR 701.60a's ending can reach a set.
  Effect.Unsuspect ref -> objectRefSlots ref
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
  -- CR 729.1/729.1b: the slot is a DEFINITION (the subgame's winner), not a read.
  Effect.PlaySubgame _ -> Map.empty
  -- A DEFINITION too: chosen as this effect is applied (CR 608.2d), never read.
  Effect.ChooseOpponent _ -> Map.empty
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn ref _) -> playerRefSlots ref
  -- Both halves may name a slot: what is shuffled, and whose library.
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named ref) -> joinTwo (maybe Map.empty playerRefSlots named) (objectRefSlots ref)
  -- BOTH are reads: the slot is bound by an earlier effect of the list (CR 400.7).
  Effect.OfferCast (OfferCast.MkOfferCast slot caster _ _) -> joinTwo (oneSlot slot) (playerRefSlots caster)
  -- Both reads: the ref is normally bound earlier in the same list (CR 400.7).
  Effect.GrantPlayFromExile grant -> joinTwo (durationSlots (GrantPlayFromExile.duration grant)) (objectRefSlots (GrantPlayFromExile.ref grant))
  -- The swept ref and everything the BODY reads. The loop's own slot is NOT
  -- subtracted as the rider's reserved slot is: boundSlots below defines it.
  Effect.ForEach (ForEach.MkForEach ref _ body) ->
    joinTwo (objectRefSlots ref) (joinSlots (fmap slotsOf (Foldable.toList body)))

-- CR 611.2b: only ForAsLongAs carries a Quantity, through its Condition.
durationSlots :: Duration.Duration -> Map.Map SlotName SlotArity
durationSlots duration = case duration of
  Duration.UntilEndOfTurn -> Map.empty
  Duration.Indefinite -> Map.empty
  Duration.UntilYourNextTurn -> Map.empty
  Duration.UntilEndOfYourNextTurn -> Map.empty
  Duration.ForAsLongAs condition -> conditionSlots condition
  Duration.UntilEndOfCombat -> Map.empty

-- Every slot a whole MODE reads: its effects', every payer CR 118.12a's "unless
-- [a player] pays" names, and every slot a target slot's own pool or filter
-- names. A payer or pool slot no effect also reads would otherwise dangle.
modeSlots :: Mode.Mode Card.Type.Card -> Map.Map SlotName SlotArity
modeSlots mode =
  joinSlots
    [ joinSlots (fmap slotsOf (Foldable.toList (Mode.allEffects mode))),
      joinSlots (fmap payerSlot (Foldable.toList (Mode.clauses mode))),
      joinSlots (fmap (poolSlot . TargetSlot.pool) (Map.elems (Mode.targetSlots mode))),
      -- And every slot a target slot's own FILTER names -- CR 603.2's "target
      -- artifact or enchantment that player controls".
      joinSlots (fmap filterSlots (Map.elems (Mode.targetSlots mode)))
    ]
  where
    filterSlots =
      maybe Map.empty (Map.fromSet (const SlotArity.One) . Filter.boundSlots)
        . TargetSlot.filter
    -- Every clause's payer: CR 118.12 scopes a resolution cost to its clause.
    payerSlot = maybe Map.empty (playerRefSlots . PayGate.payer) . Clause.payGate

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
  Pool.CardsInGraveyard scope -> case scope of
    GraveyardScope.Scoped _ -> Map.empty
    GraveyardScope.InSlot slot -> oneSlot slot
  Pool.CardsInExile -> Map.empty
  -- The graveyard half's scope; the battlefield half names no slot.
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

-- The slots a replacement's PATTERN names: every arm's pattern says which objects
-- the row applies to as a Filter, and Filter.IsBound in one is a read of the
-- installing resolution's binding environment (Pawl.Types.ActiveReplacement).
-- Arity One, since IsBound is a membership test rather than a target slot.
--
-- No wildcard: an arm added to Pawl.Types.ReplacementEffect must answer here, and
-- the three that carry no Filter say so rather than falling through. The nested
-- EFFECTS an EntryR rewrite or a DamageR rider carries are not walked -- see
-- slotsAreExhaustive's Replace arm.
replacementPatternSlots :: ReplacementEffect.ReplacementEffect (Effect Card.Type.Card) -> Map.Map SlotName SlotArity
replacementPatternSlots re = case re of
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR pat _) -> filterSlotsOf (ZoneChangePattern.whatObject pat)
  ReplacementEffect.EntryR (EntryR.MkEntryR pat _) -> filterSlotsOf pat
  ReplacementEffect.DamageR (DamageR.MkDamageR pat _ _) ->
    joinTwo (filterSlotsOf (DamagePattern.whatSource pat)) (joinSlots (fmap filterSlotsOf (Maybe.maybeToList (DamagePattern.whatRecipient pat))))
  ReplacementEffect.DestructionR _ -> Map.empty
  ReplacementEffect.CounterR (CounterR.MkCounterR pat _) -> filterSlotsOf (CounterPattern.onWhat pat)
  ReplacementEffect.TokenR _ -> Map.empty
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR pat _) -> filterSlotsOf pat
  ReplacementEffect.PhaseR _ -> Map.empty

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
-- No wildcard: a new opcode must answer here as well as in slotsOf. The `{}` arms
-- answer a constant, so a new FIELD on one is not forced -- hence the four
-- ObjectRef-taking opcodes routed through objectRefQuantities.
slotsAreExhaustive :: Effect Card.Type.Card -> Bool
slotsAreExhaustive effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage _ quantity _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.Fight {} -> True
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) ->
    durationSlotsAreExhaustive duration
      && all Quantity.slotsAreExhaustive (Projection.quantitiesOf modification)
  Effect.ChangeText {} -> True
  Effect.AddMana _ -> True
  Effect.Search (Search.MkSearch _ _ quantity _ _ _) -> Quantity.slotsAreExhaustive quantity
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
  Effect.TurnFaceUp _ -> True
  Effect.RemoveFromCombat _ -> True
  Effect.BecomesBlocked _ -> True
  -- Three of the four whose ref may nest a Quantity; ForEach is the fourth.
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ _ _) -> all Quantity.slotsAreExhaustive (objectRefQuantities ref)
  Effect.Draw (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Mill (Mill.MkMill _ quantity _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.Reveal (Reveal.MkReveal ref _) -> all Quantity.slotsAreExhaustive (objectRefQuantities ref)
  Effect.LookAt (LookAt.MkLookAt ref _) -> all Quantity.slotsAreExhaustive (objectRefQuantities ref)
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
  -- CR 111.1's token is minted with empty bindings, so its card is literal text.
  Effect.Create (Create.MkCreate quantity _ _ _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.BecomeCopy {} -> True
  -- The ReplacementEffect holds no Quantity, and the one reference it can hold --
  -- a Filter.IsBound in its pattern -- slotsOf reports through
  -- replacementPatternSlots. Not implemented: the effects a rewrite or a CR 615.5
  -- rider nests under this opcode read slots of their own and neither this answer
  -- nor slotsOf reports them; every Effect.Replace in data/cards/ nests none
  -- (gap #1962).
  Effect.Replace (Replace.MkReplace duration _ _ condition _) ->
    durationSlotsAreExhaustive duration && all conditionSlotsAreExhaustive condition
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> True
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ _ _ quantity rider) ->
    durationSlotsAreExhaustive duration && Quantity.slotsAreExhaustive quantity && all slotsAreExhaustive rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ _ _ rider) ->
    durationSlotsAreExhaustive duration && all slotsAreExhaustive rider
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ _ _) -> durationSlotsAreExhaustive duration
  Effect.Counter {} -> True
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.PayAnyEnergy _ -> True
  Effect.Tap _ -> True
  Effect.Untap _ -> True
  Effect.Detain _ -> True
  Effect.Goad _ -> True
  Effect.DoesNotUntapNext _ -> True
  Effect.Transform _ -> True
  Effect.PhaseOut _ -> True
  Effect.AddPhases _ -> True
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
  -- RequireBlock's reason, one axis over.
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CR 114.2's emblem is minted with EMPTY bindings, so its card is literal text.
  Effect.CreateEmblem _ -> True
  Effect.BecomeMonarch MonarchTarget.TheController -> True
  -- The one arm answering NO: CR 725.2 reads Binding.triggerSource.
  Effect.BecomeMonarch MonarchTarget.ControllerOfSource -> False
  Effect.BecomeMonarch (MonarchTarget.InSlot _) -> True
  Effect.Designate (Designate.MkDesignate _ _) -> True
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> True
  Effect.Unsuspect _ -> True
  Effect.Evolve _ -> True
  Effect.Mentor _ -> True
  Effect.Train _ -> True
  Effect.ItBecomes _ -> True
  Effect.ExileUntilMonarch _ -> True
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting _ _) -> True
  Effect.Attach _ -> True
  Effect.AttachTarget (AttachTarget.MkAttachTarget _ _) -> True
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget _ _) -> True
  -- CR 729.1b: a DEFINITION, and the subgame reads no binding of the outer game.
  Effect.PlaySubgame _ -> True
  -- PlaySubgame's answer: a definition reads no slot.
  Effect.ChooseOpponent _ -> True
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn _ _) -> True
  Effect.ShuffleIntoLibrary {} -> True
  Effect.OfferCast {} -> True
  Effect.GrantPlayFromExile grant -> durationSlotsAreExhaustive (GrantPlayFromExile.duration grant)
  -- PreventNextDamage's answer for the body, plus the fourth ObjectRef-taking
  -- opcode's ref: a PlayerRef nested in the DEPTH is one slotsOf cannot see.
  Effect.ForEach (ForEach.MkForEach ref _ body) -> all Quantity.slotsAreExhaustive (objectRefQuantities ref) && all slotsAreExhaustive body

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
-- cost (CR 107.3, CR 107.3a, CR 118.4), the same reads-equal-declares contract
-- slotsOf draws for target slots.
--
-- NOTE: when an opcode gains a Quantity FIELD, add its arm here by hand. A new
-- OPCODE the compiler forces, this case being exhaustive; widening an existing
-- one it does not, since an arm written `{} -> False` keeps compiling. The four
-- ObjectRef-taking opcodes route through objectRefQuantities for that reason.
readsX :: [Effect Card.Type.Card] -> Bool
readsX = any effectReadsX
  where
    effectReadsX effect = case effect of
      Effect.DealDamage (DealDamage.MkDealDamage _ quantity _ _) -> Quantity.readsX quantity
      Effect.Fight {} -> False
      -- Untamed Might's "+X/+X" sits inside the Modification, not on the effect.
      Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ modification _) -> any Quantity.readsX (Projection.quantitiesOf modification)
      Effect.ChangeText {} -> False
      Effect.AddMana _ -> False
      Effect.Search (Search.MkSearch _ _ quantity _ _ _) -> Quantity.readsX quantity
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
      Effect.TurnFaceUp _ -> False
      Effect.RemoveFromCombat _ -> False
      Effect.BecomesBlocked _ -> False
      -- Commune with Lava's X sits inside the ObjectRef rather than beside it, so
      -- these three -- and ForEach below -- go through objectRefQuantities.
      Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ _ _) -> any Quantity.readsX (objectRefQuantities ref)
      Effect.Draw (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Mill (Mill.MkMill _ quantity _ _) -> Quantity.readsX quantity
      Effect.Reveal (Reveal.MkReveal ref _) -> any Quantity.readsX (objectRefQuantities ref)
      Effect.LookAt (LookAt.MkLookAt ref _) -> any Quantity.readsX (objectRefQuantities ref)
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
      Effect.Create (Create.MkCreate quantity _ _ _ _) -> Quantity.readsX quantity
      Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _) -> Quantity.readsX quantity
      Effect.BecomeCopy {} -> False
      Effect.Replace {} -> False
      Effect.SkipNextPhase {} -> False
      -- CR 601.2b's X reaches the rider too.
      Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ quantity rider) -> Quantity.readsX quantity || readsX (Foldable.toList rider)
      Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ rider) -> readsX (Foldable.toList rider)
      Effect.RedirectDamage {} -> False
      Effect.Counter {} -> False
      Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.readsX quantity
      Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.readsX quantity
      Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      -- CR 107.14's amount is asked for as the spell resolves, never CR
      -- 601.2b's announced X.
      Effect.PayAnyEnergy _ -> False
      Effect.Tap _ -> False
      Effect.Untap _ -> False
      Effect.Detain _ -> False
      Effect.Goad _ -> False
      Effect.DoesNotUntapNext _ -> False
      Effect.Transform _ -> False
      Effect.PhaseOut _ -> False
      Effect.AddPhases _ -> False
      Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
      Effect.ArmDelayedTrigger {} -> False
      Effect.AffectPlayers {} -> False
      Effect.RequireBlock {} -> False
      Effect.RequireAttack {} -> False
      Effect.CreateEmblem {} -> False
      Effect.BecomeMonarch {} -> False
      Effect.Designate (Designate.MkDesignate _ _) -> False
      Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> False
      Effect.Unsuspect _ -> False
      Effect.Evolve _ -> False
      Effect.Mentor _ -> False
      Effect.Train _ -> False
      Effect.ItBecomes _ -> False
      Effect.ExileUntilMonarch _ -> False
      Effect.ExileHaunting {} -> False
      Effect.Attach _ -> False
      Effect.AttachTarget {} -> False
      Effect.AttachTargetToEach {} -> False
      Effect.PlaySubgame _ -> False
      Effect.ChooseOpponent _ -> False
      Effect.TakeExtraTurn {} -> False
      Effect.ShuffleIntoLibrary {} -> False
      Effect.OfferCast {} -> False
      Effect.GrantPlayFromExile {} -> False
      -- CR 608.2f's body is an effect list like any other, so an X inside it counts.
      Effect.ForEach (ForEach.MkForEach ref _ body) -> any Quantity.readsX (objectRefQuantities ref) || readsX (Foldable.toList body)

-- CR 601.3 (Panglacial): does this effect search a library? Stack asks before
-- resolving, to offer the cast-while-searching opportunity.
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
  Effect.Fight {} -> False
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
  Effect.TurnFaceUp _ -> False
  Effect.RemoveFromCombat _ -> False
  Effect.BecomesBlocked _ -> False
  Effect.MoveToZone {} -> False
  Effect.Draw {} -> False
  Effect.Mill {} -> False
  -- These show NAMED cards; CR 701.23a's search looks through a whole zone.
  Effect.Reveal {} -> False
  Effect.LookAt {} -> False
  Effect.Scry {} -> False
  Effect.Surveil {} -> False
  Effect.Fateseal {} -> False
  -- CR 701.44a reveals the top card; CR 701.23a's search looks through a zone.
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
  Effect.BecomeCopy {} -> False
  Effect.Replace {} -> False
  Effect.SkipNextPhase {} -> False
  -- CR 615.5's rider is not descended into: this classification is asked of the
  -- RESOLUTION about to run, and the rider runs later, outside any resolution.
  Effect.PreventNextDamage {} -> False
  Effect.PreventAllDamage {} -> False
  Effect.RedirectDamage {} -> False
  Effect.Counter {} -> False
  Effect.PutCounters {} -> False
  Effect.RemoveCounters {} -> False
  Effect.GainPlayerCounters {} -> False
  Effect.RemovePlayerCounters {} -> False
  Effect.PayAnyEnergy _ -> False
  Effect.Tap _ -> False
  Effect.Untap _ -> False
  Effect.Detain _ -> False
  Effect.Goad _ -> False
  Effect.DoesNotUntapNext _ -> False
  Effect.Transform _ -> False
  Effect.PhaseOut _ -> False
  Effect.AddPhases _ -> False
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
  Effect.ArmDelayedTrigger {} -> False
  Effect.AffectPlayers {} -> False
  Effect.RequireBlock {} -> False
  Effect.RequireAttack {} -> False
  Effect.CreateEmblem {} -> False
  Effect.BecomeMonarch {} -> False
  Effect.Designate (Designate.MkDesignate _ _) -> False
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> False
  Effect.Unsuspect _ -> False
  Effect.Evolve _ -> False
  Effect.Mentor _ -> False
  Effect.Train _ -> False
  Effect.ItBecomes _ -> False
  Effect.ExileUntilMonarch _ -> False
  Effect.ExileHaunting {} -> False
  Effect.Attach _ -> False
  Effect.AttachTarget {} -> False
  Effect.AttachTargetToEach {} -> False
  Effect.PlaySubgame _ -> False
  Effect.ChooseOpponent _ -> False
  -- CR 701.24 shuffles a library but never LOOKS at one (CR 701.23a).
  Effect.ShuffleIntoLibrary {} -> False
  -- CR 608.2g's other producer, and not a search: the cast names one known object.
  Effect.OfferCast {} -> False
  Effect.GrantPlayFromExile {} -> False
  Effect.TakeExtraTurn {} -> False
  -- Descended into, unlike PreventNextDamage's rider: this body runs INSIDE the
  -- resolution being asked about (CR 608.2f).
  Effect.ForEach (ForEach.MkForEach _ _ body) -> any searchesLibrary body

-- CR 603.7: the delayed abilities an effect list ARMS, by name.
armedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- CR 603.7: armedAbilities narrowed to the arms whose firing is gated past the
-- turn that armed them, i.e. not Onset.Immediately.
onsetGatedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
onsetGatedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger _ Onset.Immediately _) -> Nothing
        Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name _ _) -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- boundSlots over a whole effect list: the write half of the dataflow lint.
definedSlots :: [Effect Card.Type.Card] -> Set SlotName
definedSlots = foldMap boundSlots

-- definedSlots' other half, one MODE at a time: the slot a CR 118.12 gate binds
-- as it is answered (Binding.gatePlayers, stamped by payGateAdmits). A mode
-- stating no gate binds nothing, so a card reading that name without offering a
-- resolution cost is still caught by the dataflow lint.
gateDefinedSlots :: Mode.Mode card -> Set SlotName
gateDefinedSlots mode
  | any (Maybe.isJust . Clause.payGate) (Mode.clauses mode) = Set.singleton Binding.gatePlayers
  | otherwise = Set.empty

-- slotsOf's mirror for ONE effect: the slots it BINDS rather than reads, which
-- is also the set Pawl.CardSpec's reserved-name sweep ranges over. Exhaustive
-- deliberately: a wildcard would file a new bind position under "binds nothing"
-- in both the dataflow lint and that sweep, with no diagnostic.
boundSlots :: Effect Card.Type.Card -> Set SlotName
boundSlots effect = case effect of
  -- CR 400.7: the incarnation minted at the destination.
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ _ mSlot _ _) -> foldMap Set.singleton mSlot
  -- The tokens this Create minted, for CR 603.7c's delayed trigger to name.
  Effect.Create (Create.MkCreate _ _ _ mSlot _) -> foldMap Set.singleton mSlot
  Effect.CreateCopy {} -> Set.empty
  -- Binds nothing: no new object comes into existence.
  Effect.BecomeCopy {} -> Set.empty
  -- CR 729.1b: the subgame's winner, reported rather than chosen.
  Effect.PlaySubgame slot -> Set.singleton slot
  -- CR 608.2d: the opponent this effect chose.
  Effect.ChooseOpponent slot -> Set.singleton slot
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
  Effect.TurnFaceUp _ -> Set.empty
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
  -- The shield itself binds nothing; CR 615.5's rider is an effect list, so a
  -- name IT authors is a name this card authors. Both shields.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ rider) -> foldMap boundSlots rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ rider) -> foldMap boundSlots rider
  Effect.RedirectDamage {} -> Set.empty
  -- How many spells this countering ACTUALLY countered, for a "for each spell
  -- countered this way".
  Effect.Counter (Counter.MkCounter _ mSlot) -> foldMap Set.singleton mSlot
  Effect.PutCounters {} -> Set.empty
  Effect.RemoveCounters {} -> Set.empty
  Effect.GainPlayerCounters {} -> Set.empty
  Effect.RemovePlayerCounters {} -> Set.empty
  -- CR 107.14: how much {E} the payer paid, for a later effect of the same
  -- resolution to read as Quantity.InSlot.
  Effect.PayAnyEnergy slot -> Set.singleton slot
  Effect.Tap _ -> Set.empty
  Effect.Untap _ -> Set.empty
  Effect.Detain _ -> Set.empty
  Effect.Goad _ -> Set.empty
  Effect.DoesNotUntapNext _ -> Set.empty
  Effect.Transform _ -> Set.empty
  Effect.PhaseOut _ -> Set.empty
  Effect.AddPhases _ -> Set.empty
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> Set.empty
  Effect.ArmDelayedTrigger {} -> Set.empty
  Effect.AffectPlayers {} -> Set.empty
  Effect.RequireBlock {} -> Set.empty
  Effect.RequireAttack {} -> Set.empty
  Effect.CreateEmblem {} -> Set.empty
  Effect.BecomeMonarch {} -> Set.empty
  Effect.Designate (Designate.MkDesignate _ _) -> Set.empty
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> Set.empty
  Effect.Unsuspect _ -> Set.empty
  Effect.Evolve _ -> Set.empty
  Effect.Mentor _ -> Set.empty
  Effect.Train _ -> Set.empty
  Effect.ItBecomes _ -> Set.empty
  Effect.ExileUntilMonarch _ -> Set.empty
  Effect.ExileHaunting {} -> Set.empty
  Effect.Attach _ -> Set.empty
  Effect.AttachTarget {} -> Set.empty
  Effect.AttachTargetToEach {} -> Set.empty
  Effect.TakeExtraTurn {} -> Set.empty
  Effect.ShuffleIntoLibrary {} -> Set.empty
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
-- a state-based action. An UNDETERMINABLE quantity is left standing, which keeps
-- CR 208.2's star working for Projection.seedCharacteristicPT. P/T is the whole
-- of it: the only printed characteristics a Face holds as a Quantity.
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
-- with every text change affecting it applied (CR 612). Modes rather than a flat
-- effect list because CR 603.5's "may" belongs to a clause within a mode.
--
-- The mode's TARGET SLOTS are rewritten by targetSlotsOf instead: CR 608.2b
-- re-reads them off the printed face, which unions in CR 303.4a's enchant slot.
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
                -- A clause gate's Filters are printed words CR 612.1 changes.
                Clause.condition = fmap (Projection.rewriteCondition changes) (Clause.condition c)
              }
          rewriteMode m = m {Mode.clauses = fmap rewriteClause (Mode.clauses m)}
       in fmap (fmap rewriteMode) (Card.chosenModes chosen face)

-- CR 608.2b: a target can stop being legal because an effect changed the spell's
-- text, so the re-check measures the printed slots with CR 612.1's changes
-- applied. Off the face rather than off modesOf, which has no room for CR
-- 303.4a's enchant slot. Both readers -- the fizzle test and resolveSpellWith's
-- per-effect skip -- go through this, so they cannot disagree.
targetSlotsOf :: Object.Object -> ObjectId -> GameState -> Face.Face Card.Type.Card -> Map.Map SlotName TargetSlot.TargetSlot
targetSlotsOf obj oid gs face =
  fmap
    (Projection.rewriteTargetSlot (Projection.textChangesAffecting oid gs))
    (Card.modesTargetSlots (Binding.modesOf (Object.bindings obj)) face)

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
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just (spellController obj oid gs)) chosen oid recipient targetSlot gs) recipients
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
            -- The slots as FILLED, which a slot-scoped pool is re-derived
            -- against (Target.stillLegal).
            chosen = Binding.targetsOf (Object.bindings obj)
            -- CR 700.2d: the slots the MODES own -- `slots` minus CR 303.4a's
            -- enchant slot.
            modeOwnedSlots = Modal.modesTargetSlots chosenSelection (Face.spell face)
            legalSlot slot recipients = case Map.lookup slot slots of
              -- CR 608.2b is about TARGETS. A slot declaring none is a RESERVED
              -- binding and was never targeted.
              Nothing -> recipients
              -- Per RECIPIENT and not per slot (CR 608.2b): the slot's surviving
              -- targets are still affected.
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
                  -- CR 608.2e's clause is the unit all three gates cover, so each
                  -- is asked once per clause. The fold carries this mode
                  -- INSTANCE's CR 118.12 answers, per instance because CR 700.2d
                  -- makes a mode chosen twice make its offer twice.
                  Monad.foldM_
                    ( \answers (cIdx, clause) -> do
                        -- CR 701.46a's printed "if" first, against the LIVE
                        -- bindings (CR 608.2c): a slot an earlier clause DEFINED
                        -- is part of the state this one is read against, and the
                        -- re-read adds only defined slots. A REGRESSION FENCE --
                        -- mutating this half back leaves the suite green.
                        gateBindings <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject oid)
                        gated <- gateHolds effectController oid (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Binding.targetsOf gateBindings)) (Binding.groupsOf gateBindings) clause
                        -- CR 603.5 / 608.2d: then the printed "may", against the
                        -- SAME live bindings CR 608.2b's filter is applied to, so
                        -- a clause whose every read is dead is not asked about.
                        let legalNowForMay = Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                            boundNowForMay = Map.keysSet (Modal.instanceView modeOwnedSlots mi (Mode.targetSlots mode) gateBindings)
                        taken <- if gated then exercises oid effectController idx cIdx boundNowForMay legalNowForMay clause else pure False
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
                                    answers
                                    clause
                            else pure (False, answers)
                        Monad.when admitted (Monad.mapM_ applyOne (Clause.effects clause))
                        pure answers'
                    )
                    Map.empty
                    (zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode)))
                finishSpell oid face effectController

-- CR 608.2n / 715.3d: where the spell goes as the last part of its resolution --
-- its owner's graveyard, unless it was cast as an Adventure, when its controller
-- exiles it and CR 715.3d's permission to play it goes onto the exiled card.
--
-- Reached only from the RESOLVING path: a fizzled spell does not resolve (CR
-- 608.2b), so CR 715.3d's "as it resolves" never applies to it. Written onto the
-- id the move RETURNS, since CR 400.7 mints a fresh incarnation in exile.
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
resolveModesWith :: Game Result -> ObjectId -> ObjectId -> [(ModeInstance, Mode.Mode Card.Type.Card)] -> Game ()
resolveModesWith runSubgame stackId srcId modes = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      -- CR 700.2d: instance-named, not printed-named -- two instances of one
      -- repeated mode fill two slots this union would otherwise collapse. CR
      -- 608.2b re-judges each against the SAME declaration CR 603.3d offered, so
      -- the "that player controls" atoms are baked here too; an ability whose
      -- environment binds no player leaves them standing, admitting nothing.
      let slots = Target.bakeSlots (Binding.playerSlots (Object.bindings obj)) (Map.unions (fmap (\(mi, mode) -> Map.mapKeys (Modal.instanceSlot mi) (Mode.targetSlots mode)) modes))
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipients = case Map.lookup slot slots of
            -- CR 608.2b is about TARGETS. A slot declaring none is a RESERVED
            -- binding and can never have become an illegal target.
            Nothing -> recipients
            -- CR 608.2b: the perspective is the ABILITY's controller. `srcId`
            -- stays the source (CR 113.7) and may well be gone -- exactly the
            -- case this rule is about. Judged per RECIPIENT.
            Just targetSlot -> Set.filter (\recipient -> Target.stillLegal (Just effectController) chosen srcId recipient targetSlot gs) recipients
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
                  bindingsNow <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject stackId)
                  let chosenNow = Binding.targetsOf bindingsNow
                      legalNow = Map.mapWithKey legalSlot chosenNow
                  applyEffectWith runSubgame stackId srcId effectController (instanceView legalNow) (instanceView chosenNow) eff
             in -- CR 608.2e's clause is what each gate covers. Run only when
                -- `fizzles` is False.
                Monad.foldM_
                  ( \answers (cIdx, clause) -> do
                      -- CR 701.46a's printed "if" first, read against `srcId` --
                      -- the rule says "this permanent", which is also why
                      -- `payGatePaid` is given `srcId`. Off the LIVE bindings of
                      -- the STACK object (CR 608.2c), where this resolution's
                      -- slots are bound (see bindSlot).
                      gateBindings <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject stackId)
                      gated <- gateHolds effectController srcId (instanceView (Binding.targetsOf gateBindings)) (Binding.groupsOf gateBindings) clause
                      -- CR 603.5 / 608.2d: then the printed "may", against the
                      -- SAME live bindings CR 608.2b's filter is applied to, so a
                      -- clause whose every read is dead is not asked about.
                      let legalNowForMay = instanceView (Map.mapWithKey legalSlot (Binding.targetsOf gateBindings))
                          boundNowForMay = Map.keysSet (instanceView gateBindings)
                      taken <- if gated then exercises stackId effectController idx cIdx boundNowForMay legalNowForMay clause else pure False
                      -- CR 118.12: then the cost paid on resolution, against the
                      -- START-of-resolution slots.
                      (admitted, answers') <- if taken then payGateAdmits stackId srcId effectController idx cIdx (instanceView legal) answers clause else pure (False, answers)
                      Monad.when admitted (Monad.mapM_ applyOne (Clause.effects clause))
                      pure answers'
                  )
                  Map.empty
                  (zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode)))
       in do
            Monad.unless fizzles (Monad.forM_ modes resolveOne)
            State.modify' (Game.cease stackId)

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
-- The GROUP bindings come in beside them, from the same live read the caller
-- takes the chosen slots off, so a gate can ask after a batch an earlier clause
-- named (CR 115.10a). Under the printed slot names, which is how every other
-- group read is written (slotGroups) and unlike the chosen map, which the caller
-- has projected into CR 700.2d's mode instance.
gateHolds :: PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Seq.Seq ObjectId) -> Clause.Clause Card.Type.Card -> Game Bool
gateHolds controller source chosen groups clause = case Clause.condition clause of
  Nothing -> pure True
  Just condition -> do
    gs <- State.get
    pure (Condition.holds (Projection.viewWithLastKnownAnywhere gs) (effectContext controller source chosen groups) gs source condition)

-- CR 603.5 / 608.2d: does this clause's instruction list happen at all? A
-- mandatory clause always does; an optional one is its controller's call, made
-- HERE as the effect is applied. The unit is CR 608.2e's clause and not the whole
-- mode, so a "may" printed on one sentence leaves its neighbours alone.
--
-- `controller` is who "you" means (CR 405.4 for a spell, CR 113.8 for an ability)
-- and therefore who is asked, through Decide.deciderFor so a player controlled
-- under CR 723.1 has their controller answer.
--
-- `bound` is every slot the live bindings hold and `legal` is CR 608.2b's
-- surviving recipients, both under the names this mode instance prints (CR
-- 700.2d); an inert clause is not asked about at all -- see clauseIsInert.
exercises :: ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Set SlotName -> Map.Map SlotName (Set Recipient) -> Clause.Clause Card.Type.Card -> Game Bool
exercises resolving controller idx cIdx bound legal clause = case Clause.optionality clause of
  Optionality.Mandatory -> pure True
  Optionality.Optional
    | clauseIsInert bound legal clause -> pure False
    | otherwise -> do
        gs <- State.get
        let decider = Decide.deciderFor controller gs
        decision <- Game.choose (Prompt.ChooseOptional decider controller resolving idx cIdx)
        pure $ case decision of
          OptionalDecision.Exercises -> True
          OptionalDecision.Declines -> False

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
clauseIsInert :: Set SlotName -> Map.Map SlotName (Set Recipient) -> Clause.Clause Card.Type.Card -> Bool
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
-- restores the entry state and an Unpaid result is a complete no-op; or the
-- reference never named them at all, which is not an answer and leaves them out
-- of both branches.
--
-- The cost is paid AGAINST `source` rather than the resolving stack object (CR
-- 113.7a); the two are the same object for a spell.
--
-- ONE offer per payment (CR 118.12): a second clause hanging off the same cost
-- names the first (PayGate.offeredAt) and reuses the recorded answers, `answers`
-- being keyed on the offering clause's ordinal. A clause naming an offer never
-- made falls through and makes it, the named clause having failed its own CR
-- 701.46a "if" or CR 603.5 "may".
payGateAdmits :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> Map.Map ClauseIndex (Map.Map PlayerId Bool) -> Clause.Clause Card.Type.Card -> Game (Bool, Map.Map ClauseIndex (Map.Map PlayerId Bool))
payGateAdmits resolving source controller idx cIdx legal answers clause = case Clause.payGate clause of
  Nothing -> pure (True, answers)
  Just gate -> do
    let offerAt = Maybe.fromMaybe cIdx (PayGate.offeredAt gate)
    (asked, answers') <- case Map.lookup offerAt answers of
      Just recorded -> pure (recorded, answers)
      Nothing -> do
        recorded <- payGatePaid resolving source controller idx cIdx legal gate
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
-- CR 101.4's APNAP order over the players the reference names, which is what
-- `apnapPlayersOf` imposes: rule 101.4b lets a later payer answer knowing what an
-- earlier one did. The board is re-read for each of them (payGatePaidBy's own
-- State.get) rather than measured once, so a cost that changes the board -- CR
-- 118.12's own "sacrifice this enchantment" -- is affordable to the next payer
-- against the board it left. Each payer spends only their own resources, so the
-- sequencing is not observable as an ordering of the ACTIONS.
payGatePaid :: ObjectId -> ObjectId -> PlayerId -> ModeIndex -> ClauseIndex -> Map.Map SlotName (Set Recipient) -> PayGate.PayGate -> Game (Map.Map PlayerId Bool)
payGatePaid resolving source controller idx cIdx legal gate = do
  gs <- State.get
  Monad.foldM
    ( \acc payer -> do
        paid <- payGatePaidBy resolving source idx cIdx payer gate
        pure (Map.insert payer paid acc)
    )
    Map.empty
    (apnapPlayersOf (PayGate.payer gate) legal controller gs)

-- One player's answer to one gate. The cost is the PRINTED one with CR 107.3's X
-- resolved (`announcedXOn`), and that substitution is what every reader below
-- sees -- CR 118.3's affordability test, the prompt the payer is shown, and the
-- payment itself -- so none of them can disagree about what {X} is.
payGatePaidBy :: ObjectId -> ObjectId -> ModeIndex -> ClauseIndex -> PlayerId -> PayGate.PayGate -> Game Bool
payGatePaidBy resolving source idx cIdx payer gate = do
  gs <- State.get
  let cost = Cost.substituteX (announcedXOn resolving gs) (PayGate.cost gate)
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
          (announced, _) <- Cost.announce Nothing ManaSpending.AsProduced payer source pure cost
          outcome <- Cost.pay Nothing ManaSpending.AsProduced payer source announced
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
resolveModes :: ObjectId -> ObjectId -> [(ModeInstance, Mode.Mode Card.Type.Card)] -> Game ()
resolveModes = resolveModesWith noSubgame

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (CR 113.7a), not the ability object, and only the CHOSEN modes are read (CR
-- 700.2c). The ability then ceases (CR 608.2n) rather than being buried.
--
-- `runSubgame` rides through to the effects for the reason resolveModesWith
-- gives (CR 729.1a).
resolveAbilityWith :: Game Result -> ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Game ()
resolveAbilityWith runSubgame abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.modesOf (Object.bindings obj)
       in resolveModesWith runSubgame abilId srcId (Modal.chosenModes chosen (ActivatedAbility.modal ability))

-- The no-subgame activated-ability resolver.
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Game ()
resolveAbility = resolveAbilityWith noSubgame

-- CR 701.27a over ONE object: turn it over, or leave the map as it was. The turn
-- itself is Game.turnFaceOver, shared with Pawl.Engine.Daytime's CR 702.145c/f
-- sweep. What this adds is the two gates that belong to an instruction rather
-- than to the act, and a static ability's turn has neither: CR 701.27f's
-- already-turned check, and CR 702.145b/e's "can't transform except due to its
-- daybound/nightbound ability" (Pawl.Engine.Daytime.restrictsTransform).
--
-- `now` is minted ONCE for the whole instruction by the caller, because CR 608.2f
-- processes a swept set simultaneously and a later CR 701.27f comparison must not
-- tell two victims apart. `pcs` is hoisted likewise.
turnOver :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> Timestamp.Timestamp -> GameState -> ObjectId -> Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
turnOver pcs resolving now gs oid objects
  | alreadyTurnedFor resolving oid gs = objects
  | Daytime.restrictsTransform pcs oid = objects
  | otherwise = Game.turnFaceOver now gs oid objects

-- CR 701.27f, first sentence. True when this resolution must be ignored. Two
-- conditions, and BOTH narrow: the resolving object is an ability whose SOURCE is
-- the very permanent being turned over (CR 113.7a) -- a spell is not one, and
-- neither is an ability of some other permanent -- and the permanent's last turn
-- is later than that ability object's CR 613.7d timestamp. Both stamps come from
-- GameState.nextTimestamp, so one `>` decides it and equality cannot arise.
--
-- The rule's SECOND sentence measures a delayed triggered ability from when it
-- was CREATED rather than from when it reached the stack, and that half is not
-- implemented: Pawl.Types.DelayedTrigger records no creation moment, so this
-- measures it from the stack like any other trigger (#694).
alreadyTurnedFor :: ObjectId -> ObjectId -> GameState -> Bool
alreadyTurnedFor resolving victim gs = case Game.lookupObject resolving gs of
  Nothing -> False
  Just ability ->
    abilityOf (Object.source ability)
      && maybe False (> Object.timestamp ability) (Game.lookupObject victim gs >>= Object.turnedOverAt)
  where
    -- A CLASSIFICATION of the resolving object, never which card it is. CR
    -- 725.2's sourceless inherent trigger has no permanent to be an ability OF,
    -- so it falls out with the spells.
    abilityOf source = case source of
      Source.OfAbility srcId _ -> srcId == victim
      Source.OfTrigger srcId _ -> srcId == victim
      Source.OfCard _ -> False
      Source.OfToken _ -> False
      Source.OfEmblem _ -> False
      Source.OfInherentTrigger _ _ -> False

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
  PlayerRef.Relative PlayerRelation.You -> [controller]
  PlayerRef.Relative PlayerRelation.Opponent -> filter (PlayerRelation.holds PlayerRelation.Opponent controller) everyone
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
         in filter (\pid -> PlayerRelation.holds relation controller pid && pid `elem` attackers) everyone
  where
    everyone = Game.stillPlaying gs

-- The objects an ObjectRef names DURING a resolution, and the ONE place a
-- filter-selected set is swept. InSlot takes every recipient CR 608.2b left legal
-- (CR 601.2c); a slot bound to a GROUP is answered before that question, a group
-- being a definition rather than a target (CR 115.10a).
--
-- EachMatching folds the battlefield (CR 109.2) against the projection, so a
-- permanent that is a creature only by a layer-4 effect is in the set. The filter
-- context is this effect's own -- CR 109.5's "you" is the ability's controller --
-- because the filter IS the ability's card text. EachCardInGraveyard is the same
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
-- opens, and it asks rather than reading this order.
objectRefObjects :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [ObjectId]
objectRefObjects legal resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- The slot names every object bound there as a group, all at once. Ahead of
    -- the target read and not subject to `legal`: a group binding is a definition,
    -- never a target (CR 115.10a). slotGroup says why being ahead is safe.
    Just group -> Foldable.toList group
    Nothing -> Maybe.mapMaybe Recipient.objectOf (legalMany slot legal)
  ObjectRef.EachMatching filter_ ->
    -- CR 303.4b's host is supplied here and nowhere else in this module: this
    -- sweep is the one effect-borne Filter position naming what the SOURCE
    -- enchants. Read live, so an Aura moved between the trigger and its
    -- resolution acts on the host it has now.
    --
    -- Through effectContext, so the resolution's own slot bindings ride along and
    -- a sweep can exclude what another slot already named: Showstopping Surprise's
    -- "each OTHER creature" is `Not (IsBound "target")`. Filter.IsBound answers
    -- False for every candidate against an empty slot map, so a bare contextFor
    -- here would leave such a card silently sweeping in its own target.
    let context = (effectContext controller source legal (slotGroups resolving gs)) {Filter.sourceAttachedTo = Projection.hostOf source gs}
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
  -- EachMatching's sweep with CR 109.2's battlefield default switched off by the
  -- card's own words (CR 109.2a), over CR 400.1's per-player zone. Whose
  -- graveyards is graveyardScopePlayers below -- either the perspective's own
  -- reading of CR 109.5 or the players another slot of this announcement targets
  -- -- and what matches within each is graveyardCardsOf.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard scope filter_) ->
    concatMap (\pid -> graveyardCardsOf (effectContext controller source legal (slotGroups resolving gs)) gs pid filter_) (graveyardScopePlayers legal controller gs scope)
  -- CR 400.1's per-player zone again, but only the RESOLVING CONTROLLER's, so no
  -- scope to fold over and no APNAP order to impose. In the zone's own order,
  -- which no rule reads: CR 400.5 leaves a hand's arrangement to its owner.
  ObjectRef.EachCardInYourHand -> Game.zoneMembers Zone.Hand controller gs
  -- CR 607.2a's linked set: the cards GameState.exiledWith files against this
  -- effect's SOURCE. The relation, not a zone sweep, is the membership test, so a
  -- card exiled by a second copy of the same printing is not named; a stated
  -- Filter then narrows it. `source` and not `resolving`, since rule 607.2a links
  -- two abilities of one OBJECT and for a dies trigger the two ids differ. Read
  -- off GameState.exile directly because CR 400.1 makes exile one SHARED zone --
  -- no player to ask, and no APNAP sort, so ascending id and thus arrival order.
  ObjectRef.EachCardExiledWithSource mFilter ->
    let context = effectContext controller source legal (slotGroups resolving gs)
        stated oid = case mFilter of
          Nothing -> True
          Just filter_ -> Filter.matches context (Projection.viewOfObject oid gs) filter_
     in filter
          (\oid -> Map.lookup oid (GameState.exiledWith gs) == Just source && stated oid)
          (Set.toList (GameState.exile gs))
  -- CR 109.2b's reading of a description carrying the word "spell" -- the stack,
  -- not the battlefield. Game.isSpell keeps the abilities sharing the zone out
  -- (CR 112.1), a classification of the object's kind and not of its identity. In
  -- the STACK's own order, top first (CR 405.2), not APNAP: one shared zone has an
  -- order the rules already read. Read LIVE (CR 608.2c).
  ObjectRef.EachSpell filter_ ->
    let context = effectContext controller source legal (slotGroups resolving gs)
     in filter
          (\oid -> Game.isSpell oid gs && Filter.matches context (Projection.viewOfObject oid gs) filter_)
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
        context = effectContext controller source legal (slotGroups resolving gs)
        wanted = maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source count)
        matches oid = Filter.matches context (Projection.viewOfObject oid gs) filter_
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

-- The players a PlayerScope names, in APNAP order: the seat half of
-- graveyardCards, needed by ObjectRef.ChosenCardInGraveyard's EachInScope
-- chooser, which asks each seat separately. An absent perspective is empty.
--
-- No bindings, and none to pass: a PlayerScope has no arm that names a slot,
-- which is exactly why ObjectRef.EachCardInGraveyard stopped carrying one.
graveyardPlayers :: PlayerId -> GameState -> PlayerScope.PlayerScope -> [PlayerId]
graveyardPlayers controller gs = graveyardScopePlayers Map.empty controller gs . GraveyardScope.Scoped

-- graveyardPlayers over the WIDER scope an ObjectRef.EachCardInGraveyard carries,
-- and the function that actually reads it: whose graveyards is
-- Target.graveyardScopePlayers, the same answer a target pool over CR 400.1's
-- per-player zone gets, and the order imposed on it here is APNAP (CR 608.2f, CR
-- 101.4) restricted to the players still in the game.
--
-- The bindings are the ones the CALLER holds, which is CR 608.2b's re-checked set
-- at resolution: an InSlot scope naming a slot whose target went illegal names
-- nobody, and CR 101.3 ignores that share of the effect.
graveyardScopePlayers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> GraveyardScope.GraveyardScope -> [PlayerId]
graveyardScopePlayers bindings controller gs scope =
  let named = Target.graveyardScopePlayers (Just controller) bindings scope gs
   in filter (`elem` named) (Game.apnapOrder gs)

-- The cards in ONE player's graveyard matching the filter, in ascending
-- ObjectId. The filter is matched in THIS EFFECT's context -- the caller's, so
-- CR 109.5's "you" is the resolving controller rather than whoever is choosing,
-- and the resolution's own slots ride along: Midnight Tilling's "from among
-- them" is `IsBound` over the slot its own mill defined, which a bare contextFor
-- would answer False for on every candidate.
graveyardCardsOf :: Filter.Context -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
graveyardCardsOf context gs pid filter_ =
  List.sort
    ( filter
        (\oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_)
        (Game.zoneMembers Zone.Graveyard pid gs)
    )

-- The cards in the named graveyards matching the filter, for
-- ChosenCardInGraveyard's TheController chooser -- ObjectRef.EachCardInGraveyard
-- is this fold over the wider scope, spelled out at its own arm. A card in a
-- graveyard has no controller, so Filter.ControlledBy is vacuously False. APNAP
-- (CR 101.4) then ascending ObjectId, not the graveyard's own pile order (CR
-- 404.2), which no rule makes a batch's processing order.
graveyardCards :: Filter.Context -> PlayerId -> GameState -> PlayerScope.PlayerScope -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
graveyardCards context controller gs scope filter_ =
  concatMap (\pid -> graveyardCardsOf context gs pid filter_) (graveyardPlayers controller gs scope)

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
-- own order, which no rule reads (CR 400.5). Narrowing must not reorder.
handCardsOf :: Filter.Context -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
handCardsOf context gs pid filter_ =
  filter
    (\oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_)
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
  ObjectRef.EachCardExiledWithSource {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.TopOfLibrary {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.TopOfLibraryUntil {} -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  ObjectRef.EachSpell _ -> fmap Recipient.ToObject (objectRefObjects legal resolving controller source gs ref)
  -- CR 120.3a: a player is a damage recipient. APNAP (CR 608.2f) via
  -- Game.apnapOrder.
  ObjectRef.EachPlayer -> fmap Recipient.ToPlayer (Game.apnapOrder gs)
  -- CR 120.3a again, over CR 102.1's opponents alone -- the arm above filtered
  -- by PlayerRelation.holds against CR 109.5's "you", which is the resolving
  -- controller. APNAP order survives the filter (CR 608.2f).
  ObjectRef.EachOpponent -> fmap Recipient.ToPlayer (filter (PlayerRelation.holds PlayerRelation.Opponent controller) (Game.apnapOrder gs))
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

-- CR 608.2f's order for the per-object loop: APNAP first, reading a player
-- recipient as that seat and an object as its controller's. Imposed here rather
-- than in objectRefRecipients, whose InSlot arm answers in Recipient (set)
-- order; this loop is the first reader that takes recipients one at a time.
--
-- The second key is CR 608.2f's secondary sentence and belongs to the RESOLVING
-- CONTROLLER, so each seat's group is handed to them as a Prompt.OrderForEach.
-- Asked once per group, in APNAP order of the groups; a group of one is not
-- asked. Game.permute keeps the engine's order for a non-permutation answer.
--
-- A recipient the board no longer holds has no controller and sorts last, which
-- is reachable rather than defensive (CR 400.7); two such share that bucket.
forEachOrder :: ObjectId -> PlayerId -> [Recipient] -> Game [Recipient]
forEachOrder resolving controller recipients = do
  gs <- State.get
  let order = Game.apnapOrder gs
      last_ = length order
      seat recipient = maybe last_ (\pid -> Maybe.fromMaybe last_ (List.elemIndex pid order)) (recipientSeat gs recipient)
      groups = List.groupBy (\a b -> seat a == seat b) (List.sortOn (\recipient -> (seat recipient, recipient)) recipients)
      pick group = case group of
        _ : _ : _ -> do
          answer <- Game.choose (Prompt.OrderForEach (Decide.deciderFor controller gs) controller resolving group)
          pure (Game.permute group answer)
        _ -> pure group
  fmap concat (traverse pick groups)

-- WHOSE a recipient is: a player recipient is that seat, an object's is its
-- controller (CR 110.2) -- and CR 608.2h's last known information for an object
-- the board no longer holds, which is reachable rather than defensive (CR
-- 400.7): Effect.ForEach walks the permanents a destruction already removed.
-- Nothing only where neither answer exists; each caller says what it does with
-- that.
recipientSeat :: GameState -> Recipient -> Maybe PlayerId
recipientSeat gs recipient = case recipient of
  Recipient.ToPlayer pid -> Just pid
  _ -> Recipient.objectOf recipient >>= \oid -> Projection.controllerWithLastKnown oid gs

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
--      Card.castableFaces (CR 709.3, CR 712.11b, CR 715.3).
--   3. WHAT IT COSTS (CR 118.9): `withoutPayingManaCost` or a stated
--      `payingInstead` (CR 702.94a); otherwise CR 601.2b's own candidates.
--   4. MAY IT BE CAST AT ALL -- Cast.castableWhenOffered, asked BEFORE the
--      prompt so no cast is offered that the announcement would reverse.
--
-- Questions 3 and 4 are asked of EACH half separately (CR 709.3a, CR 712.11c);
-- where more than one survives, CR 709.3's choice is put to the caster before
-- the "may" below, since CR 118.8c's excuse is a property of the spell being
-- cast. At Optionality.Mandatory the cast is not a decision, so
-- Prompt.OfferedCast is elided; question 4 is what a printed "if able" comes to
-- (CR 601.3, CR 609.3). CR 118.8c is the exception: `excused` turns the
-- mandatory branch back into a may, classified by Cost.statesHiddenQuality.
--
-- The caster is a parameter and not the resolving controller: CR 608.2g says "a
-- player". Everything above is a CLASSIFICATION carried by the opcode's
-- CastOffer and its Optionality; nothing here asks which card is offered.
offerCast :: ObjectId -> PlayerId -> SlotName -> Optionality.Optionality -> CastOffer.CastOffer -> Game ()
offerCast resolving caster slot optionality offer = do
  gs <- State.get
  let -- CR 712.11a for the transformed rider; CR 709.3, CR 712.11b and CR 715.3
      -- otherwise, via Card.castableFaces. Nothing for a card with no back face
      -- (CR 712.14a): an offer that cannot be made is not made.
      faces card
        | CastOffer.transformed offer = fmap pure (Card.backFace card)
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
            candidates = maybe (Cost.costsFor name oid proposed) pure applied
         in if Cast.castableWhenOffered caster oid name candidates proposed
              then
                -- CR 118.8c, read off the same candidates the cast will be
                -- announced with: CR 118.9d keeps the face's additional costs on
                -- an alternative, so every candidate already carries them.
                --
                -- Not implemented: a cost APPLIED from another effect (CR 118.8)
                -- arrives as CostAdjustments.components and is not read here
                -- (#1834).
                Just (oid, name, applied, any Cost.statesHiddenQuality candidates)
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
      let cast = Cast.castSpellWith applied caster oid name Facing.FaceUp
          -- The SAME prompt on both paths: CR 118.8c creates no new decision.
          mayCast = do
            let decider = Decide.deciderFor caster gs
            decision <- Game.choose (Prompt.OfferedCast decider caster oid name)
            case decision of
              OptionalDecision.Declines -> pure ()
              OptionalDecision.Exercises -> cast
      case optionality of
        Optionality.Mandatory | not excused -> cast
        Optionality.Mandatory -> mayCast
        Optionality.Optional -> mayCast

-- CR 615.3: install one floating damage row, for a duration. Shared by
-- Effect.PreventNextDamage, Effect.PreventAllDamage and Effect.RedirectDamage,
-- which differ only in the DamageRewrite -- CR 615.7's countdown, CR 615.1's
-- unbounded shield, or CR 614.9's redirection.
--
-- One shield PER RECIPIENT (CR 615.11), which is why the callers fold this over
-- the set their ObjectRef names; every producer in the pool names exactly one,
-- so the fold is over a singleton today (gap #1108).
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
installDamageRow :: Map.Map SlotName PlayerId -> PlayerId -> ObjectId -> Duration.Duration -> Maybe DamageKind.DamageKind -> DamageRewrite.DamageRewrite -> Maybe PreventionRider.PreventionRider -> GameState -> (Maybe Recipient, Maybe (Filter.Type.Filter Keyword.Type.Keyword, ObjectId)) -> GameState
installDamageRow players controller source duration kind rewrite rider g (recipient, sourceChoice) = case Expiry.arm players controller source duration g of
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
                        { -- PRINTED, not assumed: Nothing takes combat and
                          -- noncombat alike, Just Combat only the former.
                          DamagePattern.whichKind = kind,
                          -- A caller that names no source gets the trivial
                          -- predicate, which is CR 615.7's own "the number of
                          -- events or sources dealing it doesn't matter"; one
                          -- that named a CHOSEN source (CR 609.7a) gets that
                          -- source's printed properties, for CR 615.9's recheck
                          -- at the damage event.
                          DamagePattern.whatSource = maybe (Filter.Type.And []) fst sourceChoice,
                          -- The recipient is BAKED below rather than described:
                          -- the resolution has already chosen it, or the row
                          -- names none and covers every recipient.
                          DamagePattern.whatRecipient = Nothing,
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
                  ),
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
              ActiveReplacement.rider = rider,
              ActiveReplacement.slots = Map.empty
            }
     in g1 {GameState.replacements = active : GameState.replacements g1}

-- CR 609.7a: choose the SOURCE a prevention effect names, and answer the pair
-- installDamageRow bakes -- the printed properties beside the id the choice
-- landed on. Nothing for a shield naming no source, and Nothing again where the
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

-- CR 609.7a's candidate set: "a permanent; a spell on the stack (including a
-- permanent spell); ... or a face-up object in the command zone", narrowed to the
-- properties the card printed. Read off the PROJECTION, so a permanent that is a
-- red source only by a continuous effect is a candidate.
--
-- ASCENDING, so both the single-candidate shortcut and a transcript are
-- deterministic -- Pawl.Engine.Blight's posture.
--
-- Not implemented: rule 609.7a's third class, "any object referred to by an
-- object on the stack, by a replacement or prevention effect that's waiting to
-- apply, or by a delayed triggered ability that's waiting to trigger", which
-- narrows the choice a player is offered (#1904).
damageSourceCandidates :: Filter.Context -> GameState -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
damageSourceCandidates context gs filter_ =
  let faceUp oid = fmap Object.facing (Game.lookupObject oid gs) == Just Facing.FaceUp
      pool = Set.toList (GameState.battlefield gs) <> GameState.stack gs <> filter faceUp (Set.toList (GameState.command gs))
   in List.sort (filter (\oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_) pool)

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
-- read live off the resolving object (slotGroups) instead. It reaches
-- Filter.IsBound whole, and the singular readers decline it
-- (Filter.slotOneObject).
effectContext :: PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Seq.Seq ObjectId) -> Filter.Context
effectContext controller source legal groups =
  Filter.contextWithSlots (Just controller) (Just source) (Binding.withGroups (effectSlotObjects legal) groups)

-- The ONE object each of a resolution's TARGET slots names, shared by
-- effectContext above and effectViewOf below so the two cannot disagree about
-- which object a slot is.
effectSlotObjects :: Map.Map SlotName (Set Recipient) -> Map.Map SlotName ObjectId
effectSlotObjects = Map.mapMaybe Recipient.objectOf . Map.mapMaybe Binding.onlyOne

-- The GROUP bindings a resolution has made so far, read LIVE off the resolving
-- object (CR 608.2c): a slot an earlier clause of this same resolution defined is
-- part of the state a later one is read against, which is exactly what "from
-- among them" needs. By the name the effect wrote, as slotGroup above reads it.
slotGroups :: ObjectId -> GameState -> Map.Map SlotName (Seq.Seq ObjectId)
slotGroups resolving gs = Binding.groupsOf (maybe Map.empty Object.bindings (Game.lookupObject resolving gs))

-- CR 608.2h's reader for one resolution: Projection.viewWithLastKnown, which
-- answers the SOURCE off its last known information, widened to the permanent a
-- COST payment sacrificed (Binding.sacrificedPermanent).
--
-- That slot's object is gone by construction -- CR 601.2h paid the cost before
-- the ability was on the stack at all, and CR 701.21a put the permanent in a
-- graveyard as a new object (CR 400.7) -- so viewWithLastKnown's blank answer for
-- a non-source object would leave Jarad, Golgari Lich Lord's "the sacrificed
-- creature's power" permanently unanswerable.
--
-- The blank is still right for every OTHER non-source id, and that is why this
-- names one slot rather than lifting the scope: those ids are TARGETS, and CR
-- 608.2b wants a target that has left to answer with nothing. A slot the payment
-- defined was never a target (CR 115.10a).
effectViewOf :: ObjectId -> Map.Map SlotName (Set Recipient) -> GameState -> ObjectId -> Maybe Filter.View
effectViewOf source legal gs oid =
  if Map.lookup Binding.sacrificedPermanent (effectSlotObjects legal) == Just oid
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
  let context = effectContext controller source legal (slotGroups resolving gs)
   in filter (\oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_) members

-- The printed "from among them", a CR 608.2d choice: the candidates are the
-- members of a GROUP an earlier clause of this resolution bound rather than a
-- zone's contents, which is the whole difference from the zone-keyed choices --
-- and the reason a batch CR 701.20a's reveal or CR 701.20e's look left in the
-- LIBRARY is reachable at all (CR 701.20b), a library still having no filtered
-- sweep (#1309).
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
-- WHO is asked is the resolving controller and only ever them (CR 608.2d, #1957).
-- Elided at one candidate and skipped at none (CR 101.3, CR 609.3). Filtered, not
-- trusted: an answer naming a card never offered falls back to the first
-- candidate. At most one object comes back, which is #1956's count read as one.
chooseCardFromAmong ::
  ObjectId ->
  ObjectId ->
  PlayerId ->
  Map.Map SlotName (Set Recipient) ->
  Map.Map SlotName (Set Recipient) ->
  ChosenCardFromAmong.ChosenCardFromAmong ->
  Game [ObjectId]
chooseCardFromAmong resolving source controller legal chosen (ChosenCardFromAmong.MkChosenCardFromAmong slot filter_) = do
  members <- fromAmongMembers legal resolving chosen slot
  gs <- State.get
  case matchingFromAmong legal resolving controller source gs filter_ members of
    [] -> pure []
    [only] -> pure [only]
    first : second : more -> do
      let offered = first NonEmpty.:| (second : more)
      answer <- Game.choose (Prompt.ChooseCardFromAmong (Decide.deciderFor controller gs) controller source offered)
      pure [if List.elem answer (NonEmpty.toList offered) then answer else first]

-- One effect, applied, wrapped in the window CR 607.2a's link is filed from:
-- what was in exile before, and what is in it after.
applyEffectWith :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card -> Game ()
applyEffectWith runSubgame resolving source controller legal chosen effect = do
  before <- State.gets GameState.exile
  applyOneEffect runSubgame resolving source controller legal chosen effect
  State.modify' (recordExiledWith source before)

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
applyOneEffect :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName (Set Recipient) -> Effect Card.Type.Card -> Game ()
applyOneEffect runSubgame resolving source controller legal chosen effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage ref quantity dealer excess) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        -- CR 120.1a: damage only to a battle, creature, or planeswalker, so both
        -- arms of the ObjectRef go through Damage.damageRecipient and neither is
        -- trusted. A player recipient survives untouched (CR 115.4, CR 120.3a).
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
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
        -- HOW MUCH, read ONCE PER RECIPIENT: Acidic Soil's "damage to each
        -- player equal to the number of lands they control" is one amount per
        -- seat. Still one action under CR 608.2f, since every read is against
        -- the same pre-effect state. Both kinds of recipient get it, keyed to
        -- recipientSeat. An unevaluable amount drops that recipient, and so does
        -- a zero (CR 120.8).
        let amountFor recipient = case recipientSeat gs recipient of
              Nothing -> Quantity.evaluateFor viewOf context gs resolving source quantity
              Just pid -> evaluateForRecipient viewOf context gs resolving source pid quantity
            events =
              Maybe.mapMaybe
                ( \recipient -> do
                    n <- amountFor recipient
                    Monad.guard (n > 0)
                    pure (Damage.damageEvent gs DamageKind.Noncombat dealt recipient (Integer.toNaturalSaturating n))
                )
                recipients
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
            case Projection.freezeQuantities gs resolving source (Just controller) modification of
              Nothing -> gs
              Just frozen ->
                let (ts, gs1) = Game.freshTimestamp gs
                    eff =
                      ContinuousEffect.MkContinuousEffect
                        { ContinuousEffect.source = source,
                          ContinuousEffect.timestamp = ts,
                          ContinuousEffect.expiry = expiry,
                          -- CR 611.2: the stored modification is the narrow
                          -- (grantless) one, widened for the projection.
                          ContinuousEffect.modification = Projection.widenModification frozen,
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
  -- The RETENTION is the one thing read off the instruction (CR 106.4).
  Effect.AddMana (ManaAddition.MkManaAddition ref production retention restriction) -> do
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
                  ManaUnit.restriction = restriction
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
                        ManaUnit.restriction = restriction
                      }
              State.modify' (Mana.addMana pid [unit])
  Effect.Search (Search.MkSearch searcherRef ownerRef quantity filter_ upTo destination) ->
    -- CR 701.23a: match each library card through its own CR 613 projection --
    -- rule 613.1 names no zone, so a library card is folded exactly as a
    -- permanent is, and CR 208.2a's characteristic-defining power rides along at
    -- layer 7a.
    --
    -- The context has no perspective (CR 109.5): a search filter never
    -- references a player, so ControlledBy is vacuously False.
    --
    -- CR 701.3a: one candidate's VIEW carries the field no projection can fill --
    -- whether the CANDIDATE could legally be attached to the object this
    -- instruction fixes (Auratouched Mage's "an Aura card that could enchant
    -- it"), which for a search is the searching ability's own SOURCE. Answered by
    -- Attach.attachmentFor, the same function the move goes through, so the offer
    -- and the move cannot disagree. Lazy, so a filter that never names
    -- Filter.CanAttachToSubject pays nothing for it.
    --
    -- No recursion to bound: this is called from a resolution rather than from
    -- inside a CR 613 fold, so the projections attachmentFor reaches start fresh.
    let searchContext = Filter.contextFor Nothing Nothing
        viewOfCandidate g oid =
          (Projection.viewOfObject oid g)
            { Filter.canAttachToSubject = Maybe.isJust (Attach.attachmentFor oid (Recipient.ToObject source) g)
            }
        matches1 g oid = Filter.matches searchContext (viewOfCandidate g oid) filter_
     in do
          gs0 <- State.get
          -- Search.searcher names who searches; Search.owner names whose library
          -- is read and shuffled. The searcher is prompted and offered CR 601.3's
          -- cast. Neither is `controller` except where a ref says so. CR 701.23i
          -- supplies the order: apnapOrder supplies the ORDER and the ref the
          -- MEMBERSHIP, for searchers and owners alike.
          --
          -- Not implemented: CR 701.23i's SIMULTANEOUS look, each searcher seeing
          -- the libraries before any of them decides (#1319).
          let inApnapOrder r =
                let named = playerRefPlayers legal controller gs0 r
                 in filter (\pid -> List.elem pid named) (Game.apnapOrder gs0)
              searchers = inApnapOrder searcherRef
              -- "Each player searches THEIR library" (Jungle Wayfinder) is ONE
              -- instruction applied per player, not the cross product of two
              -- folds: the library read is whichever searcher this pass has
              -- reached. Rule 701.23a says only how to look, so which library
              -- that is comes from the card's own sentence rather than from the
              -- rule. That is Pawl.Types.PlayerRef.Candidate's reading, and
              -- the substitution is the same move Pawl.Engine.Quantity
              -- .forCandidate makes for a per-player amount. Every other ref
              -- names a set of its own, so Extract's You/InSlot pair still
              -- crosses -- one searcher over one owner.
              ownersFor searcher = case ownerRef of
                PlayerRef.Candidate -> [searcher]
                _ -> inApnapOrder ownerRef
              -- How many cards this search may find (CR 701.23a), evaluated ONCE
              -- before the loop: one instruction names one count. An unevaluable
              -- or non-positive quantity comes out as 0.
              cap = case Quantity.evaluateFor (effectViewOf source legal gs0) (effectContext controller source legal (slotGroups resolving gs0)) gs0 resolving source quantity of
                Just n | n > 0 -> Integer.toNaturalSaturating n
                _ -> 0
          Monad.forM_ searchers $ \searcher -> Monad.forM_ (ownersFor searcher) $ \owner -> do
            -- CR 101.2: a player who can't search libraries does not, and finds
            -- nothing. Asked BEFORE CR 601.3's offer below, which is made WHILE
            -- SEARCHING. The rest of the instruction still happens -- CR 701.23
            -- says only how to look, so the shuffle is the card's own.
            prohibited <- State.gets (PlayerEffect.prohibitsSearching searcher)
            -- A cap of zero asks nothing and finds nothing: one legal answer is
            -- no choice to put to a player.
            found <-
              if prohibited || cap == 0
                then pure []
                else do
                  -- CR 601.3 (Panglacial Wurm): the chance to cast is offered AT
                  -- THE SEARCH, not when the resolution began, so earlier
                  -- effects of the resolution have already happened. Both spells
                  -- and abilities reach here. The Wurm's "while you're searching
                  -- your library" makes the offer the SEARCHER's, and only where
                  -- the library being searched is their own.
                  Monad.when (searcher == owner) (Cast.castWhileSearching searcher)
                  gs <- State.get
                  let matches = filter (matches1 gs) (Game.zoneMembers Zone.Library owner gs)
                      decider = Decide.deciderFor searcher gs
                  answer <- Game.choose (Prompt.SearchLibrary decider searcher matches cap)
                  -- CR 701.23a: every card found is one the filter admits.
                  -- Filtered, not trusted, deduplicated, and truncated to
                  -- the cap. What a SHORT answer leaves is the difference between
                  -- CR 701.23b and CR 701.23d, and Filter.statesAQuality is which
                  -- rule this search is under: stating a quality it may find
                  -- fewer, and a bare quantity must find as many as it can, so
                  -- the answer is COMPLETED from the remaining matches.
                  -- Search.upTo is the third case, a card's own "up to" over a
                  -- filter stating no quality; it lands in CR 701.23b's branch.
                  let picked = List.genericTake cap . List.nub $ filter (\oid -> List.elem oid matches) answer
                      filler = filter (\oid -> List.notElem oid picked) matches
                  pure $
                    if Filter.statesAQuality filter_ || upTo
                      then picked
                      else List.genericTake cap (picked <> filler)
            -- Where the cards go is the CARD's instruction, not rule 701.23's;
            -- CR 701.23e says the same of the reveal. The searcher is the
            -- revealer (CR 701.20a), and the cards go in the order the searcher
            -- named them.
            Monad.mapM_ (putFound searcher source destination) found
            -- The shuffle is the CARD's instruction too (CR 701.23h, CR 701.24b).
            -- The library shuffled is the one that was READ, so this seat is the
            -- owner.
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
    --     this way", "return each CARD put into a graveyard this way". The filter
    --     also keeps CR 111.8 (a token that has left the battlefield can't come
    --     back) out of reach of the one shape that would ask for it (gap #1950).
    --
    -- Nothing is bound when nothing qualifies, MoveToZone's rule: no slot names
    -- an empty set, so the later clause finds an unbound slot and does nothing --
    -- which is exactly what "IF a creature card is put into a graveyard this way"
    -- asks for.
    Monad.forM_ mBuried $ \slot -> do
      after <- State.get
      let buriedCards = filter (\oid -> isCardInAGraveyard oid after) (Maybe.mapMaybe snd destroyed)
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
  Effect.Sacrifice slot -> do
    -- A slot a Create bound to a GROUP names every token at once, so all of them
    -- are sacrificed, in mint order. Read off the resolving object's live
    -- bindings rather than out of `chosen`: a group binding is never a target, so
    -- it owes CR 608.2b nothing.
    --
    -- CR 603.7c's zone check applies per MEMBER rather than to the whole word: a
    -- member that is gone is simply not affected, and the rest still are.
    bound <- State.gets (slotGroup slot resolving)
    case bound of
      -- One at a time rather than as one event (#757).
      Just victims -> Monad.mapM_ (Event.sacrifice controller) victims
      Nothing -> case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> pure () -- a player recipient cannot be sacrificed
          -- CR 701.21: through the single funnel, which is NOT Event.destroy (CR
          -- 701.21a). The sacrificing player is this effect's controller; the
          -- funnel's CR 701.21a guard makes any other case a no-op.
          Just target -> Event.sacrifice controller target
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> pure ()
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown slot listed) ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> gs -- a player is not a permanent and has no face
          -- CR 708.2: ONE assignment to Object.facing is the whole effect. What
          -- the permanent becomes is the list the effect carries (CR 708.2a's 2/2
          -- when it lists nothing); the rule calls those copiable values, so this
          -- is a copiable swap rather than a CR 613 layer, performed by
          -- Game.faceOf. FaceDownReason.TurnedFaceDown is CR 708.6's other half:
          -- it closes CR 701.40b's turn-face-up procedure and leaves CR 702.37e's
          -- open. No CR 400.7 incarnation is minted, so the object id, marked
          -- damage, counters, attachments, statuses and the CR 613.7d timestamp
          -- all ride through -- the mirror of FaceDown.performTurnFaceUp.
          --
          -- CR 708.2b is the guard below: an effect that LISTS its own values
          -- would otherwise overwrite the list already there. No event is
          -- recorded, so nothing triggers on the turning-over (#984).
          Just target
            | maybe False (Facing.isFaceDown . Object.facing) (Map.lookup target (GameState.objects gs)) -> gs
            | otherwise ->
                gs
                  { GameState.objects =
                      Map.adjust (\o -> o {Object.facing = Facing.FaceDown FaceDownReason.TurnedFaceDown listed}) target (GameState.objects gs)
                  }
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
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
  Effect.RemoveFromCombat slot ->
    State.modify' $ \gs ->
      case legalOne slot legal of
        Just recipient -> case Recipient.objectOf recipient of
          Nothing -> gs -- a player recipient is not in combat
          -- CR 506.4: through Game.removeFromCombat, the one performer of every
          -- clause of that rule, so CR 509.1h's asymmetry comes along for free.
          -- Unprompted and undirected: the rule leaves nothing to ask.
          Just target -> Game.removeFromCombat target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op. A target
        -- already out of combat needs no guard either.
        _ -> gs
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
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone entry mSlot _ placement) ->
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
        -- but nothing in data/cards separates it from Nothing -- every
        -- ReplacementEffect.ZoneChangeR there names a `whenDestination` of
        -- Graveyard or Stack (rest-in-peace, leyline-of-the-void,
        -- anafenza-the-foremost, yawgmoths-will, synthetic-stack-interdiction),
        -- and no member of a batch moving ONTO THE BATTLEFIELD goes to either.
        -- The engine installs one row naming NO destination -- CR 702.34a's
        -- exile, Pawl.Engine.Keyword.castFromGraveyardExile -- which admits the
        -- battlefield too. It separates nothing either: the three rules that
        -- install it arm it onto an INSTANT OR SORCERY spell alone (CR 702.34a
        -- and CR 702.133a say so outright; CR 702.127a's aftermath is printed on
        -- a split card's instant and sorcery halves), and no such spell is a
        -- member of a batch entering the battlefield. A card whose ZoneChangeR
        -- named the battlefield would separate them.
        moveOne before (sofar, acc) (target, position) = do
          mNew <- Event.changeZoneEnteringIn (Just before) sofar target zone position entry (Just controller)
          -- CR 614.6: the move was cancelled, or the id was already gone (CR
          -- 603.7c). Nothing entered, so there is nothing to bind.
          Monad.forM_ mNew $ \newId ->
            -- CR 508.4, via Pawl.Engine.Combat -- which is also what keeps this
            -- from looking like a declaration, so CR 508.3a's attack triggers see
            -- nothing. CR 506.3b refuses a controller who is not the active
            -- player, which the funnel above has already settled.
            Monad.when (EntryRiders.attacking entry) (Combat.putOntoBattlefieldAttacking newId)
          pure (maybe sofar (`Set.insert` sofar) mNew, mNew : acc)
        -- The context a CHOICE's candidates are filtered in, off the board the
        -- choice is being made on: the resolution's own slots ride along, so a
        -- card offering "a permanent card from among them" (Midnight Tilling)
        -- offers only what an earlier clause of this resolution named.
        chooseContext g = effectContext controller source legal (slotGroups resolving g)
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
            -- CR 607.2a, swept once from the PRE-MOVE state (CR 608.2c, CR
            -- 608.2f). CR 400.3 files a hand arrival under Object.owner.
            ObjectRef.EachCardExiledWithSource {} -> do
              gs <- State.get
              pure (objectRefObjects legal resolving controller source gs ref)
            ObjectRef.EachSpell _ -> do
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
                Chooser.TheController -> ask controller (graveyardCards (chooseContext gs) controller gs scope filter_)
                Chooser.EachInScope ->
                  fmap concat . Monad.mapM (\pid -> ask pid (graveyardCardsOf (chooseContext gs) gs pid filter_)) $
                    graveyardPlayers controller gs scope
                -- ONE chooser, read out of the slot a ChooseOpponent bound,
                -- choosing out of their own graveyard. Through playerRefPlayers so
                -- the slot is read as every other is (CR 608.2b): an unfilled,
                -- illegal, non-player or many-valued slot names nobody, and nobody
                -- asked is nothing moved (CR 101.3). Intersected with the scope, so
                -- a chooser the scope does not name is offered nothing.
                Chooser.BoundInSlot slot ->
                  case playerRefPlayers legal controller gs (PlayerRef.InSlot slot) of
                    [pid] | List.elem pid (graveyardPlayers controller gs scope) -> ask pid (graveyardCardsOf (chooseContext gs) gs pid filter_)
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
            -- ONE card: what the group has left over is not named here at all.
            -- "The rest" is the same slot read by ObjectRef.InSlot in a LATER
            -- clause, which finds the chosen card gone -- CR 400.7 minted a new
            -- object for it on the way to its new zone, and the id the group still
            -- holds resolves to nothing, so moveOne passes over it.
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
          arrivals <- settleArrivals zone placement targets
          -- The batch's own board, read after CR 401.4's arrangement asks (which
          -- move nothing) and before any member does.
          before <- State.get
          arrived <- fmap (reverse . snd) (Monad.foldM (moveOne before) (Set.empty, []) arrivals)
          Monad.mapM_ (\slot -> bindArrivals slot (Maybe.catMaybes arrived)) mSlot
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
    Monad.forM_ (filter (`Set.member` owners) (Game.apnapOrder gs)) Mulligan.shuffleLibrary
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
  Effect.ForEach (ForEach.MkForEach ref slot body) -> do
    gs0 <- State.get
    -- CR 608.2f: WHICH members, swept ONCE from the pre-loop board and then fixed,
    -- so the body can neither shorten the batch nor add to it. Recipients rather
    -- than objects, since rule 608.2f is about "players and/or objects" and
    -- Soulfire Eruption's targets are both.
    members <- forEachOrder resolving controller (objectRefRecipients legal resolving controller source gs0 ref)
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
      -- CR 608.2c: the body's instructions in written order, per member.
      Monad.forM_ body $ \eff -> do
        defined <- State.gets (\gs -> Map.restrictKeys (Binding.targetsOf (bindingsOf gs)) bodyDefined)
        applyEffectWith runSubgame resolving source controller (withMember member defined legal) (withMember member defined chosen) eff
    State.modify' rescope
  Effect.Draw (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        named = playerRefPlayers legal controller gs ref
        -- CR 121.2c: the active player draws first, then each other player in turn
        -- order. Observable rather than cosmetic: each draw records a zone change
        -- the trigger scan reads (CR 603.2). CR 121.2d has no reader -- pawl has no
        -- teams (#175). An intersection: apnapOrder supplies the ORDER and `named`
        -- the MEMBERSHIP, which matters for a seat apnapOrder names and `named`
        -- does not -- a departure leaves the roster (CR 800.4k) but takes the
        -- library (CR 800.4a), so drawing would write drewFromEmpty.
        drawers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    -- PER DRAWER (evaluateForRecipient), every amount off the same pre-effect `gs`,
    -- so a seat drawing first cannot change what a later seat draws.
    Monad.forM_ drawers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 ->
              -- CR 121.2: draw n one at a time, so each draw re-reads the library
              -- top and the CR 104.3c empty-library loss is preserved.
              Monad.replicateM_ (Integer.toIntSaturating n) (Event.drawCard pid)
        _ -> pure ()
  Effect.Mill (Mill.MkMill ref quantity mTally mSlot) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
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
      arrived <- Maybe.catMaybes <$> Monad.mapM (\c -> Event.changeZoneReturning c Zone.Graveyard) cards
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
    Monad.forM_ mTally $ \tally ->
      let tallyContext = Filter.contextFor Nothing Nothing
          counted oid = Filter.matches tallyContext (Projection.viewOfObject oid gs) (MillTally.filter tally)
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
      -- the hand as CR 608.2c reaches it in the zone's own order (CR 400.5), seats
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
    -- (CR 608.2b) or a player recipient answers with nobody. CR 701.44d's APNAP
    -- half is objectRefObjects' order; its second key is the engine's (#1345).
    Monad.mapM_ exploreOne (objectRefObjects legal resolving controller source gs ref)
  Effect.Discard (Discard.MkDiscard slot quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
    case legalOne slot legal of
      Just (Recipient.ToPlayer target) ->
        -- One recipient, so the loop above is the identity -- but the READING is
        -- the same, so "cards equal to the number of creatures they control" is
        -- answered against the discarding player, not the controller.
        case evaluateForRecipient viewOf context gs resolving source target quantity of
          Just n
            | n > 0 -> do
                let held = Game.zoneMembers Zone.Hand target gs
                    -- CR 701.9a's move, through the shared discard funnel, so the
                    -- discard is recorded for a trigger to read.
                    bury :: [ObjectId] -> Game ()
                    bury = Monad.mapM_ (Event.discard DiscardCause.Ordinary target)
                    -- `n > 0` above, so the clamp never decides anything here.
                    count = Integer.toNaturalSaturating n
                if count >= Natural.length held
                  -- CR 609.3: discarding the whole hand is "as much as possible," so
                  -- it is forced -- no choice, so no prompt.
                  then bury held
                  else do
                    -- CR 701.9b: the discarding player chooses which cards.
                    let decider = Decide.deciderFor target gs
                    choices <- Game.choose (Prompt.ChooseDiscard decider target held count)
                    -- FILTERED AND COMPLETED, PlayerSacrifices' posture. This
                    -- branch is reached only when the hand is LARGER than the
                    -- count, so CR 609.3 does no work and every omitted card is one
                    -- the player could have discarded. Deduplicated too, since the
                    -- answer is a LIST and a card named twice would fill two of the
                    -- n slots; `valid <> filler` permutes `held`, so the take is n.
                    let valid = List.nub (filter (\c -> elem c held) choices)
                        filler = filter (\c -> List.notElem c valid) held
                    bury (List.genericTake count (valid <> filler))
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        -- Whoever the PlayerRef names loses the life. Unordered: there is no CR
        -- 121.2c for life, and CR 704.3 checks state-based actions only as a player
        -- would get priority, so no total is observable in between.
        losers = playerRefPlayers legal controller gs ref
    -- PER PAYER (evaluateForRecipient): Shahrazad's "half THEIR life" reads each
    -- payer's own total, and every number is read off the SAME `gs`.
    Monad.forM_ losers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 ->
              -- CR 119.3: the life total is simply adjusted, directly on the player
              -- record. Not through Pawl.Engine.Damage: CR 119.2 makes damage a
              -- CAUSE of life loss, not a synonym. CR 704.5a's state-based action
              -- is the existing one in Pawl.Engine.Sba.
              changeLife pid (negate n)
        _ -> pure ()
  -- CR 119.3's other half, LoseLife's mirror but for the sign. The `n > 0` guard
  -- is CR 119.9: a gain of 0 is no life gain event to trigger on.
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        gainers = playerRefPlayers legal controller gs ref
    Monad.forM_ gainers $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Just n
          | n > 0 -> changeLife pid n
        _ -> pure ()
  -- CR 701.12c: both sides reach each other's PREVIOUS total, so both deltas are
  -- read off the same game state before either is written. Written as a gain and
  -- a loss rather than two assignments, which is what puts a LifeGained and a
  -- LifeLost in the log.
  --
  -- Not implemented: CR 701.12c's deferral to CR 119.7-8, under which an
  -- exchange that would raise a player who can't gain life doesn't happen.
  -- Vacuous: Pawl.Types.PlayerEffect has no such arm to consult.
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
      -- CR 701.12a: if the entire exchange can't be completed, no part of it
      -- occurs.
      Nothing -> pure ()
  -- CR 119.5: a DELTA per player against that player's own current total, so one
  -- seat may gain while another loses. Through changeLife rather than a raw write
  -- to Player.life, for the sake of the log the rule describes.
  --
  -- Evaluated ONCE PER RECIPIENT, a card being able to name a number that is each
  -- recipient's own (Biorhythm). Every evaluation and delta is read off `gs`, the
  -- state before any life moves (CR 608.2f).
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        recipients = playerRefPlayers legal controller gs ref
    Monad.forM_ recipients $ \pid ->
      -- A player with no row is nobody to move.
      Monad.forM_ (Map.lookup pid (GameState.players gs)) $ \player ->
        -- An undeterminable total is no instruction, asked per recipient: one
        -- seat's unanswerable count says nothing about the others.
        Monad.forM_ (evaluateForRecipient viewOf context gs resolving source pid quantity) $ \total ->
          changeLife pid (total - Player.life player)
  -- CR 119.7 / 119.8: redistribute life totals, each new total being CR 119.5's
  -- gain or loss of the necessary amount. The roster is CR 102.1's players IN the
  -- game, not the keys of GameState.players, which keep a departed seat's row.
  -- Every total is read ONCE, before the prompt and before any life moves (CR
  -- 608.2h); a rotation would otherwise leave two seats on one number.
  --
  -- FILTERED, NOT TRUSTED, all-or-nothing: only a whole permutation is a
  -- legal answer, so a bad one falls back to redistributing among nobody.
  --
  -- Not implemented: CR 119.7-8's own restrictions on a player who can't gain or
  -- lose life (vacuous, as for ExchangeLifeTotals), nor CR 810.9f's "not more
  -- than one member of each team", pawl having no teams (#175).
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
      Monad.when isPermutation . Monad.forM_ (Map.toList assignment) $ \(taker, giver) ->
        changeLife taker (lifeOf giver - lifeOf taker)
  -- CR 702.179c: each named player's speed increases by this much. Its two
  -- readings -- a player who HAS speed goes up, a player with NONE has their
  -- speed BECOME the value -- are spelled separately, since DecreaseSpeed below
  -- must not create a speed out of nothing. No cap: nothing in rule 702.179
  -- bounds speed from above, and whether an effect may push past 4 is unsettled
  -- (#809).
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
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
  -- once the object moved (#1441).
  Effect.DecreaseSpeed d -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
                  let candidates = Replacement.sacrificeCandidates victim Nothing filter_ gs
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
    Monad.forM_ doomed (\(victim, oids) -> Monad.mapM_ (Event.sacrifice victim) oids)
  Effect.Create (Create.MkCreate quantity card entry mSlot creator) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        -- CR 111.2: WHOSE tokens. CR 109.5's "you" is the default reference, and
        -- every other one is somebody the sentence identified -- Rampage of the
        -- Clans' "its controller", read off the loop's member through CR 608.2h.
        -- APNAP (CR 608.2f) for a reference naming several, apnapPlayersOf's own
        -- intersection so a reference naming a departed seat mints nothing.
        creators = apnapPlayersOf creator legal controller gs
    -- PER CREATOR, every amount off the same pre-effect `gs` (CR 608.2f), so one
    -- seat's tokens cannot change how many the next seat gets.
    minted <- fmap concat . Monad.forM creators $ \creating ->
      case evaluateForRecipient viewOf context gs resolving source creating quantity of
        Just n
          | n > 0 -> do
              -- CR 111: create n tokens under that player's control (CR 111.2)
              -- through the single funnel, so CR 614's token replacements get their
              -- opportunity. CR 110.5b: the funnel is handed the entry's tap state.
              -- CR 122.6a's counters ride along through CR 122.6's own door, so a
              -- counter replacement reaches them.
              made <- Event.createTokens creating (bakeTokenCharacteristics (Quantity.evaluateFor viewOf context gs resolving source) card) Nothing (Integer.toNaturalSaturating n) (EntryRiders.tapped entry) (EntryRiders.counters entry)
              -- CR 508.4: a creature put onto the battlefield attacking has its
              -- defending player chosen in Pawl.Engine.Combat, and CR 508.3a's
              -- attack triggers see nothing. After the entry loops rather than
              -- inside them: CR 614.16's replacement settles the COUNT first.
              Monad.when (EntryRiders.attacking entry) (Monad.mapM_ Combat.putOntoBattlefieldAttacking made)
              pure made
        _ -> pure []
    case (mSlot, namesEveryToken quantity, minted) of
      (Nothing, _, _) -> pure ()
      -- Nothing was minted, so no slot names anything: an unevaluable or
      -- non-positive count, or a creator reference naming nobody (CR 101.3).
      -- createTokens itself always places what it is asked for (CR 111.2).
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
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref) -> do
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
        context = effectContext controller source legal (slotGroups resolving gs)
        sources = objectRefObjects legal resolving controller source gs ref
    -- The count is Create's, read the same way and off the same `gs` (CR 707.1).
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Just n
        | n > 0 ->
            Monad.forM_ sources $ \src ->
              Monad.forM_ (Game.cardOfWithLastKnown src gs) $ \card ->
                -- No riders: CR 707.2 copies no counters.
                --
                -- Not implemented: a copy token an effect says enters with
                -- counters on it (Ochre Jelly, Littjara Mirrorlake) arrives
                -- bare (#1255).
                --
                -- ONE call per named permanent, with the whole count: CR 614.12's
                -- entry loop is handed the batch, so the copies enter
                -- simultaneously and none may copy a sibling.
                Monad.void (Event.createTokens controller card (Just (Event.copiedSnapshotWithLastKnown src gs)) (Integer.toNaturalSaturating n) TapState.Untapped Map.empty)
      _ -> pure ()
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy originalRef subjectRef) ->
    State.modify' $ \gs ->
      -- CR 707.4: each named subject becomes a copy of the named original while
      -- staying on the battlefield. Both sides are enumerated ONCE off the same
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
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name onset duration) -> do
    gs <- State.get
    -- CR 608.2h's last-known fallback, and not belt and braces: the source can
    -- have left an opcode earlier in this same list, CR 400.7 having deleted the
    -- id `source` names. CR 603.7: a rule 702 keyword has no card text to declare
    -- the far end in, so a name a minted ability arms resolves against rule 702's
    -- own roster instead; the two namespaces are kept disjoint by Pawl.CardSpec.
    case (Game.faceOfWithLastKnown source gs >>= (Map.lookup name . Face.delayedAbilities)) <|> Keyword.mintedDelayedAbility name of
      -- For a CARD's name the dataflow lint makes a dangling one a failing test,
      -- and this arm only keeps the executor total. A MINTED name has no such
      -- lint, so a forgotten roster row lands here and does nothing.
      Nothing -> pure ()
      Just ability ->
        -- CR 603.7d-f: the controller is the player who controlled the spell or
        -- ability AS IT RESOLVED, baked in now. CR 603.7a: an entry appended here
        -- never fires on an event that already happened.
        let captured = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)
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
                  DelayedTrigger.expiry = duration >>= \d -> Expiry.arm (Binding.playersIn legal) controller source d gs
                }
         in State.put gs {GameState.delayedTriggers = GameState.delayedTriggers gs Seq.|> entry}
  Effect.Replace (Replace.MkReplace duration uses origin condition re) ->
    -- CR 614.3 / 615.3: install the floating replacement. Targetless and
    -- unprompted. CR 113.7: the SOURCE is this effect's source, which with the
    -- timestamp is the row's CR 614.5 identity (#687). CR 614.15: the ORIGIN
    -- travels with the row rather than being re-derived.
    State.modify' $ \gs ->
      -- The clause's own "if", read with the resolution's controller as CR
      -- 109.5's "you". The full view, not viewWithLastKnown: a spell creating a
      -- self-replacement is on the stack and the board is live.
      let context = effectContext controller source legal (slotGroups resolving gs)
          met = maybe True (Condition.holds (Projection.fullView gs) context gs source) condition
       in case (met, Expiry.arm (Binding.playersIn legal) controller source duration gs) of
            -- The stated condition is false, so the clause creates nothing.
            (False, _) -> gs
            -- CR 611.2b: the duration never started.
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
                        -- when the row is consulted.
                        ActiveReplacement.controller = controller,
                        ActiveReplacement.timestamp = ts,
                        ActiveReplacement.expiry = expiry,
                        ActiveReplacement.uses = uses,
                        ActiveReplacement.origin = origin,
                        ActiveReplacement.rider = Nothing,
                        -- The resolution's own slot bindings, captured as the
                        -- row is installed for the reason Pawl.Types.DelayedTrigger
                        -- captures them (CR 603.7c): this resolution is about to
                        -- end and its object with it, so a pattern naming a slot
                        -- would have nothing live to read. The SAME context the
                        -- condition above was asked in, so the gate and the row
                        -- cannot disagree about what a slot names.
                        ActiveReplacement.slots = Filter.slotObjects context
                      }
               in gs1 {GameState.replacements = active : GameState.replacements gs1}
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration kind ref sourceFilter quantity riderEffects) -> do
    -- CR 615.3 / 615.7: install one floating prevention shield per recipient the
    -- ref names, consulted at the damage funnel until the shield is spent or the
    -- duration expires. Its own opcode rather than an Effect.Replace carrying a
    -- DamageR, because the pattern has to name the shielded permanent or player
    -- by id, which card data cannot. Through Damage.damageRecipient, so the baked
    -- recipient is in the same vocabulary a DamageEvent's target arrives in (CR
    -- 120.1a).
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
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
        -- baked into a row. The recipients are asked for the same reason: CR
        -- 608.2b's gone target leaves none, so there is nothing to shield.
        Monad.when (n > 0 && not (null recipients)) $ do
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
                 in List.foldl' (installDamageRow (Binding.playersIn legal) controller source duration kind (DamageRewrite.PreventNext amount) rider) g0 (fmap (\recipient -> (Just recipient, sourceChoice)) recipients)
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration kind ref direction riderEffects) -> do
    -- CR 615.1 / 615.3: one floating shield per object the ref names, with no
    -- amount to count down. PreventNextDamage's row but for its rewrite, hence
    -- the shared `installDamageRow`; CR 615.7's "reduced to 0" terminator does
    -- not exist here, so only the duration ends it. The recipient side goes
    -- through Damage.damageRecipient for PreventNextDamage's reason (CR
    -- 120.1a); the source side is an ObjectId and needs no such translation.
    gs <- State.get
    let named = objectRefRecipients legal resolving controller source gs ref
        -- Which SIDE of the damage event the ref's objects sit on. A DealtBy
        -- row watches CR 120.1's source, which is always an OBJECT, so a player
        -- the ref named drops out -- and it names no recipient, so the damage
        -- Dovin, Hand of Control's shielded permanent would deal to a PLAYER is
        -- prevented as much as the damage it would deal to a permanent.
        rows = case direction of
          DamageDirection.DealtTo -> fmap (\recipient -> (Just recipient, Nothing)) (Maybe.mapMaybe (Damage.damageRecipient gs) named)
          -- The trivial predicate beside the id: CR 615.9's recheck is of a
          -- CHOSEN source's printed properties, and a target has none -- CR
          -- 608.2b already rechecked its legality as the ability resolved.
          DamageDirection.DealtBy -> fmap (\oid -> (Nothing, Just (Filter.Type.And [], oid))) (Maybe.mapMaybe Recipient.objectOf named)
        -- CR 615.5's additional effect. With no amount to count down, "this way"
        -- is what THIS application prevented, which Prevention.amount carries.
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
    -- The source half is CR 601.2c's TARGET where the direction is DealtBy;
    -- CR 609.7a's player CHOICE is a different question, and this opcode has no
    -- chosenSource field for a card to ask it (Samite Ministration, Consulate
    -- Surveillance) (gap #1902).
    State.modify' $ \g0 -> List.foldl' (installDamageRow (Binding.playersIn legal) controller source duration kind DamageRewrite.PreventAll rider) g0 rows
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration kind srcRef destRef) -> do
    -- CR 614.9: install a floating redirection effect. BOTH sides are baked here,
    -- both being known only at resolution: the source side into
    -- DamagePattern.whichRecipient, the destination into the rewrite. Both
    -- through Damage.damageRecipient (CR 120.1a). The rule's own guard is re-asked
    -- at redirect time, in Event.apply, the destination being able to leave.
    gs <- State.get
    let recipientsOf ref = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legal resolving controller source gs ref)
    -- EXACTLY ONE destination (CR 614.9). None means CR 608.2b's target is
    -- already gone, so no row is installed.
    case recipientsOf destRef of
      [dest] ->
        -- Nothing for CR 609.7a's choice: this opcode has no chosenSource field
        -- either, so Kor Chant's redirection "by a source of your choice" is
        -- unsayable (gap #1902).
        State.modify' $ \g0 -> List.foldl' (installDamageRow (Binding.playersIn legal) controller source duration kind (DamageRewrite.Redirect dest) Nothing) g0 (fmap (\recipient -> (Just recipient, Nothing)) (recipientsOf srcRef))
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
          MonarchTarget.ControllerOfSource ->
            Map.lookup Binding.triggerSource chosen
              >>= Binding.onlyOne
              >>= Recipient.objectOf
              >>= (\o -> Projection.controllerOf o gs)
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
  -- Not implemented: CR 716's "when this Class becomes level N" triggered
  -- abilities, which want an event carrying the level before and after, the way
  -- GameEvent.CountersPut carries a counter tally for CR 603.2's threshold
  -- crossing (#1944).
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel level slot) ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target ->
          State.modify'
            (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.classLevel = Just level}) target (GameState.objects g)})
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
    _ <- Daytime.becomes designation
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
  Effect.ExileUntilMonarch slot ->
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        Just target -> do
          -- CR 400.7: exile through the funnel and register the incarnation for
          -- return when an opponent of `controller` (CR 102.2) BECOMES the
          -- monarch. Armed undischarged whoever holds the crown now, so an
          -- opponent who already holds it does not free the creature.
          mNew <- Event.changeZoneReturning target Zone.Exile
          case mNew of
            Nothing -> pure ()
            Just newId -> do
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
  Effect.Counter (Counter.MkCounter ref mSlot) -> do
    gs <- State.get
    -- CR 701.6a: counter each named object through the single funnel, which picks
    -- that rule's ending from each object's own kind. A player recipient or an
    -- illegal target (CR 608.2b) counters nothing. The whole set goes as ONE
    -- batch (CR 608.2f).
    --
    -- The funnel is handed THIS effect's source and controller, which Baral,
    -- Chief of Compliance reads off the event: by the CR 117.5 trigger scan the
    -- controller can no longer be asked for exactly (see Pawl.Types.Countering).
    countered <- Event.counterReturning source controller (objectRefObjects legal resolving controller source gs ref)
    -- CR 701.6a's "countered this way" is what the funnel COUNTERED, never what
    -- the sweep named. Bound onto this effect's SOURCE, and bound even at zero.
    Monad.forM_ mSlot $ \slot ->
      State.modify' (bindAmountSlot source slot (Natural.length countered))
  Effect.PutCounters (PutCounters.MkPutCounters kind quantity ref) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        -- CR 608.2c: the set is swept as this instruction is reached, and an
        -- illegal slot (CR 608.2b) or a player recipient answers with nobody.
        targets = objectRefObjects legal resolving controller source gs ref
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
      -- ONE evaluation for the whole set (CR 608.2f), then CR 122.6's funnel per
      -- recipient, so CR 614's counter replacements apply against each placement.
      Just n ->
        Monad.when (n > 0) . Monad.forM_ targets $ \target ->
          Event.putCounters (CounterCause.ByEffect controller) target kind (Integer.toNaturalSaturating n)
  -- CR 122: PutCounters' mirror, deliberately NOT through a CR 614.16 gate --
  -- nothing in CR 614 replaces a removal.
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters kind quantity slot) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
    case legalOne slot legal of
      Just recipient -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient has no object counters
        Just target -> case Quantity.evaluateFor viewOf context gs resolving source quantity of
          Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
          Just n -> Monad.when (n > 0) (Event.removeCounters target kind (Integer.toNaturalSaturating n))
      _ -> pure () -- illegal slot at resolution (CR 608.2b): no-op
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
  -- Not implemented: rule 101.4's actions happen SIMULTANEOUSLY, and each
  -- blighter's counters are placed before the next is asked instead (#1651).
  --
  -- Not implemented: nothing records which creature was blighted, so CR 701.68c's
  -- "blighted creature" and CR 701.68d's trigger have nothing to read (#1492).
  Effect.Blight (PlayerQuantity.MkPlayerQuantity ref quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
        named = playerRefPlayers legal controller gs ref
        blighters = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    Monad.forM_ blighters $ \pid ->
      case evaluateForRecipient viewOf context gs resolving source pid quantity of
        Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
        Just n -> Monad.void (Blight.blight pid resolving (Integer.toNaturalSaturating n))
  -- CR 701.54a: the Ring tempts the resolving controller; the keyword action is
  -- Pawl.Engine.Ring.tempt's.
  Effect.TemptWithTheRing -> Ring.tempt controller
  -- CR 701.49: the whole keyword action, which Pawl.Engine.Dungeon owns.
  Effect.Venture -> Dungeon.venture controller
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters ref kind quantity) -> do
    gs <- State.get
    let viewOf = effectViewOf source legal gs
        context = effectContext controller source legal (slotGroups resolving gs)
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
        context = effectContext controller source legal (slotGroups resolving gs)
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
  Effect.Tap ref ->
    State.modify' $ \gs ->
      -- CR 701.26a: turn each named permanent sideways. The victims are
      -- enumerated ONCE (CR 608.2f), so an illegal slot (CR 608.2b), a player
      -- recipient and a set that matched nothing all tap nothing. Rule 701.26a's
      -- "only untapped permanents can be tapped" needs no guard: the assignment
      -- is idempotent.
      let tap o = o {Object.tapped = TapState.Tapped}
       in gs
            { GameState.objects =
                foldr (Map.adjust tap) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
  Effect.Untap ref ->
    State.modify' $ \gs ->
      -- CR 701.26b: rotate each named permanent back upright. The victims are
      -- enumerated ONCE (CR 608.2f); an illegal slot (CR 608.2b), a player
      -- recipient and an empty match all untap nothing.
      let untap o = o {Object.tapped = TapState.Untapped}
       in gs
            { GameState.objects =
                foldr (Map.adjust untap) (GameState.objects gs) (objectRefObjects legal resolving controller source gs ref)
            }
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
  Effect.Transform ref ->
    State.modify' $ \gs ->
      -- CR 701.27a: turn each victim over -- one assignment to Object.face, which
      -- is all a turn IS here, every characteristic read already resolving
      -- through Game.faceOf. The victims are enumerated ONCE (CR 608.2f). WHICH
      -- face is Pawl.Engine.Card.turnedOver's answer, off the card's layout,
      -- which withholds a turn from a permanent that is not double-faced (CR
      -- 701.27c) or whose other face is an instant or sorcery (CR 701.27d).
      --
      -- CR 701.27b: turning over is its own game action, and a card that triggers
      -- ON it needs a trigger condition pawl does not have (#695). ONE fresh
      -- timestamp for the whole instruction (CR 701.27f), minted even when nothing
      -- turns over, and ONE whole-board projection, CR 702.145b's restriction
      -- being read off the layer fold.
      let (now, g1) = Game.freshTimestamp gs
          pcs = Projection.projectAll g1
       in g1
            { GameState.objects =
                foldr (turnOver pcs resolving now g1) (GameState.objects g1) (objectRefObjects legal resolving controller source g1 ref)
            }
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
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn ref skips) -> do
    gs <- State.get
    let named = playerRefPlayers legal controller gs ref
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
    let entry pid = ExtraTurn.MkExtraTurn {ExtraTurn.taker = pid, ExtraTurn.source = source, ExtraTurn.skipped = skips}
    State.modify' (\g -> g {GameState.extraTurns = List.foldl' (\ts pid -> entry pid : ts) (GameState.extraTurns g) takers})

-- CR 119.3: move one player's life total by this much, and record the CR 608.2i
-- event of the matching sign. The write LoseLife, GainLife and
-- ExchangeLifeTotals share, so a life total moves in exactly one place.
--
-- A zero delta writes nothing at all: CR 119.9 says so for the gain side, and the
-- loss side takes the same posture.
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
-- CR 615.5's "amount of damage that was prevented" is a Quantity.InSlot read of
-- the reserved Binding.eventAmount slot, published through
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
  State.modify' (\gs -> gs {GameState.ambientAmounts = Map.insert Binding.eventAmount (Prevention.amount prevention) was})
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
bindPlayerSlot :: ObjectId -> SlotName -> PlayerId -> GameState -> GameState
bindPlayerSlot holder slot player gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toPlayer player) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

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

-- CR 701.23: do to a found card what the search said -- a move for every
-- destination and, for one of them, a CR 701.20a reveal first, through the CR
-- 400.7 funnel either way.
putFound :: PlayerId -> ObjectId -> SearchDestination.SearchDestination -> ObjectId -> Game ()
putFound searcher source destination cardId = case destination of
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
  SearchDestination.BattlefieldAttachedToSource -> do
    gs <- State.get
    case Attach.attachmentFor cardId (Recipient.ToObject source) gs of
      Nothing -> pure ()
      Just seed ->
        Monad.void
          (Event.changeZoneAttaching Nothing Set.empty cardId Zone.Battlefield LibraryPosition.defaultValue (Just seed) TapState.Untapped Map.empty (Just searcher) Nothing Facing.FaceUp False)

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
    -- CR 122.6 through the one counter funnel, so a CR 614.16 replacement
    -- (Hardened Scales) gets its opportunity against this placement too.
    grow pid = Monad.void (Event.putCounters (CounterCause.ByEffect pid) oid CounterKind.PlusOnePlusOne 1)
