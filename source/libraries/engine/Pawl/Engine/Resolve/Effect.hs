-- Carrying out one effect (CR 608.2): applyOneEffect, the sole casing on
-- Effect at resolution, with the prevention riders, hand actions and mana
-- abilities that share its recursion. Split out of Pawl.Engine.Resolve for
-- size; the spell and ability resolution that drives it stays there, and both
-- are imported by callers under the same Resolve alias.
module Pawl.Engine.Resolve.Effect where

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
import qualified Pawl.Engine.Coin as Coin
import qualified Pawl.Engine.Combat as Combat
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
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Plot as Plot
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replacement as Replacement
import Pawl.Engine.Resolve.Slots (battlefieldMatching, boundSlots, conditionSlots, effectContext, effectViewOf, graveyardCardsOf, handCardsOf, legalMany, legalOne, matchingFromAmong, objectRefObjects, playerRefPlayers, replacementRowSlots, slotBindings, slotGroup, zoneScopePlayers)
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
import qualified Pawl.Types.ActiveActivationProhibition as ActiveActivationProhibition
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
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.AttachTarget as AttachTarget
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
import qualified Pawl.Types.ChoosePlayer as ChoosePlayer
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.CoinReading as CoinReading
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
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndTurnSignal as EndTurnSignal
import qualified Pawl.Types.EndingStep as EndingStep
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
import qualified Pawl.Types.ForbidActivation as ForbidActivation
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
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.Modal as Modal.Type
import qualified Pawl.Types.ModeIndex as ModeIndex
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
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PendingTrigger as PendingTrigger
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
import qualified Pawl.Types.RandomCardInHand as RandomCardInHand
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
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneScope as ZoneScope

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
--   1. IS THERE ANYTHING TO OFFER -- an id the reference named (CR 400.7) may no
--      longer resolve to an object (CR 603.7c).
--   2. WHICH FACE: CR 712.11a for the `transformed` rider, otherwise
--      Card.castableFaces (CR 709.3, CR 712.11b, CR 715.3, CR 720.3), less the face CR
--      702.162a's alternative cost is the only road to when this offer states an
--      alternative cost of its own (CR 118.9a).
--   3. WHAT IT COSTS (CR 118.9): `withoutPayingManaCost` or a stated
--      `payingInstead` (CR 702.94a); otherwise CR 601.2b's own candidates, and
--      how mana may be spent toward it (CR 118.14's `spending`).
--   4. MAY IT BE CAST AT ALL -- Cast.castableWhenOffered, asked BEFORE the
--      prompt so no cast is offered that the announcement would reverse.
--
-- Questions 3 and 4 are asked of EACH half of EACH named card separately (CR
-- 709.3a, CR 712.11c); where more than one survives, CR 601.3's choice is put to
-- the caster before
-- the "may" below, since CR 118.8c's excuse is a property of the spell being
-- cast. At CastObligation.Mandatory the cast is not a decision, so
-- Prompt.OfferedCast is elided; question 4 is what a printed "if able" comes to
-- (CR 601.3, CR 609.3). CR 118.8c is the exception: `excused` turns the
-- mandatory branch back into a may, classified by Cost.statesHiddenQuality.
--
-- The caster is a parameter and not the resolving controller: CR 608.2g says "a
-- player". Everything above is a CLASSIFICATION carried by the opcode's
-- CastOffer and its CastObligation; nothing here asks which card is offered.
offerCast :: [ObjectId] -> PlayerId -> CastObligation.CastObligation -> CastOffer.CastOffer -> Game ()
offerCast named caster optionality offer = do
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
            candidates = maybe (Cost.candidateCostsGiven True caster name oid proposed) (pure . Cost.untagged) applied
         in if Cast.castableWhenOffered (CastOffer.spending offer) caster oid name candidates proposed
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
      -- EVERY object the reference names, each contributing one entry per
      -- castable half (CR 709.3a) -- Shell of the Last Kappa's whole exiled pile
      -- as readily as Tinybones, the Pickpocket's one target. The caller's
      -- objectRefObjects is what makes the two the same read.
      offers =
        concatMap
          ( \oid -> Maybe.fromMaybe [] $ do
              card <- Game.cardOf oid gs
              fmap (Maybe.mapMaybe (proposal oid)) (faces card)
          )
          named
  -- No survivor is no offer; one survivor is one outcome, so CR 601.3's choice is
  -- elided there rather than asked.
  chosen <- case offers of
    [] -> pure Nothing
    [sole] -> pure (Just sole)
    first : rest -> do
      let decider = Decide.deciderFor caster gs
          keyOf (oid, name, _, _) = (oid, name)
      picked <- Game.choose (Prompt.ChooseOfferedCastSpell decider caster (fmap keyOf (first NonEmpty.:| rest)))
      -- Reject-not-repair: a pair the offer did not include is no cast at all.
      -- The PAIR and not the name alone, for castWhileSearching's reason: one
      -- card's half must not answer another card's.
      pure (List.find ((== picked) . keyOf) offers)
  case chosen of
    Nothing -> pure ()
    Just (oid, name, applied, excused) -> do
      let cast = Cast.castSpellWith performManaAbility True applied (CastOffer.spending offer) caster oid name Facing.FaceUp
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
  ReplacementEffect.LifeGainR _ -> []
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
-- ChoosePlayer filled earlier in this resolution. Read through playerRefPlayers
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
-- What insertWith ALSO keeps is CR 614.14's link, which Pawl.Engine.Event's
-- EntryRewrite.ExileFromGraveyard arm files as the entry replacement applies --
-- inside this window, since the permanent enters as a spell resolves, and the
-- card belongs to the entering permanent rather than to that spell (Living
-- Lore). It keeps CR 607.2b's link the same way, which Pawl.Engine.Event files
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

-- CR 707.10d and CR 707.10e's shared test, and the whole of what either rule
-- leaves to arithmetic: for each candidate, the target maps the copy would carry
-- if EVERY instance of the word "target" named that candidate. A candidate that
-- cannot fill them all is dropped, which is rule 707.10d's last sentence -- "if
-- that player or object isn't a legal target for each instance of the word
-- 'target', a copy isn't created for that player or object" -- and rule
-- 707.10e's "the copy isn't created", the same test with one candidate.
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
copyRetargets :: PlayerId -> ObjectId -> GameState -> [ObjectId] -> [(ObjectId, Map.Map SlotName (Set Recipient))]
copyRetargets controller original gs candidates = Maybe.fromMaybe [] $ do
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

-- CR 707.10e: ONE copy, every one of whose targets is the object the effect
-- names. Answers the empty list where "the copy isn't created", so the copy does
-- not exist rather than existing and being countered for an illegal target (CR
-- 608.2b).
--
-- A ref naming anything but exactly ONE object also answers the empty list: rule
-- 707.10e specifies "a new target", singular, so a ref that swept several has
-- not said which, and a ref that named none has said nothing. Ivy, Gleeful
-- Spellthief's ref is the effect's own source, so a departed source is what
-- reaches the second case.
--
-- No prompt of any kind: rule 707.10d's order is a choice among several copies,
-- and there is only ever one here.
copyStatedTargets :: PlayerId -> ObjectId -> ObjectId -> Map.Map SlotName (Set Recipient) -> ObjectId -> ObjectRef -> Game [Map.Map SlotName (Set Recipient)]
copyStatedTargets controller resolving source legal original newRef = do
  gs <- State.get
  pure $ case objectRefObjects legal resolving controller source gs newRef of
    [new] -> fmap snd (copyRetargets controller original gs [new])
    _ -> []

-- CR 707.10d: the copies' targets, one map per candidate, in the order their
-- controller chose. Answers the empty list where nothing is copied at all.
--
-- The candidates are the card's own description ("each other creature you
-- control"), narrowed by the rule's "could target" -- copyRetargets above, whose
-- test rule 707.10e shares.
--
-- The ORDER is the whole of what CR 707.10d leaves to a player, and
-- Prompt.OrderForEach is the question; the rule states no primary key, so the
-- one prompt covers the whole list rather than forEachOrder's APNAP groups.
-- Elided for fewer than two, which is one order.
copyForEachTargets :: PlayerId -> ObjectId -> ObjectId -> Map.Map SlotName (Set Recipient) -> ObjectId -> ObjectRef -> Game [Map.Map SlotName (Set Recipient)]
copyForEachTargets controller resolving source legal original candidateRef = do
  gs <- State.get
  let picks = copyRetargets controller original gs (objectRefObjects legal resolving controller source gs candidateRef)
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
  -- The second place mana reaches a pool (CR 106.3). An ACTIVATED mana ability is
  -- applied by Cost.tapForMana and never resolves (CR 605.3b), and CR 605.4a's
  -- triggered one reaches this arm through performTriggeredManaAbility rather
  -- than off the stack; what resolves here in the ordinary way is a triggered
  -- producer CR 605.1b leaves out, Burning-Tree Emissary's shape.
  --
  -- Not implemented: CR 605.1b's other two triggers -- mana being added, and a
  -- mana ability being activated -- so a producer that watches either still
  -- resolves off the stack here (#1572).
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
  Effect.AddMana (ManaAddition.MkManaAddition ref production count retention restriction rider) -> do
    gs0 <- State.get
    let howMany = Natural.toIntSaturating count
    case Mana.producedTypes source gs0 production of
      -- One settled type needs no question; the COUNT is how many units this one
      -- instruction adds, and a clause adding mana of two DIFFERENT types writes
      -- two effects, run in printed order (CR 608.2c).
      [manaType] ->
        let unit =
              ManaUnit.MkManaUnit
                { ManaUnit.manaType = manaType,
                  ManaUnit.tags = Mana.productionTagsGiven Map.empty source gs0,
                  ManaUnit.retention = retention,
                  ManaUnit.restriction = restriction,
                  ManaUnit.rider = rider
                }
         in State.modify' (\gs -> foldr (\pid -> Mana.addMana pid (replicate howMany unit)) gs (playerRefPlayers legal controller gs0 ref))
      -- No type at all is CR 607.2d's "the chosen color" with nothing chosen:
      -- adding nothing is the honest answer.
      [] -> pure ()
      -- Several types is CR 105.4's choice, and it is the RECIPIENT's: CR 106.3
      -- has the effect instruct a player to add the mana, and CR 106.4 puts it in
      -- that player's pool. CR 101.4: several recipients are asked in APNAP
      -- order, with apnapOrder supplying the ORDER and the ref the MEMBERSHIP; a
      -- recipient apnapOrder does not name keeps its place at the end rather than
      -- losing the mana CR 106.4 puts in their pool.
      --
      -- CR 105.4's choice is made ONCE for the whole instruction, which is what
      -- "two mana of any one color they choose" says (Stadium Vendors): the
      -- count replicates the unit the answer settled rather than asking again.
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
              State.modify' (Mana.addMana pid (replicate howMany unit))
  Effect.Search (Search.MkSearch searcherRef ownerRef zones quantity filter_ upTo destination) ->
    -- CR 701.23a: match each candidate through its own CR 613 projection --
    -- rule 613.1 names no zone, so a card in any of the searched zones is folded
    -- exactly as a permanent is, and CR 208.2a's characteristic-defining power
    -- rides along at layer 7a.
    --
    -- Through effectContext and NOT Filter.contextFor, so the resolution's own
    -- slots ride along and a search filter naming one is answerable: Bifurcate's
    -- "with the same name as target nontoken creature" is CR 201.2a's
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
  -- CR 608.2d: "choose an opponent" (Skullwinder) or "choose a player" (Stadium
  -- Vendors), announced as this effect is applied and bound so the sentence
  -- after it can say "that player". NOT A TARGET (CR 115.10a), so nothing was
  -- announced at CR 601.2c and the pick is made here, against the board as this
  -- effect runs.
  --
  -- WHICH players are offered is the payload's PlayerScope, read through
  -- PlayerEffect.playersInScope against CR 109.5's "you" -- the resolving
  -- controller -- so this arm classifies the choice and never names a card. That
  -- fold is over Game.stillPlaying, so a seat that has left (CR 104.3a) is not
  -- offered; CR 102.2 leaves "an opponent" nothing to decide at two seats, where
  -- "a player" there has two candidates and must be asked. An answer naming
  -- somebody never offered falls back to the first candidate, since the
  -- instruction is mandatory. Nobody in scope binds nothing, so the following
  -- sentence names no player and does nothing (CR 101.3).
  --
  -- The PROMPT is picked by whether the offer contains the chooser, which is
  -- what separates Prompt.ChooseOpponent (whose haddock claims it never offers
  -- them) from Prompt.ChoosePlayer -- a property of the candidate set rather
  -- than of the scope's name, so no arm of PlayerScope can drift out of step
  -- with it.
  Effect.ChoosePlayer choice -> do
    gs <- State.get
    let slot = ChoosePlayer.slot choice
        candidates = Maybe.fromMaybe [] (PlayerEffect.playersInScope (Just controller) gs (ChoosePlayer.scope choice))
    chosenPlayer <- case candidates of
      [] -> pure Nothing
      [sole] -> pure (Just sole)
      first : second : rest -> do
        let offered = first NonEmpty.:| (second : rest)
            decider = Decide.deciderFor controller gs
            question =
              if List.elem controller (NonEmpty.toList offered)
                then Prompt.ChoosePlayer decider controller source offered
                else Prompt.ChooseOpponent decider controller source offered
        answer <- Game.choose question
        pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
    Monad.forM_ chosenPlayer $ \pid -> State.modify' (bindPlayerSlot resolving slot pid)
  -- ChoosePlayer's twin with the decision replaced by randomness (Ruhan of the
  -- Fomori): the same filter, the same bind, and the same CR 608.2d moment --
  -- the question changes, and so does the offer, which is CR 102.3's opponents
  -- with no scope beside the slot to widen them (#3230). CR 701.9b's distinction
  -- between "at random" and "the player chooses" is why it is a separate opcode
  -- and a separate prompt.
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
  -- ONE ENTRY PER INSTRUCTION, however many dice CR 706.1's count named, where
  -- FlipCoin below records one per coin: the condition reading this event is
  -- Feywild Trickster's "whenever you roll one or more dice", which the printed
  -- words scope to the instruction rather than to the die. An instruction that
  -- rolls no dice at all records nothing, having rolled none.
  Effect.RollDie rollDie -> do
    before <- State.get
    let sides = RollDie.sides rollDie
        -- How many dice the instruction throws, read ONCE before the first of
        -- them, FlipCoin's count below and for its reason: the number is part of
        -- the instruction, and re-reading it per die would let a count over the
        -- board throw a different number than the instruction named. CR 107.2's
        -- posture for a quantity that cannot be evaluated: no dice at all.
        dice =
          Integer.toNaturalSaturating
            ( Maybe.fromMaybe
                0
                ( Quantity.evaluateFor
                    (effectViewOf source legal before)
                    (effectContext before controller source legal (slotBindings resolving before))
                    before
                    resolving
                    source
                    (RollDie.count rollDie)
                )
            )
        rollOne = do
          rolled <- Game.ask (Prompt.RollDie sides)
          gs <- State.get
          let viewOf = effectViewOf source legal gs
              context = effectContext gs controller source legal (slotBindings resolving gs)
              -- CR 706.2, read AFTER the roll as the rule words it, and per die:
              -- the rule adds the modifier to the natural result of a roll, so a
              -- count above one adds it to each. CR 107.2's posture for a
              -- modifier that cannot be evaluated: no modifier at all.
              modifier = case RollDie.modifier rollDie of
                Nothing -> 0
                Just quantity -> Maybe.fromMaybe 0 (Quantity.evaluateFor viewOf context gs resolving source quantity)
              natural = if rolled >= 1 && rolled <= sides then rolled else 1
          pure (Integer.toNaturalSaturating (toInteger natural + modifier))
    results <- traverse (const rollOne) [1 .. dice]
    Foldable.for_ (NonEmpty.nonEmpty results) $ \offered -> do
      gs <- State.get
      -- CR 706.4: WHICH result the instruction uses, where it threw more than
      -- one ("roll two d6 and choose one result"). A choice and not a roll, so
      -- it goes through Game.choose and CR 723.5's controller may make it.
      -- Elided where every result is the same number: both bindings below come
      -- out the same whichever die is named, so no board can tell the answers
      -- apart. FILTERED, NOT TRUSTED, the ChooseBolster posture: an index past
      -- the end takes the first die rolled.
      index <-
        if all (== NonEmpty.head offered) (NonEmpty.tail offered)
          then pure 0
          else do
            answer <- Game.choose (Prompt.ChooseDieResult (Decide.deciderFor controller gs) controller resolving offered)
            pure (if answer < List.genericLength results then answer else 0)
      State.modify' (bindAmountSlot source (RollDie.slot rollDie) (Replacement.at results index (NonEmpty.head offered)))
      -- CR 706.4's "the other result", off the same throw rather than re-derived
      -- from the count, FlipCoin's `misses` below and for its reason. Only a
      -- two-die instruction has an "other": at any other count what is left is
      -- not one number, and the slot stays unbound rather than guessing which of
      -- them the card meant. Pawl.CardSpec's lint keeps data/cards/ to counts
      -- this can answer for.
      Foldable.for_ (RollDie.other rollDie) $ \other ->
        case [result | (i, result) <- zip [0 ..] results, i /= index] of
          [rest] -> State.modify' (bindAmountSlot source other rest)
          _ -> pure ()
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
                  -- ONE chooser, read out of the slot a ChoosePlayer bound,
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
              -- under this opcode names no object; that rule's other exception --
              -- a discard another player chooses -- needs a design call (#1733).
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
  Effect.OfferCast (OfferCast.MkOfferCast ref caster optionality offer) -> do
    gs <- State.get
    -- The sweep every ObjectRef-taking opcode shares, read HERE rather than
    -- inside offerCast so that one function takes the objects and never the
    -- reference: CR 601.3's offer over a set (Shell of the Last Kappa) and over
    -- one target (Tinybones, the Pickpocket) are then the same call.
    let named = objectRefObjects legal resolving controller source gs ref
    -- CR 608.2g names "a player", and a reference resolving to nobody offers the
    -- cast to nobody.
    Monad.forM_ (playerRefPlayers legal controller gs caster) $ \pid ->
      offerCast named pid optionality offer
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
    -- Through effectContext and NOT Filter.contextFor, so the resolution's own
    -- reads answer here as they do at every other position of it (CR 608.2c): the
    -- slots earlier clauses bound, their names and their amounts, and CR 201.4's
    -- chosen name overlaid the way the Effect.Search arm overlays it -- Predict's
    -- "a card with the chosen name was milled this way", which Pawl.ResolveSpec
    -- proves. Read off the LIVE state rather than the pre-effect `gs` the views
    -- come from, since the clause that chose the name is one this resolution has
    -- already carried out.
    --
    -- The VIEWS stay on the pre-effect `gs` for the reason above: CR 400.7 has
    -- minted new ids for the cards themselves, and `milled` holds the library
    -- ones. So the mill's OWN slot, bound just above off the arrival ids, names
    -- nothing this fold can match -- Filter.IsBound over it is False here whatever
    -- the map holds. Not implemented, and not what the rule says: CR 400.7j makes
    -- a card the same effect moved to a PUBLIC zone findable by the rest of that
    -- effect, and a graveyard is one, so the False is this fold's pre-move views
    -- rather than a rule-sanctioned answer (gap #2141).
    --
    -- The chosen-name half is PROVED (Pawl.ResolveSpec's "Predict's mill tally
    -- reads the name its own first clause chose"); the SLOT half is a regression
    -- fence, since no card in the pool names an earlier clause's slot from a
    -- tally, and swapping effectContext back for a bare Filter.contextFor leaves
    -- the whole suite green. It is written because CR 608.2c states it.
    --
    -- Bound onto this effect's SOURCE, so a later effect reads it as
    -- Quantity.InSlot; bound even at zero, since zero is an answer. ONE number
    -- across every miller, as no Quantity has a per-player reader.
    Monad.forM_ mTally $ \tally -> do
      gs' <- State.get
      let tallyContext = (effectContext gs' controller source legal (slotBindings resolving gs')) {Filter.sourceChosenNames = PlayerEffect.chosenNamesOf (Just source) gs'}
          viewOfMilled = Projection.viewsOf gs
          counted oid = Filter.matches tallyContext (viewOfMilled oid) (MillTally.filter tally)
      State.modify' (bindAmountSlot source (MillTally.slot tally) (Natural.length (filter counted milled)))
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
    -- bindObjectsSlot: the single shape is the one every reader sees -- slotOne
    -- included, where Filter.IsBound reads either. Used by the ChosenCardFromAmong
    -- arm alone, which is where that write-once-per-card shape is elided (#2859);
    -- the random arm below binds the whole group it named in one write.
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
      -- Elided at one candidate and skipped at none (CR 101.3, CR 609.3).
      -- Candidates are the hand as CR 608.2c reaches it in the zone's own order (CR
      -- 402.3) narrowed by the ref's own Filter, and the seats come from
      -- handChoosers so the asks run in CR 608.2e's APNAP order.
      --
      -- The count names DISTINCT cards -- Fall's "two cards at random" is two cards
      -- and not two picks that may coincide -- so each card named is dropped from
      -- the candidates before the next ask. CR 609.3 caps the run at the hand's
      -- matching size.
      ObjectRef.RandomCardInHand (RandomCardInHand.MkRandomCardInHand player filter_ count) -> do
        let viewOf = effectViewOf source legal gs
            context = effectContext gs controller source legal (slotBindings resolving gs)
            wanted = maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source count)
            pick remaining candidates =
              if remaining <= (0 :: Natural)
                then pure []
                else case candidates of
                  [] -> pure []
                  [only] -> pure [only]
                  first : second : more -> do
                    answer <- Game.ask (Prompt.RandomObject (first NonEmpty.:| (second : more)))
                    let named = if List.elem answer candidates then answer else first
                    rest <- pick (remaining - 1) (filter (/= named) candidates)
                    pure (named : rest)
        -- Every card named across every seat, so the binding below sees one group
        -- rather than one write per seat -- Fall reads it back with an
        -- EachCardFromAmong, which a per-seat single binding would answer with the
        -- last card alone.
        picked <-
          fmap concat . Monad.mapM (\pid -> fmap (fmap ((,) pid)) (pick wanted (handCardsOf context gs pid filter_))) $
            handChoosers legal controller gs player
        Monad.mapM_ (uncurry (Event.reveal RevealCause.Ordinary)) picked
        Monad.forM_ mSlot $ \slot -> case fmap snd picked of
          [] -> pure ()
          [only] -> State.modify' (bindSlot resolving slot only)
          several -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList several))
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
          -- Through Event.resolveLifeGain, CR 614.1's funnel for the gain class,
          -- LoseLife's road above one direction over: a LifeGainR row resizes the
          -- gain and the SETTLED amount is what moves the total.
          | n > 0 -> do
              settled <- Event.resolveLifeGain pid (Integer.toNaturalSaturating n)
              Event.changeLife pid (toInteger settled)
        _ -> pure ()
  -- CR 701.12c: both sides reach each other's PREVIOUS total, so both deltas are
  -- read off the same game state before either is written. Written as a gain and
  -- a loss rather than two assignments, which is what puts a LifeGained and a
  -- LifeLost in the log.
  --
  -- BOTH sides go through changeLifeByDelta, so each is proposed -- the lowered
  -- one as a life loss, the raised one as a life gain -- and CR 701.12c's
  -- "replacement effects may modify these gains and losses" is reachable;
  -- ReplacementSpec's Bloodletter group proves the loss half.
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
  -- life loss and an UPWARD one as a life gain, since rule 119.5 spells a lower
  -- total as the player losing "the necessary amount of life" and a higher one as
  -- their gaining it.
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
  -- Every total goes through changeLifeByDelta, the ExchangeLifeTotals arm's
  -- road: rule 119.5's loss or gain, proposed so a replacement reaches it (CR
  -- 614.1).
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
          -- included, its offer below being a separate act), CR 707.10d's one
          -- map per candidate, in the order its controller chose, and CR
          -- 707.10e's single map for the target the effect states. The list's
          -- LENGTH is how both of the latter two say "a copy isn't created".
          plan <- case targets of
            CopyTargets.Copied -> pure [Map.empty]
            CopyTargets.ChosenByController -> pure [Map.empty]
            CopyTargets.ForEach candidateRef -> copyForEachTargets controller resolving source legal original candidateRef
            CopyTargets.Stated newRef -> copyStatedTargets controller resolving source legal original newRef
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
                      -- CR 707.10d's and CR 707.10e's targets, where the effect
                      -- chose them, over the decisions CR 707.10 copied. Empty
                      -- for the other two answers, which leave every one of them
                      -- standing.
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
        --
        -- A shield naming NO recipient side at all -- Pay No Heed's "prevent all
        -- damage a source of your choice would deal this turn" -- gets that same
        -- lone row, whose Nothing recipient is DamagePattern's "every recipient".
        -- The empty `ref` is what tells it apart from CR 608.2b's gone target,
        -- which is a card that DID name a recipient and lost it: that one keeps
        -- installing nothing.
        rows = if Maybe.isJust whatRecipient || Maybe.isNothing ref then [Nothing] else fmap Just recipients
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
        -- No row is CR 608.2b's gone target -- a named recipient that left -- so
        -- there is nothing to shield and CR 609.7a's choice, a choice existing
        -- only to be baked into a row, is not raised either. PreventNextDamage's
        -- posture. A DESCRIBED shield always has its one row, its recipients
        -- being read at the damage event rather than settled here, and so does a
        -- shield naming no recipient at all (`rows` above).
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
            -- CR 601.2c / 608.2b: a zone reference inside the payload is a read of
            -- this resolution's bindings, so it is baked here for the reason the
            -- Named set above is -- Sen Triplets' "that player's hand" is the
            -- targeted opponent, and the slot is gone once this resolution is.
            -- Pawl.Engine.Condition.bakeBound is the precedent, and its posture
            -- for a slot naming nobody: the reference is left standing and reads
            -- as naming nobody, rather than falling back to some other seat.
            bakedEffect = PlayerEffect.mapPlayerRefs (Quantity.bakePlayerRef (Binding.playersIn legal)) playerEffect
            install g scope =
              let (ts, g1) = Game.freshTimestamp g
                  active =
                    ActivePlayerEffect.MkActivePlayerEffect
                      { ActivePlayerEffect.source = source,
                        ActivePlayerEffect.controller = controller,
                        ActivePlayerEffect.timestamp = ts,
                        ActivePlayerEffect.expiry = expiry,
                        ActivePlayerEffect.scope = scope,
                        ActivePlayerEffect.effect = bakedEffect
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
  Effect.ForbidActivation (ForbidActivation.MkForbidActivation duration ref) ->
    -- CR 602.2 / 611.1: store one prohibition per permanent the ref names.
    -- ForbidBlock above is the model and every one of its arguments carries over:
    -- the ref is enumerated ONCE, for the CR 608.2f simultaneity objectRefObjects
    -- buys, and an illegal slot (CR 608.2b) stores nothing, which is Deadlock
    -- Trap's fizzle.
    --
    -- Nothing is written onto the permanent itself, and nothing is projected: CR
    -- 613.11 keeps a prohibition on an activation out of the layers, so the row
    -- is read at Pawl.Engine.ActivationProhibition.cantActivate and never by a
    -- projection.
    State.modify' $ \gs -> case Expiry.arm (Binding.playersIn legal) controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let objects = objectRefObjects legal resolving controller source gs ref
            (ts, gs1) = Game.freshTimestamp gs
            stored =
              [ ActiveActivationProhibition.MkActiveActivationProhibition
                  { ActiveActivationProhibition.source = source,
                    ActiveActivationProhibition.timestamp = ts,
                    ActiveActivationProhibition.expiry = expiry,
                    ActiveActivationProhibition.object = object
                  }
              | object <- objects
              ]
         in gs1 {GameState.activationProhibitions = stored <> GameState.activationProhibitions gs1}
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
      -- and Pawl.Engine.Event.Trigger.batchScoped is that fork.
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
  -- The answer is not filtered HERE, the posture the entry twin already takes:
  -- the engine holds no Oracle card reference, so it cannot resolve a name.
  -- Pawl.Interpreter.policingCardNames judges it on the far side of
  -- Pawl.Engine.Engine.runGameAsked, where the registry is, and covers this arm
  -- and the entry twin alike by covering the one Prompt they share.
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
  -- Pawl.Engine.Event.bringInto is the whole of it, and this arm asks nothing about
  -- which effect the filter came from.
  Effect.FromOutsideTheGame payload -> Event.bringInto payload source controller
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
  -- CR 701.68d's GameEvent.Blighted is written per blighter inside
  -- Pawl.Engine.Blight.blight, one per seat that actually blighted -- the
  -- bracket groups them, which changes nothing here, PlayerBlights not being a
  -- CR 603.2c batch condition (Pawl.Engine.Event.Trigger.batchScoped).
  --
  -- Not implemented: nothing records which creature was blighted, so CR 701.68c's
  -- "blighted creature" has nothing to read (#1492).
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
  -- The offer is skipped at a bound of 0, where CR 118.3 leaves 0 as the only
  -- payable amount and there is nothing to decide. Proven by
  -- Pawl.CounterKeywordTriggerSpec's "a payer with no {E} is not asked how much
  -- to pay", whose twin one {E} higher is asked.
  Effect.PayAnyEnergy slot -> do
    gs <- State.get
    let have = Cost.energyOf controller gs
    paid <-
      if have == 0
        then pure 0
        else do
          answer <- Game.choose (Prompt.ChoosePaidEnergy (Decide.deciderFor controller gs) controller resolving have)
          pure (min answer have)
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

-- The two halves Pawl.Engine.Cost reaches through the
-- Pawl.Types.ManaAbilityPerformer parameter: CR 405.6c's other effects of the
-- activated mana ability being paid, and CR 605.4a's triggered mana ability.
--
-- Both stand on the noSubgame floor, performHandAction's reason: no mana ability
-- starts a subgame (#1900).
performManaAbility :: ManaAbilityPerformer.ManaAbilityPerformer
performManaAbility =
  ManaAbilityPerformer.MkManaAbilityPerformer
    { ManaAbilityPerformer.effects = performManaAbilityEffects,
      ManaAbilityPerformer.triggered = performTriggeredManaAbility
    }

-- CR 605.4a: apply one triggered mana ability where it stands. CR 605.1b's
-- classification and CR 603.4's intervening "if" are already spent by
-- Pawl.Engine.Cost.applyManaTriggers, which is the only caller; this is the
-- resolution half, and it lives here because Pawl.Engine.Cost cannot reach
-- applyEffect.
--
-- No stack object, exactly as for the activated mana ability above, so the
-- SOURCE stands in for the resolving one and the slots are stamped rather than
-- read off an object. CR 605.1b leaves no targets to bind, so what the event
-- bound (Pawl.Engine.Event.Binding.eventBindings) is the whole environment --
-- Binding.manaSource, which is how Wild Growth's "its controller" names the
-- land rather than the Aura.
--
-- A SOURCELESS pending trigger cannot arrive: no inherent ability the rulebook
-- states adds mana, and Pawl.Engine.Cost gathers only from an object's
-- conditions. Answered by doing nothing rather than by a partial pattern.
--
-- CR 700.2b's mode choice is FORCED or nothing: a modal triggered mana ability
-- would want the prompt Engine.placeBorne raises, and CR 605.4a leaves no stack
-- object to raise it against. Not implemented: such an ability; no card prints
-- one (#1572).
performTriggeredManaAbility :: PendingTrigger.PendingTrigger -> Game ()
performTriggeredManaAbility pending = case PendingTrigger.source pending of
  TriggerSource.Sourceless -> pure ()
  TriggerSource.OfObject source -> do
    let controller = PendingTrigger.controller pending
        modal = TriggeredAbility.modal (PendingTrigger.ability pending)
        -- Every PRINTED mode is fillable: CR 605.1b's no-target clause leaves
        -- nothing for Target.fillableModes to reject, which is the gate
        -- Engine.placeBorne applies instead. Zipped against the modes themselves
        -- rather than counted through Modal.modeCount, whose Natural predecessor
        -- would underflow on a modeless ability.
        every = Set.fromList (fmap fst (zip (fmap ModeIndex.MkModeIndex [0 ..]) (Modal.modeEffects modal)))
        bound =
          Map.union
            (Binding.targetsOf (PendingTrigger.bindings pending))
            ( Map.fromList
                [ (Binding.triggerSource, Set.singleton (Recipient.ToObject source)),
                  (Binding.you, Set.singleton (Recipient.ToPlayer controller))
                ]
            )
    case Modal.forcedSelection every (Modal.Type.selection modal) of
      Nothing -> pure ()
      Just selection -> Monad.mapM_ (applyEffect source source controller bound bound) (Modal.modesEffects selection modal)

-- CR 405.6c: run the non-mana effects of a mana ability, once
-- Pawl.Engine.Cost.tapForManaWith has added the mana.
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
performManaAbilityEffects :: ObjectId -> PlayerId -> [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Game ()
performManaAbilityEffects source controller =
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
-- than the total the effect named. An upward delta is a life GAIN and goes
-- through Event.resolveLifeGain, the same funnel one direction over -- CR 119.5
-- spelling a higher total as the player gaining "the necessary amount of life",
-- and CR 701.12c saying out loud that "replacement effects may modify these gains
-- and losses".
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
    else do
      settled <- Event.resolveLifeGain pid (Integer.toNaturalSaturating delta)
      Event.changeLife pid (toInteger settled)

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
