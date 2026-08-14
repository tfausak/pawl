{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Resolve and Pawl.Engine.Target: targeting legality, spell resolution, and
-- the CR 608.2b fizzle.
module Pawl.ResolveSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

targetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
targetSpec s registry = Spec.describe s "Target" $ do
  Spec.it s "CR 115.4 AnyTarget offers every creature and every playing player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "creature and both players"
      (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.AnyTarget Nothing) gs)
      (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])
  Spec.it s "a departed player is not a legal target" $ do
    let gs = Departure.depart Departure.Type.Lost S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertBool
      s
      (not (Set.member (Recipient.ToPlayer S.bob) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.AnyTarget Nothing) gs)))
      "bob gone"
  Spec.it s "CR 800.4b an object does not change to the control of a player who has left the game" $ do
    -- CR 800.4b: "If an object would change to the control of a player who has
    -- left the game, it doesn't." Resolve.applyEffect takes the controller
    -- explicitly, which is what makes this testable: the effect is asked to
    -- resolve on behalf of a player who is no longer in the game.
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myr, board) = S.addCreature darksteelMyr S.carol S.threePlayerGame
        gone = Departure.depart Departure.Type.Conceded S.bob board
        slot = SlotName.MkSlotName (Text.pack "target")
        after =
          S.runPure S.identityAnswer gone $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.bob
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.InSlot slot)))
        control =
          S.runPure S.identityAnswer board $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.bob
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Map.singleton slot (Set.singleton (Recipient.ToObject myr)))
              (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.InSlot slot)))
    Spec.assertEqWith s "no control effect is stored for a departed controller" (GameState.continuousEffects after) []
    Spec.assertEqWith s "and the Myr's controller is unchanged" (Projection.controllerOf myr after) (Just S.carol)
    Spec.assertEqWith s "the same call for a player still in the game DOES store one -- the guard is what did it" (length (GameState.continuousEffects control)) 1
    Spec.assertEqWith s "and takes control" (Projection.controllerOf myr control) (Just S.bob)
  Spec.it s "CR 608.2b a creature that left its zone is no longer legal" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.AnyTarget Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.AnyTarget Nothing) gone)) "illegal once moved"
  Spec.it s "legalSets maps each slot to its legal recipients" $ do
    let slots = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.AnyTarget Nothing)
        gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith
      s
      "one slot, two players"
      (Target.legalSets Nothing S.noSource slots gs)
      (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
  Spec.it s "CR 115.4 CreatureTarget offers creatures but no players" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "just the creature"
      (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Creatures Nothing) gs)
      (Set.singleton (Recipient.ToCreature oid))
  Spec.it s "CR 601.2c CreatureTarget has an empty legal set with no creatures" $ do
    Spec.assertBool
      s
      (Set.null (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Creatures Nothing) (Setup.emptyGame S.bothPlayers)))
      "nothing to target"
  Spec.it s "CR 608.2b a creature that left is no longer a legal CreatureTarget" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.Creatures Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.Creatures Nothing) gone)) "illegal once moved"
  Spec.it s "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (permId, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertBool
      s
      (Set.member (Recipient.ToObject permId) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.SpellsAndPermanents Nothing) gs))
      "the permanent is a legal object target"
  Spec.it s "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (permId, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (spellId, gs) = S.spellOnStack lightningBolt S.alice base
        legal = Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Spells Nothing) gs
    Spec.assertBool s (Set.member (Recipient.ToObject spellId) legal) "the stack spell is a legal target"
    Spec.assertBool s (not (Set.member (Recipient.ToObject permId) legal)) "the battlefield permanent is not a legal target"
  Spec.it s "LandTarget offers a land as an object target, not a creature or player" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
    Spec.assertBool s (Set.member (Recipient.ToObject landId) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs)) "the land is legal"
    Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs))) "no players"
  Spec.it s "CR 115: PlayerTarget is exactly the players still in the game" $ do
    let gs = Setup.emptyGame S.bothPlayers
        expected = Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]
    Spec.assertEqWith s "both players, no creatures" (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Players Nothing) gs) expected
  -- CR 115.1a / 700.2c: "target Wall" (Chaos Charm) restricts CreatureTarget to
  -- creatures whose PROJECTED subtypes include Wall. Wall of Stone (a real 0/8
  -- Creature - Wall, M4g) is the Wall; a Piker is the non-Wall control.
  Spec.it s "CR 115.1a / 700.2c \"target Wall\" offers a Wall creature but not a non-Wall creature" $ do
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    piker <- S.printingOf s registry "Goblin Piker"
    let (wallId, base) = S.addCreature wallOfStone S.bob (Setup.emptyGame S.bothPlayers)
        (pikerId, gs) = S.addCreature piker S.alice base
        slot = SlotName.MkSlotName (Text.pack "target")
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing S.noSource (Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
    Spec.assertBool s (Set.member (Recipient.ToCreature wallId) legal) "the Wall is legal"
    Spec.assertBool s (not (Set.member (Recipient.ToCreature pikerId) legal)) "the non-Wall creature is not legal"
  -- The same "target Wall", against a Wall that Ashaya animated into a land
  -- and Blood Moon then set to Mountain. CR 305.7 retires the land's OLD LAND
  -- TYPES and nothing else on the subtype axis, and its fourth sentence keeps
  -- the card types -- so the Wall is still a creature, still a Wall, and still
  -- a legal target. This is the gameplay-level half of Pawl.ProjectionSpec's
  -- "a Blood Moon'd creature-land keeps its creature types".
  Spec.it s "CR 305.7 an Ashaya-animated, Blood Moon'd Wall of Stone is still a legal \"target Wall\"" $ do
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (wallId, g1) = S.addCreature wallOfStone S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addCreature ashaya S.alice g1
        (_, gs) = S.addCreature bloodMoon S.alice g2
        slot = SlotName.MkSlotName (Text.pack "target")
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing S.noSource (Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
    Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf wallId gs)) "it really is a Mountain"
    Spec.assertBool s (Projection.isCreatureOf wallId gs) "and still a creature (CR 305.7 removes no card types)"
    Spec.assertBool s (Set.member (Recipient.ToCreature wallId) legal) "so \"target Wall\" still offers it"
  Spec.it s "CR 115.1a ArtifactTarget is the battlefield's projected artifacts" $ do
    -- boardWithCreatureArtifactLand: alice has a Piker, a Mindslaver
    -- (Legendary Artifact) and a Mountain.
    piker <- S.printingOf s registry "Goblin Piker"
    mindslaver <- S.printingOf s registry "Mindslaver"
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
        legal = Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))) gs
    Spec.assertEqWith s "exactly the artifact" legal (Set.singleton (Recipient.ToObject (S.artifactId gs)))
    Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) legal)) "no players"
  Spec.it s "CR 115.1a / 109.5 OpponentCreatureTarget excludes the source's controller's creatures" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (mine, gs1) = S.addCreature piker S.alice gs0
        (theirs, gs2) = S.addCreature warMammoth S.bob gs1
        legal = Target.legalRecipients (Just S.alice) mine (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs2
    Spec.assertEqWith s "only the opponent's creature" legal (Set.singleton (Recipient.ToCreature theirs))
    Spec.assertBool s (not (Set.member (Recipient.ToCreature mine) legal)) "not the source's controller's own"
  -- CR 115.1 / 109.5: "target OPPONENT". Until Ravenous Rats there was no
  -- card in the pool that narrowed a PLAYER target, so Target.legalRecipients
  -- kept every player unconditionally (#168). Three seats, so "an opponent"
  -- is a real set rather than the only other player.
  Spec.it s "CR 115.1 a Players pool narrowed by IsPlayer Opponent excludes the source's controller" $ do
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let (src, gs) = S.addCreature ravenousRats S.alice (Setup.emptyGame S.threePlayers)
        theSlot = TargetSlot.required Pool.Players (Just (Filter.Type.IsPlayer PlayerRelation.Opponent))
        legal = Target.legalRecipients (Just S.alice) src theSlot gs
    Spec.assertEqWith
      s
      "exactly bob and carol, never alice"
      legal
      (Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol])
  -- The card itself, so the narrowing is proven through the real target slot
  -- the JSON carries rather than one hand-built in the test.
  Spec.it s "CR 115.1 Ravenous Rats' entry trigger may only target an opponent" $ do
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let (src, gs) = S.addCreature ravenousRats S.bob (Setup.emptyGame S.threePlayers)
        -- The slot lives on the ENTRY TRIGGER, not the spell, so
        -- Card.allTargetSlots (which covers the spell and the enchant slot)
        -- is the wrong door -- read the ability the card actually prints.
        slots = fmap (Modal.allTargetSlots . TriggeredAbility.modal) (Face.triggeredAbilities (S.combinedFace ravenousRats))
    case concatMap Map.elems slots of
      [theSlot] ->
        Spec.assertEqWith
          s
          "bob is excluded from his own Rats' trigger"
          (Target.legalRecipients (Just S.bob) src theSlot gs)
          (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.carol])
      _ -> Spec.assertFailure s "Ravenous Rats should declare exactly one target slot"
  -- The gameplay-level proof design.md section 4 asks for: an opcode is not
  -- done until a card exercises it end to end. Ravenous Rats enters, its
  -- trigger is placed and targeted from the narrowed set, and an OPPONENT
  -- loses a card from hand -- not alice, who cast it.
  Spec.it s "CR 115.1 Ravenous Rats' entry trigger makes an opponent discard, never its own controller" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let base0 = S.landsInPlay swamp 2
        -- Both players hold a card, so "whose hand shrank" is a real question.
        (_, base1) = S.addHandCard piker S.bob base0
        (gs, spellId) = S.handOne ravenousRats base1
        aliceBefore = S.handSize S.alice gs
        bobBefore = S.handSize S.bob gs
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
    Spec.assertEqWith s "bob discarded one" (S.handSize S.bob settled) (bobBefore - 1)
    Spec.assertEqWith s "alice lost only the Rats she cast" (S.handSize S.alice settled) (aliceBefore - 1)
    Spec.assertEqWith s "the Rats resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ravenous Rats") S.alice settled) 1
  Spec.it s "CR 806.1 at three seats a ControlledBy Opponent pool spans BOTH opponents' creatures" $ do
    -- Palace Jailer's second trigger targets a creature an opponent controls.
    -- At three seats that is a choice across two boards, and the engine must
    -- offer all of it. DISCRIMINATING: a relation resolved as "the next seat"
    -- offers only bob's, and carol is deliberately the far seat -- so an
    -- implementation that took one opponent fails on the set equality, not on
    -- a membership check that a superset would also satisfy.
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let gs0 = Setup.emptyGame S.threePlayers
        (mine, gs1) = S.addCreature piker S.alice gs0
        (bobs, gs2) = S.addCreature warMammoth S.bob gs1
        (carols, gs3) = S.addCreature piker S.carol gs2
        legal = Target.legalRecipients (Just S.alice) mine (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs3
    Spec.assertEqWith
      s
      "exactly bob's and carol's, and nothing of alice's"
      legal
      (Set.fromList [Recipient.ToCreature bobs, Recipient.ToCreature carols])
  Spec.it s "CR 613.1b OpponentCreatureTarget follows PROJECTED control, not ownership" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (mine, gs1) = S.addCreature piker S.alice gs0
        (theirs, gs2) = S.addCreature warMammoth S.bob gs1
        (alsoTheirs, gs3) = S.addCreature typhoidRats S.bob gs2
        -- alice steals one of bob's creatures: it stops being "a creature an
        -- opponent controls" for alice's source, and becomes one for bob's.
        stolen = S.giveControl theirs S.alice gs3
    Spec.assertEqWith
      s
      "for alice's source, only the creature still under bob's control"
      (Target.legalRecipients (Projection.controllerOf mine stolen) mine (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
      (Set.singleton (Recipient.ToCreature alsoTheirs))
    Spec.assertEqWith
      s
      "for bob's source, the two alice now controls"
      (Target.legalRecipients (Projection.controllerOf alsoTheirs stolen) alsoTheirs (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
      (Set.fromList [Recipient.ToCreature mine, Recipient.ToCreature theirs])
  -- P9 (#40): the reshaped TargetSlot = Pool + Maybe Filter reproduces the
  -- retired hand-carved constructors as data. A black creature
  -- (Typhoid Rats, {B}) and a nonblack one (Goblin Piker, {1}{R}) exercise
  -- the Not (HasColor Black) filter that WAS NonblackCreatureTarget.
  Spec.it s "P9 Creatures + Not (HasColor Black) excludes a black creature" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
        (plainOid, gs) = S.addCreature piker S.alice gs1
        theSlot = TargetSlot.required Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))
        legal = Target.legalRecipients Nothing S.noSource theSlot gs
    Spec.assertBool s (not (Set.member (Recipient.ToCreature blackOid) legal)) "black creature illegal"
    Spec.assertBool s (Set.member (Recipient.ToCreature plainOid) legal) "nonblack creature legal"
  Spec.it s "P9 Creatures + Nothing narrows nothing" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
        (plainOid, gs) = S.addCreature piker S.alice gs1
        theSlot = TargetSlot.required Pool.Creatures Nothing
        expectedAllCreatures = Set.fromList [Recipient.ToCreature blackOid, Recipient.ToCreature plainOid]
    Spec.assertEqWith s "all creatures legal" (Target.legalRecipients Nothing S.noSource theSlot gs) expectedAllCreatures
  -- CR 601.2c "another" over a Creatures pool (#163). The pool tags its
  -- candidates ToCreature (CR 115.1a); a Not IsSource conjunct drops the
  -- source whatever tag the pool gave it, which the retired Exclusion field
  -- did not -- it deleted a ToObject recipient a Creatures pool never emits,
  -- so "another target creature" left the source legal.
  Spec.it s "another target creature excludes the source (CR 601.2c)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs1) = S.addCreature piker S.alice gs0
        (otherId, gs) = S.addCreature piker S.alice gs1
        slot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.Not Filter.Type.IsSource)))
    Spec.assertEqWith
      s
      "source excluded from its own set"
      (Target.legalSets Nothing srcId slots gs)
      (Map.singleton slot (Set.singleton (Recipient.ToCreature otherId)))
  -- The other half of the same claim: a slot carrying no Not IsSource does
  -- not exclude, so Prodigal Sorcerer may still ping itself (CR 115.4).
  Spec.it s "a slot without Not IsSource still admits the source (CR 115.4)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs) = S.addCreature piker S.alice gs0
        slot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton slot (TargetSlot.required Pool.Creatures Nothing)
    Spec.assertEqWith
      s
      "source is its own legal target"
      (Target.legalSets Nothing srcId slots gs)
      (Map.singleton slot (Set.singleton (Recipient.ToCreature srcId)))
  -- Gate cards for P9 Task 5: Terror and Reprisal. Both cards' printed text
  -- ends "It can't be regenerated."; regeneration is not modelled (no
  -- regeneration shield to suppress), so that clause is a no-op and is
  -- omitted from data/cards/{terror,reprisal}.json -- regeneration clause
  -- omitted; not modelled (#113).
  Spec.it s "Terror: And of Not(HasColor Black) and Not(HasCardType Artifact) excludes black and artifact creatures" $ do
    terror <- S.printingOf s registry "Terror"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot terror of
      Nothing -> Spec.assertFailure s "Terror's printing carries no 'target' slot"
      Just theSlot -> do
        let gs0 = Setup.emptyGame S.bothPlayers
            (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
            (artifactOid, gs2) = S.addCreature darksteelMyr S.bob gs1
            (plainOid, gs) = S.addCreature piker S.alice gs2
            legal = Target.legalRecipients Nothing S.noSource theSlot gs
        Spec.assertBool s (not (Set.member (Recipient.ToCreature blackOid) legal)) "black creature illegal"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature artifactOid) legal)) "artifact creature illegal"
        Spec.assertBool s (Set.member (Recipient.ToCreature plainOid) legal) "nonblack, nonartifact creature legal"
  Spec.it s "Reprisal: PowerAtLeast 4 legality tracks a projected power pump" $ do
    reprisal <- S.printingOf s registry "Reprisal"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot reprisal of
      Nothing -> Spec.assertFailure s "Reprisal's printing carries no 'target' slot"
      Just theSlot -> do
        let gs0 = Setup.emptyGame S.bothPlayers
            (smallOid, gs) = S.addCreature piker S.bob gs0 -- power 2, {1}{R}
            legalBefore = Target.legalRecipients Nothing S.noSource theSlot gs
            pumped = S.withEffect smallOid (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0))) gs
            legalAfter = Target.legalRecipients Nothing S.noSource theSlot pumped
        Spec.assertBool s (not (Set.member (Recipient.ToCreature smallOid) legalBefore)) "power 2 is illegal (below the PowerAtLeast 4 floor)"
        Spec.assertBool s (Set.member (Recipient.ToCreature smallOid) legalAfter) "pumped to power 4 becomes legal"
  -- CR 508.1k: Kill Shot's IsAttacking narrowing, read off the committed card
  -- data. The defender is a creature in every other respect, so only combat
  -- status can be what separates the two.
  Spec.it s "Kill Shot: IsAttacking admits the attacker and rejects the untapped defender" $ do
    killShot <- S.printingOf s registry "Kill Shot"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot killShot of
      Nothing -> Spec.assertFailure s "Kill Shot's printing carries no 'target' slot"
      Just theSlot -> do
        let (board, mine, theirs) = S.combatBoardOf [piker] [piker]
            declared = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
            legal = Target.legalRecipients Nothing S.noSource theSlot declared
        case (mine, theirs) of
          (attacker : _, defender : _) -> do
            Spec.assertBool s (Map.member attacker (Combat.Type.attackers (GameState.combat declared))) "the fixture really did attack"
            Spec.assertBool s (Set.member (Recipient.ToCreature attacker) legal) "the attacker is legal"
            Spec.assertBool s (not (Set.member (Recipient.ToCreature defender) legal)) "the creature that stayed home is not"
          _ -> Spec.assertFailure s "fixture should have one creature a side"

resolveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
resolveSpec s registry = Spec.describe s "Resolve" $ do
  Spec.it s "CR 608 a resolved spell's damage is Noncombat" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (_target, gs0) = S.addCreature piker S.bob base
        (gs1, spellId) = S.handOne lightningBolt gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice spellId))
        -- resolveTop applies the damage but does NOT run SBAs, so the event persists.
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "the Bolt's damage event is Noncombat"
      (fmap DamageEvent.kind (S.damageEventsOf resolved))
      [DamageKind.Noncombat]
  Spec.it s "CR 608.3 / 704.5g a resolved Bolt kills a Piker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
    Spec.assertEqWith s "no creature survives" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Piker in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
  Spec.it s "CR 608.2n the resolved Bolt is in its owner's graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one card" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 120.3a a Bolt at a player drains life without marking" $ do
    -- No creature on the battlefield, so identityAnswer's lookupMin picks
    -- ToPlayer alice: a self-Bolt, which is legal Magic.
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "seventeen" (S.lifeOf S.alice after) (Just 17)
  Spec.it s "the resolved damage flows through the event funnel" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one event of amount 3" (fmap DamageEvent.amount (S.damageEventsOf after)) [3]
  Spec.it s "resolving a Bolt conserves objects" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
    Spec.assertEqWith s "conserved" (Game.objectCount (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))) (Game.objectCount cast)
  Spec.it s "CR 608.2b a Bolt whose only target died fizzles" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (base, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        -- Kill the Piker while the Bolt is on the stack, as Bolt B will in
        -- the integration test, then check state-based actions.
        dead = S.settleSba (S.markDamage (S.pikerOf base) 3 cast)
        after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
    Spec.assertEqWith s "Bolt in the graveyard, unresolved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "no damage was dealt" (S.damageEventsOf after) []
    Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
  Spec.it s "CR 608.2b a fizzled spell applies none of its effects" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (base, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        dead = S.settleSba (S.markDamage (S.pikerOf base) 3 cast)
        after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
    Spec.assertEqWith s "life totals unchanged" (S.lifeOf S.alice after) (Just 20)
  -- The deterministic successor to the retired "instants happen" property: a
  -- Bolt cast in a game and resolved ends in its owner's graveyard.
  Spec.it s "a cast Bolt reaches its owner's graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one card in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 612 slotsOf finds a ChangeText slot" $ do
    let slot = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "slotsOf" (Resolve.slotsOf (Effect.ChangeText (ChangeText.MkChangeText SubtypeFamily.CreatureType (Set.singleton Subtype.Wall) slot))) (Map.singleton slot SlotArity.One)
  Spec.it s "CR 605 manaProduced reads AddMana, nothing else" $ do
    Spec.assertEqWith s "add mana" (ManaAbility.manaProduced (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green)))) (Just (ManaProduction.OfType (ManaType.Colored Color.Green)))
    Spec.assertEqWith s "add mana of any color" (ManaAbility.manaProduced (Effect.AddMana ManaProduction.AnyColor)) (Just ManaProduction.AnyColor)
    Spec.assertEqWith s "damage produces no mana" (ManaAbility.manaProduced (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "x"))) (Quantity.Literal 1)))) Nothing
  Spec.it s "CR 612.1 a text change reaches a Filter carried by an effect" $ do
    -- Boil ("Destroy all Islands") is the first card whose effect selects by
    -- a BASIC LAND TYPE, so it is the first that can tell whether CR 612.1's
    -- "any words or symbols printed on that object" reaches inside an
    -- effect's Filter. The stored ChangeSubtypeWord is what a resolved
    -- Magical Hack leaves on the spell.
    --
    -- The Filter half of read-point 3 (Resolve.modesOf) rests on this case
    -- alone: no real instant or sorcery SETS a land's subtype. The Modification
    -- half of the same read-point is Turn to Frog's SetCreatureSubtype under an
    -- Artificial Evolution (the ArtificialEvolution group below), and
    -- Pawl.ActivateSpec's Tidal Warrior chain reaches the same
    -- Projection.rewriteEffect ModifyTarget arm through an ACTIVATED ability.
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    boil <- S.printingOf s registry "Boil"
    let base = Setup.emptyGame S.bothPlayers
        (islandId, g1) = S.addCreature island S.alice base
        (forestId, g2) = S.addCreature forest S.alice g1
        (boilId, g3) = Game.freshObjectId g2
        boilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfCard boil,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              -- CR 700.2: Boil has one mode, and a directly-built stack object
              -- (bypassing Cast.castSpell) must stamp it chosen (mode 0), or
              -- Resolve.effectsOf/resolveSpell -- scoped to CHOSEN modes --
              -- would see no effects and no target slots at all.
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g4 =
          g3
            { GameState.objects = Map.insert boilId boilObj (GameState.objects g3),
              GameState.stack = boilId : GameState.stack g3
            }
        resolve g = snd (Engine.runGamePure S.identityAnswer g (Resolve.resolveSpell boilId))
        onBattlefield oid g = Set.member oid (GameState.battlefield g)
        plain = resolve g4
        hacked = resolve (S.withEffectAt boilId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Island Subtype.Forest)) g4)
    -- The control: unhacked, Boil does what it prints.
    Spec.assertBool s (not (onBattlefield islandId plain)) "unhacked, the Island dies"
    Spec.assertBool s (onBattlefield forestId plain) "unhacked, the Forest lives"
    -- And hacked, the word swap moves which lands the filter admits.
    Spec.assertBool s (not (onBattlefield forestId hacked)) "hacked, the Forest dies"
    Spec.assertBool s (onBattlefield islandId hacked) "hacked, the Island lives"
  Spec.it s "CR 400.7a hacking Blood Moon on the stack carries onto the permanent" $ do
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (nonbasicId, g1) = S.addCreature urborg S.alice base
        (bloodMoonSpellId, g2) = Game.freshObjectId g1
        bmObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfCard bloodMoon,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Map.empty,
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g3 =
          g2
            { GameState.objects = Map.insert bloodMoonSpellId bmObj (GameState.objects g2),
              GameState.stack = bloodMoonSpellId : GameState.stack g2
            }
        hacked = S.withEffectAt bloodMoonSpellId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Mountain Subtype.Island)) g3
        after = snd (Engine.runGamePure S.identityAnswer hacked Stack.resolveTop)
    -- CR 400.7 mints a NEW object for the permanent, but CR 400.7a is the
    -- exception: an effect that changes a PERMANENT SPELL's characteristics
    -- keeps applying to the permanent that spell becomes, and rules text is a
    -- characteristic (CR 109.3). So the hacked Blood Moon reads "Nonbasic lands
    -- are Islands" on the battlefield, and Urborg -- a nonbasic land -- is an
    -- Island. Stack.carryOver is what re-keys the stored effect.
    Spec.assertEqWith s "hack carried over: nonbasic land is Island" (Projection.subtypesOf nonbasicId after) (Set.singleton Subtype.Island)
  Spec.it s "CR 608.2n a resolving ability deals its damage and ceases" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        ability = case Face.activatedAbilities (S.combinedFace prodigalSorcerer) of
          ab : _ -> ab
          [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Nothing
        (abilId, g1) = Game.freshObjectId g0
        (ts, g2) = Game.freshTimestamp g1
        slot = SlotName.MkSlotName (Text.pack "target")
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfAbility srcId ability,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.bob))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g3 =
          g2
            { GameState.objects = Map.insert abilId abilObj (GameState.objects g2),
              GameState.stack = abilId : GameState.stack g2
            }
        resolved = snd (Engine.runGamePure S.identityAnswer g3 Stack.resolveTop)
    Spec.assertEqWith s "bob took 1" (S.lifeOf S.bob resolved) (Just 19)
    Spec.assertEqWith s "ability object gone" (Game.lookupObject abilId resolved) Nothing
    Spec.assertEqWith s "stack empty" (GameState.stack resolved) []
  Spec.it s "CR 701.23 Search fetches a basic land to the battlefield tapped" $ do
    -- The fetched card gets a NEW object id (CR 400.7 changeZone), so assert by
    -- count/tapped-count, never by the library incarnation's id.
    mountain <- S.printingOf s registry "Mountain"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addLibraryCard mountain S.alice base
        ability =
          ActivatedAbility.MkActivatedAbility
            (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Quantity.Literal 1, Search.filter = basicLandFilter, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing Set.empty ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
    Spec.assertEqWith s "one permanent on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice resolved)) 1
    Spec.assertEqWith s "it is tapped" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice resolved) []
  Spec.it s "CR 701.23b Search may fail to find" $ do
    mountain <- S.printingOf s registry "Mountain"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addLibraryCard mountain S.alice base
        ability = ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Quantity.Literal 1, Search.filter = basicLandFilter, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1)) [] Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj = Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing Set.empty ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findNothing g4 Stack.resolveTop)
    Spec.assertEqWith s "nothing entered the battlefield" (GameState.battlefield resolved) Set.empty
  Spec.it s "CR 701.23a Search (And [HasCardType Land, HasSupertype Basic]) offers a basic land, not a nonland" $ do
    -- P9: the Search filter reads each library card through the PRINTED-card
    -- view (Projection.viewOfCardIn) -- a card in a library has no projection.
    -- With a Mountain (basic land) and a Piker (creature) both in the library,
    -- only the Mountain is a candidate: findFirst fetches it while the Piker
    -- stays put. The Piker is added SECOND, so it is the head of the library
    -- (Support.addLibraryCard prepends); a filter that matched everything would
    -- fetch the Piker and this test would fail.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, g0) = S.addLibraryCard mountain S.alice base
        (pikerId, g1) = S.addLibraryCard piker S.alice g0
        ability =
          ActivatedAbility.MkActivatedAbility
            (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Quantity.Literal 1, Search.filter = basicLandFilter, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing Set.empty ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
    Spec.assertEqWith s "the basic land is offered and fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Mountain") S.alice resolved) 1
    Spec.assertBool s (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)) "the nonland is not offered -- it remains in the library"
  -- #222: CR 701.23a's filter defines what the search may find. An
  -- interpreter that names a card the filter excluded must find nothing --
  -- "fails to find" is already a legal outcome, so rejecting needs no new
  -- branch. Same fixture as the test above, so the only variable is the answer.
  Spec.it s "#222 a search that names a card the filter excluded fetches nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, g0) = S.addLibraryCard mountain S.alice base
        (pikerId, g1) = S.addLibraryCard piker S.alice g0
        ability =
          ActivatedAbility.MkActivatedAbility
            (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Quantity.Literal 1, Search.filter = basicLandFilter, Search.destination = SearchDestination.BattlefieldTapped}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing Set.empty ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure (findForbidden pikerId) g4 Stack.resolveTop)
    Spec.assertEqWith s "the Piker was NOT fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice resolved) 0
    Spec.assertBool s (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)) "it is still in the library"
    Spec.assertEqWith s "and nothing else was fetched either" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Mountain") S.alice resolved) 0
  -- Hoarding Dragon -- "Flying. When this creature enters, you may search your
  -- library for an artifact card, exile it, then shuffle." The whole-card proof
  -- of SearchDestination.Exile, cast and resolved rather than assembled.
  --
  -- The printed card also has "When this creature dies, you may put the exiled
  -- card into its owner's hand". CR 607.2a links that ability to the first one,
  -- and pawl records nothing about which cards an instruction exiled (#968), so
  -- that trigger is omitted from data/cards/hoarding-dragon.json. The omission
  -- runs STRICTER for the card's controller: the printed second half only ever
  -- hands its controller a card back, so pawl's Dragon buries the artifact for
  -- good and no assertion below can pass because of what is missing.
  --
  -- The destination is the assertion, and three readings have to be told apart:
  -- exile, hand (RevealThenHand) and battlefield (BattlefieldTapped). So the
  -- Altar is asserted present in exile AND absent from both other zones, and the
  -- empty reveal log separates CR 701.23e's silent exile from the Sextant's
  -- "reveal that card". The Piker is in the library so the filter has a card to
  -- reject; it is added second, so Support.addLibraryCard makes it the head and a
  -- filter that admitted everything would exile it instead.
  Spec.it s "CR 701.23a/701.23e whole card: Hoarding Dragon exiles the artifact it finds, unrevealed" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    let base0 = S.landsInPlay mountain 5
        (_, base1) = S.addLibraryCard altar S.alice base0
        (pikerId, base2) = S.addLibraryCard piker S.alice base1
        (gs, spellId) = S.handOne dragon base2
        cast = snd (Engine.runGamePure findFirstExercising gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure findFirstExercising cast Engine.priorityLoop)
    Spec.assertEqWith s "the Dragon resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Hoarding Dragon") S.alice settled) 1
    Spec.assertEqWith
      s
      "the Altar, and only the Altar, is in exile"
      (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.alice settled))
      [CardName.MkCardName $ Text.pack "Ashnod's Altar"]
    Spec.assertEqWith s "it did NOT go to her hand -- she cast her only card" (S.handSize S.alice settled) 0
    Spec.assertEqWith s "nor onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ashnod's Altar") S.alice settled) 0
    Spec.assertEqWith s "CR 701.23e: the card says only \"exile it\", so nothing was revealed" (S.revealsOf settled) []
    Spec.assertEqWith s "the nonartifact was no candidate and stayed in the library" (Game.zoneMembers Zone.Library S.alice settled) [pikerId]
  -- The paired negative: the SAME board, the same mana, the same answers, with
  -- CR 603.5's "may" declined instead of exercised. Nothing is searched and
  -- nothing is exiled, so an engine that exiled a card off some other path than
  -- this search would fail here.
  Spec.it s "CR 603.5 declining Hoarding Dragon's \"may\" exiles nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Hoarding Dragon"
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    let base0 = S.landsInPlay mountain 5
        (altarId, base1) = S.addLibraryCard altar S.alice base0
        (pikerId, base2) = S.addLibraryCard piker S.alice base1
        (gs, spellId) = S.handOne dragon base2
        cast = snd (Engine.runGamePure findFirstDeclining gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure findFirstDeclining cast Engine.priorityLoop)
    Spec.assertEqWith s "the Dragon still resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Hoarding Dragon") S.alice settled) 1
    Spec.assertEqWith s "exile is empty" (Game.zoneMembers Zone.Exile S.alice settled) []
    Spec.assertEqWith s "both library cards are still there" (Set.fromList (Game.zoneMembers Zone.Library S.alice settled)) (Set.fromList [altarId, pikerId])
  -- Fertilid's Favor -- "Target player searches their library for a basic land
  -- card, puts it onto the battlefield tapped, then shuffles. Put two +1/+1
  -- counters on up to one target artifact or creature." The whole-card proof that
  -- a search reads the library the effect's PlayerRef names rather than its
  -- controller's, cast and resolved rather than assembled.
  --
  -- THREE seats, because two collapse "the targeted player" onto "the one
  -- opponent" and a controller-defaulting engine would still be caught only by
  -- luck. alice casts, carol is targeted, and bob is the seat neither role names.
  --
  -- A Mountain in EVERY library, because one library holding the only basic land
  -- cannot tell "read carol's library" from "read every library": the assertions
  -- below are which SEAT gained the land and which libraries kept theirs, and
  -- each of the three answers a different reading of the rule. The Piker in
  -- carol's library gives the filter a card to reject; it is added second, so
  -- Support.addLibraryCard makes it the head and a filter that admitted
  -- everything would fetch it instead.
  Spec.it s "CR 701.23a whole card: Fertilid's Favor searches the TARGET player's library, not its controller's" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    favor <- S.printingOf s registry "Fertilid's Favor"
    let withLands = List.foldl' (\g _ -> snd (S.addCreature forest S.alice g)) S.threePlayerGame [1 .. (4 :: Int)]
        (aliceCard, g1) = S.addLibraryCard mountain S.alice withLands
        (bobCard, g2) = S.addLibraryCard mountain S.bob g1
        (_, g3) = S.addLibraryCard mountain S.carol g2
        (carolPiker, g4) = S.addLibraryCard piker S.carol g3
        (gs, spellId) = S.handOne favor g4
        cast = snd (Engine.runGamePure atCarolFinding gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure atCarolFinding cast Engine.priorityLoop)
        mountainName = CardName.MkCardName $ Text.pack "Mountain"
    Spec.assertEqWith s "carol got the basic land" (S.countOnBattlefieldByName mountainName S.carol settled) 1
    Spec.assertEqWith s "and it entered tapped" (S.tappedCount S.carol settled) 1
    Spec.assertEqWith s "the spell's controller got nothing -- her own Mountain was no candidate" (S.countOnBattlefieldByName mountainName S.alice settled) 0
    Spec.assertEqWith s "nor did the third seat" (S.countOnBattlefieldByName mountainName S.bob settled) 0
    Spec.assertEqWith s "alice's library is untouched" (Game.zoneMembers Zone.Library S.alice settled) [aliceCard]
    Spec.assertEqWith s "bob's library is untouched" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
    Spec.assertEqWith s "carol's library kept only the card the filter rejected" (Game.zoneMembers Zone.Library S.carol settled) [carolPiker]
  -- Explosive Vegetation -- "Search your library for up to two basic land cards,
  -- put them onto the battlefield tapped, then shuffle." The whole-card proof
  -- that a search's count is a MAXIMUM the searcher chooses within (CR 701.23a),
  -- cast and resolved rather than assembled. Its whole printed text is
  -- expressible, so nothing about pawl's copy runs weaker than the card.
  --
  -- THREE basic lands in the library against a cap of two, so the count is
  -- observable rather than exhausted: a search that found three because only
  -- three were there proves nothing about the "up to two". All three are
  -- DIFFERENT basics, so which two were found is assertable -- and none of them
  -- is the Forest the mana came from, which would otherwise make a fetched land
  -- indistinguishable from one already in play. The Piker gives the filter a
  -- nonland to reject.
  --
  -- The find is PINNED to specific ids rather than "the first n offered":
  -- Support.addLibraryCard prepends, so the head of the library is the Piker and
  -- then the Plains, and an engine that took the head of the candidate list
  -- would fetch the Plains this answer never names.
  --
  -- The three cases below are the same board and the same mana, differing in
  -- exactly one thing: how many of the two the searcher takes.
  Spec.it s "CR 701.23a whole card: Explosive Vegetation finds both of its \"up to two\"" $ do
    board <- vegetationBoard s registry
    let settled = resolveVegetation (findPinned [vegetationMountain board, vegetationIsland board]) board
    Spec.assertEqWith
      s
      "the Mountain and the Island she named are on the battlefield, beside her four Forests"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Battlefield S.alice settled)))
      (List.sort (fmap (CardName.MkCardName . Text.pack) ["Forest", "Forest", "Forest", "Forest", "Island", "Mountain"]))
    -- Every Forest paid for the spell, so nothing on the battlefield is untapped
    -- unless a fetched land entered that way -- which is the destination's whole
    -- assertion. Rule 701.23 says only how to LOOK, so "onto the battlefield
    -- tapped" is the card's own instruction.
    Spec.assertEqWith s "and both entered TAPPED" (fmap (`S.soleFaceName` settled) (untappedOf S.alice settled)) []
    Spec.assertEqWith
      s
      "the third basic and the nonland stayed in the library"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList [vegetationPlains board, vegetationPiker board])
  Spec.it s "CR 701.23b whole card: Explosive Vegetation may find FEWER than its \"up to two\"" $ do
    board <- vegetationBoard s registry
    let settled = resolveVegetation (findPinned [vegetationMountain board]) board
    Spec.assertEqWith
      s
      "only the one basic she named is on the battlefield"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Battlefield S.alice settled)))
      (List.sort (fmap (CardName.MkCardName . Text.pack) ["Forest", "Forest", "Forest", "Forest", "Mountain"]))
    Spec.assertEqWith s "and it entered tapped" (fmap (`S.soleFaceName` settled) (untappedOf S.alice settled)) []
    Spec.assertEqWith
      s
      "the two basics she passed over are still in the library"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList [vegetationIsland board, vegetationPlains board, vegetationPiker board])
  Spec.it s "CR 701.23b whole card: Explosive Vegetation may decline to find at all" $ do
    board <- vegetationBoard s registry
    let settled = resolveVegetation (findPinned []) board
    Spec.assertEqWith
      s
      "nothing was fetched -- only the Forests she paid with are on the battlefield"
      (List.sort (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Battlefield S.alice settled)))
      (replicate 4 (CardName.MkCardName (Text.pack "Forest")))
    Spec.assertEqWith
      s
      "every library card is still there"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList [vegetationMountain board, vegetationIsland board, vegetationPlains board, vegetationPiker board])
  -- Extract -- "{U} Sorcery: Search target player's library for a card and exile
  -- it. Then that player shuffles." The whole-card proof that the player LOOKING
  -- and the player whose library is looked at can be different seats (CR 701.23a),
  -- and that a search stating no quality must find (CR 701.23d).
  --
  -- Its filter is `And []`, the trivial predicate: "a card" states nothing about
  -- what may be found, so every card in the library is a candidate and no
  -- assertion here can pass because a filter quietly rejected something.
  Spec.it s "CR 701.23a whole card: Extract's controller searches the TARGET player's library" $ do
    island <- S.printingOf s registry "Island"
    extract <- S.printingOf s registry "Extract"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let (gs, spellId, aliceLib, bobCard, carolLib) = extractBoard island extract piker altar mountain forest plains
        pinned = case carolLib of
          _ : middle : _ -> middle
          _ -> ObjectId.MkObjectId 0
        cast = snd (Engine.runGamePure (aliceFinding pinned) gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure (aliceFinding pinned) cast Engine.priorityLoop)
    Spec.assertEqWith s "carol's library starts in the order the pins assume" (Game.zoneMembers Zone.Library S.carol gs) carolLib
    Spec.assertEqWith
      s
      "the card alice named, and only it, is in exile"
      (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.carol settled))
      [CardName.MkCardName $ Text.pack "Forest"]
    Spec.assertEqWith s "carol keeps the other two, in order" (Game.zoneMembers Zone.Library S.carol settled) (filter (/= pinned) carolLib)
    Spec.assertEqWith s "the searcher's own library is untouched -- she read carol's" (Game.zoneMembers Zone.Library S.alice settled) aliceLib
    Spec.assertEqWith s "and so is the third seat's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
    Spec.assertEqWith s "nothing of alice's was exiled" (Game.zoneMembers Zone.Exile S.alice settled) []
    Spec.assertEqWith s "nor of bob's" (Game.zoneMembers Zone.Exile S.bob settled) []
    Spec.assertEqWith s "the found card was exiled, not fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Forest") S.carol settled) 0
    Spec.assertEqWith s "nor put into a hand" (S.handSize S.carol settled) 0
  -- The same board, with the search answered "nothing". CR 701.23b's permission
  -- does NOT apply -- it is for a search stating a quality, and Extract states
  -- none -- so CR 701.23d makes the find mandatory and the answer is completed.
  -- The paired negative is "CR 701.23b Search may fail to find" above: same
  -- declining answer, a filter that states a quality, nothing found.
  Spec.it s "CR 701.23d whole card: Extract must find, so declining still exiles a card" $ do
    island <- S.printingOf s registry "Island"
    extract <- S.printingOf s registry "Extract"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let (gs, spellId, aliceLib, bobCard, carolLib) = extractBoard island extract piker altar mountain forest plains
        cast = snd (Engine.runGamePure aliceFindingNothing gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure aliceFindingNothing cast Engine.priorityLoop)
    Spec.assertEqWith
      s
      "a card was exiled anyway, and it is the head of the library the engine completed with"
      (fmap (`S.soleFaceName` settled) (Game.zoneMembers Zone.Exile S.carol settled))
      [CardName.MkCardName $ Text.pack "Plains"]
    Spec.assertEqWith s "carol's library lost exactly that one" (Game.zoneMembers Zone.Library S.carol settled) (drop 1 carolLib)
    Spec.assertEqWith s "alice's library is still untouched" (Game.zoneMembers Zone.Library S.alice settled) aliceLib
    Spec.assertEqWith s "and bob's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
  -- The same board again, with the shuffle answered by reversing whatever library
  -- it is offered. Which library gets shuffled is the CARD's sentence -- "then
  -- that player shuffles", its target -- rather than rule 701.24's, which says
  -- only what shuffling does (CR 701.24a). So carol's order changes and nobody
  -- else's does. An engine that shuffled the SEARCHER's library instead would
  -- overwrite alice's with carol's cards, which the last two assertions read
  -- directly.
  Spec.it s "CR 701.24a whole card: Extract shuffles the library it searched, the TARGET player's" $ do
    island <- S.printingOf s registry "Island"
    extract <- S.printingOf s registry "Extract"
    piker <- S.printingOf s registry "Goblin Piker"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let (gs, spellId, aliceLib, bobCard, carolLib) = extractBoard island extract piker altar mountain forest plains
        pinned = case carolLib of
          _ : middle : _ -> middle
          _ -> ObjectId.MkObjectId 0
        cast = snd (Engine.runGamePure (aliceFindingReversing pinned) gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure (aliceFindingReversing pinned) cast Engine.priorityLoop)
    Spec.assertEqWith s "carol's remaining cards came back reversed" (Game.zoneMembers Zone.Library S.carol settled) (reverse (filter (/= pinned) carolLib))
    Spec.assertEqWith s "alice's library kept its order -- hers was never shuffled" (Game.zoneMembers Zone.Library S.alice settled) aliceLib
    Spec.assertEqWith s "nor was bob's" (Game.zoneMembers Zone.Library S.bob settled) [bobCard]
  Spec.it s "CR 603/608.2n Rest in Peace's ETB exiles graveyards and ceases" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let g0 = Setup.emptyGame S.bothPlayers
        (ripId, g1) = S.addCreature restInPeace S.alice g0
        (deadId, g2) = S.addLibraryCard piker S.bob g1
        -- move the Piker into bob's graveyard
        g3 = S.runPure S.identityAnswer g2 (Event.changeZone deadId Zone.Graveyard)
        ability =
          TriggeredAbility.MkTriggeredAbility
            TriggerCondition.SelfEnters
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ExileAllGraveyards]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            Nothing
        (abilId, g4) = Game.freshObjectId g3
        (ts, g5) = Game.freshTimestamp g4
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfTrigger ripId ability,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
        resolved = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
    Spec.assertEqWith s "bob's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    Spec.assertEqWith s "ability ceased" (Game.lookupObject abilId resolved) Nothing
  Spec.it s "CR 103.5b ExileHandThenDraw exiles the whole hand, then draws that many" $ do
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    let g0 = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addHandCard mountain S.alice g0
        (_, g2) = S.addHandCard swamp S.alice g1
        g3 = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.alice g)) g2 (replicate 5 ())
        after =
          S.runPure S.identityAnswer g3 $
            Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty Effect.ExileHandThenDraw
    Spec.assertEqWith s "the hand is refilled to the size it had" (S.handSize S.alice after) 2
    Spec.assertEqWith s "both old cards went to exile" (length (Game.zoneMembers Zone.Exile S.alice after)) 2
    Spec.assertEqWith s "and the library is two shorter" (length (Game.zoneMembers Zone.Library S.alice after)) 3
  Spec.it s "CR 723.1: Mindslaver's ability installs pending control, promoted next turn" $ do
    mindslaver <- S.printingOf s registry "Mindslaver"
    let g0 = Setup.emptyGame S.bothPlayers
        (srcId, g1) = S.addCreature mindslaver S.alice g0
        slot = SlotName.MkSlotName (Text.pack "target")
        ability =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost =
                Cost.Type.MkCost
                  { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                    Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                  },
              ActivatedAbility.modal =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ControlPlayerNextTurn slot]))) (Map.singleton slot (TargetSlot.required Pool.Players Nothing))))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.restrictions = [],
              ActivatedAbility.condition = Nothing
            }
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfAbility srcId ability,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.bob))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = abilId : GameState.stack g3}
        resolved = snd (Engine.runGamePure S.identityAnswer g4 Stack.resolveTop)
        bobsTurn = snd (Engine.runGamePure S.identityAnswer resolved Engine.handoffTurn)
        afterBob = snd (Engine.runGamePure S.identityAnswer bobsTurn Engine.handoffTurn)
    Spec.assertEqWith s "control pending for bob" (Map.lookup S.bob (GameState.pendingControl resolved)) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "promoted on bob's turn" (GameState.activeControl bobsTurn) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "bob's decisions route to alice" (Decide.deciderFor S.bob bobsTurn) (Decider.MkDecider S.alice)
    Spec.assertEqWith s "control expired after bob's turn" (Decide.deciderFor S.bob afterBob) (Decider.MkDecider S.bob)
  Spec.it s "CR 723.1a: a second player-controlling effect overwrites the first (last created wins)" $ do
    mindslaver <- S.printingOf s registry "Mindslaver"
    let base = Setup.emptyGame S.bothPlayers
        -- First: alice controls bob.
        afterAlice = installControlBy mindslaver S.alice S.bob base
        -- Then: bob controls bob (CR 723.9 self-control), created LATER.
        afterBob = installControlBy mindslaver S.bob S.bob afterAlice
    Spec.assertEqWith s "the first effect installed alice as bob's decider" (Map.lookup S.bob (GameState.pendingControl afterAlice)) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "CR 723.1a: the later effect overwrites — bob's own control wins" (Map.lookup S.bob (GameState.pendingControl afterBob)) (Just (Decider.MkDecider S.bob))
  Spec.it s "CR 727.1a: resolving a RestartGame ability restarts with its controller as starting player" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        -- alice owns a card on the battlefield; it must survive the restart.
        -- aliceId only threads into the ability's Source.OfAbility below --
        -- CR 400.7 mints a fresh id for this card on the opening draw's zone
        -- change (Event.changeZone), so the post-restart check is ownership-
        -- based (SetupSpec's CR 727.2 test uses the same idiom), not a
        -- lookup by this specific pre-restart id.
        (aliceId, g1) = S.addCreature mountain S.alice g0
        -- bob owns 8 cards (enough for a full opening hand, no CR 727.3 loss).
        g2 = addMany mountain 8 S.bob g1
        g3 = addMany mountain 7 S.alice g2
        -- Hand-build bob's ability object on the stack: one mode, effect
        -- RestartGame, no targets. Object.owner = bob is the resolving
        -- controller (Resolve.hs), which restartGame uses as the starter.
        (abilId, g4) = Game.freshObjectId g3
        (ts, g5) = Game.freshTimestamp g4
        ability =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost =
                Cost.Type.MkCost
                  { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                    Cost.Type.components = []
                  },
              ActivatedAbility.modal =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton Effect.RestartGame))) Map.empty))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.restrictions = [],
              ActivatedAbility.condition = Nothing
            }
        abilObj =
          Object.MkObject
            { Object.owner = S.bob,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfAbility aliceId ability,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.bob,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
        after = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
    Spec.assertEqWith s "the game restarted with bob as the starting player (CR 727.1a)" (GameState.activePlayer after) S.bob
    Spec.assertEqWith s "alice's 8 cards all survived the restart, still hers (CR 727.2)" (length (filter (\o -> Object.owner o == S.alice) (Map.elems (GameState.objects after)))) 8
    Spec.assertEqWith s "the resolving ability object ceased to exist (not a card)" (Game.lookupObject abilId after) Nothing
  Spec.it s "CR 729.1b: PlaySubgame binds the loser, a later DealDamage reads it (mid-resolution binding visible)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        slot = SlotName.MkSlotName (Text.pack "loser")
        -- a stub runner: no real subgame, just report alice won -> loser = bob.
        stubRunner :: Game Result.Result
        stubRunner = pure (Result.Won S.alice)
        -- hand-build alice's spell on the stack: one chosen mode (index 0),
        -- effects [PlaySubgame slot, DealDamage slot (Literal 3)], no targets.
        (spellId, g1) = Game.freshObjectId g0
        (ts, g2) = Game.freshTimestamp g1
        -- a minimal synthetic card whose spell has the two effects above;
        -- mirrors the file's existing synthetic-card idiom (CR 612 test above).
        card = Card.Type.MkCard {Card.Type.layout = Layout.Normal, Card.Type.faces = NonEmpty.singleton face}
        face =
          Face.MkFace
            { Face.name = CardName.MkCardName $ Text.pack "Subgame Test Spell",
              Face.manaCost = Nothing,
              Face.typeLine = Face.typeLine (S.combinedFace lightningBolt),
              Face.power = Nothing,
              Face.toughness = Nothing,
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.empty,
              Face.staticAbilities = [],
              Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot slot) (Quantity.Literal 3))]))) Map.empty))
                  (ModeSelection.ChooseExactly 1),
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.rooms = Seq.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.characteristicPT = Nothing,
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
              Face.specialActions = [],
              Face.additionalCosts = [],
              Face.alternativeCosts = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable
            }
        spellObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfToken card,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g3 = g2 {GameState.objects = Map.insert spellId spellObj (GameState.objects g2), GameState.stack = spellId : GameState.stack g2}
        after = snd (Engine.runGamePure S.identityAnswer g3 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "bob (the derived loser) lost 3 life to the follow-on DealDamage" (S.lifeOf S.bob after) (Just 17)
  Spec.it s "CR 729.1b: PlaySubgame's derived loser is drawn from the subgame roster, not the full main-game seating (a departed seat is never the loser)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    -- bob departed the MAIN game before this effect resolves, so bob was never
    -- seated for the subgame (Setup.subgameStateFrom seats only
    -- Game.stillPlayingInOrder) -- only alice and carol played it. The
    -- stub reports alice won, so the derived loser must be carol; bob still
    -- appears in the raw seating roster (GameState.turnOrder) and is the
    -- non-participant a roster bug would wrongly name.
    let g0 = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
        slot = SlotName.MkSlotName (Text.pack "loser")
        stubRunner :: Game Result.Result
        stubRunner = pure (Result.Won S.alice)
        (spellId, g1) = Game.freshObjectId g0
        (ts, g2) = Game.freshTimestamp g1
        card = Card.Type.MkCard {Card.Type.layout = Layout.Normal, Card.Type.faces = NonEmpty.singleton face}
        face =
          Face.MkFace
            { Face.name = CardName.MkCardName $ Text.pack "Subgame Test Spell (Three Seats, One Departed)",
              Face.manaCost = Nothing,
              Face.typeLine = Face.typeLine (S.combinedFace lightningBolt),
              Face.power = Nothing,
              Face.toughness = Nothing,
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.empty,
              Face.staticAbilities = [],
              Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot slot) (Quantity.Literal 3))]))) Map.empty))
                  (ModeSelection.ChooseExactly 1),
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.rooms = Seq.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.characteristicPT = Nothing,
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
              Face.specialActions = [],
              Face.additionalCosts = [],
              Face.alternativeCosts = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable
            }
        spellObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfToken card,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.exiledFaceDown = False,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.ringBearerFor = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.kicked = False
            }
        g3 = g2 {GameState.objects = Map.insert spellId spellObj (GameState.objects g2), GameState.stack = spellId : GameState.stack g2}
        after = snd (Engine.runGamePure S.identityAnswer g3 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "carol (a genuine subgame participant) lost 3 life to the follow-on DealDamage" (S.lifeOf S.carol after) (Just 17)
    Spec.assertEqWith s "bob (departed before the subgame; never played it) was not named the loser and took no damage" (S.lifeOf S.bob after) (Just 20)
  Spec.it s "CR 111 Dragon Fodder creates two 1/1 Goblin tokens" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let base = S.landsInPlay mountain 2
        (gs, spellId) = S.handOne dragonFodder base
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- Two Goblin tokens exist (count == 2 proves two distinct objects). The
    -- battlefield also holds alice's 2 Mountains, so filter by name/creature.
    -- CR 111.4: Dragon Fodder does not name its tokens, so each is named
    -- "Goblin Token" -- its subtype plus the word "Token".
    Spec.assertEqWith s "two Goblin tokens on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 2
    Spec.assertEqWith s "alice controls two creatures (the tokens)" (S.creaturesInPlay S.alice after) 2
    Spec.assertEqWith s "Dragon Fodder went to the graveyard (CR 608.2n)" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- The control leg for Hanweir Garrison's "tapped and attacking" riders
    -- (CombatSpec's PutOntoBattlefieldAttacking group): a Create that says
    -- neither takes CR 110.5b's default and joins no combat, so the riders
    -- are the effect's and not something every token gets.
    Spec.assertEqWith s "CR 110.5b: the Goblins enter untapped" (Maybe.mapMaybe (\oid -> fmap Object.tapped (Game.lookupObject oid after)) (S.tokensOf after)) [TapState.Untapped, TapState.Untapped]
    Spec.assertEqWith s "and attacking nothing" (Combat.Type.attackers (GameState.combat after)) Map.empty
  Spec.it s "CR 615 Fog prevents combat damage but not spell damage (the gate)" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    fog <- S.printingOf s registry "Fog"
    let base = S.landsInPlay forest 1
        (victim, gs0) = S.addCreature piker S.bob base
        (gs1, fogId) = S.handOne fog gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice fogId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        combat = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Combat])
        spell = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Noncombat])
    Spec.assertEqWith s "Fog installed one replacement" (length (GameState.replacements resolved)) 1
    Spec.assertEqWith s "combat damage prevented (the cancel shape)" (S.damageOf victim combat) (Just 0)
    -- The falsifier: a tag-blind Fog would also blunt this spell damage.
    Spec.assertEqWith s "spell damage untouched (Noncombat)" (S.damageOf victim spell) (Just 2)
  -- Sudden Impact: "deals damage to target player equal to the number of
  -- cards in THAT player's hand." Cast through the real path (Cast.castSpell
  -- + resolveTop), not S.spellOnStack -- that helper sets Object.bindings =
  -- Map.empty and so does not fill the target slot the InSlot count reads.
  Spec.it s "Sudden Impact reads the TARGET's hand, not the caster's" $ do
    -- THE FALSIFIER for a perspective baked into the count: Alice holds
    -- five and Bob holds two, and Bob takes two. A count whose "you" were
    -- the resolving controller (Alice) would deal five instead.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    suddenImpact <- S.printingOf s registry "Sudden Impact"
    let gs0 = S.landsInPlay mountain 4
        fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
        gs1 = fill S.alice 5 (fill S.bob 2 gs0)
        (spellId, gs2) = S.addHandCard suddenImpact S.alice gs1
        cast = snd (Engine.runGamePure atBobAnswer gs2 (S.cast S.alice spellId))
        before = S.lifeOf S.bob cast
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "two damage" (S.lifeOf S.bob after) (fmap (subtract 2) before)
  Spec.it s "CR 608.2h the number is read as the effect is applied, not as the spell is cast" $ do
    -- Bob's hand grows AFTER Sudden Impact is on the stack and BEFORE it
    -- resolves; the damage follows the hand size at resolution.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    suddenImpact <- S.printingOf s registry "Sudden Impact"
    let gs0 = S.landsInPlay mountain 4
        fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
        gs1 = fill S.bob 2 gs0
        (spellId, gs2) = S.addHandCard suddenImpact S.alice gs1
        cast = snd (Engine.runGamePure atBobAnswer gs2 (S.cast S.alice spellId))
        (_, cast1) = S.addHandCard piker S.bob cast
        before = S.lifeOf S.bob cast1
        after = snd (Engine.runGamePure atBobAnswer cast1 Stack.resolveTop)
    Spec.assertEqWith s "three damage" (S.lifeOf S.bob after) (fmap (subtract 3) before)
  Spec.it s "the same count with Relative You reads the caster's hand" $ do
    -- The direct contrast: the SAME Count shape (InZone Hand, Members) that
    -- Sudden Impact scopes with PlayerRef.InSlot also serves Inner Calm,
    -- Outer Strength's PlayerRef.Relative You -- one shape, two
    -- perspectives, neither welded into a constructor.
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
        gs = fill S.alice 5 (fill S.bob 2 gs0)
        yourHand =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Hand (PlayerRef.Relative PlayerRelation.You)))
            (Filter.Type.And [])
            Aggregation.Members
    Spec.assertEqWith
      s
      "Alice's five"
      (S.countOf (\oid -> Just (Projection.viewOfObject oid gs)) (Filter.contextFor (Just S.alice) Nothing) gs yourHand)
      (Just 5)
  -- CR 205.4g, end to end: "any permanent with the supertype 'snow' is a
  -- snow permanent." Skred deals damage equal to the number of snow
  -- permanents YOU control, cast through the real path (Cast.castSpell +
  -- resolveTop) so the count is read at resolution off a real projection.
  --
  -- THE FALSIFIER, in both directions at once, which is why the board is
  -- lopsided. Alice has two Snow-Covered Mountains and two plain Mountains;
  -- Bob has one Snow-Covered Mountain and the Wall of Stone that takes the
  -- damage. The right answer is 2. A count blind to the supertype would see
  -- four permanents Alice controls and deal 4; a count blind to CR 109.5's
  -- controller would see three snow permanents and deal 3. All three numbers
  -- differ, so no single wrong reading can pass.
  --
  -- Wall of Stone is 0/8, so it survives and carries the damage as a mark
  -- (CR 120.3e, removed at CR 514.2's cleanup) that the assertion can read
  -- exactly -- a dead creature would only tell us the damage was at least
  -- its toughness.
  Spec.it s "CR 205.4g Skred counts the snow permanents YOU control, and nothing else" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    mountain <- S.printingOf s registry "Mountain"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    skred <- S.printingOf s registry "Skred"
    let gs0 = S.landsInPlay snowMountain 2
        gs1 = snd (S.addCreature mountain S.alice (snd (S.addCreature mountain S.alice gs0)))
        gs2 = snd (S.addCreature snowMountain S.bob gs1)
        (wall, gs3) = S.addCreature wallOfStone S.bob gs2
        (spellId, gs4) = S.addHandCard skred S.alice gs3
        cast = snd (Engine.runGamePure (atCreature wall) gs4 (S.cast S.alice spellId))
        after = snd (Engine.runGamePure (atCreature wall) cast Stack.resolveTop)
    Spec.assertEqWith s "no damage before it resolves" (S.damageOf wall cast) (Just 0)
    Spec.assertEqWith s "two snow permanents you control, so two damage" (S.damageOf wall after) (Just 2)
  -- CR 608.2h: the answer "is determined only once, when the effect is
  -- applied", so a quantity Projection.freezeQuantities cannot evaluate at
  -- that one moment has no later moment to be evaluated in. Storing the raw
  -- quantity would hand it to applyModification, which reads it against the
  -- AFFECTED object on every projection -- a wrong answer, not a deferred
  -- one. Nothing is stored instead, which is the posture CR 611.2b already
  -- gives this opcode when the duration never starts.
  --
  -- A bare Star is the unevaluable quantity here (CR 208.2: it has no value
  -- of its own); the literal leg is the control that keeps the empty result
  -- from passing vacuously.
  Spec.it s "CR 608.2h a modification that cannot be frozen is not stored at all" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        slot = SlotName.MkSlotName (Text.pack "target")
        store m =
          S.runPure S.identityAnswer gs $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.alice
              (Map.singleton slot (Set.singleton (Recipient.ToCreature pikerId)))
              (Map.singleton slot (Set.singleton (Recipient.ToCreature pikerId)))
              (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn m (ObjectRef.InSlot slot)))
        refused = store (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) Quantity.Star))
        stored = store (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)))
    Spec.assertEqWith s "no effect is stored for an unevaluable quantity" (GameState.continuousEffects refused) []
    Spec.assertEqWith s "and the Piker is its printed 2/1" (Projection.powerOf pikerId refused, Projection.toughnessOf pikerId refused) (Just 2, Just 1)
    Spec.assertEqWith s "the same call with two Literals DOES store one -- the refusal is what did it" (length (GameState.continuousEffects stored)) 1
    Spec.assertEqWith s "and pumps the Piker to 5/4" (Projection.powerOf pikerId stored, Projection.toughnessOf pikerId stored) (Just 5, Just 4)

-- Add n Mountains to pid's battlefield, discarding the ids (used to bulk up a
-- pool of owned cards). replicate n () avoids a list comprehension (CLAUDE.md).
addMany :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addMany mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature mountain pid g)) gs (replicate n ())

-- Build a Mindslaver-shaped ControlPlayerNextTurn ability owned by `controller`,
-- targeting `target`, put it on the stack, and resolve it. Returns the resulting
-- state. Object.owner is the resolving ability's controller (Resolve.hs), so this
-- installs pendingControl[target] = MkDecider controller.
installControlBy :: Printing.Printing -> PlayerId.PlayerId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
installControlBy mindslaver controller target gs0 =
  let (srcId, gs1) = S.addCreature mindslaver controller gs0
      slot = SlotName.MkSlotName (Text.pack "target")
      ability =
        ActivatedAbility.MkActivatedAbility
          { ActivatedAbility.cost =
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                  Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                },
            ActivatedAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ControlPlayerNextTurn slot]))) (Map.singleton slot (TargetSlot.required Pool.Players Nothing))))
                (ModeSelection.ChooseExactly 1),
            ActivatedAbility.restrictions = [],
            ActivatedAbility.condition = Nothing
          }
      (abilId, gs2) = Game.freshObjectId gs1
      (ts, gs3) = Game.freshTimestamp gs2
      abilObj =
        Object.MkObject
          { Object.owner = controller,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfAbility srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer target))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False
          }
      gs4 = gs3 {GameState.objects = Map.insert abilId abilObj (GameState.objects gs3), GameState.stack = abilId : GameState.stack gs3}
   in snd (Engine.runGamePure S.identityAnswer gs4 Stack.resolveTop)

-- CR 205.4c / 701.23a: a basic land card is one with the Land card type and the
-- Basic supertype -- Evolving Wilds' search filter, the printed-card predicate
-- that replaced CardCriterion.BasicLandCard.
basicLandFilter :: Filter.Type.Filter Keyword.Keyword
basicLandFilter =
  Filter.Type.And
    [ Filter.Type.HasCardType CardType.Land,
      Filter.Type.HasSupertype Supertype.Basic
    ]

-- Explosive Vegetation's board, built once and shared by its three cases so
-- they differ in the ANSWER alone. Four Forests pay the {3}{G} -- all of them,
-- which is what makes "nothing untapped" an assertion about the fetch -- and the
-- library holds three DIFFERENT basic lands against a cap of two, plus a nonland
-- for the filter to reject.
data VegetationBoard = MkVegetationBoard
  { vegetationState :: GameState.GameState,
    vegetationSpell :: ObjectId.ObjectId,
    vegetationMountain :: ObjectId.ObjectId,
    vegetationIsland :: ObjectId.ObjectId,
    vegetationPlains :: ObjectId.ObjectId,
    vegetationPiker :: ObjectId.ObjectId
  }

vegetationBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m VegetationBoard
vegetationBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  vegetation <- S.printingOf s registry "Explosive Vegetation"
  let (mountainId, g1) = S.addLibraryCard mountain S.alice (S.landsInPlay forest 4)
      (islandId, g2) = S.addLibraryCard island S.alice g1
      (plainsId, g3) = S.addLibraryCard plains S.alice g2
      (pikerId, g4) = S.addLibraryCard piker S.alice g3
      (gs, spellId) = S.handOne vegetation g4
  pure (MkVegetationBoard gs spellId mountainId islandId plainsId pikerId)

resolveVegetation :: (forall r. Prompt.Prompt r -> r) -> VegetationBoard -> GameState.GameState
resolveVegetation answer board =
  let cast = snd (Engine.runGamePure answer (vegetationState board) (S.cast S.alice (vegetationSpell board)))
   in snd (Engine.runGamePure answer cast Engine.priorityLoop)

-- Finds exactly the cards named and nothing else, whatever the engine offers.
-- PINNED rather than picked out of the candidate list: an answerer that went
-- looking for a legal choice would find one again after a mutation, and the
-- assertion would stay green while the engine's own count was broken.
findPinned :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
findPinned wanted p = case p of
  Prompt.SearchLibrary {} -> wanted
  _ -> S.identityAnswer p

untappedOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
untappedOf pid gs =
  let isUntapped oid = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
   in filter isUntapped (Game.zoneMembers Zone.Battlefield pid gs)

-- Finds as many as the search allows, taking them off the head of the offered
-- list -- one card for the searches that ask for one.
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches cap -> List.genericTake cap matches
  _ -> S.identityAnswer p

-- Names a card the search filter did NOT admit -- the lying interpreter #222 is
-- about. Parameterised so the test can point it at a specific nonland.
findForbidden :: ObjectId.ObjectId -> Prompt.Prompt r -> r
findForbidden wanted p = case p of
  Prompt.SearchLibrary {} -> [wanted]
  _ -> S.identityAnswer p

-- findFirst, plus CR 603.5's printed "may" taken. The pair below it declines the
-- same "may" and answers every other prompt identically, so a board run through
-- both differs in exactly that one decision.
findFirstExercising :: Prompt.Prompt r -> r
findFirstExercising p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> findFirst p

findFirstDeclining :: Prompt.Prompt r -> r
findFirstDeclining p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  _ -> findFirst p

findNothing :: Prompt.Prompt r -> r
findNothing p = case p of
  Prompt.SearchLibrary {} -> []
  _ -> S.identityAnswer p

-- Fertilid's Favor's answerer, in three parts. CR 601.2c is announced at its
-- FLOOR, so the Favor's "up to one target artifact or creature" takes no target
-- at all and the searching player is the only slot left to fill; what remains is
-- aimed at carol wherever she is offered (line 4536's idiom, a preference rather
-- than a filter, so a slot she is no candidate for still gets a legal answer).
--
-- The find is PINNED to carol: an engine that asked the spell's controller to
-- search instead finds nothing at all, rather than helpfully finding a card in
-- whichever library it was handed.
atCarolFinding :: Prompt.Prompt r -> r
atCarolFinding p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (TargetCount.least . fst) offers
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (List.nub (filter (== Recipient.ToPlayer S.carol) (Set.toAscList legal) <> Set.toAscList legal))))
      sets
  Prompt.SearchLibrary _ pid matches cap ->
    if pid == S.carol
      then List.genericTake cap matches
      else []
  _ -> S.identityAnswer p

-- Extract's board, built once so the three cases below differ in exactly one
-- thing: the answer. Three seats, because two collapse "target player" onto "the
-- one opponent" and the whole point of the card is that the searcher and the
-- library's owner are different players.
--
-- Every library is stocked, and alice's with TWO cards, so "which library was
-- read" and "which library was shuffled" are both observable: a single-card
-- library cannot show a reordering. The printings, in argument order: the Island
-- alice taps, the Extract in her hand, the Goblin Piker she has two of, bob's
-- Ashnod's Altar, then carol's Mountain, Forest and Plains -- added in that
-- order, so Support.addLibraryCard's prepending leaves the Plains at the head and
-- the Forest in the middle. The first case below asserts that order before
-- casting anything, since the pins rest on it.
extractBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId, [ObjectId.ObjectId])
extractBoard island extract piker altar mountain forest plains =
  let g0 = S.landsFor island S.alice 1 S.threePlayerGame
      (aliceOne, g1) = S.addLibraryCard piker S.alice g0
      (aliceTwo, g2) = S.addLibraryCard piker S.alice g1
      (bobCard, g3) = S.addLibraryCard altar S.bob g2
      (carolMountain, g4) = S.addLibraryCard mountain S.carol g3
      (carolForest, g5) = S.addLibraryCard forest S.carol g4
      (carolPlains, g6) = S.addLibraryCard plains S.carol g5
      (gs, spellId) = S.handOne extract g6
   in (gs, spellId, [aliceTwo, aliceOne], bobCard, [carolPlains, carolForest, carolMountain])

-- Extract's answerers, sharing one targeting half with atCarolFinding above:
-- carol is preferred wherever a target is offered, so the spell's one slot names
-- her. They differ ONLY in what the search is answered and how the shuffle is,
-- so the three boards below are one board with one variable each.
--
-- The find is pinned to a card id AND to the asking seat. Asking carol -- the
-- library's owner rather than the spell's controller -- gets an empty answer,
-- which under CR 701.23d is not "nothing happens" but "the head is completed in",
-- so the pinned card is deliberately NOT the head.
atCarolTargeted :: Prompt.Prompt r -> r
atCarolTargeted p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (TargetCount.least . fst) offers
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (List.nub (filter (== Recipient.ToPlayer S.carol) (Set.toAscList legal) <> Set.toAscList legal))))
      sets
  _ -> S.identityAnswer p

aliceFinding :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aliceFinding wanted p = case p of
  Prompt.SearchLibrary _ pid _ _ -> if pid == S.alice then [wanted] else []
  _ -> atCarolTargeted p

aliceFindingNothing :: Prompt.Prompt r -> r
aliceFindingNothing p = case p of
  Prompt.SearchLibrary {} -> []
  _ -> atCarolTargeted p

-- aliceFinding, plus a shuffle that REVERSES the library it is offered. Game
-- .honourShuffle accepts any permutation of what was offered, so the reversal is
-- honoured and names which library the shuffle read and wrote.
aliceFindingReversing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aliceFindingReversing wanted p = case p of
  Prompt.Shuffle offered -> reverse offered
  _ -> aliceFinding wanted p

-- Casts every castable spell (targets via lookupMin: creatures first),
-- otherwise passes. Drives the Bolt-vs-Bolt integration falsifier.
boltAnswer :: Prompt.Prompt r -> r
boltAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          A.Cast {} -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> A.Pass
  _ -> S.identityAnswer p

-- bob's Piker on the battlefield; alice holds TWO Bolts and two Mountains, in
-- her main phase. boltAnswer casts both (CR 117.3c keeps priority), both
-- target the Piker (the only creature), and the priority loop resolves them
-- LIFO: B kills the Piker, the mid-loop SBA buries it, A fizzles.
twoBoltState :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
twoBoltState piker mountain lightningBolt =
  let (_, withPiker) = S.addCreature piker S.bob (S.landsInPlay mountain 2)
      (gs1, _oid1) = S.handOne lightningBolt withPiker
      (oid2, gs2) = Game.freshObjectId gs1
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard lightningBolt,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.timestamp = Timestamp.MkTimestamp 0,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False
          }
   in gs2
        { GameState.objects = Map.insert oid2 obj (GameState.objects gs2),
          -- handOne already put oid1 in hand; ADD the second Bolt, oid2.
          GameState.hand = Map.adjust (oid2 Seq.<|) S.alice (GameState.hand gs2)
        }

-- alice has 3 Islands and Cancel in hand; a `victim` spell (bob's) sits on the
-- stack. Returns (victimId, state after alice casts Cancel at it and it resolves).
cancelVictim :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
cancelVictim island cancel victim =
  let base = S.landsInPlay island 3
      (victimId, onStack) = S.spellOnStack victim S.bob base
      (gs, cancelId) = S.handOne cancel onStack
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice cancelId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (victimId, resolved)

-- Append a second card of `printing` to `pid`'s hand (handOne overwrites the hand,
-- so a second in-hand card must be appended, not re-inserted).
handAppend :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
handAppend printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      obj = Object.MkObject pid Nothing (Source.OfCard printing) Zone.Hand TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled pid) Map.empty Map.empty Nothing Nothing Nothing Set.empty (Timestamp.MkTimestamp 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False
   in ( oid,
        gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs1)
          }
      )

-- alice has 6 Islands and TWO Cancels; a Piker (bob's) sits on the stack. alice
-- casts Cancel A at the Piker, then Cancel B at the Piker (CR 117.3c keeps
-- priority). Stack [B, A, Piker]; resolveTop LIFO: B counters the Piker, then A --
-- its only target gone -- fizzles (CR 608.2b).
racingCounters :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
racingCounters island piker cancel =
  let base = S.landsInPlay island 6
      (victimId, onStack) = S.spellOnStack piker S.bob base
      (gs1, cancelA) = S.handOne cancel onStack
      (cancelB, gs2) = handAppend cancel S.alice gs1
      atVictim :: Prompt.Prompt r -> r
      atVictim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject victimId))) sets
        _ -> S.identityAnswer p
      castA = snd (Engine.runGamePure atVictim gs2 (S.cast S.alice cancelA))
      castB = snd (Engine.runGamePure atVictim castA (S.cast S.alice cancelB))
      r1 = snd (Engine.runGamePure atVictim castB Stack.resolveTop) -- B counters the Piker
      r2 = snd (Engine.runGamePure atVictim r1 Stack.resolveTop) -- A fizzles
   in r2

counterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
counterSpec s registry = Spec.describe s "Counter" $ do
  Spec.it s "CR 701.6 Cancel counters a spell into its owner's graveyard" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, resolved) = cancelVictim island cancel piker
    Spec.assertEqWith s "victim countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 1
    Spec.assertEqWith s "victim never resolved onto the battlefield" (S.creaturesInPlay S.bob resolved) 0
    Spec.assertEqWith s "Cancel in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
  -- CR 113.6g: "an object's ability that states it can't be countered …
  -- functions on the stack", and CR 101.2 makes the "can't" win. The twin is
  -- the case directly above: the same Cancel, cast the same way at a spell
  -- that does not say it, DOES counter -- so this is the card's clause and
  -- not a broken Cancel.
  Spec.it s "CR 113.6g whole card: Cancel resolves but cannot counter Rending Volley" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    rendingVolley <- S.printingOf s registry "Rending Volley"
    let (victimId, resolved) = cancelVictim island cancel rendingVolley
    Spec.assertBool s (elem victimId (GameState.stack resolved)) "Rending Volley is still on the stack"
    Spec.assertEqWith s "and not in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    -- CR 101.2 again, from the other side: the countering spell is not itself
    -- stopped. Cancel targeted legally (CR 113.6g grants no shroud), resolved,
    -- did nothing, and CR 608.2n put it into its owner's graveyard as the
    -- final part of that resolution.
    Spec.assertEqWith s "Cancel resolved into alice's graveyard regardless" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
  Spec.it s "CR 608.2b a Cancel whose target already left the stack fizzles" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    cancel <- S.printingOf s registry "Cancel"
    let after = racingCounters island piker cancel
    Spec.assertEqWith s "the Piker moved exactly once, to bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "both Cancels in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
    Spec.assertEqWith s "the Piker never hit the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "stack cleared" (length (GameState.stack after)) 0
  Spec.it s "CR 614 Cancel under Rest in Peace exiles the countered spell" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    cancel <- S.printingOf s registry "Cancel"
    let (_, ripOut) = S.addCreature restInPeace S.alice (S.landsInPlay island 3)
        (_victimId, onStack) = S.spellOnStack piker S.bob ripOut
        (gs, cancelId) = S.handOne cancel onStack
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice cancelId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the countered spell is not in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    Spec.assertEqWith s "the countered spell is exiled" (length (Game.zoneMembers Zone.Exile S.bob resolved)) 1

-- The board every Mana Leak case starts from, with only `bobLands` varying.
-- alice has two Islands (Mana Leak's {1}{U}) and a Mana Leak in hand; bob has
-- `bobLands` untapped Islands of his own and a Goblin Piker already on the
-- stack. Returns the Piker's id and the state after alice casts Mana Leak at it.
--
-- The Piker is on the stack BEFORE Mana Leak is cast, so it holds the lower
-- object id and identityAnswer's ChooseTargets -- Set.lookupMin over the legal
-- recipients -- aims the Leak at it. The cancelVictim route above, and the same
-- reason.
manaLeakBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
manaLeakBoard island manaLeak piker bobLands =
  let (victimId, leakId, gs) = manaLeakHand island manaLeak piker bobLands
   in (victimId, snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice leakId)))

-- manaLeakBoard one step earlier, with the Leak still in alice's hand. Split out
-- for the case that has to RECORD the cast as well as the resolution: an engine
-- that offered CR 118.12a's cost at cast time would put its prompt outside a
-- transcript that starts afterwards, and the countered-Leak case below exists to
-- catch exactly that.
manaLeakHand :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
manaLeakHand island manaLeak piker bobLands =
  let base = S.landsInPlay island 2
      withBob = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) base [1 .. bobLands]
      (victimId, onStack) = S.spellOnStack piker S.bob withBob
      (gs, leakId) = S.handOne manaLeak onStack
   in (victimId, leakId, gs)

-- Pays what a resolving spell or ability offers `who`, and takes the identity
-- fallback elsewhere (the hackToIsland liar pattern). Deliberately unlike
-- identityAnswer's Declines, so a test can tell an honoured answer from the
-- fallback -- and so a pair of branches can differ in NOTHING but this.
--
-- Guarded on a NAMED player rather than paying whoever is asked, which is what
-- makes the cases below prove CR 118.12's "who". An engine that offered the cost
-- to the wrong player falls through to Declines and fails, rather than paying and
-- passing: for Mana Leak the payer is the TARGETED spell's controller and not the
-- resolving spell's, and for Whipstitched Zombie it is the ability's own (CR
-- 603.3a). The Decider is checked alongside the player for CR 723.1: nobody is
-- controlling anybody in these fixtures, so the two must agree.
--
-- Rank-1, like Pawl.Support.attackTo: the implicit forall is outermost, so
-- `paysFor S.bob` is the `forall r. Prompt r -> r` that Replay.record wants.
paysFor :: PlayerId.PlayerId -> Prompt.Prompt r -> r
paysFor who p = case p of
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == who && player == who ->
        PaymentDecision.Pays
  _ -> S.identityAnswer p

bobPaysAnswer :: Prompt.Prompt r -> r
bobPaysAnswer = paysFor S.bob

-- manaLeakBoard with a Thalia on bob's side and one more Island each. alice
-- needs the third Island because Thalia taxes HER cast (CR 601.2f), which is
-- what makes the case below a paired assertion rather than one; bob needs three
-- untapped Islands and no more, so a gate cost routed through that same rule
-- would be one mana short.
--
-- Thalia is added BEFORE the Piker, so the Piker still holds the lower stack id
-- and the Leak is aimed as manaLeakBoard describes.
thaliaLeakBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
thaliaLeakBoard island thalia manaLeak piker =
  let base = S.landsInPlay island 3
      (_thaliaId, withThalia) = S.addCreature thalia S.bob base
      withBob = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) withThalia [1 .. 3 :: Int]
      (victimId, onStack) = S.spellOnStack piker S.bob withBob
      (gs, leakId) = S.handOne manaLeak onStack
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice leakId))
   in (victimId, cast)

-- bobPaysAnswer, plus: BOB's target choices avoid `notThis`. What lets one
-- interpreter drive the whole countered-Leak exchange -- alice's Mana Leak takes
-- identityAnswer's lowest-id recipient and hits the Piker, which was on the stack
-- first, while bob's Cancel skips the Piker and hits the Leak above it. The two
-- casts are told apart by WHO is casting, which is on the prompt.
--
-- Still pays for bob wherever a cost is offered, which is the whole point: the
-- exchange must be able to answer a ChooseToPay, so that a transcript with none
-- in it says the prompt was never raised rather than that nobody would have paid.
bobPaysAndCounters :: ObjectId.ObjectId -> Prompt.Prompt r -> r
bobPaysAndCounters notThis p = case p of
  Prompt.ChooseTargets _ player _ sets
    | player == S.bob ->
        fmap (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (Set.toAscList (Set.filter (\r -> Recipient.objectOf r /= Just notThis) legal)))) sets
  _ -> bobPaysAnswer p

-- The pay-or-not answers in a transcript, in order.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isExileResponse :: Response.Response -> Bool
isExileResponse response = case response of
  Response.ChoseExilesFromGraveyard _ -> True
  _ -> False

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- CR 118.12 / 118.12a: Mana Leak's "Counter target spell unless its controller
-- pays {3}" -- a cost paid when the spell RESOLVES, by a player who is not the
-- resolving spell's controller, with the counter on the refusal branch.
--
-- The first two cases run the SAME board and the SAME cast and differ in NOTHING
-- but bob's answer, so the difference in outcome is the gate and nothing else;
-- the third changes only how many Islands he holds. CR 608.2n's "Mana Leak in
-- alice's graveyard" is asserted in all three: the resolution continues either
-- way, a refusal being the other branch rather than a failure.
manaLeakSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manaLeakSpec s registry = Spec.describe s "ManaLeak" $ do
  Spec.it s "CR 118.12a the targeted spell's controller declines, so it is countered" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, cast) = manaLeakBoard island manaLeak piker 3
        ((_, after), transcript) = Replay.record S.identityAnswer cast Stack.resolveTop
    -- bob COULD have paid -- three untapped Islands -- so he was really asked,
    -- and the refusal is his rather than CR 118.3's.
    Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "the Piker was countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and never reached the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "declining spent nothing: bob's Islands are all untapped" (S.tappedCount S.bob after) 0
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  Spec.it s "CR 118.12a the targeted spell's controller pays, so it is not countered" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = manaLeakBoard island manaLeak piker 3
        ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- The payment really happened: CR 605.3a lets the payer activate mana
    -- abilities "whenever a rule or effect asks for a mana payment, even if
    -- it's in the middle of ... resolving a spell", and three Islands paid {3}.
    Spec.assertEqWith s "paying tapped three of bob's Islands" (S.tappedCount S.bob after) 3
    -- CR 118.12a's other branch: the counter did not happen, and the spell is
    -- still there to resolve. Asserting only "not in the graveyard" would pass
    -- for a spell that never resolved at all, so the next line resolves it.
    Spec.assertBool s (elem victimId (GameState.stack after)) "the Piker is still on the stack"
    Spec.assertEqWith s "nothing of bob's is in his graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- CR 400.7 mints a fresh incarnation on the battlefield, so the permanent
    -- is counted rather than looked up by the spell's id.
    let played = snd (Engine.runGamePure bobPaysAnswer after Stack.resolveTop)
    Spec.assertEqWith s "and the Piker then resolves onto the battlefield" (S.creaturesInPlay S.bob played) 1
  -- CR 118.3 / 118.12: "can't" is the rule's own third case, and its Standstill
  -- example is exactly an unpayable cost. Two Islands cannot pay {3}, so there
  -- is one possible answer and the prompt is not raised -- proved by the
  -- transcript, under an interpreter that WOULD have paid.
  Spec.it s "CR 118.12 a controller who cannot pay {3} is not asked, and is countered" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, cast) = manaLeakBoard island manaLeak piker 2
        ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "bob was never asked" (payResponses transcript) []
    Spec.assertEqWith s "nothing of bob's was tapped" (S.tappedCount S.bob after) 0
    Spec.assertEqWith s "the Piker was countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and never reached the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  -- CR 601.2f totals the cost of a spell being CAST -- "the player determines the
  -- total cost of the spell ... plus all additional costs and cost increases" --
  -- and a cost paid during resolution is not that, so no cost increase reaches
  -- it. ONE Thalia proves both halves at once: bob's tax is live enough to make
  -- alice spend a third Island casting the Leak, and it still leaves the {3} at
  -- {3}. Routed through Cost.total, the gate would ask for {4}, bob's three
  -- Islands would fail CR 118.3, and he would never be asked at all.
  Spec.it s "CR 601.2f a cost increase taxes the CAST and not the resolution payment" $ do
    island <- S.printingOf s registry "Island"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = thaliaLeakBoard island thalia manaLeak piker
    -- The control, and the half that must NOT change: Thalia is on the
    -- battlefield and taxing. Mana Leak is {1}{U}, so an untaxed cast leaves an
    -- Island untapped and this reads 2.
    Spec.assertEqWith s "Thalia taxed alice's cast: all three of her Islands paid {2}{U}" (S.tappedCount S.alice cast) 3
    let ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "bob was asked, so {3} was still payable" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- Three, not four: the same Thalia that just cost alice an Island adds
    -- nothing here. Thalia herself is untapped, so this counts Islands only.
    Spec.assertEqWith s "and {3} cost bob exactly three Islands" (S.tappedCount S.bob after) 3
    Spec.assertBool s (elem victimId (GameState.stack after)) "the Piker was not countered"
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  -- CR 608.2d's timing, from the other side of Prompt.ChooseToPay's claim that a
  -- countered Mana Leak never asks: the cost is offered when the spell RESOLVES,
  -- so a spell that never resolves offers nothing. The MagicalHackTiming pair
  -- makes the same argument about a resolution-time choice, and this is built the
  -- same way -- the CAST is inside the recording, because an engine that offered
  -- the cost at CR 601.2b-f would raise its prompt there and a transcript opened
  -- afterwards would never see it.
  Spec.it s "CR 608.2d a countered Mana Leak never offers its cost" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    cancel <- S.printingOf s registry "Cancel"
    -- SIX Islands for bob, not three: three pay Cancel's {1}{U}{U} and three are
    -- left over. Without the spare three he could not have paid {3} anyway, and
    -- an empty transcript would prove nothing (CR 118.3 would be the reason).
    let (victimId, leakId, board) = manaLeakHand island manaLeak piker 6
        (cancelId, gs) = S.addHandCard cancel S.bob board
        exchange = do
          S.cast S.alice leakId
          S.cast S.bob cancelId
          Stack.resolveTop
        ((_, after), transcript) = Replay.record (bobPaysAndCounters victimId) gs exchange
    -- The control: the exchange really happened. CR 701.6a puts the countered
    -- Leak into its owner's graveyard, and CR 608.2n puts Cancel into bob's.
    Spec.assertEqWith s "CR 701.6a: the countered Leak is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "CR 608.2n: Cancel resolved into bob's" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertBool s (elem victimId (GameState.stack after)) "and the Piker, never the Leak's business again, is still on the stack"
    -- And the point: a spell that never resolves never offers its cost.
    Spec.assertEqWith s "bob was never offered the {3}" (payResponses transcript) []
    -- Not because he could not have paid it: three Islands are still untapped,
    -- and this interpreter pays whenever it is asked. Offered at either end of
    -- the exchange, this would read 6.
    Spec.assertEqWith s "only Cancel's three Islands are tapped" (S.tappedCount S.bob after) 3

-- Whipstitched Zombie and one untapped Swamp on alice's battlefield, her upkeep
-- begun and the trigger settled onto the stack (CR 603.3b). Returns the Zombie,
-- the Swamp and that state.
--
-- The Bitterblossom fixture in Pawl.TriggerSpec, one step short: the trigger is
-- left ON the stack so a case can assert what the board looked like before it
-- resolved, which is what rules out a Zombie that was never there.
zombieUpkeep :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
zombieUpkeep swamp zombie =
  let (zombieId, g1) = S.addCreature zombie S.alice (Setup.emptyGame S.bothPlayers)
      (swampId, g2) = S.addCreature swamp S.alice g1
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (g2 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in (zombieId, swampId, snd (Engine.runGamePure S.identityAnswer begun Engine.settleForPriority))

-- CR 118.12a again, reached from Pawl.Engine.Resolve.resolveModes rather than
-- resolveSpellWith: Whipstitched Zombie's "At the beginning of your upkeep,
-- sacrifice this creature unless you pay {B}" is a TRIGGERED ability, so the
-- ability executor asks the gate instead of the spell path. The seam is one
-- `paid` call shared by both, and this is the half Mana Leak cannot reach.
--
-- The payer is the ability's own controller (CR 603.3a's "your upkeep", bound
-- into Pawl.Engine.Binding.you as the trigger is placed) rather than Mana Leak's
-- targeted-spell controller -- the same slot read answering a different card.
--
-- Both cases start from the SAME board and the SAME settled trigger and differ
-- in NOTHING but alice's answer.
whipstitchedZombieSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
whipstitchedZombieSpec s registry = Spec.describe s "WhipstitchedZombie" $ do
  Spec.it s "CR 118.12a declining the {B} sacrifices the Zombie to its owner's graveyard" $ do
    swamp <- S.printingOf s registry "Swamp"
    zombie <- S.printingOf s registry "Whipstitched Zombie"
    let (zombieId, swampId, onStack) = zombieUpkeep swamp zombie
        ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
    -- The two controls the assertions below would otherwise be satisfied by:
    -- a Zombie that was never on the battlefield, and a trigger that never
    -- fired. Both are ruled out BEFORE the resolution.
    Spec.assertBool s (S.onBattlefield zombieId onStack) "the Zombie is on the battlefield before its upkeep trigger resolves"
    Spec.assertBool s (not (null (GameState.stack onStack))) "and the upkeep trigger really reached the stack"
    -- alice COULD have paid -- one untapped Swamp -- so the refusal is hers
    -- rather than CR 118.3's.
    Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    -- CR 701.21a: sacrificed, so it is IN the graveyard rather than merely gone.
    -- The Swamp is still on the battlefield, so that one graveyard card can only
    -- be the Zombie.
    Spec.assertEqWith s "the Zombie is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertBool s (S.onBattlefield swampId after) "and the Swamp, which is not what was sacrificed, is still in play"
    Spec.assertEqWith s "no creature is left in play" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "declining spent nothing: the Swamp is untapped" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  Spec.it s "CR 118.12a paying the {B} leaves the Zombie on the battlefield" $ do
    swamp <- S.printingOf s registry "Swamp"
    zombie <- S.printingOf s registry "Whipstitched Zombie"
    let (zombieId, _swampId, onStack) = zombieUpkeep swamp zombie
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- CR 605.3a lets her tap the Swamp for it mid-resolution.
    Spec.assertEqWith s "paying tapped the Swamp" (S.tappedCount S.alice after) 1
    -- The same id, not a fresh one: nothing moved zones, so this is the very
    -- permanent the trigger was about.
    Spec.assertBool s (S.onBattlefield zombieId after) "the Zombie is still on the battlefield"
    Spec.assertEqWith s "nothing was sacrificed" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Circling Vultures on alice's battlefield with the given cards already in her
-- graveyard, IN THE ORDER GIVEN so the last one is the top (CR 404.1), her
-- upkeep begun and the trigger settled onto the stack -- zombieUpkeep's shape
-- one card over. Returns the Vultures and that state; the buried cards are
-- asserted on by NAME, since CR 400.7 renames them on the way out.
vulturesUpkeep :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
vulturesUpkeep vultures buried =
  let (vulturesId, g1) = S.addCreature vultures S.alice (Setup.emptyGame S.bothPlayers)
      bury g printing = snd (S.addGraveyardCard printing S.alice g)
      g2 = List.foldl' bury g1 buried
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (g2 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in (vulturesId, snd (Engine.runGamePure S.identityAnswer begun Engine.settleForPriority))

-- CR 118.12a's gate again, over the cost component that has no choice in it:
-- Circling Vultures' "At the beginning of your upkeep, sacrifice this creature
-- unless you exile the top creature card of your graveyard"
-- (CostComponent.ExileTopFromGraveyard). CR 404.2 keeps a graveyard's order
-- fixed, so "the top creature card" names ONE card and nothing is prompted for
-- -- which is the whole difference from Headless Skaab's chosen exile
-- (Pawl.CostSpec).
--
-- THE FIXTURE SHAPE that makes the last case discriminating: TWO creature cards
-- in the graveyard, buried in a known order. An implementation reading the
-- wrong end exiles the other one and passes every other case here.
circlingVulturesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
circlingVulturesSpec s registry = Spec.describe s "CirclingVultures" $ do
  Spec.it s "CR 118.3 an empty graveyard cannot pay, so the Vultures are sacrificed" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    let (vulturesId, onStack) = vulturesUpkeep vultures []
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (S.onBattlefield vulturesId onStack) "the Vultures are on the battlefield before the trigger resolves"
    Spec.assertBool s (not (null (GameState.stack onStack))) "and the upkeep trigger really reached the stack"
    Spec.assertBool s (not (S.onBattlefield vulturesId after)) "the Vultures were sacrificed"
    Spec.assertEqWith s "CR 701.21a into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
    -- CR 118.3: an unpayable cost is not offered, so this interpreter's
    -- willingness to pay never comes up.
    Spec.assertEqWith s "alice was not asked to pay" (payResponses transcript) []
  -- The primary negative, Headless Skaab's argument unchanged: an
  -- implementation that ignored the Filter still refuses an empty graveyard,
  -- and only a graveyard holding exactly one INELIGIBLE card tells the two
  -- apart.
  Spec.it s "CR 601.2f a noncreature card in the graveyard cannot pay it either" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (vulturesId, onStack) = vulturesUpkeep vultures [bolt]
        ((_, after), _) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (not (S.onBattlefield vulturesId after)) "the Vultures were sacrificed"
    Spec.assertEqWith s "nothing was exiled" (namesIn Zone.Exile S.alice after) []
    -- CR 404.1 again, from the other side: the sacrificed Vultures arrive on top
    -- of the Bolt that could not pay for them.
    Spec.assertEqWith
      s
      "and the Bolt is still in the graveyard, under them"
      (namesIn Zone.Graveyard S.alice after)
      [Just (S.printingName bolt), Just (S.printingName vultures)]
  Spec.it s "CR 118.12a a creature card in the graveyard pays it and the Vultures survive" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    piker <- S.printingOf s registry "Goblin Piker"
    let (vulturesId, onStack) = vulturesUpkeep vultures [piker]
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (S.onBattlefield vulturesId after) "the Vultures are still on the battlefield"
    Spec.assertEqWith s "CR 406.2 the Piker was exiled" (namesIn Zone.Exile S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "and alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "alice was asked whether to pay, and once" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- CR 404.2 leaves nothing to choose, so paying prompts for no exile.
    Spec.assertEqWith s "and never asked WHICH card to exile" (filter isExileResponse transcript) []
  -- CR 404.1 / 404.2: "the TOP creature card". The Bird Maiden went to the
  -- graveyard second, so it is the one that leaves; the Piker underneath it
  -- stays. Nothing is prompted for, which is the claim the assertion on the
  -- Piker carries.
  Spec.it s "CR 404.2 with two creature cards it is the TOP one that is exiled" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    piker <- S.printingOf s registry "Goblin Piker"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (vulturesId, onStack) = vulturesUpkeep vultures [piker, birdMaiden]
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (S.onBattlefield vulturesId after) "the Vultures are still on the battlefield"
    Spec.assertEqWith s "the Bird Maiden, buried last, was exiled" (namesIn Zone.Exile S.alice after) [Just (S.printingName birdMaiden)]
    Spec.assertEqWith s "and the Piker under it is still in the graveyard" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "with two candidates, alice was still never asked which" (filter isExileResponse transcript) []

-- The battlefield objects whose current face carries this name. Used instead of
-- an id taken before the cast, since CR 400.7 mints a new object on the way in.
byNameOnBattlefield :: String -> GameState.GameState -> [ObjectId.ObjectId]
byNameOnBattlefield name gs =
  [ oid
  | oid <- Set.toList (GameState.battlefield gs),
    fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))
  ]

-- Fortress Kin-Guard cast from alice's hand off two Plains and resolved, with its
-- CR 603.6a enters trigger settled onto the stack but NOT resolved -- zombieUpkeep's
-- shape, so a case can read the board before the endure happens. `others` are put
-- on alice's battlefield first.
kinGuardOnStack :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> GameState.GameState
kinGuardOnStack plains kinGuard others =
  let base = List.foldl' (\g p -> snd (S.addCreature p S.alice g)) (S.landsInPlay plains 2) others
      (gs, spellId) = S.handOne kinGuard base
      cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
   in S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)

-- CR 701.63a's endure, which is CR 118.12a's "unless" over a cost that puts
-- counters rather than one that spends a resource: "that permanent's controller
-- creates an N/N white Spirit creature token UNLESS THEY PUT N +1/+1 COUNTERS ON
-- THAT PERMANENT" (CostComponent.PutPlusOneCountersOnThis). Fortress Kin-Guard
-- ({1}{W} 1/2 Creature -- Dog Soldier, "When this creature enters, it endures 1")
-- is the printing.
--
-- The first two cases start from the SAME board and the SAME settled trigger and
-- differ in NOTHING but alice's answer, Whipstitched Zombie's shape. Endure 1 on a
-- 1/2 keeps every reading distinct: 2/3 with the counter, 1/2 with the token, 3/4
-- under Hardened Scales.
fortressKinGuardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fortressKinGuardSpec s registry = Spec.describe s "FortressKinGuard" $ do
  let kinGuardOf = byNameOnBattlefield "Fortress Kin-Guard"
  Spec.it s "CR 701.63a paying the counters leaves the Kin-Guard a 2/3 and makes no token" $ do
    plains <- S.printingOf s registry "Plains"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    let onStack = kinGuardOnStack plains kinGuard []
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    case kinGuardOf onStack of
      [guardId] -> do
        -- The controls: the Kin-Guard really entered, and its trigger really
        -- reached the stack, before anything below is read.
        Spec.assertEqWith s "it entered as a 1/2 with no counters" (S.powerToughnessOf guardId onStack, S.counterOf CounterKind.PlusOnePlusOne guardId onStack) (Just (1, 2), 0)
        Spec.assertEqWith s "and its enters trigger is on the stack" (length (GameState.stack onStack)) 1
        Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
        Spec.assertEqWith s "CR 122.6: one +1/+1 counter went on" (S.counterOf CounterKind.PlusOnePlusOne guardId after) 1
        Spec.assertEqWith s "CR 613.4c: so it reads 2/3" (S.powerToughnessOf guardId after) (Just (2, 3))
        Spec.assertEqWith s "CR 118.12a: the paid branch made no Spirit" (S.tokensOf after) []
        Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
      other -> Spec.assertFailure s ("expected one Fortress Kin-Guard, got " <> show (length other))
  Spec.it s "CR 701.63a declining creates a 1/1 white Spirit and leaves the Kin-Guard a 1/2" $ do
    plains <- S.printingOf s registry "Plains"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    let onStack = kinGuardOnStack plains kinGuard []
        ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
    case kinGuardOf onStack of
      [guardId] -> do
        Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
        Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne guardId after) 0
        Spec.assertEqWith s "so it is still a 1/2" (S.powerToughnessOf guardId after) (Just (1, 2))
        case S.tokensOf after of
          [spiritId] -> do
            Spec.assertEqWith s "CR 111.4: the token is named Spirit Token" (fmap Face.name (Game.faceOf spiritId after)) (Just . CardName.MkCardName $ Text.pack "Spirit Token")
            Spec.assertEqWith s "a Creature" (Projection.cardTypesOf spiritId after) (Set.singleton CardType.Creature)
            Spec.assertEqWith s "with subtype Spirit" (Projection.subtypesOf spiritId after) (Set.singleton Subtype.Spirit)
            -- CR 202.2e: the token face carries a colour indicator, which is how
            -- "white" is spelled for an object with no mana cost.
            Spec.assertEqWith s "and white" (Projection.colorsOf spiritId after) (Set.singleton Color.White)
            Spec.assertEqWith s "endure 1 makes it 1/1" (S.powerToughnessOf spiritId after) (Just (1, 1))
            Spec.assertEqWith s "CR 111.2: alice created it, so alice controls it" (Projection.controllerOf spiritId after) (Just S.alice)
          other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
      other -> Spec.assertFailure s ("expected one Fortress Kin-Guard, got " <> show (length other))
  -- CR 614.16 over a cost paid DURING a resolution. The board differs from the
  -- first case in NOTHING but the Hardened Scales, and Hardened Scales does apply,
  -- because CR 118.12 pays this cost as the ability resolves and CR 609.1 makes
  -- what happens then an effect of that ability. A payment routed around the
  -- counter funnel reads 2/3 here.
  Spec.it s "CR 614.16 Hardened Scales sees endure's counter, so the Kin-Guard reads 3/4" $ do
    plains <- S.printingOf s registry "Plains"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    scales <- S.printingOf s registry "Hardened Scales"
    let onStack = kinGuardOnStack plains kinGuard [scales]
        after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
    case kinGuardOf onStack of
      [guardId] -> do
        Spec.assertEqWith s "it still entered as a 1/2" (S.powerToughnessOf guardId onStack) (Just (1, 2))
        Spec.assertEqWith s "one counter became two" (S.counterOf CounterKind.PlusOnePlusOne guardId after) 2
        Spec.assertEqWith s "so it reads 3/4" (S.powerToughnessOf guardId after) (Just (3, 4))
        Spec.assertEqWith s "and still no Spirit" (S.tokensOf after) []
      other -> Spec.assertFailure s ("expected one Fortress Kin-Guard, got " <> show (length other))
  -- CR 118.3 plus CR 701.63a's own ruling: "if you can't put +1/+1 counters on the
  -- creature for any reason (for example, if the creature is no longer on the
  -- battlefield), you'll just create a Spirit token." A Lightning Bolt kills the
  -- 1/2 while its endure trigger waits, and CR 113.7a resolves the trigger off a
  -- source that has left.
  --
  -- The interpreter PAYS wherever it is offered a cost, so an empty transcript
  -- says the prompt was never raised rather than that alice would have refused.
  Spec.it s "CR 118.3 a Kin-Guard that has left cannot pay, so the Spirit is created unasked" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = snd (S.addCreature mountain S.alice (S.landsInPlay plains 2))
        (withGuard, guardSpell) = S.handOne kinGuard base
        (boltSpell, withBolt) = S.addHandCard bolt S.alice withGuard
        cast = S.runPure S.identityAnswer withBolt (S.cast S.alice guardSpell)
        onStack = S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
        -- The Kin-Guard is the only creature on the board, so identityAnswer's
        -- lowest recipient is it; CR 704.5g then buries it in the settle.
        bolted = S.runPure S.identityAnswer onStack (S.cast S.alice boltSpell >> Stack.resolveTop >> Engine.settleForPriority)
        ((_, after), transcript) = Replay.record (paysFor S.alice) bolted Stack.resolveTop
    Spec.assertEqWith s "the Kin-Guard entered and its trigger is on the stack" (length (kinGuardOf onStack), length (GameState.stack onStack)) (1, 1)
    Spec.assertEqWith s "the Bolt killed it, and the endure trigger is still there" (length (kinGuardOf bolted), length (GameState.stack bolted)) (0, 1)
    Spec.assertEqWith s "CR 118.3: alice was never offered the counters" (payResponses transcript) []
    Spec.assertEqWith s "and the Spirit was created anyway" (length (S.tokensOf after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

-- The board both Magical Hack timing cases start from. alice has a Mountain --
-- added FIRST, so it holds the lowest object id and identityAnswer's
-- ChooseTargets (Set.lookupMin over the recipients) aims the Hack at it -- plus
-- an Island for the Hack's {U}; bob has three Islands for Cancel's {1}{U}{U} and
-- a Cancel in hand. Returns the Mountain, alice's Magical Hack and bob's Cancel
-- alongside the state.
hackBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hackBoard mountain island magicalHack cancel =
  let (mountainId, g1) = S.addCreature mountain S.alice (Setup.emptyGame S.bothPlayers)
      (_aliceIsland, g2) = S.addCreature island S.alice g1
      (g3, hackId) = S.handOne magicalHack g2
      g4 = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) g3 [1 :: Int .. 3]
      (cancelId, g5) = S.addHandCard cancel S.bob g4
   in (mountainId, hackId, cancelId, g5)

-- Hacks Mountain -> Island, and takes the identity fallback elsewhere (the liar
-- pattern). Deliberately unlike identityAnswer's Mountain -> Mountain, so a
-- test can tell an honoured answer from the fallback.
hackToIsland :: Prompt.Prompt r -> r
hackToIsland p = case p of
  Prompt.ChooseLandTypeSwap {} -> (Subtype.Mountain, Subtype.Island)
  _ -> S.identityAnswer p

-- The basic-land-type answers in a transcript, in order.
basicLandTypeResponses :: [Response.Response] -> [Response.Response]
basicLandTypeResponses = filter isBasicLandTypesResponse

isBasicLandTypesResponse :: Response.Response -> Bool
isBasicLandTypesResponse response = case response of
  Response.ChoseLandTypeSwap _ -> True
  _ -> False

-- CR 608.2d: Magical Hack's "replacing all instances of one basic land type
-- with another" is a choice its EFFECT offers, not one CR 601.2b-d makes as the
-- spell is cast, so it is announced while the effect is applied. The two cases
-- below are what makes cast-time and resolution-time binding distinguishable.
magicalHackTimingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magicalHackTimingSpec s registry = Spec.describe s "MagicalHackTiming" $ do
  Spec.it s "CR 608.2d a countered Magical Hack is never asked for its basic land types" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    magicalHack <- S.printingOf s registry "Magical Hack"
    cancel <- S.printingOf s registry "Cancel"
    let (_mountainId, hackId, cancelId, gs) = hackBoard mountain island magicalHack cancel
        exchange = do
          S.cast S.alice hackId
          S.cast S.bob cancelId
          Stack.resolveTop
        ((_, after), transcript) = Replay.record S.identityAnswer gs exchange
    -- The control: the exchange really happened. CR 701.6a puts the countered
    -- spell into its owner's graveyard, and CR 608.2n puts Cancel into bob's.
    Spec.assertEqWith s "Magical Hack countered into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "Cancel resolved into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
    -- And the point: a spell that never resolves never offers its effect's
    -- choice. Bound at cast, this list would hold one response.
    Spec.assertEqWith s "no basic land types were ever asked for" (basicLandTypeResponses transcript) []
  Spec.it s "CR 608.2d an uncountered Magical Hack is asked at resolution, and the swap applies" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    magicalHack <- S.printingOf s registry "Magical Hack"
    cancel <- S.printingOf s registry "Cancel"
    let (mountainId, hackId, _cancelId, gs) = hackBoard mountain island magicalHack cancel
        ((_, cast), castTranscript) = Replay.record hackToIsland gs (S.cast S.alice hackId)
        ((_, resolved), resolveTranscript) = Replay.record hackToIsland cast Stack.resolveTop
    Spec.assertEqWith s "the cast asked nothing about land types" (basicLandTypeResponses castTranscript) []
    Spec.assertEqWith
      s
      "the resolution asked exactly once"
      (basicLandTypeResponses resolveTranscript)
      [Response.ChoseLandTypeSwap (Subtype.Mountain, Subtype.Island)]
    -- CR 612 / 305.6: the answer is honoured, so the choice did not go missing
    -- when it moved. Mountain -> Island, not identityAnswer's Mountain ->
    -- Mountain, is what tells the two apart.
    Spec.assertEqWith s "the hacked Mountain projects Island" (Projection.subtypesOf mountainId resolved) (Set.singleton Subtype.Island)
    -- M0 determinism: the prompt moved, so the recorded stream has to still
    -- feed a replay of the same run back to the same state.
    let ((_, replayed), desync) = Replay.replay resolveTranscript cast Stack.resolveTop
    Spec.assertEqWith s "the resolution replays deterministically" replayed resolved
    Spec.assertEqWith s "and the transcript answered every prompt" desync Nothing

-- Aims every target slot at `oid` as an object (the SpellsAndPermanents pool's
-- recipient shape), and swaps `from` for `to` when the text-changer asks. Every
-- other prompt takes the identity fallback.
evolveAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
evolveAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseCreatureTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- Cast Turn to Frog at alice's Bog Wraith; optionally cast Artificial Evolution
-- at the Turn to Frog SPELL and resolve it, swapping the named creature type
-- words; then resolve the Turn to Frog. Returns the Wraith's id and the final
-- state.
turnToFrogChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, GameState.GameState)
turnToFrogChain s registry swap = do
  island <- S.printingOf s registry "Island"
  bogWraith <- S.printingOf s registry "Bog Wraith"
  turnToFrog <- S.printingOf s registry "Turn to Frog"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let (wraithId, g1) = S.addCreature bogWraith S.alice (S.landsInPlay island 3)
      (turnToFrogId, g2) = S.addHandCard turnToFrog S.alice g1
      (evolutionId, g3) = S.addHandCard artificialEvolution S.alice g2
      onStack = S.runPure (aimAtCreature wraithId) g3 (S.cast S.alice turnToFrogId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
  pure (wraithId, S.runPure S.identityAnswer evolved Stack.resolveTop)

-- Records the words a swap prompt says the new one may not be, so a test can
-- assert what the player was actually offered. Targets go to `oid` (a
-- text-changer aimed at a permanent needs no second card on the stack), and the
-- swap itself is answered with an identity on a word neither family forbids.
recordingForbidden :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State (Set.Set Subtype.Subtype) r
recordingForbidden oid p = case p of
  Prompt.ChooseCreatureTypeSwap _ _ _ _ forbidden -> do
    State.modify' (Set.union forbidden)
    pure (Subtype.Elf, Subtype.Elf)
  Prompt.ChooseLandTypeSwap _ _ _ _ forbidden -> do
    State.modify' (Set.union forbidden)
    pure (Subtype.Mountain, Subtype.Mountain)
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToObject oid))) sets)
  _ -> pure (S.identityAnswer p)

-- Cast Dragon Fodder; optionally cast Artificial Evolution at the Dragon Fodder
-- SPELL and resolve it, swapping the named creature type words; then resolve the
-- Fodder. Returns the tokens it minted and the final state.
--
-- Two Mountains and two Islands: the Fodder is {1}{R} and the Evolution {U}, and
-- the generic half may be paid from either colour without stranding the
-- Evolution.
dragonFodderChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m ([ObjectId.ObjectId], GameState.GameState)
dragonFodderChain s registry swap = do
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  dragonFodder <- S.printingOf s registry "Dragon Fodder"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let g1 = snd (S.addCreature island S.alice (snd (S.addCreature island S.alice (S.landsInPlay mountain 2))))
      (fodderId, g2) = S.addHandCard dragonFodder S.alice g1
      (evolutionId, g3) = S.addHandCard artificialEvolution S.alice g2
      onStack = S.runPure S.identityAnswer g3 (S.cast S.alice fodderId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      after = S.runPure S.identityAnswer evolved Stack.resolveTop
  pure (S.tokensOf after, after)

-- The permanent half of the same rule: alice controls Bitterblossom, optionally
-- has an Artificial Evolution resolved at IT (a permanent, not a spell), and then
-- her upkeep begins so the printed trigger fires and resolves. Returns the tokens
-- and the final state.
bitterblossomChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m ([ObjectId.ObjectId], GameState.GameState)
bitterblossomChain s registry swap = do
  island <- S.printingOf s registry "Island"
  bitterblossom <- S.printingOf s registry "Bitterblossom"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let (blossomId, g1) = S.addCreature bitterblossom S.alice (S.landsInPlay island 1)
      (evolutionId, g2) = S.addHandCard artificialEvolution S.alice g1
      evolved = case swap of
        Nothing -> g2
        Just (from, to) ->
          S.runPure (evolveAt blossomId from to) g2 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (evolved {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      onStack = S.runPure S.identityAnswer begun Engine.settleForPriority
      after = S.runPure S.identityAnswer onStack Engine.priorityLoop
  pure (S.tokensOf after, after)

-- Aims every target slot at `oid` as a creature (Turn to Frog's Pool.Creatures
-- recipient shape); the board holds more than one creature, so the choice has to
-- be answered rather than forced by construction.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- CR 612.2's OTHER half, end to end through the real engine: "a creature type
-- word used as a creature type".
--
-- Artificial Evolution {U} Instant -- "Change the text of target spell or
-- permanent by replacing all instances of one creature type with another. The
-- new creature type can't be Wall." (checked against Scryfall) -- is the card
-- that makes the difference observable, and Turn to Frog {1}{U} ("target
-- creature ... becomes a blue Frog with base power and toughness 1/1") is the
-- spell it rewrites: its SetCreatureSubtype names the Frog, so an Evolution
-- resolved at the spell on the stack has to make the target something else.
artificialEvolutionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
artificialEvolutionSpec s registry = Spec.describe s "ArtificialEvolution" $ do
  -- The control: with no Evolution the printed word stands, so this cannot pass
  -- vacuously on a chain that never got as far as resolving the Frog.
  Spec.it s "CR 205.1b whole card: an unevolved Turn to Frog still makes a Frog" $ do
    (wraithId, after) <- turnToFrogChain s registry Nothing
    Spec.assertEqWith s "Creature -- Frog" (Projection.subtypesOf wraithId after) (Set.singleton Subtype.Frog)

  -- And the point: the Evolution's word swap reaches the resolving spell's
  -- SetCreatureSubtype, so the Wraith becomes an Elf and never a Frog.
  Spec.it s "CR 612.2 whole card: Artificial Evolution on the Turn to Frog spell makes an Elf instead" $ do
    (wraithId, after) <- turnToFrogChain s registry (Just (Subtype.Frog, Subtype.Elf))
    Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf wraithId after) (Set.singleton Subtype.Elf)

  -- "The new creature type can't be Wall" is printed card text, so it travels
  -- with the card: the data says it, Effect.ChangeText carries it, and the
  -- prompt offers it. Nothing in the engine knows which card is asking.
  Spec.it s "CR 612 the Evolution's own restriction reaches the player being asked" $ do
    island <- S.printingOf s registry "Island"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    artificialEvolution <- S.printingOf s registry "Artificial Evolution"
    magicalHack <- S.printingOf s registry "Magical Hack"
    let (wraithId, g1) = S.addCreature bogWraith S.alice (S.landsInPlay island 3)
        forbiddenBy printing =
          let (spellId, g2) = S.addHandCard printing S.alice g1
              cast = do
                S.cast S.alice spellId
                Stack.resolveTop
           in State.execState (Engine.runGame (recordingForbidden wraithId) g2 cast) Set.empty
    Spec.assertEqWith s "the Evolution forbids Wall, and nothing else" (forbiddenBy artificialEvolution) (Set.singleton Subtype.Wall)
    -- The falsifier for "the engine hard-codes Wall somewhere": Magical Hack
    -- prints no restriction, so its swap forbids nothing.
    Spec.assertEqWith s "and the Hack forbids nothing" (forbiddenBy magicalHack) Set.empty

  -- CR 612.1's "any words or symbols printed on that object" reaches a
  -- text-changer's own restriction clause: Wall in "The new creature type can't
  -- be Wall" is a creature type word used as a creature type. Wizards' own
  -- Artificial Evolution ruling puts it plainly -- the swap "alters all
  -- occurrences of the chosen word in the text box and the type line of the
  -- given card" -- so one Evolution aimed at another leaves a spell whose
  -- restriction names the new word.
  Spec.it s "CR 612.1 an Evolution on an Evolution rewrites the restriction itself" $ do
    island <- S.printingOf s registry "Island"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    artificialEvolution <- S.printingOf s registry "Artificial Evolution"
    let (wraithId, g1) = S.addCreature bogWraith S.alice (S.landsInPlay island 3)
        (firstId, g2) = S.addHandCard artificialEvolution S.alice g1
        (secondId, g3) = S.addHandCard artificialEvolution S.alice g2
        onStack = S.runPure S.identityAnswer g3 (S.cast S.alice secondId)
        spellId = case GameState.stack onStack of
          top : _ -> top
          [] -> ObjectId.MkObjectId 999
        -- The first Evolution replaces the second's every Wall with Frog.
        evolved = S.runPure (evolveAt spellId Subtype.Wall Subtype.Frog) onStack $ do
          S.cast S.alice firstId
          Stack.resolveTop
        forbidden = State.execState (Engine.runGame (recordingForbidden wraithId) evolved Stack.resolveTop) Set.empty
    Spec.assertEqWith s "the evolved Evolution forbids Frog, not Wall" forbidden (Set.singleton Subtype.Frog)

  -- CR 612.2a, the SPELL half: "most spells and abilities that create creature
  -- tokens use creature types to define both the creature types and the names of
  -- the tokens. A text-changing effect that affects such a spell ... can change
  -- these words because they're being used as creature types, even though
  -- they're also being used as names." Dragon Fodder {1}{R} ("Create two 1/1 red
  -- Goblin creature tokens") is the spell; the Evolution is resolved at it on the
  -- stack.
  --
  -- The control first, so the pair cannot pass vacuously on a chain that never
  -- minted anything.
  Spec.it s "CR 111.4 an unevolved Dragon Fodder still mints two Goblins named Goblin Token" $ do
    (tokens, after) <- dragonFodderChain s registry Nothing
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Goblin" (Projection.subtypesOf oid after) (Set.singleton Subtype.Goblin)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Goblin Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Goblin Token")))) tokens

  -- And the point. BOTH halves of CR 612.2a: the type line, and the name those
  -- same words define.
  Spec.it s "CR 612.2a whole card: an evolved Dragon Fodder mints Elves, name and all" $ do
    (tokens, after) <- dragonFodderChain s registry (Just (Subtype.Goblin, Subtype.Elf))
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf oid after) (Set.singleton Subtype.Elf)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Elf Token")))) tokens

  -- CR 612.2a's OTHER half: "or an object with such an ability". Bitterblossom
  -- {1}{B} Kindred Enchantment -- Faerie ("At the beginning of your upkeep, you
  -- lose 1 life and create a 1/1 black Faerie Rogue creature token with flying",
  -- checked against Scryfall) is a PERMANENT whose triggered ability defines a
  -- token by creature type, so the Evolution reaches it through the printed
  -- ability rather than through a spell on the stack. Only the word the swap
  -- names moves: Rogue is untouched, in the type line and in the name alike.
  Spec.it s "CR 111.4 an unevolved Bitterblossom still mints a Faerie Rogue Token" $ do
    (tokens, after) <- bitterblossomChain s registry Nothing
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Faerie Rogue" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Faerie, Subtype.Rogue])) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Faerie Rogue Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Faerie Rogue Token")))) tokens

  Spec.it s "CR 612.2a whole card: an evolved Bitterblossom's trigger mints an Elf Rogue Token" $ do
    (tokens, after) <- bitterblossomChain s registry (Just (Subtype.Faerie, Subtype.Elf))
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf Rogue" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Elf, Subtype.Rogue])) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Rogue Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Elf Rogue Token")))) tokens

  -- The BOUNDARY the four tests above sit on, and the falsifier for reading them
  -- as "a text change rewrites names": CR 612.2's closing sentence -- "an effect
  -- that changes a color word or a subtype can't change a card name, even if
  -- that name contains a word or a series of letters that is the same as a Magic
  -- color word, basic land type, or creature type". Goblin Piker is Creature --
  -- Goblin Warrior and is NAMED "Goblin Piker", so it is the pool's one card
  -- where the coincidence is real. CR 612.2a's exception does not reach it --
  -- the Piker defines no token -- so the Evolution must make it an Elf Warrior
  -- still named Goblin Piker.
  --
  -- What this pins is the SCOPE of the exception: the swap reaches an object's
  -- name only through the card a Create defines, never through the projection of
  -- the object it is aimed at. Projection.rewriteCard's own gate -- the word must
  -- be a subtype of the card whose name it is rewriting -- has no card in this
  -- pool that makes it observable, since every token here is named after exactly
  -- its own subtypes (CR 111.4).
  Spec.it s "CR 612.2 an evolved Goblin Piker is an Elf Warrior still NAMED Goblin Piker" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    artificialEvolution <- S.printingOf s registry "Artificial Evolution"
    let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay island 1)
        (evolutionId, g2) = S.addHandCard artificialEvolution S.alice g1
        after = S.runPure (evolveAt pikerId Subtype.Goblin Subtype.Elf) g2 $ do
          S.cast S.alice evolutionId
          Stack.resolveTop
    Spec.assertEqWith s "Creature -- Elf Warrior" (Projection.subtypesOf pikerId after) (Set.fromList [Subtype.Elf, Subtype.Warrior])
    Spec.assertEqWith s "and the name is untouched" (Projection.namesOf pikerId after) (Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")))

-- The one activated ability of a printing that declares exactly one -- Prodigal
-- Sorcerer's {T}, which is all these fixtures reach for. Nothing for any other
-- printing, so a card that grew a second ability fails the case that names it
-- rather than silently picking whichever came first (Pawl.TargetSpec's
-- soleTargetSlot is the same shape for the same reason).
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
soleActivatedAbility p = case Face.activatedAbilities (S.combinedFace p) of
  [only] -> Just only
  _ -> Nothing

-- bob has a settled Prodigal Sorcerer ("{T}: This creature deals 1 damage to any
-- target"); alice has `lands` Islands and `stifles` Stifles in hand. bob
-- activates the Sorcerer at ALICE, so the ability's effect is observable as her
-- life total, and the returned state is the one where the ability is on the
-- stack, waiting.
stifleBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> Maybe ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
stifleBoard island stifle sorcerer lands stifles = case soleActivatedAbility sorcerer of
  Nothing -> Nothing
  Just ability ->
    let (srcId, withSorcerer) = S.addCreature sorcerer S.bob (Setup.emptyGame S.bothPlayers)
        -- CR 302.6: the Sorcerer must have settled under bob before its {T} may
        -- be activated at all.
        settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.bob)
        withLands = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) settled [1 .. lands]
        (stifleIds, withStifles) =
          List.foldl'
            (\(ids, g) _ -> let (i, g') = S.addHandCard stifle S.alice g in (ids <> [i], g'))
            ([], withLands)
            [1 .. stifles]
        atAlice :: Prompt.Prompt r -> r
        atAlice p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
          _ -> S.identityAnswer p
        activated = S.runPure atAlice (withStifles {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob srcId ability)
     in Just (stifleIds, srcId, activated)

-- CR 701.6a covers "a spell or ability", and Stifle ({U} Instant, "Counter
-- target activated or triggered ability. (Mana abilities can't be targeted.)")
-- is the first card in the pool that reaches the second half. Cancel proved the
-- spell half above; these cases are the ability half, and what makes them a
-- different test rather than the same one twice is rule 701.6a's LAST sentence:
-- "a countered spell is put into its owner's graveyard." Only a spell. CR 608.2n
-- says how an ability leaves instead -- "the ability is removed from the stack
-- and ceases to exist" -- so the graveyard assertions here are the load-bearing
-- ones, and they are stated as counts of what did NOT arrive.
--
-- CR 113.9 is why one card cannot do both: "activated and triggered abilities on
-- the stack aren't spells, and therefore can't be countered by anything that
-- counters only spells. Activated and triggered abilities on the stack can be
-- countered by effects that specifically counter abilities." Pawl.TargetSpec
-- holds that half, as the two disjoint pools.
--
-- Stifle's parenthetical needs nothing implemented and is not tested for: CR
-- 605.3b ("an activated mana ability doesn't go on the stack, so it can't be
-- targeted, countered, or otherwise responded to") and CR 605.4a keep a mana
-- ability off the stack in the first place, so it is never a candidate. See
-- Pawl.Types.Pool.Abilities.
stifleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stifleSpec s registry = Spec.describe s "Stifle" $ do
  -- The ACTIVATED half (CR 113.3b). The discriminating assertions are alice's
  -- untouched life -- rule 701.6a's "it doesn't resolve and none of its effects
  -- occur" -- and bob's EMPTY graveyard, which is what tells a cease (CR 608.2n)
  -- apart from the graveyard move Cancel makes.
  Spec.it s "CR 701.6a whole cards: Stifle counters Prodigal Sorcerer's activated ability, which ceases (CR 608.2n)" $ do
    island <- S.printingOf s registry "Island"
    stifle <- S.printingOf s registry "Stifle"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    case stifleBoard island stifle sorcerer 1 1 of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (stifleIds, srcId, activated) -> do
        let abilIds = GameState.stack activated
            cast = List.foldl' (\g oid -> S.runPure S.identityAnswer g (S.cast S.alice oid)) activated stifleIds
            countered = S.runPure S.identityAnswer cast Stack.resolveTop
        Spec.assertEqWith s "the activation put exactly one ability on the stack" (length abilIds) 1
        Spec.assertEqWith s "and both it and the Stifle are gone from the stack" (GameState.stack countered) []
        -- CR 701.6a: "it doesn't resolve and none of its effects occur."
        Spec.assertEqWith s "alice took no damage: the ability never resolved" (S.lifeOf S.alice countered) (Just 20)
        -- CR 608.2n: the ability ceased. It is not in a graveyard -- an ability
        -- is not a card and has no owner's graveyard to be put into -- and it is
        -- not an object at all any more.
        Spec.assertEqWith s "nothing arrived in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob countered)) 0
        Spec.assertEqWith s "alice's graveyard holds the spent Stifle and nothing else" (length (Game.zoneMembers Zone.Graveyard S.alice countered)) 1
        Spec.assertEqWith s "the ability object ceased to exist" (fmap (\oid -> Game.lookupObject oid countered) abilIds) [Nothing]
        -- CR 113.7a: the ability was its own object, so countering it leaves the
        -- SOURCE alone -- and CR 701.6b gives no refund, so the Sorcerer stays
        -- tapped for a {T} that bought nothing.
        Spec.assertBool s (Set.member srcId (GameState.battlefield countered)) "the Prodigal Sorcerer is untouched on the battlefield"
        Spec.assertEqWith s "still tapped: CR 701.6b refunds no cost" (fmap Object.tapped (Game.lookupObject srcId countered)) (Just TapState.Tapped)
  -- The TRIGGERED half (CR 113.3c), and a different observation: Aether Flash's
  -- trigger is what kills a Goblin Piker in Pawl.TriggerSpec's own case, so the
  -- Piker being ALIVE with no damage marked is the same effect not occurring.
  Spec.it s "CR 701.6a whole cards: Stifle counters Aether Flash's triggered ability, so the Piker lives" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    aetherFlash <- S.printingOf s registry "Aether Flash"
    piker <- S.printingOf s registry "Goblin Piker"
    stifle <- S.printingOf s registry "Stifle"
    let (flashId, withFlash) = S.addCreature aetherFlash S.alice (Setup.emptyGame S.bothPlayers)
        withMountains = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) withFlash [1 .. (2 :: Int)]
        (_, withIsland) = S.addCreature island S.bob withMountains
        (stifleId, withStifle) = S.addHandCard stifle S.bob withIsland
        (pikerId, gs) = S.addHandCard piker S.alice withStifle
        cast = S.runPure S.identityAnswer gs (S.cast S.alice pikerId)
        -- The Piker resolves and enters; CR 603.3 then puts Aether Flash's
        -- trigger on the stack the next time a player would receive priority.
        entered = S.runPure S.identityAnswer cast Stack.resolveTop
        placed = S.runPure S.identityAnswer entered Engine.settleForPriority
        stifled = S.runPure S.identityAnswer placed (S.cast S.bob stifleId)
        countered = S.runPure S.identityAnswer stifled Stack.resolveTop
        after = S.runPure S.identityAnswer countered Engine.settleForPriority
        entrantId = case filter (\oid -> fmap Face.name (Game.faceOf oid after) == Just (CardName.MkCardName $ Text.pack "Goblin Piker")) (Set.toList (GameState.battlefield after)) of
          [only] -> Just only
          _ -> Nothing
    Spec.assertEqWith s "the trigger is the only thing on the stack before the Stifle" (length (GameState.stack placed)) 1
    Spec.assertEqWith s "the stack is empty afterwards" (GameState.stack after) []
    -- The falsifier is Pawl.TriggerSpec's aetherFlashSpec, where the same
    -- Aether Flash's 2 damage kills the same 2/1 (CR 704.5g): a Piker alive with
    -- NO damage marked is rule 701.6a's "none of its effects occur".
    Spec.assertEqWith s "the Piker survived" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice after) 1
    Spec.assertEqWith s "with no damage marked on it at all" (fmap (\oid -> fmap Object.damage (Game.lookupObject oid after)) entrantId) (Just (Just 0))
    Spec.assertEqWith s "no damage was ever dealt" (fmap DamageEvent.amount (Maybe.mapMaybe Event.damageOf (S.eventsOf after))) []
    -- CR 608.2n again: the countered trigger went nowhere. alice's graveyard is
    -- empty (no Piker corpse, and no residue of the trigger), and bob's holds
    -- only the Stifle that did the countering.
    Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "bob's holds the spent Stifle alone" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertBool s (Set.member flashId (GameState.battlefield after)) "and Aether Flash itself is untouched"
  -- CR 608.2b, for a target that CEASED rather than moved: "a target that's no
  -- longer in the zone it was in when it was targeted is illegal ... If all its
  -- targets ... are now illegal, the spell or ability doesn't resolve. It's
  -- removed from the stack and, IF IT'S A SPELL, put into its owner's
  -- graveyard." Stifle is a spell, so the fizzled one is buried; the ability it
  -- was aimed at left by ceasing, which is not a zone change at all.
  --
  -- The twin of the racing Cancels above, one card over.
  Spec.it s "CR 608.2b a Stifle whose ability already ceased fizzles" $ do
    island <- S.printingOf s registry "Island"
    stifle <- S.printingOf s registry "Stifle"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    case stifleBoard island stifle sorcerer 2 2 of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (stifleIds, _, activated) -> do
        let castAll g oid = S.runPure S.identityAnswer g (S.cast S.alice oid)
            bothCast = List.foldl' castAll activated stifleIds
            first' = S.runPure S.identityAnswer bothCast Stack.resolveTop
            second' = S.runPure S.identityAnswer first' Stack.resolveTop
        Spec.assertEqWith s "two Stifles were cast onto the ability" (length (GameState.stack bothCast)) 3
        Spec.assertEqWith s "the first counters the ability" (length (GameState.stack first')) 1
        Spec.assertEqWith s "and the second fizzles off the stack" (GameState.stack second') []
        Spec.assertEqWith s "both Stifles are in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice second')) 2
        Spec.assertEqWith s "bob's graveyard stayed empty throughout" (length (Game.zoneMembers Zone.Graveyard S.bob second')) 0
        Spec.assertEqWith s "and alice never took the damage" (S.lifeOf S.alice second') (Just 20)

fizzleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fizzleSpec s registry = Spec.describe s "Fizzle" $ do
  Spec.it s "CR 608.2b Bolt-vs-Bolt through the priority loop: the second fizzles" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let after = snd (Engine.runGamePure boltAnswer (twoBoltState piker mountain lightningBolt) Engine.priorityLoop)
    Spec.assertEqWith s "stack cleared" (length (GameState.stack after)) 0
    Spec.assertEqWith s "Piker dead" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "both Bolts in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
    Spec.assertEqWith s "the Piker in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "bob's life untouched: the fizzled Bolt hit nothing" (S.lifeOf S.bob after) (Just 20)
  -- CR 608.2b pins the `targeted` restriction Task 3 added (Resolve.hs's
  -- resolveEffects/resolveSpell): a reserved slot (Binding.triggerSource)
  -- is vacuously legal, since CR 608.2b is about TARGETS and a reserved
  -- slot was never one -- but its vacuous legality must not rescue a
  -- fizzle whose one genuinely-targeted slot IS illegal. This needs an
  -- ability with BOTH kinds of slot at once, plus a second, targetless
  -- effect (Draw) whose execution is the only way to observe whether the
  -- fizzle happened: with a single targeted slot alone, fizzling and
  -- resolving-with-the-slot-skipped are indistinguishable (Destroy's own
  -- per-slot legality check already no-ops it either way).
  Spec.it s "CR 608.2b the reserved trigger-source slot does not rescue a fizzle: the targetless Draw after the ability's only real target dies does not run" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    forest <- S.printingOf s registry "Forest"
    let base0 = Setup.emptyGame S.bothPlayers
        (source, base1) = S.addCreature piker S.alice base0
        (victim, base2) = S.addCreature piker S.bob base1
        (_, base3) = S.addLibraryCard forest S.alice base2
        handBefore = S.handSize S.alice base3
        targetSlot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton targetSlot (TargetSlot.required Pool.Creatures Nothing)
        (abilId, base4) = S.spellOnStack piker S.alice base3
        -- Mirrors Engine.placeOne's own construction: a real chosen
        -- target under `targetSlot`, plus the reserved self slot every
        -- placed trigger carries (Binding.setTriggerSource).
        bindings =
          Binding.setTriggerSource
            source
            (Binding.fromChoices (Map.singleton targetSlot (Set.singleton (Recipient.ToCreature victim))) Nothing Seq.empty)
        withBindings = base4 {GameState.objects = Map.adjust (\o -> o {Object.bindings = bindings}) abilId (GameState.objects base4)}
        -- Kill the sole real target before resolution: CR 608.2b makes it
        -- illegal (it's no longer a legal CreatureTarget), while the
        -- reserved slot -- never targeted -- stays vacuously legal.
        gone = S.runPure S.identityAnswer withBindings (Event.changeZone victim Zone.Graveyard)
        mode = Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot targetSlot) Regenerability.Regenerable Nothing), Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))]))) slots
        run = Resolve.resolveModes abilId source [(ModeInstance.MkModeInstance (ModeIndex.MkModeIndex 0) 0, mode)]
        after = snd (Engine.runGamePure S.identityAnswer gone run)
    Spec.assertEqWith s "the targetless Draw did not run: the ability fizzled" (S.handSize S.alice after) handBefore
  Spec.it s "CR 704.5a a Bolt can end the game mid-step" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        lowBob =
          gs {GameState.players = Map.adjust (\pl -> pl {Player.life = 3}) S.bob (GameState.players gs)}
        atBob :: Prompt.Prompt r -> r
        atBob p = case p of
          Prompt.ChooseTargets _ _ _ sets ->
            fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          Prompt.ChooseAction _ _ actions ->
            case filter (S.isCastOf oid) actions of
              h : _ -> h
              [] -> A.Pass
          _ -> S.identityAnswer p
        after = snd (Engine.runGamePure atBob lowBob Engine.priorityLoop)
    Spec.assertEqWith s "alice wins" (GameState.result after) (Just (Result.Won S.alice))
    Spec.assertEqWith s "the loop released priority" (GameState.priority after) Nothing

indestructibleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
indestructibleSpec s registry = Spec.describe s "Indestructible" $ do
  Spec.it s "CR 704.5g an indestructible creature survives lethal marked damage" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myrId, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        -- Myr is 0/1; 3 marked damage is lethal (704.5g) but indestructible saves it.
        after = S.settleSba (S.markDamage myrId 3 gs)
    Spec.assertEqWith s "Myr still on the battlefield" (S.creaturesInPlay S.bob after) 1
    Spec.assertEqWith s "Myr not in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
  Spec.it s "CR 704.5h an indestructible creature survives deathtouch" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myrId, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        -- Zero marked damage (so 704.5g is silent) plus a deathtouch event isolates
        -- the 704.5h path; indestructible must guard it too (CR 700.4).
        wounded = S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True False False 0 Nothing DamageKind.Combat)] gs
        after = S.settleSba wounded
    Spec.assertEqWith s "Myr survives deathtouch" (S.creaturesInPlay S.bob after) 1
  Spec.it s "CR 704.5f indestructible does NOT save a creature with toughness <= 0" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myrId, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        -- A real -1/-1 counter drops Myr (0/1) to 0/0 (CR 122.1a); 704.5f is a
        -- put-into-graveyard, not a destroy, so indestructible does not apply
        -- (Myr's own reminder text).
        zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 myrId gs
        after = S.settleSba zeroed
    Spec.assertEqWith s "Myr left the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Myr in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
  Spec.it s "CR 704.5f regeneration does NOT save a creature with toughness <= 0" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (victim, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers) -- 2/1
    -- A real -1/-1 counter drops the toughness to 0 (CR 122.1a); 704.5f is a
    -- put-into-graveyard, not a destruction, so a regeneration shield cannot
    -- save it.
        zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs
        shielded = S.addRegenShield victim zeroed
        after = S.settleSba shielded
    Spec.assertEqWith s "died despite the shield (704.5f is not a destruction)" (S.creaturesInPlay S.bob after) 0

-- alice controls `n` Swamps and holds `printing` in a main phase with priority;
-- bob controls one `foe`. Returns (foe's id, post-cast-and-resolve state).
castBlackRemovalAt :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
castBlackRemovalAt swamp printing foe =
  let base = S.landsInPlay swamp 3
      (foeId, withFoe) = S.addCreature foe S.bob base
      (gs, spellId) = S.handOne printing withFoe
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (foeId, resolved)

-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- atBobAnswer's creature counterpart: aim every target slot at one named
-- creature, rather than at whatever Set.lookupMin happens to offer first.
atCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
atCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- Add k cards of a printing to pid's hand (each a fresh Hand-zone object).
handCards :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
handCards printing pid k gs = List.foldl' (\g _ -> addOne g) gs [1 .. k]
  where
    addOne g =
      let (oid, g1) = Game.freshObjectId g
          obj = Object.MkObject pid Nothing (Source.OfCard printing) Zone.Hand TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled pid) Map.empty Map.empty Nothing Nothing Nothing Set.empty (Timestamp.MkTimestamp 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty False
       in g1
            { GameState.objects = Map.insert oid obj (GameState.objects g1),
              GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand g1)
            }

-- Put k cards of a printing into pid's library, each on top of the last, for a
-- draw to find.
stockLibrary :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
stockLibrary printing pid k gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. k]

-- alice's upkeep begins, settled to the point where any trigger it woke is on
-- the stack (CR 603.3b) waiting to resolve.
settleAtAlicesUpkeep :: GameState.GameState -> GameState.GameState
settleAtAlicesUpkeep gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)

-- Who drew, in the order they drew, read off the turn-scoped event log. CR
-- 121.1 makes a draw one library-to-hand move, and a library and a hand each
-- belong to one player, so the moved card's owner is the drawer. Any OTHER route
-- from library to hand would count here too; no fixture below has one.
drawersOf :: GameState.GameState -> [PlayerId.PlayerId]
drawersOf gs = Maybe.mapMaybe drawer (S.zoneChangesOf gs)
  where
    drawer zc =
      if ZoneChange.from zc == Zone.Library && ZoneChange.to zc == Zone.Hand
        then fmap Object.owner (Game.lookupObject (ZoneChange.object zc) gs)
        else Nothing

zoneChangeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
zoneChangeSpec s registry = Spec.describe s "ZoneChange" $ do
  Spec.it s "CR 701.8 Murder destroys a normal creature into its owner's graveyard" $ do
    swamp <- S.printingOf s registry "Swamp"
    murder <- S.printingOf s registry "Murder"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, after) = castBlackRemovalAt swamp murder piker
    Spec.assertEqWith s "no creature survives" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Piker in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
  Spec.it s "CR 700.4 Murder does nothing to an indestructible creature (destroy /= move)" $ do
    swamp <- S.printingOf s registry "Swamp"
    murder <- S.printingOf s registry "Murder"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (_, after) = castBlackRemovalAt swamp murder darksteelMyr
    -- The falsifier: modelling Destroy as MoveToZone slot Graveyard would
    -- bury the Myr. It stays; the spell still resolved and was buried.
    Spec.assertEqWith s "Myr still on the battlefield" (S.creaturesInPlay S.bob after) 1
    Spec.assertEqWith s "bob's graveyard empty" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "Murder in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 701.19a Murder is replaced by regeneration" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    murder <- S.printingOf s registry "Murder"
    let base = S.landsInPlay swamp 3
        (victim, withFoe) = S.addCreature piker S.bob base
        shielded = S.addRegenShield victim withFoe
        (gs, spellId) = S.handOne murder shielded
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "the shielded creature survived Murder" (S.creaturesInPlay S.bob after) 1
  Spec.it s "CR 400.7 Unsummon returns a creature to its owner's hand" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (_, withPiker) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne unsummon withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "no creature on the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "a card in bob's hand (its owner)" (S.handSize S.bob after) 1
    Spec.assertEqWith s "Unsummon in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 701.19a regeneration does not save a bounced creature" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (victim, withFoe) = S.addCreature piker S.bob base
        shielded = S.addRegenShield victim withFoe
        (gs, spellId) = S.handOne unsummon shielded
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the creature left the battlefield (bounce is not a destruction)" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "it is in bob's hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
  Spec.it s "CR 701.13 Angelic Edict exiles a target creature" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let base = S.landsInPlay plains 5
        (_, withPiker) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne angelicEdict withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "no creature on the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "one card in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
  Spec.it s "CR 115 Angelic Edict may exile an enchantment (non-creature permanent)" $ do
    plains <- S.printingOf s registry "Plains"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let base = S.landsInPlay plains 5
        -- bob controls only Rest in Peace (an enchantment, not a creature), so
        -- it is the single legal CreatureOrEnchantmentTarget.
        (ripId, withRip) = S.addCreature restInPeace S.bob base
        (gs, spellId) = S.handOne angelicEdict withRip
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the enchantment left the battlefield" (Game.lookupObject ripId after) Nothing
    Spec.assertEqWith s "one card in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
  Spec.it s "CR 121.1 Divination draws its controller two cards" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    divination <- S.printingOf s registry "Divination"
    let base = S.landsInPlay island 3
        (_, g1) = S.addLibraryCard piker S.alice base
        (_, g2) = S.addLibraryCard piker S.alice g1
        (gs, spellId) = S.handOne divination g2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "two cards drawn to hand" (S.handSize S.alice after) 2
    Spec.assertEqWith s "library emptied" (Game.zoneMembers Zone.Library S.alice after) []
  Spec.it s "CR 121.4 a Draw that outruns the library records the loss" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    divination <- S.printingOf s registry "Divination"
    let base = S.landsInPlay island 3
        (_, g1) = S.addLibraryCard piker S.alice base
        (gs, spellId) = S.handOne divination g1
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty after)) "drewFromEmpty marked"
  -- The card that proves Effect.Draw's recipient (#272): CR 121.1 says who
  -- draws, and here that is the player the spell TARGETS (CR 601.2c), not
  -- the controller who paid for it. Divination above is the same opcode
  -- pointed at `Relative You`; the two together are the falsifier for a
  -- Draw that always drew for its controller.
  Spec.it s "CR 121.1 Ancestral Recall draws three cards for the player it targets, not its controller" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    ancestralRecall <- S.printingOf s registry "Ancestral Recall"
    let base = S.landsInPlay island 1
        withLib = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.bob g)) base [1 .. (4 :: Int)]
        (gs, spellId) = S.handOne ancestralRecall withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "three cards drawn to bob's hand" (S.handSize S.bob after) 3
    Spec.assertEqWith s "one card left in bob's library" (length (Game.zoneMembers Zone.Library S.bob after)) 1
    Spec.assertEqWith s "alice drew nothing" (S.handSize S.alice after) 0
  -- The card that proves Effect.Draw's `EachPlayer` arm (#276). Divination
  -- above draws for the controller alone and Ancestral Recall for one named
  -- player; Vision Skeins is the first Draw in the pool that reaches the
  -- whole table at once.
  Spec.it s "CR 121.1 Vision Skeins draws two cards for each player, its caster included" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let base = S.landsInPlay island 2
        withLibs = stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 base)
        (gs, spellId) = S.handOne visionSkeins withLibs
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew two" (S.handSize S.alice after) 2
    Spec.assertEqWith s "bob drew two as well" (S.handSize S.bob after) 2
    Spec.assertEqWith s "no draw outran a library" (GameState.drewFromEmpty after) Set.empty
  -- CR 121.2c: "If more than one player is instructed to draw cards, the
  -- active player performs all of their draws first, then each other player
  -- in turn order does the same." The seat order the players map answers in
  -- is not that order, so this needs an active player who is not the first
  -- seat: alice casts an INSTANT on BOB's turn, which makes seat order
  -- [alice, bob, carol] and turn order [bob, carol, alice] disagree.
  --
  -- The draws are read back off the turn-scoped event log -- the same log a
  -- trigger scans (CR 603.2) -- because that is where the order of the
  -- individual draws is observable; the hand sizes alone are order-blind.
  Spec.it s "CR 121.2c Vision Skeins draws for the active player first, then in turn order" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let -- S.landsInPlay builds its own two-seat game, so the {1}{U} goes on
        -- a three-seat board one Island at a time.
        withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        -- handOne hands alice the turn along with the card, so bob takes the
        -- turn back. Cast.castSpell gates neither timing nor priority, but
        -- the fixture is a legal board regardless: Vision Skeins is an
        -- INSTANT, which alice may cast on bob's turn.
        gs = gs0 {GameState.activePlayer = S.bob}
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "bob (active) draws both of his, then carol, then the caster"
      (drawersOf after)
      [S.bob, S.bob, S.carol, S.carol, S.alice, S.alice]
    Spec.assertEqWith s "and everyone holds two" (fmap (\pid -> S.handSize pid after) [S.alice, S.bob, S.carol]) [2, 2, 2]
  -- The card that proves Effect.Draw's `Relative Opponent` arm (#276), and
  -- the one shape no "you draw" card can stand in for: Master of the Feast's
  -- trigger is a DRAWBACK, drawing for everyone except the player who
  -- controls it (CR 109.5 makes "your upkeep" that controller's).
  Spec.it s "CR 121.1 Master of the Feast's upkeep trigger draws for the opponent, not its controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addCreature masterOfTheFeast S.alice (Setup.emptyGame S.bothPlayers)
        withLibs = stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board)
        onStack = settleAtAlicesUpkeep withLibs
        after = snd (Engine.runGamePure S.identityAnswer onStack Stack.resolveTop)
    Spec.assertBool s (not (null (GameState.stack onStack))) "the upkeep trigger really reached the stack"
    Spec.assertEqWith s "bob drew" (S.handSize S.bob after) 1
    Spec.assertEqWith s "alice, who controls it, did not" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and alice's library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  -- The discriminator, and it needs a THIRD seat: at two players an
  -- `Opponent` arm that reached only ONE opponent is indistinguishable from
  -- one that reaches them all. CR 806.1: in a Free-for-All the players
  -- compete as individuals, so every other player is an opponent (CR 102.3's
  -- teammates are the one exception, and pawl has no teams, #175) and both
  -- of them draw.
  Spec.it s "CR 806.1 at three seats each opponent draws off Master of the Feast, and only opponents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addCreature masterOfTheFeast S.alice S.threePlayerGame
        withLibs = stockLibrary piker S.carol 1 (stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board))
        after = snd (Engine.runGamePure S.identityAnswer (settleAtAlicesUpkeep withLibs) Stack.resolveTop)
    -- A drawer whose library was empty would draw no card and so record no
    -- zone change; this is what keeps the list below honest about that.
    Spec.assertEqWith s "no draw outran a library" (GameState.drewFromEmpty after) Set.empty
    Spec.assertEqWith s "both opponents drew, and the controller did not" (drawersOf after) [S.bob, S.carol]
  -- CR 102.1: "A player is one of the people in the game", so once CR 800.4a
  -- takes carol out, `EachPlayer` stops naming her (#279). It needs three
  -- seats twice over: CR 800.4 says only a multiplayer game -- CR 800.1's,
  -- one that BEGAN with more than two players -- continues after a
  -- departure, and a two-seat game would already have ended under CR 104.2a
  -- with nothing left to resolve.
  --
  -- drewFromEmpty is what makes this observable rather than merely tidy.
  -- CR 800.4a took carol's library out of the game with her, so a draw aimed
  -- at her finds it empty and Event.drawCard writes her seat into that set --
  -- engine state recorded for someone who is not in the game.
  Spec.it s "CR 800.4a Vision Skeins does not draw for a player who has left the game" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        gs = Departure.depart Departure.Type.Conceded S.carol gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the two players still in the game drew, in APNAP order" (drawersOf after) [S.alice, S.alice, S.bob, S.bob]
    Spec.assertEqWith s "and nothing was drawn against carol's departed library" (GameState.drewFromEmpty after) Set.empty
  Spec.it s "CR 701.17 Tome Scour mills five from a target player's library" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    tomeScour <- S.printingOf s registry "Tome Scour"
    let base = S.landsInPlay island 1
        withLib = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.bob g)) base [1 .. (6 :: Int)]
        (gs, spellId) = S.handOne tomeScour withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "five milled to graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 5
    Spec.assertEqWith s "one card left in library" (length (Game.zoneMembers Zone.Library S.bob after)) 1
  Spec.it s "CR 701.17b milling a short library mills fewer with no loss" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    tomeScour <- S.printingOf s registry "Tome Scour"
    let base = S.landsInPlay island 1
        (_, g1) = S.addLibraryCard piker S.bob base
        (_, g2) = S.addLibraryCard piker S.bob g1
        (gs, spellId) = S.handOne tomeScour g2
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "two milled" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertBool s (not (Set.member S.bob (GameState.drewFromEmpty after))) "bob did not lose (milling is not drawing)"
  Spec.it s "CR 701.9 Mind Rot discards two chosen cards from a hand of three" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one card left in bob's hand" (S.handSize S.bob after) 1
    Spec.assertEqWith s "two cards in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
  Spec.it s "CR 609.3 a forced full-hand discard is not prompted" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 2 base
        (gs, spellId) = S.handOne mindRot withHand
        -- Answer ChooseDiscard with [] so a prompt would discard nothing;
        -- aim the spell at bob.
        noDiscard q = case q of
          Prompt.ChooseDiscard {} -> []
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure noDiscard gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure noDiscard cast Stack.resolveTop)
    -- Elision (hand == count): the whole hand is discarded without asking (#63).
    Spec.assertEqWith s "bob's hand emptied" (S.handSize S.bob after) 0
    Spec.assertEqWith s "both cards discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
  -- The three below are about the PROMPTED branch -- hand of three, discard
  -- two -- where the elision above does not apply and the answer is a real
  -- choice. Mind Rot is not "may", and CR 609.3's "as much as possible" caps
  -- nothing here (the hand is larger than the count), so every card an answer
  -- omits is one the player could have discarded.
  Spec.it s "CR 701.9b an empty ChooseDiscard answer still discards the full count" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        noDiscard q = case q of
          Prompt.ChooseDiscard {} -> []
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure noDiscard gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure noDiscard cast Stack.resolveTop)
    Spec.assertEqWith s "two discarded despite the answer naming none" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "one card left in bob's hand" (S.handSize S.bob after) 1
  Spec.it s "CR 701.9b a valid pick is honoured and only the shortfall is completed" $ do
    -- Discriminating against "ignore the answer and take the first n": the
    -- answer names the LAST card in hand, which a first-n completion would
    -- leave behind.
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        onlyLast q = case q of
          Prompt.ChooseDiscard _ _ ids _ -> take 1 (reverse ids)
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure onlyLast gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure onlyLast cast Stack.resolveTop)
    case reverse (Game.zoneMembers Zone.Hand S.bob cast) of
      [] -> Spec.assertFailure s "fixture should leave bob a hand to discard from"
      lastCard : _ -> do
        Spec.assertEqWith s "two discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
        Spec.assertBool s (List.notElem lastCard (Game.zoneMembers Zone.Hand S.bob after)) "and the card the answer named is one of them"
  Spec.it s "CR 701.9b naming the same card twice fills one slot, not two" $ do
    -- ChooseDiscard is answered with a LIST, so unlike ChooseSacrifices'
    -- Set the duplicate has to be removed here or it discards one card short.
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        sameTwice q = case q of
          Prompt.ChooseDiscard _ _ ids _ -> concat (replicate 2 (take 1 ids))
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure sameTwice gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure sameTwice cast Stack.resolveTop)
    Spec.assertEqWith s "two distinct cards discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "one card left in bob's hand" (S.handSize S.bob after) 1

-- Griptide is "Put target creature on top of its owner's library", the pool's
-- producer for a library arrival that is NOT the bottom (#989). Everything the
-- group asserts is one card's worth of rules: CR 400.3 picks the library (its
-- OWNER's, not the caster's), CR 400.7 mints the incarnation that lands in it,
-- and CR 401.2 keeps the order a thing only the position can decide.
libraryPositionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
libraryPositionSpec s registry = Spec.describe s "LibraryPosition" $ do
  -- Three cards deep, top to bottom, because with ONE card the top and the
  -- bottom are the same index and the two positions are indistinguishable; and
  -- aimed at BOB's creature, because against her own "its owner's library" and
  -- "the caster's library" would name the same library.
  --
  -- Three is also deep enough that the draw below is an ordinary draw rather
  -- than CR 104.3c's loss: bob draws one of four.
  Spec.it s "CR 400.3 / 401.2 whole card: Griptide puts the creature on TOP of its OWNER's library, and its owner draws it" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    griptide <- S.printingOf s registry "Griptide"
    -- S.addLibraryCard puts each card ON TOP, so the LAST seeded is the head.
    let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay island 4)
        (deepId, g2) = S.addLibraryCard bolt S.bob g1
        (middleId, g3) = S.addLibraryCard bolt S.bob g2
        (oldTopId, g4) = S.addLibraryCard bolt S.bob g3
        (gs, spellId) = S.handOne griptide g4
        aimAtPiker :: Prompt.Prompt r -> r
        aimAtPiker p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure aimAtPiker gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure aimAtPiker cast Stack.resolveTop)
        -- CR 400.7 mints a FRESH id at the destination, so asserting the
        -- battlefield id is gone is not the same claim as asserting the new one
        -- is on top. Both are made.
        bobsLibrary = Game.zoneMembers Zone.Library S.bob after
    Spec.assertBool s (not (S.onBattlefield pikerId after)) "the creature left the battlefield"
    Spec.assertEqWith s "bob's library grew by exactly one" (length bobsLibrary) 4
    Spec.assertEqWith s "alice's library is untouched" (Game.zoneMembers Zone.Library S.alice after) []
    case bobsLibrary of
      arrived : rest -> do
        Spec.assertEqWith
          s
          "and the card at INDEX 0 is the returned creature"
          (fmap S.nameOf (Game.cardOf arrived after))
          (Just (CardName.MkCardName (Text.pack "Goblin Piker")))
        Spec.assertEqWith s "with the previous top card now at index 1" rest [oldTopId, middleId, deepId]
        -- What makes the position OBSERVABLE rather than an internal detail:
        -- CR 121.1's draw puts "the top card of their library" into the hand, so
        -- a Piker in bob's hand is the rule and a Bolt is the bottom-of-library
        -- behaviour this closes.
        let drawn =
              S.runPure aimAtPiker after $ do
                State.modify' $ \g -> g {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
                S.drawStep
        -- By NAME, not by id: the draw is itself a zone change, so CR 400.7
        -- mints a second incarnation and the card in hand is not `arrived`
        -- either. bob's library holds nothing but Bolts, so the name is what
        -- tells the returned creature from the card that was on top before it.
        Spec.assertEqWith
          s
          "bob draws it in his draw step"
          (fmap (fmap S.nameOf . (`Game.cardOf` drawn)) (Game.zoneMembers Zone.Hand S.bob drawn))
          [Just (CardName.MkCardName (Text.pack "Goblin Piker"))]
        Spec.assertEqWith s "leaving the three he started with" (Game.zoneMembers Zone.Library S.bob drawn) [oldTopId, middleId, deepId]
      [] -> Spec.assertFailure s "bob's library should hold the seeded cards"
  -- The elision LibraryPlacement.Stated buys: a card that NAMES the end asks
  -- nobody for it. Paired with the Aetherspouts group's positive, which requires
  -- the prompt on an owner-chosen board -- without the pair either alone would
  -- pass an implementation that never asks at all.
  Spec.it s "CR 401.2 a STATED end raises no ChooseLibraryEnd" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    griptide <- S.printingOf s registry "Griptide"
    let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay island 4)
        (_, g2) = S.addLibraryCard bolt S.bob g1
        (gs, spellId) = S.handOne griptide g2
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseLibraryEnd {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets)
          _ -> pure (S.identityAnswer p)
        (after, asked) =
          State.runState
            ( fmap snd . Engine.runGame countingAnswer gs $ do
                S.cast S.alice spellId
                Stack.resolveTop
            )
            0
    Spec.assertEqWith s "nobody was asked which end" asked 0
    Spec.assertEqWith s "and the creature still went to the TOP" (fmap (fmap S.nameOf . (`Game.cardOf` after)) (take 1 (Game.zoneMembers Zone.Library S.bob after))) [Just (CardName.MkCardName (Text.pack "Goblin Piker"))]
  -- The control: Unsummon states no library position at all, so its bounce must
  -- be exactly what it was before the field existed.
  Spec.it s "CR 400.3 the control: Unsummon on the same board still returns the creature to its owner's HAND" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    unsummon <- S.printingOf s registry "Unsummon"
    let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay island 4)
        (_, g2) = S.addLibraryCard bolt S.bob g1
        (gs, spellId) = S.handOne unsummon g2
        aimAtPiker :: Prompt.Prompt r -> r
        aimAtPiker p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure aimAtPiker gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure aimAtPiker cast Stack.resolveTop)
    Spec.assertEqWith s "one card in bob's hand" (S.handSize S.bob after) 1
    Spec.assertEqWith s "and his library is the one card it was" (length (Game.zoneMembers Zone.Library S.bob after)) 1

-- Every question Aetherspouts raises: which object each owner was asked to place
-- (CR 401.2), and which batch each owner was asked to arrange (CR 401.4).
type SpoutsLog = ([(PlayerId.PlayerId, ObjectId.ObjectId)], [(PlayerId.PlayerId, LibraryPosition.LibraryPosition, [ObjectId.ObjectId])])

-- alice is mid-combat attacking with `mine` creatures she owns and `stolen`
-- creatures BOB owns under her control, holds an Aetherspouts and the five
-- Islands that pay for it, and both libraries are two cards deep so an arrival
-- at either end is distinguishable from one at the other.
--
-- The stolen creature is what makes a ONE-COMBAT board hold two owners at all:
-- CR 508.1a says "the active player chooses which creatures THAT THEY CONTROL
-- ... will attack", so every attacker in one combat shares a controller and only
-- separating owner from controller can put two owners' cards in the batch.
-- S.giveControl also settles it under alice, which is that rule's second
-- sentence ("controlled by the active player continuously since the turn
-- began").
spoutsBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
spoutsBoard island spouts mine stolen =
  let addAll pid ps gs = List.foldl' (\(ids, g) p -> let (oid, g1) = S.addCreature p pid g in (ids <> [oid], g1)) ([], gs) ps
      (gs0, ours, _) = S.combatBoardOf mine []
      (theirs, gs1) = addAll S.bob stolen gs0
      gs2 = List.foldl' (\g oid -> S.giveControl oid S.alice g) gs1 theirs
      withLands = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) gs2 [1 :: Int .. 5]
      stocked = List.foldl' (\g pid -> snd (S.addLibraryCard island pid (snd (S.addLibraryCard island pid g)))) withLands [S.alice, S.bob]
      (withCard, spell) = S.handOne spouts stocked
   in ( -- handOne parks its state in a precombat main phase; this board is
        -- mid-combat, the way trumpetBoard restores it.
        withCard
          { GameState.phase = GameState.phase gs0,
            GameState.priority = GameState.priority gs0
          },
        spell,
        ours,
        theirs
      )

-- Declare alice's attack, then cast and resolve the Aetherspouts under an
-- answerer that records every question it is asked.
--
-- `end` picks each owner's answer BY WHO IS ASKED, which is the whole point of
-- the two-owner board: an implementation that raised the prompt with the
-- resolving CONTROLLER would hand both cards the same end.
castSpouts :: (PlayerId.PlayerId -> LibraryPosition.LibraryPosition) -> [Natural] -> GameState.GameState -> ObjectId.ObjectId -> (GameState.GameState, SpoutsLog)
castSpouts end arrangement board spell =
  let attacking = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
      answerer :: Prompt.Prompt r -> State.State SpoutsLog r
      answerer p = case p of
        Prompt.ChooseLibraryEnd _ pid oid -> do
          State.modify (\(ends, arrs) -> (ends <> [(pid, oid)], arrs))
          pure (end pid)
        Prompt.ArrangeLibraryArrivals _ pid position oids -> do
          State.modify (\(ends, arrs) -> (ends, arrs <> [(pid, position, oids)]))
          pure arrangement
        _ -> pure (S.identityAnswer p)
   in State.runState
        ( fmap snd . Engine.runGame answerer attacking $ do
            S.cast S.alice spell
            Stack.resolveTop
        )
        ([], [])

-- Aetherspouts ({3}{U}{U} instant, "For each attacking creature, its owner puts
-- it on their choice of the top or bottom of their library"): the pool's
-- producer for a library end the OWNER picks (CR 401.2, #1035) and for CR 401.4's
-- arrangement of two or more cards reaching one end at once (#990). WotC's own
-- 2014-07-18 ruling on the card states both halves.
--
-- Nothing here bears on #379: CR 608.2f's secondary sentence is guarded by "if
-- the action can't be processed simultaneously", and CR 401.4 gives a library
-- destination its own rule with its own decider -- so a correct Aetherspouts
-- SCREENS the sweep order off rather than exposing it.
aetherspoutsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aetherspoutsSpec s registry = Spec.describe s "Aetherspouts" $ do
  -- The claim: the end each creature goes to is decided by that creature's
  -- OWNER, not by the spell's controller and not by the engine. alice controls
  -- both attackers; bob owns one of them, and only he can send it to the top.
  Spec.it s "CR 401.2 each attacking creature's OWNER picks the end, not the resolving controller" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    spouts <- S.printingOf s registry "Aetherspouts"
    let (board, spell, ours, theirs) = spoutsBoard island spouts [piker] [giant]
        (after, (ends, arrangements)) = castSpouts (\pid -> if pid == S.bob then LibraryPosition.Top else LibraryPosition.Bottom) [] board spell
        -- By NAME, through `namesIn`: CR 400.7 mints a fresh id at the
        -- destination, so a library arrival is never the id it had on the
        -- battlefield. That is also why the two attackers are two different
        -- printings -- two Pikers would be indistinguishable at either end.
        bobs = namesIn Zone.Library S.bob after
        alices = namesIn Zone.Library S.alice after
    -- Without this the sweep could have found nothing and every assertion below
    -- would pass vacuously.
    Spec.assertEqWith s "both attackers left the battlefield" (filter (`S.onBattlefield` after) (ours <> theirs)) []
    Spec.assertEqWith s "each creature's own owner was asked, once, in the sweep's APNAP order" ends (fmap ((,) S.alice) ours <> fmap ((,) S.bob) theirs)
    -- One card per (owner, end) group, so CR 401.4 has nothing to arrange. The
    -- negative half of the elision pair; the positive is the next test.
    Spec.assertEqWith s "and nobody was asked to arrange a batch of one" arrangements []
    Spec.assertEqWith s "each library grew by exactly its owner's card" (length bobs, length alices) (3, 3)
    -- The discriminating half. An implementation that ignored the answers would
    -- fall back on LibraryPosition.defaultValue and put everything on the
    -- BOTTOM, so it is bob's Giant at the TOP that catches it -- a
    -- both-cards-to-the-bottom board would pass for the wrong reason.
    Spec.assertEqWith
      s
      "bob answered Top so his Giant heads his library; alice answered Bottom so her Piker is last in hers"
      (take 1 bobs <> drop 2 alices)
      [Just (S.printingName giant), Just (S.printingName piker)]
  -- CR 401.4's positive, at the TOP. Two of alice's own creatures, both sent to
  -- the top of her library, arranged with the NON-CANONICAL answer [1, 0] --
  -- answering [0, 1] is the sweep order, so every assertion would pass under an
  -- implementation that never asked.
  Spec.it s "CR 401.4 two cards reaching the TOP at once are arranged by their owner" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    spouts <- S.printingOf s registry "Aetherspouts"
    let (board, spell, ours, _) = spoutsBoard island spouts [piker, giant] []
        (after, (ends, arrangements)) = castSpouts (const LibraryPosition.Top) [1, 0] board spell
        alices = namesIn Zone.Library S.alice after
    Spec.assertEqWith s "both attackers left the battlefield" (filter (`S.onBattlefield` after) ours) []
    Spec.assertEqWith s "alice was asked about each of her two creatures" (length ends) 2
    Spec.assertEqWith s "and asked ONCE to arrange the pair, at the top of her library" arrangements [(S.alice, LibraryPosition.Top, ours)]
    -- The answer names the cards from the chosen end inward, so the creature the
    -- sweep offered SECOND finishes on top.
    Spec.assertEqWith
      s
      "read from the top inward, the library is the order she gave"
      (take 2 alices)
      [Just (S.printingName giant), Just (S.printingName piker)]
  -- The same claim at the other end. Both ends want the SAME traversal, because
  -- the Sequence grows from opposite ends for them -- the answer's head is the
  -- last card moved either way -- so this is the leg that catches a moves-in-
  -- answer-order implementation just as the one above does.
  Spec.it s "CR 401.4 two cards reaching the BOTTOM at once are arranged by their owner" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    spouts <- S.printingOf s registry "Aetherspouts"
    let (board, spell, ours, _) = spoutsBoard island spouts [piker, giant] []
        (after, (_, arrangements)) = castSpouts (const LibraryPosition.Bottom) [1, 0] board spell
        alices = namesIn Zone.Library S.alice after
    Spec.assertEqWith s "asked once, at the bottom of her library" arrangements [(S.alice, LibraryPosition.Bottom, ours)]
    Spec.assertEqWith
      s
      "read from the bottom inward, the library is the order she gave"
      (reverse (drop 2 alices))
      [Just (S.printingName giant), Just (S.printingName piker)]

drawCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
drawCardSpec s registry = Spec.describe s "DrawCard" $ do
  Spec.it s "CR 121.2 drawCard moves the top library card to hand" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, withCard) = S.addLibraryCard piker S.alice base
        after = S.runPure S.identityAnswer withCard (Event.drawCard S.alice)
    Spec.assertEqWith s "one card in hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice after) []
  Spec.it s "CR 121.3 drawing from an empty library records the failed draw" $ do
    let after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Event.drawCard S.alice)
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty after)) "drewFromEmpty marked"

loseLifeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
loseLifeSpec s registry = Spec.describe s "LoseLife" $ do
  -- Both cases are Sign in Blood, the card that proves the opcode (#273): its
  -- two clauses share one target slot, so the player who draws is the player
  -- who loses life, and neither is aimed at the caster.
  -- The last assertion is the falsifier for a life loss spelled as damage.
  -- CR 119.2 makes damage a CAUSE of life loss, not a synonym for it, so
  -- this records no damage event for CR 614/615's replacement and
  -- prevention, infect's CR 120.3b diversion or CR 704.5h's deathtouch scan
  -- to read.
  Spec.it s "CR 119.3 Sign in Blood makes the player it targets draw two and lose two life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let base = S.landsInPlay swamp 2
        withLib = stockLibrary piker S.bob 3 base
        (gs, spellId) = S.handOne signInBlood withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        isDamage ev = case ev of
          GameEvent.DamageDealt _ -> True
          _ -> False
    Spec.assertEqWith s "bob drew two" (S.handSize S.bob after) 2
    Spec.assertEqWith s "and lost two life" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "alice, who cast it, lost none" (S.lifeOf S.alice after) (S.lifeOf S.alice gs)
    Spec.assertBool s (not (any isDamage (S.eventsOf after))) "no damage was dealt (CR 119.2)"
  -- CR 704.5a: life lost without damage still reaches the state-based
  -- action -- the same check a CR 119.4 pay-life cost answers to. Bob is at
  -- two, so the second clause is lethal though nothing dealt damage.
  Spec.it s "CR 704.5a Sign in Blood's life loss can take a player to 0 and lose them the game" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let base = S.landsInPlay swamp 2
        withLib = stockLibrary piker S.bob 3 base
        (gs0, spellId) = S.handOne signInBlood withLib
        gs = gs0 {GameState.players = Map.adjust (\pl -> pl {Player.life = 2}) S.bob (GameState.players gs0)}
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob is at 0" (S.lifeOf S.bob after) (Just 0)
    Spec.assertEqWith s "and alice wins" (GameState.result (S.settleSba after)) (Just (Result.Won S.alice))

-- Mirror Universe (Legends) on alice's battlefield, in her own upkeep, with the
-- three seats at three DIFFERENT life totals: "{T}, Sacrifice this artifact:
-- Exchange life totals with target opponent. Activate only during your upkeep."
--
-- Three seats because a two-player board cannot tell the exchange's TARGET from
-- "the other player". carol is a second legal target the interpreter can pick,
-- and the totals are distinct so that no pair of them coincides.
--
-- The schedule loses its head for augurBoard's reason (ActivateSpec): emptyGame's
-- `remaining` still begins with the upkeep step, so a runStep-driven test would
-- otherwise advance out of the step the card names.
mirrorBoard :: Printing.Printing -> Integer -> Integer -> Integer -> (ObjectId.ObjectId, GameState.GameState)
mirrorBoard mirror aliceLife bobLife carolLife =
  let (mirrorId, gs1) = S.addCreature mirror S.alice S.threePlayerGame
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
   in ( mirrorId,
        gs1
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.drop 1 (GameState.remaining gs1),
            GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players gs1)))
          }
      )

-- Soul Conduit (Eldritch Moon) on alice's battlefield over six untapped Islands,
-- with the three seats at three DIFFERENT life totals: "{6}, {T}: Two target
-- players exchange life totals."
--
-- The same three seats and the same schedule surgery as mirrorBoard, and for the
-- same reasons -- but here the point of the third seat is that the two sides of
-- the exchange can BOTH be players other than the controller, which two seats
-- cannot express.
soulConduitBoard :: Printing.Printing -> Printing.Printing -> Integer -> Integer -> Integer -> (ObjectId.ObjectId, GameState.GameState)
soulConduitBoard conduit island aliceLife bobLife carolLife =
  let withLands = List.foldl' (\gs _ -> snd (S.addCreature island S.alice gs)) S.threePlayerGame [1 .. 6 :: Int]
      (conduitId, gs1) = S.addCreature conduit S.alice withLands
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
   in ( conduitId,
        gs1
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.drop 1 (GameState.remaining gs1),
            GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players gs1)))
          }
      )

-- Takes the first activation offered, taps whatever the payment asks for, and
-- fills the target slot with `sides` -- S.preferring rather than a fixed set, so
-- the announced count (CR 601.2c) is what decides how many are named.
conduitAnswer :: [PlayerId.PlayerId] -> Prompt.Prompt r -> r
conduitAnswer sides p = case p of
  Prompt.ChooseAction _ _ options -> case filter isActivation options of
    a : _ -> a
    [] -> A.Pass
  Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
  Prompt.ChooseTargets _ _ _ sets -> S.preferring wanted sets
  _ -> S.identityAnswer p
  where
    wanted r = case r of
      Recipient.ToPlayer pid -> elem pid sides
      _ -> False

-- Takes the first activation offered and aims every target slot at `who`.
exchangeAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
exchangeAnswer who p = case p of
  Prompt.ChooseAction _ _ options -> case filter isActivation options of
    a : _ -> a
    [] -> A.Pass
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
  _ -> S.identityAnswer p

isActivation :: A.Action -> Bool
isActivation a = case a of
  A.Activate {} -> True
  A.Pass -> False
  A.Play {} -> False
  A.Cast {} -> False
  A.TurnFaceUp _ -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.Plot _ -> False
  A.Foretell _ -> False
  A.ActivateManaAbility _ -> False
  A.Ignore _ -> False

-- The life events the whole step logged, by player and amount. CR 701.12c makes
-- the exchange a GAIN and a LOSS rather than two assignments, so this is what a
-- "whenever you gain life" trigger would have to read.
lifeGains :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeGains gs = [(pid, n) | GameEvent.LifeGained (LifeChange.MkLifeChange pid n) <- S.eventsOf gs]

lifeLosses :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeLosses gs = [(pid, n) | GameEvent.LifeLost (LifeChange.MkLifeChange pid n) <- S.eventsOf gs]

exchangeLifeTotalsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exchangeLifeTotalsSpec s registry = Spec.describe s "ExchangeLifeTotals" $ do
  -- The gameplay-level proof (design.md section 4), driven through
  -- Engine.runStep and the priority loop: alice at 4 and bob at 27 swap, and
  -- each reaches the other's PREVIOUS total -- an implementation that wrote one
  -- side before reading the other would leave both on one number.
  Spec.it s "CR 701.12c whole card: Mirror Universe swaps its controller's total with the target's" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (mirrorId, board) = mirrorBoard mirror 4 27 13
        after = S.runPure (exchangeAnswer S.bob) board Engine.runStep
    Spec.assertEqWith s "alice took bob's 27" (S.lifeOf S.alice after) (Just 27)
    Spec.assertEqWith s "bob took alice's 4" (S.lifeOf S.bob after) (Just 4)
    Spec.assertEqWith s "carol, untargeted, is untouched" (S.lifeOf S.carol after) (Just 13)
    Spec.assertBool s (not (Set.member mirrorId (GameState.battlefield after))) "the Universe paid itself"
    -- CR 701.12c's "gains or loses the amount of life necessary", as events:
    -- 27 - 4 either way, and nothing else moved a life total this step.
    Spec.assertEqWith s "alice gained 23" (lifeGains after) [(S.alice, 23)]
    Spec.assertEqWith s "bob lost 23" (lifeLosses after) [(S.bob, 23)]
    -- The printed "target opponent" is the card's own filter, read from the
    -- perspective of the player activating it (CR 109.5), so alice is never a
    -- candidate. Paired with the outcome above rather than asserted alone, since
    -- a candidate list nothing consumed proves nothing.
    let candidates = case Activate.abilitiesFor mirrorId board of
          [ability] -> case Seq.lookup 0 (Modal.modes (ActivatedAbility.modal ability)) of
            Just mode -> Map.elems (Target.legalSets (Just S.alice) mirrorId (Mode.targetSlots mode) board)
            Nothing -> []
          _ -> []
    Spec.assertEqWith s "both opponents are candidates, alice is not" candidates [Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol]]

  -- The same board and the same interpreter, aimed at carol: the slot is READ
  -- rather than the exchange running against a fixed second seat.
  Spec.it s "CR 601.2c the other side is the slot's target, not simply the opponent" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (_, board) = mirrorBoard mirror 4 27 13
        after = S.runPure (exchangeAnswer S.carol) board Engine.runStep
    Spec.assertEqWith s "alice took carol's 13" (S.lifeOf S.alice after) (Just 13)
    Spec.assertEqWith s "carol took alice's 4" (S.lifeOf S.carol after) (Just 4)
    Spec.assertEqWith s "bob, untargeted, is untouched" (S.lifeOf S.bob after) (Just 27)

  -- CR 119.9: equal totals are an exchange that moves nobody, and a gain of 0 is
  -- no life gain event at all -- so "whenever you gain life" must not fire on it.
  Spec.it s "CR 119.9 an exchange between equal totals logs no life event" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (mirrorId, board) = mirrorBoard mirror 15 15 13
        after = S.runPure (exchangeAnswer S.bob) board Engine.runStep
    Spec.assertEqWith s "alice is still at 15" (S.lifeOf S.alice after) (Just 15)
    Spec.assertEqWith s "bob is still at 15" (S.lifeOf S.bob after) (Just 15)
    Spec.assertBool s (not (Set.member mirrorId (GameState.battlefield after))) "and the ability was activated: its sacrifice was paid"
    Spec.assertEqWith s "no gain" (lifeGains after) []
    Spec.assertEqWith s "no loss" (lifeLosses after) []

  -- CR 701.12c's other shape: BOTH sides come out of one instance of the word
  -- "target" (CR 601.2c), and neither of them need be the controller. bob at 27
  -- and carol at 13 swap while alice, who activated it, keeps her 4 -- so the
  -- reading in which the controller is always one side gets a different answer
  -- for every seat. The four numbers (4, 13, 27 and the 14 that moves) are
  -- distinct, so no pair of readings coincides.
  Spec.it s "CR 701.12c Soul Conduit exchanges the totals of two players, neither of them its controller" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (conduitId, board) = soulConduitBoard conduit island 4 27 13
        after = S.runPure (conduitAnswer [S.bob, S.carol]) board Engine.runStep
    Spec.assertEqWith s "bob took carol's 13" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith s "carol took bob's 27" (S.lifeOf S.carol after) (Just 27)
    Spec.assertEqWith s "alice, who activated it, is untouched" (S.lifeOf S.alice after) (Just 4)
    Spec.assertEqWith s "carol gained 14" (lifeGains after) [(S.carol, 14)]
    Spec.assertEqWith s "bob lost 14" (lifeLosses after) [(S.bob, 14)]
    -- The ability was really activated, so an exchange that did nothing cannot
    -- pass the assertions above by leaving the board alone.
    Spec.assertEqWith s "and the Conduit paid its own {T}" (fmap Object.tapped (Game.lookupObject conduitId after)) (Just TapState.Tapped)

  -- The same board and the same interpreter, differing only in which two players
  -- the answer names: the controller is a side when she is TARGETED, and the slot
  -- is what decides.
  Spec.it s "CR 601.2c both sides are read from the slot, the controller included when named" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (_, board) = soulConduitBoard conduit island 4 27 13
        after = S.runPure (conduitAnswer [S.alice, S.bob]) board Engine.runStep
    Spec.assertEqWith s "alice took bob's 27" (S.lifeOf S.alice after) (Just 27)
    Spec.assertEqWith s "bob took alice's 4" (S.lifeOf S.bob after) (Just 4)
    Spec.assertEqWith s "carol, whom nobody named, is untouched" (S.lifeOf S.carol after) (Just 13)

  -- CR 701.12a: "if the entire exchange can't be completed, no part of the
  -- exchange occurs." One of the two targets leaves the game after the ability is
  -- on the stack, so CR 608.2b drops her (a departed player is no longer in CR
  -- 115's pool) and the ability resolves with one side and no exchange -- rather
  -- than falling back on the controller, which is the reading this discriminates.
  Spec.it s "CR 701.12a an exchange left with one side does nothing at all" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (conduitId, board) = soulConduitBoard conduit island 4 27 13
        answer :: Prompt.Prompt r -> r
        answer = conduitAnswer [S.bob, S.carol]
    case Activate.abilitiesFor conduitId board of
      [ability] -> do
        let activated = S.runPure answer board (Activate.activateAbility S.alice conduitId ability)
            gone = S.runPure answer activated (Departure.leaveGame Departure.Type.Conceded S.carol)
            after = S.runPure answer gone Stack.resolveTop
        Spec.assertEqWith s "bob, the surviving target, keeps his 27" (S.lifeOf S.bob after) (Just 27)
        Spec.assertEqWith s "and alice, who is no side of it, keeps her 4" (S.lifeOf S.alice after) (Just 4)
        Spec.assertEqWith s "no gain" (lifeGains after) []
        Spec.assertEqWith s "no loss" (lifeLosses after) []
      other -> Spec.assertFailure s ("expected exactly one activated ability on the Conduit, got " <> show (length other))

-- CR 119.5: "If an effect sets a player's life total to a specific number, the
-- player gains or loses the necessary amount of life to end up with the new
-- total." So a set is NOT a third kind of life event: it is a gain or a loss,
-- whichever the arithmetic makes it, and everything that watches gaining or
-- losing life sees it. The whole group exists to hold that reading in place.
--
-- Two cards prove it, and it takes two because neither reaches both directions:
--
--   * Magister Sphinx, {4}{W}{U}{B} Artifact Creature -- Sphinx 5/5 with flying,
--     "When this creature enters, target player's life total becomes 10." The
--     literal, so the SAME card is a gain at one seat and a loss at another --
--     and the first two cases below are one board differing in nothing but which
--     seat the trigger names.
--   * Arbiter of Knollridge, {6}{W} Creature -- Giant Wizard 5/5 with vigilance,
--     "When this creature enters, each player's life total becomes the highest
--     life total among all players." The fold, and the several-recipients shape a
--     targeted card cannot reach.
--
-- Three seats at 4, 27 and 13 -- distinct, and one above 10 and two below, so the
-- Sphinx cases tell a gain from a loss. 27 is not the sum (44), the count (3) or
-- the least (4), so Arbiter's one number falsifies every other fold. Only the
-- CR 119.9 case changes a starting total, moving carol to 10 so that the seat the
-- trigger names is already there.
--
-- The watchers are what make the claim about EVENTS rather than about totals.
-- Ajani's Pridemate ("whenever you gain life, put a +1/+1 counter on this
-- creature") is on the board for the gain side and Mindcrank ("whenever an
-- opponent loses life, that player mills that many cards") for the loss side, so
-- every case asserts which of the two fired -- and a set that wrote Player.life
-- directly would leave both silent while every life total still came out right.
setLifeTotalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
setLifeTotalSpec s registry =
  let -- Cast, then settle-and-resolve until the stack runs dry: the spell, then
      -- CR 603.6a's entry trigger, then whatever the life change itself
      -- triggered. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears GameState.events out from under lifeGains and lifeLosses. Six
      -- passes is more than the deepest case needs, and settling or resolving an
      -- empty stack is a no-op.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- Pinned, not searched: the trigger's target slot takes `who` and nothing
      -- else, so a mutation cannot be repaired by an answerer that goes looking
      -- for a legal option. Three players and a count of one, so there is a real
      -- choice to pin.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer who) sets
        _ -> S.identityAnswer p
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- The three seats, alice holding the mana and both watchers, and every
      -- library stocked so Mindcrank has something to mill and CR 104.3c never
      -- fires. `lands` is the mana base the spell needs; everything else is
      -- shared by all four cases.
      setBoard lands pridemate mindcrank filler spell aliceLife bobLife carolLife =
        let withLands = List.foldl' (\board (printing, n) -> S.landsFor printing S.alice n board) S.threePlayerGame lands
            (aliceMate, withAliceMate) = S.addCreature pridemate S.alice withLands
            (bobMate, withBobMate) = S.addCreature pridemate S.bob withAliceMate
            (_, withCrank) = S.addCreature mindcrank S.alice withBobMate
            stocked = List.foldl' (\board pid -> stockLibrary filler pid 30 board) withCrank [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players stocked)))}
            (gs, spellId) = S.handOne spell lifed
         in (aliceMate, bobMate, spellId, gs)
      addPikers piker pid n gs = List.foldl' (\board _ -> snd (S.addCreature piker pid board)) gs [1 .. n :: Int]
      -- Biorhythm's board, where what differs between the seats is their CREATURE
      -- COUNT rather than their life total. setBoard leaves alice a Pridemate and
      -- a Mindcrank -- an ARTIFACT, so not one of her creatures -- and bob a
      -- Pridemate, so the Pikers below take alice to 4 creatures and bob to 3.
      -- `carolPikers` is the one knob the pair of cases turns.
      --
      -- Life totals 2, 3 and 9 against counts 4, 3 and 0: every seat's answer is
      -- distinct, no seat's answer is any other seat's, and only bob's happens to
      -- equal his own starting total -- which is what the CR 119.9 half of the
      -- pair needs. bob's count and bob's life coinciding is why the other two
      -- seats are there: alice's 2 against 4 and carol's 9 against 0 tell "counts
      -- creatures" from "reads a life total".
      biorhythmBoard carolPikers = do
        forest <- S.printingOf s registry "Forest"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        biorhythm <- S.printingOf s registry "Biorhythm"
        let (aliceMate, bobMate, spellId, gs) = setBoard [(forest, 8)] pridemate mindcrank piker biorhythm 2 3 9
        pure (aliceMate, bobMate, spellId, addPikers piker S.carol carolPikers (addPikers piker S.bob 2 (addPikers piker S.alice 3 gs)))
      sphinxBoard aliceLife bobLife carolLife = do
        plains <- S.printingOf s registry "Plains"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sphinx <- S.printingOf s registry "Magister Sphinx"
        pure (setBoard [(plains, 5), (island, 1), (swamp, 1)] pridemate mindcrank piker sphinx aliceLife bobLife carolLife)
   in Spec.describe s "SetLifeTotal" $ do
        -- The gain direction. alice is BELOW 10, so reaching it is a gain of 6 --
        -- and her Pridemate sees it, which is the whole CR 119.5 claim.
        Spec.it s "CR 119.5 Magister Sphinx sets a total UPWARD, and that is a life gain" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.alice) spellId gs
          Spec.assertEqWith s "alice reached 10" (S.lifeOf S.alice after) (Just 10)
          Spec.assertEqWith s "bob, untargeted, keeps his 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly 6" (lifeGains after) [(S.alice, 6)]
          Spec.assertEqWith s "and as no loss at all" (lifeLosses after) []
          Spec.assertEqWith s "alice's Pridemate saw the gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: it was not his life" (countersOn bobMate after) (Just 0)
          Spec.assertEqWith s "and Mindcrank stayed silent: nobody lost life" (graveyardSize S.bob after) 0
        -- The control twin, differing in ONE thing: the seat the trigger names.
        -- bob is ABOVE 10, so the identical card is a LOSS of 17 -- Mindcrank
        -- fires and the Pridemate does not, which is the pair that tells a set
        -- from a gain.
        Spec.it s "CR 119.5 the control: the same card set DOWNWARD is a life loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.bob) spellId gs
          Spec.assertEqWith s "bob came down to 10" (S.lifeOf S.bob after) (Just 10)
          Spec.assertEqWith s "alice, untargeted, keeps her 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a loss of exactly 17" (lifeLosses after) [(S.bob, 17)]
          Spec.assertEqWith s "and as no gain at all" (lifeGains after) []
          Spec.assertEqWith s "Mindcrank milled bob for exactly 17" (graveyardSize S.bob after) 17
          Spec.assertEqWith s "alice's Pridemate stayed silent" (countersOn aliceMate after) (Just 0)
          Spec.assertEqWith s "and so did bob's: losing life is not gaining it" (countersOn bobMate after) (Just 0)
        -- CR 119.9's own last sentence, on the set: carol is ALREADY at 10, so
        -- the necessary amount is 0, no life event occurs, and neither watcher
        -- may fire. The Sphinx really entered, so a spell that did nothing cannot
        -- pass this by leaving the board alone.
        Spec.it s "CR 119.9 setting a total to the number it already holds is neither a gain nor a loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 10
          let after = castAndTrigger (aimedAt S.carol) spellId gs
          Spec.assertEqWith s "carol is still at 10" (S.lifeOf S.carol after) (Just 10)
          Spec.assertEqWith s "no gain" (lifeGains after) []
          Spec.assertEqWith s "no loss" (lifeLosses after) []
          Spec.assertEqWith s "no Pridemate counter anywhere" (fmap (\oid -> countersOn oid after) [aliceMate, bobMate]) [Just 0, Just 0]
          Spec.assertEqWith s "and Mindcrank milled nobody" (graveyardSize S.carol after) 0
          Spec.assertEqWith s "and the Sphinx really entered, so a spell that never resolved cannot pass this" (Set.size (GameState.battlefield after)) (Set.size (GameState.battlefield gs) + 1)
        -- Arbiter of Knollridge: SEVERAL recipients from one instruction, and a
        -- folded number rather than a literal. 27 is the highest, and it is not
        -- the sum (44), the count (3), the least (4) or any seat's own total, so
        -- one set of three assertions falsifies every other reading.
        --
        -- bob is the seat that is ALREADY highest, and his own Pridemate is the
        -- point of the case: he ends on the number he started on, so CR 119.9
        -- says no life gain event happened to him even though the effect named
        -- him. That is the assertion a raw "write the total to every player"
        -- implementation fails.
        Spec.it s "CR 119.5 Arbiter of Knollridge raises every seat to the HIGHEST total, gaining only where the total moves" $ do
          plains <- S.printingOf s registry "Plains"
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          mindcrank <- S.printingOf s registry "Mindcrank"
          piker <- S.printingOf s registry "Goblin Piker"
          arbiter <- S.printingOf s registry "Arbiter of Knollridge"
          let (aliceMate, bobMate, spellId, gs) = setBoard [(plains, 7)] pridemate mindcrank piker arbiter 4 27 13
              after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice rose from 4 to 27" (S.lifeOf S.alice after) (Just 27)
          Spec.assertEqWith s "bob, already highest, stayed at 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol rose from 13 to 27" (S.lifeOf S.carol after) (Just 27)
          Spec.assertEqWith s "exactly the two seats that moved gained, by exactly their deltas" (lifeGains after) [(S.alice, 23), (S.carol, 14)]
          Spec.assertEqWith s "nobody lost life" (lifeLosses after) []
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: a total set to itself is no gain (CR 119.9)" (countersOn bobMate after) (Just 0)
          Spec.assertEqWith s "and Mindcrank milled nobody" (graveyardSize S.carol after) 0
        -- Biorhythm, {6}{G}{G} Sorcery: "Each player's life total becomes the
        -- number of creatures they control." The number is EACH RECIPIENT'S OWN,
        -- which neither producer above can tell from a single evaluation: the
        -- Sphinx names one seat and Arbiter names one number for the whole table.
        --
        -- Three seats, three different counts -- alice 4, bob 3, carol 0 -- so no
        -- seat's answer can stand in for another's, and a reading that evaluated
        -- once from the CONTROLLER's perspective would hand bob and carol alice's
        -- 4. Each seat carries one half of the rule besides:
        --
        --   * alice gains (2 -> 4), and her Pridemate sees it;
        --   * bob's count is his current total, so CR 119.9 leaves him with no
        --     life event at all and his Pridemate silent;
        --   * carol controls nothing, so her total becomes 0 and CR 104.3b takes
        --     her out of the game.
        Spec.it s "CR 119.5 Biorhythm sets EACH seat to its OWN creature count" $ do
          (aliceMate, bobMate, spellId, gs) <- biorhythmBoard 0
          let after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice, controlling 4 creatures, rose from 2 to 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob, controlling 3, is at 3 -- not alice's 4" (S.lifeOf S.bob after) (Just 3)
          Spec.assertEqWith s "carol, controlling none, fell from 9 to 0" (S.lifeOf S.carol after) (Just 0)
          Spec.assertEqWith s "only alice gained, and by her own delta" (lifeGains after) [(S.alice, 2)]
          Spec.assertEqWith s "only carol lost, and by hers" (lifeLosses after) [(S.carol, 9)]
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: his total was already his count (CR 119.9)" (countersOn bobMate after) (Just 0)
          Spec.assertBool s (notElem S.carol (Game.stillPlaying after)) "CR 104.3b took carol, at 0 life, out of the game"
          Spec.assertEqWith s "and left the other two in it" (filter (`elem` Game.stillPlaying after) [S.alice, S.bob]) [S.alice, S.bob]
        -- The control twin, differing in ONE thing: carol controls a single Piker.
        -- Her answer moves 0 -> 1 while alice's and bob's do not move at all,
        -- which is the pair that shows the count is read per seat; and a total of
        -- 1 is a total CR 104.3b has no quarrel with, so the state-based action
        -- above fired on carol's number rather than on her being named.
        Spec.it s "CR 104.3b the control: one creature is one life, and carol stays in the game" $ do
          (aliceMate, bobMate, spellId, gs) <- biorhythmBoard 1
          let after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice is unmoved at 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob is unmoved at 3" (S.lifeOf S.bob after) (Just 3)
          Spec.assertEqWith s "carol, controlling one creature, fell from 9 to 1" (S.lifeOf S.carol after) (Just 1)
          Spec.assertEqWith s "alice still gained 2" (lifeGains after) [(S.alice, 2)]
          Spec.assertEqWith s "carol lost 8 rather than 9" (lifeLosses after) [(S.carol, 8)]
          Spec.assertBool s (elem S.carol (Game.stillPlaying after)) "and stayed in the game"
          Spec.assertEqWith s "Mindcrank milled carol for exactly her loss" (graveyardSize S.carol after) 8
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's stayed silent" (countersOn bobMate after) (Just 0)

-- Reverse the Sands, {6}{W}{W} Sorcery: "Redistribute any number of players'
-- life totals. (Each of those players gets one life total back.)" CR 119.7 and
-- CR 119.8 name the action; every seat's new total is CR 119.5's gain or loss,
-- which setLifeTotalSpec above holds in place for one recipient and this group
-- holds for a whole permutation.
--
-- The choice is the resolving controller's (CR 608.2c-d, and the card's own
-- ruling "you choose which player gets which life total when the spell
-- resolves"), so the engine may never pick it. Three seats at 27, 4 and 13 --
-- distinct, so no two assignments produce the same board -- and the two positive
-- cases are ONE board answered two ways, differing in nothing but the
-- permutation. They disagree at every seat, which is what no fixed permutation
-- the engine could have chosen for itself can do.
--
-- Neither permutation is the identity and neither is a rotation: both are
-- transpositions, so each has a seat that is chosen and mapped to ITSELF. That
-- seat is the one that tells "kept its own total" from "was never chosen" --
-- both leave the total alone, and only the watchers agree with CR 119.9 that
-- neither is a life event.
--
-- The watchers are what make the claim about EVENTS rather than totals. A
-- Pridemate under each seat is the gain side ("whenever you gain life") and
-- bob's Mindcrank is the loss side ("whenever an opponent loses life, that
-- player mills that many cards"), so each case pins which of the two fired at
-- which seat. A redistribution written as three raw Player.life writes would
-- leave all four silent while every total still came out right.
redistributeLifeTotalsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
redistributeLifeTotalsSpec s registry =
  let -- setLifeTotalSpec's driver: cast, then settle-and-resolve until the stack
      -- runs dry, so the spell and everything its life changes triggered all
      -- resolve. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears GameState.events out from under lifeGains and lifeLosses.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- PINNED, not searched: the answer is exactly these pairs whatever the
      -- prompt offers, so a mutation to the engine's own handling cannot be
      -- repaired by an answerer that goes hunting for a legal permutation.
      assigning :: [(PlayerId.PlayerId, PlayerId.PlayerId)] -> Prompt.Prompt r -> r
      assigning pairs p = case p of
        Prompt.ChooseRedistribution {} -> Map.fromList pairs
        _ -> S.identityAnswer p
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- alice holds the mana and casts; every seat has a Pridemate, bob has the
      -- Mindcrank, and every library is stocked deep enough that a 23-card mill
      -- never reaches CR 104.3c.
      sandsBoard = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sands <- S.printingOf s registry "Reverse the Sands"
        let withLands = S.landsFor plains S.alice 8 S.threePlayerGame
            (aliceMate, g1) = S.addCreature pridemate S.alice withLands
            (bobMate, g2) = S.addCreature pridemate S.bob g1
            (carolMate, g3) = S.addCreature pridemate S.carol g2
            (_, g4) = S.addCreature mindcrank S.bob g3
            stocked = List.foldl' (\board pid -> stockLibrary piker pid 40 board) g4 [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players stocked)))}
            (gs, spellId) = S.handOne sands lifed
        pure (aliceMate, bobMate, carolMate, spellId, gs)
      -- The board every refused answer is checked against: nothing moved, and
      -- nothing was logged. Shared so the three #222 cases differ in exactly one
      -- thing -- the answer.
      assertUntouched after = do
        Spec.assertEqWith s "alice keeps her 27" (S.lifeOf S.alice after) (Just 27)
        Spec.assertEqWith s "bob keeps his 4" (S.lifeOf S.bob after) (Just 4)
        Spec.assertEqWith s "carol keeps her 13" (S.lifeOf S.carol after) (Just 13)
        Spec.assertEqWith s "no gain" (lifeGains after) []
        Spec.assertEqWith s "no loss" (lifeLosses after) []
        -- Load-bearing: the spent sorcery and nothing else is in alice's
        -- graveyard, so the spell really RESOLVED and no Mindcrank mill followed.
        -- Without it every case below would pass just as well on a spell that
        -- fizzled.
        Spec.assertEqWith s "the spell resolved, and milled nobody doing it" (graveyardSize S.alice after) 1
   in Spec.describe s "RedistributeLifeTotals" $ do
        -- alice hands her 27 to carol and takes carol's 13; bob is CHOSEN and
        -- mapped to himself. One seat gains, one loses, one is chosen and stays
        -- put -- and 13, 4 and 27 are three different numbers, so this one set of
        -- assertions falsifies every other permutation of the three.
        Spec.it s "Reverse the Sands whole card: the controller's permutation is what happens, seat by seat" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.carol), (S.bob, S.bob), (S.carol, S.alice)]) spellId gs
          Spec.assertEqWith s "alice took carol's 13" (S.lifeOf S.alice after) (Just 13)
          Spec.assertEqWith s "bob, mapped to himself, is still on 4" (S.lifeOf S.bob after) (Just 4)
          Spec.assertEqWith s "carol took alice's 27" (S.lifeOf S.carol after) (Just 27)
          -- CR 119.5, per seat: the necessary amount, and its own sign.
          Spec.assertEqWith s "carol gained exactly the difference" (lifeGains after) [(S.carol, 14)]
          Spec.assertEqWith s "alice lost exactly the difference" (lifeLosses after) [(S.alice, 14)]
          Spec.assertEqWith s "carol's Pridemate saw her gain" (countersOn carolMate after) (Just 1)
          Spec.assertEqWith s "alice's did not: losing life is not gaining it" (countersOn aliceMate after) (Just 0)
          -- CR 119.9 on the fixed point: bob was chosen, so a redistribution
          -- that handed out totals blindly would still have "given" him one.
          -- Taking his own is a delta of 0 and therefore no life event at all.
          Spec.assertEqWith s "bob's Pridemate stayed silent: his own total back is no gain" (countersOn bobMate after) (Just 0)
          -- 14 milled, plus the spent sorcery itself (CR 608.2m), which is alice's
          -- card and lands in her graveyard -- so this also witnesses that the
          -- spell really resolved rather than fizzling quietly.
          Spec.assertEqWith s "bob's Mindcrank milled alice for exactly what she lost" (graveyardSize S.alice after) 15
          Spec.assertEqWith s "and milled carol for nothing: she gained" (graveyardSize S.carol after) 0
        -- The control twin: the SAME board, differing in nothing but the
        -- permutation the controller names. Every seat lands somewhere else than
        -- it did above, so no permutation the engine picked for itself can
        -- satisfy both cases -- which is the whole second-invariant claim.
        Spec.it s "the same board answered differently redistributes differently at every seat" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.bob), (S.bob, S.alice), (S.carol, S.carol)]) spellId gs
          Spec.assertEqWith s "alice took bob's 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob took alice's 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol, mapped to herself, is still on 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "bob gained exactly the difference" (lifeGains after) [(S.bob, 23)]
          Spec.assertEqWith s "alice lost exactly the difference" (lifeLosses after) [(S.alice, 23)]
          Spec.assertEqWith s "bob's Pridemate saw his gain" (countersOn bobMate after) (Just 1)
          Spec.assertEqWith s "alice's stayed silent" (countersOn aliceMate after) (Just 0)
          Spec.assertEqWith s "carol's stayed silent: her own total back is no gain" (countersOn carolMate after) (Just 0)
          -- 23 milled plus the spent sorcery, as in the case above.
          Spec.assertEqWith s "bob's Mindcrank milled alice for exactly what she lost" (graveyardSize S.alice after) 24
        -- A ROTATION, and the reason the two transpositions above are not enough
        -- on their own: a transposition is its own inverse, so reading the answer
        -- backwards -- giving each named player's total AWAY instead of handing it
        -- TO them -- lands on the very same board. This assignment's inverse is
        -- the other rotation, which lands on a different total at all three
        -- seats, so this is the case that pins the direction of the map.
        Spec.it s "a rotation moves every seat, and in the direction the answer names" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.bob), (S.bob, S.carol), (S.carol, S.alice)]) spellId gs
          Spec.assertEqWith s "alice took bob's 4, not carol's 13" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob took carol's 13, not alice's 27" (S.lifeOf S.bob after) (Just 13)
          Spec.assertEqWith s "carol took alice's 27, not bob's 4" (S.lifeOf S.carol after) (Just 27)
          Spec.assertEqWith s "the two seats that rose gained their own differences" (lifeGains after) [(S.bob, 9), (S.carol, 14)]
          Spec.assertEqWith s "and the one that fell lost hers" (lifeLosses after) [(S.alice, 23)]
          Spec.assertEqWith s "bob's Pridemate saw his gain" (countersOn bobMate after) (Just 1)
          Spec.assertEqWith s "carol's saw hers" (countersOn carolMate after) (Just 1)
          Spec.assertEqWith s "alice's stayed silent" (countersOn aliceMate after) (Just 0)
          -- The rotation is also what proves the totals are read from ONE
          -- snapshot: an implementation that set each seat in turn against the
          -- live board would hand bob the 4 alice had just taken.
          Spec.assertEqWith s "23 milled plus the spent sorcery" (graveyardSize S.alice after) 24
        -- The ruling's option (a), "leave the life totals as they are": "any
        -- number of players" includes none, so the empty answer is legal and
        -- quiet rather than refused.
        Spec.it s "redistributing among nobody is a legal answer and moves nothing" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning []) spellId gs)
        -- #222, splitting: alice takes carol's 13 while carol keeps it, so one
        -- life total would end up on two seats -- exactly what "you can't split
        -- up a life total when you redistribute it" forbids. Refused ENTIRE,
        -- because there is no honest partial permutation to keep.
        Spec.it s "#222 an answer that hands out a total its owner keeps is refused" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.carol)]) spellId gs)
        -- #222, duplication: alice and bob both take carol's 13. The keys are all
        -- three seats but the totals handed out are only two, so it is not a
        -- bijection and alice's 27 would simply vanish.
        Spec.it s "#222 an answer giving two players the same life total is refused" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.carol), (S.bob, S.carol), (S.carol, S.alice)]) spellId gs)
        -- #222, an outsider: dave is not in this game. This answer IS a
        -- permutation -- its keys and its values are the same two seats -- so
        -- only the candidate check refuses it, which is what makes this case
        -- discriminating rather than a second copy of the one above.
        Spec.it s "#222 an answer naming a player who is not in the game is refused" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.dave), (S.dave, S.alice)]) spellId gs)
        -- CR 102.1: the offer is the players IN the game, not the keys of
        -- GameState.players, which keep a departed seat's row. Driven through
        -- Resolve.applyEffect rather than a cast, the narrowest path that raises
        -- the prompt at all.
        Spec.it s "CR 102.1 every player in the game is offered, beside the total they hold, and a departed seat is not" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, g0) = S.addCreature piker S.alice S.threePlayerGame
              at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
              gs = g0 {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players g0)))}
              recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Integer)] r
              recording p = case p of
                Prompt.ChooseRedistribution _ _ offered -> do
                  State.put offered
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              offerOf g = List.sort (State.execState (Engine.runGame recording g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) [])
              gone = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.carol)
          Spec.assertEqWith s "all three seats, each beside its own total" (offerOf gs) [(S.alice, 27), (S.bob, 4), (S.carol, 13)]
          Spec.assertEqWith s "carol conceded, so she is no longer a candidate" (offerOf gone) [(S.alice, 27), (S.bob, 4)]
        -- Where the rules leave nothing to ask, do not ask. One candidate admits
        -- only the identity and no candidate not even that, so both are the same
        -- assignment however they are answered; two candidates is a real choice.
        Spec.it s "one remaining player leaves only the identity, so no prompt is raised" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, gs) = S.addCreature piker S.alice S.threePlayerGame
              countingAnswer :: Prompt.Prompt r -> State.State Int r
              countingAnswer p = case p of
                Prompt.ChooseRedistribution {} -> do
                  State.modify (+ 1)
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) 0
              leaves pid g = S.runPure S.identityAnswer g (Departure.leaveGame Departure.Type.Conceded pid)
              two = leaves S.carol gs
              one = leaves S.bob two
          Spec.assertEqWith s "three seats: a real decision" (asks gs) 1
          Spec.assertEqWith s "two seats: still a real decision, the swap being legal" (asks two) 1
          Spec.assertEqWith s "one seat: only the identity, so nothing to ask" (asks one) 0

-- One with the Machine, the card that proves Aggregation.Greatest (#254):
-- "Draw cards equal to the greatest mana value among artifacts you control."
-- Nothing but the fold is new -- the effect is the existing Draw, the scope and
-- the filter were both already expressible, and the per-member quantity is the
-- existing Quantity.ManaValue (CR 202.3), the same read Karn, Legacy Reforged
-- wants.
--
-- Alice's board is Bonesplitter ({1}), Serum Powder ({3}) and Mindslaver ({6}),
-- chosen so that greatest (6), count (3), sum (10) and least (1) are four
-- DIFFERENT numbers: one hand-size assertion falsifies every other fold.
greatestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
greatestSpec s registry = Spec.describe s "Greatest" $ do
  Spec.it s "CR 202.3 One with the Machine draws the GREATEST mana value, not the count, the sum or the least" $ do
    island <- S.printingOf s registry "Island"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    serumPowder <- S.printingOf s registry "Serum Powder"
    mindslaver <- S.printingOf s registry "Mindslaver"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withOne) = S.addCreature bonesplitter S.alice base
        (_, withTwo) = S.addCreature serumPowder S.alice withOne
        (_, withThree) = S.addCreature mindslaver S.alice withTwo
        withLib = stockLibrary piker S.alice 10 withThree
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- The spell left the hand as it was cast, so the hand size IS the draw.
    Spec.assertEqWith s "alice drew six" (S.handSize S.alice after) 6
  Spec.it s "CR 109.5 an opponent's larger artifact does not raise \"artifacts YOU control\"" $ do
    -- Bob's Mindslaver ({6}) is on the same battlefield and is the largest
    -- artifact in the game; Alice's own Bonesplitter ({1}) is the answer.
    island <- S.printingOf s registry "Island"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    mindslaver <- S.printingOf s registry "Mindslaver"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withMine) = S.addCreature bonesplitter S.alice base
        (_, withTheirs) = S.addCreature mindslaver S.bob withMine
        withLib = stockLibrary piker S.alice 10 withTheirs
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew one, not six" (S.handSize S.alice after) 1
  Spec.it s "CR 205.2a a larger NONARTIFACT permanent does not raise \"ARTIFACTS you control\"" $ do
    -- Panglacial Wurm is {5}{G}{G} -- mana value 7, larger than any artifact
    -- in the pool -- and Alice controls it, so only the card-type conjunct
    -- keeps it out of the fold. Her four Islands are the same falsifier at
    -- mana value 0 (CR 202.1b / 202.3a), which no maximum could ever show.
    island <- S.printingOf s registry "Island"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withArtifact) = S.addCreature bonesplitter S.alice base
        (_, withWurm) = S.addCreature panglacialWurm S.alice withArtifact
        withLib = stockLibrary piker S.alice 10 withWurm
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew one, not seven" (S.handSize S.alice after) 1
  -- The empty matched set. No rule in the CR gives a maximum over nothing a
  -- value: CR 208.2a's "use 0 instead of that number" is scoped to a
  -- characteristic-defining ability, which One with the Machine's draw is not,
  -- and where the CR does want an empty maximum to be 0 it says so card-by-card
  -- (CR 714.2d, a Saga with no chapter abilities). So the fold answers Nothing
  -- -- undeterminable, the posture this codebase propagates everywhere -- and
  -- Resolve's Draw arm draws nothing for it.
  --
  -- OBSERVATIONALLY, Nothing and 0 are the same here, and the Gatherer
  -- ruling on Rishkar's Expertise ("if you control no creatures with power
  -- greater than 0 ... you draw no cards") is what this matches either way.
  -- Pawl.CountSpec pins the distinction where it IS visible, at the fold.
  Spec.it s "CR 208.2a controlling no artifacts draws nothing rather than substituting 0" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        withLib = stockLibrary piker S.alice 10 base
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and her library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 10
  -- CR 707.2 / 613.2a: a copy's mana value is the COPIED object's, because the
  -- mana cost is one of the copiable values layer 1 replaces. The two numbers
  -- cannot coincide here: Clone is printed {3}{U} (mana value 4) and Darksteel
  -- Myr is printed {3} (mana value 3), so reading the printed card gives 4 and
  -- reading the copy gives 3.
  --
  -- The Myr is BOB's, which is what leaves the Clone alone among "artifacts YOU
  -- control" -- the maximum is then a single member and the assertion is about
  -- that member's mana value and nothing else.
  Spec.it s "CR 707.2 a Clone copying Darksteel Myr counts as mana value 3, not its own printed 4" $ do
    island <- S.printingOf s registry "Island"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withMyr) = S.addCreature darksteelMyr S.bob base
        (_, staged) = S.spellOnStack clone S.alice withMyr
        -- CR 614.12a: the copy choice happens inside the Clone's own entry, so
        -- the answerer takes the one legal target -- bob's Myr is the only
        -- creature on the battlefield.
        entered = snd (Engine.runGamePure copyTheOnlyTarget staged (Stack.resolveTop >> Engine.settleForPriority))
        -- CR 104.3c: ten cards is far more than the three this draws, so alice
        -- cannot deck herself before the assertion.
        withLib = stockLibrary piker S.alice 10 entered
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew three, not four" (S.handSize S.alice after) 3
  -- CR 202.3b, second sentence: "If a permanent or spell is a copy of the back
  -- face of a nonmodal double-faced object (even if the card representing that
  -- copy is itself a double-faced card), the mana value of the copy is 0."
  --
  -- The NUMBER is the whole of what this adds to the Darksteel Myr case above.
  -- CR 707.2 already makes the Clone read the copied object's mana value rather
  -- than its own printed {3}{U}, and CR 712.8e already makes the copied object
  -- -- a transformed Thraben Gargoyle // Stonewing Antagonizer -- read its FRONT
  -- face's {1}. Without this rule the copy inherits that 1; with it the source
  -- and the copy report DIFFERENT mana values off the same copiable snapshot,
  -- and CR 202.3b is the only thing separating them.
  --
  -- The Gargoyle is BOB's, for the Darksteel Myr case's reason: it leaves the
  -- Clone alone among "artifacts YOU control", so the maximum folds over one
  -- member and the hand size is that member's mana value and nothing else. Both
  -- faces are Artifact Creature, so the copy is in the fold whichever face was
  -- copied and the card type is not what changes.
  --
  -- 0 is a dangerous number to assert: an empty fold, a copy that never
  -- happened and a Clone left as its printed 0/0 self all draw nothing too. So
  -- the copy is IDENTIFIED before its mana value is read -- its name, card types
  -- and 4/2 body say it really is Stonewing Antagonizer under alice's control --
  -- and bob's source is asserted at 1 in the same breath, which is what shows
  -- the 0 is the copy's own answer rather than a mana value reader that broke.
  Spec.it s "CR 202.3b a Clone copying a TRANSFORMED Stonewing Antagonizer has mana value 0, not the front face's 1" $ do
    island <- S.printingOf s registry "Island"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    case cloneOfGargoyle True island gargoyle clone piker oneWithTheMachine of
      (_, [], _) -> Spec.assertFailure s "no copy on alice's battlefield"
      (source, copy : _, after) -> do
        Spec.assertEqWith s "the copy is Stonewing Antagonizer" (Projection.namesOf copy after) (Set.singleton antagonizerName)
        Spec.assertEqWith s "an artifact creature alice controls" (Projection.cardTypesOf copy after, Projection.controllerOf copy after) (Set.fromList [CardType.Artifact, CardType.Creature], Just S.alice)
        Spec.assertEqWith s "with the back face's 4/2 body" (S.powerToughnessOf copy after) (Just (4, 2))
        Spec.assertEqWith s "CR 712.8e: bob's transformed permanent still reads its front face's 1" (Filter.manaValue (Projection.viewOfObject source after)) (Just 1)
        Spec.assertEqWith s "CR 202.3b: the copy of that back face reads 0" (Filter.manaValue (Projection.viewOfObject copy after)) (Just 0)
        Spec.assertEqWith s "so alice drew nothing" (S.handSize S.alice after) 0
        Spec.assertEqWith s "and her library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 10
  -- The control, and the reason the case above is not passed by an engine that
  -- answers 0 for every copy: the SAME fixture with the Gargoyle left front-face
  -- up. CR 202.3b's second sentence is about a copy of the BACK face, so a copy
  -- of the front face keeps CR 707.2's ordinary answer -- the copied object's
  -- {1} -- and alice draws one card rather than none.
  Spec.it s "CR 707.2 a Clone copying the UNTRANSFORMED Thraben Gargoyle keeps that face's mana value" $ do
    island <- S.printingOf s registry "Island"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    case cloneOfGargoyle False island gargoyle clone piker oneWithTheMachine of
      (_, [], _) -> Spec.assertFailure s "no copy on alice's battlefield"
      (source, copy : _, after) -> do
        Spec.assertEqWith s "the copy is Thraben Gargoyle" (Projection.namesOf copy after) (Set.singleton gargoyleName)
        Spec.assertEqWith s "an artifact creature alice controls" (Projection.cardTypesOf copy after, Projection.controllerOf copy after) (Set.fromList [CardType.Artifact, CardType.Creature], Just S.alice)
        Spec.assertEqWith s "with the front face's 2/2 body" (S.powerToughnessOf copy after) (Just (2, 2))
        Spec.assertEqWith s "bob's permanent reads 1" (Filter.manaValue (Projection.viewOfObject source after)) (Just 1)
        Spec.assertEqWith s "and so does the copy of it" (Filter.manaValue (Projection.viewOfObject copy after)) (Just 1)
        Spec.assertEqWith s "so alice drew one" (S.handSize S.alice after) 1

-- The two names Thraben Gargoyle // Stonewing Antagonizer prints, for the CR
-- 202.3b pair above.
gargoyleName, antagonizerName :: CardName.CardName
gargoyleName = CardName.MkCardName (Text.pack "Thraben Gargoyle")
antagonizerName = CardName.MkCardName (Text.pack "Stonewing Antagonizer")

-- The board the two CR 202.3b cases share, differing only in `turnOver`: bob's
-- Thraben Gargoyle, turned over or not; alice's Clone copying it; then alice
-- casting One with the Machine and its draw resolving. Answers with bob's
-- permanent, the creatures alice controls, and the state after the draw.
--
-- The printings come in the order a case fetches them: Island, Thraben
-- Gargoyle, Clone, Goblin Piker, One with the Machine.
cloneOfGargoyle ::
  Bool ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
cloneOfGargoyle turnOver island gargoyle clone piker oneWithTheMachine =
  let base = S.landsInPlay island 4
      (source, withGargoyle) = S.addCreature gargoyle S.bob base
      turned = if turnOver then transformEveryCreature withGargoyle else withGargoyle
      (_, staged) = S.spellOnStack clone S.alice turned
      -- CR 614.12a: the copy choice happens inside the Clone's own entry, and
      -- bob's Gargoyle is the only creature on the battlefield to offer.
      entered = snd (Engine.runGamePure copyTheOnlyTarget staged (Stack.resolveTop >> Engine.settleForPriority))
      -- CR 400.7 minted a new id when the Clone left the stack, so the copy is
      -- found by what it IS: the only creature alice controls, her other four
      -- permanents being Islands.
      copies = filter (\oid -> Projection.isCreatureOf oid entered) (Game.zoneMembers Zone.Battlefield S.alice entered)
      -- CR 104.3c: ten cards is far more than the one this draws at most, so
      -- alice cannot deck herself before the assertion.
      withLib = stockLibrary piker S.alice 10 entered
      (gs, spellId) = S.handOne oneWithTheMachine withLib
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
      after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (source, copies, after)

-- "Transform each creature" applied straight, which is Moonmist's shape (CR
-- 701.27a) with a wider filter. Stonewing Antagonizer prints no way back and the
-- Gargoyle here is BOB's, so paying its own {6} would need a board of bob's
-- lands that says nothing about CR 202.3b.
transformEveryCreature :: GameState.GameState -> GameState.GameState
transformEveryCreature gs =
  S.runPure
    S.identityAnswer
    gs
    (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty (Effect.Transform (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature))))

-- Answers the CR 614.12a copy choice with the first legal target and delegates
-- everything else, for a fixture where exactly one creature is legal.
copyTheOnlyTarget :: Prompt.Prompt r -> r
copyTheOnlyTarget p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> Maybe.listToMaybe legal
  _ -> S.identityAnswer p

-- Aims every target slot at one creature, for a fixture with several legal ones.
targetingCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
targetingCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- Soul's Majesty, the card that proves Quantity.AgainstSlot (#1171): "Draw cards
-- equal to the power of target creature you control." The power read is the
-- TARGET's, where every other object-reading quantity is aimed at the effect's
-- SOURCE (CR 113.7) -- here a sorcery, which has no power at all, so the source
-- reading answers Nothing and draws nothing.
--
-- Alice's Thragtusk is 5/3 and her Giant Spider 2/4; bob's Panglacial Wurm is
-- 9/5. Targeting each of hers in turn separates the slot's power (5, then 2)
-- from that creature's toughness (3, then 4), from the other creature's power
-- (2, then 5), from the count of her creatures (2), from the greatest power in
-- the game (9), and from the source's nothing (0).
soulsMajestySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulsMajestySpec s registry = Spec.describe s "SoulsMajesty" $ do
  Spec.it s "CR 113.7 draws the power of the TARGET rather than of the sorcery" $ do
    forest <- S.printingOf s registry "Forest"
    thragtusk <- S.printingOf s registry "Thragtusk"
    giantSpider <- S.printingOf s registry "Giant Spider"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    soulsMajesty <- S.printingOf s registry "Soul's Majesty"
    let base = S.landsInPlay forest 5
        (tusk, withTusk) = S.addCreature thragtusk S.alice base
        (_, withSpider) = S.addCreature giantSpider S.alice withTusk
        (_, withWurm) = S.addCreature panglacialWurm S.bob withSpider
        -- CR 104.3c: twelve is far more than any reading here draws.
        withLib = stockLibrary piker S.alice 12 withWurm
        (gs, spellId) = S.handOne soulsMajesty withLib
        cast = snd (Engine.runGamePure (targetingCreature tusk) gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure (targetingCreature tusk) cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew five" (S.handSize S.alice after) 5
  Spec.it s "CR 601.2c the SAME board draws two when the other creature is the target" $ do
    forest <- S.printingOf s registry "Forest"
    thragtusk <- S.printingOf s registry "Thragtusk"
    giantSpider <- S.printingOf s registry "Giant Spider"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    soulsMajesty <- S.printingOf s registry "Soul's Majesty"
    let base = S.landsInPlay forest 5
        (_, withTusk) = S.addCreature thragtusk S.alice base
        (spider, withSpider) = S.addCreature giantSpider S.alice withTusk
        (_, withWurm) = S.addCreature panglacialWurm S.bob withSpider
        withLib = stockLibrary piker S.alice 12 withWurm
        (gs, spellId) = S.handOne soulsMajesty withLib
        cast = snd (Engine.runGamePure (targetingCreature spider) gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure (targetingCreature spider) cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew two" (S.handSize S.alice after) 2

countersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
countersSpec s registry = Spec.describe s "Counters" $ do
  Spec.it s "CR 122.6 Battlegrowth puts a +1/+1 counter (gate)" $ do
    -- alice casts Battlegrowth on bob's Piker (2/1). After resolution the Piker
    -- is 3/2 and carries one +1/+1 counter.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    let base = S.landsInPlay forest 1
        (victim, withFoe) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne battlegrowth withFoe
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "power 3" (Projection.powerOf victim after) (Just 3)
    Spec.assertEqWith s "toughness 2" (Projection.toughnessOf victim after) (Just 2)
  Spec.it s "CR 122 counter persists through cleanup (vs Giant Growth wearing off)" $ do
    -- After a cleanup step, the +1/+1 counter is still on the Piker.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    let base = S.landsInPlay forest 1
        (victim, withFoe) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne battlegrowth withFoe
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        afterCleanup = Expiry.dropAtCleanup resolved
    Spec.assertEqWith s "still 3/2 after cleanup" (Projection.powerOf victim afterCleanup) (Just 3)
    Spec.assertEqWith s "still 3/2 after cleanup" (Projection.toughnessOf victim afterCleanup) (Just 2)
  -- CR 122.1b: Spontaneous Flight is the one card where the two halves have
  -- DIFFERENT durations, which is what proves the flying is a counter rather
  -- than a second until-end-of-turn effect. The +2/+2 wears off at cleanup
  -- (CR 514.2); the flying counter does not.
  Spec.it s "CR 122.1b whole card: Spontaneous Flight pumps until EOT and grants flying for good" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    spontaneousFlight <- S.printingOf s registry "Spontaneous Flight"
    let base = S.landsInPlay plains 3
        (target, withCreature) = S.addCreature piker S.alice base
        (gs, spellId) = S.handOne spontaneousFlight withCreature
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        afterCleanup = Expiry.dropAtCleanup resolved
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying target withCreature)) "the Piker did not fly to begin with"
    Spec.assertEqWith s "pumped to 4/3" (Projection.powerOf target resolved) (Just 4)
    Spec.assertEqWith s "pumped to 4/3" (Projection.toughnessOf target resolved) (Just 3)
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying target resolved) "and it flies"
    -- The discriminator between a counter and another until-EOT effect.
    Spec.assertEqWith s "the pump wore off" (Projection.powerOf target afterCleanup) (Just 2)
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying target afterCleanup) "the flying did not"
  Spec.it s "CR 122.6 Instill Infection puts a -1/-1 counter and draws" $ do
    -- alice casts Instill Infection on bob's Piker; Piker becomes 1/0 and dies
    -- (704.5f); alice draws a card.
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    instillInfection <- S.printingOf s registry "Instill Infection"
    forest <- S.printingOf s registry "Forest"
    let base = S.landsInPlay swamp 4
        (_, withFoe) = S.addCreature piker S.bob base
        -- Baseline before Instill Infection itself enters alice's hand: casting
        -- moves that same card from hand to the stack, so measuring after it is
        -- already there would net the draw against the spell's own departure.
        handBefore = S.handSize S.alice withFoe
        (gs0, spellId) = S.handOne instillInfection withFoe
        -- put a card in alice's library so the draw has something to find.
        (_, gs) = S.addLibraryCard forest S.alice gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "Piker died to the -1/-1 counter (704.5f)" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "alice drew a card" (S.handSize S.alice after) (handBefore + 1)
  Spec.it s "CR 704.5q both counter kinds on one creature annihilate; net 2/1 survives" $ do
    -- Both counters on the same creature (placed directly); the SBA removes both.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay forest 5
        (victim, withFoe) = S.addCreature piker S.alice base
        gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
        gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs1
        after = S.settleSba gs2
    Spec.assertEqWith s "creature survives (net 2/1)" (S.creaturesInPlay S.alice after) 1
    Spec.assertEqWith s "no counters remain" (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject victim after)) Map.empty
  Spec.it s "CR 122 RemoveCounters takes counters off the slot's target" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.addCounter CounterKind.MinusOneMinusOne 2 oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 1) slot))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "one of the two counters is gone" (fmap Object.counters (Game.lookupObject oid after)) (Just (Map.singleton CounterKind.MinusOneMinusOne 1))
  -- CR 122 states no rule making the instruction fail when there are fewer
  -- counters than asked for, so it takes what is there. The kind leaves the map
  -- entirely rather than sitting at zero, which is what keeps Object.counters a
  -- tally of what is present.
  Spec.it s "CR 122 removing more counters than are present removes what is there" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.addCounter CounterKind.MinusOneMinusOne 1 oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 3) slot))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "the kind is gone, not negative" (fmap Object.counters (Game.lookupObject oid after)) (Just Map.empty)
  -- CR 608.2d over CR 608.2e's unit, on a whole card: Shed Weakness ({G} Instant,
  -- Amonkhet 185) reads "Target creature gets +2/+2 until end of turn. You may
  -- remove a -1/-1 counter from it." Two clauses, one target, and only the second
  -- clause is gated -- so the pump lands whichever way the "may" is answered.
  --
  -- The -1/-1 counter is placed directly, as the CR 704.5q case just below does:
  -- casting Instill Infection for it would add a draw and a second resolution
  -- that this case does not want in the way of what it is proving.
  Spec.it s "CR 608.2d Shed Weakness pumps either way; only the removal is optional" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    shedWeakness <- S.printingOf s registry "Shed Weakness"
    let (victim, withFoe) = S.addCreature piker S.bob (S.landsInPlay forest 1)
        withCounter = S.addCounter CounterKind.MinusOneMinusOne 1 victim withFoe
        (gs, spellId) = S.handOne shedWeakness withCounter
        -- Written out per answerer rather than through a helper taking one: a
        -- let-bound function over an answerer would need a rank-2 argument, and
        -- the neighbouring Deem Worthy case inlines them for the same reason.
        --
        -- S.identityAnswer declines every optional prompt (Script.declining), so
        -- it is the declining half unaided; exerciseOptional is its opposite.
        castDeclining = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        declined = snd (Engine.runGamePure S.identityAnswer castDeclining Stack.resolveTop)
        castExercising = snd (Engine.runGamePure exerciseOptional gs (S.cast S.alice spellId))
        exercised = snd (Engine.runGamePure exerciseOptional castExercising Stack.resolveTop)
    Spec.assertEqWith s "before: the -1/-1 counter makes the 2/2 a 1/1" (Projection.powerOf victim gs) (Just 1)
    -- The discriminator. Under a MODE-wide gate, declining would skip the pump
    -- too and this would read 1.
    Spec.assertEqWith s "declined: pumped to 3/3 anyway" (Projection.powerOf victim declined) (Just 3)
    Spec.assertEqWith s "declined: the counter is still there" (fmap Object.counters (Game.lookupObject victim declined)) (Just (Map.singleton CounterKind.MinusOneMinusOne 1))
    Spec.assertEqWith s "exercised: pumped to 4/4" (Projection.powerOf victim exercised) (Just 4)
    Spec.assertEqWith s "exercised: no counters remain" (fmap Object.counters (Game.lookupObject victim exercised)) (Just Map.empty)
  Spec.it s "CR 122.2 Unsummon removes a counter-bearing creature's counters" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (victim, withFoe) = S.addCreature piker S.bob base
        withCounter = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
        (gs, spellId) = S.handOne unsummon withCounter
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- Total (no `head`): expect exactly one bounced card in hand, empty counters.
        handCounters = fmap (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h after)) (Game.zoneMembers Zone.Hand S.bob after)
    Spec.assertEqWith s "the bounced incarnation in hand has no counters" handCounters [Map.empty]

-- CR 701.46a: "'Adapt N' means 'If this permanent has no +1/+1 counters on it,
-- put N +1/+1 counters on it.'" Sauroform Hybrid prints adapt 4 and nothing
-- else -- no other ability to reach the counters -- so the SECOND activation
-- isolates the clause gate.
--
-- The gate is on the EFFECT, not on the activation: the second activation is
-- legal, is paid for, resolves, and does nothing. `tappedCount` is what keeps
-- the negative from passing because the ability was never activated, and the
-- projected P/T is what keeps it from passing because the layer walk never saw
-- the counters.
--
-- Twelve Forests: two activations at {4}{G}{G}, so a short board cannot be the
-- reason the second one changes nothing.
sauroformHybridSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
sauroformHybridSpec s registry = Spec.describe s "SauroformHybrid" $ do
  Spec.it s "CR 701.46a whole card: adapt 4 fills an empty Hybrid, and a second adapt does nothing" $ do
    forest <- S.printingOf s registry "Forest"
    hybrid <- S.printingOf s registry "Sauroform Hybrid"
    let (hybridId, placed) = S.addCreature hybrid S.alice (S.landsInPlay forest 12)
        board = placed {GameState.priority = Just S.alice}
        adapt gs ability = S.runPure S.identityAnswer gs $ do
          Activate.activateAbility S.alice hybridId ability
          Stack.resolveTop
        countersOn gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject hybridId gs)
    case Activate.abilitiesFor hybridId board of
      [ability] -> do
        let once = adapt board ability
            twice = adapt once ability
        Spec.assertEqWith s "no counters to begin with" (countersOn board) (Just 0)
        Spec.assertEqWith s "a 2/2 to begin with" (S.powerToughnessOf hybridId board) (Just (2, 2))
        Spec.assertEqWith s "the first adapt puts four counters on" (countersOn once) (Just 4)
        Spec.assertEqWith s "and the projection reads 6/6" (S.powerToughnessOf hybridId once) (Just (6, 6))
        Spec.assertEqWith s "six Forests paid for it" (S.tappedCount S.alice once) 6
        Spec.assertEqWith s "the second adapt adds none" (countersOn twice) (Just 4)
        Spec.assertEqWith s "and it is still 6/6" (S.powerToughnessOf hybridId twice) (Just (6, 6))
        Spec.assertEqWith s "but it was activated and paid for all the same" (S.tappedCount S.alice twice) 12
        Spec.assertEqWith s "and nothing is left on the stack" (length (GameState.stack twice)) 0
      abilities -> Spec.assertFailure s ("expected one adapt ability, got " <> show (length abilities))

-- CR 701.37a: "'Monstrosity N' means 'If this permanent isn't monstrous, put N
-- +1/+1 counters on it and it becomes monstrous.'" Nessian Asp prints monstrosity
-- 4 and reach, so the SECOND activation isolates the gate the way Sauroform
-- Hybrid's does above -- legal, paid for, resolves, does nothing.
--
-- What separates this from adapt is the second case. Adapt's gate reads
-- COUNTERS; monstrosity's reads the DESIGNATION, and an Asp that was given a
-- +1/+1 counter from elsewhere is still not monstrous, so it still becomes
-- monstrous and still takes its four. An implementation that reused adapt's
-- condition passes the first case and fails that one.
--
-- Sixteen Forests: two activations at {6}{G}, so a short board cannot be the
-- reason the second one changes nothing.
nessianAspSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nessianAspSpec s registry = Spec.describe s "NessianAsp" $ do
  let monstrousOf oid gs = fmap (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject oid gs)
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
  Spec.it s "CR 701.37a whole card: monstrosity 4 marks the Asp, and a second monstrosity does nothing" $ do
    forest <- S.printingOf s registry "Forest"
    asp <- S.printingOf s registry "Nessian Asp"
    let (aspId, placed) = S.addCreature asp S.alice (S.landsInPlay forest 16)
        board = placed {GameState.priority = Just S.alice}
        monstrosity gs ability = S.runPure S.identityAnswer gs $ do
          Activate.activateAbility S.alice aspId ability
          Stack.resolveTop
    case Activate.abilitiesFor aspId board of
      [ability] -> do
        let once = monstrosity board ability
            twice = monstrosity once ability
        Spec.assertEqWith s "not monstrous to begin with" (monstrousOf aspId board) (Just False)
        Spec.assertEqWith s "a 4/5 to begin with" (S.powerToughnessOf aspId board) (Just (4, 5))
        Spec.assertEqWith s "the first monstrosity puts four counters on" (countersOn aspId once) (Just 4)
        Spec.assertEqWith s "and marks it monstrous" (monstrousOf aspId once) (Just True)
        Spec.assertEqWith s "and the projection reads 8/9" (S.powerToughnessOf aspId once) (Just (8, 9))
        Spec.assertEqWith s "seven Forests paid for it" (S.tappedCount S.alice once) 7
        Spec.assertEqWith s "the second monstrosity adds none" (countersOn aspId twice) (Just 4)
        Spec.assertEqWith s "and it is still 8/9" (S.powerToughnessOf aspId twice) (Just (8, 9))
        Spec.assertEqWith s "but it was activated and paid for all the same" (S.tappedCount S.alice twice) 14
        Spec.assertEqWith s "and nothing is left on the stack" (length (GameState.stack twice)) 0
      abilities -> Spec.assertFailure s ("expected one monstrosity ability, got " <> show (length abilities))
  -- CR 701.37b's designation, not CR 701.46a's counter count: the two gates agree
  -- on every board where the only counters are monstrosity's own, and this is the
  -- board where they part.
  Spec.it s "CR 701.37a the gate reads the designation, so counters from elsewhere do not stop it" $ do
    forest <- S.printingOf s registry "Forest"
    asp <- S.printingOf s registry "Nessian Asp"
    let (aspId, placed) = S.addCreature asp S.alice (S.landsInPlay forest 16)
        board = (S.addCounter CounterKind.PlusOnePlusOne 1 aspId placed) {GameState.priority = Just S.alice}
    case Activate.abilitiesFor aspId board of
      [ability] -> do
        let after = S.runPure S.identityAnswer board $ do
              Activate.activateAbility S.alice aspId ability
              Stack.resolveTop
        Spec.assertEqWith s "one counter on it, and not monstrous" (countersOn aspId board, monstrousOf aspId board) (Just 1, Just False)
        Spec.assertEqWith s "monstrosity still puts its four on" (countersOn aspId after) (Just 5)
        Spec.assertEqWith s "and still marks it monstrous" (monstrousOf aspId after) (Just True)
        Spec.assertEqWith s "so it reads 9/10" (S.powerToughnessOf aspId after) (Just (9, 10))
      abilities -> Spec.assertFailure s ("expected one monstrosity ability, got " <> show (length abilities))
  -- CR 701.37b: "once a permanent becomes monstrous, it stays monstrous until it
  -- leaves the battlefield". The designation is per-incarnation state, so CR
  -- 400.7's new object has none -- the same reading Object.newIncarnation gives
  -- counters, which the Unsummon case above proves for CR 122.2.
  Spec.it s "CR 701.37b the designation leaves with the permanent" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    asp <- S.printingOf s registry "Nessian Asp"
    unsummon <- S.printingOf s registry "Unsummon"
    let (aspId, placed) = S.addCreature asp S.alice (S.landsInPlay forest 16)
        (_, withIsland) = S.addCreature island S.alice placed
        board = withIsland {GameState.priority = Just S.alice}
    case Activate.abilitiesFor aspId board of
      [ability] -> do
        let once = S.runPure S.identityAnswer board $ do
              Activate.activateAbility S.alice aspId ability
              Stack.resolveTop
            (withSpell, spellId) = S.handOne unsummon once
            bounced = S.runPure S.identityAnswer withSpell $ do
              S.cast S.alice spellId
              Stack.resolveTop
            -- Total (no `head`): the Asp is the only card that can be in hand.
            inHand = fmap (\h -> maybe True (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject h bounced)) (Game.zoneMembers Zone.Hand S.alice bounced)
        Spec.assertEqWith s "monstrous on the battlefield" (monstrousOf aspId once) (Just True)
        Spec.assertEqWith s "the bounced incarnation is not monstrous" inHand [False]
      abilities -> Spec.assertFailure s ("expected one monstrosity ability, got " <> show (length abilities))

untapSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
untapSpec s registry = Spec.describe s "Untap" $ do
  Spec.it s "CR 701.26b Untap untaps the slot's target" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.tapObject oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.Untap (ObjectRef.InSlot slot))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "target is untapped" (fmap Object.tapped (Game.lookupObject oid after)) (Just TapState.Untapped)

gainControlSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gainControlSpec s registry = Spec.describe s "GainControl" $ do
  Spec.it s "GainControl gives the source's controller control until end of turn and re-Sicks (CR 302.6)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        slot = SlotName.MkSlotName (Text.pack "target")
        -- Apply as though a spell alice controls (controller = alice) resolved it.
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.GainControl (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot slot)))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "alice now controls it" (Projection.controllerOf oid after) (Just S.alice)
    Spec.assertEqWith s "it is summoning sick for the new controller" (fmap Object.sickness (Game.lookupObject oid after)) (Just Sickness.Sick)
    Spec.assertEqWith s "control reverts after cleanup" (Projection.controllerOf oid (Expiry.dropAtCleanup after)) (Just S.bob)
  -- CR 302.6 asks whether control was CONTINUOUS. Gaining control of a
  -- permanent you already control interrupts nothing, so the clock must not
  -- reset. The sibling case above is the one where it must.
  --
  -- Isolated from haste on purpose: Act of Treason is the card that reaches
  -- this, and it grants haste, which would mask the difference on the ability
  -- path. Driving Effect.GainControl directly shows the sickness itself.
  Spec.it s "CR 302.6 GainControl does NOT re-Sick a permanent its controller already controlled" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer base (Engine.settleAll S.alice)
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.GainControl (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot slot)))
        after = snd (Engine.runGamePure S.identityAnswer settled run)
    Spec.assertEqWith s "alice controlled it before" (Projection.controllerOf oid settled) (Just S.alice)
    Spec.assertEqWith s "and still does" (Projection.controllerOf oid after) (Just S.alice)
    Spec.assertEqWith s "its settle under alice is untouched" (fmap Object.sickness (Game.lookupObject oid after)) (Just (Sickness.Settled S.alice))

gainPlayerCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gainPlayerCountersSpec s registry = Spec.describe s "GainPlayerCounters" $ do
  Spec.it s "CR 107.14 GainPlayerCounters gives the resolving controller energy" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        act = Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2)))
        after = S.runPure S.identityAnswer gs0 act
    Spec.assertEqWith s "alice has two energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2
  -- CR 122.1: `EachPlayer` on GainPlayerCounters had no card producer until
  -- Ichor Rats ({1}{B}{B} Creature -- Phyrexian Rat 2/1, "Infect. When this
  -- creature enters, each player gets a poison counter."), and design.md
  -- section 4 says an implemented, unproven arm is not done. This case is
  -- what proves it.
  --
  -- THREE seats, and the caster's own counter is the discriminator. At two
  -- seats `EachPlayer` and `Relative Opponent` differ only in whether the
  -- caster is included, so a single wrong `Opponent` authoring would be
  -- invisible against the Prologue to Phyresis cases (which prove the
  -- `Relative Opponent` arm, in proliferateSpec below) -- alice holding a
  -- poison counter is the one reading `Relative Opponent` cannot produce, at
  -- any number of seats. The counts are asserted as one tuple because every
  -- number here is 1: three separate checks would let a partial answer look
  -- like a coincidence rather than a failure.
  Spec.it s "CR 122.1 whole card: Ichor Rats poisons all three players, the caster included" $ do
    swamp <- S.printingOf s registry "Swamp"
    ichorRats <- S.printingOf s registry "Ichor Rats"
    -- Three Swamps for the {1}{B}{B}. S.landsInPlay builds its own two-seat
    -- game, so a three-seat board adds them one at a time instead.
    let withMana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (3 :: Int)]
        (gs, spellId) = S.handOne ichorRats withMana
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
        poisonIn g = (S.playerCounterOf PlayerCounterKind.Poison S.alice g, S.playerCounterOf PlayerCounterKind.Poison S.bob g, S.playerCounterOf PlayerCounterKind.Poison S.carol g)
    -- Nobody is poisoned before the Rats resolve, so the 1s below are the
    -- effect's doing rather than the fixture's.
    Spec.assertEqWith s "the table starts clean" (poisonIn gs) (0, 0, 0)
    Spec.assertEqWith s "the Rats resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ichor Rats") S.alice settled) 1
    Spec.assertEqWith s "alice, bob and carol each got one" (poisonIn settled) (1, 1, 1)
  -- The gameplay-level consequence: CR 704.5c's tenth poison counter. carol
  -- sits on nine, so the counter `EachPlayer` hands her is the one that loses
  -- her the game -- and alice, the caster, is poisoned in the same resolution
  -- without reaching ten.
  Spec.it s "CR 704.5c Ichor Rats' counter is carol's tenth, and she loses the game" $ do
    swamp <- S.printingOf s registry "Swamp"
    ichorRats <- S.printingOf s registry "Ichor Rats"
    let withMana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (3 :: Int)]
        nearlyDead = S.addPlayerCounter PlayerCounterKind.Poison 9 S.carol withMana
        (gs, spellId) = S.handOne ichorRats nearlyDead
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
        statusOf pid = fmap Player.status (Map.lookup pid (GameState.players settled))
    Spec.assertEqWith s "carol reached ten" (S.playerCounterOf PlayerCounterKind.Poison S.carol settled) 10
    Spec.assertEqWith s "and lost the game" (statusOf S.carol) (Just (Status.Departed Departure.Type.Lost))
    Spec.assertEqWith s "alice, who cast it, is poisoned but playing" (S.playerCounterOf PlayerCounterKind.Poison S.alice settled, statusOf S.alice) (1, Just Status.Playing)

-- Answers Prompt.ChooseProliferate by taking everything on offer. Its sibling
-- declines everything: between them the tests prove the ANSWER decides who gets
-- counters, rather than the order the candidates happen to be enumerated in.
proliferatesAll :: Prompt.Prompt r -> r
proliferatesAll p = case p of
  Prompt.ChooseProliferate _ _ oids pids -> (Set.fromList oids, Set.fromList pids)
  _ -> S.identityAnswer p

proliferatesNothing :: Prompt.Prompt r -> r
proliferatesNothing p = case p of
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  _ -> S.identityAnswer p

-- Resolve one Proliferate for alice against `gs`, answered by `answer`.
proliferate :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
proliferate answer src gs =
  S.runPure answer gs (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.Proliferate)

proliferateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
proliferateSpec s registry = Spec.describe s "Proliferate" $ do
  -- CR 701.34a: "give each one additional counter of each kind that permanent
  -- or player already has." One more, never a doubling, and never a kind that
  -- was not already there.
  Spec.it s "CR 701.34a proliferate adds exactly one counter of a kind already there" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.addCounter CounterKind.PlusOnePlusOne 2 src g0
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "two became three" (S.counterOf CounterKind.PlusOnePlusOne src after) 3
  -- "each kind" is the clause a naive implementation drops: a creature holding
  -- both kinds gets one more of BOTH, not one of whichever was found first.
  --
  -- Holding both kinds at once is a state CR 704.5q would annihilate on the
  -- next state-based-action pass, which is exactly why this drives the opcode
  -- directly instead of resolving a spell: the question here is what
  -- Proliferate does to the counters it finds, not what survives afterwards.
  Spec.it s "CR 701.34a a permanent with two kinds gets one more of each" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addCounter CounterKind.PlusOnePlusOne 1 src g0
        gs = S.addCounter CounterKind.MinusOneMinusOne 3 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "+1/+1 went up" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
    Spec.assertEqWith s "-1/-1 went up too" (S.counterOf CounterKind.MinusOneMinusOne src after) 4
  -- CR 701.34a: only permanents "that have a counter" are choosable, so a bare
  -- permanent is never offered and never gains a first counter this way.
  Spec.it s "CR 701.34a a permanent with no counters is not a candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (bare, g1) = S.addCreature piker S.alice g0
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "the bare Piker gained nothing" (S.counterOf CounterKind.PlusOnePlusOne bare after) 0
    Spec.assertEqWith s "the countered one moved" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
  -- CR 102.2 / 109.5: `Relative Opponent` on GainPlayerCounters had no card
  -- producer until Prologue to Phyresis. The arm was implemented and
  -- unproven, which design.md section 4 says is not done; these cases are
  -- what prove it.
  Spec.it s "CR 122.1 whole card: Prologue to Phyresis poisons the opponent, not the caster" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    prologueToPhyresis <- S.printingOf s registry "Prologue to Phyresis"
    let base = S.landsInPlay island 2
        (_, withLibrary) = S.addLibraryCard piker S.alice base
        handBefore = S.handSize S.alice withLibrary
        (gs, spellId) = S.handOne prologueToPhyresis withLibrary
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob is poisoned" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
    Spec.assertEqWith s "alice is not" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
    Spec.assertEqWith s "and alice drew" (S.handSize S.alice after) (handBefore + 1)
  -- The discriminator, and it needs a THIRD seat: at two players `Relative
  -- Opponent` and `EachPlayer` differ only in whether the caster is included,
  -- which the case above catches -- but `Opponent` reaching only ONE of two
  -- opponents would still pass there. CR 806.1: in a Free-for-All the
  -- players compete as individuals, so every other player is an opponent and
  -- both must be poisoned. (CR 102.2 is the TWO-player rule, which is
  -- exactly what a third seat is here to get past.)
  Spec.it s "CR 806.1 at three seats every opponent is poisoned, and only opponents" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    prologueToPhyresis <- S.printingOf s registry "Prologue to Phyresis"
    let (_, withLibrary) = S.addLibraryCard piker S.alice S.threePlayerGame
        -- Two Islands for the {1}{U}. S.landsInPlay builds its own two-seat
        -- game, so a three-seat board adds them one at a time instead.
        withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withLibrary [1 .. (2 :: Int)]
        (gs, spellId) = S.handOne prologueToPhyresis withMana
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- No separate "the fixture is payable" assertion: an unpayable cast is a
    -- no-op, so the poison counts below are what prove it resolved.
    Spec.assertEqWith s "bob poisoned" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
    Spec.assertEqWith s "carol poisoned too" (S.playerCounterOf PlayerCounterKind.Poison S.carol after) 1
    Spec.assertEqWith s "alice untouched" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  -- CR 102.1 / CR 800.4a: an opponent is one of the OTHER people in the
  -- game, and carol is no longer one of them (#279). Poison on a departed
  -- player's record is not idle bookkeeping -- the proliferate case below
  -- reads Player.counters to build its candidate list, so this is the write
  -- that would put a non-player on the next prompt.
  Spec.it s "CR 800.4a Prologue to Phyresis does not poison a player who has left the game" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    prologueToPhyresis <- S.printingOf s registry "Prologue to Phyresis"
    let (_, withLibrary) = S.addLibraryCard piker S.alice S.threePlayerGame
        withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withLibrary [1 .. (2 :: Int)]
        (gs0, spellId) = S.handOne prologueToPhyresis withMana
        gs = Departure.depart Departure.Type.Conceded S.carol gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob, still in the game, is poisoned" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
    Spec.assertEqWith s "carol, who left, is not" (S.playerCounterOf PlayerCounterKind.Poison S.carol after) 0
    Spec.assertEqWith s "and neither is the caster" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  -- CR 701.34a: players carry counters too, and proliferate reaches them.
  Spec.it s "CR 701.34a proliferate adds to a player's poison and energy" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g0
        gs = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "bob's poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 4
    Spec.assertEqWith s "alice's energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2
  -- A player with no counters is not a candidate, the same clause the bare
  -- permanent above tests -- so proliferate never starts someone on poison.
  Spec.it s "CR 701.34a a player with no counters is not a candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.addPlayerCounter PlayerCounterKind.Poison 2 S.bob g0
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "alice stays clean" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  -- CR 102.1: proliferate reaches "any number of permanents and/or PLAYERS",
  -- and a player is one of the people in the game -- so a departed seat is
  -- not a candidate (#279). This is the case that made the filter worth
  -- writing rather than deferring again: CR 800.4a removes a departing
  -- player's OBJECTS, and a player counter is not an object (CR 109.1), so
  -- carol's poison is still sitting on her record for kindsFor to find. The
  -- engine would offer someone who is not in the game as a choice, which is
  -- the second invariant's other half -- where the rules leave nothing to
  -- ask, do not ask.
  --
  -- proliferatesAll takes everything offered, so the assertion is exactly
  -- "carol was not offered". bob is the discriminator: he is poisoned too and
  -- still in the game, so a filter that dropped every player would fail here.
  Spec.it s "CR 800.4a a player who has left the game is not a proliferate candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice S.threePlayerGame
        g1 = S.addPlayerCounter PlayerCounterKind.Poison 2 S.bob g0
        g2 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.carol g1
        gs = Departure.depart Departure.Type.Conceded S.carol g2
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "carol has left, so her poison does not move" (S.playerCounterOf PlayerCounterKind.Poison S.carol after) 3
    Spec.assertEqWith s "bob is still in the game, so his does" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
  -- CR 701.34a: "any number" includes none. The discriminating twin of the
  -- first test -- same board, opposite answer -- so this fails if the engine
  -- proliferates for the player instead of asking.
  Spec.it s "CR 701.34a choosing nothing is legal and adds nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addCounter CounterKind.PlusOnePlusOne 2 src g0
        gs = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g1
        after = proliferate proliferatesNothing src gs
    Spec.assertEqWith s "the creature is untouched" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
    Spec.assertEqWith s "bob is untouched" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
  -- The counter placement rides Event.putCounters, so CR 614's counter
  -- replacements get their opportunity -- proliferate is not a side door that
  -- bypasses Hardened Scales.
  Spec.it s "CR 614 Hardened Scales applies to the counter proliferate adds" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addCreature hardenedScales S.alice g0
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "one proliferated counter became two" (S.counterOf CounterKind.PlusOnePlusOne src after) 3
  -- Where the rules leave nothing to ask, do not ask: no permanent and no
  -- player holds a counter, so there is no choice to make.
  Spec.it s "CR 701.34a an empty candidate set raises no prompt" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseProliferate {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.Proliferate)) 0
    Spec.assertEqWith s "nobody has a counter: nothing to ask" (asks gs) 0
    Spec.assertEqWith s "someone does: one real decision" (asks (S.addCounter CounterKind.PlusOnePlusOne 1 src gs)) 1
  -- The gameplay-level proof (design.md section 4): a real card, cast and
  -- resolved, doing both halves of its text.
  Spec.it s "Steady Progress whole card: proliferate, then draw a card" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    steadyProgress <- S.printingOf s registry "Steady Progress"
    let base = S.landsInPlay island 3
        (creature, g1) = S.addCreature piker S.alice base
        g2 = S.addCounter CounterKind.PlusOnePlusOne 1 creature g1
        -- Something to draw: an empty library would make the draw a no-op
        -- (and a CR 104.3c loss), hiding whether the effect ran at all.
        (_, g3) = S.addLibraryCard island S.alice g2
        (withSpell, spell) = S.handOne steadyProgress g3
        handBefore = length (Game.zoneMembers Zone.Hand S.alice withSpell)
        afterCast = S.runPure proliferatesAll withSpell (S.cast S.alice spell)
        resolved = S.runPure proliferatesAll afterCast Stack.resolveTop
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the counter was proliferated" (S.counterOf CounterKind.PlusOnePlusOne creature resolved) 2
    -- The spell left the hand and one card was drawn, so the hand is level.
    Spec.assertEqWith s "drew a card" (length (Game.zoneMembers Zone.Hand S.alice resolved)) handBefore

-- CR 701.22a: "to 'scry N' means to look at the top N cards of your library,
-- then put any number of them on the bottom of your library in any order and
-- the rest on top of your library in any order."
--
-- Crystal Ball ({3} Artifact, "{1}, {T}: Scry 2") is the producer, and scry TWO
-- is what lets this group discriminate at all: scry 1 cannot tell "any number
-- to the bottom" from all-or-nothing, and neither end's ORDER is a question
-- when only one card can reach it.
--
-- Four DIFFERENT printings in alice's library, top-first [piker, maiden,
-- mountain, forest]. Interchangeable cards could not tell "put back in the
-- chosen order" from "put back in the order they were found", which is exactly
-- the reading a scry that ignored its answer would produce.
--
-- `stock` is how many of them to deal, taken from the TOP so a shorter library
-- keeps the same top cards and the elision pair below differs in one card only.
scryBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Int ->
  m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
scryBoard s registry stock = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  crystalBall <- S.printingOf s registry "Crystal Ball"
  let (ballId, placed) = S.addCreature crystalBall S.alice (S.landsInPlay forest 4)
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deck = reverse (take stock [piker, maiden, mountain, forest])
      deal (acc, gs) printing = let (oid, gs') = S.addLibraryCard printing S.alice gs in (oid : acc, gs')
      (ids, stocked) = List.foldl' deal ([], placed) deck
  pure (ids, ballId, stocked {GameState.priority = Just S.alice})

-- Answers Prompt.ChooseScry with a FIXED pair of lists, whatever the engine
-- offers. Pinned rather than derived from the offered list: an answerer that
-- searched what it was handed for a legal pick would find the right cards again
-- after a mutation broke which cards the engine looked at, and this group would
-- stay green over a broken choice.
scryAnswer :: ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
scryAnswer split p = case p of
  Prompt.ChooseScry {} -> split
  _ -> S.identityAnswer p

-- Activates Crystal Ball's one activated ability and resolves it. A board
-- offering any other number of abilities activates none, leaving the state
-- untouched -- which fails every assertion below rather than passing one for a
-- reason the case did not choose.
runScry ::
  (forall r. Prompt.Prompt r -> r) ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  GameState.GameState
runScry answer ballId gs = case Activate.abilitiesFor ballId gs of
  [ability] -> S.runPure answer gs $ do
    Activate.activateAbility S.alice ballId ability
    Stack.resolveTop
  _ -> gs

scryLibrary :: GameState.GameState -> [ObjectId.ObjectId]
scryLibrary = Game.zoneMembers Zone.Library S.alice

scrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
scrySpec s registry = Spec.describe s "Scry" $ do
  -- The SPLIT, which scry 1 cannot reach: one looked-at card goes under and the
  -- other stays on top, so neither "all of them" nor "none of them" produces
  -- this library.
  Spec.it s "CR 701.22a whole card: Crystal Ball's scry 2 bottoms one and keeps one" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([piker], [maiden])) ballId board
        Spec.assertEqWith s "the library started top-first piker, maiden, mountain, forest" (scryLibrary board) [piker, maiden, mountain, forest]
        Spec.assertEqWith s "the kept card is on top and the bottomed one is last" (scryLibrary after) [maiden, mountain, forest, piker]
        Spec.assertEqWith s "the ability left the stack" (length (GameState.stack after)) 0
      _ -> Spec.assertFailure s "expected four library cards"
  -- CR 701.22a's "the rest on top of your library IN ANY ORDER": both cards stay
  -- on top, swapped. A scry that put them back in the order it found them
  -- leaves the library untouched, which is the reading this case rules out.
  Spec.it s "CR 701.22a the kept cards go back in the CHOSEN order, not the order they were in" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([], [maiden, piker])) ballId board
        Spec.assertEqWith s "the top two are swapped and the rest is untouched" (scryLibrary after) [maiden, piker, mountain, forest]
      _ -> Spec.assertFailure s "expected four library cards"
  -- The other "in any order", on the bottom half: both go under, in an order
  -- that is not the order they were looked at in.
  Spec.it s "CR 701.22a the bottomed cards go under in the CHOSEN order too" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([maiden, piker], [])) ballId board
        Spec.assertEqWith s "mountain and forest rose, maiden above piker beneath them" (scryLibrary after) [mountain, forest, maiden, piker]
      _ -> Spec.assertFailure s "expected four library cards"
  -- A card the answer names in NEITHER list still has to end up somewhere, and
  -- an effect has no way to reject an answer -- Effect.Discard's completion
  -- posture. It stays on top, behind the one that was named.
  Spec.it s "CR 701.22a a looked-at card the answer never names stays on top" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([], [maiden])) ballId board
        Spec.assertEqWith s "maiden was named and piker fell in behind it" (scryLibrary after) [maiden, piker, mountain, forest]
      _ -> Spec.assertFailure s "expected four library cards"
  -- Rule 701.22 states no penalty for scrying more cards than there are, unlike
  -- CR 104.3c's draw: a two-card library is looked at whole and still split.
  Spec.it s "CR 701.22a a library shorter than the count is looked at as far as it goes" $ do
    (ids, ballId, board) <- scryBoard s registry 2
    case ids of
      [piker, maiden] -> do
        let after = runScry (scryAnswer ([piker], [maiden])) ballId board
        Spec.assertEqWith s "the whole library was looked at" (scryLibrary board) [piker, maiden]
        Spec.assertEqWith s "and the answer swapped it" (scryLibrary after) [maiden, piker]
      _ -> Spec.assertFailure s "expected two library cards"

-- The elision half. Each case counts the scry prompts one activation raises,
-- and the two-card board is the one-card board's PAIR: same seats, same mana,
-- same ability, one more card in the library.
scryPromptSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
scryPromptSpec s registry = Spec.describe s "ScryPrompt" $ do
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseScry {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      asks ballId gs = case Activate.abilitiesFor ballId gs of
        [ability] ->
          State.execState
            ( Engine.runGame counting gs $ do
                Activate.activateAbility S.alice ballId ability
                Stack.resolveTop
            )
            0
        -- Negative, so a board that could not activate at all fails every case
        -- rather than passing the two that expect no prompt.
        _ -> -1
  -- Nothing to LOOK at. CR 701.22a's process has no cards to run on, so there is
  -- no question to put.
  Spec.it s "CR 701.22a an empty library raises no scry prompt" $ do
    (_, ballId, board) <- scryBoard s registry 0
    Spec.assertEqWith s "not asked" (asks ballId board) 0
  -- Nothing to DECIDE: one card that IS the whole library. Its top and its
  -- bottom are the same position, so both answers produce the same library and
  -- declining to ask takes no choice away from the player.
  Spec.it s "CR 701.22a one card that is the whole library raises no scry prompt" $ do
    (ids, ballId, board) <- scryBoard s registry 1
    let after = runScry (scryAnswer ([], [])) ballId board
    Spec.assertEqWith s "not asked" (asks ballId board) 0
    Spec.assertEqWith s "and the library is what it was" (scryLibrary after) ids
  -- The pair's other half, one card deeper: with something beneath it the top
  -- card is a real top-or-bottom question, so it IS asked -- and the answer is
  -- honoured, which is what separates "asked" from "asked and ignored".
  Spec.it s "CR 701.22a a second card beneath makes it a real choice, and it is asked" $ do
    (ids, ballId, board) <- scryBoard s registry 2
    case ids of
      [piker, maiden] -> do
        let after = runScry (scryAnswer ([maiden, piker], [])) ballId board
        Spec.assertEqWith s "asked once" (asks ballId board) 1
        Spec.assertEqWith s "and both went under, maiden above piker" (scryLibrary after) [maiden, piker]
      _ -> Spec.assertFailure s "expected two library cards"
  -- CR 701.22b: "if a player is instructed to scry 0, no scry event occurs."
  -- Driven through the opcode rather than a card, no printing scrying zero and
  -- Crystal Ball's count being fixed at two.
  Spec.it s "CR 701.22b scry 0 raises no prompt and moves nothing" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    let scryZero = Effect.Scry (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 0))
        zero = Resolve.applyEffect ballId ballId S.alice Map.empty Map.empty scryZero
        asked = State.execState (Engine.runGame counting board zero) 0
        after = S.runPure (scryAnswer ([], [])) board zero
    Spec.assertEqWith s "not asked" asked 0
    Spec.assertEqWith s "and the library is what it was" (scryLibrary after) ids

-- CR 701.25a: "to 'surveil N' means to look at the top N cards of your library,
-- then put any number of them into your graveyard and the rest on top of your
-- library in any order."
--
-- Curate ({1}{U} instant, "Surveil 2. Draw a card.") is the producer, cast for
-- real: surveil TWO for the reason the scry group takes two, and the draw is
-- what makes the kept ORDER observable from outside the library -- whichever
-- card the answer left on top is the card that ends up in hand.
--
-- Four DIFFERENT printings in alice's library, top-first [piker, maiden,
-- mountain, forest]. Interchangeable cards could not tell a chosen order from
-- the order they were found in, and could not tell a graveyard arrival from any
-- other.
surveilBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
surveilBoard s registry = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  curate <- S.printingOf s registry "Curate"
  let deal (acc, g) printing = let (oid, g') = S.addLibraryCard printing S.alice g in (oid : acc, g')
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first and
      -- `ids` comes back top-first.
      (ids, stocked) = List.foldl' deal ([], S.landsInPlay island 2) [forest, mountain, maiden, piker]
      (board, spellId) = S.handOne curate stocked
  pure (ids, spellId, board)

-- Answers Prompt.ChooseSurveil with a FIXED pair of lists, scryAnswer's posture
-- and for its reason: an answerer that searched the offered list for a legal
-- pick would repair the assertion after a mutation broke which cards the engine
-- looked at.
surveilAnswer :: ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
surveilAnswer split p = case p of
  Prompt.ChooseSurveil {} -> split
  _ -> S.identityAnswer p

-- The card names in alice's graveyard, bottom-first (Pawl.Engine.Game's arrival
-- end), which is the only way to read a graveyard arrival: CR 400.7 minted a
-- fresh id for it, so the id the prompt named is not the id that landed.
surveilGraveyard :: GameState.GameState -> [Maybe CardName.CardName]
surveilGraveyard gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Graveyard S.alice gs)

cardNamed :: String -> Maybe CardName.CardName
cardNamed = Just . CardName.MkCardName . Text.pack

surveilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
surveilSpec s registry = Spec.describe s "Surveil" $ do
  -- The SPLIT: one looked-at card into the graveyard, the other kept, and then
  -- Curate's own draw takes the kept one. A surveil that BOTTOMED the unwanted
  -- card instead -- CR 701.22a's scry, the neighbouring reading -- would leave
  -- piker under forest and the graveyard holding nothing but Curate.
  Spec.it s "CR 701.25a whole card: Curate's surveil 2 bins one, keeps one, then draws it" $ do
    (ids, spellId, board) <- surveilBoard s registry
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = S.runPure (surveilAnswer ([piker], [maiden])) board $ do
              S.cast S.alice spellId
              Stack.resolveTop
        Spec.assertEqWith s "the library started top-first piker, maiden, mountain, forest" (Game.zoneMembers Zone.Library S.alice board) [piker, maiden, mountain, forest]
        Spec.assertEqWith s "maiden was drawn off the top, leaving mountain and forest" (Game.zoneMembers Zone.Library S.alice after) [mountain, forest]
        Spec.assertEqWith
          s
          "and the drawn card is the one surveil kept"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Hand S.alice after))
          [cardNamed "Bird Maiden"]
        -- Curate follows its own surveilled card in, CR 608.2n putting the spell
        -- into its owner's graveyard as the final part of its resolution.
        Spec.assertEqWith s "piker is in the graveyard, under Curate" (surveilGraveyard after) [cardNamed "Goblin Piker", cardNamed "Curate"]
      _ -> Spec.assertFailure s "expected four library cards"
  -- CR 701.25a's "the rest on top of your library IN ANY ORDER": nothing is
  -- binned and the two looked-at cards go back swapped, so the draw takes the
  -- card that was SECOND. A surveil that put them back as it found them draws
  -- piker instead.
  Spec.it s "CR 701.25a the kept cards go back in the CHOSEN order" $ do
    (ids, spellId, board) <- surveilBoard s registry
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = S.runPure (surveilAnswer ([], [maiden, piker])) board $ do
              S.cast S.alice spellId
              Stack.resolveTop
        Spec.assertEqWith s "piker fell to second and was left there by the draw" (Game.zoneMembers Zone.Library S.alice after) [piker, mountain, forest]
        Spec.assertEqWith
          s
          "maiden was on top, so maiden was drawn"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Hand S.alice after))
          [cardNamed "Bird Maiden"]
        Spec.assertEqWith s "and nothing but the spell reached the graveyard" (surveilGraveyard after) [cardNamed "Curate"]
      _ -> Spec.assertFailure s "expected four library cards"
  -- "Any number" reaching ALL of them, and the graveyard's own order: the answer
  -- names maiden first, so maiden is put in first and ends up UNDER piker.
  Spec.it s "CR 701.25a both looked-at cards can go, in the order the answer names them" $ do
    (ids, spellId, board) <- surveilBoard s registry
    case ids of
      [piker, maiden, _, forest] -> do
        let after = S.runPure (surveilAnswer ([maiden, piker], [])) board $ do
              S.cast S.alice spellId
              Stack.resolveTop
        Spec.assertEqWith s "mountain rose to the top and was drawn, leaving forest" (Game.zoneMembers Zone.Library S.alice after) [forest]
        Spec.assertEqWith
          s
          "the draw took mountain, the card that was third"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Hand S.alice after))
          [cardNamed "Mountain"]
        Spec.assertEqWith s "maiden went in first, so piker sits on top of it" (surveilGraveyard after) [cardNamed "Bird Maiden", cardNamed "Goblin Piker", cardNamed "Curate"]
      _ -> Spec.assertFailure s "expected four library cards"

-- The elision half, driven through the opcode: Curate's count is fixed at two,
-- and casting it on a one-card library would deck alice (CR 104.3c) before the
-- assertion could read anything.
--
-- alice has a Piker on the battlefield to apply the effect from, and `stock`
-- distinct cards in her library, top-first.
surveilOpcodeBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Int ->
  m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
surveilOpcodeBoard s registry stock = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  let (sourceId, base) = S.addCreature piker S.alice (S.landsInPlay island 1)
      deal (acc, gs) printing = let (oid, gs') = S.addLibraryCard printing S.alice gs in (oid : acc, gs')
      (ids, stocked) = List.foldl' deal ([], base) (reverse (take stock [maiden, mountain]))
  pure (ids, sourceId, stocked {GameState.priority = Just S.alice})

surveilPromptSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
surveilPromptSpec s registry = Spec.describe s "SurveilPrompt" $ do
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseSurveil {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      surveilTwo = Effect.Surveil (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
      apply effect sourceId = Resolve.applyEffect sourceId sourceId S.alice Map.empty Map.empty effect
      asks effect sourceId gs = State.execState (Engine.runGame counting gs (apply effect sourceId)) 0
  -- Nothing to LOOK at, the one case rule 701.25a's process cannot run on.
  Spec.it s "CR 701.25a an empty library raises no surveil prompt" $ do
    (_, sourceId, board) <- surveilOpcodeBoard s registry 0
    Spec.assertEqWith s "not asked" (asks surveilTwo sourceId board) 0
  -- The case that separates surveil from scry, and the reason this pair exists:
  -- with ONE card that is the whole library, Pawl.Engine.Resolve.scryOne asks
  -- nothing because top and bottom are the same position -- but a graveyard is
  -- somewhere else, so the player IS asked, and the answer is honoured.
  Spec.it s "CR 701.25a one card that is the whole library is still a real choice" $ do
    (ids, sourceId, board) <- surveilOpcodeBoard s registry 1
    let after = S.runPure (surveilAnswer (ids, [])) board (apply surveilTwo sourceId)
    Spec.assertEqWith s "asked once" (asks surveilTwo sourceId board) 1
    Spec.assertEqWith s "and the card it named left the library" (Game.zoneMembers Zone.Library S.alice after) []
    Spec.assertEqWith s "for the graveyard" (surveilGraveyard after) [cardNamed "Bird Maiden"]
  -- CR 701.25c: "if a player is instructed to surveil 0, no surveil event
  -- occurs." Driven through the opcode, no printing surveilling zero.
  Spec.it s "CR 701.25c surveil 0 raises no prompt and moves nothing" $ do
    (ids, sourceId, board) <- surveilOpcodeBoard s registry 2
    let surveilZero = Effect.Surveil (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 0))
        after = S.runPure (surveilAnswer ([], [])) board (apply surveilZero sourceId)
    Spec.assertEqWith s "not asked" (asks surveilZero sourceId board) 0
    Spec.assertEqWith s "and the library is what it was" (Game.zoneMembers Zone.Library S.alice after) ids
    Spec.assertEqWith s "with an empty graveyard" (surveilGraveyard after) []

-- CR 701.29a: "to 'fateseal N' means to look at the top N cards of an opponent's
-- library, then put any number of them on the bottom of that library in any
-- order and the rest on top of that library in any order."
--
-- Spin into Myth ({4}{U} instant, "Put target creature on top of its owner's
-- library, then fateseal 2") is the producer, cast for real.
--
-- THREE SEATS, because two cannot tell "the opponent the fatesealer chose" from
-- "an opponent" or from "every opponent" -- and the answer names CAROL, who is
-- not the first candidate, so an implementation that ignored the answer and took
-- the head would fateseal bob and fail.
--
-- alice targets HER OWN Piker with the first half, so the library the creature
-- lands in and the library the fateseal reorders are different libraries: a
-- fateseal that looked at its own controller's library would have to disturb the
-- card just placed there.
--
-- Returns (alice's library card, bob's library top-first, carol's library
-- top-first, alice's creature, the spell in hand, the board).
fatesealBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  NonEmpty.NonEmpty PlayerId.PlayerId ->
  m (ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
fatesealBoard s registry seats = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  spin <- S.printingOf s registry "Spin into Myth"
  let deal pid (acc, g) printing = let (oid, g') = S.addLibraryCard printing pid g in (oid : acc, g')
      (creatureId, b1) = S.addCreature piker S.alice (S.landsFor island S.alice 5 (Setup.emptyGame seats))
      (aliceLib, b2) = S.addLibraryCard forest S.alice b1
      (bobIds, b3) = List.foldl' (deal S.bob) ([], b2) [forest, mountain]
      -- Only when carol is at the table: a library belonging to a seat the game
      -- does not have would be a fixture nothing in the rules can reach.
      (carolIds, b4)
        | List.elem S.carol (NonEmpty.toList seats) = List.foldl' (deal S.carol) ([], b3) [forest, mountain, maiden]
        | otherwise = ([], b3)
      (board, spellId) = S.handOne spin b4
  pure (aliceLib, bobIds, carolIds, creatureId, spellId, board)

-- Answers Prompt.ChooseFateseal with a FIXED pair of lists, surveilAnswer's
-- posture and for its reason.
fatesealAnswer :: ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
fatesealAnswer split p = case p of
  Prompt.ChooseFateseal {} -> split
  _ -> S.identityAnswer p

fatesealSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fatesealSpec s registry = Spec.describe s "Fateseal" $ do
  let aimAt :: ObjectId.ObjectId -> PlayerId.PlayerId -> ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
      aimAt creatureId victim split p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature creatureId))) sets
        -- PINNED to the second candidate, not the first: S.identityAnswer and
        -- Replay.defaultAnswer both take the head, so a fateseal that dropped
        -- this answer would still reorder a library and still pass a membership
        -- assertion -- against the WRONG seat.
        Prompt.ChooseOpponent {} -> victim
        Prompt.ChooseFateseal {} -> split
        _ -> S.identityAnswer p
      castSpin :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castSpin answer spellId board = S.runPure answer board $ do
        S.cast S.alice spellId
        Stack.resolveTop
  Spec.it s "CR 701.29a whole card: Spin into Myth reorders the CHOSEN opponent's library and nobody else's" $ do
    (aliceLib, bobIds, carolIds, creatureId, spellId, board) <- fatesealBoard s registry S.threePlayers
    case (bobIds, carolIds) of
      ([bobTop, bobDeep], [carolTop, carolMiddle, carolDeep]) -> do
        -- The pinned answer names a card from BOB'S library as well as carol's.
        -- Against carol's prompt the stray id is filtered out and changes
        -- nothing; against a fateseal that swept every opponent it would bottom
        -- bob's top card, which is what makes the next assertion discriminate
        -- rather than pass because the answer named nothing bob owns.
        let after = castSpin (aimAt creatureId S.carol ([carolTop, bobTop], [carolMiddle])) spellId board
        Spec.assertEqWith s "carol's library started top-first maiden, mountain, forest" (Game.zoneMembers Zone.Library S.carol board) [carolTop, carolMiddle, carolDeep]
        Spec.assertEqWith s "the kept card is on top of carol's library and the bottomed one is last" (Game.zoneMembers Zone.Library S.carol after) [carolMiddle, carolDeep, carolTop]
        -- The seat the answer did NOT name. Two opponents is what makes this
        -- assertion mean anything: on a two-player board it would hold for a
        -- fateseal that swept every opponent.
        Spec.assertEqWith s "bob's library is untouched" (Game.zoneMembers Zone.Library S.bob after) [bobTop, bobDeep]
        -- CR 701.29a's library is an OPPONENT'S, never the fatesealer's: alice's
        -- library holds the returned creature on top of the card she started
        -- with, in the order the first half of the card put them there.
        Spec.assertBool s (not (S.onBattlefield creatureId after)) "the targeted creature left the battlefield"
        Spec.assertEqWith
          s
          "alice's library is the returned creature on top of her own card"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Library S.alice after))
          [cardNamed "Goblin Piker", cardNamed "Forest"]
        Spec.assertEqWith s "and the card she started with is still the one underneath" (drop 1 (Game.zoneMembers Zone.Library S.alice after)) [aliceLib]
      _ -> Spec.assertFailure s "expected two library cards for bob and three for carol"
  -- WHO is asked and about WHOSE library -- the half a board cannot show by its
  -- final state. The fatesealer is shown the cards; the library's owner is shown
  -- nothing and asked nothing.
  Spec.it s "CR 701.29a the fatesealer is asked, about the chosen opponent's top cards" $ do
    (_, _, carolIds, creatureId, spellId, board) <- fatesealBoard s registry S.threePlayers
    case carolIds of
      [carolTop, carolMiddle, _] -> do
        let recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, PlayerId.PlayerId, [ObjectId.ObjectId])] r
            recording p = case p of
              Prompt.ChooseFateseal _ seat owner looked -> do
                State.modify (<> [(seat, owner, looked)])
                pure ([], looked)
              _ -> pure (aimAt creatureId S.carol ([], []) p)
            asked =
              State.execState
                ( Engine.runGame recording board $ do
                    S.cast S.alice spellId
                    Stack.resolveTop
                )
                []
        Spec.assertEqWith s "alice asked, about carol's library, showing its top two" asked [(S.alice, S.carol, [carolTop, carolMiddle])]
      _ -> Spec.assertFailure s "expected three library cards for carol"
  -- The elision pair for the OPPONENT choice, two boards differing in seat count
  -- alone: CR 102.2's two-player game leaves exactly one opponent and nothing to
  -- ask, and a third seat makes it a real question.
  Spec.it s "CR 102.2 the opponent is chosen only when there are two of them" $ do
    let counting :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
        counting creatureId p = case p of
          Prompt.ChooseOpponent {} -> do
            State.modify (+ 1)
            pure (aimAt creatureId S.carol ([], []) p)
          _ -> pure (aimAt creatureId S.carol ([], []) p)
        asks (_, _, _, creatureId, spellId, board) =
          State.execState
            ( Engine.runGame (counting creatureId) board $ do
                S.cast S.alice spellId
                Stack.resolveTop
            )
            0
    two <- fatesealBoard s registry S.bothPlayers
    three <- fatesealBoard s registry S.threePlayers
    Spec.assertEqWith s "one opponent, not asked" (asks two) 0
    Spec.assertEqWith s "two opponents, asked once" (asks three) 1
    -- And the two-seat board still fateseals: the elision skips the question,
    -- not the action.
    case two of
      (_, bobIds, _, creatureId, spellId, board) -> case bobIds of
        [bobTop, bobDeep] -> do
          let after = castSpin (aimAt creatureId S.carol ([bobTop], [])) spellId board
          Spec.assertEqWith s "bob's only opponent fatesealed him" (Game.zoneMembers Zone.Library S.bob after) [bobDeep, bobTop]
        _ -> Spec.assertFailure s "expected two library cards for bob"
  -- The elision pair for the SPLIT question, two boards differing in one card:
  -- a lone card that is the whole library has its top and its bottom at the same
  -- position, so both answers give the same library and there is nothing to ask
  -- -- scryOne's case, and NOT surveil's, where the two destinations differ.
  -- Driven through the opcode, Spin into Myth's count being fixed at two.
  Spec.it s "CR 701.29a a one-card library raises no fateseal prompt, and a card beneath it does" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let (sourceId, base) = S.addCreature piker S.alice (S.landsInPlay island 1)
        (deep, one) = S.addLibraryCard forest S.bob base
        (top, two) = S.addLibraryCard mountain S.bob one
        counting :: Prompt.Prompt r -> State.State Int r
        counting p = case p of
          Prompt.ChooseFateseal {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        fatesealTwo = Effect.Fateseal (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
        apply = Resolve.applyEffect sourceId sourceId S.alice Map.empty Map.empty fatesealTwo
        asks gs = State.execState (Engine.runGame counting gs apply) 0
    Spec.assertEqWith s "one card, not asked" (asks one) 0
    Spec.assertEqWith s "a card beneath it, asked once" (asks two) 1
    -- Asked AND honoured, which is what separates the pair from an engine that
    -- raises the prompt and drops the answer.
    Spec.assertEqWith
      s
      "and the named card went under"
      (Game.zoneMembers Zone.Library S.bob (S.runPure (fatesealAnswer ([top], [])) two apply))
      [deep, top]

-- CR 701.44a: "certain spells and abilities instruct a permanent to explore. To
-- do so, that permanent's controller reveals the top card of their library. If a
-- land card is revealed this way, that player puts that card into their hand.
-- Otherwise, that player puts a +1/+1 counter on the exploring permanent and may
-- put the revealed card into their graveyard."
--
-- Merfolk Branchwalker {1}{G} Creature -- Merfolk Scout 2/1, "When this creature
-- enters, it explores", cast off two Forests and run to a stable board -- the
-- gameplay-level route baneOfProgressSpec takes, so CR 603.6a's enters trigger
-- is placed by the engine rather than by the fixture.
--
-- The library is STACKED so the branch is chosen rather than drawn: the top card
-- is this helper's argument and a Bird Maiden always sits beneath it. Every case
-- below is the same board with one card changed, which is what makes the land
-- and nonland branches a pair rather than two unrelated boards. Branchwalker
-- enters BARE, so the one +1/+1 counter the nonland branch adds cannot be
-- confused with a counter it already had.
exploreBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [String] ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
exploreBoard s registry deck = do
  forest <- S.printingOf s registry "Forest"
  branchwalker <- S.printingOf s registry "Merfolk Branchwalker"
  myr <- S.printingOf s registry "Darksteel Myr"
  printings <- mapM (S.printingOf s registry) deck
  let -- A second creature alice controls, so "the exploring permanent" is told
      -- apart from "a creature you control": rule 701.44a's counter goes on the
      -- one that explored and this one must stay bare.
      (bystander, withMyr) = S.addCreature myr S.alice (S.landsInPlay forest 2)
      (withSpell, spell) = S.handOne branchwalker withMyr
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deal gs printing = snd (S.addLibraryCard printing S.alice gs)
      stocked = List.foldl' deal withSpell (reverse printings)
  pure (spell, bystander, stocked)

-- Answers Prompt.ChooseExplore with a FIXED decision, whatever the engine
-- offers. Pinned rather than derived: an answerer that read the prompt's own
-- fields would still produce a legal answer after a mutation broke which card
-- was revealed, and these cases would stay green over a broken choice.
exploreAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
exploreAnswer decision p = case p of
  Prompt.ChooseExplore {} -> decision
  _ -> S.identityAnswer p

-- Cast the Branchwalker and settle: the creature spell resolves, its enters
-- trigger is placed, and the next round of passes resolves that.
runExplore ::
  (forall r. Prompt.Prompt r -> r) ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  GameState.GameState
runExplore answer spell gs =
  let afterCast = S.runPure answer gs (S.cast S.alice spell)
   in S.runPure answer afterCast Engine.priorityLoop

-- The card NAMES in one of alice's zones, in zone order. Names and not ids
-- because CR 400.7 mints a fresh incarnation for the card the explore moves, so
-- the id the fixture stocked is gone by the time the assertion reads the hand.
exploreZone :: Zone.Zone -> GameState.GameState -> [String]
exploreZone zone gs =
  fmap
    (\oid -> maybe "?" (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs))
    (Game.zoneMembers zone S.alice gs)

-- The names alice revealed this turn, in order. CR 701.44a's reveal is PUBLIC
-- (CR 701.20a), so unlike scry's private look it leaves a GameEvent behind, and
-- that event is the only thing an assertion can read it through.
exploreReveals :: GameState.GameState -> [String]
exploreReveals gs = Maybe.mapMaybe revealedName (S.eventsOf gs)
  where
    revealedName event = case event of
      GameEvent.Revealed (Revealed.MkRevealed pid _ _ pc)
        | pid == S.alice ->
            fmap (Text.unpack . CardName.unwrap) (Maybe.listToMaybe (Set.toList (PC.names pc)))
      _ -> Nothing

exploreSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exploreSpec s registry = Spec.describe s "Explore" $ do
  -- The LAND branch. Nothing else on the board differs from the two cases below:
  -- the top card is a Mountain rather than a Goblin Piker.
  Spec.it s "CR 701.44a a revealed land card goes to hand, with no counter and no question" $ do
    (spell, bystander, board) <- exploreBoard s registry ["Mountain", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertBool s (Maybe.isJust walker) "the Branchwalker resolved onto the battlefield"
    Spec.assertEqWith s "stack empty: the spell and its trigger both resolved" (length (GameState.stack after)) 0
    Spec.assertEqWith s "the Mountain left the top of the library" (exploreZone Zone.Library after) ["Bird Maiden"]
    Spec.assertEqWith s "and is in hand" (exploreZone Zone.Hand after) ["Mountain"]
    -- CR 701.20a: the reveal is public, so it is in the log every player reads.
    Spec.assertEqWith s "the Mountain was revealed on the way" (exploreReveals after) ["Mountain"]
    Spec.assertEqWith s "nothing was binned" (exploreZone Zone.Graveyard after) []
    -- The counter is the discriminator between the branches: rule 701.44a's
    -- "otherwise" is the only sentence that puts one on.
    Spec.assertEqWith s "CR 701.44a no +1/+1 counter on the land branch" (plusOnePlusOnesOn walker after) 0
    Spec.assertEqWith s "and none on the other creature alice controls" (plusOnePlusOnesOn (Just bystander) after) 0
  -- The NONLAND branch, exercising the "may". Same board, Goblin Piker on top.
  Spec.it s "CR 701.44a a revealed nonland card grows the explorer, and the choice bins it" $ do
    (spell, bystander, board) <- exploreBoard s registry ["Goblin Piker", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertBool s (Maybe.isJust walker) "the Branchwalker resolved onto the battlefield"
    Spec.assertEqWith s "one +1/+1 counter" (plusOnePlusOnesOn walker after) 1
    Spec.assertEqWith s "the Piker is in the graveyard" (exploreZone Zone.Graveyard after) ["Goblin Piker"]
    Spec.assertEqWith s "the Maiden it was sitting on is now the top card" (exploreZone Zone.Library after) ["Bird Maiden"]
    -- The TOP card and not just a card: the Maiden beneath it was never shown.
    Spec.assertEqWith s "only the Piker was revealed" (exploreReveals after) ["Goblin Piker"]
    Spec.assertEqWith s "a nonland card never reaches the hand" (exploreZone Zone.Hand after) []
    -- The counter went on the permanent that EXPLORED, not on every creature.
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0
  -- The other half of the "may", the ONE thing changed being the answer. Without
  -- this case a bin-always implementation passes the case above.
  Spec.it s "CR 701.44a declining leaves the revealed card on top of the library" $ do
    (spell, bystander, board) <- exploreBoard s registry ["Goblin Piker", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Declines) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertEqWith s "the counter went on either way" (plusOnePlusOnesOn walker after) 1
    Spec.assertEqWith s "the library is untouched, Piker still on top" (exploreZone Zone.Library after) ["Goblin Piker", "Bird Maiden"]
    Spec.assertEqWith s "nothing was binned" (exploreZone Zone.Graveyard after) []
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0
  -- CR 701.44b: the permanent explores "even if some or all of those actions were
  -- impossible". No card is revealed, so nothing is a land card and the
  -- "otherwise" branch runs -- the counter goes on with no card to ask about.
  Spec.it s "CR 701.44b an empty library still grows the explorer" $ do
    (spell, bystander, board) <- exploreBoard s registry []
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertBool s (Maybe.isJust walker) "the Branchwalker resolved onto the battlefield"
    Spec.assertEqWith s "one +1/+1 counter" (plusOnePlusOnesOn walker after) 1
    Spec.assertEqWith s "no card moved anywhere" (exploreZone Zone.Hand after <> exploreZone Zone.Graveyard after) []
    Spec.assertEqWith s "and nothing was revealed" (exploreReveals after) []
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0

-- The elision half: which boards raise CR 701.44a's question at all. Each case
-- counts the explore prompts one cast-and-settle raises.
explorePromptSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
explorePromptSpec s registry = Spec.describe s "ExplorePrompt" $ do
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseExplore {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      asks spell gs =
        State.execState
          ( Engine.runGame counting gs $ do
              S.cast S.alice spell
              Engine.priorityLoop
          )
          0
  -- A real fork, and it is put to the player: the card can end on top or in the
  -- graveyard, and no rule settles which.
  Spec.it s "CR 701.44a a revealed nonland card is asked about" $ do
    (spell, _, board) <- exploreBoard s registry ["Goblin Piker", "Bird Maiden"]
    Spec.assertEqWith s "asked once" (asks spell board) 1
  -- The pair's other half, one card changed. CR 701.44a's first sentence settles
  -- a land card outright, so there is nothing to ask.
  Spec.it s "CR 701.44a a revealed land card raises no question" $ do
    (spell, _, board) <- exploreBoard s registry ["Mountain", "Bird Maiden"]
    Spec.assertEqWith s "not asked" (asks spell board) 0
  -- Nothing was revealed, so there is no card the answer could be about.
  Spec.it s "CR 701.44b an empty library raises no question" $ do
    (spell, _, board) <- exploreBoard s registry []
    Spec.assertEqWith s "not asked" (asks spell board) 0

slotTarget :: SlotName.SlotName
slotTarget = SlotName.MkSlotName (Text.pack "target")

-- Diabolic Edict's "a creature of their choice".
creatureFilter :: Filter.Type.Filter Keyword.Keyword
creatureFilter = Filter.Type.HasCardType CardType.Creature

-- Targets `victim` with every slot that offers them, deferring the rest to
-- S.identityAnswer -- which picks the lowest ObjectId/PlayerId and so would aim
-- an edict at its own caster.
targetsPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
targetsPlayer victim p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (List.nub (filter (== Recipient.ToPlayer victim) (Set.toAscList legal) <> Set.toAscList legal))))
      sets
  _ -> S.identityAnswer p

-- A lying interpreter: names `wanted` for a sacrifice regardless of whether it
-- was offered. The only way to reach CR 701.21a's guard from a test, since the
-- candidate list is built from what the sacrificing player controls.
namesInstead :: ObjectId.ObjectId -> Prompt.Prompt r -> r
namesInstead wanted p = case p of
  Prompt.ChooseSacrifices {} -> Set.singleton wanted
  Prompt.ChooseAnyNumberToSacrifice {} -> Set.empty
  Prompt.ChooseTapsForTotalPower _ _ _ candidates _ -> Set.fromList candidates
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseSacrifices with `wanted`, when it is on offer. A pair of
-- tests differing only in this argument proves the ANSWER decides which permanent
-- is sacrificed, rather than the order the candidates are enumerated in.
sacrifices :: ObjectId.ObjectId -> Prompt.Prompt r -> r
sacrifices wanted p = case p of
  Prompt.ChooseSacrifices _ _ _ candidates _ ->
    if elem wanted candidates then Set.singleton wanted else Set.fromList (take 1 candidates)
  Prompt.ChooseAnyNumberToSacrifice {} -> Set.empty
  Prompt.ChooseTapsForTotalPower _ _ _ candidates _ -> Set.fromList candidates
  _ -> S.identityAnswer p

playerSacrificesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
playerSacrificesSpec s registry = Spec.describe s "PlayerSacrifices" $ do
  -- CR 701.21a: "its controller moves it from the battlefield directly to its
  -- owner's graveyard." Diabolic Edict names a PLAYER, and that player picks.
  Spec.it s "Diabolic Edict: the targeted player chooses which of their creatures dies" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (hisPiker, g1) = S.addCreature piker S.bob g0
        (hisRats, gs) = S.addCreature rats S.bob g1
        edict = Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
        keptRats = S.runPure (sacrifices hisPiker) gs edict
        keptPiker = S.runPure (sacrifices hisRats) gs edict
    Spec.assertBool s (S.onBattlefield hisRats keptRats) "choosing the Piker leaves the Rats"
    Spec.assertBool s (not (S.onBattlefield hisPiker keptRats)) "and the Piker is gone"
    -- The discriminating twin: same board, same effect, opposite answer.
    Spec.assertBool s (S.onBattlefield hisPiker keptPiker) "choosing the Rats leaves the Piker"
    Spec.assertBool s (not (S.onBattlefield hisRats keptPiker)) "and the Rats are gone"
    Spec.assertBool s (S.onBattlefield src keptRats) "alice's own creature is never touched"
  -- CR 701.21a: "A player can't sacrifice ... a permanent they don't control."
  -- The guard the whole issue is about, reached the only way it can be: an
  -- interpreter naming a permanent outside the offered set.
  --
  -- Bob controls TWO creatures on purpose. With one, candidates <= count and
  -- the prompt is elided, so the lying answerer is never consulted and the
  -- test passes without exercising anything -- which is what it did before
  -- review caught it.
  Spec.it s "CR 701.21a an answer naming a permanent the player does not control is refused" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (hers, g1) = S.addCreature piker S.alice g0
        (hisPiker, g2) = S.addCreature piker S.bob g1
        (hisRats, gs) = S.addCreature rats S.bob g2
        after = S.runPure (namesInstead hers) gs (Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))))
        bobsLeft = length (filter (`S.onBattlefield` after) [hisPiker, hisRats])
    Spec.assertBool s (S.onBattlefield hers after) "alice's creature is untouched"
    -- The edict still takes exactly one: an answer the engine refuses does not
    -- become an answer of "none". CR 609.3 caps it at what bob controls, and
    -- he controls two.
    Spec.assertEqWith s "bob still lost exactly one of his own" bobsLeft 1
  -- Where the rules leave nothing to ask, don't prompt: one candidate is
  -- forced (CR 609.3 does as much as possible, which here is all of it).
  Spec.it s "CR 609.3 a lone creature is sacrificed without a prompt" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (his, gs) = S.addCreature piker S.bob g0
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseSacrifices {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        act = Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
        asked = State.execState (Engine.runGame countingAnswer gs act) 0
        after = S.runPure S.identityAnswer gs act
    Spec.assertEqWith s "nothing to choose" asked 0
    Spec.assertBool s (not (S.onBattlefield his after)) "but it still died"
  -- CR 609.3 again: a player with no creatures sacrifices nothing, and the
  -- edict simply does as much as it can -- which is nothing.
  Spec.it s "CR 609.3 an edict against an empty board does nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer gs (Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))))
    Spec.assertBool s (S.onBattlefield src after) "alice keeps hers"
  -- The gameplay-level proof: the real card, cast and resolved.
  Spec.it s "Diabolic Edict whole card: cast off two Swamps, bob sacrifices" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    diabolicEdict <- S.printingOf s registry "Diabolic Edict"
    let base = S.landsInPlay swamp 2
        (his, g1) = S.addCreature piker S.bob base
        (withSpell, spell) = S.handOne diabolicEdict g1
        afterCast = S.runPure (targetsPlayer S.bob) withSpell (S.cast S.alice spell)
        resolved = S.runPure (targetsPlayer S.bob) afterCast Stack.resolveTop
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertBool s (not (S.onBattlefield his resolved)) "bob's creature was sacrificed"

createEmblemSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
createEmblemSpec s registry = Spec.describe s "CreateEmblem" $ do
  Spec.it s "CR 114.2 CreateEmblem puts an emblem in the command zone under the resolver" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        act = Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.CreateEmblem (Printing.card piker))
        after = S.runPure S.identityAnswer gs0 act
        emblems = filter (\oid -> fmap Object.zone (Game.lookupObject oid after) == Just Zone.Command) (Set.toList (GameState.command after))
    Spec.assertEqWith s "one emblem in command" (Set.size (GameState.command after)) 1
    Spec.assertEqWith s "owned by the resolver" (fmap (\oid -> fmap Object.owner (Game.lookupObject oid after)) emblems) [Just S.alice]

becomeMonarchSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
becomeMonarchSpec s registry = Spec.describe s "BecomeMonarch" $ do
  Spec.it s "CR 725 BecomeMonarch TheController makes the resolver the monarch" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer gs0 (Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.BecomeMonarch MonarchTarget.TheController))
    Spec.assertEqWith s "alice is monarch" (GameState.monarch after) (Just S.alice)
    Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "a BecameMonarch event was recorded"

-- The slot Denethor's crown half names, and the slot its damage half names.
denethorCrownSlot, denethorDamageSlot :: SlotName.SlotName
denethorCrownSlot = SlotName.MkSlotName (Text.pack "player")
denethorDamageSlot = SlotName.MkSlotName (Text.pack "damage")

-- Fills every CR 601.2c slot BY NAME from `answers`, and records the map it
-- returned.
--
-- Both halves matter, and both are about this being the pool's first card with
-- TWO target slots in one mode. S.identityAnswer fills every slot with the
-- lowest-sorting legal recipient, so left to itself it answers both slots the
-- same shape and "bob became the monarch" could be an accident of PlayerId
-- ordering rather than of the crown reading its own slot; overriding by name is
-- what makes the two slots differ. Recording is how the test asserts the map it
-- fed in is the map the engine received, instead of inferring it from the board.
answerSlots ::
  Map.Map SlotName.SlotName (Set.Set Recipient.Recipient) ->
  Prompt.Prompt r ->
  State.State [Map.Map SlotName.SlotName (Set.Set Recipient.Recipient)] r
answerSlots answers p = case p of
  Prompt.ChooseTargets {} -> do
    let deflt = S.identityAnswer p
        filled = Map.union (Map.intersection answers deflt) deflt
    State.modify' (<> [filled])
    pure filled
  _ -> pure (S.identityAnswer p)

-- Denethor, Stone Seer -- "{3}{R}, {T}, Sacrifice Denethor: Target player
-- becomes the monarch. Denethor deals 3 damage to any target."
--
-- The printed card also has "When Denethor enters, scry 2", which
-- data/cards/denethor-stone-seer.json now carries: Effect.Scry landed with
-- Crystal Ball. It reaches none of the assertions below -- S.addCreature places
-- the permanent rather than moving it there, so no CR 603.2 entry trigger is
-- gathered, and the ability under test is the activated one.
--
-- Settled under alice, who already holds the crown, with four Mountains to pay
-- the {3}{R} and priority in hand. The first activated ability of the card is
-- the one under test; the empty fallback is ActivateSpec.theAbility's, and would
-- fail every assertion below rather than silently pass one.
--
-- FOUR seats. At two players "target player" and "the controller's one opponent"
-- name the same seat, so a two-seat board cannot tell which arm the resolver
-- took; three separate the crown's target (bob) from the damage's (carol) from
-- the controller (alice). The fourth (dave) is what lets CR 608.2b's
-- all-targets-illegal case be reached by conceding both targets without CR
-- 104.2a ending the game first.
denethorBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ActivatedAbility.ActivatedAbility Card.Type.Card, ObjectId.ObjectId, GameState.GameState)
denethorBoard s registry = do
  denethor <- S.printingOf s registry "Denethor, Stone Seer"
  mountain <- S.printingOf s registry "Mountain"
  let lands = List.foldl' (\gs _ -> snd (S.addCreature mountain S.alice gs)) (Setup.emptyGame S.fourPlayers) [1 .. 4 :: Int]
      (srcId, gs1) = S.addCreature denethor S.alice lands
      ability = case Face.activatedAbilities (S.combinedFace denethor) of
        ab : _ -> ab
        [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Nothing
  pure (ability, srcId, S.withMonarch S.alice (gs1 {GameState.priority = Just S.alice}))

-- CR 725.1: "The monarch is a designation a player can have. There is no monarch
-- in a game until an effect instructs a player to become the monarch." Every
-- BecomeMonarch before this one derived the player it crowned -- the resolving
-- controller, or CR 725.2's controller of the damaging creature. Denethor is the
-- first card in the pool whose crown reads a TARGET slot, so the player it names
-- is the activator's CHOICE, announced under CR 601.2c and re-checked under CR
-- 608.2b like any other target.
--
-- CR 601.2c is what lets the two slots coexist: "if the spell uses the word
-- 'target' in multiple places, the same object or player can be chosen once for
-- each instance of the word 'target'". Denethor writes it twice, so the crown
-- and the damage are independent choices that may or may not land on one player.
targetedMonarchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
targetedMonarchSpec s registry = Spec.describe s "TargetedMonarch" $ do
  Spec.it s "CR 725.1/725.3 the crown goes to the TARGETED player, not the controller and not the damage's target" $ do
    (ability, srcId, gs0) <- denethorBoard s registry
    let answers = Map.fromList [(denethorCrownSlot, Set.singleton (Recipient.ToPlayer S.bob)), (denethorDamageSlot, Set.singleton (Recipient.ToPlayer S.carol))]
        act = do Activate.activateAbility S.alice srcId ability; Stack.resolveTop
        ((_, after), asked) = State.runState (Engine.runGame (answerSlots answers) gs0 act) []
    Spec.assertEqWith s "alice held the crown going in" (GameState.monarch gs0) (Just S.alice)
    Spec.assertEqWith s "CR 601.2c asked once, for both slots, and got the map fed in" asked [answers]
    Spec.assertEqWith s "CR 725.3 the crown moved to bob, the targeted player" (GameState.monarch after) (Just S.bob)
    Spec.assertBool s (elem (GameEvent.BecameMonarch S.bob) (S.eventsOf after)) "and the crowning event names bob"
    Spec.assertEqWith s "CR 115.4 carol, the any-target, took the 3" (S.lifeOf S.carol after) (Just 17)
    Spec.assertEqWith s "bob took none of it" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and neither did alice" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "the cost sacrificed Denethor into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and the ability left the stack" (GameState.stack after) []

  -- CR 725.3: "Only one player can be the monarch at a time. As a player becomes
  -- the monarch, the current monarch ceases to be the monarch." The unseating is
  -- observable only through CR 725.2's inherent end-step draw, which belongs to
  -- whoever holds the crown -- so alice, who held it, must stop drawing and bob,
  -- who took it, must start.
  Spec.it s "CR 725.3 the unseated monarch stops drawing at end step, and the new one starts" $ do
    (ability, srcId, gs0) <- denethorBoard s registry
    piker <- S.printingOf s registry "Goblin Piker"
    let answers = Map.fromList [(denethorCrownSlot, Set.singleton (Recipient.ToPlayer S.bob)), (denethorDamageSlot, Set.singleton (Recipient.ToPlayer S.carol))]
        act = do Activate.activateAbility S.alice srcId ability; Stack.resolveTop
        ((_, after), _) = State.runState (Engine.runGame (answerSlots answers) gs0 act) []
        -- CR 104.3c: a seat asked to draw from an empty library loses instead, so
        -- both candidates get a card. That also makes "drew nothing" mean the
        -- trigger did not fire rather than that there was nothing to take.
        stocked = snd (S.addLibraryCard piker S.bob (snd (S.addLibraryCard piker S.alice after)))
        endStep = Phase.Ending EndingStep.EndStep
        endStepOf pid gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep pid)) (gs {GameState.phase = endStep, GameState.activePlayer = pid})
        run gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
    Spec.assertEqWith s "bob really has the crown" (GameState.monarch after) (Just S.bob)
    Spec.assertEqWith s "CR 725.2 bob, the new monarch, draws on his own end step" (length (Game.zoneMembers Zone.Hand S.bob (run (endStepOf S.bob stocked)))) 1
    Spec.assertEqWith s "CR 725.3 alice, unseated, draws nothing on hers" (length (Game.zoneMembers Zone.Hand S.alice (run (endStepOf S.alice stocked)))) 0

  -- CR 608.2b: "If all its targets, for every instance of the word 'target', are
  -- now illegal, the spell or ability doesn't resolve. ... Otherwise, the spell
  -- or ability will resolve normally. Illegal targets, if any, won't be affected
  -- by parts of a resolving spell's effect for which they're illegal. Other parts
  -- of the effect for which those targets are not illegal may still affect them."
  --
  -- Denethor is the first card in the pool that can reach the PARTIAL clause: it
  -- takes two targets in one mode, so exactly one of them can go illegal. A
  -- conceding player is the lever: CR 104.3a takes them out of the game
  -- immediately, so Target.playerRecipients -- which is built from
  -- Game.stillPlaying -- stops offering them, and both of Denethor's slots take
  -- players. CR 800.4 is what lets the game go on around the concession.
  Spec.it s "CR 608.2b one illegal target does not fizzle the ability, and the other half still happens" $ do
    (ability, srcId, gs0) <- denethorBoard s registry
    let answers = Map.fromList [(denethorCrownSlot, Set.singleton (Recipient.ToPlayer S.bob)), (denethorDamageSlot, Set.singleton (Recipient.ToPlayer S.carol))]
        activated = snd (State.evalState (Engine.runGame (answerSlots answers) gs0 (Activate.activateAbility S.alice srcId ability)) [])
        concede = Departure.depart Departure.Type.Conceded
        resolveAfter f = S.runPure S.identityAnswer (f activated) Stack.resolveTop
        damageIllegal = resolveAfter (concede S.carol)
        crownIllegal = resolveAfter (concede S.bob)
        bothIllegal = resolveAfter (concede S.bob . concede S.carol)
    Spec.assertEqWith s "the ability waits on the stack with both targets chosen" (length (GameState.stack activated)) 1
    -- The damage's target is gone; the crown's is not, so the crown still moves.
    Spec.assertEqWith s "carol illegal: bob is still crowned" (GameState.monarch damageIllegal) (Just S.bob)
    Spec.assertEqWith s "and the 3 damage went nowhere" (fmap (`S.lifeOf` damageIllegal) [S.alice, S.bob, S.dave]) [Just 20, Just 20, Just 20]
    -- The mirror: the crown's target is gone, the damage's is not.
    Spec.assertEqWith s "bob illegal: the crown does not move" (GameState.monarch crownIllegal) (Just S.alice)
    Spec.assertBool s (notElem (GameEvent.BecameMonarch S.bob) (S.eventsOf crownIllegal)) "and nobody was crowned"
    Spec.assertEqWith s "but carol still took the 3" (S.lifeOf S.carol crownIllegal) (Just 17)
    -- Both gone: CR 608.2b's first clause, the ability does not resolve. Denethor
    -- cannot tell that apart from resolving with both slots skipped -- every
    -- effect it has is slot-gated, so the two produce the same board -- so what
    -- is asserted here is the OUTCOME, which the rule fixes either way. The
    -- discriminating half of CR 608.2b is the partial clause above.
    Spec.assertEqWith s "both illegal: no crown moves" (GameState.monarch bothIllegal) (Just S.alice)
    Spec.assertEqWith s "and no damage is dealt" (fmap (`S.lifeOf` bothIllegal) [S.alice, S.dave]) [Just 20, Just 20]
    Spec.assertEqWith s "the ability leaves the stack either way" (GameState.stack bothIllegal) []

  -- The classification half, asserted directly. slotsOf is the READ side of the
  -- D4 dataflow lint and has no runtime consumer: Resolve.resolveModes re-derives
  -- CR 608.2b's legality from the card's declared targetSlots, so the gameplay
  -- cases above pass whatever slotsOf answers.
  --
  -- The InSlot line is now ALSO covered by CardSpec's dataflow lint, which since
  -- #1043 states its equality over an activated ability's modes too -- reverting
  -- this arm to Set.empty fails Denethor there as well as here. Kept rather than
  -- deleted for the two lines the lint cannot reach: no card in the pool uses
  -- TheController or ControllerOfSource, so an arm wrongly REPORTING a slot for
  -- either would be swept by nothing. Those are the arm-level pin; the first line
  -- is a locality convenience, keeping all three answers in one place.
  Spec.it s "CR 725.1 slotsOf reads the targeted monarch's slot, and only that arm's" $ do
    let slot = SlotName.MkSlotName (Text.pack "player")
    Spec.assertEqWith s "the targeted arm names its slot" (Resolve.slotsOf (Effect.BecomeMonarch (MonarchTarget.InSlot slot))) (Map.singleton slot SlotArity.One)
    Spec.assertEqWith s "the resolving controller names none" (Resolve.slotsOf (Effect.BecomeMonarch MonarchTarget.TheController)) Map.empty
    Spec.assertEqWith s "and neither does CR 725.2's crown steal" (Resolve.slotsOf (Effect.BecomeMonarch MonarchTarget.ControllerOfSource)) Map.empty

-- Palace Jailer's ruling (Scryfall, 2021-03-19): "If you're not the monarch as
-- Palace Jailer's second ability resolves, the creature will be exiled until
-- there's a new monarch and that player is one of your opponents. The creature
-- won't immediately return just because an opponent is the monarch." A companion
-- ruling fixes the same reading from the other side: "Palace Jailer leaving the
-- battlefield won't cause the exiled creature to return. The game will continue
-- to watch for the NEXT TIME an opponent becomes the monarch."
--
-- So the watch is for an EVENT -- a new monarch being crowned who is an opponent
-- -- not for the STATE "an opponent currently holds the crown".
exileUntilMonarchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exileUntilMonarchSpec s registry = Spec.describe s "ExileUntilMonarch" $ do
  -- Reachable at two seats: CR 603.3b lets alice order Palace Jailer's two
  -- entry triggers, so the exile can resolve BEFORE she becomes the monarch,
  -- while bob still holds the crown.
  Spec.it s "CR 725 an exile that resolves while an opponent is already the monarch does not return at once" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.ExileUntilMonarch slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        settled = snd (Engine.runGamePure S.identityAnswer exiled Monarch.returnExiledForMonarch)
    Spec.assertEqWith s "the watch was registered" (Map.size (GameState.exiledUntilMonarch exiled)) 1
    Spec.assertEqWith s "bob is still the monarch, unchanged" (GameState.monarch settled) (Just S.bob)
    Spec.assertEqWith s "nothing came back to the battlefield" (Set.size (GameState.battlefield settled)) 0
    Spec.assertEqWith s "and the watch is still armed" (Map.size (GameState.exiledUntilMonarch settled)) 1
  -- The whole arc, still two seats. The crown must actually CHANGE HANDS to an
  -- opponent before the creature comes back, and alice taking it herself in
  -- between must not discharge the watch.
  Spec.it s "CR 725 the exile returns when a NEW monarch is crowned who is an opponent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.ExileUntilMonarch slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        -- Palace Jailer's OTHER entry trigger: alice takes the crown. She is
        -- not her own opponent, so this must not return the creature.
        alicesCrown = snd (Engine.runGamePure S.identityAnswer exiled {GameState.monarch = Just S.alice} Monarch.returnExiledForMonarch)
        -- bob deals combat damage to the monarch (CR 725.3) and takes it back.
        bobsCrown = snd (Engine.runGamePure S.identityAnswer alicesCrown {GameState.monarch = Just S.bob} Monarch.returnExiledForMonarch)
    Spec.assertEqWith s "alice holding the crown does not discharge the watch" (Map.size (GameState.exiledUntilMonarch alicesCrown)) 1
    Spec.assertEqWith s "nor return the creature" (Set.size (GameState.battlefield alicesCrown)) 0
    Spec.assertEqWith s "bob retaking it does return the creature" (Set.size (GameState.battlefield bobsCrown)) 1
    Spec.assertEqWith s "and discharges the watch" (Map.size (GameState.exiledUntilMonarch bobsCrown)) 0
  -- The crown VANISHING is not an opponent becoming the monarch. CR 725.1's
  -- ruling says the game keeps exactly one monarch once it has one, and the
  -- single way back to none is CR 725.4's last player standing leaving -- but
  -- the watch must not read "no monarch" as "not the controller" and fire.
  Spec.it s "CR 725.1 the crown vanishing is not an opponent becoming the monarch" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.ExileUntilMonarch slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        noMonarch = snd (Engine.runGamePure S.identityAnswer exiled {GameState.monarch = Nothing} Monarch.returnExiledForMonarch)
    Spec.assertEqWith s "the watch is still armed" (Map.size (GameState.exiledUntilMonarch noMonarch)) 1
    Spec.assertEqWith s "and nothing returned" (Set.size (GameState.battlefield noMonarch)) 0

-- M4.5 P1 gate: Act of Treason strings GainControl + Untap + ModifyTarget
-- (GainKeyword Haste) together end to end -- cast, resolve, attack, revert.
actOfTreasonSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
actOfTreasonSpec s registry = Spec.describe s "Act of Treason" $ do
  Spec.it s "steal, untap, haste, attack, then revert" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    actOfTreason <- S.printingOf s registry "Act of Treason"
    let base0 = S.landsInPlay mountain 3 -- alice: {R}{R}{R} for {2}{R}
        (oid, base1) = S.addCreature piker S.bob base0
        base = S.tapObject oid base1 -- start it tapped to prove the untap rider
        (gs1, spellId) = S.handOne actOfTreason base
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice controls the Piker" (Projection.controllerOf oid resolved) (Just S.alice)
    Spec.assertEqWith s "the untap rider untapped it" (fmap Object.tapped (Game.lookupObject oid resolved)) (Just TapState.Untapped)
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste oid resolved) "it has haste"
    Spec.assertBool s (oid `elem` Combat.legalAttackers S.alice resolved) "alice may attack with it this turn"
    Spec.assertBool s (oid `notElem` Combat.legalAttackers S.bob resolved) "bob may not attack with it"
    Spec.assertEqWith s "control reverts at cleanup" (Projection.controllerOf oid (Expiry.dropAtCleanup resolved)) (Just S.bob)

-- CR 603.5 / 608.2d: an OPTIONAL effect -- "you may" -- decided as the ability
-- resolves, not as it is put on the stack.
--
-- Renewed Faith is the card: a {2}{W} instant with "You gain 6 life", Cycling
-- {1}{W}, and "When you cycle this card, you may gain 2 life". It targets
-- nothing, so nothing here can be passing on the targeting machinery: the only
-- new thing is whether the trigger's one effect happens.
optionalEffectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
optionalEffectSpec s registry =
  let -- Takes the option ONLY if the prompt names the right decider, the right
      -- player and the right mode. A prompt addressed to anybody else, or naming
      -- a mode this ability does not have, declines -- so the life total below
      -- is discriminating about the whole payload, not just about the answer.
      takeOptional :: Prompt.Prompt r -> r
      takeOptional p = case p of
        Prompt.ChooseOptional (Decider.MkDecider d) player _ idx cIdx
          | d == S.alice && player == S.alice && idx == ModeIndex.MkModeIndex 0 && cIdx == ClauseIndex.MkClauseIndex 0 ->
              OptionalDecision.Exercises
        Prompt.ChooseOptional {} -> OptionalDecision.Declines
        _ -> S.identityAnswer p
      -- The named card in alice's hand with two of the named land in play, which
      -- is what Renewed Faith's {1}{W} cycling costs, and alice holding priority.
      handWithTwoLands printing land = do
        faith <- S.printingOf s registry printing
        plains <- S.printingOf s registry land
        let (g1, faithId) = S.handOne faith (S.landsInPlay plains 2)
        pure (g1 {GameState.priority = Just S.alice}, faithId)
      -- Deem Worthy in hand with four Mountains for its {3}{R} cycling, and one
      -- Goblin Piker on the battlefield as the only legal creature target.
      deemWorthyBoard = do
        worthy <- S.printingOf s registry "Deem Worthy"
        mountain <- S.printingOf s registry "Mountain"
        piker <- S.printingOf s registry "Goblin Piker"
        let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 4)
            (g1, worthyId) = S.handOne worthy g0
        pure (g1 {GameState.priority = Just S.alice}, worthyId, creature)
   in Spec.describe s "OptionalEffect" $ do
        Spec.it s "CR 603.5 declining the may gains nothing, and the ability still resolves" $ do
          (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
          case Activate.abilitiesFor faithId gs of
            [ability] -> do
              let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice faithId ability)
                  placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
                  after = S.runPure S.identityAnswer placed Stack.resolveTop
              Spec.assertEqWith s "the trigger is on the stack, above the draw" (length (GameState.stack placed)) 2
              Spec.assertEqWith s "declining gains no life" (S.lifeOf S.alice after) (Just 20)
              -- CR 608.2n, not CR 608.2b: a declined "may" is not a fizzle.
              -- The ability resolved -- it just did nothing -- and leaving the
              -- stack is the last part of that resolution.
              Spec.assertEqWith s "and the ability left the stack anyway -- it did not fizzle" (length (GameState.stack after)) 1
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        Spec.it s "CR 603.5 whole card: cycling Renewed Faith and taking the may gains exactly 2" $ do
          (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
          case Activate.abilitiesFor faithId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice faithId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  after = S.runPure takeOptional placed Stack.resolveTop
              Spec.assertEqWith s "the Faith is in the graveyard, cycled" (length (Game.zoneMembers Zone.Graveyard S.alice cycled)) 1
              Spec.assertEqWith s "taking it gains exactly 2" (S.lifeOf S.alice after) (Just 22)
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- The prompt itself, not just its consequence: recording the run puts
        -- the answer in the transcript, which is the only place a raised
        -- prompt is directly observable. Twinned with the mandatory control
        -- below, which must record NO such response.
        Spec.it s "CR 608.2d the choice is announced as a real prompt, and lands in the transcript" $ do
          (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
          case Activate.abilitiesFor faithId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice faithId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  (_, transcript) = Replay.record takeOptional placed Stack.resolveTop
              Spec.assertEqWith
                s
                "exactly one may was asked, and it was taken"
                (filter isOptionalResponse transcript)
                [Response.ChoseOptional OptionalDecision.Exercises]
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- The control: Windcaller Aven's cycling trigger is the SAME shape one
        -- word short of a "may", and it must not be asked about at all.
        Spec.it s "CR 603.5 a mandatory cycling trigger raises no such prompt" $ do
          aven <- S.printingOf s registry "Windcaller Aven"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
              (g1, avenId) = S.handOne aven g0
              gs = g1 {GameState.priority = Just S.alice}
          case Activate.abilitiesFor avenId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice avenId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  (_, transcript) = Replay.record takeOptional placed Stack.resolveTop
              Spec.assertEqWith s "nothing was asked about a may" (filter isOptionalResponse transcript) []
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- The second card, and the one that puts a TARGET under the "may":
        -- Deem Worthy {4}{R} Instant, "Deem Worthy deals 7 damage to target
        -- creature. Cycling {3}{R}. When you cycle this card, you may have it
        -- deal 2 damage to target creature." The target is chosen as the
        -- trigger goes on the stack (CR 603.3d) and the option only on
        -- resolution (CR 603.5), which is the ordering a mode-selection
        -- encoding of "may" would have collapsed.
        Spec.it s "CR 603.5 whole card: cycling Deem Worthy and taking the may deals 2 to the target" $ do
          (gs, worthyId, piker) <- deemWorthyBoard
          case Activate.abilitiesFor worthyId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice worthyId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  taken = S.runPure takeOptional placed Stack.resolveTop
                  declined = S.runPure S.identityAnswer placed Stack.resolveTop
              Spec.assertEqWith s "taking it marks 2 damage" (fmap Object.damage (Game.lookupObject piker taken)) (Just 2)
              Spec.assertEqWith s "declining marks none" (fmap Object.damage (Game.lookupObject piker declined)) (Just 0)
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- CR 608.2b before CR 603.5: with its only target gone, the ability
        -- "doesn't resolve. It's removed from the stack" -- so there is nothing
        -- left for the "may" to decide and the prompt is never raised. The
        -- engine does not ask a question whose answer cannot matter.
        Spec.it s "CR 608.2b a fizzled optional trigger is not asked about at all" $ do
          (gs, worthyId, piker) <- deemWorthyBoard
          case Activate.abilitiesFor worthyId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice worthyId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  gone = S.runPure S.identityAnswer placed (Event.changeZone piker Zone.Graveyard)
                  ((_, after), transcript) = Replay.record takeOptional gone Stack.resolveTop
              Spec.assertEqWith s "the trigger left the stack" (length (GameState.stack after)) (length (GameState.stack placed) - 1)
              Spec.assertEqWith s "and no may was ever asked" (filter isOptionalResponse transcript) []
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- CR 608.2d over CR 608.2e's unit: a "may" covers the CLAUSE it is
        -- printed on, not the whole mode. Two clauses in one mode -- a mandatory
        -- Draw and an optional Draw -- and declining the second must still leave
        -- the first having happened. Driven through resolveModes directly,
        -- because no card in the pool has two clauses yet (#335).
        Spec.it s "CR 608.2d a declined clause skips only its own effects" $ do
          forest <- S.printingOf s registry "Forest"
          piker <- S.printingOf s registry "Goblin Piker"
          let base = Setup.emptyGame S.bothPlayers
              -- Two cards in alice's library, so BOTH draws could find one and
              -- the count separates "declined" from "drew off an empty library".
              (_, gs0) = S.addLibraryCard forest S.alice base
              (_, gs1) = S.addLibraryCard forest S.alice gs0
              -- A Stack-zone object whose Object.owner is the effect controller
              -- resolveModes reads, without paying to cast anything.
              (stackId, gs) = S.spellOnStack piker S.alice gs1
              draw = Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
              mode =
                Mode.MkMode
                  ( Seq.fromList
                      [ Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton draw),
                        Clause.MkClause Nothing Optionality.Optional Nothing (Seq.singleton draw)
                      ]
                  )
                  Map.empty
              before = S.handSize S.alice gs
              -- S.identityAnswer declines every optional prompt, so this is the
              -- declining half with no bespoke answerer needed.
              after = S.runPure S.identityAnswer gs (Resolve.resolveModes stackId stackId [(ModeInstance.MkModeInstance (ModeIndex.MkModeIndex 0) 0, mode)])
          Spec.assertEqWith s "the mandatory clause drew, the declined one did not" (S.handSize S.alice after) (before + 1)

-- Takes every printed "may" it is offered. Rank-1 like paysFor above: the
-- implicit forall is outermost, so this is the `forall r. Prompt r -> r` that
-- Engine.runGamePure wants, which a let-bound local could not be.
exerciseOptional :: Prompt.Prompt r -> r
exerciseOptional p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Is this transcript entry an answer to a printed "may"? The filter both
-- optional-effect transcript assertions share.
isOptionalResponse :: Response.Response -> Bool
isOptionalResponse r = case r of
  Response.ChoseOptional _ -> True
  _ -> False

-- Day of Judgment, cast off four Plains from alice's hand and resolved. Every
-- test in the group below goes through the whole card -- cast, pay, resolve --
-- because "Destroy all creatures" has nothing to exercise at the opcode level
-- that the card does not exercise better: it takes no target and prompts for
-- nothing, so a hand-built applyEffect call would differ from a real cast only
-- in the mana.
castDayOfJudgment :: Printing.Printing -> Printing.Printing -> GameState.GameState -> GameState.GameState
castDayOfJudgment plains dayOfJudgment board =
  let (withSpell, spell) = S.handOne dayOfJudgment (List.foldl' (\gs _ -> snd (S.addCreature plains S.alice gs)) board [1 :: Int .. 4])
      afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
   in S.runPure S.identityAnswer afterCast Stack.resolveTop

destroyAllSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
destroyAllSpec s registry = Spec.describe s "DestroyAll" $ do
  -- CR 109.2: "Destroy all creatures" includes no "card" or "spell", so it
  -- means every CREATURE PERMANENT on the battlefield -- both players' and,
  -- pointedly, the caster's own. Nothing else on the battlefield is touched.
  Spec.it s "Day of Judgment destroys every creature, the caster's own included, and leaves noncreature permanents alone" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (hers, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (his, g2) = S.addCreature piker S.bob g1
        (equipment, g3) = S.addCreature bonesplitter S.alice g2
        resolved = castDayOfJudgment plains dayOfJudgment g3
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertBool s (not (S.onBattlefield his resolved)) "bob's creature died"
    Spec.assertBool s (not (S.onBattlefield hers resolved)) "and so did alice's own"
    Spec.assertBool s (S.onBattlefield equipment resolved) "the Equipment is not a creature and stands"
    Spec.assertEqWith s "no creatures left at all" (Set.size (Set.filter (`Projection.isCreatureOf` resolved) (GameState.battlefield resolved))) 0
  -- CR 702.12b: "A permanent with indestructible can't be destroyed." The
  -- mass form goes through Event.destroy exactly as the single-target form
  -- does, so it inherits that gate rather than bypassing it.
  Spec.it s "CR 702.12b an indestructible creature survives Day of Judgment" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (myr, g1) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        (his, g2) = S.addCreature piker S.bob g1
        resolved = castDayOfJudgment plains dayOfJudgment g2
    Spec.assertBool s (S.onBattlefield myr resolved) "the Myr can't be destroyed"
    Spec.assertBool s (not (S.onBattlefield his resolved)) "the Piker can"
  -- CR 701.19a: a regeneration shield "protects the permanent the next time
  -- it would be destroyed this turn". Day of Judgment says nothing about
  -- regeneration, so it carries Regenerability.Regenerable and the shield
  -- applies -- the creature is instead tapped and stays.
  Spec.it s "CR 701.19a a regeneration shield saves its creature from Day of Judgment" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (shielded, g1) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (bare, g2) = S.addCreature piker S.bob g1
        resolved = castDayOfJudgment plains dayOfJudgment (S.addRegenShield shielded g2)
    Spec.assertBool s (S.onBattlefield shielded resolved) "the shielded creature stands"
    Spec.assertEqWith s "and CR 701.19a taps it" (fmap Object.tapped (Game.lookupObject shielded resolved)) (Just TapState.Tapped)
    Spec.assertBool s (not (S.onBattlefield bare resolved)) "its unshielded twin died"
  -- CR 608.2f: "Some spells and abilities include actions taken on multiple
  -- players and/or objects. In most cases, each such action is processed
  -- simultaneously." So the affected set is fixed once, before the first
  -- creature dies, and a creature that only IS one because of another
  -- creature dies with it rather than being spared.
  --
  -- Opalescence animates March of the Machines (a non-Aura enchantment);
  -- March in turn animates the Bonesplitter (a noncreature artifact). March
  -- is added BEFORE the Bonesplitter on purpose: it therefore has the lower
  -- ObjectId and is swept first, so an implementation that re-derived "is it
  -- a creature?" after each destruction would spare the Bonesplitter. Both
  -- die. Opalescence itself is never a creature ("each OTHER") and stands.
  Spec.it s "CR 608.2f the affected set is fixed before the first destruction: March of the Machines and the Bonesplitter it animates die together" $ do
    plains <- S.printingOf s registry "Plains"
    opalescence <- S.printingOf s registry "Opalescence"
    march <- S.printingOf s registry "March of the Machines"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (opal, g1) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (animator, g2) = S.addCreature march S.alice g1
        (equipment, board) = S.addCreature bonesplitter S.alice g2
    Spec.assertBool s (Projection.isCreatureOf animator board) "setup: March is a creature via Opalescence"
    Spec.assertBool s (Projection.isCreatureOf equipment board) "setup: the Bonesplitter is a creature via March"
    Spec.assertBool s (animator < equipment) "setup: March is swept first"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield animator resolved)) "March died"
    Spec.assertBool s (not (S.onBattlefield equipment resolved)) "and so did the Bonesplitter it animated"
    Spec.assertBool s (S.onBattlefield opal resolved) "Opalescence animates each OTHER enchantment, so it was never a creature"
  -- CR 608.2f again, on the other half of what "simultaneously" means: not
  -- just WHICH permanents the instruction names, but WHEN each one's CR
  -- 702.12b gate is judged. "A permanent with indestructible can't be
  -- destroyed" is asked of every victim while every other victim is still on
  -- the battlefield -- including the one whose static ability is granting the
  -- indestructible. So the Walls of Ba Sing Se die and what they protect does
  -- not.
  --
  -- The Walls are added FIRST on purpose, so they hold the lower ObjectId and
  -- are swept first. An implementation that judged each victim against the
  -- board the previous ones had already left would find the grant gone by the
  -- time it reached the Piker and kill it too.
  Spec.it s "CR 608.2f every victim's CR 702.12b gate is judged before any of them dies: the Walls of Ba Sing Se die, what they protect stands" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    walls <- S.printingOf s registry "The Walls of Ba Sing Se"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (granter, g1) = S.addCreature walls S.alice (Setup.emptyGame S.bothPlayers)
        (protected, g2) = S.addCreature piker S.alice g1
        (his, board) = S.addCreature piker S.bob g2
    Spec.assertBool s (granter < protected) "setup: the Walls are swept before the creature they protect"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Indestructible granter board)) "setup: the Walls do not benefit from their own grant"
    Spec.assertBool s (Projection.hasKeyword Keyword.Indestructible protected board) "setup: their controller's other creature does"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Indestructible his board)) "setup: the opponent's does not"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield granter resolved)) "the Walls are destroyed"
    Spec.assertBool s (S.onBattlefield protected resolved) "the creature they protected stands"
    Spec.assertBool s (not (S.onBattlefield his resolved)) "and the opponent's creature, never protected, died"
  -- The same board with the two permanents added in the other order, so the
  -- Walls are swept LAST. CR 608.2f leaves nothing for the sweep order to
  -- decide here, and that is the claim: the outcome is identical. This is the
  -- arrangement the sequential reading happens to get right, and it is worth
  -- pinning precisely because it is the one that would keep passing.
  Spec.it s "CR 608.2f the outcome does not depend on where the granter falls in the sweep order" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    walls <- S.printingOf s registry "The Walls of Ba Sing Se"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (protected, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (granter, board) = S.addCreature walls S.alice g1
    Spec.assertBool s (protected < granter) "setup: the Walls are swept last this time"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield granter resolved)) "the Walls are destroyed"
    Spec.assertBool s (S.onBattlefield protected resolved) "the creature they protected stands"
  -- CR 608.2f a third time, now about the CR 616.1 loop each victim's
  -- put-into-graveyard runs rather than about the CR 702.12b gate above. The
  -- batch is one simultaneous event, so the replacement effects in force for
  -- it are the ones on the battlefield when it began -- including one
  -- belonging to a permanent the batch is itself killing.
  --
  -- Opalescence animates Rest in Peace (a non-Aura enchantment) into a 2/2,
  -- so Day of Judgment sweeps it alongside bob's Piker. Rest in Peace is
  -- added FIRST on purpose: it holds the lower ObjectId and is swept first,
  -- so an implementation that re-collected each victim's candidates from the
  -- live board would find it already gone by the time it reached the Piker
  -- and bury the Piker instead of exiling it.
  Spec.it s "CR 608.2f a Rest in Peace dying in the sweep still exiles the cards the sweep puts into graveyards" $ do
    plains <- S.printingOf s registry "Plains"
    opalescence <- S.printingOf s registry "Opalescence"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let (opal, g1) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (rip, g2) = S.addCreature restInPeace S.alice g1
        (his, board) = S.addCreature piker S.bob g2
    Spec.assertBool s (Projection.isCreatureOf rip board) "setup: Opalescence animates Rest in Peace"
    Spec.assertBool s (rip < his) "setup: Rest in Peace is swept before the Piker"
    let resolved = castDayOfJudgment plains dayOfJudgment board
    Spec.assertBool s (not (S.onBattlefield rip resolved)) "Rest in Peace died"
    Spec.assertBool s (not (S.onBattlefield his resolved)) "and so did the Piker"
    Spec.assertEqWith s "the Piker was exiled, not buried" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    Spec.assertEqWith s "the Piker's card is in exile" (length (Game.zoneMembers Zone.Exile S.bob resolved)) 1
    Spec.assertEqWith s "and Rest in Peace exiled its own card too" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 1
    Spec.assertBool s (S.onBattlefield opal resolved) "Opalescence animates each OTHER enchantment, so it stands"
  -- CR 115.10a: "Unless that object or player is identified by the word
  -- 'target' ... it's not a target." "All creatures" is not a target, so the
  -- card declares no target slot and the cast never raises a target prompt
  -- -- and CR 608.2b, which is about targets, has nothing to fizzle.
  Spec.it s "CR 115.10a Day of Judgment targets nothing: no target slot and no target prompt" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let card = Printing.card dayOfJudgment
        (his, g1) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (withSpell, spell) = S.handOne dayOfJudgment (List.foldl' (\gs _ -> snd (S.addCreature plains S.alice gs)) g1 [1 :: Int .. 4])
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseTargets {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asked = State.execState (Engine.runGame countingAnswer withSpell (S.cast S.alice spell)) 0
    Spec.assertEqWith s "no target slot anywhere on the card" (Modal.allTargetSlots (Face.spell (Card.combined card))) Map.empty
    Spec.assertEqWith s "and nothing was asked to target" asked 0
    -- The board still resolves the way the first test says it does, from the
    -- same cast -- so "targets nothing" is not "affects nothing".
    Spec.assertBool s (not (S.onBattlefield his (castDayOfJudgment plains dayOfJudgment g1))) "the creature still died"

-- Evacuation ({3}{U}{U} instant, "Return all creatures to their owners'
-- hands"), the pool's producer for a MoveToZone over a SET rather than over a
-- slot. Cast off five Islands from alice's hand and resolved, for the reason
-- castDayOfJudgment gives: the card takes no target and prompts for nothing, so
-- a hand-built applyEffect call would differ from a real cast only in the mana.
returnAllSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
returnAllSpec s registry = Spec.describe s "ReturnAll" $ do
  -- CR 109.2 makes "all creatures" every creature PERMANENT on the battlefield,
  -- and CR 400.3 files each arrival in its OWNER's hand -- so a 2/1 board splits
  -- 2/1 across the two hands rather than piling into the caster's. The land is
  -- what an implementation returning every permanent would trip over, and the
  -- 2/1 asymmetry is what one returning a creature per player would.
  Spec.it s "Evacuation returns every creature to its owner's hand and leaves a land alone" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    evacuation <- S.printingOf s registry "Evacuation"
    let (herFirst, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (herSecond, g2) = S.addCreature piker S.alice g1
        (his, g3) = S.addCreature piker S.bob g2
        (land, board) = S.addCreature forest S.alice g3
        (withSpell, spell) = S.handOne evacuation (List.foldl' (\gs _ -> snd (S.addCreature island S.alice gs)) board [1 :: Int .. 5])
        -- The baseline is taken AFTER the cast, where the Evacuation itself has
        -- already left alice's hand for the stack, so the two deltas below count
        -- returning creatures and nothing else.
        afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
        resolved = S.runPure S.identityAnswer afterCast Stack.resolveTop
        survivors = Set.difference (GameState.battlefield afterCast) (Set.fromList [herFirst, herSecond, his])
    Spec.assertEqWith
      s
      "exactly the three creatures left the battlefield, and each owner's hand grew by their own"
      ( GameState.battlefield resolved,
        Set.member land (GameState.battlefield resolved),
        S.handSize S.alice resolved - S.handSize S.alice afterCast,
        S.handSize S.bob resolved - S.handSize S.bob afterCast
      )
      (survivors, True, 2, 1)

-- CR 109.2a's reading of a description -- "a description of an object that
-- includes the word 'card' and the name of a zone ... means a card matching that
-- description in the stated zone" -- swept as a SET rather than targeted:
-- ObjectRef.EachCardInGraveyard, where returnAllSpec above is CR 109.2's
-- battlefield default.
--
-- Rise of the Dark Realms {7}{B}{B} Sorcery -- "Put all creature cards from all
-- graveyards onto the battlefield under your control." (name, cost, type line and
-- Oracle text checked against api.scryfall.com). Its whole text is that one
-- sentence, so nothing else on the card can be what these assertions read.
--
-- THREE SEATS, and a graveyard per seat, because the board has to tell four
-- readings of "all graveyards" apart:
--
--   * EACH PLAYER'S versus YOUR OWN. bob and carol each bury a creature card of a
--     printing nobody else has, and both must be reanimated.
--   * EACH PLAYER'S versus AN OPPONENT'S. alice buries one too, and a two-seat
--     board would leave "opponents" and "each other player" indistinguishable
--     besides.
--   * CREATURE CARDS versus the whole zone. Every graveyard also holds one
--     non-creature card, and each must stay buried.
--   * A GRAVEYARD versus the battlefield. bob controls a Benalish Hero, which the
--     battlefield reading of the same sentence would hand to alice "under your
--     control"; it stays bob's, and the graveyards empty of creatures instead.
--
-- Cast off nine Swamps and resolved, for the reason castDayOfJudgment gives: the
-- card takes no target and prompts for nothing, so a hand-built applyEffect call
-- would differ from a real cast only in the mana.
riseOfTheDarkRealmsSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
riseOfTheDarkRealmsSpec s registry = Spec.describe s "RiseOfTheDarkRealms" $ do
  Spec.it s "CR 109.2a every creature card in every graveyard is reanimated under the caster's control" $ do
    swamp <- S.printingOf s registry "Swamp"
    rise <- S.printingOf s registry "Rise of the Dark Realms"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    sentry <- S.printingOf s registry "Ogre Sentry"
    hero <- S.printingOf s registry "Benalish Hero"
    murder <- S.printingOf s registry "Murder"
    judgment <- S.printingOf s registry "Day of Judgment"
    forest <- S.printingOf s registry "Forest"
    let mana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (9 :: Int)]
        (heroId, withHero) = S.addCreature hero S.bob mana
        buried =
          List.foldl'
            (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g))
            withHero
            [ (piker, S.alice),
              (murder, S.alice),
              (maiden, S.bob),
              (judgment, S.bob),
              (sentry, S.carol),
              (forest, S.carol)
            ]
        (withSpell, spell) = S.handOne rise buried
        afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
        after = S.runPure S.identityAnswer afterCast Stack.resolveTop
        -- Every battlefield object that is not one of alice's nine Swamps, by
        -- name and CONTROLLER: CR 400.7 mints a new object at the destination, so
        -- a reanimated card cannot be found by the id it was buried under.
        reanimated gs =
          List.sort
            [ (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs)
            | oid <- Set.toList (GameState.battlefield gs),
              fmap S.nameOf (Game.cardOf oid gs) /= Just (S.nameOf (Printing.card swamp))
            ]
        named = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "all three buried creature cards are on the battlefield under alice's control, and bob keeps the one he already controlled"
      (reanimated after)
      ( List.sort
          [ (named "Benalish Hero", Just S.bob),
            (named "Bird Maiden", Just S.alice),
            (named "Goblin Piker", Just S.alice),
            (named "Ogre Sentry", Just S.alice)
          ]
      )
    Spec.assertEqWith
      s
      "each graveyard keeps its non-creature card, and alice's also takes the spent sorcery (CR 608.2n)"
      ( List.sort (namesIn Zone.Graveyard S.alice after),
        namesIn Zone.Graveyard S.bob after,
        namesIn Zone.Graveyard S.carol after
      )
      ( List.sort [named "Murder", named "Rise of the Dark Realms"],
        [named "Day of Judgment"],
        [named "Forest"]
      )
    Spec.assertBool s (S.onBattlefield heroId after) "bob's creature was never moved, so nothing swept the battlefield"

-- CR 608.2d's choice made WHILE APPLYING an effect, over a graveyard:
-- ObjectRef.ChosenCardInGraveyard, where riseOfTheDarkRealmsSpec above is the
-- same zone swept as a set.
--
-- Port of Karfell -- Land, "This land enters tapped. {T}: Add {U}. {3}{U}{B}{B},
-- {T}, Sacrifice this land: Mill four cards, then return a creature card from
-- your graveyard to the battlefield tapped." (name, type line and Oracle text
-- checked against api.scryfall.com). The whole card is transcribed.
--
-- NOT A TARGET, which is the distinction the arm exists for: the card never says
-- the word, so CR 115.1c leaves the ability untargeted, nothing is announced as
-- it goes on the stack (CR 601.2c) and nothing is re-checked at resolution (CR
-- 608.2b). A graveyard being a public zone (CR 400.2) is what would ALLOW such a
-- card to target -- it is not what makes this one choose.
--
-- THREE SEATS, and a board built so that five readings of "a creature card from
-- your graveyard" are told apart:
--
--   * THE CHOSEN card versus the FIRST matching one. alice buries two creature
--     cards; the answer is pinned to the second, and Replay.defaultAnswer -- what
--     S.identityAnswer falls through to -- picks the first. The two legs below
--     differ in the answerer and in nothing else, so an engine that picked for
--     the player would give the same card twice.
--   * A CREATURE CARD versus the whole zone. alice buries a Murder as well, and
--     the four Swamps her own mill puts there are candidates for no reading.
--   * YOUR graveyard versus each player's, and versus an opponent's. bob and
--     carol each bury a creature card of a printing alice does not have, and
--     both must stay buried.
--   * A GRAVEYARD versus the battlefield. carol controls a Benalish Hero, which
--     a battlefield reading of the same sentence could hand to alice.
--   * A CHOICE versus a sweep. Exactly one card comes back, though two match.
--
-- Ten lands rather than the six the ability costs: the payment taps sources one
-- prompt at a time, and a board with no slack could fail to cover {U}{B}{B} for
-- reasons that have nothing to do with what is under test.
portOfKarfellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
portOfKarfellSpec s registry =
  let -- alice controls five Swamps, five Islands and one untapped Port of
      -- Karfell; `buried` goes into the named graveyards in the order given, and
      -- `stock` into alice's library. Returns the Port's id.
      board port swamp island hero buried stock =
        let mana = S.landsFor island S.alice 5 (S.landsFor swamp S.alice 5 S.threePlayerGame)
            (_, withHero) = S.addCreature hero S.carol mana
            (portId, withPort) = S.addCreature port S.alice withHero
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) withPort buried
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) withGraves stock
         in (portId, withStock {GameState.priority = Just S.alice})
      -- The ability that mills and returns, told from the mana ability by the
      -- sacrifice its cost carries -- never by position in the list, which no
      -- rule fixes.
      returnAbility portId gs =
        filter
          (elem CostComponent.SacrificeThis . Cost.Type.components . ActivatedAbility.cost)
          (Activate.abilitiesFor portId gs)
      -- Activate the ability and resolve it, keeping the RESPONSES beside the
      -- board so the same call answers both "what happened" and "who was asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Maybe (GameState.GameState, [Response.Response])
      run answer portId gs = case returnAbility portId gs of
        [ability] ->
          let ((_, after), responses) = Replay.record answer gs (Activate.activateAbility S.alice portId ability >> Stack.resolveTop)
           in Just (after, responses)
        _ -> Nothing
      named = Just . CardName.MkCardName . Text.pack
      -- The WHOLE battlefield minus the basic lands: after the ability resolves
      -- that is carol's Benalish Hero, which nothing may move, and whatever came
      -- back -- the Port sacrificed itself to pay for the ability. By NAME, TAP
      -- STATE and CONTROLLER, because CR 400.7 mints a fresh id at the
      -- destination and CR 110.2a is what decides whose the arrival is.
      arrivals gs =
        List.sort
          [ (fmap S.nameOf (Game.cardOf oid gs), fmap Object.tapped (Game.lookupObject oid gs), Projection.controllerOf oid gs)
          | oid <- Set.toList (GameState.battlefield gs),
            notElem (fmap S.nameOf (Game.cardOf oid gs)) [named "Swamp", named "Island"]
          ]
      -- The board with nothing returned: carol's creature and nothing else.
      untouched = [(named "Benalish Hero", Just TapState.Untapped, Just S.carol)]
      wasAsked responses =
        let isChoice r = case r of
              Response.ChoseCardInGraveyard _ -> True
              _ -> False
         in any isChoice responses
   in Spec.describe s "PortOfKarfell" $ do
        -- The headline: the SECOND buried creature card comes back, tapped, and
        -- everything else stays where it was.
        Spec.it s "CR 608.2d the creature card the controller chose returns tapped" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice), (maiden, S.alice), (sentry, S.bob), (cavalry, S.carol)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
              -- The Bird Maiden's id, which is the SECOND of alice's two
              -- creature cards in ascending order -- graveyardCards' own order,
              -- and the order the prompt offers.
              maidenId = case Game.zoneMembers Zone.Graveyard S.alice gs of
                [_, _, third] -> Just third
                _ -> Nothing
              choosing wanted p = case p of
                Prompt.ChooseCardInGraveyard {} -> wanted
                _ -> S.identityAnswer p
          case (maidenId, maidenId >>= \wanted -> run (choosing wanted) portId gs) of
            (Just _, Just (after, responses)) -> do
              Spec.assertBool s (wasAsked responses) "the controller was asked which card to return"
              Spec.assertEqWith
                s
                "the Bird Maiden is on alice's battlefield, tapped, and nothing else arrived"
                (arrivals after)
                (List.sort ((named "Bird Maiden", Just TapState.Tapped, Just S.alice) : untouched))
              Spec.assertEqWith
                s
                "the unchosen creature card, the noncreature card, the four milled Swamps and the spent land stay in alice's graveyard"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort ([named "Goblin Piker", named "Murder", named "Port of Karfell"] <> replicate 4 (named "Swamp")))
              Spec.assertEqWith
                s
                "and neither opponent's graveyard was touched"
                (namesIn Zone.Graveyard S.bob after, namesIn Zone.Graveyard S.carol after)
                ([named "Ogre Sentry"], [named "Benalish Cavalry"])
            _ -> Spec.assertBool s False "expected exactly one returning ability and three cards in alice's graveyard"
        -- The paired control, and the whole reason the board buries TWO creature
        -- cards: the same activation on the same board with the DEFAULT answerer
        -- brings back the other one. If the engine were picking, both legs would
        -- name the same card.
        Spec.it s "CR 608.2d the engine does not pick: another answer returns the other card" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice), (maiden, S.alice)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, _) ->
              Spec.assertEqWith
                s
                "the first candidate comes back instead"
                (arrivals after)
                (List.sort ((named "Goblin Piker", Just TapState.Tapped, Just S.alice) : untouched))
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- Where the rules leave nothing to ask, don't prompt: one matching card
        -- is the whole of "a creature card in your graveyard". The board differs
        -- from the leg above in the Bird Maiden and nothing else.
        Spec.it s "one candidate elides the prompt and still returns the card" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith
                s
                "the lone candidate came back anyway"
                (arrivals after)
                (List.sort ((named "Goblin Piker", Just TapState.Tapped, Just S.alice) : untouched))
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- CR 101.3 and CR 609.3: a graveyard with nothing matching makes the
        -- instruction impossible, so it is ignored -- and nobody is asked. The
        -- mill still happens, which is what keeps this from passing because the
        -- ability never resolved at all.
        Spec.it s "CR 101.3 no matching card returns nothing and asks nothing" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(murder, S.alice), (sentry, S.bob)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith s "nothing arrived, and carol keeps the creature she controls" (arrivals after) untouched
              Spec.assertEqWith
                s
                "the mill still ran, so the ability really did resolve"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort ([named "Murder", named "Port of Karfell"] <> replicate 4 (named "Swamp")))
              Spec.assertEqWith s "and the opponent's creature card is not a candidate" (namesIn Zone.Graveyard S.bob after) [named "Ogre Sentry"]
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- An EMPTY graveyard and an empty library: the ability resolves, mills
        -- nothing (CR 701.17b), and returns nothing.
        Spec.it s "CR 609.3 an empty graveyard is a no-op rather than a failure" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          hero <- S.printingOf s registry "Benalish Hero"
          let (portId, gs) = board port swamp island hero [] []
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith s "nothing arrived, and carol keeps the creature she controls" (arrivals after) untouched
              Spec.assertEqWith s "only the land that paid for the ability is in the graveyard" (namesIn Zone.Graveyard S.alice after) [named "Port of Karfell"]
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"

-- The same arm reached from a TRIGGER rather than an activated ability, and over
-- LAND cards rather than creature cards -- the two axes portOfKarfellSpec above
-- holds fixed.
--
-- Blossoming Tortoise {2}{G}{G} Creature -- Turtle 3/3, "Whenever this creature
-- enters or attacks, mill three cards, then return a land card from your
-- graveyard to the battlefield tapped. Activated abilities of lands you control
-- cost {1} less to activate. Land creatures you control get +1/+1." (name, cost,
-- type line and Oracle text checked against api.scryfall.com). Only the trigger
-- is read here; the two static abilities are Pawl.ActivateSpec's.
--
-- Two land cards are buried and the answer is pinned to the SECOND, so the
-- assertion cannot be met by taking the first; a creature card is buried beside
-- them and the three cards the trigger's own mill adds are creature cards too, so
-- an arm that ignored the Filter would have five candidates rather than two.
blossomingTortoiseSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
blossomingTortoiseSpec s registry = Spec.describe s "BlossomingTortoise" $ do
  Spec.it s "CR 608.2d the enters trigger returns the land card its controller chose, tapped" $ do
    tortoise <- S.printingOf s registry "Blossoming Tortoise"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    let (tortoiseId, entered) = S.entersWithTrigger tortoise S.alice (Setup.emptyGame S.bothPlayers)
        buried = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) entered [forest, piker, island]
        gs = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) buried [piker, piker, piker]
        named = Just . CardName.MkCardName . Text.pack
        -- The Island, buried last and so the SECOND of the two land cards in the
        -- ascending order the prompt offers.
        wanted = case Game.zoneMembers Zone.Graveyard S.alice gs of
          [_, _, third] -> Just third
          _ -> Nothing
        choosing chosen p = case p of
          Prompt.ChooseCardInGraveyard {} -> chosen
          _ -> S.identityAnswer p
        onBattlefield gs1 =
          List.sort
            [ (fmap S.nameOf (Game.cardOf oid gs1), fmap Object.tapped (Game.lookupObject oid gs1))
            | oid <- Set.toList (GameState.battlefield gs1)
            ]
    case wanted of
      Nothing -> Spec.assertBool s False "expected three cards in alice's graveyard"
      Just chosen ->
        let answer :: Prompt.Prompt r -> r
            answer = choosing chosen
            placed = S.runPure answer gs Engine.placePendingTriggers
            resolved = S.runPure answer placed Stack.resolveTop
         in do
              Spec.assertEqWith
                s
                "the Island is on the battlefield tapped, beside the untapped Tortoise"
                (onBattlefield resolved)
                (List.sort [(named "Blossoming Tortoise", Just TapState.Untapped), (named "Island", Just TapState.Tapped)])
              Spec.assertEqWith
                s
                "the unchosen Forest, the buried creature card and the three milled ones stay put"
                (List.sort (namesIn Zone.Graveyard S.alice resolved))
                (List.sort (named "Forest" : replicate 4 (named "Goblin Piker")))
              Spec.assertBool s (S.onBattlefield tortoiseId resolved) "and the Tortoise itself never moved"

-- The same arm with a CHOOSER other than the resolving controller:
-- Chooser.EachInScope, where portOfKarfellSpec above is Chooser.TheController.
--
-- Exhume {1}{B} Sorcery -- "Each player puts a creature card from their graveyard
-- onto the battlefield." (name, cost, type line and Oracle text checked against
-- api.scryfall.com). Its whole text is that one sentence, so nothing else on the
-- card can be what these assertions read.
--
-- CR 608.2d has the player an effect instructs announce the choices it offers,
-- and this sentence instructs EACH PLAYER -- so each of them chooses, out of
-- their own graveyard alone, in APNAP order (CR 608.2e, CR 101.4). CR 110.2a then
-- gives each arrival to the player who put it there, which is the graveyard's own
-- player: a graveyard is filed under the card's owner (CR 400.3), so the card
-- writes EntryRiders' underOwner and every returning creature enters under its
-- owner rather than under the caster's control. That is the whole difference from
-- riseOfTheDarkRealmsSpec's "under your control".
--
-- THREE SEATS, with a board built so that the readings are told apart:
--
--   * EACH PLAYER as chooser versus the CONTROLLER as chooser. The answerer
--     replies by WHICH PLAYER the prompt names -- alice and carol take their
--     second candidate, bob his LAST, and bob's graveyard holds three so that
--     his three readings come apart: an engine that asked alice about every
--     graveyard would take bob's second card, one that chose for the player
--     would take his first, and only the right one takes his third. An engine
--     that asked one player about the union of the graveyards would return one
--     card rather than three besides.
--   * THE CHOSEN card versus the FIRST matching one. The paired leg below runs
--     the same board through Replay.defaultAnswer, which takes every first.
--   * EACH PLAYER'S OWN graveyard versus the union. No creature card ever
--     crosses seats, which the per-owner control assertion is what pins.
--   * A CREATURE CARD versus the whole zone. Each graveyard also holds a
--     noncreature card, and each must stay buried.
--   * ONE EACH versus a sweep. Two match per graveyard and one comes back.
exhumeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exhumeSpec s registry =
  let -- alice controls four Swamps -- twice what the spell costs, so a payment
      -- that taps one source at a time cannot fail for reasons of its own -- and
      -- holds an Exhume; `buried` goes into the named graveyards in the order
      -- given. Returns the spell's id.
      board exhume swamp buried =
        let mana = S.landsFor swamp S.alice 4 S.threePlayerGame
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) mana buried
            (withSpell, spell) = S.handOne exhume withGraves
         in (spell, withSpell {GameState.priority = Just S.alice})
      -- Cast and resolve, keeping the RESPONSES beside the board so the same call
      -- answers both "what came back" and "how many players were asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Response.Response])
      run answer spell gs =
        let ((_, after), responses) = Replay.record answer gs (S.cast S.alice spell >> Stack.resolveTop)
         in (after, responses)
      named = Just . CardName.MkCardName . Text.pack
      -- The whole battlefield minus alice's four Swamps, by NAME and CONTROLLER:
      -- CR 400.7 mints a fresh id at the destination, so a returned card cannot
      -- be found by the id it was buried under, and CR 110.2a is what decides
      -- whose the arrival is.
      arrivals gs =
        List.sort
          [ (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs)
          | oid <- Set.toList (GameState.battlefield gs),
            fmap S.nameOf (Game.cardOf oid gs) /= named "Swamp"
          ]
      choices responses =
        length
          [ () | Response.ChoseCardInGraveyard _ <- responses
          ]
      -- The prompt's candidates in the order it offers them, which is the order
      -- Resolve.graveyardCardsOf sorts each graveyard into.
      secondOf offered = case offered of
        _ NonEmpty.:| (second : _) -> second
        only NonEmpty.:| [] -> only
   in Spec.describe s "Exhume" $ do
        -- The headline: three players, three separate choices, three creatures
        -- back under three different controllers.
        Spec.it s "CR 608.2d each player chooses in their own graveyard, and keeps what they choose" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          forest <- S.printingOf s registry "Forest"
          let buried =
                [ (piker, S.alice),
                  (murder, S.alice),
                  (maiden, S.alice),
                  (sentry, S.bob),
                  (judgment, S.bob),
                  (cavalry, S.bob),
                  (wraith, S.bob),
                  (hero, S.carol),
                  (forest, S.carol),
                  (berserkers, S.carol)
                ]
              (spell, gs) = board exhume swamp buried
              -- BY THE PLAYER THE PROMPT NAMES, which is the whole assertion:
              -- alice and carol take their second candidate and bob his last, so
              -- no one answer can stand in for another's.
              choosing p = case p of
                Prompt.ChooseCardInGraveyard _ pid _ offered ->
                  if pid == S.bob then NonEmpty.last offered else secondOf offered
                _ -> S.identityAnswer p
              (after, responses) = run choosing spell gs
          Spec.assertEqWith s "all three players were asked" (choices responses) 3
          Spec.assertEqWith
            s
            "each player's own choice is on the battlefield under their own control"
            (arrivals after)
            ( List.sort
                [ (named "Bird Maiden", Just S.alice),
                  (named "Bog Wraith", Just S.bob),
                  (named "Berserkers of Blood Ridge", Just S.carol)
                ]
            )
          Spec.assertEqWith
            s
            "the unchosen creature cards and the noncreature stay buried in every graveyard, and the spent sorcery joins alice's (CR 608.2n)"
            ( List.sort (namesIn Zone.Graveyard S.alice after),
              List.sort (namesIn Zone.Graveyard S.bob after),
              List.sort (namesIn Zone.Graveyard S.carol after)
            )
            ( List.sort [named "Goblin Piker", named "Murder", named "Exhume"],
              List.sort [named "Ogre Sentry", named "Benalish Cavalry", named "Day of Judgment"],
              List.sort [named "Benalish Hero", named "Forest"]
            )
        -- The paired control, and the whole reason each graveyard buries TWO
        -- creature cards: the same cast on the same board with the DEFAULT
        -- answerer brings back the other one in every seat. If the engine were
        -- picking, both legs would name the same three cards.
        Spec.it s "CR 608.2d the engine does not pick: another answer returns the other card in every seat" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          forest <- S.printingOf s registry "Forest"
          let buried =
                [ (piker, S.alice),
                  (murder, S.alice),
                  (maiden, S.alice),
                  (sentry, S.bob),
                  (judgment, S.bob),
                  (cavalry, S.bob),
                  (wraith, S.bob),
                  (hero, S.carol),
                  (forest, S.carol),
                  (berserkers, S.carol)
                ]
              (spell, gs) = board exhume swamp buried
              (after, _) = run S.identityAnswer spell gs
          Spec.assertEqWith
            s
            "each seat's first candidate comes back instead"
            (arrivals after)
            ( List.sort
                [ (named "Goblin Piker", Just S.alice),
                  (named "Ogre Sentry", Just S.bob),
                  (named "Benalish Hero", Just S.carol)
                ]
            )
        -- CR 101.3 and CR 609.3 applied PER PLAYER: a graveyard with nothing
        -- matching drops that player out of the batch rather than the
        -- instruction out of the effect, and a graveyard with exactly one
        -- matching card leaves them nothing to decide, so they are not asked.
        Spec.it s "CR 101.3 an empty share is skipped and a forced one is not asked" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          forest <- S.printingOf s registry "Forest"
          let buried = [(piker, S.alice), (maiden, S.alice), (sentry, S.bob), (forest, S.carol)]
              (spell, gs) = board exhume swamp buried
              choosing p = case p of
                Prompt.ChooseCardInGraveyard _ _ _ offered -> secondOf offered
                _ -> S.identityAnswer p
              (after, responses) = run choosing spell gs
          Spec.assertEqWith s "only alice, who had two candidates, was asked" (choices responses) 1
          Spec.assertEqWith
            s
            "alice's chosen card and bob's forced one came back; carol had no creature card and nothing happened for her"
            (arrivals after)
            (List.sort [(named "Bird Maiden", Just S.alice), (named "Ogre Sentry", Just S.bob)])
          Spec.assertEqWith s "and carol's noncreature card is still buried" (namesIn Zone.Graveyard S.carol after) [named "Forest"]
        -- Every graveyard empty: the spell resolves, asks nobody and returns
        -- nothing. The spent sorcery in alice's graveyard is what keeps this from
        -- passing because the spell never resolved at all.
        Spec.it s "CR 609.3 empty graveyards are a no-op rather than a failure" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          let (spell, gs) = board exhume swamp []
              (after, responses) = run S.identityAnswer spell gs
          Spec.assertEqWith s "nobody was asked" (choices responses) 0
          Spec.assertEqWith s "nothing arrived" (arrivals after) []
          Spec.assertEqWith s "and the spell really did resolve" (namesIn Zone.Graveyard S.alice after) [named "Exhume"]

-- TWO chosen graveyard cards in ONE resolution, where the second must not be the
-- first: Blood for Bones, the card that made #1433 look like a missing exclusion.
--
-- Blood for Bones {3}{B} Sorcery -- "As an additional cost to cast this spell,
-- sacrifice a creature. Return a creature card from your graveyard to the
-- battlefield, then return another creature card from your graveyard to your
-- hand." (name, cost, type line and Oracle text checked against
-- api.scryfall.com). The whole card is transcribed.
--
-- "ANOTHER" NEEDS NO EXCLUSION HERE: the two returns are two effects of one
-- resolution, each gathering its own candidates from the state it runs in (CR
-- 608.2c), and the first return has already taken its card out of the graveyard
-- -- CR 400.7 mints a new object at the destination and retires the old id --
-- before the second is offered. So the second choice cannot see the first, and
-- what the printed word forbids is already impossible.
--
-- The ANSWERER BELOW ASKS FOR IT ANYWAY, naming the first return's card at both
-- prompts, which is what keeps that from being an assumption: a second gather
-- that read a PRE-MOVE snapshot would put that id back on offer, the answerer
-- would take it, and the move of an id that no longer resolves would leave the
-- hand empty rather than holding the card these assertions name. No mutation
-- makes the engine offer it, because nothing in the engine can -- so this leg is
-- a regression fence for that property rather than a proof of an exclusion rule
-- pawl does not have.
--
-- The additional cost is load-bearing beside that: the sacrificed creature is in
-- the graveyard by the time the spell resolves, so it is a candidate for both
-- returns.
bloodForBonesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bloodForBonesSpec s registry =
  let -- alice controls six Swamps -- slack over the spell's four, for
      -- portOfKarfellSpec's reason -- and ONE creature, so the additional cost's
      -- victim is forced and no prompt of its own can be mistaken for the
      -- returns' prompts. `buried` goes into the named graveyards in the order
      -- given.
      board blood swamp victim buried =
        let mana = S.landsFor swamp S.alice 6 S.threePlayerGame
            (_, withVictim) = S.addCreature victim S.alice mana
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) withVictim buried
            (withSpell, spell) = S.handOne blood withGraves
         in (spell, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      arrivals gs =
        List.sort
          [ (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs)
          | oid <- Set.toList (GameState.battlefield gs),
            fmap S.nameOf (Game.cardOf oid gs) /= named "Swamp"
          ]
      choices responses =
        length
          [ () | Response.ChoseCardInGraveyard _ <- responses
          ]
   in Spec.describe s "BloodForBones" $ do
        -- The headline: the card alice chose is on the battlefield, a DIFFERENT
        -- one she chose is in her hand, and the answerer asked for the first one
        -- both times.
        Spec.it s "CR 608.2c the second return cannot take the card the first one took" $ do
          blood <- S.printingOf s registry "Blood for Bones"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          sentry <- S.printingOf s registry "Ogre Sentry"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(maiden, S.alice), (murder, S.alice), (cavalry, S.alice), (sentry, S.alice), (hero, S.bob)]
              (spell, gs) = board blood swamp piker buried
              -- Pinned BY NAME rather than by position: the Ogre Sentry is the
              -- third of the four creature cards alice's graveyard holds once the
              -- Piker has paid the cost, so it is neither the first candidate nor
              -- next to it, and the Piker is the last.
              wantedBy name gs1 =
                Maybe.listToMaybe
                  [ oid
                  | oid <- Game.zoneMembers Zone.Graveyard S.alice gs1,
                    fmap S.nameOf (Game.cardOf oid gs1) == named name
                  ]
              -- FIRST the Sentry, and then the Sentry AGAIN -- which the second
              -- return cannot grant, so the fallback is the pinned Piker.
              choosing sentryId pikerId p = case p of
                Prompt.ChooseCardInGraveyard _ _ _ offered ->
                  if List.elem sentryId (NonEmpty.toList offered) then sentryId else pikerId
                _ -> S.identityAnswer p
              afterCast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
          case (wantedBy "Ogre Sentry" afterCast, wantedBy "Goblin Piker" afterCast) of
            (Just sentryId, Just pikerId) -> do
              let answer :: Prompt.Prompt r -> r
                  answer = choosing sentryId pikerId
                  ((_, after), responses) = Replay.record answer afterCast Stack.resolveTop
              Spec.assertEqWith s "alice was asked twice" (choices responses) 2
              Spec.assertEqWith
                s
                "the Ogre Sentry she chose first is on the battlefield under her control"
                (arrivals after)
                [(named "Ogre Sentry", Just S.alice)]
              Spec.assertEqWith
                s
                "and the Goblin Piker -- not the Sentry the answerer asked for twice -- is the card in her hand"
                (namesIn Zone.Hand S.alice after)
                [named "Goblin Piker"]
              Spec.assertEqWith
                s
                "the two cards neither return took, the noncreature and the spent sorcery stay in her graveyard"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort [named "Bird Maiden", named "Benalish Cavalry", named "Murder", named "Blood for Bones"])
              Spec.assertEqWith s "and bob's creature card was never a candidate" (namesIn Zone.Graveyard S.bob after) [named "Benalish Hero"]
            _ -> Spec.assertBool s False "expected the sacrificed Piker and the buried Sentry in alice's graveyard after the cast"
        -- The paired control on the same board: the default answerer takes the
        -- first candidate each time, so BOTH returns name different cards than
        -- the leg above -- and they are still different from each other.
        Spec.it s "CR 608.2d the engine does not pick: another answer moves two other cards" $ do
          blood <- S.printingOf s registry "Blood for Bones"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          sentry <- S.printingOf s registry "Ogre Sentry"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(maiden, S.alice), (murder, S.alice), (cavalry, S.alice), (sentry, S.alice), (hero, S.bob)]
              (spell, gs) = board blood swamp piker buried
              after = S.runPure S.identityAnswer gs (S.cast S.alice spell >> Stack.resolveTop)
          Spec.assertEqWith
            s
            "the first candidate is on the battlefield"
            (arrivals after)
            [(named "Bird Maiden", Just S.alice)]
          Spec.assertEqWith s "and the next one is in hand" (namesIn Zone.Hand S.alice after) [named "Benalish Cavalry"]
        -- Where "another" and "a creature card" come apart: ONE creature card in
        -- the whole graveyard. The first return takes it, the second has nothing
        -- left to name and is ignored (CR 101.3, CR 609.3), and nobody is asked
        -- at either step -- one candidate is not a choice.
        Spec.it s "CR 101.3 a lone creature card returns to the battlefield and nothing goes to hand" $ do
          blood <- S.printingOf s registry "Blood for Bones"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(murder, S.alice), (hero, S.bob)]
              (spell, gs) = board blood swamp piker buried
              ((_, after), responses) = Replay.record S.identityAnswer gs (S.cast S.alice spell >> Stack.resolveTop)
          Spec.assertEqWith s "neither return had anything to ask" (choices responses) 0
          Spec.assertEqWith
            s
            "the sacrificed Piker is the lone candidate and it comes back"
            (arrivals after)
            [(named "Goblin Piker", Just S.alice)]
          Spec.assertEqWith s "and alice's hand is empty" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith
            s
            "the noncreature card and the spent sorcery stay buried"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.sort [named "Murder", named "Blood for Bones"])

-- The third CHOOSER, and the effect that fills it: Chooser.BoundInSlot over a
-- slot Effect.ChooseOpponent bound as the same resolution ran.
--
-- Skullwinder {2}{G} Creature -- Snake 1/3: "Deathtouch. When this creature
-- enters, return target card from your graveyard to your hand, then choose an
-- opponent. That player returns a card from their graveyard to their hand."
-- (name, cost, type line, power, toughness and Oracle text checked against
-- api.scryfall.com). The whole card is transcribed; nothing is elided.
--
-- TWO CHOICES AND AN ORDER BETWEEN THEM, which is the unit:
--
--   * WHICH OPPONENT, announced by the RESOLVING CONTROLLER as the effect is
--     applied (CR 608.2c, CR 608.2d). Not a target -- CR 115.10a makes a player
--     a target only where the text identifies them with the word, and this
--     sentence does not -- so no slot was announced at CR 601.2c and CR 608.2b
--     re-validates nothing.
--   * WHICH CARD, announced by THAT PLAYER, out of their own graveyard (CR
--     608.2d again: the player an effect instructs is the one who announces its
--     choices). "That player ... their graveyard" is the possessive Exhume's
--     "each player ... their graveyard" is, over one seat instead of every seat.
--
-- The order is the printed one (CR 608.2c "in the order written"): the opponent
-- must be chosen before there is a player for the second sentence to instruct.
--
-- CR 603.3d is why every leg stocks ALICE's graveyard: the first sentence has a
-- required target, and a trigger that can choose no legal target is removed from
-- the stack, taking the sentences after it along.
--
-- THREE SEATS, with the board built so the readings come apart -- a duel would
-- collapse "an opponent" onto the only other player and prove nothing:
--
--   * SOMEBODY ELSE CHOSE versus THE CONTROLLER CHOSE, twice over. The opponent
--     answer is pinned to CAROL, the LAST candidate, where the default answerer
--     takes bob; and the card answer is pinned to carol's THIRD, where the
--     default takes her first. The paired leg below runs the same board through
--     that default answerer and lands two different cards in a different hand.
--   * THE CHOSEN PLAYER was asked versus THE CONTROLLER was asked about their
--     graveyard. The answerer replies by the player the prompt NAMES: only a
--     prompt aimed at carol gets the third candidate, and one aimed at anybody
--     else gets the first.
--   * THEIR OWN graveyard versus the union of all of them. Alice's remaining
--     card and bob's three come before carol's in the union's ascending order,
--     so the THIRD candidate of a union is one of BOB's -- a different card, in a
--     different hand, than the third of carol's own.
--   * A GRAVEYARD RETURN at all versus a battlefield sweep: bob's graveyard must
--     be untouched in every leg.
skullwinderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
skullwinderSpec s registry =
  let -- alice controls four Forests -- slack over the creature's {2}{G}, so no
      -- payment order can fail for reasons of its own -- and holds a
      -- Skullwinder. `buried` goes into the named graveyards in the order given,
      -- which is also the ascending-id order the prompts offer them in.
      board skullwinder forest buried =
        let mana = S.landsFor forest S.alice 4 S.threePlayerGame
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) mana buried
            (withCard, handId) = S.handOne skullwinder withGraves
         in (handId, withCard {GameState.priority = Just S.alice})
      -- Cast, let the creature enter, let CR 603.3b put the enters trigger on the
      -- stack (which is where CR 603.3d picks its target), then resolve it.
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Response.Response])
      run answer handId gs =
        let ((_, after), responses) =
              Replay.record answer gs $ do
                S.cast S.alice handId
                Stack.resolveTop
                Engine.settleForPriority
                Stack.resolveTop
         in (after, responses)
      named = Just . CardName.MkCardName . Text.pack
      opponentChoices responses = length [() | Response.ChoseOpponent _ <- responses]
      cardChoices responses = length [() | Response.ChoseCardInGraveyard _ <- responses]
      -- The third candidate the prompt offers, by POSITION rather than by name:
      -- a name would be found again in a union of every graveyard, and the
      -- position is what tells the two offers apart.
      thirdOf offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop 2 (NonEmpty.toList offered)))
      -- Pinned to CAROL and to her THIRD card. Keyed on the player the prompt
      -- NAMES, so a prompt put to anybody else takes the first candidate and
      -- lands a different card in a different hand.
      choosing p = case p of
        Prompt.ChooseOpponent _ _ _ offered -> NonEmpty.last offered
        Prompt.ChooseCardInGraveyard _ pid _ offered ->
          if pid == S.carol then thirdOf offered else NonEmpty.head offered
        _ -> S.identityAnswer p
      -- alice's two, bob's three and carol's three, all distinct names so no
      -- assertion can read one seat's card as another's.
      stock hero cavalry berserkers murder maiden sentry judgment wraith =
        [ (murder, S.alice),
          (maiden, S.alice),
          (sentry, S.bob),
          (judgment, S.bob),
          (wraith, S.bob),
          (hero, S.carol),
          (cavalry, S.carol),
          (berserkers, S.carol)
        ]
   in Spec.describe s "Skullwinder" $ do
        -- The headline: alice picks the opponent, carol picks the card, and the
        -- card that moves is the one CAROL named out of CAROL's graveyard.
        Spec.it s "CR 608.2d the chosen opponent chooses in their own graveyard, and keeps what they choose" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          let (handId, gs) = board skullwinder forest (stock hero cavalry berserkers murder maiden sentry judgment wraith)
              (after, responses) = run choosing handId gs
          Spec.assertEqWith s "one opponent was chosen" (opponentChoices responses) 1
          Spec.assertEqWith s "and exactly one graveyard card choice was put, not one per seat" (cardChoices responses) 1
          Spec.assertEqWith
            s
            "carol's THIRD card is in carol's hand"
            (namesIn Zone.Hand S.carol after)
            [named "Berserkers of Blood Ridge"]
          Spec.assertEqWith
            s
            "her other two stay buried"
            (List.sort (namesIn Zone.Graveyard S.carol after))
            (List.sort [named "Benalish Hero", named "Benalish Cavalry"])
          Spec.assertEqWith
            s
            "bob was not the opponent chosen, so his graveyard is whole and his hand empty"
            (List.sort (namesIn Zone.Graveyard S.bob after), namesIn Zone.Hand S.bob after)
            (List.sort [named "Ogre Sentry", named "Day of Judgment", named "Bog Wraith"], [])
          Spec.assertEqWith
            s
            "alice got her own targeted card back and nothing else"
            (namesIn Zone.Hand S.alice after, namesIn Zone.Graveyard S.alice after)
            ([named "Murder"], [named "Bird Maiden"])
          Spec.assertEqWith
            s
            "and the Snake itself is on the battlefield"
            (List.sort [fmap S.nameOf (Game.cardOf oid after) | oid <- Set.toList (GameState.battlefield after), fmap S.nameOf (Game.cardOf oid after) /= named "Forest"])
            [named "Skullwinder"]
        -- The paired control, on the SAME board with only the answerer changed:
        -- the default takes the first opponent and the first card, so a different
        -- seat is asked and a different card moves. Two seats' worth of
        -- difference from one answerer swap is what tells "the engine chose" from
        -- "the players chose".
        Spec.it s "CR 608.2d the engine picks neither choice: another answer names another seat and another card" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          let (handId, gs) = board skullwinder forest (stock hero cavalry berserkers murder maiden sentry judgment wraith)
              (after, _) = run S.identityAnswer handId gs
          Spec.assertEqWith s "bob is the first opponent, and his first card comes back" (namesIn Zone.Hand S.bob after) [named "Ogre Sentry"]
          Spec.assertEqWith s "carol is untouched" (namesIn Zone.Hand S.carol after, length (namesIn Zone.Graveyard S.carol after)) ([], 3)
        -- CR 101.3 / CR 609.3 for the chosen player: an empty graveyard leaves
        -- nothing to name, so that share of the instruction is ignored rather
        -- than the trigger failing. The first sentence's return is what proves
        -- the ability really did resolve.
        Spec.it s "CR 101.3 a chosen opponent with an empty graveyard is a no-op rather than a failure" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let buried = [(murder, S.alice), (maiden, S.alice), (sentry, S.bob)]
              (handId, gs) = board skullwinder forest buried
              (after, responses) = run choosing handId gs
          Spec.assertEqWith s "carol was chosen and had nothing to be asked about" (opponentChoices responses, cardChoices responses) (1, 0)
          Spec.assertEqWith s "nothing came out of any graveyard but alice's own target" (namesIn Zone.Hand S.carol after, namesIn Zone.Hand S.bob after) ([], [])
          Spec.assertEqWith s "bob's graveyard is whole" (namesIn Zone.Graveyard S.bob after) [named "Ogre Sentry"]
          Spec.assertEqWith s "and the trigger really did resolve" (namesIn Zone.Hand S.alice after) [named "Murder"]
        -- One candidate is not a choice: the chosen player is NOT asked, and the
        -- lone card still comes back. Paired with the leg above on the same
        -- board, one card apart.
        Spec.it s "CR 608.2d a lone card in the chosen player's graveyard is not put to them" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(murder, S.alice), (maiden, S.alice), (sentry, S.bob), (hero, S.carol)]
              (handId, gs) = board skullwinder forest buried
              (after, responses) = run choosing handId gs
          Spec.assertEqWith s "the opponent was chosen, the card was not asked about" (opponentChoices responses, cardChoices responses) (1, 0)
          Spec.assertEqWith s "and carol's only card came back anyway" (namesIn Zone.Hand S.carol after, namesIn Zone.Graveyard S.carol after) ([named "Benalish Hero"], [])

-- CR 401.2's ordered pile named by POSITION rather than by characteristics:
-- ObjectRef.TopOfLibrary, the arm no Filter could stand in for.
--
-- Count on Luck {R}{R}{R} Enchantment -- "At the beginning of your upkeep, exile
-- the top card of your library. You may play that card this turn." (name, cost,
-- type line and Oracle text checked against api.scryfall.com). Its whole text is
-- the one trigger, so nothing else on the card can be what these assertions read.
--
-- The board is built so that three readings of "the top card of your library"
-- are told apart, since a board that cannot distinguish them proves nothing:
--
--   * TOP versus any other card. alice's library holds three distinct cards, so
--     an arm reading the bottom or an arbitrary member names a different one.
--   * YOUR library versus each player's. bob's library is stocked too, with a
--     printing that appears nowhere in alice's, and it must be untouched -- which
--     also covers the "target opponent's" reading.
--   * A LIBRARY read at all versus a battlefield sweep. The enchantment itself
--     and nothing else is on the battlefield, and it stays there.
--
-- The permission the second sentence grants is asserted only as far as CR 400.7's
-- binding goes: the exiled card carries one, which is what proves the MoveToZone's
-- slot bound the incarnation this arm minted rather than some other object.
countOnLuckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
countOnLuckSpec s registry =
  let -- alice controls Count on Luck and her library holds `stock`, DEEPEST
      -- FIRST -- S.addLibraryCard puts each card on top, so the last name given
      -- is the top card. bob's library holds one Ogre Sentry, a printing alice
      -- never has. Her upkeep then begins, the trigger goes on the stack and
      -- resolves.
      board stock = do
        countOnLuck <- S.printingOf s registry "Count on Luck"
        sentry <- S.printingOf s registry "Ogre Sentry"
        stocked <- mapM (S.printingOf s registry) stock
        let (luckId, g1) = S.addCreature countOnLuck S.alice (Setup.emptyGame S.bothPlayers)
            g2 = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) g1 stocked
            g3 = snd (S.addLibraryCard sentry S.bob g2)
            upkeep = Phase.Beginning BeginningStep.Upkeep
            begun =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
                (g3 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            onStack = S.runPure S.identityAnswer begun Engine.settleForPriority
        pure (luckId, S.runPure S.identityAnswer onStack Engine.priorityLoop)
      -- What the top-level namesIn answers with. It reports a zone in its own
      -- order, which for a library is top first -- Pawl.Engine.Game.zoneMembers
      -- hands the Seq back as stored.
      named = Just . CardName.MkCardName . Text.pack
      permissionsIn pid gs = fmap Object.playableFromExile (Maybe.mapMaybe (\oid -> Game.lookupObject oid gs) (Game.zoneMembers Zone.Exile pid gs))
   in Spec.describe s "CountOnLuck" $ do
        Spec.it s "CR 401.2 the top card of your library, and only it, is exiled" $ do
          (luckId, after) <- board ["Goblin Piker", "Bird Maiden", "Benalish Hero"]
          Spec.assertEqWith
            s
            "the Benalish Hero on top is in exile and the two under it are still in the library, in order"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Library S.alice after)
            ([named "Benalish Hero"], [named "Bird Maiden", named "Goblin Piker"])
          Spec.assertEqWith
            s
            "bob's library is untouched, so this is not each player's library and not an opponent's"
            (namesIn Zone.Library S.bob after, namesIn Zone.Exile S.bob after)
            ([named "Ogre Sentry"], [])
          Spec.assertBool s (S.onBattlefield luckId after) "the enchantment is still on the battlefield, so nothing swept it"
          Spec.assertEqWith
            s
            "the one exiled card carries the play permission, so the move bound the incarnation IT minted"
            (fmap Maybe.isJust (permissionsIn S.alice after))
            [True]
        -- The empty-library case, which is the same board minus the stock alone.
        -- CR 104.3c takes nobody out of the game here: an empty library only
        -- loses when its owner would DRAW from it, and the trigger draws nothing.
        Spec.it s "CR 401.2 an empty library has no top card, so the exile does nothing" $ do
          (luckId, after) <- board []
          Spec.assertEqWith
            s
            "nothing at all was exiled"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Exile S.bob after)
            ([], [])
          Spec.assertEqWith s "bob's library is still untouched" (namesIn Zone.Library S.bob after) [named "Ogre Sentry"]
          Spec.assertBool s (S.onBattlefield luckId after) "and alice is still in the game with her enchantment"
          Spec.assertEqWith s "the game has no result: an empty library is not itself a loss" (GameState.result after) Nothing

-- The DEPTH on ObjectRef.TopOfLibrary, and the group binding a move of several
-- cards owes its second sentence.
--
-- Act on Impulse {2}{R} Sorcery -- "Exile the top three cards of your library.
-- Until end of turn, you may play those cards." (name, cost, type line and Oracle
-- text checked against api.scryfall.com). Its whole printed text is those two
-- sentences, so nothing else on the card can be what these assertions read.
--
-- alice casts it off three Mountains and the priority loop resolves it, which is
-- what makes this gameplay-level rather than an applyEffect call.
--
-- The board tells the readings apart that a wrong depth or a wrong binding would
-- take:
--
--   * THREE versus one, and versus all of them. Her library holds FIVE distinct
--     cards, so "the top card" leaves four behind and "her library" leaves none;
--     both the exiled three and the two left are asserted, in the pile's order
--     for the two that stay (CR 401.2).
--   * THE TOP three versus the bottom three. The five are distinct printings, so
--     the two answers name disjoint sets.
--   * YOUR library versus each player's. bob's library is stocked with a
--     printing alice never has, and it must be untouched.
--   * THOSE CARDS versus one of them. All three exiled cards must carry the play
--     permission: a MoveToZone that bound only the last incarnation it minted
--     leaves two of the three without one.
actOnImpulseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
actOnImpulseSpec s registry =
  let -- alice's library holds `stock`, DEEPEST FIRST -- S.addLibraryCard puts
      -- each card on top, so the last name given is the top card. bob's library
      -- holds one Ogre Sentry, a printing alice never has.
      board stock = do
        mountain <- S.printingOf s registry "Mountain"
        actOnImpulse <- S.printingOf s registry "Act on Impulse"
        sentry <- S.printingOf s registry "Ogre Sentry"
        stocked <- mapM (S.printingOf s registry) stock
        let g1 = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) (S.landsInPlay mountain 3) stocked
            g2 = snd (S.addLibraryCard sentry S.bob g1)
            (withSpell, spell) = S.handOne actOnImpulse g2
            afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
        pure (S.runPure S.identityAnswer afterCast Engine.priorityLoop)
      named = Just . CardName.MkCardName . Text.pack
      -- SORTED, because exile is a holding area with no order of its own (CR
      -- 406.1) -- unlike the library below, whose order CR 401.2 fixes.
      exiledNames pid = List.sort . namesIn Zone.Exile pid
      permissionsIn pid gs = fmap (Maybe.isJust . Object.playableFromExile) (Maybe.mapMaybe (\oid -> Game.lookupObject oid gs) (Game.zoneMembers Zone.Exile pid gs))
   in Spec.describe s "ActOnImpulse" $ do
        Spec.it s "CR 401.2 the top three cards of your library are exiled, and the rest stay put" $ do
          after <- board ["Goblin Piker", "Bird Maiden", "Benalish Hero", "Hill Giant", "Sabretooth Tiger"]
          Spec.assertEqWith
            s
            "the top three are in exile and the two under them are still in the library, in order"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            ( List.sort [named "Sabretooth Tiger", named "Hill Giant", named "Benalish Hero"],
              [named "Bird Maiden", named "Goblin Piker"]
            )
          Spec.assertEqWith
            s
            "bob's library is untouched, so this is not each player's library"
            (namesIn Zone.Library S.bob after, namesIn Zone.Exile S.bob after)
            ([named "Ogre Sentry"], [])
          Spec.assertEqWith
            s
            "ALL THREE exiled cards carry the play permission, so the move bound the whole group and not one incarnation of it"
            (permissionsIn S.alice after)
            [True, True, True]
        -- Fewer cards than the depth: CR 609.3 does only as much as possible, and
        -- CR 104.3c takes nobody out of the game for it -- an empty library only
        -- loses when its owner would DRAW from it, and this spell draws nothing.
        Spec.it s "CR 609.3 a library shorter than the depth gives up what it has" $ do
          after <- board ["Goblin Piker", "Bird Maiden"]
          Spec.assertEqWith
            s
            "both cards were exiled and the library is empty"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            (List.sort [named "Goblin Piker", named "Bird Maiden"], [])
          Spec.assertEqWith s "and both carry the permission" (permissionsIn S.alice after) [True, True]
          Spec.assertEqWith s "the game has no result: an empty library is not itself a loss" (GameState.result after) Nothing
        -- ONE card, which is the other binding shape: a single arrival binds the
        -- singular slot, and the permission still reaches it.
        Spec.it s "CR 609.3 a one-card library gives up its one card" $ do
          after <- board ["Goblin Piker"]
          Spec.assertEqWith s "the one card is exiled" (exiledNames S.alice after) [named "Goblin Piker"]
          Spec.assertEqWith s "and carries the permission" (permissionsIn S.alice after) [True]
        Spec.it s "CR 401.2 an empty library has no top cards, so the exile does nothing" $ do
          after <- board []
          Spec.assertEqWith
            s
            "nothing at all was exiled, by either player"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Exile S.bob after)
            ([], [])
          Spec.assertEqWith s "bob's library is still untouched" (namesIn Zone.Library S.bob after) [named "Ogre Sentry"]
          Spec.assertEqWith s "the game has no result" (GameState.result after) Nothing

-- alice is mid-combat with one creature per printing in `mine`, bob defends with
-- one per printing in `theirs`, and alice holds a Trumpet Blast plus exactly the
-- three Mountains that pay for it. The board sits at the declare attackers step
-- like every combatBoardOf board, so the ENGINE declares the attack and the
-- combat record every test below reads is its own, never hand-written.
-- S.addCreature is what puts the Mountains out: the "any printing, on the
-- battlefield, untapped and Settled" helper its haddock says it is.
trumpetBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
trumpetBoard mountain trumpetBlast mine theirs =
  let (gs0, ours, yours) = S.combatBoardOf mine theirs
      withLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) gs0 [1 :: Int .. 3]
      (withCard, _) = S.handOne trumpetBlast withLands
   in ( -- handOne parks its state in a precombat main phase; this board is
        -- mid-combat.
        withCard
          { GameState.phase = GameState.phase gs0,
            GameState.priority = GameState.priority gs0
          },
        ours,
        yours
      )

-- Attack with everything, cast whenever a cast is offered, and never block.
-- Blocks are DECLINED so the attacker survives into the postcombat main phase,
-- which is where the "the set does not shrink either" leg reads it.
attackAndCast :: Prompt.Prompt r -> r
attackAndCast p = case p of
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Run whole steps until `step` is the current phase, WITHOUT running it. Bounded
-- so a bug cannot loop forever.
runToStep :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToStep step answer gs0 =
  let go n g =
        if n <= (0 :: Int) || GameState.phase g == step
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 8 gs0

-- Every stored continuous effect's affected set. CR 611.2c is a claim about
-- exactly this field, so the tests below read it directly as well as through the
-- projection: a filter stored here and re-evaluated would pass a naive
-- power-is-4 assertion.
affectedSets :: GameState.GameState -> [Affected.Affected]
affectedSets = fmap ContinuousEffect.affected . GameState.continuousEffects

-- The attacking creatures, by id, in the engine's own combat record.
attackerIds :: GameState.GameState -> [ObjectId.ObjectId]
attackerIds = Map.keys . Combat.Type.attackers . GameState.combat

-- Trumpet Blast ({2}{R} instant, "Attacking creatures get +2/+0 until end of
-- turn") is the pool's first card whose CONTINUOUS effect names a filter-selected
-- set rather than a target. Day of Judgment's EachMatching feeds a ONE-SHOT, so
-- CR 608.2c/608.2f are the whole of its story; this one is stored and keeps
-- applying, which puts it under CR 611.2c as well:
--
--   "If a continuous effect generated by the resolution of a spell or ability
--   modifies the characteristics or changes the controller of any objects, the
--   set of objects it affects is determined when that continuous effect begins.
--   After that point, the set won't change."
--
-- So the sweep happens ONCE, at resolution, and its RESULT is frozen into the
-- stored effect as Affected.TheseObjects. The three legs below are the ones a
-- stored-and-re-evaluated Filter would fail: it would pump a creature that
-- became attacking later, and drop the pump from one that left combat.
--
-- The modification is layer 7c (CR 613.4c: "effects and counters that modify
-- power and/or toughness"), the same layer Giant Growth's already lands in --
-- what is new here is the affected set, not the modification.
trumpetBlastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trumpetBlastSpec s registry = Spec.describe s "TrumpetBlast" $ do
  -- CR 109.2: "attacking creatures" names no zone and no card, so it means
  -- attacking creature PERMANENTS on the battlefield -- both players', if both
  -- had attackers, and pointedly not a creature that is merely sitting there.
  Spec.it s "Trumpet Blast gives every attacking creature +2/+0 and leaves a non-attacker alone" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, yours) = trumpetBoard mountain trumpetBlast [piker, piker] [piker]
        after = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackAndCast board
    Spec.assertEqWith s "the spell resolved" (length (GameState.stack after)) 0
    Spec.assertEqWith s "both of alice's creatures are attacking" (List.sort (attackerIds after)) (List.sort ours)
    Spec.assertEqWith s "each attacker is a 4/1" (fmap (`Projection.powerOf` after) ours) (fmap (const (Just 4)) ours)
    Spec.assertEqWith s "and only power moved" (fmap (`Projection.toughnessOf` after) ours) (fmap (const (Just 1)) ours)
    Spec.assertEqWith s "bob's creature never attacked, so it is still a 2/1" (fmap (`Projection.powerOf` after) yours) (fmap (const (Just 2)) yours)
  -- The structural half of CR 611.2c, read off the stored effect rather than
  -- through the projection: what is stored is an ID SET, not the Filter that
  -- found it. Every behavioural leg below follows from this one field, and an
  -- implementation that stored Affected.Matching would fail here first.
  Spec.it s "CR 611.2c the stored effect holds the swept ids, not the filter that swept them" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, _) = trumpetBoard mountain trumpetBlast [piker, piker] [piker]
        after = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackAndCast board
    Spec.assertEqWith s "one stored effect, over exactly the two attackers" (affectedSets after) [Affected.TheseObjects (Set.fromList ours)]
  -- CR 611.2c's own sentence, in the direction it is usually quoted: the set
  -- is fixed when the effect BEGINS, so a creature that becomes attacking
  -- afterwards is not in it.
  --
  -- Hanweir Garrison is the pool's producer for "becomes attacking later":
  -- its CR 508.3a attack trigger creates two 1/1 Humans "that are tapped and
  -- attacking". The trigger is put on the stack as attackers are declared,
  -- alice casts Trumpet Blast on top of it, and the spell therefore resolves
  -- FIRST -- so the tokens are minted, already attacking, after the continuous
  -- effect began. They are attacking, which is exactly what makes this
  -- discriminating: a stored Filter re-evaluated each projection would find
  -- them and pump them to 3/1.
  Spec.it s "CR 611.2c a creature that becomes attacking after the spell resolves is not in the set" $ do
    mountain <- S.printingOf s registry "Mountain"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, _) = trumpetBoard mountain trumpetBlast [garrison] []
        after = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackAndCast board
        tokens = filter (`List.notElem` ours) (attackerIds after)
    Spec.assertEqWith s "the stack is empty: both the spell and the trigger resolved" (length (GameState.stack after)) 0
    Spec.assertEqWith s "the trigger made two tokens" (length tokens) 2
    Spec.assertEqWith s "the Garrison was attacking when the spell resolved, so it is a 4/3" (fmap (`Projection.powerOf` after) ours) (fmap (const (Just 4)) ours)
    Spec.assertEqWith s "the tokens ARE attacking" (length (filter (`List.elem` attackerIds after) tokens)) 2
    Spec.assertEqWith s "and are 1/1 all the same: they were not in the set when it was determined" (fmap (`Projection.powerOf` after) tokens) (fmap (const (Just 1)) tokens)
    Spec.assertEqWith s "the stored set still names only the Garrison" (affectedSets after) [Affected.TheseObjects (Set.fromList ours)]
  -- "After that point, the set won't change" runs in BOTH directions, which is
  -- the half a re-evaluated filter gets wrong even more loudly. CR 511.3
  -- removes every creature from combat as the end of combat step ends, so by
  -- the postcombat main phase nothing is attacking at all -- and the pump is
  -- still there, because it lasts until end of turn (CR 611.2a) and its set
  -- was fixed at resolution.
  Spec.it s "CR 611.2c an attacker that leaves combat keeps the +2/+0" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, _) = trumpetBoard mountain trumpetBlast [piker] []
        postcombat = runToStep Phase.PostcombatMain attackAndCast board
    Spec.assertEqWith s "the leg really reached the postcombat main phase" (GameState.phase postcombat) Phase.PostcombatMain
    Spec.assertEqWith s "CR 511.3: nothing is attacking any more" (attackerIds postcombat) []
    Spec.assertEqWith s "the creature is still a 4/1" (fmap (`Projection.powerOf` postcombat) ours) (fmap (const (Just 4)) ours)
    -- The pumped power is what got through: an unblocked 4/1 takes bob from
    -- 20 to 16, where an unpumped 2/1 would leave him on 18.
    Spec.assertEqWith s "and it dealt 4 combat damage on the way" (S.lifeOf S.bob postcombat) (Just 16)
  -- CR 400.7: "An object that moves from one zone to another becomes a new
  -- object with no memory of, or relation to, its previous existence." A
  -- frozen set is a set of ObjectIds, so the creature that comes back is
  -- simply not in it -- which is the reason CR 611.2c can be implemented as an
  -- id set at all.
  Spec.it s "CR 400.7 a creature that leaves the battlefield and returns is a new object outside the frozen set" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, _) = trumpetBoard mountain trumpetBlast [piker] []
        after = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackAndCast board
    case ours of
      [attacker] -> do
        let bounced = S.runPure S.identityAnswer after (Event.changeZone attacker Zone.Hand)
            (returned, back) = S.addCreature piker S.alice bounced
        Spec.assertEqWith s "it was a 4/1 before it left" (Projection.powerOf attacker after) (Just 4)
        Spec.assertBool s (returned /= attacker) "what came back is a different object"
        Spec.assertEqWith s "and it is a plain 2/1" (Projection.powerOf returned back) (Just 2)
        Spec.assertEqWith s "the stored set still names the incarnation that left" (affectedSets back) [Affected.TheseObjects (Set.singleton attacker)]
      _ -> Spec.assertFailure s "fixture should have exactly one attacker"

-- Aura Thief ({3}{U} 2/2 Creature -- Illusion, "Flying / When this creature
-- dies, you gain control of all enchantments") is the CONTROL-side twin of
-- Trumpet Blast, and the other half of what CR 611.2c names: that rule fixes the
-- affected set of a resolution effect that "modifies the characteristics OR
-- CHANGES THE CONTROLLER of any objects". The layer differs (CR 613.1b's layer 2
-- rather than 613.4c's 7c) and the opcode differs, but the freeze is the same
-- one, and these tests are the proof that GainControl performs it too.
--
-- The trigger is a dies trigger, so the whole card runs the way Doomed
-- Traveler's does in Pawl.TriggerSpec: a Lightning Bolt kills the 2/2, CR
-- 704.5g's state-based action puts it in the graveyard, the CR 603.10a look-back
-- trigger reaches the stack in that same settle, and resolving it is what
-- steals the enchantments. Nothing here hand-builds a continuous effect.
--
-- The printed reminder "(You don't get to move Auras.)" is not a rule this
-- opcode has to implement: nothing in GainControl moves an attachment, and CR
-- 701.3 is the only thing that does.
auraThiefSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auraThiefSpec s registry =
  let -- alice: one Mountain (the Bolt's {R}), an Aura Thief, and a Greed of her
      -- own; bob: a Bad Moon and a Hardened Scales. All four enchantments are
      -- inert on this board -- no black creature, no +1/+1 counter, no activation
      -- -- so the only thing any test here reads off them is who controls them.
      -- S.identityAnswer targets the least Recipient and Recipient.ToCreature
      -- sorts before Recipient.ToPlayer, so the Thief, the only creature on the
      -- board, is the Bolt's target without a bespoke interpreter.
      thiefBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        auraThief <- S.printingOf s registry "Aura Thief"
        greed <- S.printingOf s registry "Greed"
        badMoon <- S.printingOf s registry "Bad Moon"
        hardenedScales <- S.printingOf s registry "Hardened Scales"
        let (thief, g1) = S.addCreature auraThief S.alice (S.landsInPlay mountain 1)
            (hers, g2) = S.addCreature greed S.alice g1
            (moon, g3) = S.addCreature badMoon S.bob g2
            (scales, g4) = S.addCreature hardenedScales S.bob g3
            (withBolt, spell) = S.handOne lightningBolt g4
        pure (withBolt, spell, thief, [hers], [moon, scales])
      -- Cast the Bolt, resolve it, settle (CR 704.5g destroys the damaged 2/2 and
      -- the same settle places its CR 603.10a look-back trigger), then resolve
      -- the trigger.
      boltIt (gs, spell) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
   in Spec.describe s "AuraThief" $ do
        -- CR 109.2 again: "all enchantments" names no zone and no card, so it
        -- means every enchantment PERMANENT on the battlefield -- both
        -- players', and pointedly the Thief's controller's own, which is the
        -- one that would be missing if the sweep had quietly read "you don't
        -- control".
        Spec.it s "Aura Thief whole card: its dies trigger gives its controller control of every enchantment" $ do
          (board, spell, thief, hers, theirs) <- thiefBoard
          let (settled, after) = boltIt (board, spell)
          Spec.assertBool s (not (S.onBattlefield thief settled)) "the Thief died"
          Spec.assertEqWith s "its trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "the trigger resolved" (length (GameState.stack after)) 0
          Spec.assertEqWith s "alice took bob's enchantments" (fmap (`Projection.controllerOf` after) theirs) (fmap (const (Just S.alice)) theirs)
          Spec.assertEqWith s "and still has her own" (fmap (`Projection.controllerOf` after) hers) (fmap (const (Just S.alice)) hers)
        -- The structural half of CR 611.2c, on the control side: what is stored
        -- is the swept id set, not the Filter that found it.
        Spec.it s "CR 611.2c the stored control effect holds the swept ids, not the filter that swept them" $ do
          (board, spell, _, hers, theirs) <- thiefBoard
          let (_, after) = boltIt (board, spell)
          Spec.assertEqWith
            s
            "one stored effect, over all three enchantments"
            (affectedSets after)
            [Affected.TheseObjects (Set.fromList (hers <> theirs))]
        -- "After that point, the set won't change." An enchantment that arrives
        -- after the trigger has resolved is not in the set, so its controller
        -- keeps it -- the control-side twin of the Hanweir Garrison tokens.
        Spec.it s "CR 611.2c an enchantment that enters after the trigger resolves is not stolen" $ do
          (board, spell, _, _, theirs) <- thiefBoard
          greed <- S.printingOf s registry "Greed"
          let (_, after) = boltIt (board, spell)
              (latecomer, later) = S.addCreature greed S.bob after
          Spec.assertEqWith s "the ones that were there are alice's" (fmap (`Projection.controllerOf` later) theirs) (fmap (const (Just S.alice)) theirs)
          Spec.assertEqWith s "the one that arrived afterwards is still bob's" (Projection.controllerOf latecomer later) (Just S.bob)
        -- CR 611.2a: "If no duration is stated, it lasts until the end of the
        -- game." Aura Thief states none, so the grant is Duration.Indefinite and
        -- survives the cleanup step that would end an Act of Treason.
        Spec.it s "CR 611.2a the grant states no duration, so it does not end at cleanup" $ do
          (board, spell, _, _, theirs) <- thiefBoard
          let (_, after) = boltIt (board, spell)
              swept = Expiry.dropAtCleanup after
          Spec.assertEqWith s "alice still controls them after cleanup" (fmap (`Projection.controllerOf` swept) theirs) (fmap (const (Just S.alice)) theirs)
        -- CR 302.6: "A creature's activated ability with the tap symbol ... in
        -- its activation cost can't be activated unless the creature has been
        -- under its controller's control continuously since their most recent
        -- turn began." Gaining control interrupts that continuity, and gaining
        -- control of something you already control does not -- so the sweep has
        -- to ask per object rather than re-Sicking everything it names.
        Spec.it s "CR 302.6 the newly gained enchantments are re-Sicked and the one alice already controlled is not" $ do
          (board, spell, _, hers, theirs) <- thiefBoard
          let (_, after) = boltIt (board, spell)
              sicknessOf oid = fmap Object.sickness (Game.lookupObject oid after)
          Spec.assertEqWith s "bob's, taken from him, start their clock over" (fmap sicknessOf theirs) (fmap (const (Just Sickness.Sick)) theirs)
          Spec.assertEqWith s "alice's own was never interrupted" (fmap sicknessOf hers) (fmap (const (Just (Sickness.Settled S.alice))) hers)
        -- The card is named Aura Thief, so an Aura is the case worth proving,
        -- and Control Magic is the pool's one control-granting Aura. CR 109.5:
        -- "For a static ability, [you] is the current controller of the object
        -- it's on" -- so taking the Aura takes what the Aura grants, WITHOUT
        -- moving the Aura. That is the whole content of the printed reminder
        -- "(You don't get to move Auras.)": Object.attachedTo is untouched here.
        --
        -- The Thief is added before the Piker so it holds the lower ObjectId
        -- and is therefore the Bolt's target under S.identityAnswer, which picks
        -- the least Recipient.
        Spec.it s "CR 109.5 taking bob's Control Magic hands alice back the creature it steals, without moving the Aura" $ do
          mountain <- S.printingOf s registry "Mountain"
          lightningBolt <- S.printingOf s registry "Lightning Bolt"
          auraThief <- S.printingOf s registry "Aura Thief"
          piker <- S.printingOf s registry "Goblin Piker"
          controlMagic <- S.printingOf s registry "Control Magic"
          let (thief, g1) = S.addCreature auraThief S.alice (S.landsInPlay mountain 1)
              (creature, g2) = S.addCreature piker S.alice g1
              (aura, g3) = S.addCreature controlMagic S.bob g2
              stolen = S.attach aura creature g3
              (withBolt, spell) = S.handOne lightningBolt stolen
              (_, after) = boltIt (withBolt, spell)
          Spec.assertBool s (thief < creature) "setup: the Thief is the Bolt's target, holding the lower id"
          Spec.assertEqWith s "setup: bob's Control Magic has taken alice's creature" (Projection.controllerOf creature stolen) (Just S.bob)
          Spec.assertEqWith s "alice now controls the Aura" (Projection.controllerOf aura after) (Just S.alice)
          Spec.assertEqWith s "and so has her creature back" (Projection.controllerOf creature after) (Just S.alice)
          Spec.assertEqWith
            s
            "the Aura never moved: it still enchants the same creature"
            (fmap Object.attachedTo (Game.lookupObject aura after))
            (Just (Just (Recipient.ToCreature creature)))

-- Bane of Progress {4}{G}{G} Creature -- Elemental 2/2: "When this creature
-- enters, destroy all artifacts and enchantments. Put a +1/+1 counter on this
-- creature for each permanent destroyed this way."
--
-- Cast off six Forests from alice's hand and then run the PRIORITY LOOP to a
-- stable board, which is what makes this a gameplay-level test rather than an
-- applyEffect call: the loop resolves the creature spell, its own settle places
-- CR 603.6a's enters trigger, and the next round of passes resolves that. Answers
-- with the id Bane entered the battlefield under (CR 400.7 mints a fresh one on
-- the way in) and the finished board.
castBaneOfProgress :: Printing.Printing -> Printing.Printing -> GameState.GameState -> (Maybe ObjectId.ObjectId, GameState.GameState)
castBaneOfProgress forest bane board =
  let (withSpell, spell) = S.handOne bane (List.foldl' (\gs _ -> snd (S.addCreature forest S.alice gs)) board [1 :: Int .. 6])
      afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
      finished = S.runPure S.identityAnswer afterCast Engine.priorityLoop
   in (namedOnBattlefield "Bane of Progress" finished, finished)

-- The one battlefield permanent whose card carries this name. Bane's printed
-- incarnation is gone by the time the trigger resolves (CR 400.7), so the test
-- cannot hold the id it was cast under.
namedOnBattlefield :: String -> GameState.GameState -> Maybe ObjectId.ObjectId
namedOnBattlefield name gs =
  List.find
    (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack name))
    (Set.toList (GameState.battlefield gs))

-- How many +1/+1 counters (CR 122.6) sit on a permanent, 0 for none.
plusOnePlusOnesOn :: Maybe ObjectId.ObjectId -> GameState.GameState -> Natural
plusOnePlusOnesOn moid gs =
  Maybe.fromMaybe 0 $ do
    oid <- moid
    obj <- Game.lookupObject oid gs
    Map.lookup CounterKind.PlusOnePlusOne (Object.counters obj)

baneOfProgressSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
baneOfProgressSpec s registry = Spec.describe s "BaneOfProgress" $ do
  -- The proving case for #380: a mass effect whose RIDER reads the sweep back.
  -- The board is arranged so that the three readings a wrong implementation
  -- could take all give different numbers, and only one of them is right:
  --
  --   * "everything the filter matched" is 3 (the Myr, the Bonesplitter, Bad
  --     Moon) -- CR 702.12b says the Myr "can't be destroyed", and CR 701.8b
  --     says a permanent that reached a graveyard some other way "hasn't been
  --     'destroyed'", so matching is not being destroyed;
  --   * a FRESH count of artifacts and enchantments after the sweep is 1 (the
  --     Myr, still standing);
  --   * what was actually destroyed this way is 2.
  --
  -- The Piker is neither an artifact nor an enchantment and is the control:
  -- "destroy all artifacts and enchantments" leaves it alone, and Bane itself
  -- is a plain creature and never sweeps itself up.
  Spec.it s "CR 701.8b the rider counts what was destroyed, not what the sweep matched" $ do
    forest <- S.printingOf s registry "Forest"
    bane <- S.printingOf s registry "Bane of Progress"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    badMoon <- S.printingOf s registry "Bad Moon"
    piker <- S.printingOf s registry "Goblin Piker"
    let (myr, g1) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        (equipment, g2) = S.addCreature bonesplitter S.alice g1
        (moon, g3) = S.addCreature badMoon S.bob g2
        (bystander, board) = S.addCreature piker S.bob g3
        (entered, resolved) = castBaneOfProgress forest bane board
    Spec.assertBool s (Maybe.isJust entered) "Bane is on the battlefield"
    Spec.assertEqWith s "stack empty: the spell and its trigger both resolved" (length (GameState.stack resolved)) 0
    Spec.assertBool s (not (S.onBattlefield equipment resolved)) "the artifact died"
    Spec.assertBool s (not (S.onBattlefield moon resolved)) "the enchantment died"
    Spec.assertBool s (S.onBattlefield myr resolved) "CR 702.12b the indestructible artifact creature was swept at and stands"
    Spec.assertBool s (S.onBattlefield bystander resolved) "the creature that is neither was never named"
    Spec.assertEqWith s "two permanents were destroyed this way, so two counters" (plusOnePlusOnesOn entered resolved) 2
    -- CR 122.1a: "A +X/+Y counter on a creature ... adds X to that object's
    -- power and Y to that object's toughness." A printed 2/2 with two of them
    -- is a 4/4, which is what the counters being real means.
    Spec.assertEqWith s "CR 122.1a a printed 2/2 with two +1/+1 counters is a 4/4" (entered >>= \oid -> Projection.powerOf oid resolved) (Just 4)
    Spec.assertEqWith s "and 4 toughness" (entered >>= \oid -> Projection.toughnessOf oid resolved) (Just 4)
  -- The discriminating twin of the test above: the SAME board with the
  -- indestructible permanent removed. The filter now matches two rather than
  -- three, and the count is unchanged at two -- so the two counters above were
  -- the destroyed set and not the matched one.
  Spec.it s "CR 702.12b removing the indestructible permanent leaves the count unchanged" $ do
    forest <- S.printingOf s registry "Forest"
    bane <- S.printingOf s registry "Bane of Progress"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    badMoon <- S.printingOf s registry "Bad Moon"
    let (_, g1) = S.addCreature bonesplitter S.alice (Setup.emptyGame S.bothPlayers)
        (_, board) = S.addCreature badMoon S.bob g1
        (entered, resolved) = castBaneOfProgress forest bane board
    Spec.assertEqWith s "still two destroyed, so still two counters" (plusOnePlusOnesOn entered resolved) 2
  -- CR 701.19a: a regeneration shield "protects the permanent the next time it
  -- would be destroyed this turn ... instead remove all damage marked on it
  -- and its controller taps it". Bane says nothing about regeneration (CR
  -- 701.19c), so the shield applies -- and CR 701.8c calls that replacing the
  -- destruction event, so the permanent it saved was never destroyed and is
  -- not counted.
  Spec.it s "CR 701.19a a regenerated permanent is not destroyed and not counted" $ do
    forest <- S.printingOf s registry "Forest"
    bane <- S.printingOf s registry "Bane of Progress"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    badMoon <- S.printingOf s registry "Bad Moon"
    let (equipment, g1) = S.addCreature bonesplitter S.alice (Setup.emptyGame S.bothPlayers)
        (moon, g2) = S.addCreature badMoon S.bob g1
        (entered, resolved) = castBaneOfProgress forest bane (S.addRegenShield equipment g2)
    Spec.assertBool s (S.onBattlefield equipment resolved) "the shielded artifact stands"
    Spec.assertEqWith s "and CR 701.19a taps it" (fmap Object.tapped (Game.lookupObject equipment resolved)) (Just TapState.Tapped)
    Spec.assertBool s (not (S.onBattlefield moon resolved)) "its unshielded neighbour died"
    Spec.assertEqWith s "one destroyed this way, so one counter" (plusOnePlusOnesOn entered resolved) 1
  -- CR 608.2c: the instructions run in the order written, so with nothing for
  -- the sweep to destroy the rider reads a bound zero rather than an unbound
  -- slot. No counters, and Bane is the 2/2 it was printed as.
  Spec.it s "an empty sweep binds zero, so the rider puts no counters on" $ do
    forest <- S.printingOf s registry "Forest"
    bane <- S.printingOf s registry "Bane of Progress"
    piker <- S.printingOf s registry "Goblin Piker"
    let (bystander, board) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (entered, resolved) = castBaneOfProgress forest bane board
    Spec.assertBool s (S.onBattlefield bystander resolved) "the creature stands: it is neither an artifact nor an enchantment"
    Spec.assertEqWith s "no counters" (plusOnePlusOnesOn entered resolved) 0
    Spec.assertEqWith s "so Bane is the printed 2/2" (entered >>= \oid -> Projection.powerOf oid resolved) (Just 2)

-- Plummet ({1}{G} Instant, "Destroy target creature with flying"), the pool's
-- first card whose Filter names a KEYWORD (Filter.HasKeyword, CR 702.9).
--
-- The negative half of every pair here is the one that carries the claim: a
-- Filter that admitted everything would pass the positive assertions unchanged.
plummetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
plummetSpec s registry = Spec.describe s "Plummet" $ do
  -- CR 702.9b: "A creature with flying can't be blocked except by creatures with
  -- flying and/or reach" -- the ability Bird Maiden prints and Goblin Piker does
  -- not. Nothing else separates the two here, so only the keyword can be what
  -- decides the offer.
  Spec.it s "CR 702.9 HasKeyword Flying admits the flier and rejects the ground creature" $ do
    plummet <- S.printingOf s registry "Plummet"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSlot -> do
        let (flierId, gs1) = S.addCreature birdMaiden S.bob (Setup.emptyGame S.bothPlayers)
            (groundId, gs) = S.addCreature piker S.bob gs1
            legal = Target.legalRecipients Nothing S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature flierId) legal) "the flier is a legal target"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature groundId) legal)) "the creature without flying is not"
  -- CR 613.1f: layer 6 is where abilities are added, so the read has to go
  -- through the PROJECTION rather than the printed card. Spontaneous Flight
  -- ({2}{W}, "+2/+2 and a flying counter") is the pool's grant, and the Piker it
  -- lands on printed no flying at all.
  Spec.it s "CR 613.1f a Piker that GAINS flying becomes a legal target" $ do
    plummet <- S.printingOf s registry "Plummet"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    spontaneousFlight <- S.printingOf s registry "Spontaneous Flight"
    case S.spellTargetSlot plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSlot -> do
        let (groundId, before) = S.addCreature piker S.alice (S.landsInPlay plains 3)
            (withSpell, spellId) = S.handOne spontaneousFlight before
            cast = snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        Spec.assertBool s (not (Set.member (Recipient.ToCreature groundId) (Target.legalRecipients Nothing S.noSource theSlot before))) "no flying, no offer"
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying groundId after) "the grant landed"
        Spec.assertBool s (Set.member (Recipient.ToCreature groundId) (Target.legalRecipients Nothing S.noSource theSlot after)) "and the grant makes it a legal target"
  -- The other direction, and the one that proves the read is not of the printed
  -- card: Humility (CR 613.1f, "all creatures lose all abilities") takes the
  -- flying off a creature that PRINTS it, and the offer goes with it.
  Spec.it s "CR 613.1f Humility strips the printed flying, and the offer goes with it" $ do
    plummet <- S.printingOf s registry "Plummet"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    humility <- S.printingOf s registry "Humility"
    case S.spellTargetSlot plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSlot -> do
        let (flierId, before) = S.addCreature birdMaiden S.bob (Setup.emptyGame S.bothPlayers)
            after = S.withHumility humility before
        Spec.assertBool s (Set.member (Recipient.ToCreature flierId) (Target.legalRecipients Nothing S.noSource theSlot before)) "legal while it flies"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying flierId after)) "Humility took the flying"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature flierId) (Target.legalRecipients Nothing S.noSource theSlot after))) "so it is no longer a legal target"
  -- CR 701.8: the whole card, cast and resolved. The Piker beside the flier is
  -- the control: it survives because Plummet could never have been aimed at it.
  Spec.it s "CR 701.8 Plummet destroys the flier it targets, and leaves the ground creature standing" $ do
    plummet <- S.printingOf s registry "Plummet"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    forest <- S.printingOf s registry "Forest"
    let (flierId, g1) = S.addCreature birdMaiden S.bob (S.landsInPlay forest 2)
        (groundId, g2) = S.addCreature piker S.bob g1
        (gs, spellId) = S.handOne plummet g2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertBool s (not (S.onBattlefield flierId after)) "the flier was destroyed"
    Spec.assertBool s (S.onBattlefield groundId after) "the creature without flying was never a candidate"
    Spec.assertEqWith s "and the flier is in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1

-- Announces X=2 and takes the identity fallback everywhere else -- which answers
-- CR 601.2b's Phyrexian question with the FIRST offer, the mana route, so the
-- {G/P} is paid with a Forest rather than with life.
answerXTwo :: Prompt.Prompt r -> r
answerXTwo p = case p of
  Prompt.ChooseX {} -> 2
  _ -> S.identityAnswer p

-- The damage marked on a permanent (CR 120.3e), or Nothing if it is gone.
markedOn :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural
markedOn oid gs = fmap Object.damage (Game.lookupObject oid gs)

-- Corrosive Gale ({X}{G/P} Sorcery, "Corrosive Gale deals X damage to each
-- creature with flying") -- the pool's first Effect.DealDamage over a SET rather
-- than a slot, and the first producer of ObjectRef.EachMatching at all whose
-- filter names a keyword.
--
-- One board throughout: bob's Bird Maiden (1/2, prints flying), alice's
-- Narcomoeba (1/1, prints flying) and bob's Goblin Piker (2/1, prints none),
-- beside three of alice's Forests. The fliers are split between the two players
-- on purpose: "each creature with flying" is not "each creature your opponents
-- control", and alice burning her own Narcomoeba is what says so. The Piker is
-- the other half of the claim: CR 109.2 hands an EachMatching the WHOLE
-- battlefield, so a filter missing its HasKeyword half would burn it too.
--
-- The Forests are not a third control and could not be: CR 120.1a takes a land
-- out of the batch at Damage.damageRecipient whatever the filter said. The
-- HasCardType half of the filter is pinned by CardsSpec instead.
corrosiveGaleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
corrosiveGaleSpec s registry = Spec.describe s "CorrosiveGale" $ do
  Spec.it s "CR 109.2 Corrosive Gale deals X to each creature with flying, and none to the one without" $ do
    gale <- S.printingOf s registry "Corrosive Gale"
    forest <- S.printingOf s registry "Forest"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    narcomoeba <- S.printingOf s registry "Narcomoeba"
    piker <- S.printingOf s registry "Goblin Piker"
    let (maidenId, g1) = S.addCreature birdMaiden S.bob (S.landsInPlay forest 3)
        (moebaId, g2) = S.addCreature narcomoeba S.alice g1
        (pikerId, g3) = S.addCreature piker S.bob g2
        (gs, spellId) = S.handOne gale g3
        cast = snd (Engine.runGamePure answerXTwo gs (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure answerXTwo cast Stack.resolveTop)
        after = S.settleSba resolved
    Spec.assertEqWith s "three Forests paid {2}{G}" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "CR 120.3e: 2 marked on the Bird Maiden" (markedOn maidenId resolved) (Just 2)
    Spec.assertEqWith s "CR 120.3e: 2 marked on the Narcomoeba, an opponent's flier is no different" (markedOn moebaId resolved) (Just 2)
    Spec.assertEqWith s "and nothing at all on the Goblin Piker" (markedOn pikerId resolved) (Just 0)
    Spec.assertBool s (not (S.onBattlefield maidenId after)) "CR 704.5g buried the 1/2"
    Spec.assertBool s (not (S.onBattlefield moebaId after)) "and the 1/1"
    Spec.assertBool s (S.onBattlefield pikerId after) "the creature without flying was never in the set"
  -- CR 613.1f: layer 6 is where abilities are removed, so the sweep reads the
  -- PROJECTION and not the printed card. Humility ("all creatures lose all
  -- abilities and have base power and toughness 1/1") takes the flying off the
  -- Bird Maiden that prints it, and the set the Gale sweeps goes empty -- the
  -- cast and the payment being unaffected is what separates "found nobody" from
  -- "never happened".
  Spec.it s "CR 613.1f Humility strips the printed flying, and the Gale finds nobody" $ do
    gale <- S.printingOf s registry "Corrosive Gale"
    forest <- S.printingOf s registry "Forest"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    humility <- S.printingOf s registry "Humility"
    let (maidenId, g1) = S.addCreature birdMaiden S.bob (S.landsInPlay forest 3)
        (gs, spellId) = S.handOne gale (S.withHumility humility g1)
        cast = snd (Engine.runGamePure answerXTwo gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure answerXTwo cast Stack.resolveTop))
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying maidenId after)) "Humility took the flying"
    Spec.assertEqWith s "three Forests paid {2}{G} all the same" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "no damage marked on the grounded Bird Maiden" (markedOn maidenId after) (Just 0)
    Spec.assertBool s (S.onBattlefield maidenId after) "so it survives"

-- CR 701.16a: "'Investigate' means 'Create a Clue token.' See rule 111.10f."
-- The keyword action is pure shorthand for a Create, which is why Thraben
-- Inspector needs no opcode of its own: the card data spells CR 111.10f's
-- predefined Clue out literally ("a colorless Clue artifact token with '{2},
-- Sacrifice this token: Draw a card.'"), which is the "given, not derived" side
-- of Effect.Create's own doc comment rather than a lookup of the predefined
-- definition.
--
-- Gameplay level throughout: the Inspector is cast from hand for {W}, its
-- CR 603.6a enters trigger is placed by the settle and resolved off the stack,
-- and the Clue's own ability is then activated and resolved.
--
-- Alice keeps THREE Plains so that after the {W} exactly two stay untapped --
-- the Clue's {2} -- and a Goblin Piker sits in her library so the draw has
-- something to find (CR 104.3c would otherwise decide the game first) and so
-- the drawn card is identifiable by name rather than by a count that the
-- Inspector's own 1/2 could coincide with.
investigateBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
investigateBoard s registry = do
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  inspector <- S.printingOf s registry "Thraben Inspector"
  let (gs0, spellId) = S.handOne inspector (S.landsInPlay plains 3)
      (_, gs1) = S.addLibraryCard piker S.alice gs0
      cast = S.runPure S.identityAnswer gs1 (S.cast S.alice spellId)
      -- The settle is what places the CR 603.6a trigger; the second resolveTop
      -- is the trigger itself.
      entered = S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
  pure (S.runPure S.identityAnswer entered (Stack.resolveTop >> Engine.settleForPriority))

-- The one Clue on the board, by the fact that it is the only token there.
clueOf :: GameState.GameState -> Maybe ObjectId.ObjectId
clueOf gs = case S.tokensOf gs of
  [oid] -> Just oid
  _ -> Nothing

-- The untapped lands on the board -- on this board, alice's Plains and nothing
-- else. Used to build the one-mana board from the two-mana board by tapping one
-- more land and changing nothing else.
untappedPlains :: GameState.GameState -> [ObjectId.ObjectId]
untappedPlains gs =
  [ oid
  | oid <- Set.toList (GameState.battlefield gs),
    Set.member CardType.Land (Projection.cardTypesOf oid gs),
    fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
  ]

investigateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
investigateSpec s registry = Spec.describe s "Investigate" $ do
  Spec.it s "CR 701.16a Thraben Inspector's ETB creates one colorless Clue artifact token" $ do
    after <- investigateBoard s registry
    -- Three Plains, the Inspector and exactly one more permanent. Stated as a
    -- total rather than as "one token" so that a Create minting two fails here
    -- as well as at clueOf below.
    Spec.assertEqWith s "five permanents: three Plains, the Inspector and one more" (Set.size (GameState.battlefield after)) 5
    Spec.assertEqWith s "the Inspector resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Thraben Inspector") S.alice after) 1
    case clueOf after of
      Nothing -> Spec.assertFailure s "expected exactly one token on the battlefield"
      Just clueId -> do
        -- CR 111.4: investigate does not name its token, so the name is the
        -- subtype plus the word "Token".
        Spec.assertEqWith s "the token is named Clue Token" (fmap Face.name (Game.faceOf clueId after)) (Just . CardName.MkCardName $ Text.pack "Clue Token")
        Spec.assertEqWith s "CR 111.10f: an artifact" (Projection.cardTypesOf clueId after) (Set.singleton CardType.Artifact)
        Spec.assertEqWith s "CR 111.10f: with subtype Clue" (Projection.subtypesOf clueId after) (Set.singleton Subtype.Clue)
        -- CR 202.2b ("objects with no colored mana symbols in their mana costs
        -- are colorless") plus CR 202.2e (a color indicator is the other way an
        -- object gets a color): the token face carries neither, which is how
        -- the card data spells "colorless". The falsifier for the clause being
        -- asserted rather than assumed -- a token face given colorIndicator
        -- White fails here and nowhere else.
        Spec.assertEqWith s "CR 111.10f: and colorless" (Projection.colorsOf clueId after) Set.empty
        -- CR 111.2: the player who creates a token controls it.
        Spec.assertEqWith s "CR 111.2: alice created it, so alice controls it" (Projection.controllerOf clueId after) (Just S.alice)
  Spec.it s "CR 111.10f the Clue's {2} is real: one untapped Plains cannot pay it" $ do
    -- The negative board differs from the positive one ONLY in how many lands
    -- are untapped: same permanents, same phase, same empty stack. Without
    -- that, "not activatable" would pass for any of the reasons a cost check
    -- can fail.
    twoMana <- investigateBoard s registry
    case (clueOf twoMana, untappedPlains twoMana) of
      (Just clueId, first : _) -> do
        let oneMana = S.tapObject first twoMana
        Spec.assertEqWith s "two Plains untapped after the {W}" (length (untappedPlains twoMana)) 2
        Spec.assertEqWith s "one on the negative board" (length (untappedPlains oneMana)) 1
        case Activate.abilitiesFor clueId twoMana of
          [ability] -> do
            Spec.assertBool s (Activate.activatable S.alice clueId ability twoMana) "two mana pays {2}"
            Spec.assertBool s (not (Activate.activatable S.alice clueId ability oneMana)) "one does not"
          other -> Spec.assertFailure s ("expected exactly one activated ability on the Clue, got " <> show (length other))
      _ -> Spec.assertFailure s "expected one token and at least one untapped Plains"
  Spec.it s "CR 111.10f cracking the Clue draws a card, and the token ceases to exist (CR 111.7)" $ do
    before <- investigateBoard s registry
    case clueOf before of
      Nothing -> Spec.assertFailure s "expected exactly one token on the battlefield"
      Just clueId -> case Activate.abilitiesFor clueId before of
        [ability] -> do
          let activated = S.runPure S.identityAnswer before (Activate.activateAbility S.alice clueId ability)
              after = S.runPure S.identityAnswer activated (Stack.resolveTop >> Engine.settleForPriority)
          Spec.assertEqWith s "alice's hand was empty before" (S.handSize S.alice before) 0
          -- Named, not counted -- and NOT through S.countByName, which spans
          -- hand and library together and so cannot tell "drawn" from "still
          -- in the library". The hand's own member is what says the Piker
          -- moved, and the emptied library is the other half of it.
          Spec.assertEqWith s "alice drew the Goblin Piker" (fmap (`S.soleFaceName` after) (Game.zoneMembers Zone.Hand S.alice after)) [CardName.MkCardName $ Text.pack "Goblin Piker"]
          Spec.assertEqWith s "and her library is empty" (length (Game.zoneMembers Zone.Library S.alice after)) 0
          Spec.assertBool s (not (S.onBattlefield clueId after)) "the Clue was sacrificed as a cost"
          -- CR 111.7: a token in any zone other than the battlefield ceases to
          -- exist, so the honest assertion is that it exists nowhere -- NOT
          -- that it reached the graveyard.
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject clueId after)) "CR 111.7: and no longer exists in any zone"
          Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
          Spec.assertEqWith s "three Plains are now tapped: the {W} and the {2}" (S.tappedCount S.alice after) 3
        other -> Spec.assertFailure s ("expected exactly one activated ability on the Clue, got " <> show (length other))

-- CR 701.60, proved by Person of Interest {3}{R} Creature -- Human Rogue 2/2,
-- "When this creature enters, suspect it. Create a 2/2 white and blue Detective
-- creature token."
--
-- The Detective is what makes every case below a PAIR on one board: it is
-- alice's (or bob's) other creature, created by the same resolution, and the only
-- thing that differs between the two is the designation. A fixture that read the
-- card wrong, or a menace grant aimed at the wrong object, fails the second half
-- of each assertion rather than passing for a board-shaped reason.
--
-- CR 701.60c has two halves in two different subsystems, so both are asserted at
-- gameplay level: menace goes through Pawl.Engine.Projection's layer-6 grant and
-- is read by a block declaration, "can't block" goes through
-- Pawl.Engine.CombatRestriction and is read by another.
personOfInterestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
personOfInterestSpec s registry = Spec.describe s "PersonOfInterest" $ do
  let -- The board after the CR 603.6a enters trigger has resolved: `pid` gets the
      -- Person of Interest and its Detective, and the Pikers fill out whichever
      -- side the case needs. S.entersWithTrigger rather than a cast, because
      -- S.combatBoardOf starts in the declare attackers step, where no spell can
      -- be cast.
      board mine theirs pid = do
        poi <- S.printingOf s registry "Person of Interest"
        piker <- S.printingOf s registry "Goblin Piker"
        let (gs0, ours, yours) = S.combatBoardOf (replicate mine piker) (replicate theirs piker)
            (poiId, gs1) = S.entersWithTrigger poi pid gs0
            settled = S.runPure S.identityAnswer gs1 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
        pure (settled, poiId, ours, yours)
  Spec.it s "CR 701.60a the enters trigger suspects the Person and not the Detective it makes" $ do
    (gs, poiId, _, _) <- board 0 0 S.alice
    case S.tokensOf gs of
      [tokenId] -> do
        Spec.assertEqWith s "the Person is suspected, the Detective is not" (suspectedOf poiId gs, suspectedOf tokenId gs) (Just True, Just False)
        Spec.assertEqWith s "the token is a 2/2" (S.powerToughnessOf tokenId gs) (Just (2, 2))
        Spec.assertEqWith s "a Detective creature" (Projection.cardTypesOf tokenId gs, Projection.subtypesOf tokenId gs) (Set.singleton CardType.Creature, Set.singleton Subtype.Detective)
        Spec.assertEqWith s "white and blue" (Projection.colorsOf tokenId gs) (Set.fromList [Color.White, Color.Blue])
        Spec.assertEqWith s "CR 111.2: alice created it, so alice controls it" (Projection.controllerOf tokenId gs) (Just S.alice)
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
  -- CR 701.60a's "until it leaves the battlefield", asserted on
  -- Object.newIncarnation directly for the reason Pawl.TriggerSpec's renown case
  -- gives: nothing writes the field on an entry, and Pawl.SetupSpec's CR 400.7
  -- case is blind to a field the forgetting never touches.
  Spec.it s "CR 701.60a the designation does not survive CR 400.7" $ do
    (gs, poiId, _, _) <- board 0 0 S.alice
    case Game.lookupObject poiId gs of
      Nothing -> Spec.assertFailure s "expected to find the Person"
      Just obj -> Spec.assertEqWith s "this incarnation is suspected, the next one is not" (isSuspected obj, isSuspected (Object.newIncarnation obj)) (True, False)
  Spec.it s "CR 701.60c a suspected creature has menace, so one blocker cannot block it" $ do
    -- bob's two Pikers are the falsifier for reading rule 701.60c as "can't be
    -- blocked": the very creature that cannot block the Person alone can block it
    -- alongside the other, and the block survives a real declare blockers step.
    (entered, poiId, _, blockers) <- board 0 2 S.alice
    let gs = S.runPure S.aggressiveAnswer entered (Combat.declareAttackers S.alice)
    case (S.tokensOf gs, blockers) of
      ([tokenId], [first, second]) -> do
        Spec.assertEqWith s "the Person has menace and the Detective does not" (Projection.hasKeyword Keyword.Menace poiId gs, Projection.hasKeyword Keyword.Menace tokenId gs) (True, False)
        Spec.assertEqWith s "the Person is attacking" (S.attackerDeclarationsOf gs) [poiId]
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton poiId)) gs)) "one blocker is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton poiId), (second, Set.singleton poiId)]) gs) "two are legal"
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
        Spec.assertEqWith s "and both block" (Combat.blockersOf poiId after) (Set.fromList [first, second])
      other -> Spec.assertFailure s ("expected one token and two blockers, got " <> show other)
  Spec.it s "CR 701.60c a suspected creature can't block, where the Detective beside it can" $ do
    -- The designation on the DEFENDING side, so rule 701.60c's second half is the
    -- only thing separating the two creatures bob could block with. Both are his,
    -- both entered this turn, and only one is suspected.
    (entered, poiId, attackers, _) <- board 1 0 S.bob
    let gs = S.runPure S.aggressiveAnswer entered (Combat.declareAttackers S.alice)
    case (S.tokensOf gs, attackers) of
      ([tokenId], [attackerId]) -> do
        Spec.assertEqWith s "alice's Piker is attacking" (S.attackerDeclarationsOf gs) [attackerId]
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton poiId (Set.singleton attackerId)) gs)) "the Person cannot block"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton tokenId (Set.singleton attackerId)) gs) "the Detective can"
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
        Spec.assertEqWith s "so only the Detective blocks" (Combat.blockersOf attackerId after) (Set.singleton tokenId)
      other -> Spec.assertFailure s ("expected one token and one attacker, got " <> show other)

-- CR 701.60a's second ending, "until a spell or ability causes it to no longer be
-- suspected", proved by Eliminate the Impossible {1}{U} Instant, "Investigate.
-- Creatures your opponents control get -2/-0 until end of turn. If any of them
-- are suspected, they're no longer suspected."
--
-- One board carries the whole case: bob's Person of Interest is suspected and his
-- Detective is not, so the -2/-0 lands on both while only one designation ends.
-- The two things rule 701.60c hangs off the designation are then asserted gone at
-- gameplay level, each through its own subsystem -- menace through the layer-6
-- grant, "can't block" through the combat restriction.
eliminateTheImpossibleSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
eliminateTheImpossibleSpec s registry = Spec.describe s "EliminateTheImpossible" $ do
  Spec.it s "CR 701.60a a spell ends the designation, and CR 701.60c's menace and can't-block end with it" $ do
    poi <- S.printingOf s registry "Person of Interest"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    eliminate <- S.printingOf s registry "Eliminate the Impossible"
    let (gs0, attackers, _) = S.combatBoardOf [piker] []
        (poiId, gs1) = S.entersWithTrigger poi S.bob gs0
        entered = S.runPure S.identityAnswer gs1 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
        -- S.addCreature places any permanent; these two are the {1}{U}.
        (_, gs2) = S.addCreature island S.alice entered
        (_, gs3) = S.addCreature island S.alice gs2
        (spellId, gs4) = S.addHandCard eliminate S.alice gs3
        declared = S.runPure S.aggressiveAnswer gs4 (Combat.declareAttackers S.alice)
    case (S.tokensOf declared, attackers) of
      ([detectiveId], [attackerId]) -> do
        let after = S.runPure S.identityAnswer declared (S.cast S.alice spellId >> Stack.resolveTop >> Engine.settleForPriority)
        -- The before half, on the very board the spell is cast from: without it
        -- every "after" assertion could be passing because the fixture never
        -- suspected anything.
        Spec.assertEqWith s "before: the Person is suspected and the Detective is not" (suspectedOf poiId declared, suspectedOf detectiveId declared) (Just True, Just False)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton poiId (Set.singleton attackerId)) declared)) "before: the Person cannot block"
        Spec.assertEqWith s "after: neither is suspected" (suspectedOf poiId after, suspectedOf detectiveId after) (Just False, Just False)
        Spec.assertEqWith s "CR 701.60c: and the menace it had is gone" (Projection.hasKeyword Keyword.Menace poiId after, Projection.hasKeyword Keyword.Menace detectiveId after) (False, False)
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton poiId (Set.singleton attackerId)) after) "CR 701.60c: so the Person can block"
        let blocked = S.runPure S.aggressiveAnswer after Combat.declareBlockers
        Spec.assertBool s (Set.member poiId (Combat.blockersOf attackerId blocked)) "and it does block"
        -- The two clauses either side of the ending, so a card file that dropped
        -- one fails here: the -2/-0 reaches both of bob's creatures, and the
        -- Clue is alice's.
        Spec.assertEqWith s "both of bob's creatures took -2/-0" (S.powerToughnessOf poiId after, S.powerToughnessOf detectiveId after) (Just (0, 2), Just (0, 2))
        Spec.assertEqWith s "and alice's own attacker did not, so the sweep is opponents-only" (S.powerToughnessOf attackerId after) (Just (2, 1))
        Spec.assertEqWith s "and alice investigated" (fmap (`S.soleFaceName` after) (filter (/= detectiveId) (S.tokensOf after))) [CardName.MkCardName $ Text.pack "Clue Token"]
      other -> Spec.assertFailure s ("expected one token and one attacker, got " <> show other)

-- CR 701.60b read as a number, proved by Repeat Offender {1}{B} Creature -- Human
-- Assassin 2/1, "{2}{B}: If this creature is suspected, put a +1/+1 counter on
-- it. Otherwise, suspect it."
--
-- The card is its own pair: the same activation on the same board takes the other
-- branch once the designation is there, so the two clause conditions are the only
-- thing that can separate the two outcomes.
repeatOffenderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
repeatOffenderSpec s registry = Spec.describe s "RepeatOffender" $ do
  Spec.it s "CR 701.60b the first activation suspects and the second adds a counter" $ do
    swamp <- S.printingOf s registry "Swamp"
    offender <- S.printingOf s registry "Repeat Offender"
    let (offenderId, board) = S.addCreature offender S.alice (S.landsInPlay swamp 6)
        activate gs = case Activate.abilitiesFor offenderId gs of
          [ability] -> Right (S.runPure S.identityAnswer gs (Activate.activateAbility S.alice offenderId ability >> Stack.resolveTop >> Engine.settleForPriority))
          other -> Left (length other)
        state gs = (suspectedOf offenderId gs, S.counterOf CounterKind.PlusOnePlusOne offenderId gs, S.powerToughnessOf offenderId gs)
    case activate board of
      Left n -> Spec.assertFailure s ("expected exactly one activated ability, got " <> show n)
      Right once -> do
        Spec.assertEqWith s "it starts unsuspected, with no counter" (state board) (Just False, 0, Just (2, 1))
        Spec.assertEqWith s "the first activation suspects it and places NOTHING" (state once) (Just True, 0, Just (2, 1))
        case activate once of
          Left n -> Spec.assertFailure s ("expected exactly one activated ability, got " <> show n)
          Right twice -> Spec.assertEqWith s "the second finds it suspected and places a counter" (state twice) (Just True, 1, Just (3, 2))

-- CR 701.60b read as a CRITERION, proved by Rune-Brand Juggler {B}{R} Creature --
-- Human Shaman 2/2 (data/cards/rune-brand-juggler.json): "When this creature
-- enters, suspect up to one target creature you control. {3}{B}{R}, Sacrifice a
-- suspected creature: Target creature gets -5/-5 until end of turn."
--
-- The Filter atom rides a CR 701.21a sacrifice cost, so the designation decides
-- both whether the ability can be activated at all and which permanent pays for
-- it. Boggart Brute is on the board in both cases below, and it is what separates
-- the designation from what CR 701.60c hangs off it: its menace is PRINTED and it
-- is never suspected, so a criterion reading the menace grant rather than the
-- designation would offer it as fodder.
runeBrandJugglerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
runeBrandJugglerSpec s registry = Spec.describe s "RuneBrandJuggler" $ do
  Spec.it s "CR 701.60b the cost takes the suspected creature, and the menace one is not a candidate" $ do
    (jugglerId, pikerId, bruteId, wallId, gs0) <- jugglerBoard s registry
    let entered = S.runPure (takingTargets 1 [pikerId]) gs0 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
    -- The board the activation happens on: exactly one of alice's three creatures
    -- is suspected, so every assertion below has a same-board counterexample.
    Spec.assertEqWith s "the Piker is suspected, the Brute and the Juggler are not" (fmap (`suspectedOf` entered) [pikerId, bruteId, jugglerId]) [Just True, Just False, Just False]
    Spec.assertEqWith s "and the Brute's menace is printed rather than the designation's" (Projection.hasKeyword Keyword.Menace bruteId entered, suspectedOf bruteId entered) (True, Just False)
    case Activate.abilitiesFor jugglerId entered of
      [ability] -> do
        Spec.assertBool s (Activate.activatable S.alice jugglerId ability entered) "a suspected creature to sacrifice makes it activatable"
        let after = S.runPure (jugglerAnswer wallId bruteId) entered (Activate.activateAbility S.alice jugglerId ability >> Stack.resolveTop >> Engine.settleForPriority)
        -- The interpreter asks for the BRUTE whenever a sacrifice is on offer, and
        -- CR 701.21a's prompt is raised only above one candidate -- so a criterion
        -- that dropped the designation would sacrifice the Brute here, and a
        -- criterion that read menace would sacrifice it instead of the Piker.
        Spec.assertBool s (not (S.onBattlefield pikerId after)) "the suspected creature paid the cost"
        Spec.assertEqWith s "and reached alice's graveyard (CR 701.21a)" (fmap (`S.soleFaceName` after) (Game.zoneMembers Zone.Graveyard S.alice after)) [CardName.MkCardName $ Text.pack "Goblin Piker"]
        Spec.assertBool s (S.onBattlefield bruteId after) "the creature with menace and no designation did not"
        Spec.assertBool s (S.onBattlefield jugglerId after) "and neither did the Juggler"
        Spec.assertEqWith s "before: the Wall is a 0/8" (S.powerToughnessOf wallId entered) (Just (0, 8))
        Spec.assertEqWith s "after: the ability resolved for -5/-5" (S.powerToughnessOf wallId after) (Just (-5, 3))
      other -> Spec.assertFailure s ("expected exactly one activated ability on the Juggler, got " <> show (length other))
  -- The same board, the same lands and the same three creatures; the one
  -- difference is CR 115.6's announcement, which leaves nothing suspected.
  Spec.it s "CR 701.60b with nothing suspected the cost cannot be paid, though the creatures and the mana are the same" $ do
    (jugglerId, pikerId, bruteId, _, gs0) <- jugglerBoard s registry
    let entered = S.runPure decliningTargets gs0 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith s "the ETB was declined, so no creature is suspected" (fmap (`suspectedOf` entered) [pikerId, bruteId, jugglerId]) [Just False, Just False, Just False]
    case Activate.abilitiesFor jugglerId entered of
      [ability] -> do
        -- NOT a mana or a timing failure: the case above answers True for the same
        -- ability, on the same five lands, with the same three creatures and the
        -- same target available -- the designation is the only thing that moved.
        Spec.assertBool s (not (Activate.activatable S.alice jugglerId ability entered)) "three unsuspected creatures are not candidates"
      other -> Spec.assertFailure s ("expected exactly one activated ability on the Juggler, got " <> show (length other))

-- Rune-Brand Juggler entering under alice, who also controls the Goblin Piker its
-- trigger will suspect and a Boggart Brute it will not, plus exactly the {3}{B}{R}
-- the activated ability costs (four Swamps and a Mountain). bob's Wall of Stone is
-- the ability's target, a 0/8 so that -5/-5 is legible without CR 704.5f taking it
-- away.
jugglerBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
jugglerBoard s registry = do
  swamp <- S.printingOf s registry "Swamp"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  brute <- S.printingOf s registry "Boggart Brute"
  wall <- S.printingOf s registry "Wall of Stone"
  juggler <- S.printingOf s registry "Rune-Brand Juggler"
  let (_, g1) = S.addCreature mountain S.alice (S.landsInPlay swamp 4)
      (pikerId, g2) = S.addCreature piker S.alice g1
      (bruteId, g3) = S.addCreature brute S.alice g2
      (wallId, g4) = S.addCreature wall S.bob g3
      (jugglerId, g5) = S.entersWithTrigger juggler S.alice g4
      gs =
        g5
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
  pure (jugglerId, pikerId, bruteId, wallId, gs)

-- CR 701.60b's designation, read off the object.
suspectedOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Bool
suspectedOf oid gs = fmap isSuspected (Game.lookupObject oid gs)

-- CR 701.60b asked of one object, which is Set membership rather than a field.
isSuspected :: Object.Object -> Bool
isSuspected = Set.member Designation.Suspected . Object.designations

-- Aims the ability at `victim` and asks for `fodder` whenever CR 701.21a offers a
-- sacrifice choice. The fodder is deliberately the permanent the criterion must
-- NOT offer: at one candidate Prompt.ChooseSacrifices is elided, so this half of
-- the interpreter can only ever fire on a criterion that is too wide.
jugglerAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
jugglerAnswer victim fodder p = case p of
  Prompt.ChooseSacrifices {} -> sacrifices fodder p
  _ -> takingTargets 1 [victim] p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  targetSpec s registry
  plummetSpec s registry
  corrosiveGaleSpec s registry
  investigateSpec s registry
  personOfInterestSpec s registry
  eliminateTheImpossibleSpec s registry
  repeatOffenderSpec s registry
  runeBrandJugglerSpec s registry
  resolveSpec s registry
  fizzleSpec s registry
  indestructibleSpec s registry
  zoneChangeSpec s registry
  libraryPositionSpec s registry
  aetherspoutsSpec s registry
  drawCardSpec s registry
  loseLifeSpec s registry
  exchangeLifeTotalsSpec s registry
  setLifeTotalSpec s registry
  redistributeLifeTotalsSpec s registry
  greatestSpec s registry
  soulsMajestySpec s registry
  counterSpec s registry
  manaLeakSpec s registry
  whipstitchedZombieSpec s registry
  circlingVulturesSpec s registry
  fortressKinGuardSpec s registry
  magicalHackTimingSpec s registry
  artificialEvolutionSpec s registry
  stifleSpec s registry
  countersSpec s registry
  sauroformHybridSpec s registry
  nessianAspSpec s registry
  untapSpec s registry
  gainControlSpec s registry
  gainPlayerCountersSpec s registry
  proliferateSpec s registry
  scrySpec s registry
  scryPromptSpec s registry
  surveilSpec s registry
  surveilPromptSpec s registry
  fatesealSpec s registry
  exploreSpec s registry
  explorePromptSpec s registry
  playerSacrificesSpec s registry
  createEmblemSpec s registry
  becomeMonarchSpec s registry
  targetedMonarchSpec s registry
  exileUntilMonarchSpec s registry
  actOfTreasonSpec s registry
  optionalEffectSpec s registry
  destroyAllSpec s registry
  returnAllSpec s registry
  riseOfTheDarkRealmsSpec s registry
  portOfKarfellSpec s registry
  blossomingTortoiseSpec s registry
  exhumeSpec s registry
  bloodForBonesSpec s registry
  skullwinderSpec s registry
  trumpetBlastSpec s registry
  auraThiefSpec s registry
  baneOfProgressSpec s registry
  upToOneTargetSpec s registry
  multiTargetSpec s registry
  supportSpec s registry
  bolsterSpec s registry
  amassSpec s registry
  blightSpec s registry
  countOnLuckSpec s registry
  actOnImpulseSpec s registry
  soulfireEruptionSpec s registry

-- CR 601.2c's announcement, answered with a stated number for every variable
-- slot -- where S.identityAnswer announces as many as the board allows.
announcingCount :: Natural -> Prompt.Prompt r -> r
announcingCount n p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const n) offers
  _ -> S.identityAnswer p

-- Announces `n` targets per slot and aims them at `wanted`, in that order of
-- preference. S.identityAnswer would take the least Recipients instead, which on
-- these boards is not what the assertions are about.
takingTargets :: Natural -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
takingTargets n wanted p = case p of
  Prompt.AnnounceTargets {} -> announcingCount n p
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (\r -> maybe False (\oid -> elem oid wanted) (Recipient.objectOf r)) sets
  _ -> S.identityAnswer p

-- The +1/+1 counters on one permanent.
plusCountersOn :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural
plusCountersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)

-- CR 601.2c's count above one, read at resolution.
--
-- Hearts on Fire {1}{R} Instant (data/cards/hearts-on-fire.json): "One or two
-- target creatures each get +2/+1 until end of turn." A range whose minimum is
-- neither zero nor its maximum, so it exercises both ends -- castability gates on
-- the minimum, the announcement chooses between one and two, and CR 608.2b's
-- per-recipient legality shows on the survivor when the other target goes.
--
-- Agent Bishop, Man in Black {2}{W} 1/2 (data/cards/agent-bishop-man-in-black.json):
-- "At the beginning of combat on your turn, put a +1/+1 counter on each of up to
-- two target creatures." The same count on a TRIGGERED ability, where
-- Resolve.resolveModes rather than Resolve.targetsAllIllegal asks CR 608.2b's
-- question.
multiTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
multiTargetSpec s registry = Spec.describe s "MultiTarget" $ do
  -- Three creatures with three different power/toughness boxes, so which two were
  -- pumped is legible; two targets out of three is what makes the count a choice
  -- rather than a sweep.
  Spec.it s "CR 601.2c Hearts on Fire pumps the two creatures it named, and only those" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- heartsBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker is +2/+1" (S.powerToughnessOf pikerId after) (Just (4, 2))
    Spec.assertEqWith s "the Wall is +2/+1" (S.powerToughnessOf wallId after) (Just (2, 9))
    Spec.assertEqWith s "the Rats, whom nobody named, are untouched" (S.powerToughnessOf ratsId after) (Just (1, 1))
  -- The same board and the same spell, differing only in the announced number.
  Spec.it s "CR 601.2c announcing one target pumps one creature" $ do
    (pikerId, _, wallId, gs, spellId) <- heartsBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 1 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker is +2/+1" (S.powerToughnessOf pikerId after) (Just (4, 2))
    Spec.assertEqWith s "and the second creature it would have taken is untouched" (S.powerToughnessOf wallId after) (Just (0, 8))
  -- CR 608.2b: "Illegal targets, if any, won't be affected by parts of a
  -- resolving spell's effect for which they're illegal." One target of two leaves
  -- the battlefield between the announcement and the resolution, which under a
  -- per-SLOT reading of that rule would take the survivor down with it.
  Spec.it s "CR 608.2b one of two targets leaving does not stop the other being pumped" $ do
    (pikerId, _, wallId, gs, spellId) <- heartsBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        gone = snd (Engine.runGamePure answer cast (Event.changeZone wallId Zone.Graveyard))
        after = snd (Engine.runGamePure answer gone Stack.resolveTop)
    Spec.assertEqWith s "the surviving target is +2/+1" (S.powerToughnessOf pikerId after) (Just (4, 2))
    Spec.assertBool s (not (Set.member wallId (GameState.battlefield after))) "and the other one is gone"
  -- CR 601.2c's minimum, which is castability's question: one legal creature is
  -- enough for "one or two", and none is not. Both boards hold the same two
  -- Mountains, so the creature is the only difference between them.
  Spec.it s "CR 601.2c a minimum above zero gates castability" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    hearts <- S.printingOf s registry "Hearts on Fire"
    let lands = S.landsInPlay mountain 2
        (_, withCreature) = S.addCreature piker S.bob lands
        castable board = let (gs, spellId) = S.handOne hearts board in S.castable S.alice spellId gs
    Spec.assertBool s (castable withCreature) "one creature is enough for one or two targets"
    Spec.assertBool s (not (castable lands)) "and no creature is not"
  -- The ability path's own CR 608.2b (Resolve.resolveModes), which the spell path
  -- above does not reach.
  Spec.it s "CR 601.2c Agent Bishop's trigger counters the two creatures it named" $ do
    (bishopId, pikerId, ratsId, wallId, gs) <- bishopBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        after = S.runPure answer (S.runPure answer gs Engine.settleForPriority) Stack.resolveTop
    Spec.assertEqWith s "the Piker took a counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "so did the Wall" (plusCountersOn wallId after) (Just 1)
    Spec.assertEqWith s "the Rats, whom nobody named, took none" (plusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "and neither did Bishop" (plusCountersOn bishopId after) (Just 0)
  Spec.it s "CR 608.2b Agent Bishop's trigger still counters the target that survives" $ do
    (_, pikerId, _, wallId, gs) <- bishopBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        -- The trigger is PLACED (CR 603.3, targets announced) and then one of them
        -- leaves, so CR 608.2b has something to re-validate when it resolves.
        placed = S.runPure answer gs Engine.settleForPriority
        gone = S.runPure answer placed (Event.changeZone wallId Zone.Graveyard)
        after = S.runPure answer gone Stack.resolveTop
    Spec.assertEqWith s "the surviving target took its counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertBool s (not (Set.member wallId (GameState.battlefield after))) "and the other one is gone"

-- Two Mountains for Hearts on Fire, three of bob's creatures with three distinct
-- printed boxes (2/1, 1/1, 0/8), and the spell in alice's hand.
heartsBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
heartsBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  hearts <- S.printingOf s registry "Hearts on Fire"
  let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay mountain 2)
      (ratsId, g2) = S.addCreature rats S.bob g1
      (wallId, g3) = S.addCreature wall S.bob g2
      (gs, spellId) = S.handOne hearts g3
  pure (pikerId, ratsId, wallId, gs, spellId)

-- Agent Bishop on alice's battlefield with the same three creatures, at the
-- beginning of her combat, where its ability triggers.
bishopBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
bishopBoard s registry = do
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  bishop <- S.printingOf s registry "Agent Bishop, Man in Black"
  let (bishopId, g1) = S.addCreature bishop S.alice (Setup.emptyGame S.bothPlayers)
      (pikerId, g2) = S.addCreature piker S.bob g1
      (ratsId, g3) = S.addCreature rats S.bob g2
      (wallId, g4) = S.addCreature wall S.bob g3
      combat = Phase.Combat CombatStep.BeginningOfCombat
      gs =
        Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan combat S.alice)) $
          g4
            { GameState.phase = combat,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
  pure (bishopId, pikerId, ratsId, wallId, gs)

-- Cast a spell from alice's hand and resolve it.
resolveOne :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveOne answer gs spellId =
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- CR 701.47 amass, which is an opcode: Effect.Amass over a subtype and a Quantity,
-- whose Army token, candidate pool and counter kind are rule 701.47a's rather than
-- the card's.
--
-- Relentless Advance {3}{U} Sorcery (data/cards/relentless-advance.json): "Amass
-- Zombies 3.", and nothing else -- so every token and every counter on these boards
-- came from this keyword action.
--
-- Mordor Muster {1}{B} Sorcery (data/cards/mordor-muster.json): "You draw a card
-- and you lose 1 life. Amass Orcs 1." A SECOND subtype is what makes rule 701.47a's
-- last instruction observable at all: a card that only ever amasses its own subtype
-- cannot tell the type addition from the token's printed types, because the token it
-- would have created already has them.
--
-- Three and one are distinct from each other and from their sum, so a counter
-- assertion cannot be satisfied by a coincidence: 3, 1, 4 and 6 each name exactly
-- one history of amasses.
amassSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
amassSpec s registry = Spec.describe s "Amass" $ do
  Spec.it s "CR 701.47a amass with no Army creates the 0/0 black Army token the rule prints" $ do
    (gs, advanceId, _) <- amassBoard s registry
    let after = resolveOne S.identityAnswer gs advanceId
    case S.tokensOf after of
      [army] -> do
        Spec.assertEqWith s "black, which is the rule's colour and not the card's" (Projection.colorsOf army after) (Set.singleton Color.Black)
        -- CR 111.4: rule 701.47a names no token, so the name is its subtypes plus
        -- the word "Token".
        Spec.assertEqWith s "named for its subtypes" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Zombie Army Token") S.alice after) 1
        Spec.assertEqWith s "a Zombie Army" (Projection.subtypesOf army after) (Set.fromList [Subtype.Zombie, Subtype.Army])
        Spec.assertEqWith s "three +1/+1 counters" (plusCountersOn army after) (Just 3)
        -- CR 704.3: state-based actions are not checked mid-resolution, so the 0/0
        -- the rule prints lives to take its counters.
        Spec.assertEqWith s "0/0 plus three counters" (S.powerToughnessOf army after) (Just (3, 3))
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
  -- Rule 701.47a's first instruction taken the other way, and its last, on one
  -- board: the second amass finds an Army and so creates nothing, and its subtype
  -- lands on the Army that was already there.
  Spec.it s "CR 701.47a a second amass creates no second token and adds its subtype to the Army" $ do
    (gs, advanceId, musterId) <- amassBoard s registry
    let after = resolveOne S.identityAnswer (resolveOne S.identityAnswer gs advanceId) musterId
    case S.tokensOf after of
      [army] -> do
        Spec.assertEqWith s "three counters and then one more" (plusCountersOn army after) (Just 4)
        -- CR 205.1b: "in addition to its other types", so the Zombie survives the
        -- Orc rather than being replaced by it.
        Spec.assertEqWith s "an Orc Zombie Army" (Projection.subtypesOf army after) (Set.fromList [Subtype.Orc, Subtype.Zombie, Subtype.Army])
        Spec.assertEqWith s "0/0 plus four counters" (S.powerToughnessOf army after) (Just (4, 4))
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
  -- CR 701.47a's "an Army creature YOU CONTROL", both times it appears: bob's Army
  -- neither stops alice's token being created nor takes her counters.
  Spec.it s "CR 701.47a an opponent's Army is not an Army you control" $ do
    (gs, advanceId, musterId) <- opposedAmassBoard s registry
    let after = resolveOne S.identityAnswer (resolveFor S.bob S.identityAnswer gs musterId) advanceId
    case S.tokensOf after of
      [bobArmy, aliceArmy] -> do
        Spec.assertEqWith s "alice amassed her own Army" (plusCountersOn aliceArmy after) (Just 3)
        Spec.assertEqWith s "a Zombie Army" (Projection.subtypesOf aliceArmy after) (Set.fromList [Subtype.Zombie, Subtype.Army])
        Spec.assertEqWith s "bob's Army kept the one counter he amassed" (plusCountersOn bobArmy after) (Just 1)
        Spec.assertEqWith s "and did not become a Zombie" (Projection.subtypesOf bobArmy after) (Set.fromList [Subtype.Orc, Subtype.Army])
      other -> Spec.assertFailure s ("expected exactly two tokens, got " <> show (length other))
  Spec.it s "CR 701.47a amass counters the Army its controller chose" $ do
    (bobArmy, aliceArmy, gs, advanceId) <- stolenArmyBoard s registry
    let after = resolveOne (amassing aliceArmy) gs advanceId
    Spec.assertEqWith s "her own Army, whom she named, took three more" (plusCountersOn aliceArmy after) (Just 6)
    Spec.assertEqWith s "the borrowed Army took none" (plusCountersOn bobArmy after) (Just 1)
    Spec.assertEqWith s "and no third token was created" (length (S.tokensOf after)) 2
  -- The same board and the same spell, differing only in the answer: the engine
  -- makes no choice, so the other Army is equally reachable -- and the subtype
  -- follows the choice, which is what makes rule 701.47a's last instruction act on
  -- the CHOSEN Army rather than on the amassing player's Armies at large.
  Spec.it s "CR 701.47a the same board answered the other way counters the other Army" $ do
    (bobArmy, aliceArmy, gs, advanceId) <- stolenArmyBoard s registry
    let after = resolveOne (amassing bobArmy) gs advanceId
    Spec.assertEqWith s "the borrowed Army, whom she named, took three" (plusCountersOn bobArmy after) (Just 4)
    Spec.assertEqWith s "and became a Zombie as well as an Orc" (Projection.subtypesOf bobArmy after) (Set.fromList [Subtype.Orc, Subtype.Zombie, Subtype.Army])
    Spec.assertEqWith s "her own Army took none" (plusCountersOn aliceArmy after) (Just 3)
    Spec.assertEqWith s "and stayed a plain Zombie Army" (Projection.subtypesOf aliceArmy after) (Set.fromList [Subtype.Zombie, Subtype.Army])
  -- Where the rules leave nothing to ask, do not ask. The two boards differ in how
  -- many Armies their controller has, which is the whole of what makes rule
  -- 701.47a's choice a choice.
  Spec.it s "CR 701.47a a lone Army raises no prompt" $ do
    (alone, aloneSpell, _) <- amassBoard s registry
    (_, _, two, twoSpell) <- stolenArmyBoard s registry
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseAmass {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g spellId =
          State.execState (Engine.runGame countingAnswer g (S.cast S.alice spellId >> Stack.resolveTop)) 0
    -- The first amass on the first board has no Army at all until it makes one, and
    -- one Army is the whole of the candidate set.
    Spec.assertEqWith s "one Army: nothing to ask" (asks alone aloneSpell) 0
    Spec.assertEqWith s "two Armies: one real decision" (asks two twoSpell) 1

-- alice has six Islands and six Swamps untapped, Relentless Advance and Mordor
-- Muster in hand, and a card left in her library for the Muster's draw (CR 104.3c).
-- Twelve lands rather than the six the two spells cost, so that whichever lands the
-- first payment takes, the second is still payable.
amassBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
amassBoard s registry = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  advance <- S.printingOf s registry "Relentless Advance"
  muster <- S.printingOf s registry "Mordor Muster"
  let g1 = S.landsFor swamp S.alice 6 (S.landsInPlay island 6)
      (g2, advanceId) = S.handOne advance g1
      (musterId, g3) = S.addHandCard muster S.alice g2
      g4 = snd (S.addLibraryCard island S.alice g3)
  pure (g4, advanceId, musterId)

-- amassBoard with the Muster moved across the table: bob holds it, with his own
-- lands and his own library card, so the two spells are cast from two seats. The
-- same printings, the same counts and the same phase -- only the seat differs.
opposedAmassBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
opposedAmassBoard s registry = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  advance <- S.printingOf s registry "Relentless Advance"
  muster <- S.printingOf s registry "Mordor Muster"
  let g1 = S.landsFor swamp S.bob 6 (S.landsInPlay island 6)
      (g2, advanceId) = S.handOne advance g1
      (musterId, g3) = S.addHandCard muster S.bob g2
      g4 = snd (S.addLibraryCard island S.bob g3)
  pure (g4, advanceId, musterId)

-- Two Armies under one player's control, which is the only board on which rule
-- 701.47a's choice is a choice. bob amasses Orcs, alice amasses Zombies, and alice
-- then gains control of bob's Army (CR 613.1b's layer 2) -- so her second Relentless
-- Advance sees two Armies, one of each subtype. Returns bob's Army, alice's own, the
-- board and the spell still in her hand.
--
-- Ten Islands, since alice casts Relentless Advance twice: six leaves the second
-- unpayable, and an uncast spell is a board that proves nothing.
stolenArmyBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
stolenArmyBoard s registry = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  advance <- S.printingOf s registry "Relentless Advance"
  muster <- S.printingOf s registry "Mordor Muster"
  let g1 = S.landsFor swamp S.bob 6 (S.landsInPlay island 10)
      (g2, firstId) = S.handOne advance g1
      (secondId, g3) = S.addHandCard advance S.alice g2
      (musterId, g4) = S.addHandCard muster S.bob g3
      g5 = snd (S.addLibraryCard island S.bob g4)
      amassed = resolveOne S.identityAnswer (resolveFor S.bob S.identityAnswer g5 musterId) firstId
  case S.tokensOf amassed of
    [bobArmy, aliceArmy] -> pure (bobArmy, aliceArmy, S.giveControl bobArmy S.alice amassed, secondId)
    other -> do
      Spec.assertFailure s ("expected exactly two tokens, got " <> show (length other))
      pure (S.noSource, S.noSource, amassed, secondId)

-- resolveOne for a seat other than alice's: bob casts and the spell resolves.
resolveFor :: PlayerId.PlayerId -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveFor pid answer gs spellId =
  let cast = snd (Engine.runGamePure answer gs (S.cast pid spellId))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- CR 701.68 blight, which is an opcode: Effect.Blight over a Quantity, whose
-- candidate pool and counter kind are rule 701.68a's rather than the card's.
--
-- Sinister Gnarlbark {2}{B} 0/4 Creature -- Treefolk Warlock
-- (data/cards/sinister-gnarlbark.json): "At the beginning of your end step, draw a
-- card and blight 1." (Name, cost, type line, P/T and oracle text checked against
-- Scryfall.) Every -1/-1 counter on these boards came from the keyword action, and
-- the draw beside it is what shows the REST of a mandatory instruction still runs
-- when the blight itself cannot (CR 101.3).
--
-- The pool is UNCONSTRAINED, which is the whole difference from bolster: the
-- boards below carry a 2/1, a 1/1, a 0/8 and the 0/4 source -- toughnesses 1, 1, 8
-- and 4, so the least is TIED and two creatures are clear of it. A case that names
-- the 0/8 proves no least-toughness narrowing is happening, and one that names the
-- source proves the blighting permanent is in its own pool.
--
-- Every assertion below reads counters on ONE named creature and reads the other
-- three back as zero, so no case can be satisfied by counters that landed
-- somewhere else.
blightSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blightSpec s registry = Spec.describe s "Blight" $ do
  Spec.it s "CR 701.68a blight 1 counters the creature its controller chose" $ do
    (pikerId, ratsId, wallId, gnarlbarkId, gs) <- blightBoard s registry S.alice
    let after = S.runPure (blighting pikerId) gs Stack.resolveTop
    Spec.assertEqWith s "the Piker, whom their controller named, took one" (minusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "the Rats took none" (minusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (minusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor the Gnarlbark itself" (minusCountersOn gnarlbarkId after) (Just 0)
    Spec.assertEqWith s "and the card was drawn" (S.handSize S.alice after) 1
  -- The same board and the same trigger, differing only in the answer: the engine
  -- makes no choice, so every other creature in the pool is equally reachable.
  Spec.it s "CR 701.68a the same board answered another way counters that creature" $ do
    (pikerId, ratsId, wallId, gnarlbarkId, gs) <- blightBoard s registry S.alice
    let after = S.runPure (blighting ratsId) gs Stack.resolveTop
    Spec.assertEqWith s "the Rats, whom their controller named, took one" (minusCountersOn ratsId after) (Just 1)
    Spec.assertEqWith s "the Piker took none" (minusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (minusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor the Gnarlbark itself" (minusCountersOn gnarlbarkId after) (Just 0)
  -- Rule 701.68a's pool is "a creature you control" and stops there. The 0/8 is the
  -- TOUGHEST creature on the board and the 0/4 is the blighting permanent itself;
  -- both are candidates, where CR 701.39a's least-toughness narrowing would offer
  -- neither.
  Spec.it s "CR 701.68a any creature its controller controls is a candidate, including the source" $ do
    (pikerId, ratsId, wallId, _, wallBoard) <- blightBoard s registry S.alice
    (_, _, _, gnarlbarkId, sourceBoard) <- blightBoard s registry S.alice
    let onWall = S.runPure (blighting wallId) wallBoard Stack.resolveTop
        onSource = S.runPure (blighting gnarlbarkId) sourceBoard Stack.resolveTop
    Spec.assertEqWith s "the Wall, at toughness 8, took one" (minusCountersOn wallId onWall) (Just 1)
    Spec.assertEqWith s "and the 1/1 beside it took none" (minusCountersOn ratsId onWall) (Just 0)
    Spec.assertEqWith s "the Gnarlbark blighted itself" (minusCountersOn gnarlbarkId onSource) (Just 1)
    Spec.assertEqWith s "and the Piker took none" (minusCountersOn pikerId onSource) (Just 0)
  -- CR 701.68a's "a creature YOU CONTROL". The two boards hold the same four
  -- printings and differ in exactly one thing -- which seat the other three
  -- creatures sit on -- and the answer names bob's Piker on both. It is never
  -- offered, so alice's own Gnarlbark takes the counter instead.
  Spec.it s "CR 701.68a an opponent's creature is not a creature you control" $ do
    (pikerId, ratsId, wallId, gnarlbarkId, gs) <- blightBoard s registry S.bob
    let after = S.runPure (blighting pikerId) gs Stack.resolveTop
    Spec.assertEqWith s "alice's Gnarlbark, her only creature, took one" (minusCountersOn gnarlbarkId after) (Just 1)
    Spec.assertEqWith s "bob's Piker, whom the answer named, took none" (minusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did bob's Rats" (minusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "nor his Wall" (minusCountersOn wallId after) (Just 0)
  -- Where the rules leave nothing to ask, do not ask. The same pair of boards, and
  -- what differs is how many creatures the blighting player controls -- which is
  -- the whole of what makes rule 701.68a's choice a choice.
  Spec.it s "CR 701.68a a lone creature raises no prompt" $ do
    (_, _, _, _, four) <- blightBoard s registry S.alice
    (_, _, _, _, alone) <- blightBoard s registry S.bob
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseBlight {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g = State.execState (Engine.runGame countingAnswer g Stack.resolveTop) 0
    Spec.assertEqWith s "one creature: nothing to ask" (asks alone) 0
    Spec.assertEqWith s "four creatures: one real decision" (asks four) 1
  -- CR 101.3: the impossible PART is ignored, not the instruction. The Gnarlbark
  -- dies to state-based actions with its own trigger already on the stack (CR
  -- 603.3b), so the blight has no creature to reach -- and the draw beside it still
  -- happens, which is what tells "ignored" apart from "aborted".
  Spec.it s "CR 101.3 a controller with no creature blights nothing and draws anyway" $ do
    (pikerId, _, _, gnarlbarkId, gs) <- blightBoard s registry S.bob
    let dead = S.settleSba (S.markDamage gnarlbarkId 4 gs)
        after = S.runPure S.identityAnswer dead Stack.resolveTop
    Spec.assertBool s (not (S.onBattlefield gnarlbarkId after)) "the Gnarlbark left the battlefield before its trigger resolved"
    Spec.assertEqWith s "the card was drawn all the same" (S.handSize S.alice after) 1
    Spec.assertEqWith s "and bob's creatures, who were never candidates, took nothing" (minusCountersOn pikerId after) (Just 0)

-- Sinister Gnarlbark on alice's battlefield and Goblin Piker, Typhoid Rats and Wall
-- of Stone on `pid`'s, with a card in alice's library for the draw (CR 104.3c), her
-- end step begun and the trigger settled onto the stack (CR 603.3b). Returns the
-- three creatures, the Gnarlbark and that state.
--
-- The seat is the ONLY parameter, so the pool board and its negative are the same
-- four printings, the same library and the same step.
blightBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
blightBoard s registry pid = do
  swamp <- S.printingOf s registry "Swamp"
  gnarlbark <- S.printingOf s registry "Sinister Gnarlbark"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  let (pikerId, g1) = S.addCreature piker pid (Setup.emptyGame S.bothPlayers)
      (ratsId, g2) = S.addCreature rats pid g1
      (wallId, g3) = S.addCreature wall pid g2
      (gnarlbarkId, g4) = S.addCreature gnarlbark S.alice g3
      g5 = snd (S.addLibraryCard swamp S.alice g4)
      endStep = Phase.Ending EndingStep.EndStep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice))
          (g5 {GameState.phase = endStep, GameState.activePlayer = S.alice})
  pure (pikerId, ratsId, wallId, gnarlbarkId, snd (Engine.runGamePure S.identityAnswer begun Engine.settleForPriority))

-- Answers Prompt.ChooseBlight with a named creature, deferring everything else to
-- S.identityAnswer. PINNED BY ID rather than picked by searching the candidates, so
-- a mutation to the candidate sweep cannot quietly repair the answer.
blighting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
blighting oid p = case p of
  Prompt.ChooseBlight {} -> oid
  _ -> S.identityAnswer p

-- The -1/-1 counters on one permanent, plusCountersOn's sibling: Nothing once the
-- object is gone, which is what keeps "took none" apart from "is not there".
minusCountersOn :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural
minusCountersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject oid gs)

-- Answers Prompt.ChooseAmass with a named Army, deferring everything else to
-- S.identityAnswer. PINNED BY ID rather than picked by searching the candidates, so
-- a mutation to the candidate sweep cannot quietly repair the answer.
amassing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
amassing oid p = case p of
  Prompt.ChooseAmass {} -> oid
  _ -> S.identityAnswer p

-- CR 701.41 support, which is card DATA and no opcode: "support N" is written out
-- as the counters it means, over a CR 601.2c slot of 0 to N.
--
-- N is a LITERAL range here. A count that reads X -- The Crowd Goes Wild's
-- "Support X" -- is not expressible (#1271).
--
-- Lead by Example {1}{G} Instant (data/cards/lead-by-example.json): "Support 2.",
-- and nothing else -- CR 701.41a's INSTANT reading, which has no "other" in it.
--
-- Joraga Auxiliary {1}{G}{W} 2/3 (data/cards/joraga-auxiliary.json):
-- "{4}{G}{W}: Support 2.", CR 701.41a's PERMANENT reading, whose "other" is a
-- Not IsSource on the slot.
--
-- Three readings of "up to two target creatures" a careless board cannot tell
-- apart -- two of three, one of three, and none -- so each case names a different
-- number of targets on the same board and the creature nobody named is asserted
-- untouched. Three candidates for a slot that takes two, because a prompt offered
-- exactly as many candidates as it needs is never asked.
supportSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
supportSpec s registry = Spec.describe s "Support" $ do
  Spec.it s "CR 701.41a support 2 counters each of the two creatures it named" $ do
    (pikerId, wallId, ratsId, gs, spellId) <- leadBoard s registry []
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker took a counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "so did the Wall" (plusCountersOn wallId after) (Just 1)
    Spec.assertEqWith s "the Rats, whom nobody named, took none" (plusCountersOn ratsId after) (Just 0)
  -- The same board and the same spell, differing only in the announced number:
  -- "up to two" allows one, and one is not two.
  Spec.it s "CR 601.2c support 2 announcing one target counters only that one" $ do
    (pikerId, wallId, ratsId, gs, spellId) <- leadBoard s registry []
    let answer :: Prompt.Prompt r -> r
        answer = takingTargets 1 [pikerId, wallId]
        after = resolveOne answer gs spellId
    Spec.assertEqWith s "the Piker took a counter" (plusCountersOn pikerId after) (Just 1)
    Spec.assertEqWith s "and the second creature it could have taken took none" (plusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor did the Rats" (plusCountersOn ratsId after) (Just 0)
  -- CR 115.6's zero. Lead by Example has no second clause, so what makes the
  -- declined case observable is that no counter appears anywhere: an engine that
  -- chose the targets itself would put two.
  Spec.it s "CR 115.6 support 2 announcing no targets counters nobody" $ do
    (pikerId, wallId, ratsId, gs, spellId) <- leadBoard s registry []
    let after = resolveOne decliningTargets gs spellId
    Spec.assertEqWith s "the Piker took none" (plusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor the Wall" (plusCountersOn wallId after) (Just 0)
    Spec.assertEqWith s "nor the Rats" (plusCountersOn ratsId after) (Just 0)
  -- CR 122.6 / 614.16: each of support's placements reaches the funnel on its own,
  -- so a counter-scaling replacement gets an opportunity against every target
  -- rather than one against the batch. Doubling Season reads whose PERMANENT it is,
  -- which is why both targets here are alice's.
  Spec.it s "CR 122.6 Doubling Season doubles support's counter on each target" $ do
    (pikerId, wallId, _, seasoned, seasonedSpell) <- leadBoard s registry [("Doubling Season", S.alice)]
    (barePiker, bareWall, _, bare, bareSpell) <- leadBoard s registry []
    let seasonedAfter = resolveOne (takingTargets 2 [pikerId, wallId]) seasoned seasonedSpell
        bareAfter = resolveOne (takingTargets 2 [barePiker, bareWall]) bare bareSpell
    Spec.assertEqWith s "1 * 2 on the Piker" (plusCountersOn pikerId seasonedAfter) (Just 2)
    Spec.assertEqWith s "1 * 2 on the Wall too" (plusCountersOn wallId seasonedAfter) (Just 2)
    Spec.assertEqWith s "and one each without the enchantment" (plusCountersOn barePiker bareAfter, plusCountersOn bareWall bareAfter) (Just 1, Just 1)
  -- The same funnel from the other side: half of one counter, rounded down, is
  -- none, so zero, one and two are three distinct answers to the same board.
  -- Vorinclex reads who is PUTTING the counters (CR 122.6a), and the targets here
  -- are alice's own permanents -- which is what separates it from Doubling Season's
  -- recipient reading, since bob's praetor halves them anyway.
  Spec.it s "CR 122.6a an opponent's Vorinclex halves support's counters away" $ do
    (pikerId, wallId, _, watched, watchedSpell) <- leadBoard s registry [("Vorinclex, Monstrous Raider", S.bob)]
    (barePiker, bareWall, _, bare, bareSpell) <- leadBoard s registry []
    let watchedAfter = resolveOne (takingTargets 2 [pikerId, wallId]) watched watchedSpell
        bareAfter = resolveOne (takingTargets 2 [barePiker, bareWall]) bare bareSpell
    Spec.assertEqWith s "half of one on the Piker" (plusCountersOn pikerId watchedAfter) (Just 0)
    Spec.assertEqWith s "half of one on the Wall" (plusCountersOn wallId watchedAfter) (Just 0)
    Spec.assertEqWith s "and one each without the praetor" (plusCountersOn barePiker bareAfter, plusCountersOn bareWall bareAfter) (Just 1, Just 1)
  -- CR 701.41a's "other", which only the PERMANENT reading has. The answerer names
  -- the Auxiliary FIRST, so a slot that offered it would spend one of its two
  -- targets on it and leave one of the Rats and the Piker at zero.
  Spec.it s "CR 701.41a support on a permanent cannot choose that permanent" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    wall <- S.printingOf s registry "Wall of Stone"
    joraga <- S.printingOf s registry "Joraga Auxiliary"
    let (_, g0) = S.addCreature plains S.alice (S.landsInPlay forest 5)
        (auxId, g1) = S.addCreature joraga S.alice g0
        (pikerId, g2) = S.addCreature piker S.bob g1
        (ratsId, g3) = S.addCreature rats S.bob g2
        (wallId, g4) = S.addCreature wall S.bob g3
        board = g4 {GameState.priority = Just S.alice}
        answer :: Prompt.Prompt r -> r
        answer = takingTargets 2 [auxId, pikerId, ratsId]
    case Activate.abilitiesFor auxId board of
      [ability] -> do
        let after = S.runPure answer board (Activate.activateAbility S.alice auxId ability >> Stack.resolveTop)
        Spec.assertEqWith s "the Auxiliary itself, which support excludes, took none" (plusCountersOn auxId after) (Just 0)
        Spec.assertEqWith s "the Piker took one" (plusCountersOn pikerId after) (Just 1)
        Spec.assertEqWith s "and so did the Rats" (plusCountersOn ratsId after) (Just 1)
        Spec.assertEqWith s "the Wall, whom nobody named, took none" (plusCountersOn wallId after) (Just 0)
      abilities -> Spec.assertFailure s ("expected one ability, got " <> show (length abilities))

-- Two Forests for Lead by Example, three creatures with three distinct printed
-- boxes (2/1, 0/8, 1/1) so which of them took a counter is legible, and the spell
-- in alice's hand. The first two are alice's, since Doubling Season's clause reads
-- whose permanent takes the counter; the Rats are bob's. `extra` seats further
-- printings by name, which is the only difference between a case and its control.
leadBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [(String, PlayerId.PlayerId)] ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
leadBoard s registry extra = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  lead <- S.printingOf s registry "Lead by Example"
  extras <- mapM (\(name, pid) -> fmap (\p -> (p, pid)) (S.printingOf s registry name)) extra
  let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay forest 2)
      (wallId, g2) = S.addCreature wall S.alice g1
      (ratsId, g3) = S.addCreature rats S.bob g2
      g4 = List.foldl' (\g (p, pid) -> snd (S.addCreature p pid g)) g3 extras
      (gs, spellId) = S.handOne lead g4
  pure (pikerId, wallId, ratsId, gs, spellId)

-- CR 701.39 bolster, which is an opcode: Effect.Bolster over a Quantity, whose
-- candidate pool and counter kind are rule 701.39a's rather than the card's.
--
-- Cached Defenses {2}{G} Sorcery (data/cards/cached-defenses.json): "Bolster 3.",
-- and nothing else -- so every counter that appears on these boards came from this
-- keyword action and nothing else on the card can stand in for it.
--
-- Two readings of "the least toughness ... or tied for least" a careless board
-- cannot tell apart -- the engine picking for the player, and the player picking
-- -- so the two boards below differ in exactly one thing each. The tie is
-- deliberate: a lone creature at the minimum is never asked about, and three
-- creatures at 1, 1 and 8 make "which of the two" and "not the third" separate
-- questions.
--
-- Three is bolster's own N, and it is distinct from every toughness on the board,
-- so no assertion can be satisfied by a coincidence.
bolsterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bolsterSpec s registry = Spec.describe s "Bolster" $ do
  Spec.it s "CR 701.39a bolster 3 counters the creature its controller chose" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- bolsterBoard s registry
    let after = resolveOne (bolstering ratsId) gs spellId
    Spec.assertEqWith s "the Rats, whom their controller named, took three" (plusCountersOn ratsId after) (Just 3)
    Spec.assertEqWith s "the Piker, tied with them, took none" (plusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (plusCountersOn wallId after) (Just 0)
  -- The same board and the same spell, differing only in the answer: the engine
  -- makes no choice, so the other half of the tie is equally reachable.
  Spec.it s "CR 701.39a the same tie answered the other way counters the other creature" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- bolsterBoard s registry
    let after = resolveOne (bolstering pikerId) gs spellId
    Spec.assertEqWith s "the Piker, whom their controller named, took three" (plusCountersOn pikerId after) (Just 3)
    Spec.assertEqWith s "the Rats took none" (plusCountersOn ratsId after) (Just 0)
    Spec.assertEqWith s "nor did the Wall" (plusCountersOn wallId after) (Just 0)
  -- "With the least toughness" is a filter on the candidates rather than advice:
  -- the Wall is named and still gets nothing, because it was never offered.
  Spec.it s "CR 701.39a a creature that is not tied for least toughness cannot be chosen" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- bolsterBoard s registry
    let after = resolveOne (bolstering wallId) gs spellId
    Spec.assertEqWith s "the Wall, at toughness 8, took none" (plusCountersOn wallId after) (Just 0)
    -- One of the tied pair took all three, and the fallback picks which; what is
    -- under test is that the counters did not follow the answer.
    Spec.assertEqWith
      s
      "the tie took them instead"
      (fmap (+) (plusCountersOn pikerId after) <*> plusCountersOn ratsId after)
      (Just 3)
  -- CR 701.39a's "among creatures you control". The two smallest creatures on the
  -- battlefield are bob's, and neither is a candidate: alice's own Wall is the
  -- whole of her pool however large it is.
  Spec.it s "CR 701.39a bolster looks only at creatures its controller controls" $ do
    (pikerId, ratsId, wallId, gs, spellId) <- opposedBolsterBoard s registry
    let after = resolveOne S.identityAnswer gs spellId
    Spec.assertEqWith s "alice's Wall, her only creature, took three" (plusCountersOn wallId after) (Just 3)
    Spec.assertEqWith s "bob's Piker, at toughness 1, took none" (plusCountersOn pikerId after) (Just 0)
    Spec.assertEqWith s "nor did bob's Rats" (plusCountersOn ratsId after) (Just 0)
  -- Where the rules leave nothing to ask, do not ask. The two boards differ in
  -- whether the least toughness is TIED, which is the whole of what makes rule
  -- 701.39a's choice a choice.
  Spec.it s "CR 701.39a a lone creature at the least toughness raises no prompt" $ do
    (_, _, _, tied, tiedSpell) <- bolsterBoard s registry
    (_, _, _, alone, aloneSpell) <- opposedBolsterBoard s registry
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseBolster {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g spellId =
          State.execState (Engine.runGame countingAnswer g (S.cast S.alice spellId >> Stack.resolveTop)) 0
    Spec.assertEqWith s "one creature at the minimum: nothing to ask" (asks alone aloneSpell) 0
    Spec.assertEqWith s "two tied for it: one real decision" (asks tied tiedSpell) 1

-- Three Forests for Cached Defenses, and three of alice's creatures whose printed
-- toughnesses are 1, 1 and 8 -- a TIE at the least, and a third creature well
-- clear of it -- with the spell in her hand.
bolsterBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
bolsterBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  defenses <- S.printingOf s registry "Cached Defenses"
  let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay forest 3)
      (ratsId, g2) = S.addCreature rats S.alice g1
      (wallId, g3) = S.addCreature wall S.alice g2
      (gs, spellId) = S.handOne defenses g3
  pure (pikerId, ratsId, wallId, gs, spellId)

-- bolsterBoard with the tied pair moved across the table: the two creatures at
-- toughness 1 are BOB's, so alice's pool is her Wall alone. The same three
-- printings, the same three lands and the same spell -- only the seats differ.
opposedBolsterBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
opposedBolsterBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  wall <- S.printingOf s registry "Wall of Stone"
  defenses <- S.printingOf s registry "Cached Defenses"
  let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay forest 3)
      (ratsId, g2) = S.addCreature rats S.bob g1
      (wallId, g3) = S.addCreature wall S.alice g2
      (gs, spellId) = S.handOne defenses g3
  pure (pikerId, ratsId, wallId, gs, spellId)

-- Answers Prompt.ChooseBolster with a named creature, deferring everything else to
-- S.identityAnswer. PINNED BY ID rather than picked by searching the candidates,
-- so a mutation to the candidate sweep cannot quietly repair the answer.
bolstering :: ObjectId.ObjectId -> Prompt.Prompt r -> r
bolstering oid p = case p of
  Prompt.ChooseBolster {} -> oid
  _ -> S.identityAnswer p

-- CR 115.6: declines every optional slot, announcing zero targets. Everything
-- else is S.identityAnswer's answer, which for ChooseTargets fills what it is
-- offered -- so the two answerers differ in exactly one decision.
decliningTargets :: Prompt.Prompt r -> r
decliningTargets p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 0) offers
  _ -> S.identityAnswer p

-- Announces one named slot and declines the rest.
announcingOnly :: SlotName.SlotName -> Prompt.Prompt r -> r
announcingOnly slot p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> Map.mapWithKey (\name (count, _) -> if name == slot then TargetCount.most count else 0) offers
  _ -> S.identityAnswer p

-- CR 115.6's "up to one target", read at resolution.
--
-- Rat Out {B} Instant (data/cards/rat-out.json): "Up to one target creature gets
-- -1/-1 until end of turn. You create a 1/1 black Rat creature token with 'This
-- token can't block.'" The Rat is what makes the zero-target case OBSERVABLE: a
-- spell that fizzled under CR 608.2b would make no token, and the card's own
-- graveyard trip is the same either way.
--
-- Explosive Entry {1}{R} Sorcery (data/cards/explosive-entry.json): "Destroy up
-- to one target artifact. Put a +1/+1 counter on up to one target creature." Two
-- independently optional slots, so one can be taken while the other is declined.
upToOneTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
upToOneTargetSpec s registry = Spec.describe s "UpToOneTarget" $ do
  Spec.it s "CR 115.6 Rat Out aimed at a creature shrinks it and still makes the Rat" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    let (victim, board) = S.addCreature piker S.bob (S.landsInPlay swamp 1)
        (gs, spellId) = S.handOne ratOut board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "power 1" (Projection.powerOf victim after) (Just 1)
    Spec.assertEqWith s "toughness 0" (Projection.toughnessOf victim after) (Just 0)
    Spec.assertEqWith s "one Rat" (length (S.tokensOf after)) 1
  -- The same board and the same spell, differing only in the CR 601.2c
  -- announcement: zero targets rather than one.
  Spec.it s "CR 115.6 Rat Out with zero targets announced resolves, leaving the creature alone" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    let (victim, board) = S.addCreature piker S.bob (S.landsInPlay swamp 1)
        (gs, spellId) = S.handOne ratOut board
        cast = snd (Engine.runGamePure decliningTargets gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure decliningTargets cast Stack.resolveTop)
    Spec.assertEqWith s "still 2/1" (Projection.powerOf victim after) (Just 2)
    Spec.assertEqWith s "still 2/1" (Projection.toughnessOf victim after) (Just 1)
    -- CR 608.2b does not fizzle a spell that chose no targets at all.
    Spec.assertEqWith s "the Rat was still made" (length (S.tokensOf after)) 1
  Spec.it s "CR 115.6 Explosive Entry takes one slot and declines the other" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    explosiveEntry <- S.printingOf s registry "Explosive Entry"
    let (equipment, withArtifact) = S.addCreature bonesplitter S.bob (S.landsInPlay mountain 2)
        (creature, board) = S.addCreature piker S.bob withArtifact
        (gs, spellId) = S.handOne explosiveEntry board
        answer = announcingOnly (SlotName.MkSlotName (Text.pack "artifact"))
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertBool s (not (Set.member equipment (GameState.battlefield after))) "the artifact was destroyed"
    Spec.assertEqWith s "the creature got no counter" (Projection.powerOf creature after) (Just 2)
  Spec.it s "CR 115.6 Explosive Entry takes both slots" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    piker <- S.printingOf s registry "Goblin Piker"
    explosiveEntry <- S.printingOf s registry "Explosive Entry"
    let (equipment, withArtifact) = S.addCreature bonesplitter S.bob (S.landsInPlay mountain 2)
        (creature, board) = S.addCreature piker S.bob withArtifact
        (gs, spellId) = S.handOne explosiveEntry board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (not (Set.member equipment (GameState.battlefield after))) "the artifact was destroyed"
    Spec.assertEqWith s "and the creature got its counter" (Projection.powerOf creature after) (Just 3)
  -- The same rule on an ACTIVATED ability (CR 602.2b routes it through CR
  -- 601.2c), where Resolve.resolveModes rather than Resolve.targetsAllIllegal
  -- asks CR 608.2b's question. Conjurer's Bauble {1} Artifact: "{T}, Sacrifice
  -- this artifact: Put up to one target card from your graveyard on the bottom of
  -- your library. Draw a card." The draw is the observer, and the sacrifice is a
  -- COST, so it is paid either way and cannot stand in for one.
  Spec.it s "CR 115.6 Conjurer's Bauble with zero targets announced still draws" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    bauble <- S.printingOf s registry "Conjurer's Bauble"
    let (baubleId, placed) = S.addCreature bauble S.alice (S.landsInPlay plains 1)
        (_, buried) = S.addGraveyardCard piker S.alice placed
        board = (stockLibrary piker S.alice 5 buried) {GameState.priority = Just S.alice}
        activate :: (forall r. Prompt.Prompt r -> r) -> ActivatedAbility.ActivatedAbility Card.Type.Card -> GameState.GameState
        activate answer ability = S.runPure answer board $ do
          Activate.activateAbility S.alice baubleId ability
          Stack.resolveTop
        graveyardSize gs = Seq.length (Map.findWithDefault Seq.empty S.alice (GameState.graveyard gs))
    case Activate.abilitiesFor baubleId board of
      [ability] -> do
        let declined = activate decliningTargets ability
            taken = activate S.identityAnswer ability
        Spec.assertEqWith s "declining still draws" (S.handSize S.alice declined) 1
        -- The Bauble's own sacrifice, and the Piker still where it was.
        Spec.assertEqWith s "the graveyard kept its card and gained the Bauble" (graveyardSize declined) 2
        Spec.assertEqWith s "taking the target draws too" (S.handSize S.alice taken) 1
        Spec.assertEqWith s "and the Piker went to the library" (graveyardSize taken) 1
      abilities -> Spec.assertFailure s ("expected one ability, got " <> show (length abilities))

-- CR 608.2f's per-object BODY, and the per-iteration binding that makes it more
-- than a repeated opcode.
--
-- Soulfire Eruption {6}{R}{R}{R} Sorcery (data/cards/soulfire-eruption.json) --
-- "Choose any number of target creatures, planeswalkers, and/or players. For
-- each of them, exile the top card of your library, then Soulfire Eruption deals
-- damage equal to that card's mana value to that permanent or player. You may
-- play the exiled cards until the end of your next turn." (name, cost, type line
-- and Oracle text checked against api.scryfall.com.) It is rule 608.2f's own
-- second example.
--
-- TWO DEPARTURES FROM THE PRINTED CARD, both stricter than printed and both
-- irrelevant to what is asserted here: "any number of target" is written as up
-- to three (#1476), and "until the end of your next turn" as "until your next
-- turn" (#1477).
--
-- alice casts it off nine Mountains at a THREE-seat board and the priority loop
-- resolves it, which is what makes this gameplay-level rather than an
-- applyEffect call. The board tells apart every wrong reading of the loop:
--
--   * A BODY PER MEMBER, not one body. Three victims are named, so a loop that
--     ran the body once damages one seat and leaves two at twenty.
--   * A FRESH BINDING PER MEMBER, not one shared. The three cards exiled have
--     mana values 1, 2 and 4 -- pairwise distinct AND pairwise-sum distinct, so
--     no two readings land on one number -- and each victim's damage must be its
--     OWN card's. A loop that bound the first exiled card, or the last, for
--     every victim gives all three seats the same damage.
--   * A DEPLETING RESOURCE, which is the whole reason this shape needs an
--     opcode. Each iteration reads "the top card of your library" AFTER the
--     previous one exiled its own, so the three cards must be three DIFFERENT
--     cards, and the two under them must still be in the library in order (CR
--     401.2). A body re-reading the pre-loop board would exile one card three
--     times.
--   * APNAP (CR 608.2f's primary determination). alice is the active player and
--     the seating is [alice, bob, carol], so the top card goes to alice, the
--     next to bob and the third to carol -- reversing or permuting the order
--     permutes the three life totals, which are distinct.
--   * A PER-ITERATION GRANT. Every exiled card carries CR 601.3's play
--     permission, so the third body instruction ran for each member rather than
--     once for whichever card was bound last.
--
-- Ogre Sentry sits under bob so the target pool holds a fourth candidate the
-- announcement does not take: the choice is a real choice rather than one
-- short-circuited by having exactly as many candidates as it needs, and a loop
-- that swept the battlefield instead of the slot would reach it.
soulfireEruptionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulfireEruptionSpec s registry =
  let -- alice's library holds `stock`, DEEPEST FIRST -- S.addLibraryCard puts
      -- each card on top, so the last name given is the top card.
      board stock = do
        mountain <- S.printingOf s registry "Mountain"
        soulfireEruption <- S.printingOf s registry "Soulfire Eruption"
        sentry <- S.printingOf s registry "Ogre Sentry"
        stocked <- mapM (S.printingOf s registry) stock
        let g1 = S.landsFor mountain S.alice 9 S.threePlayerGame
            g2 = List.foldl' (\g pr -> snd (S.addLibraryCard pr S.alice g)) g1 stocked
            g3 = snd (S.addCreature sentry S.bob g2)
            (withSpell, spell) = S.handOne soulfireEruption g3
            afterCast = S.runPure aimingAtEveryPlayer withSpell (S.cast S.alice spell)
        pure (S.runPure aimingAtEveryPlayer afterCast Engine.priorityLoop)
      named = Just . CardName.MkCardName . Text.pack
      exiledNames pid = List.sort . namesIn Zone.Exile pid
      permissionsIn pid gs = fmap (Maybe.isJust . Object.playableFromExile) (Maybe.mapMaybe (\oid -> Game.lookupObject oid gs) (Game.zoneMembers Zone.Exile pid gs))
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "SoulfireEruption" $ do
        Spec.it s "CR 608.2f each victim takes the mana value of the card exiled FOR IT, in APNAP order" $ do
          after <- board ["Sabretooth Tiger", "Bird Maiden", "Hill Giant", "Goblin Piker", "Benalish Hero"]
          Spec.assertEqWith
            s
            "three DIFFERENT cards left the library, top first, and the two under them stayed in order"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            ( List.sort [named "Benalish Hero", named "Goblin Piker", named "Hill Giant"],
              [named "Bird Maiden", named "Sabretooth Tiger"]
            )
          Spec.assertEqWith
            s
            "Benalish Hero (1) to alice, Goblin Piker (2) to bob, Hill Giant (4) to carol"
            (lives after)
            (Just 19, Just 18, Just 16)
          Spec.assertEqWith
            s
            "all three exiled cards carry the play permission, so the grant ran once per iteration"
            (permissionsIn S.alice after)
            [True, True, True]
        -- CR 609.3 inside the loop: the library runs out mid-sweep, so the
        -- iterations that find no top card exile nothing and their DealDamage
        -- has no mana value to read (Quantity.AgainstSlot answers Nothing, and an
        -- unevaluable quantity is a no-op). The batch is not shortened -- the
        -- members were swept before the first pass -- so the third seat simply
        -- takes nothing.
        Spec.it s "CR 609.3 a library that runs out mid-loop leaves the later members untouched" $ do
          after <- board ["Goblin Piker", "Benalish Hero"]
          Spec.assertEqWith
            s
            "alice took 1 and bob took 2; carol found no card and took nothing"
            (lives after)
            (Just 19, Just 18, Just 20)
          Spec.assertEqWith
            s
            "both cards were exiled and the library is empty"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            (List.sort [named "Benalish Hero", named "Goblin Piker"], [])
          Spec.assertEqWith s "the game has no result: an empty library is not itself a loss" (GameState.result after) Nothing

-- CR 601.2c: announce three targets per slot and aim them at the PLAYERS, which
-- on this board leaves bob's Ogre Sentry -- a legal candidate of the same
-- AnyTarget pool -- deliberately unchosen.
aimingAtEveryPlayer :: Prompt.Prompt r -> r
aimingAtEveryPlayer p = case p of
  Prompt.AnnounceTargets {} -> announcingCount 3 p
  Prompt.ChooseTargets _ _ _ sets -> S.preferring isPlayerRecipient sets
  _ -> S.identityAnswer p
  where
    isPlayerRecipient r = case r of
      Recipient.ToPlayer _ -> True
      Recipient.ToCreature _ -> False
      Recipient.ToPlaneswalker _ -> False
      Recipient.ToBattle _ -> False
      Recipient.ToObject _ -> False
