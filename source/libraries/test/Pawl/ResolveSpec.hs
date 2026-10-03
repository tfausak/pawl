{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Resolve and Pawl.Engine.Target: targeting legality and the
-- core of spell resolution. The rest of Pawl.Engine.Resolve is covered by the
-- sibling modules named in Main.hs, which all describe under the same name.
module Pawl.ResolveSpec where

-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Resolve.Slots as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.Activator as Activator
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
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryBlock as EntryBlock
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LifeLoss as LifeLoss
import qualified Pawl.Types.LifeLossCause as LifeLossCause
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PayOffer as PayOffer
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.Zone as Zone

targetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
targetSpec s registry = Spec.describe s "Target" $ do
  Spec.it s "CR 115.4 AnyTarget offers every creature and every playing player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "creature and both players"
      (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.AnyTarget Nothing) gs)
      (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])
  Spec.it s "a departed player is not a legal target" $ do
    let gs = S.departs Departure.Type.Lost S.bob (Setup.emptyGame S.bothPlayers)
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
    let (myr, board) = S.addPermanent darksteelMyr S.carol S.threePlayerGame
        gone = S.departs Departure.Type.Conceded S.bob board
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
    let (oid, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.AnyTarget Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.AnyTarget Nothing) gone)) "illegal once moved"
  Spec.it s "legalSets maps each slot to its legal recipients" $ do
    let slots = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.AnyTarget Nothing)
        gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith
      s
      "one slot, two players"
      (Target.legalSets Nothing False Map.empty S.noSource slots gs)
      (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
  Spec.it s "CR 115.4 CreatureTarget offers creatures but no players" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
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
    let (oid, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.Creatures Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing Map.empty S.noSource (Recipient.ToCreature oid) (TargetSlot.required Pool.Creatures Nothing) gone)) "illegal once moved"
  Spec.it s "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (permId, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertBool
      s
      (Set.member (Recipient.ToObject permId) (Target.legalRecipients Nothing S.noSource (TargetSlot.required Pool.SpellsAndPermanents Nothing) gs))
      "the permanent is a legal object target"
  Spec.it s "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (permId, base) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
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
    let (wallId, base) = S.addPermanent wallOfStone S.bob (Setup.emptyGame S.bothPlayers)
        (pikerId, gs) = S.addPermanent piker S.alice base
        slot = SlotName.MkSlotName (Text.pack "target")
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing False Map.empty S.noSource (Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
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
    let (wallId, g1) = S.addPermanent wallOfStone S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addPermanent ashaya S.alice g1
        (_, gs) = S.addPermanent bloodMoon S.alice g2
        slot = SlotName.MkSlotName (Text.pack "target")
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing False Map.empty S.noSource (Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
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
        (mine, gs1) = S.addPermanent piker S.alice gs0
        (theirs, gs2) = S.addPermanent warMammoth S.bob gs1
        legal = Target.legalRecipients (Just S.alice) mine (TargetSlot.required Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs2
    Spec.assertEqWith s "only the opponent's creature" legal (Set.singleton (Recipient.ToCreature theirs))
    Spec.assertBool s (not (Set.member (Recipient.ToCreature mine) legal)) "not the source's controller's own"
  -- CR 115.1 / 109.5: "target OPPONENT". Until Ravenous Rats there was no
  -- card in the pool that narrowed a PLAYER target, so Target.legalRecipients
  -- kept every player unconditionally (#168). Three seats, so "an opponent"
  -- is a real set rather than the only other player.
  Spec.it s "CR 115.1 a Players pool narrowed by IsPlayer Opponent excludes the source's controller" $ do
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let (src, gs) = S.addPermanent ravenousRats S.alice (Setup.emptyGame S.threePlayers)
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
    let (src, gs) = S.addPermanent ravenousRats S.bob (Setup.emptyGame S.threePlayers)
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
        (mine, gs1) = S.addPermanent piker S.alice gs0
        (bobs, gs2) = S.addPermanent warMammoth S.bob gs1
        (carols, gs3) = S.addPermanent piker S.carol gs2
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
        (mine, gs1) = S.addPermanent piker S.alice gs0
        (theirs, gs2) = S.addPermanent warMammoth S.bob gs1
        (alsoTheirs, gs3) = S.addPermanent typhoidRats S.bob gs2
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
        (blackOid, gs1) = S.addPermanent typhoidRats S.bob gs0
        (plainOid, gs) = S.addPermanent piker S.alice gs1
        theSlot = TargetSlot.required Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))
        legal = Target.legalRecipients Nothing S.noSource theSlot gs
    Spec.assertBool s (not (Set.member (Recipient.ToCreature blackOid) legal)) "black creature illegal"
    Spec.assertBool s (Set.member (Recipient.ToCreature plainOid) legal) "nonblack creature legal"
  Spec.it s "P9 Creatures + Nothing narrows nothing" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (blackOid, gs1) = S.addPermanent typhoidRats S.bob gs0
        (plainOid, gs) = S.addPermanent piker S.alice gs1
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
        (srcId, gs1) = S.addPermanent piker S.alice gs0
        (otherId, gs) = S.addPermanent piker S.alice gs1
        slot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton slot (TargetSlot.required Pool.Creatures (Just (Filter.Type.Not Filter.Type.IsSource)))
    Spec.assertEqWith
      s
      "source excluded from its own set"
      (Target.legalSets Nothing False Map.empty srcId slots gs)
      (Map.singleton slot (Set.singleton (Recipient.ToCreature otherId)))
  -- The other half of the same claim: a slot carrying no Not IsSource does
  -- not exclude, so Prodigal Sorcerer may still ping itself (CR 115.4).
  Spec.it s "a slot without Not IsSource still admits the source (CR 115.4)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs) = S.addPermanent piker S.alice gs0
        slot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton slot (TargetSlot.required Pool.Creatures Nothing)
    Spec.assertEqWith
      s
      "source is its own legal target"
      (Target.legalSets Nothing False Map.empty srcId slots gs)
      (Map.singleton slot (Set.singleton (Recipient.ToCreature srcId)))
  -- Gate cards for P9 Task 5: Terror and Reprisal. Both cards' printed text ends
  -- "It can't be regenerated.", which both card files carry as
  -- CantBeRegenerated; CR 701.19c's half is proved by Pawl.ReplacementSpec's
  -- "CR 701.19c a shield does not save a creature from a destruction that
  -- forbids regeneration", not here. The cases below are about the TARGET
  -- filters only.
  Spec.it s "Terror: And of Not(HasColor Black) and Not(HasCardType Artifact) excludes black and artifact creatures" $ do
    terror <- S.printingOf s registry "Terror"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot terror of
      Nothing -> Spec.assertFailure s "Terror's printing carries no 'target' slot"
      Just theSlot -> do
        let gs0 = Setup.emptyGame S.bothPlayers
            (blackOid, gs1) = S.addPermanent typhoidRats S.bob gs0
            (artifactOid, gs2) = S.addPermanent darksteelMyr S.bob gs1
            (plainOid, gs) = S.addPermanent piker S.alice gs2
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
            (smallOid, gs) = S.addPermanent piker S.bob gs0 -- power 2, {1}{R}
            legalBefore = Target.legalRecipients Nothing S.noSource theSlot gs
            pumped = S.withEffect smallOid (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0))) gs
            legalAfter = Target.legalRecipients Nothing S.noSource theSlot pumped
        Spec.assertBool s (not (Set.member (Recipient.ToCreature smallOid) legalBefore)) "power 2 is illegal (below the PowerAtLeast 4 floor)"
        Spec.assertBool s (Set.member (Recipient.ToCreature smallOid) legalAfter) "pumped to power 4 becomes legal"

resolveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
resolveSpec s registry = Spec.describe s "Resolve" $ do
  Spec.it s "CR 608 a resolved spell's damage is Noncombat" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (_target, gs0) = S.addPermanent piker S.bob base
        (gs1, spellId) = S.handOne lightningBolt gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice spellId))
        -- resolveTop applies the damage but does NOT run SBAs, so the event persists.
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "the Bolt's damage event is Noncombat"
      (fmap DamageEvent.kind (S.damageEventsOf resolved))
      [DamageKind.Noncombat]
  Spec.it s "CR 608.2n the resolved Bolt is in its owner's graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one card" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
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
  -- The card lint's READ side for CR 106.4's recipient: Shizuko, Caller of
  -- Autumn's "that player" is a slot read, so a payload naming a slot no
  -- condition binds must look dangling. Asserted here because the pool cannot
  -- observe it -- Shizuko's own slot IS bound, so the lint passes either way and
  -- only a card written wrong would notice. A regression fence, not a proof of
  -- behaviour the pool exercises.
  Spec.it s "CR 106.4 slotsOf finds the slot an AddMana recipient names" $ do
    let slot = SlotName.MkSlotName (Text.pack "thatPlayer")
    Spec.assertEqWith s "a named recipient is a read" (Resolve.slotsOf (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.InSlot slot) ManaProduction.AnyColor (Quantity.Literal 1) ManaRetention.Ordinary Nothing Nothing))) (Map.singleton slot SlotArity.One)
    Spec.assertEqWith s "and CR 109.5's unwritten one names no slot" (Resolve.slotsOf (Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) ManaProduction.AnyColor (Quantity.Literal 1) ManaRetention.Ordinary Nothing Nothing))) Map.empty
  -- The same fence for CR 509.4's blocking rider, which the MoveToZone arm reads
  -- since Aetherplasm. Asserted here because the POOL cannot observe it: the
  -- dataflow lint's equality has Mode.targetSlots on its right, which is empty
  -- for a triggered ability, and Aetherplasm's slot is a trigger binding, so it
  -- sits on the bound side whether or not slotsOf reports the read. Dropping
  -- `riderSlots` from that arm changes nothing else in the suite -- the same
  -- under-reporting the BecomeMonarch arm had (#1040) -- so this is a regression
  -- fence rather than behaviour the pool proves.
  --
  -- The two arities differ on purpose: what a MoveToZone MOVES is read at Many,
  -- a targeted slot naming every recipient CR 608.2b left legal, while CR 509.4
  -- names one attacking creature.
  Spec.it s "CR 509.4 slotsOf finds the slot a MoveToZone's blocking rider names" $ do
    let slot = SlotName.MkSlotName (Text.pack "thatAttacker")
        moved = SlotName.MkSlotName (Text.pack "self")
        move riders = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot moved) Zone.Battlefield riders Nothing Nothing LibraryPlacement.defaultValue Nothing)
    Spec.assertEqWith s "the rider is a read, beside the ref's own" (Resolve.slotsOf (move EntryRiders.defaultValue {EntryRiders.blocking = Just (EntryBlock.Specified slot)})) (Map.fromList [(moved, SlotArity.Many), (slot, SlotArity.One)])
    Spec.assertEqWith s "and a move stating no attacker names only what it moves" (Resolve.slotsOf (move EntryRiders.defaultValue)) (Map.singleton moved SlotArity.Many)
    Spec.assertEqWith s "as does one whose controller chooses the attacker" (Resolve.slotsOf (move EntryRiders.defaultValue {EntryRiders.blocking = Just EntryBlock.Chosen})) (Map.singleton moved SlotArity.Many)
  Spec.it s "CR 605 manaProduced reads AddMana whole, and nothing else" $ do
    let plain = ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) (Quantity.Literal 1) ManaRetention.Ordinary Nothing Nothing
        anyColor = ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) ManaProduction.AnyColor (Quantity.Literal 1) ManaRetention.Ordinary Nothing Nothing
    Spec.assertEqWith s "add mana" (ManaAbility.manaProduced (Effect.AddMana plain)) (Just plain)
    Spec.assertEqWith s "add mana of any color" (ManaAbility.manaProduced (Effect.AddMana anyColor)) (Just anyColor)
    -- The WHOLE instruction and not its ManaProduction alone, which is what CR
    -- 605.3b's inline payment needs to stamp CR 106.6's restriction onto the
    -- units it adds (Mana.manaOptionsOfGiven). Mishra's Workshop is the
    -- printing, and Pawl.ManaSpec's group of that name is where it is paid.
    let restricted = ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) ManaProduction.AnyColor (Quantity.Literal 1) ManaRetention.Ordinary (Just (ManaRestriction.onlyCasts (Filter.Type.HasCardType CardType.Artifact))) Nothing
    Spec.assertEqWith s "a spending restriction rides along" (ManaAbility.manaProduced (Effect.AddMana restricted)) (Just restricted)
    -- CR 605.1a asks whether the ability could add mana to "a player's" pool, so a
    -- recipient the card names is carried rather than disqualifying: an ability
    -- that adds to somebody else is still a mana ability. The payment path
    -- resolves that reference through Mana.recipientsOf, over the slots the
    -- ability's own earlier effects bound (Pawl.ManaSpec's Valleymaker group).
    let named = ManaAddition.MkManaAddition (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) ManaProduction.AnyColor (Quantity.Literal 1) ManaRetention.Ordinary Nothing Nothing
    Spec.assertEqWith s "a named recipient does not disqualify" (ManaAbility.manaProduced (Effect.AddMana named)) (Just named)
    Spec.assertEqWith s "damage produces no mana" (ManaAbility.manaProduced (Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "x"))) (Quantity.Literal 1))) Nothing Nothing))) Nothing
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
    -- Artificial Evolution (Pawl.CounterspellSpec's ArtificialEvolution group), and
    -- Pawl.ActivateSpec's Tidal Warrior chain reaches the same
    -- Projection.rewriteEffect ModifyTarget arm through an ACTIVATED ability.
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    boil <- S.printingOf s registry "Boil"
    let base = Setup.emptyGame S.bothPlayers
        (islandId, g1) = S.addPermanent island S.alice base
        (forestId, g2) = S.addPermanent forest S.alice g1
        (boilPrintingId, g2b) = Game.intern boil g2
        (boilId, g3) = Game.freshObjectId g2b
        boilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfCard boilPrintingId,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.flipped = False,
              Object.exiledFaceDown = False,
              Object.exileLookers = Set.empty,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.controlClock = Map.empty,
              -- CR 700.2: Boil has one mode, and a directly-built stack object
              -- (bypassing Cast.castSpell) must stamp it chosen (mode 0), or
              -- Resolve.modesOf and Resolve.targetSlotsOf -- both scoped to the
              -- CHOSEN modes through Binding.modesOf -- would see no effects and
              -- no target slots at all.
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.foretellCostReduction = Nothing,
              Object.warped = Nothing,
              Object.preparedCopyOf = Nothing,
              Object.ringBearerFor = Nothing,
              Object.duplicate = Nothing,
              Object.paired = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.designationValues = Map.empty,
              Object.paidCosts = Map.empty,
              Object.tributePaid = False,
              Object.bestowed = False,
              Object.mutating = False,
              Object.prototyped = False,
              Object.boughtBack = False,
              Object.spliced = Seq.empty,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.castFrom = Nothing,
              Object.castUsing = Nothing,
              Object.castGrant = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapFor = 0,
              Object.exertedBy = Set.empty,
              Object.activatedOnce = Map.empty
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
        (nonbasicId, g1) = S.addPermanent urborg S.alice base
        (bloodMoonPrintingId, g1b) = Game.intern bloodMoon g1
        (bloodMoonSpellId, g2) = Game.freshObjectId g1b
        bmObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfCard bloodMoonPrintingId,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.flipped = False,
              Object.exiledFaceDown = False,
              Object.exileLookers = Set.empty,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.controlClock = Map.empty,
              Object.bindings = Map.empty,
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.foretellCostReduction = Nothing,
              Object.warped = Nothing,
              Object.preparedCopyOf = Nothing,
              Object.ringBearerFor = Nothing,
              Object.duplicate = Nothing,
              Object.paired = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.designationValues = Map.empty,
              Object.paidCosts = Map.empty,
              Object.tributePaid = False,
              Object.bestowed = False,
              Object.mutating = False,
              Object.prototyped = False,
              Object.boughtBack = False,
              Object.spliced = Seq.empty,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.castFrom = Nothing,
              Object.castUsing = Nothing,
              Object.castGrant = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapFor = 0,
              Object.exertedBy = Set.empty,
              Object.activatedOnce = Map.empty
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
    -- Island. Event.carryOver, which the move calls, is what re-keys the stored
    -- effect.
    Spec.assertEqWith s "hack carried over: nonbasic land is Island" (Projection.subtypesOf nonbasicId after) (Set.singleton Subtype.Island)
  Spec.it s "CR 608.2n a resolving ability deals its damage and ceases" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addPermanent prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        ability = case Face.activatedAbilities (S.combinedFace prodigalSorcerer) of
          ab : _ -> ab
          [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] 0 (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Activator.Controller Nothing Nothing Nothing
        (abilId, g1) = Game.freshObjectId g0
        (ts, g2) = Game.freshTimestamp g1
        slot = SlotName.MkSlotName (Text.pack "target")
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfAbility
                  ActivatedAbilitySource.MkActivatedAbilitySource
                    { ActivatedAbilitySource.source = srcId,
                      ActivatedAbilitySource.ability = ability
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.flipped = False,
              Object.exiledFaceDown = False,
              Object.exileLookers = Set.empty,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.controlClock = Map.empty,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.bob))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.foretellCostReduction = Nothing,
              Object.warped = Nothing,
              Object.preparedCopyOf = Nothing,
              Object.ringBearerFor = Nothing,
              Object.duplicate = Nothing,
              Object.paired = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.designationValues = Map.empty,
              Object.paidCosts = Map.empty,
              Object.tributePaid = False,
              Object.bestowed = False,
              Object.mutating = False,
              Object.prototyped = False,
              Object.boughtBack = False,
              Object.spliced = Seq.empty,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.castFrom = Nothing,
              Object.castUsing = Nothing,
              Object.castGrant = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapFor = 0,
              Object.exertedBy = Set.empty,
              Object.activatedOnce = Map.empty
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
            []
            0
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.outsideTheGame = False, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped, Search.subject = Nothing, Search.slot = Nothing, Search.differentIn = Set.empty}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Activator.Controller
            Nothing
            Nothing
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False False Set.empty 0 (Sickness.Settled S.alice) Map.empty (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty Map.empty Map.empty False False False False False Seq.empty 0 (Mana.MkMana []) Nothing Nothing Nothing Nothing Set.empty Set.empty 0 Set.empty Map.empty Nothing Nothing
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
    Spec.assertEqWith s "one permanent on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice resolved)) 1
    Spec.assertEqWith s "it is tapped" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice resolved) []
  Spec.it s "CR 701.23b Search may fail to find" $ do
    mountain <- S.printingOf s registry "Mountain"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addLibraryCard mountain S.alice base
        ability = ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] 0 (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.outsideTheGame = False, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped, Search.subject = Nothing, Search.slot = Nothing, Search.differentIn = Set.empty}]))) Map.empty)) (ModeSelection.ChooseExactly 1)) [] Activator.Controller Nothing Nothing Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj = Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False False Set.empty 0 (Sickness.Settled S.alice) Map.empty (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty Map.empty Map.empty False False False False False Seq.empty 0 (Mana.MkMana []) Nothing Nothing Nothing Nothing Set.empty Set.empty 0 Set.empty Map.empty Nothing Nothing
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findNothing g4 Stack.resolveTop)
    Spec.assertEqWith s "nothing entered the battlefield" (GameState.battlefield resolved) Set.empty
  Spec.it s "CR 701.23a Search (And [HasCardType Land, HasSupertype Basic]) offers a basic land, not a nonland" $ do
    -- P9: the Search filter reads each library card through its own CR 613
    -- projection (Projection.viewOfObject) -- the card is an object and has one
    -- there like any other. For a card no continuous effect reaches, that view
    -- is the printed card.
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
            []
            0
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.outsideTheGame = False, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped, Search.subject = Nothing, Search.slot = Nothing, Search.differentIn = Set.empty}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Activator.Controller
            Nothing
            Nothing
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False False Set.empty 0 (Sickness.Settled S.alice) Map.empty (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty Map.empty Map.empty False False False False False Seq.empty 0 (Mana.MkMana []) Nothing Nothing Nothing Nothing Set.empty Set.empty 0 Set.empty Map.empty Nothing Nothing
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
            []
            0
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.outsideTheGame = False, Search.quantity = Just (Quantity.Literal 1), Search.filter = basicLandFilter, Search.upTo = False, Search.destination = SearchDestination.BattlefieldTapped, Search.subject = Nothing, Search.slot = Nothing, Search.differentIn = Set.empty}]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            []
            Activator.Controller
            Nothing
            Nothing
            Nothing
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = ObjectId.MkObjectId 0, ActivatedAbilitySource.ability = ability}) Zone.Stack TapState.Untapped Facing.FaceUp False False Set.empty 0 (Sickness.Settled S.alice) Map.empty (Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))) Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty Map.empty Map.empty False False False False False Seq.empty 0 (Mana.MkMana []) Nothing Nothing Nothing Nothing Set.empty Set.empty 0 Set.empty Map.empty Nothing Nothing
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure (findForbidden pikerId) g4 Stack.resolveTop)
    Spec.assertEqWith s "the Piker was NOT fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice resolved) 0
    Spec.assertBool s (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)) "it is still in the library"
    Spec.assertEqWith s "and nothing else was fetched either" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Mountain") S.alice resolved) 0
  -- Hoarding Dragon -- "Flying. When this creature enters, you may search your
  -- library for an artifact card, exile it, then shuffle." The whole-card proof
  -- of SearchDestination.Exile, cast and resolved rather than assembled.
  --
  -- The printed card's second half -- "When this creature dies, you may put the
  -- exiled card into its owner's hand" -- is CR 607.2a's linked ability, and the
  -- two cases at the end of this group are what prove the link picks out the
  -- right card.
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
  -- CR 608.2d, the half that says an option has to be a real one: "the player
  -- can't choose an option that's illegal or impossible". The SAME Tweeze off a
  -- board where alice's hand is empty once the spell is on the stack, so "you
  -- may discard a card" cannot be carried out at all -- and the draw its "If you
  -- do" hangs on was a card pawl handed her for nothing.
  --
  -- The answerer takes every "may" it is offered, so the empty transcript is
  -- what says none was offered, and the hand and the library are the same fact
  -- read off the board. Not an inert clause (Resolve.clauseIsInert): the `you`
  -- slot the discard reads is bound and alive, which is why this needed a
  -- second gate rather than a wider first one.
  Spec.it s "CR 608.2d Tweeze's discard is not offered with an empty hand" $ do
    (gs, tweezeId) <- emptyHandedTweezeBoard s registry
    let ((_, after), asked) = Replay.record (tweezeAnswer OptionalDecision.Exercises tweezeId) gs (S.cast S.alice tweezeId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 608.2d nothing was discarded and so nothing was drawn: alice's hand is empty" (namesIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "and her library still holds all three Pikers" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "CR 603.5's \"may\" was never put" (optionalsAnswered asked) []
    Spec.assertEqWith s "the control: the mandatory first clause still happened, so bob took the 3 damage" (S.lifeOf S.bob after) (Just 17)
  -- The same negative through CR 118.12a: Development ({3}{U}{R} Instant, the
  -- right half of Research // Development: "Create a 3/1 red Elemental creature
  -- token unless any opponent has you draw a card. Repeat this process two more
  -- times."; card_faces at api.scryfall.com 2026-09-27) reads as three pairs of
  -- "any opponent may have you draw a card. If no one does, create the token."
  -- Carol takes the first offer, bob the second, nobody the third, so one token
  -- and two cards -- the Scryfall ruling's "a different opponent may let you draw
  -- a card each time". alice takes every "may" put to her: she is no opponent,
  -- so an offer made to her would show as a third draw.
  Spec.it s "CR 118.12a Development makes a token only for the offer no opponent took" $ do
    (gs, spellId) <- developmentBoard s registry
    let takes pid cIdx = pid == S.alice || (pid, cIdx) `elem` [(S.carol, ClauseIndex.MkClauseIndex 0), (S.bob, ClauseIndex.MkClauseIndex 2)]
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseOptional _ pid _ _ cIdx _ -> if takes pid cIdx then OptionalDecision.Exercises else OptionalDecision.Declines
          _ -> S.identityAnswer p
        after = S.runPure answer gs (Cast.castSpell S.manaPerformer S.alice spellId (CardName.MkCardName (Text.pack "Development")) Facing.FaceUp >> Stack.resolveTop)
        piker = Just (CardName.MkCardName (Text.pack "Goblin Piker"))
    Spec.assertEqWith s "CR 118.12a one Elemental, for the third offer" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Elemental Token")) S.alice after) 1
    Spec.assertEqWith s "and alice drew one card for each offer taken" (namesIn Zone.Hand S.alice after) [piker, piker]
  -- CR 608.2d, one opcode over: Excavating Anurid -- "When
  -- this creature enters, you may sacrifice a land. If you do, draw a card." --
  -- entering under a controller who controls no land. Two boards differing in
  -- exactly that, and an answerer that takes every "may" it is offered, so the
  -- empty transcript is what says none was put and the hand is the same fact
  -- read off the board.
  --
  -- The hand comes FIRST because it is the rider CR 608.2d is about: an engine
  -- that offered the impossible sacrifice would hand alice the free card its "If
  -- you do" hangs on, and every other assertion here would still hold.
  Spec.it s "CR 608.2d Excavating Anurid's sacrifice is not offered to a landless controller" $ do
    (gs, fodder) <- anuridBoard s registry False
    let (asked, after) = enteredAndResolved (anuridAnswer fodder) gs
    Spec.assertEqWith s "CR 608.2d nothing was sacrificed and so nothing was drawn: alice's hand is empty" (namesIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "and her library still holds all three Pikers" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "nothing reached her graveyard either" (namesIn Zone.Graveyard S.alice after) []
    Spec.assertEqWith s "CR 603.5's \"may\" was never put" (optionalsAnswered asked) []
  -- The control, the same board plus the two lands: the option is a real one, so
  -- it is offered, the pinned Mountain goes and the rider draws.
  Spec.it s "CR 608.2d Excavating Anurid's sacrifice is offered when a land can go" $ do
    (gs, fodder) <- anuridBoard s registry True
    let (asked, after) = enteredAndResolved (anuridAnswer fodder) gs
        nameOf = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "CR 608.2c the rider ran: the drawn Piker is in alice's hand" (namesIn Zone.Hand S.alice after) [nameOf "Goblin Piker"]
    Spec.assertEqWith s "and her library is one card shorter" (length (Game.zoneMembers Zone.Library S.alice after)) 2
    Spec.assertEqWith s "the land she was asked for, and only it, is in her graveyard" (namesIn Zone.Graveyard S.alice after) [nameOf "Mountain"]
    Spec.assertEqWith s "the Forest she was offered instead is still on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Forest")) S.alice after) 1
    Spec.assertEqWith s "CR 603.5's \"may\" was asked once" (optionalsAnswered asked) [OptionalDecision.Exercises]
  -- CR 701.17b's own second sentence: "a player can't mill a number of cards
  -- greater than the number of cards in their library. If given the choice to do
  -- so, they can't choose to take that action." Mineshaft Spider -- "When this
  -- creature enters, you may mill two cards." -- over a library holding ONE card,
  -- which is the board that separates rule 701.17b from the empty-library reading
  -- it would share with the other opcodes, and from CR 121.3's carve-out, which
  -- is for drawing alone.
  --
  -- The graveyard comes first for the reason the Anurid's hand does: an engine
  -- offering the option would mill as many as possible (CR 701.17b) and bury that
  -- one card, which nothing else here would notice.
  Spec.it s "CR 701.17b Mineshaft Spider's mill is not offered over a one-card library" $ do
    gs <- spiderBoard s registry 1
    let (asked, after) = enteredAndResolved spiderAnswer gs
    Spec.assertEqWith s "CR 701.17b the card was not milled: alice's graveyard is empty" (namesIn Zone.Graveyard S.alice after) []
    Spec.assertEqWith s "and her library still holds it" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "CR 603.5's \"may\" was never put" (optionalsAnswered asked) []
  -- The control, the same board with a library deep enough to pay for it.
  Spec.it s "CR 701.17b Mineshaft Spider's mill is offered over a three-card library" $ do
    gs <- spiderBoard s registry 3
    let (asked, after) = enteredAndResolved spiderAnswer gs
        nameOf = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "CR 701.17a both cards were milled" (namesIn Zone.Graveyard S.alice after) [nameOf "Goblin Piker", nameOf "Goblin Piker"]
    Spec.assertEqWith s "and the third is still in her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "CR 603.5's \"may\" was asked once" (optionalsAnswered asked) [OptionalDecision.Exercises]
  -- CR 608.2d with a COUNT: Thrilling Discovery -- "You gain 2 life. Then you
  -- may discard two cards. If you do, draw three cards." -- cast by alice holding
  -- ONE other card. Discarding two is an option she cannot carry out, so it is
  -- not an option at all (The Mimeoplasm's 2011-09-22 ruling, "You can't choose
  -- to exile just one creature card"), where CR 609.3's "as much as possible"
  -- would have traded her one card for three.
  --
  -- The hand comes first because it is the rider the rule is about.
  Spec.it s "CR 608.2d Thrilling Discovery's discard of two is not offered over a one-card hand" $ do
    (gs, spellId, _) <- discoveryBoard s registry 1
    let ((_, after), asked) = Replay.record (discoveryAnswer []) gs (S.cast S.alice spellId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 608.2d nothing was discarded and so nothing was drawn: alice holds her one card" (namesIn Zone.Hand S.alice after) [Just (CardName.MkCardName (Text.pack "Bird Maiden"))]
    Spec.assertEqWith s "and her library still holds all four Pikers" (length (Game.zoneMembers Zone.Library S.alice after)) 4
    Spec.assertEqWith s "CR 603.5's \"may\" was never put" (optionalsAnswered asked) []
    Spec.assertEqWith s "the control: the mandatory first clause still happened" (S.lifeOf S.alice after) (Just 22)
  -- The control, the same board with three cards in hand: two can go, so the
  -- option is a real one, the pinned pair is discarded and the rider draws.
  Spec.it s "CR 608.2d Thrilling Discovery's discard of two is offered over a three-card hand" $ do
    (gs, spellId, held) <- discoveryBoard s registry 3
    let pinned = take 2 held
        ((_, after), asked) = Replay.record (discoveryAnswer pinned) gs (S.cast S.alice spellId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 608.2c the rider ran: alice holds the card she kept and three Pikers" (length (Game.zoneMembers Zone.Hand S.alice after)) 4
    Spec.assertEqWith s "and her library is three cards shorter" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "CR 701.9a the pinned pair and the spell are in her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 3
    Spec.assertEqWith s "CR 603.5's \"may\" was asked once" (optionalsAnswered asked) [OptionalDecision.Exercises]
  -- The same judgement for a sacrifice, in CR 608.2d's other offer, the
  -- either-or: Giant Opportunity -- "You may sacrifice two Foods. If you do,
  -- create a 7/7 green Giant creature token. Otherwise, create three Food
  -- tokens." -- cast by alice controlling ONE Food. Sacrificing two is not a
  -- branch she can take, so the other is forced with no prompt; offering it would
  -- have traded one Golden Egg for a 7/7.
  Spec.it s "CR 608.2d Giant Opportunity's sacrifice of two Foods is not offered with one Food" $ do
    (gs, spellId, _) <- opportunityBoard s registry 1
    let ((_, after), asked) = Replay.record (opportunityAnswer []) gs (S.cast S.alice spellId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 608.2d no Giant was made" (S.countOnBattlefieldByName giantToken S.alice after) 0
    Spec.assertEqWith s "the Otherwise branch ran: three Food tokens" (S.countOnBattlefieldByName foodToken S.alice after) 3
    Spec.assertEqWith s "and the Golden Egg still stands" (S.countOnBattlefieldByName goldenEgg S.alice after) 1
    Spec.assertEqWith s "CR 608.2d the forced branch raised no announcement" (clausesAnswered asked) []
  -- The control, three Foods: both branches are real, the sacrifice is
  -- announced, the pinned pair goes and the Giant arrives in place of the Foods.
  Spec.it s "CR 608.2d Giant Opportunity's sacrifice of two Foods is offered with three Foods" $ do
    (gs, spellId, eggs) <- opportunityBoard s registry 3
    let ((_, after), asked) = Replay.record (opportunityAnswer (take 2 eggs)) gs (S.cast S.alice spellId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 608.2c the rider ran: one Giant" (S.countOnBattlefieldByName giantToken S.alice after) 1
    Spec.assertEqWith s "and the Otherwise branch did not: no Food token" (S.countOnBattlefieldByName foodToken S.alice after) 0
    Spec.assertEqWith s "CR 701.21a two of the three Golden Eggs went" (S.countOnBattlefieldByName goldenEgg S.alice after) 1
    Spec.assertEqWith s "CR 608.2d the branch was announced once" (clausesAnswered asked) [Just (ClauseIndex.MkClauseIndex 0)]
  -- CR 608.2f / 603.12: does Effect.ForEach's own body run through the SAME
  -- happened-fold a clause's instructions do, or does every body instruction
  -- run unconditionally regardless of whether the one before it did anything?
  -- Synthetic Communal Toll -- "When ~ enters, for each opponent, that player
  -- mills a card. When they do, you gain 1 life." -- on a three-seat board
  -- where bob's library holds a card and carol's is empty (CR 101.3: milling
  -- from an empty library mills nothing and records no event): bob's own
  -- instruction happened and carol's did not, so the reflexive should arm
  -- once, not twice. A ForEach that skipped this fold would arm it for both.
  Spec.it s "CR 608.2f / 603.12 a reflexive armed inside a ForEach reads only that member's own instruction" $ do
    toll <- S.printingOf s registry "Synthetic Communal Toll"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, stocked) = S.addLibraryCard piker S.bob S.threePlayerGame
        (_, entered) = S.entersWithTrigger toll S.alice stocked
        resolveEverything gs =
          let settled = S.runPure S.identityAnswer gs Engine.settleForPriority
           in if null (GameState.stack settled)
                then settled
                else resolveEverything (S.runPure S.identityAnswer settled Stack.resolveTop)
        after = resolveEverything entered
    Spec.assertEqWith s "bob's library, which had a card, is empty: his mill happened" (Game.zoneMembers Zone.Library S.bob after) []
    Spec.assertEqWith s "carol's library was already empty and stays that way" (Game.zoneMembers Zone.Library S.carol after) []
    Spec.assertEqWith s "CR 608.2f / 603.12 the reflexive armed once, off bob's mill alone: alice gained exactly 1 life" (S.lifeOf S.alice after) (fmap (+ 1) (S.lifeOf S.alice entered))
  -- CR 608.2d's either-or, on the three boards that tell its four readings
  -- apart: Twiddle -- "You may tap or untap target artifact, creature, or land"
  -- -- aimed at bob's Goblin Piker. One mode, two clauses naming each other
  -- (Clause.orElse) over ONE target slot, which is what makes this not Dream's
  -- Grip: that card prints two MODES and CR 601.2b fixes them as it is cast.
  --
  -- The first case is the load-bearing one. On an UNTAPPED Piker, tapping is the
  -- only reading that ends Tapped: both clauses running would tap then untap (CR
  -- 608.2c's written order), untapping does nothing to an untapped permanent,
  -- and a prompt never raised leaves it alone too. The other two cases separate
  -- the readings that first one cannot -- see each.
  --
  -- And CR 608.2d's own filter is what the answerer's UNTAP settles: a permanent
  -- is tapped or untapped and never both, so one branch of this pair is
  -- impossible on every board and the rules leave nothing to choose. The
  -- answerer names the branch that is NOT on offer, so an engine that asked
  -- anyway would take the untap and leave the Piker as it found it.
  Spec.it s "CR 608.2d an untapped Piker leaves Twiddle only its tap, and it is forced" $ do
    board <- twiddleBoard s registry False
    let (asked, after) = twiddleResolved (ClauseIndex.MkClauseIndex 1) OptionalDecision.Exercises board
    Spec.assertEqWith s "CR 608.2d the possible branch ran and its sibling did not: the Piker is tapped" (twiddleTapState (thirdOf board) after) (Just TapState.Tapped)
    Spec.assertEqWith s "and no branch question was put at all" (branchesAnnounced asked) []
    Spec.assertEqWith s "CR 603.5's \"may\" was asked once, for the branch that survived and not for the one that did not" (optionalsAnswered asked) [OptionalDecision.Exercises]
  -- The same rule the other way up, and the case that separates "took the untap"
  -- from "declined": on a TAPPED Piker only the untap ends Untapped, and here it
  -- is the tap the answerer names and CR 608.2d withholds.
  Spec.it s "CR 608.2d a tapped Piker leaves Twiddle only its untap" $ do
    board <- twiddleBoard s registry True
    let (asked, after) = twiddleResolved (ClauseIndex.MkClauseIndex 0) OptionalDecision.Exercises board
    Spec.assertEqWith s "CR 608.2d the possible branch ran: the Piker is untapped" (twiddleTapState (thirdOf board) after) (Just TapState.Untapped)
    Spec.assertEqWith s "and no branch question was put at all" (branchesAnnounced asked) []
    Spec.assertEqWith s "CR 603.5's \"may\" was asked once, for the surviving branch alone" (optionalsAnswered asked) [OptionalDecision.Exercises]
  -- CR 603.5 composed with CR 608.2d, on the FIRST case's board with exactly one
  -- answer changed: the "may" is declined, so the surviving tap does not happen.
  -- That pairing is what stops the first case passing because the clause ran
  -- unconditionally, and CR 603.5's offer is still made over a branch CR 608.2d
  -- forced -- one option settles which instruction, not whether to take it.
  Spec.it s "CR 603.5 declining Twiddle's \"may\" leaves the forced tap undone" $ do
    board <- twiddleBoard s registry False
    let (asked, after) = twiddleResolved (ClauseIndex.MkClauseIndex 0) OptionalDecision.Declines board
    Spec.assertEqWith s "CR 603.5 the declined clause did nothing: the Piker is still untapped" (twiddleTapState (thirdOf board) after) (Just TapState.Untapped)
    Spec.assertEqWith s "CR 608.2d and no branch question was put" (branchesAnnounced asked) []
    Spec.assertEqWith s "and the sibling's \"may\" was never offered as a second chance" (optionalsAnswered asked) [OptionalDecision.Declines]
  -- The SAME rider on the other resolution path. Pawl.Engine.Resolve keeps two
  -- hand-duplicated clause loops -- one for a spell, one for an activated or
  -- triggered ability (CR 113.7's separate source) -- and a Twiddle board reaches
  -- only the first, so mutating the second goes green on every case above. Teardrop
  -- Kami -- "Sacrifice this creature: You may tap or untap target creature" -- is
  -- the printed producer for the second, and the pair below is the first pair's
  -- discrimination argument transplanted onto it.
  Spec.it s "CR 608.2d an untapped Piker leaves Teardrop Kami only its tap" $ do
    (gs, ability, kamiId, pikerId) <- kamiBoard s registry False
    case ability of
      Nothing -> Spec.assertFailure s "Teardrop Kami should declare one activated ability"
      Just abil -> do
        let (asked, after) = kamiResolved (ClauseIndex.MkClauseIndex 1) OptionalDecision.Exercises gs abil kamiId pikerId
        Spec.assertEqWith s "CR 608.2d the possible branch ran and its sibling did not: the Piker is tapped" (twiddleTapState pikerId after) (Just TapState.Tapped)
        Spec.assertEqWith s "and no branch question was put at all" (branchesAnnounced asked) []
        Spec.assertEqWith s "CR 603.5's \"may\" was asked once, for the surviving branch alone" (optionalsAnswered asked) [OptionalDecision.Exercises]
  -- CR 701.55d, an exception to rule 608.2e: "if more than one player is
  -- instructed to face a villainous choice, the entire process described in rule
  -- 701.55a is performed for each of those players one at a time in APNAP
  -- order". The Dalek Emperor -- "each opponent faces a villainous choice --
  -- that player sacrifices a creature of their choice, or you create a 3/3 black
  -- Dalek artifact creature token with menace" -- is the producer, and its
  -- beginning of combat trigger puts the pair on rule 608.2e's OTHER resolution
  -- loop, the one Great Intelligence's Plan never reaches.
  --
  -- THE PAIR OF CASES IS THE ARGUMENT. Running each limb once for the set of
  -- seats that announced it -- CR 608.2e's shape, which rule 701.55d carves out
  -- of -- makes ONE token however many opponents asked for one, and hands the
  -- sacrifice to every announcer at once or to nobody. The first case below
  -- counts the tokens and the second reads which seat actually lost a creature.
  Spec.it s "CR 701.55d two opponents each taking The Dalek Emperor's token limb make two tokens" $ do
    board <- emperorBoard s registry
    let (asks, after) = emperorCombat (Map.fromList [(S.bob, ClauseIndex.MkClauseIndex 1), (S.carol, ClauseIndex.MkClauseIndex 1)]) board
    Spec.assertEqWith s "CR 701.55d the whole process ran once per opponent, so alice has TWO Dalek tokens" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Dalek Token")) S.alice after) 2
    Spec.assertEqWith s "CR 701.55a and the limb neither of them took did nothing: bob keeps both Pikers" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.bob after) 2
    Spec.assertEqWith s "nor did carol lose a Construct" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Bonded Construct")) S.carol after) 2
    Spec.assertEqWith s "CR 101.4 both opponents were asked, in APNAP order, over both limbs" asks [(S.bob, [ClauseIndex.MkClauseIndex 0, ClauseIndex.MkClauseIndex 1]), (S.carol, [ClauseIndex.MkClauseIndex 0, ClauseIndex.MkClauseIndex 1])]
  -- The same board with bob's answer changed, which is what stops the case above
  -- passing because the sacrifice limb can never run: "that player" is the seat
  -- FACING the choice (Binding.facingPlayers), so bob's Piker goes and carol,
  -- who took the other limb, keeps both Constructs.
  Spec.it s "CR 701.55d only the opponent who took The Dalek Emperor's sacrifice limb loses a creature" $ do
    board <- emperorBoard s registry
    let (_, after) = emperorCombat (Map.fromList [(S.bob, ClauseIndex.MkClauseIndex 0), (S.carol, ClauseIndex.MkClauseIndex 1)]) board
    Spec.assertEqWith s "CR 701.55a bob announced the sacrifice, so one of his two Pikers is gone" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.bob after) 1
    Spec.assertEqWith s "and carol, who announced the other limb, keeps both Constructs" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Bonded Construct")) S.carol after) 2
    Spec.assertEqWith s "CR 701.55a carol's limb ran once and bob's did not, so alice has exactly one token" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Dalek Token")) S.alice after) 1
    Spec.assertEqWith s "and alice, whose creature nobody was facing a choice about, still has the Emperor" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "The Dalek Emperor")) S.alice after) 1
  -- CR 701.55c: "a replacement effect may replace an instruction to face a
  -- villainous choice with an instruction to face that choice some number of
  -- additional times", each performed "one at a time". The Valeyard -- "If an
  -- opponent would face a villainous choice, they face that choice an
  -- additional time." -- is the producer, on the Emperor's board with BOB
  -- controlling it: carol is bob's opponent and faces the choice twice, while
  -- bob, facing alice's Emperor too, is not his own opponent and faces it once.
  --
  -- Each seat's answers are a QUEUE, so the two facings get different answers:
  -- carol sacrifices the first time and takes the token the second, and bob's
  -- queue holds a sacrifice behind his token that only a wrong second facing
  -- would reach, sacrificing one of his three creatures.
  Spec.it s "CR 701.55c The Valeyard: an opponent faces the choice twice" $ do
    board <- valeyardBoard s registry True
    let after = valeyardCombat board
    Spec.assertEqWith s "CR 701.55c carol's second facing ran: alice has a token from each of bob's one facing and carol's second" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Dalek Token")) S.alice after) 2
    Spec.assertEqWith s "CR 701.55a and carol's first facing, a different choice, ran too: one Construct is gone" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Bonded Construct")) S.carol after) 1
    Spec.assertEqWith s "The Valeyard reads only its controller's opponents, so bob faced the choice once and keeps all three creatures" (length (Game.zoneMembers Zone.Battlefield S.bob after)) 3
  -- The same board and answers without The Valeyard: carol faces the choice
  -- once, her queue's first answer, so no token of hers is made.
  Spec.it s "CR 701.55a without The Valeyard carol faces the choice once" $ do
    board <- valeyardBoard s registry False
    Spec.assertEqWith s "CR 701.55a only bob's token was made" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Dalek Token")) S.alice (valeyardCombat board)) 1
  Spec.it s "CR 608.2d a tapped Piker leaves Teardrop Kami only its untap" $ do
    (gs, ability, kamiId, pikerId) <- kamiBoard s registry True
    case ability of
      Nothing -> Spec.assertFailure s "Teardrop Kami should declare one activated ability"
      Just abil -> do
        let (asked, after) = kamiResolved (ClauseIndex.MkClauseIndex 0) OptionalDecision.Exercises gs abil kamiId pikerId
        Spec.assertEqWith s "CR 608.2d the possible branch ran: the Piker is untapped" (twiddleTapState pikerId after) (Just TapState.Untapped)
        Spec.assertEqWith s "and no branch question was put at all" (branchesAnnounced asked) []
  -- CR 607.2a's linked set, on the board that can tell it from "every card in
  -- exile": TWO Hoarding Dragons, each of which exiled a different artifact, and
  -- one of them dies. The pair below runs the SAME board twice and differs in
  -- exactly one thing -- which Dragon takes the lethal damage -- so an engine
  -- whose "the exiled card" named all of exile, or the oldest entry, or the
  -- newest, fails one of the two.
  --
  -- Two objects of ONE NAME rather than two different exilers, because that is
  -- the reading CR 607.2a singles out: the link is per OBJECT, and a second copy
  -- of the same printing is a different object with its own set.
  --
  -- Each search is PINNED to a named card rather than taking the head of the
  -- offered list, so which Dragon holds which artifact is decided by the fixture
  -- and not by where a shuffle left the library.
  --
  -- The kill is marked damage plus CR 704.5g, which is a LEAVE-THE-BATTLEFIELD
  -- event: CR 603.10a makes the dies trigger look back, so its source is the
  -- permanent as it was on the battlefield -- the same id that did the exiling,
  -- and the whole reason the link survives its own object's death.
  Spec.it s "CR 607.2a: the dead Dragon returns the card IT exiled, not the other Dragon's" $ do
    board <- twoDragonBoard s registry
    Spec.assertEqWith s "the first Dragon's artifact came back to her hand" (handNames (kill (firstDragon board) board)) [firstArtifact board]
    Spec.assertEqWith s "and the surviving Dragon's is still in exile" (exileNames (kill (firstDragon board) board)) [secondArtifact board]
  Spec.it s "CR 607.2a: killing the OTHER Dragon returns the OTHER card" $ do
    board <- twoDragonBoard s registry
    Spec.assertEqWith s "the second Dragon's artifact came back to her hand" (handNames (kill (secondDragon board) board)) [secondArtifact board]
    Spec.assertEqWith s "and the first Dragon's is still in exile" (exileNames (kill (secondDragon board) board)) [firstArtifact board]
  -- Synthetic Split Reliquary -- "{3} Artifact. Warden -- {T}: Exile target
  -- creature card from your graveyard. Scholar -- {T}: Exile target land card
  -- from your graveyard. {T}, Sacrifice this artifact: Return the cards exiled
  -- with its warden ability to your hand." SYNTHETIC because no printing links a
  -- reference to one of two exiling abilities of one object (MTGJSON 2026-08-23,
  -- `exiled with [^.]*ability`: Soulflayer's delve, its only exiler). One object
  -- exiled both cards, so only CR 607.2a's per-ABILITY link keeps the land out.
  Spec.it s "CR 607.2a Synthetic Split Reliquary returns only what its warden ability exiled" $ do
    settled <- splitReliquary s registry
    -- The gameplay assertion, and first: an object-keyed link returns the Forest
    -- too, and a link that lost the name returns nothing.
    Spec.assertEqWith s "the warden ability's Piker came back to her hand, and nothing else" (handNames settled) [CardName.MkCardName (Text.pack "Goblin Piker")]
    Spec.assertEqWith s "the scholar ability's Forest is still in exile" (exileNames settled) [CardName.MkCardName (Text.pack "Forest")]
  -- Nature's Lore -- "Search your library for a Forest card, put that card onto
  -- the battlefield, then shuffle." The whole-card proof of
  -- SearchDestination.Battlefield, cast and resolved rather than assembled. Its
  -- whole printed text is expressible, so nothing about pawl's copy runs weaker
  -- than the card.
  --
  -- The PAIR to Explosive Vegetation above: the two destinations differ in the
  -- one word "tapped", and the assertions differ in exactly the same place.
  -- Those cases assert NOTHING is untapped after the fetch; this one asserts
  -- exactly one thing is, and that it is what the search found. CR 110.5b is the
  -- rule that makes it so -- this card's sentence names no tap state, so the
  -- entry defaults stand.
  --
  -- Both Forests she started with paid the {1}{G}, so an untapped permanent can
  -- only be one that entered during the resolution. That is what lets this
  -- assert about the fetched land even though it shares a name with the two that
  -- paid.
  --
  -- TWO Forest cards in the library against a cap of one, so the searcher faces a
  -- real choice rather than a candidate set the size of the cap, and the answer
  -- is PINNED to the basic: Dryad Arbor is a Forest card too and sits ahead of it
  -- (Support.addLibraryCard prepends), so an engine taking the head of the
  -- candidate list would fetch the Arbor this answer never names. The Piker gives
  -- the filter a nonland to reject.
  Spec.it s "CR 110.5b whole card: Nature's Lore puts the Forest it finds onto the battlefield UNTAPPED" $ do
    board <- loreBoard s registry
    let settled = resolveLore (findPinned [loreForest board]) board
    Spec.assertEqWith
      s
      "CR 110.5b the found Forest is the one untapped permanent -- both lands she had paid for the spell"
      (fmap (`S.soleFaceName` settled) (untappedOf S.alice settled))
      [CardName.MkCardName (Text.pack "Forest")]
    Spec.assertEqWith
      s
      "three Forests where she had two"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Forest")) S.alice settled)
      3
    Spec.assertEqWith s "CR 701.23e the card asks for no reveal, so nothing was revealed" (S.revealsOf settled) []
    Spec.assertEqWith s "it did NOT go to her hand -- she cast her only card" (S.handSize S.alice settled) 0
    Spec.assertEqWith
      s
      "the other Forest card and the nonland stayed in the library"
      (Set.fromList (Game.zoneMembers Zone.Library S.alice settled))
      (Set.fromList [loreArbor board, lorePiker board])
  -- CR 101.4b: a pair of boards differing only in alice's answer to the "may".
  -- bob and carol each give the answer their prompt says the seat before them
  -- gave, declining when told nothing, so what they do is what they were told.
  Spec.it s "CR 101.4b Jungle Wayfinder's later seats know the earlier seats' answers" $ do
    (gs, spellId) <- wayfinderBoard s registry
    let cast = snd (Engine.runGamePure findFirstExercising gs (S.cast S.alice spellId))
        onStack = snd (Engine.runGamePure findFirstExercising cast (Stack.resolveTop >> Engine.settleForPriority))
        run hers = State.runState (Engine.runGame (copyingWayfinderAnswer hers) onStack Stack.resolveTop) []
        ((_, took), toldTook) = run OptionalDecision.Exercises
        ((_, declined), toldDeclined) = run OptionalDecision.Declines
        nameOf = Just . CardName.MkCardName . Text.pack
        exercises = OptionalDecision.Exercises
        declines = OptionalDecision.Declines
    Spec.assertEqWith s "CR 101.4b told alice took it, bob took it and found a Mountain" (namesIn Zone.Hand S.bob took) [nameOf "Mountain"]
    Spec.assertEqWith s "CR 101.4b told alice declined, bob declined" (namesIn Zone.Hand S.bob declined) []
    Spec.assertEqWith s "CR 101.4b each seat was told the answers before its own" toldTook [(S.alice, []), (S.bob, [(S.alice, exercises)]), (S.carol, [(S.alice, exercises), (S.bob, exercises)])]
    Spec.assertEqWith s "CR 101.4b and so when alice declined" toldDeclined [(S.alice, []), (S.bob, [(S.alice, declines)]), (S.carol, [(S.alice, declines), (S.bob, declines)])]
  Spec.it s "CR 603/608.2n Rest in Peace's ETB exiles graveyards and ceases" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let g0 = Setup.emptyGame S.bothPlayers
        (ripId, g1) = S.addPermanent restInPeace S.alice g0
        (deadId, g2) = S.addLibraryCard piker S.bob g1
        -- move the Piker into bob's graveyard
        g3 = S.runPure S.identityAnswer g2 (Event.changeZone deadId Zone.Graveyard)
        ability =
          TriggeredAbility.MkTriggeredAbility
            TriggerCondition.SelfEnters
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ExileAllGraveyards]))) Map.empty)) (ModeSelection.ChooseExactly 1))
            Nothing
            TriggerLimit.Unlimited
        (abilId, g4) = Game.freshObjectId g3
        (ts, g5) = Game.freshTimestamp g4
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfTrigger
                  TriggeredAbilitySource.MkTriggeredAbilitySource
                    { TriggeredAbilitySource.source = ripId,
                      TriggeredAbilitySource.ability = ability,
                      TriggeredAbilitySource.createdAt = Nothing
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.flipped = False,
              Object.exiledFaceDown = False,
              Object.exileLookers = Set.empty,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.controlClock = Map.empty,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.foretellCostReduction = Nothing,
              Object.warped = Nothing,
              Object.preparedCopyOf = Nothing,
              Object.ringBearerFor = Nothing,
              Object.duplicate = Nothing,
              Object.paired = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.designationValues = Map.empty,
              Object.paidCosts = Map.empty,
              Object.tributePaid = False,
              Object.bestowed = False,
              Object.mutating = False,
              Object.prototyped = False,
              Object.boughtBack = False,
              Object.spliced = Seq.empty,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.castFrom = Nothing,
              Object.castUsing = Nothing,
              Object.castGrant = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapFor = 0,
              Object.exertedBy = Set.empty,
              Object.activatedOnce = Map.empty
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
        (srcId, g1) = S.addPermanent mindslaver S.alice g0
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
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ControlPlayerNextTurn slot]))) (Map.singleton slot (TargetSlot.required Pool.Players Nothing))))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.maximumX = [],
              ActivatedAbility.minimumX = 0,
              ActivatedAbility.restrictions = [],
              ActivatedAbility.activator = Activator.Controller,
              ActivatedAbility.condition = Nothing,
              ActivatedAbility.name = Nothing,
              ActivatedAbility.keyword = Nothing
            }
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfAbility
                  ActivatedAbilitySource.MkActivatedAbilitySource
                    { ActivatedAbilitySource.source = srcId,
                      ActivatedAbilitySource.ability = ability
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.flipped = False,
              Object.exiledFaceDown = False,
              Object.exileLookers = Set.empty,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.controlClock = Map.empty,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.bob))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.foretellCostReduction = Nothing,
              Object.warped = Nothing,
              Object.preparedCopyOf = Nothing,
              Object.ringBearerFor = Nothing,
              Object.duplicate = Nothing,
              Object.paired = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.designationValues = Map.empty,
              Object.paidCosts = Map.empty,
              Object.tributePaid = False,
              Object.bestowed = False,
              Object.mutating = False,
              Object.prototyped = False,
              Object.boughtBack = False,
              Object.spliced = Seq.empty,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.castFrom = Nothing,
              Object.castUsing = Nothing,
              Object.castGrant = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapFor = 0,
              Object.exertedBy = Set.empty,
              Object.activatedOnce = Map.empty
            }
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = abilId : GameState.stack g3}
        resolved = snd (Engine.runGamePure S.identityAnswer g4 Stack.resolveTop)
        bobsTurn = snd (Engine.runGamePure S.identityAnswer resolved Engine.handoffTurn)
        afterBob = snd (Engine.runGamePure S.identityAnswer bobsTurn Engine.handoffTurn)
    Spec.assertEqWith s "control pending for bob" (Map.lookup S.bob (GameState.pendingControl resolved)) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "promoted on bob's turn" (GameState.control bobsTurn) (S.turnControl S.alice S.bob)
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
        (aliceId, g1) = S.addPermanent mountain S.alice g0
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
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.RestartGame Nothing)))) Map.empty))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.maximumX = [],
              ActivatedAbility.minimumX = 0,
              ActivatedAbility.restrictions = [],
              ActivatedAbility.activator = Activator.Controller,
              ActivatedAbility.condition = Nothing,
              ActivatedAbility.name = Nothing,
              ActivatedAbility.keyword = Nothing
            }
        abilObj =
          Object.MkObject
            { Object.owner = S.bob,
              Object.enteredUnder = Nothing,
              Object.source =
                Source.OfAbility
                  ActivatedAbilitySource.MkActivatedAbilitySource
                    { ActivatedAbilitySource.source = aliceId,
                      ActivatedAbilitySource.ability = ability
                    },
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.facing = Facing.FaceUp,
              Object.flipped = False,
              Object.exiledFaceDown = False,
              Object.exileLookers = Set.empty,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.bob,
              Object.controlClock = Map.empty,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.counterTimestamps = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.chosenNames = Set.empty,
              Object.chosenPlayer = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing,
              Object.turnedOverAt = Nothing,
              Object.worldSince = Nothing,
              Object.playableFromExile = Nothing,
              Object.plotted = Nothing,
              Object.foretold = Nothing,
              Object.foretellCostReduction = Nothing,
              Object.warped = Nothing,
              Object.preparedCopyOf = Nothing,
              Object.ringBearerFor = Nothing,
              Object.duplicate = Nothing,
              Object.paired = Nothing,
              Object.protector = Nothing,
              Object.ventureRoom = Nothing,
              Object.classLevel = Nothing,
              Object.unlockedHalves = Set.empty,
              Object.designations = Set.empty,
              Object.designationValues = Map.empty,
              Object.paidCosts = Map.empty,
              Object.tributePaid = False,
              Object.bestowed = False,
              Object.mutating = False,
              Object.prototyped = False,
              Object.boughtBack = False,
              Object.spliced = Seq.empty,
              Object.phyrexianLifePaid = 0,
              Object.manaSpent = Mana.MkMana [],
              Object.announcedX = Nothing,
              Object.castFrom = Nothing,
              Object.castUsing = Nothing,
              Object.castGrant = Nothing,
              Object.detainedUntil = Set.empty,
              Object.goadedBy = Set.empty,
              Object.doesNotUntapFor = 0,
              Object.exertedBy = Set.empty,
              Object.activatedOnce = Map.empty
            }
        g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
        after = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
    Spec.assertEqWith s "the game restarted with bob as the starting player (CR 727.1a)" (GameState.activePlayer after) S.bob
    Spec.assertEqWith s "alice's 8 cards all survived the restart, still hers (CR 727.2)" (length (filter (\o -> Object.owner o == S.alice) (Map.elems (GameState.objects after)))) 8
    Spec.assertEqWith s "the resolving ability object ceased to exist (not a card)" (Game.lookupObject abilId after) Nothing
  Spec.it s "CR 729.1b: PlaySubgame binds the winner, a later LoseLife reads it (mid-resolution binding visible)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        -- a stub runner: no real subgame, just report alice won.
        stubRunner :: Game Result.Result
        stubRunner = pure (Result.Won S.alice)
        (spellId, g1) = subgameSpellOn lightningBolt "Subgame Test Spell" nonWinnersLose3 g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "bob, the one player who did not win, lost 3 to the follow-on" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "alice won, so the exclusion kept her out of the set" (S.lifeOf S.alice after) (Just 20)
  Spec.it s "CR 729.1b: a DRAWN subgame binds no winner, so the whole table is in the non-winner set" $ do
    -- The won board one line over with EXACTLY ONE thing changed -- the stub's
    -- Result -- so what the two cases differ by is who won and nothing else.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        stubRunner :: Game Result.Result
        stubRunner = pure Result.Drawn
        (spellId, g1) = subgameSpellOn lightningBolt "Subgame Test Spell" nonWinnersLose3 g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "bob did not win a drawn subgame, so he pays" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "and neither did alice -- a draw punishes everybody, not nobody" (S.lifeOf S.alice after) (Just 17)
  Spec.it s "CR 729.1b: the non-winner set is the players still in the game, so a departed seat is not in it" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    -- bob departed the MAIN game before this effect resolves, so bob was never
    -- seated for the subgame (Setup.subgameStateFrom seats only
    -- Game.stillPlayingInOrder) -- only alice and carol played it. The stub
    -- reports alice won, so carol is the whole non-winner set; bob still appears
    -- in the raw seating roster (GameState.turnOrder) and is the non-participant
    -- a roster bug would wrongly charge.
    let g0 = S.departs Departure.Type.Conceded S.bob S.threePlayerGame
        stubRunner :: Game Result.Result
        stubRunner = pure (Result.Won S.alice)
        (spellId, g1) = subgameSpellOn lightningBolt "Subgame Test Spell (Three Seats, One Departed)" nonWinnersLose3 g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Resolve.resolveSpellWith stubRunner spellId))
    Spec.assertEqWith s "carol, a genuine subgame participant who did not win, lost 3" (S.lifeOf S.carol after) (Just 17)
    Spec.assertEqWith s "bob departed before the subgame and never played it, so he pays nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "alice won" (S.lifeOf S.alice after) (Just 20)
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
      (S.countOf (\oid -> Just (Projection.viewOfObject oid gs)) (Filter.contextFor Teams.none (Just S.alice) Nothing) gs yourHand)
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
        gs1 = snd (S.addPermanent mountain S.alice (snd (S.addPermanent mountain S.alice gs0)))
        gs2 = snd (S.addPermanent snowMountain S.bob gs1)
        (wall, gs3) = S.addPermanent wallOfStone S.bob gs2
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
    let (pikerId, gs) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        slot = SlotName.MkSlotName (Text.pack "target")
        store m =
          S.runPure S.identityAnswer gs $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.alice
              (Map.singleton slot (Set.singleton (Recipient.ToCreature pikerId)))
              (Map.singleton slot (Set.singleton (Recipient.ToCreature pikerId)))
              (Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn m (ObjectRef.InSlot slot) Nothing))
        refused = store (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) Quantity.Star))
        stored = store (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)))
    Spec.assertEqWith s "no effect is stored for an unevaluable quantity" (GameState.continuousEffects refused) []
    Spec.assertEqWith s "and the Piker is its printed 2/1" (Projection.powerOf pikerId refused, Projection.toughnessOf pikerId refused) (Just 2, Just 1)
    Spec.assertEqWith s "the same call with two Literals DOES store one -- the refusal is what did it" (length (GameState.continuousEffects stored)) 1
    Spec.assertEqWith s "and pumps the Piker to 5/4" (Projection.powerOf pikerId stored, Projection.toughnessOf pikerId stored) (Just 5, Just 4)
  -- The OTHER half of the same freeze: a quantity that IS answerable, but only
  -- against the resolution's own slot bindings. Rush of Blood's X is
  -- Quantity.AgainstSlot "target", which reads Filter.slotObjects -- so a freeze
  -- handed a bare Filter.contextFor sees an empty slot map, answers Nothing, and
  -- the arm above stores no continuous effect at all. The card would resolve, go
  -- to the graveyard, and silently do nothing.
  --
  -- Rabid Bite is the existing precedent for the same AgainstSlot/Power pair
  -- against a target slot, through Effect.DealDamage -- which already evaluates
  -- via effectContext, which is why nothing was red.
  --
  -- A Goblin Piker (2/1) rather than a vanilla 1/1: the buggy answer (2) and a
  -- "default the slot to 0" partial fix (also 2) must both differ from the right
  -- one (4), and the toughness assertion then separates +X/+0 (4/1) from +X/+X
  -- (4/3). It is the only creature on the board, so CR 601.2c leaves one legal
  -- target and no answerer picks it.
  Spec.it s "CR 608.2h/611.2d Rush of Blood's X is the power of the creature in its own target slot" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    rushOfBlood <- S.printingOf s registry "Rush of Blood"
    -- Exactly three Mountains, the minimum that pays {2}{R}, so no spare mana
    -- can pay for a second read.
    let (pikerId, withPiker) = S.addPermanent piker S.alice (S.landsInPlay mountain 3)
        (gs, spellId) = S.handOne rushOfBlood withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "power 2 + its own 2" (Projection.powerOf pikerId resolved) (Just 4)
    Spec.assertEqWith s "toughness untouched at 1 -- +X/+0, not +X/+X" (Projection.toughnessOf pikerId resolved) (Just 1)
    Spec.assertEqWith
      s
      "and what was STORED is the frozen pair, not the raw AgainstSlot"
      (fmap ContinuousEffect.modification (GameState.continuousEffects resolved))
      [Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0))]
    Spec.assertEqWith s "the Piker is still its printed 2/1 while the spell is on the stack" (Projection.powerOf pikerId cast, Projection.toughnessOf pikerId cast) (Just 2, Just 1)

-- Add n Mountains to pid's battlefield, discarding the ids (used to bulk up a
-- pool of owned cards). replicate n () avoids a list comprehension (CLAUDE.md).
addMany :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addMany mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addPermanent mountain pid g)) gs (replicate n ())

-- Build a Mindslaver-shaped ControlPlayerNextTurn ability owned by `controller`,
-- targeting `target`, put it on the stack, and resolve it. Returns the resulting
-- state. Object.owner is the resolving ability's controller (Resolve.hs), so this
-- installs pendingControl[target] = MkDecider controller.
installControlBy :: Printing.Printing -> PlayerId.PlayerId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
installControlBy mindslaver controller target gs0 =
  let (srcId, gs1) = S.addPermanent mindslaver controller gs0
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
                (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.ControlPlayerNextTurn slot]))) (Map.singleton slot (TargetSlot.required Pool.Players Nothing))))
                (ModeSelection.ChooseExactly 1),
            ActivatedAbility.maximumX = [],
            ActivatedAbility.minimumX = 0,
            ActivatedAbility.restrictions = [],
            ActivatedAbility.activator = Activator.Controller,
            ActivatedAbility.condition = Nothing,
            ActivatedAbility.name = Nothing,
            ActivatedAbility.keyword = Nothing
          }
      (abilId, gs2) = Game.freshObjectId gs1
      (ts, gs3) = Game.freshTimestamp gs2
      abilObj =
        Object.MkObject
          { Object.owner = controller,
            Object.enteredUnder = Nothing,
            Object.source =
              Source.OfAbility
                ActivatedAbilitySource.MkActivatedAbilitySource
                  { ActivatedAbilitySource.source = srcId,
                    ActivatedAbilitySource.ability = ability
                  },
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.flipped = False,
            Object.exiledFaceDown = False,
            Object.exileLookers = Set.empty,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.controlClock = Map.empty,
            Object.bindings = Binding.fromChoices (Map.singleton slot (Set.singleton (Recipient.ToPlayer target))) Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.foretellCostReduction = Nothing,
            Object.warped = Nothing,
            Object.preparedCopyOf = Nothing,
            Object.ringBearerFor = Nothing,
            Object.duplicate = Nothing,
            Object.paired = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.designationValues = Map.empty,
            Object.paidCosts = Map.empty,
            Object.tributePaid = False,
            Object.bestowed = False,
            Object.mutating = False,
            Object.prototyped = False,
            Object.boughtBack = False,
            Object.spliced = Seq.empty,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.castUsing = Nothing,
            Object.castGrant = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapFor = 0,
            Object.exertedBy = Set.empty,
            Object.activatedOnce = Map.empty
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

-- Nature's Lore's board. Two Forests pay the {1}{G} -- both of them, which is
-- what makes "exactly one untapped" an assertion about the fetch -- and the
-- library holds two Forest CARDS against a cap of one, plus a nonland for the
-- filter to reject.
data LoreBoard = MkLoreBoard
  { loreState :: GameState.GameState,
    loreSpell :: ObjectId.ObjectId,
    loreForest :: ObjectId.ObjectId,
    loreArbor :: ObjectId.ObjectId,
    lorePiker :: ObjectId.ObjectId
  }

loreBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m LoreBoard
loreBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  arbor <- S.printingOf s registry "Dryad Arbor"
  piker <- S.printingOf s registry "Goblin Piker"
  lore <- S.printingOf s registry "Nature's Lore"
  let (forestId, g1) = S.addLibraryCard forest S.alice (S.landsInPlay forest 2)
      (arborId, g2) = S.addLibraryCard arbor S.alice g1
      (pikerId, g3) = S.addLibraryCard piker S.alice g2
      (gs, spellId) = S.handOne lore g3
  pure (MkLoreBoard gs spellId forestId arborId pikerId)

resolveLore :: (forall r. Prompt.Prompt r -> r) -> LoreBoard -> GameState.GameState
resolveLore answer board =
  let cast = snd (Engine.runGamePure answer (loreState board) (S.cast S.alice (loreSpell board)))
   in snd (Engine.runGamePure answer cast Engine.priorityLoop)

-- Finds exactly the cards named and nothing else, whatever the engine offers.
-- PINNED rather than picked out of the candidate list: an answerer that went
-- looking for a legal choice would find one again after a mutation, and the
-- assertion would stay green while the engine's own count was broken.
findPinned :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
findPinned wanted p = case p of
  Prompt.Search {} -> wanted
  _ -> S.identityAnswer p

untappedOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
untappedOf pid gs =
  let isUntapped oid = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
   in filter isUntapped (Game.zoneMembers Zone.Battlefield pid gs)

findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.Search _ _ matches cap -> List.genericTake cap matches
  _ -> S.identityAnswer p

-- Names a card the search filter did NOT admit -- the lying interpreter #222 is
-- about. Parameterised so the test can point it at a specific nonland.
findForbidden :: ObjectId.ObjectId -> Prompt.Prompt r -> r
findForbidden wanted p = case p of
  Prompt.Search {} -> [wanted]
  _ -> S.identityAnswer p

-- findFirst, plus CR 603.5's printed "may" taken. The pair below it declines the
-- same "may" and answers every other prompt identically, so a board run through
-- both differs in exactly that one decision.
findFirstExercising :: Prompt.Prompt r -> r
findFirstExercising p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> findFirst p

-- The same card with alice's hand holding nothing but the spell, so CR 608.2d's
-- "impossible" is what the discard is: the three Mountains pay the {2}{R} and
-- the three Goblin Pikers in her library make a draw visible by name, CR 104.3c
-- having no chance to decide the game first.
emptyHandedTweezeBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
emptyHandedTweezeBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  tweeze <- S.printingOf s registry "Tweeze"
  piker <- S.printingOf s registry "Goblin Piker"
  let (base, tweezeId) = S.handOne tweeze (S.landsInPlay mountain 3)
      stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard piker S.alice gs)) base [1 :: Int .. 3]
  pure (stocked, tweezeId)

-- Three seats; an Island and four Mountains pay Development's {3}{U}{R}. Four
-- Goblin Pikers in alice's
-- library, so every draw is visible and CR 104.3c cannot end the game first.
developmentBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
developmentBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  card <- S.printingOf s registry "Research"
  piker <- S.printingOf s registry "Goblin Piker"
  let lands = S.landsFor island S.alice 1 (S.landsFor mountain S.alice 4 S.threePlayerGame)
      stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard piker S.alice gs)) lands [1 :: Int .. 4]
  pure (S.handOne card stocked)

-- Tweeze's three answers in one: the damage aimed at bob by FILTERING the offer
-- (CR 608.2b re-reads it, so a hand-built recipient would be dropped), CR 603.5's
-- "may" answered as the case says, and CR 701.9b's discard pinned to one card
-- rather than to whichever the hand offers first.
tweezeAnswer :: OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> Prompt.Prompt r -> r
tweezeAnswer decision toDiscard p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer S.bob) sets
  Prompt.ChooseOptional {} -> decision
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n (filter (== toDiscard) ids)
  _ -> S.identityAnswer p

-- The board the three Twiddle cases share: alice casts it off two Islands, and
-- bob's Goblin Piker is the target, tapped or not as the case needs. Two seats,
-- so the "artifact, creature, or land" pool cannot be satisfied by something
-- alice controls by accident -- and the Islands are legal targets too, so the
-- offer holds more than the one recipient the answerer filters down to.
twiddleBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
twiddleBoard s registry startTapped = do
  island <- S.printingOf s registry "Island"
  twiddle <- S.printingOf s registry "Twiddle"
  piker <- S.printingOf s registry "Goblin Piker"
  let (pikerId, withPiker) = S.addPermanent piker S.bob (S.landsInPlay island 2)
      placed = if startTapped then S.tapObject pikerId withPiker else withPiker
      (gs, twiddleId) = S.handOne twiddle placed
  pure (gs, twiddleId, pikerId)

-- The victim's id, so a case reads the board it built rather than rebuilding it.
thirdOf :: (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId) -> ObjectId.ObjectId
thirdOf (_, _, pikerId) = pikerId

-- One cast and resolution of that board, KEEPING the transcript: the prompts a
-- case asserts about are the ones actually raised, rather than a claim about
-- what the engine would have asked.
twiddleResolved :: ClauseIndex.ClauseIndex -> OptionalDecision.OptionalDecision -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId) -> ([Response.Response], GameState.GameState)
twiddleResolved branch decision (gs, twiddleId, pikerId) =
  let ((_, after), asked) = Replay.record (twiddleAnswer branch decision pikerId) gs (S.cast S.alice twiddleId >> Stack.resolveTop)
   in (asked, after)

-- Twiddle's three answers in one: the target FILTERED out of the offer (CR
-- 608.2b re-reads it, so a hand-built recipient would be dropped), CR 608.2d's
-- branch, and CR 603.5's "may".
twiddleAnswer :: ClauseIndex.ClauseIndex -> OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> Prompt.Prompt r -> r
twiddleAnswer branch decision target p = case p of
  -- By the OBJECT the recipient names rather than by a recipient built by hand:
  -- Twiddle's Permanents pool offers ToObject and Teardrop Kami's Creatures pool
  -- offers ToCreature, and a hand-built recipient of the wrong shape is dropped
  -- by CR 608.2b's re-read with no error.
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just target) . Recipient.objectOf) sets
  Prompt.ChooseClause {} -> Just branch
  Prompt.ChooseOptional {} -> decision
  _ -> S.identityAnswer p

twiddleTapState :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
twiddleTapState oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- CR 701.55d: the board The Dalek Emperor's two cases share. THREE SEATS,
-- because "each opponent" is the whole point -- two collapse rule 701.55d's
-- several players onto one, where rule 608.2e's ask-everybody-then-act is
-- indistinguishable from it.
--
-- TWO creatures each rather than one, so CR 701.21a's pick is a real choice and
-- Prompt.ChooseSacrifices does not elide itself; two DIFFERENT printings, so
-- which seat lost one is read by name rather than by a count that a wrong seat's
-- loss would also satisfy. alice's Emperor is a creature too, which is what the
-- last assertion of the second case reads.
emperorBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
emperorBoard s registry = do
  emperor <- S.printingOf s registry "The Dalek Emperor"
  piker <- S.printingOf s registry "Goblin Piker"
  construct <- S.printingOf s registry "Bonded Construct"
  let placed pr pid n base = List.foldl' (\acc _ -> snd (S.addPermanent pr pid acc)) base [1 .. n :: Int]
      (emperorId, withEmperor) = S.addPermanent emperor S.alice S.threePlayerGame
  pure (placed construct S.carol 2 (placed piker S.bob 2 withEmperor), emperorId)

-- alice's beginning of combat step, run for its turn-based actions and its
-- trigger: Engine.runStep is what writes the CR 603.2b StepBegan record the
-- Emperor's ability matches, and the priority loop is what resolves it.
--
-- `picks` is keyed by SEAT because the two ChooseClause prompts are structurally
-- identical apart from the player they are put to: a pure answerer would answer
-- both the same way and neither case below could tell the engine's order from
-- its own. The pool is recorded alongside, so a limb that was never offered is
-- visible rather than inferred from the answer.
emperorCombat :: Map.Map PlayerId.PlayerId ClauseIndex.ClauseIndex -> (GameState.GameState, ObjectId.ObjectId) -> ([(PlayerId.PlayerId, [ClauseIndex.ClauseIndex])], GameState.GameState)
emperorCombat picks (gs, _) =
  let atCombat =
        gs
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      ((_, after), asks) = State.runState (Engine.runGame (emperorAnswer picks) atCombat (Engine.runStep >> Engine.priorityLoop)) []
   in (reverse asks, after)

-- CR 701.55c: the Emperor's board with The Valeyard under BOB's control when
-- `withValeyard`, and nothing else different.
valeyardBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, ObjectId.ObjectId)
valeyardBoard s registry withValeyard = do
  valeyard <- S.printingOf s registry "The Valeyard"
  (gs, emperorId) <- emperorBoard s registry
  pure (if withValeyard then snd (S.addPermanent valeyard S.bob gs) else gs, emperorId)

-- The Valeyard's combat: emperorCombat's step, with each seat's answers taken
-- in order from its queue -- limb 0 is the sacrifice, limb 1 the token.
valeyardCombat :: (GameState.GameState, ObjectId.ObjectId) -> GameState.GameState
valeyardCombat (gs, _) =
  let atCombat =
        gs
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      queues = Map.fromList [(S.bob, [ClauseIndex.MkClauseIndex 1, ClauseIndex.MkClauseIndex 0]), (S.carol, [ClauseIndex.MkClauseIndex 0, ClauseIndex.MkClauseIndex 1])]
   in snd (State.evalState (Engine.runGame (valeyardAnswer queues) atCombat (Engine.runStep >> Engine.priorityLoop)) Map.empty)

-- The branch is the seat's next queued answer, counted in State because a
-- seat's two ChooseClause prompts are structurally identical; filtered back
-- through the offer and pinned, emperorAnswer's posture.
valeyardAnswer :: Map.Map PlayerId.PlayerId [ClauseIndex.ClauseIndex] -> Prompt.Prompt r -> State.State (Map.Map PlayerId.PlayerId Int) r
valeyardAnswer queues p = case p of
  Prompt.ChooseClause _ pid _ _ live _ _ -> do
    asked <- State.gets (Map.findWithDefault 0 pid)
    State.modify' (Map.insertWith (+) pid 1)
    let wanted = Maybe.fromMaybe (NonEmpty.head live) (Maybe.listToMaybe (drop asked (Map.findWithDefault [] pid queues)))
    pure (Just (if elem wanted live then wanted else NonEmpty.head live))
  Prompt.ChooseSacrifices _ _ _ offered _ _ -> pure (Set.fromList (take 1 offered))
  _ -> pure (S.identityAnswer p)

-- The Emperor's two answers: the branch, pinned per seat and filtered back
-- through the offer, and CR 701.21a's sacrifice, pinned to the head of whatever
-- the announcer was offered. An answerer that hunted for a legal creature would
-- repair a mutation that aimed the sacrifice at the wrong seat.
emperorAnswer :: Map.Map PlayerId.PlayerId ClauseIndex.ClauseIndex -> Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [ClauseIndex.ClauseIndex])] r
emperorAnswer picks p = case p of
  Prompt.ChooseClause _ pid _ _ live _ _ -> do
    State.modify' ((pid, NonEmpty.toList live) :)
    let wanted = Map.findWithDefault (NonEmpty.head live) pid picks
    pure (Just (if elem wanted live then wanted else NonEmpty.head live))
  Prompt.ChooseSacrifices _ _ _ offered _ _ -> pure (Set.fromList (take 1 offered))
  _ -> pure (S.identityAnswer p)

-- Which branches CR 608.2d actually asked about, in the order asked.
branchesAnnounced :: [Response.Response] -> [Maybe ClauseIndex.ClauseIndex]
branchesAnnounced = Maybe.mapMaybe (\r -> case r of Response.ChoseClause c -> Just c; _ -> Nothing)

-- And which "may"s CR 603.5 asked about, so a second offer to the losing branch
-- would show up as a second answer.
optionalsAnswered :: [Response.Response] -> [OptionalDecision.OptionalDecision]
optionalsAnswered = Maybe.mapMaybe (\r -> case r of Response.ChoseOptional d -> Just d; _ -> Nothing)

-- The board the two Excavating Anurid cases share, differing in exactly one
-- thing: whether alice controls lands at all. Three Goblin Pikers in her library
-- make the "If you do" draw visible by name and keep CR 104.3c from deciding the
-- game first, and the lands are a Forest and a Mountain rather than two of one
-- so the pick can be pinned and the one NOT taken can be asserted still standing
-- -- two candidates for a count of one, which is also what keeps
-- Prompt.ChooseSacrifices from eliding itself.
--
-- The Anurid is placed rather than cast, so the landless board needs no mana it
-- would have had to find on lands.
anuridBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, Maybe ObjectId.ObjectId)
anuridBoard s registry withLands = do
  anurid <- S.printingOf s registry "Excavating Anurid"
  forest <- S.printingOf s registry "Forest"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  let stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard piker S.alice gs)) (Setup.emptyGame S.bothPlayers) [1 :: Int .. 3]
      (fodder, landed) =
        if withLands
          then
            let (_, withForest) = S.addPermanent forest S.alice stocked
                (mountainId, withMountain) = S.addPermanent mountain S.alice withForest
             in (Just mountainId, withMountain)
          else (Nothing, stocked)
  pure (snd (S.entersWithTrigger anurid S.alice landed), fodder)

-- Excavating Anurid's two answers in one: CR 603.5's "may" always taken, so an
-- offer the engine should have withheld shows up as a sacrifice and a draw, and
-- CR 701.21a's pick pinned to the Mountain by id rather than to whichever land
-- the offer lists first.
anuridAnswer :: Maybe ObjectId.ObjectId -> Prompt.Prompt r -> r
anuridAnswer fodder p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.ChooseSacrifices _ _ _ offered _ _ -> Set.fromList (filter (\oid -> Just oid == fodder) offered)
  _ -> S.identityAnswer p

-- The board the two Mineshaft Spider cases share, differing in exactly one
-- thing: how deep alice's library is. The Spider mills TWO, so a library of one
-- is CR 701.17b's "greater than" without being empty.
spiderBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
spiderBoard s registry depth = do
  spider <- S.printingOf s registry "Mineshaft Spider"
  piker <- S.printingOf s registry "Goblin Piker"
  let stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard piker S.alice gs)) (Setup.emptyGame S.bothPlayers) [1 .. depth]
  pure (snd (S.entersWithTrigger spider S.alice stocked))

-- CR 603.5's "may" always taken, for the reason anuridAnswer gives.
spiderAnswer :: Prompt.Prompt r -> r
spiderAnswer p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- The board the two Thrilling Discovery cases share, differing in exactly one
-- thing: how many cards besides the spell alice holds. A Mountain and a Plains
-- pay the {R}{W}, and four Goblin Pikers in her library make the draw visible
-- and keep CR 104.3c from deciding the game first. Three cards for a discard of
-- two, so Prompt.ChooseDiscard is a real choice rather than eliding itself.
discoveryBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId])
discoveryBoard s registry held = do
  mountain <- S.printingOf s registry "Mountain"
  plains <- S.printingOf s registry "Plains"
  discovery <- S.printingOf s registry "Thrilling Discovery"
  piker <- S.printingOf s registry "Goblin Piker"
  others <- traverse (S.printingOf s registry) (take held ["Bird Maiden", "Chaos Charm", "Lightning Bolt"])
  let (base, spellId) = S.handOne discovery (S.landsFor plains S.alice 1 (S.landsInPlay mountain 1))
      (ids, withHand) = List.foldl' (\(acc, gs) printing -> let (oid, gs') = S.addHandCard printing S.alice gs in (acc <> [oid], gs')) ([], base) others
      stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard piker S.alice gs)) withHand [1 :: Int .. 4]
  pure (stocked, spellId, ids)

-- CR 603.5's "may" always taken, so an offer the engine should have withheld
-- shows up as a discard and a draw, and CR 701.9b's pick pinned by id.
discoveryAnswer :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
discoveryAnswer pinned p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n (filter (`elem` pinned) ids <> filter (`notElem` pinned) ids)
  _ -> S.identityAnswer p

-- The board the two Giant Opportunity cases share, differing in exactly one
-- thing: how many Golden Eggs (Food artifacts) alice controls. Three Forests pay
-- the {2}{G}.
opportunityBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId])
opportunityBoard s registry eggs = do
  forest <- S.printingOf s registry "Forest"
  opportunity <- S.printingOf s registry "Giant Opportunity"
  egg <- S.printingOf s registry "Golden Egg"
  let (base, spellId) = S.handOne opportunity (S.landsInPlay forest 3)
      (ids, placed) = List.foldl' (\(acc, gs) _ -> let (oid, gs') = S.addPermanent egg S.alice gs in (acc <> [oid], gs')) ([], base) [1 .. eggs]
  pure (placed, spellId, ids)

-- CR 608.2d's announcement always takes the sacrifice when it is offered, so a
-- branch the engine should have withheld shows up as a Giant, and CR 701.21a's
-- pick is pinned by id.
opportunityAnswer :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
opportunityAnswer pinned p = case p of
  Prompt.ChooseClause _ _ _ _ live _ _ -> Just (if elem (ClauseIndex.MkClauseIndex 0) live then ClauseIndex.MkClauseIndex 0 else NonEmpty.head live)
  Prompt.ChooseSacrifices _ _ _ offered n _ -> Set.fromList (List.genericTake n (filter (`elem` pinned) offered <> filter (`notElem` pinned) offered))
  _ -> S.identityAnswer p

-- The CR 608.2d announcements a transcript holds.
clausesAnswered :: [Response.Response] -> [Maybe ClauseIndex.ClauseIndex]
clausesAnswered = Maybe.mapMaybe (\r -> case r of Response.ChoseClause c -> Just c; _ -> Nothing)

giantToken :: CardName.CardName
giantToken = CardName.MkCardName (Text.pack "Giant Token")

foodToken :: CardName.CardName
foodToken = CardName.MkCardName (Text.pack "Food Token")

goldenEgg :: CardName.CardName
goldenEgg = CardName.MkCardName (Text.pack "Golden Egg")

-- The entry trigger placed and resolved, KEEPING the transcript: the prompts a
-- case asserts about are the ones actually raised.
enteredAndResolved :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ([Response.Response], GameState.GameState)
enteredAndResolved answer gs =
  let ((_, after), asked) = Replay.record answer gs (Engine.settleForPriority >> Stack.resolveTop)
   in (asked, after)

-- The board the two Teardrop Kami cases share, built to match twiddleBoard as
-- closely as an ability can: alice's Kami is the source, bob's Goblin Piker the
-- target, tapped or not as the case needs. The Kami is itself a legal target
-- until its own sacrifice cost is paid (CR 601.2c chooses targets before CR
-- 601.2h pays), so the offer again holds more than the answerer takes.
kamiBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)), ObjectId.ObjectId, ObjectId.ObjectId)
kamiBoard s registry startTapped = do
  kami <- S.printingOf s registry "Teardrop Kami"
  piker <- S.printingOf s registry "Goblin Piker"
  let (kamiId, g0) = S.addPermanent kami S.alice (Setup.emptyGame S.bothPlayers)
      (pikerId, g1) = S.addPermanent piker S.bob g0
      placed = if startTapped then S.tapObject pikerId g1 else g1
  pure (placed {GameState.priority = Just S.alice}, Maybe.listToMaybe (Face.activatedAbilities (S.combinedFace kami)), kamiId, pikerId)

-- One activation and resolution of that board, keeping the transcript for
-- twiddleResolved's reason. The ability is read off the PRINTING rather than
-- conjured, so what is proved is the card in data/cards/.
kamiResolved :: ClauseIndex.ClauseIndex -> OptionalDecision.OptionalDecision -> GameState.GameState -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> ObjectId.ObjectId -> ObjectId.ObjectId -> ([Response.Response], GameState.GameState)
kamiResolved branch decision gs ability kamiId pikerId =
  let ((_, after), asked) = Replay.record (twiddleAnswer branch decision pikerId) gs (Activate.activateAbility S.alice kamiId ability >> Stack.resolveTop)
   in (asked, after)

-- The board the two Jungle Wayfinder cases share: alice casts it off three
-- Forests, and every seat's library holds three of ONE basic plus a Goblin Piker
-- for the filter to reject. See the first case for why three seats, three basics
-- each, and a different basic per seat.
wayfinderBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
wayfinderBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  wayfinder <- S.printingOf s registry "Jungle Wayfinder"
  island <- S.printingOf s registry "Island"
  mountain <- S.printingOf s registry "Mountain"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let stock printing pid g = List.foldl' (\h _ -> snd (S.addLibraryCard printing pid h)) g [1 :: Int .. 3]
      g0 = S.landsFor forest S.alice 3 S.threePlayerGame
      (_, g1) = S.addLibraryCard piker S.alice g0
      g2 = stock island S.alice g1
      (_, g3) = S.addLibraryCard piker S.bob g2
      g4 = stock mountain S.bob g3
      (_, g5) = S.addLibraryCard piker S.carol g4
      g6 = stock plains S.carol g5
  pure (S.handOne wayfinder g6)

-- CR 101.4b's answerer for Jungle Wayfinder: alice answers `hers`, and each
-- later seat gives the answer its prompt says the seat before it gave, declining
-- when told nothing. Each "may" prompt's earlier answers are recorded in order.
copyingWayfinderAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [(PlayerId.PlayerId, OptionalDecision.OptionalDecision)])] r
copyingWayfinderAnswer hers p = case p of
  Prompt.ChooseOptional _ pid _ _ _ earlier -> do
    State.modify' (<> [(pid, Foldable.toList earlier)])
    pure $
      if pid == S.alice
        then hers
        else case Seq.viewr earlier of
          _ Seq.:> (_, previous) -> previous
          Seq.EmptyR -> OptionalDecision.Declines
  Prompt.Search _ _ matches cap -> pure (List.genericTake cap matches)
  _ -> pure (S.identityAnswer p)

-- Alice's Synthetic Split Reliquary over a graveyard of one creature card and
-- one land card: each exiling ability takes its one candidate on its own
-- resolution, then the third ability is activated and resolved. Event.untap
-- between them is twiceSphinx's road to a real second {T}.
splitReliquary :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
splitReliquary s registry = do
  reliquary <- S.printingOf s registry "Synthetic Split Reliquary"
  piker <- S.printingOf s registry "Goblin Piker"
  forest <- S.printingOf s registry "Forest"
  let (reliquaryId, g0) = S.addPermanent reliquary S.alice (Setup.emptyGame S.bothPlayers)
      (_, g1) = S.addGraveyardCard piker S.alice g0
      (_, g2) = S.addGraveyardCard forest S.alice g1
      board = g2 {GameState.priority = Just S.alice}
      run ability gs = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice reliquaryId ability >> Stack.resolveTop)
      untap gs = S.runPure S.identityAnswer gs (Event.untap reliquaryId)
  case Face.activatedAbilities (S.combinedFace reliquary) of
    [warden, scholar, back] -> do
      let exiled = untap (run scholar (untap (run warden board)))
      Spec.assertEqWith s "both exiling abilities exiled their card" (List.sort (exileNames exiled)) [CardName.MkCardName (Text.pack "Forest"), CardName.MkCardName (Text.pack "Goblin Piker")]
      pure (run back exiled)
    _ -> Spec.assertFailure s "Synthetic Split Reliquary should declare three activated abilities"

-- findFirstExercising with the FIND pinned to one named card. The two Dragons of
-- the CR 607.2a pair have to exile DIFFERENT artifacts for the linked set to be
-- provable at all, and the head of the offered list is where a shuffle left it
-- rather than something the fixture chose.
findPinnedExercising :: ObjectId.ObjectId -> Prompt.Prompt r -> r
findPinnedExercising wanted p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.Search {} -> [wanted]
  _ -> S.identityAnswer p

-- The board CR 607.2a's two cases share: two Hoarding Dragons of alice's, each
-- having exiled a different artifact, plus what an assertion needs to tell the
-- two halves apart.
data TwoDragons = MkTwoDragons
  { dragonBoard :: GameState.GameState,
    firstDragon :: ObjectId.ObjectId,
    secondDragon :: ObjectId.ObjectId,
    firstArtifact :: CardName.CardName,
    secondArtifact :: CardName.CardName
  }

-- Cast one spell and settle, so the entry trigger has resolved by the time the
-- next Dragon is cast.
settleCast :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
settleCast answer spellId gs =
  let cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
   in snd (Engine.runGamePure answer cast Engine.priorityLoop)

dragonsOf :: GameState.GameState -> [ObjectId.ObjectId]
dragonsOf gs =
  filter
    (\oid -> S.soleFaceName oid gs == CardName.MkCardName (Text.pack "Hoarding Dragon"))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- The Dragons are cast one at a time so that the SECOND one's id is the
-- battlefield Dragon the first cast did not leave behind. Every claim the board
-- makes is asserted here rather than assumed, since both cases below read the
-- board's own answer back: an exile that held one card, or a hand that already
-- held one, would make them pass for the wrong reason.
twoDragonBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m TwoDragons
twoDragonBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  dragon <- S.printingOf s registry "Hoarding Dragon"
  altar <- S.printingOf s registry "Ashnod's Altar"
  sphere <- S.printingOf s registry "Chromatic Sphere"
  piker <- S.printingOf s registry "Goblin Piker"
  let base0 = S.landsInPlay mountain 10
      (altarId, base1) = S.addLibraryCard altar S.alice base0
      (sphereId, base2) = S.addLibraryCard sphere S.alice base1
      (_, base3) = S.addLibraryCard piker S.alice base2
      (base4, firstSpell) = S.handOne dragon base3
      (secondSpell, base5) = S.addHandCard dragon S.alice base4
      afterFirst = settleCast (findPinnedExercising altarId) firstSpell base5
      afterSecond = settleCast (findPinnedExercising sphereId) secondSpell afterFirst
      earlier = dragonsOf afterFirst
      later = filter (`notElem` earlier) (dragonsOf afterSecond)
  Spec.assertEqWith s "the first Dragon is alone on the battlefield" (length earlier) 1
  Spec.assertEqWith s "the second Dragon joined it" (length later) 1
  Spec.assertEqWith s "her hand is empty, so anything in it later was returned" (S.handSize S.alice afterSecond) 0
  Spec.assertEqWith
    s
    "each Dragon exiled a different artifact"
    (Set.fromList (exileNames afterSecond))
    (Set.fromList [S.printingName altar, S.printingName sphere])
  pure
    MkTwoDragons
      { dragonBoard = afterSecond,
        firstDragon = Maybe.fromMaybe S.noSource (Maybe.listToMaybe earlier),
        secondDragon = Maybe.fromMaybe S.noSource (Maybe.listToMaybe later),
        firstArtifact = S.printingName altar,
        secondArtifact = S.printingName sphere
      }

-- CR 704.5g: lethal damage on a 4/4, swept by the state-based actions the
-- priority loop runs before it hands anybody priority, which is what fires the
-- dies trigger and resolves it.
kill :: ObjectId.ObjectId -> TwoDragons -> GameState.GameState
kill oid board =
  snd (Engine.runGamePure findFirstExercising (S.markDamage oid 4 (dragonBoard board)) Engine.priorityLoop)

handNames :: GameState.GameState -> [CardName.CardName]
handNames gs = fmap (`S.soleFaceName` gs) (Game.zoneMembers Zone.Hand S.alice gs)

exileNames :: GameState.GameState -> [CardName.CardName]
exileNames gs = fmap (`S.soleFaceName` gs) (Game.zoneMembers Zone.Exile S.alice gs)

findNothing :: Prompt.Prompt r -> r
findNothing p = case p of
  Prompt.Search {} -> []
  _ -> S.identityAnswer p

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

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

-- CR 729.1b's plumbing, as a card face would write it: run the subgame, then make
-- every player who did not win lose 3. A flat 3 rather than Shahrazad's half-life
-- rider because what these three cases pin is WHO is in the set, and a per-player
-- amount would let a wrong set and a wrong amount cancel out.
nonWinnersLose3 :: [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
nonWinnersLose3 =
  let slot = SlotName.MkSlotName (Text.pack "winner")
   in [ Effect.PlaySubgame slot,
        Effect.LoseLife (LifeLoss.MkLifeLoss (PlayerRef.EachPlayerExcept slot) (Quantity.Literal 3) LifeLossCause.ByEffect Nothing)
      ]

-- A hand-built {0} sorcery of alice's on the stack, one chosen mode holding
-- `effects` and no target slots. The NARROWEST path to the Effect.PlaySubgame arm:
-- Resolve.resolveSpellWith takes the subgame runner as an argument, so a stub
-- Result decides the outcome outright and no nested game runs -- which is what
-- lets a test name a DRAWN subgame at all. `borrowed` supplies the type line, so
-- the object is a spell like any other.
subgameSpellOn :: Printing.Printing -> String -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
subgameSpellOn borrowed name effects gs0 =
  let (spellPrintingId, gs0b) = Game.intern (Printing.ofCard card) gs0
      (spellId, gs1) = Game.freshObjectId gs0b
      (ts, gs2) = Game.freshTimestamp gs1
      card = Card.Type.MkCard {Card.Type.layout = Layout.Normal, Card.Type.faces = NonEmpty.singleton face}
      face =
        Face.MkFace
          { Face.name = CardName.MkCardName $ Text.pack name,
            Face.manaCost = Nothing,
            Face.typeLine = Face.typeLine (S.combinedFace borrowed),
            Face.power = Nothing,
            Face.toughness = Nothing,
            Face.loyalty = Nothing,
            Face.defense = Nothing,
            Face.startingIntensity = Nothing,
            Face.vanguard = Nothing,
            Face.canBeYourCommander = False,
            Face.claimsStartingPlayer = False,
            Face.keywords = Map.empty,
            Face.colorIndicator = Set.empty,
            Face.staticAbilities = [],
            Face.spell =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) Map.empty))
                (ModeSelection.ChooseExactly 1),
            Face.activatedAbilities = [],
            Face.replacementEffects = [],
            Face.triggeredAbilities = [],
            Face.delayedAbilities = Map.empty,
            Face.rooms = Seq.empty,
            Face.dungeonEntryQuality = Nothing,
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
            Face.attachRestrictions = [],
            Face.counterRestrictions = [],
            Face.crewRestrictions = [],
            Face.attackPermissions = [],
            Face.activationProhibitions = [],
            Face.entryRestrictions = [],
            Face.attackCosts = [],
            Face.blockCosts = [],
            Face.mulliganActions = [],
            Face.openingHandActions = [],
            Face.specialActions = [],
            Face.additionalCosts = [],
            Face.additionalCostChoices = [],
            Face.modeCosts = Map.empty,
            Face.maximumX = [],
            Face.minimumX = 0,
            Face.alternativeCosts = [],
            Face.costReductions = [],
            Face.enchant = [],
            Face.counterability = Counterability.Counterable
          }
      spellObj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfToken spellPrintingId,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.flipped = False,
            Object.exiledFaceDown = False,
            Object.exileLookers = Set.empty,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.controlClock = Map.empty,
            Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.foretellCostReduction = Nothing,
            Object.warped = Nothing,
            Object.preparedCopyOf = Nothing,
            Object.ringBearerFor = Nothing,
            Object.duplicate = Nothing,
            Object.paired = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.designationValues = Map.empty,
            Object.paidCosts = Map.empty,
            Object.tributePaid = False,
            Object.bestowed = False,
            Object.mutating = False,
            Object.prototyped = False,
            Object.boughtBack = False,
            Object.spliced = Seq.empty,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.castUsing = Nothing,
            Object.castGrant = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapFor = 0,
            Object.exertedBy = Set.empty,
            Object.activatedOnce = Map.empty
          }
   in (spellId, gs2 {GameState.objects = Map.insert spellId spellObj (GameState.objects gs2), GameState.stack = spellId : GameState.stack gs2})

-- CR 608.2d's either-or announced BY EACH PLAYER, with CR 118.12's cost on one
-- branch and CR 603.5's "may" on the other, and CR 608.2c's "if a player does
-- either" hung off both of them.
--
-- Worms of the Earth, {2}{B}{B}{B} Enchantment, whose third sentence is "At the
-- beginning of each upkeep, any player may sacrifice two lands of their choice
-- or have this enchantment deal 5 damage to that player. If a player does
-- either, destroy this enchantment." The sacrifice is CR 118.12's cost -- which
-- is what stops a landless seat destroying the enchantment for free, CR 118.3
-- never offering a cost that cannot be paid -- and the damage is an ordinary
-- effect, no Pawl.Types.CostComponent taking damage.
--
-- THREE SEATS, none of them able to stand in for another: alice controls the
-- enchantment and is the active player, bob and carol are the two seats a case
-- puts on different branches, and a two-seat board could not tell "the seat who
-- announced the sacrifice" from "the seat who did not announce the damage".
--
-- Every branch here carries its own decline, so CR 608.2d's announcement is
-- among three outcomes and settles the seat's whole answer (Resolve.chosenBranch).
-- The answerer BACKS OUT of any second question it is asked -- the pay and the
-- "may" alike -- so a seat's branch happens only if the announcement committed
-- it, and the ANNOUNCEMENT is the only thing that decides what happens to a
-- seat. An engine offering both branches to everybody would have bob sacrifice
-- AND take the damage in the first two cases.
wormsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wormsSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- CR 503.1's upkeep step, entered the way the trigger reads it: the
      -- TriggerCondition.StepBegins condition matches the EVENT, so the fixture
      -- records one beside setting the phase. TurnScope.EachTurn is "each
      -- upkeep", so alice's own is enough.
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
      lands gs = (landCount "Swamp" S.alice gs, landCount "Forest" S.bob gs, landCount "Mountain" S.carol gs)
      landCount name = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name))
      wormsStands = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Worms of the Earth")) S.alice
      sacrificeBranch = ClauseIndex.MkClauseIndex 0
      damageBranch = ClauseIndex.MkClauseIndex 1
      -- Choice-keyed, and keyed on the SEAT: a seat the map does not name
      -- announces "neither". Any second question is declined.
      announcing :: Map.Map PlayerId.PlayerId ClauseIndex.ClauseIndex -> Prompt.Prompt r -> r
      announcing choices p = case p of
        Prompt.ChooseClause (Decider.MkDecider d) player _ _ _ _ _
          | d == player -> Map.lookup player choices
        _ -> backingOut p
      backingOut :: Prompt.Prompt r -> r
      backingOut p = case p of
        Prompt.ChooseToPay {} -> PaymentDecision.Declines
        Prompt.ChooseOptional {} -> OptionalDecision.Declines
        _ -> S.identityAnswer p
      -- CR 101.4b: alice announces `hers`, bob announces what his prompt says
      -- the seat before him announced, and carol announces neither.
      copying :: ClauseIndex.ClauseIndex -> Prompt.Prompt r -> r
      copying hers p = case p of
        Prompt.ChooseClause (Decider.MkDecider d) player _ _ _ _ earlier
          | d == player ->
              if player == S.alice
                then Just hers
                else
                  if player == S.bob
                    then case Seq.viewr earlier of
                      _ Seq.:> (_, previous) -> previous
                      Seq.EmptyR -> Nothing
                    else Nothing
        _ -> backingOut p
      -- A different basic per seat, so a land count reads one seat's payment and
      -- not the table's, and THREE of them where the cost takes two -- a seat
      -- that paid keeps one, which "sacrificed everything he had" would not.
      boardOf carolLands = do
        worms <- S.printingOf s registry "Worms of the Earth"
        swamp <- S.printingOf s registry "Swamp"
        forest <- S.printingOf s registry "Forest"
        mountain <- S.printingOf s registry "Mountain"
        let g0 = S.landsFor swamp S.alice 3 S.threePlayerGame
            g1 = S.landsFor forest S.bob 3 g0
            g2 = S.landsFor mountain S.carol carolLands g1
            (wormsId, g3) = S.addPermanent worms S.alice g2
        pure (wormsId, settle (beginUpkeep g3))
   in Spec.describe s "CR 608.2d an either-or announced by each player" $ do
        Spec.it s "CR 118.12 the seat that announced the sacrifice pays it, and the enchantment is destroyed" $ do
          (_, onStack) <- boardOf 3
          let after = S.runPure (announcing (Map.singleton S.bob sacrificeBranch)) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 701.21a bob sacrificed two of his three Forests, and nobody else lost a land" (lands after) (3, 1, 3)
          Spec.assertEqWith s "CR 608.2d the branch he did not announce did not happen: no seat took the 5 damage" (lives after) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "CR 608.2c \"if a player does either\": the enchantment is gone" (wormsStands after) 0
          Spec.assertEqWith s "CR 701.8a and it is in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "CR 603.2 the upkeep trigger really was on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "and the board it resolved against had every seat level, with three lands each" (lives onStack, lands onStack) ((Just 20, Just 20, Just 20), (3, 3, 3))
        Spec.it s "CR 608.2d the seat that announced the damage takes it instead, and the enchantment is destroyed" $ do
          (_, onStack) <- boardOf 3
          let after = S.runPure (announcing (Map.singleton S.bob damageBranch)) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 120.1 bob took the 5 damage and nobody else did" (lives after) (Just 20, Just 15, Just 20)
          Spec.assertEqWith s "CR 118.12 the cost on the branch he did not announce was never offered him: his three Forests stand" (lands after) (3, 3, 3)
          Spec.assertEqWith s "CR 608.2c \"if a player does either\": the enchantment is gone" (wormsStands after) 0
        Spec.it s "CR 608.2c two seats on different branches destroy the enchantment once" $ do
          (_, onStack) <- boardOf 3
          let after = S.runPure (announcing (Map.fromList [(S.bob, sacrificeBranch), (S.carol, damageBranch)])) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 701.21a bob paid with two Forests and carol paid nothing" (lands after) (3, 1, 3)
          Spec.assertEqWith s "CR 120.1 carol took the 5 damage and bob, who sacrificed instead, did not" (lives after) (Just 20, Just 20, Just 15)
          Spec.assertEqWith s "CR 608.2c the one destruction the sentence prints happened: alice's graveyard holds the enchantment alone" (wormsStands after, length (Game.zoneMembers Zone.Graveyard S.alice after)) (0, 1)
        -- A pair differing only in what alice announces; bob copies it.
        Spec.it s "CR 101.4b a later seat knows the branch the seat before it announced" $ do
          (_, onStack) <- boardOf 3
          let damaged = S.runPure (copying damageBranch) onStack Stack.resolveTop
              sacrificed = S.runPure (copying sacrificeBranch) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 101.4b told alice announced the damage, bob took the 5 damage beside her and kept his Forests" (lives damaged, lands damaged) ((Just 15, Just 15, Just 20), (3, 3, 3))
          Spec.assertEqWith s "CR 101.4b told alice announced the sacrifice, bob sacrificed two Forests beside her two Swamps" (lives sacrificed, lands sacrificed) ((Just 20, Just 20, Just 20), (1, 1, 3))
        -- CR 608.2d / 101.4: alice (the active player) announces the damage and
        -- bob the sacrifice, and the damage clause comes after the sacrifice
        -- clause. Asked again at her "may", alice would back out having seen
        -- bob's lands go; one announcement among three outcomes holds her to it.
        Spec.it s "CR 608.2d a seat that announced the damage is held to it" $ do
          (_, onStack) <- boardOf 3
          let ((_, after), asked) = Replay.record (announcing (Map.fromList [(S.alice, damageBranch), (S.bob, sacrificeBranch)])) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 608.2d alice took the 5 damage she announced, after bob paid with two Forests" (lives after, lands after) ((Just 15, Just 20, Just 20), (3, 1, 3))
          Spec.assertEqWith s "CR 608.2d nobody was asked a second question to back out with" [r | r <- asked, case r of { Response.ChoseToPay _ -> True; Response.ChoseOptional _ -> True; _ -> False }] []
        Spec.it s "CR 608.2c with every seat declining, the enchantment survives untouched" $ do
          (_, onStack) <- boardOf 3
          let after = S.runPure (announcing Map.empty) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 608.2c nobody did either, so the enchantment stands" (wormsStands after) 1
          Spec.assertEqWith s "no land was sacrificed" (lands after) (3, 3, 3)
          Spec.assertEqWith s "and no seat took the damage" (lives after) (Just 20, Just 20, Just 20)
        -- CR 608.2d per seat, with CR 118.3 deciding what is impossible: carol
        -- controls one land, so the sacrifice is not an option for her, and she
        -- takes the sacrifice whenever she is offered it and the damage
        -- otherwise. Offered it, she would announce a branch she cannot pay for
        -- and nothing would happen.
        Spec.it s "CR 608.2d a seat who cannot pay the sacrifice is offered only the damage" $ do
          (_, onStack) <- boardOf 1
          let preferringSacrifice :: Prompt.Prompt r -> r
              preferringSacrifice p = case p of
                Prompt.ChooseClause (Decider.MkDecider d) player _ _ live _ _
                  | d == player && player == S.carol -> Just (if elem sacrificeBranch live then sacrificeBranch else damageBranch)
                _ -> announcing Map.empty p
              after = S.runPure preferringSacrifice onStack Stack.resolveTop
          Spec.assertEqWith s "CR 608.2d carol took the damage, kept her one Mountain, and the enchantment is gone" (lives after, lands after, wormsStands after) ((Just 20, Just 20, Just 15), (3, 3, 1), 0)

-- CR 608.2d's battlefield choice put to somebody other than the resolving
-- controller, with Wormfang Crab {3}{U} Creature -- Nightmare Crab 3/6: "When
-- this creature enters, an opponent chooses a permanent you control other than
-- this creature and exiles it." CR 608.2c makes alice the player who follows the
-- instruction; the sentence still hands the ANNOUNCEMENT to a seat she names.
--
-- THREE seats, so "an opponent" is a real choice and the chooser is neither the
-- resolving controller nor the table's only other player.
--
-- A PAIR OF RUNS differing in exactly one thing -- which permanent carol names --
-- so that "the Ornithopter went" cannot be read as a fixed order over the
-- candidates.
crabSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
crabSpec s registry =
  let -- The answers pinned by IDENTITY and FILTERED from the offer (#222), the
      -- Prompt.ChoosePermanent posture. `byOthers` is what ANY seat but carol
      -- names, which is how the resolving controller's own pick is spelled: a run
      -- that put the question to the wrong player answers differently rather than
      -- alike, which is what makes the seat observable at gameplay level.
      picking :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      picking byCarol byOthers p = case p of
        Prompt.ChooseOpponent _ _ _ seats
          | List.elem S.carol (NonEmpty.toList seats) -> S.carol
        Prompt.ChoosePermanent _ asked _ offered ->
          let wanted = if asked == S.carol then byCarol else byOthers
           in if List.elem wanted (NonEmpty.toList offered) then wanted else NonEmpty.head offered
        _ -> S.identityAnswer p
      -- alice's Crab entering in front of her Llanowar Elves and her Ornithopter.
      -- The candidates are those two: the Crab is excluded by the card's "other
      -- than this creature", and bob and carol control nothing, which is what
      -- "you control" excludes.
      crabBoard = do
        crab <- S.printingOf s registry "Wormfang Crab"
        elves <- S.printingOf s registry "Llanowar Elves"
        thopter <- S.printingOf s registry "Ornithopter"
        let (elvesId, withElves) = S.addPermanent elves S.alice S.threePlayerGame
            (thopterId, withBoth) = S.addPermanent thopter S.alice withElves
            (_, entered) = S.entersWithTrigger crab S.alice withBoth
        pure (S.nameOf (Printing.card elves), S.nameOf (Printing.card thopter), elvesId, thopterId, entered)
      resolved :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      resolved answer board = S.runPure answer board (Engine.settleForPriority >> Engine.priorityLoop)
      -- Each exiled object is a CR 400.7 incarnation with an id of its own, so the
      -- board's ids cannot be compared against; the names are what the sentence
      -- talks about. alice OWNS all three, so the owner-keyed zone read reaches
      -- them.
      owned zone gs = List.sort (Maybe.catMaybes (namesIn zone S.alice gs))
   in Spec.describe s "CR 608.2d who chooses a permanent on the battlefield" $ do
        Spec.it s "CR 608.2d the opponent the sentence addresses announces the choice, not the resolving controller" $ do
          (elvesName, thopterName, elvesId, thopterId, entered) <- crabBoard
          let carolTakesThopter = resolved (picking thopterId elvesId) entered
              carolTakesElves = resolved (picking elvesId thopterId) entered
          Spec.assertEqWith s "CR 608.2d carol named the Ornithopter, so the Ornithopter is what alice exiled" (owned Zone.Exile carolTakesThopter) [thopterName]
          Spec.assertEqWith s "and the Llanowar Elves the resolving controller would have named still stands" (elem elvesName (owned Zone.Battlefield carolTakesThopter)) True
          Spec.assertEqWith s "the same board with carol naming the Elves instead exiles the Elves" (owned Zone.Exile carolTakesElves) [elvesName]
          Spec.assertEqWith s "and leaves the Ornithopter standing, so no fixed order over the candidates explains either run" (elem thopterName (owned Zone.Battlefield carolTakesElves)) True
          -- Anti-vacuity: both permanents really were alice's and really were on
          -- the battlefield before the trigger resolved, so each run had two
          -- candidates to tell apart and exile was empty to begin with.
          Spec.assertEqWith s "setup: alice controlled both candidates and the Crab, and exile was empty" (owned Zone.Exile entered, length (owned Zone.Battlefield entered)) ([], 3)
          Spec.assertEqWith s "setup: the entry trigger really went on the stack" (length (GameState.stack (S.runPure S.identityAnswer entered Engine.settleForPriority))) 1

-- Vapor Snag ({U} Instant, Scryfall 2026-09-17): "Return target creature to its
-- owner's hand. Its controller loses 1 life." Two clauses in CR 608.2c's printed
-- order, so the second names a slot the first has already moved to a hidden zone
-- -- CR 608.2h's last known information, since CR 108.4 leaves a card in a hand
-- with no controller at all.
--
-- The PAIR is the board below it, differing only in who controlled the bounced
-- creature: the sentence says "its controller", which two boards separate from
-- "you" and from "each opponent".
snagSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
snagSpec s registry = Spec.describe s "CR 608.2h a bounced target's controller" $ do
  let snagAt owner = do
        island <- S.printingOf s registry "Island"
        piker <- S.printingOf s registry "Goblin Piker"
        snag <- S.printingOf s registry "Vapor Snag"
        let base = S.landsInPlay island 1
            (_, withPiker) = S.addPermanent piker owner base
            (gs, spellId) = S.handOne snag withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        pure (gs, snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
  Spec.it s "an opponent's creature: that opponent loses the life" $ do
    (gs, after) <- snagAt S.bob
    Spec.assertEqWith s "bob, who controlled the bounced creature, went 20 -> 19" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "and alice, who cast it, lost none" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "the creature is off the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "and in bob's hand" (S.handSize S.bob after) 1
    Spec.assertEqWith s "setup: bob controlled a creature and started at 20" (S.creaturesInPlay S.bob gs, S.lifeOf S.bob gs) (1, Just 20)

-- CR 118.12a over the WHOLE TABLE: "unless any player pays {2}" is "each player
-- may pay {2}; if no player does, [do something]", with the offers in CR 101.4's
-- APNAP order (Rhystic Shield's and Rhystic Cave's rulings). The "each player"
-- reading -- one IfNotPaid answer per seat, the clause running if ANY seat
-- declined -- is what Rishadan Cutpurse needs and exactly what these cards do
-- not say.
--
-- THREE SEATS, so one payer and two decliners separate the readings: under
-- "each player" alice and carol declining would still run the clause. In the
-- Tutor cases every seat can still pay after the cast, so no decline is CR
-- 118.3's "can't".
rhysticSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rhysticSpec s registry =
  let -- The one seat that pays, if any; every search takes the first match.
      paysFor :: Maybe PlayerId.PlayerId -> Prompt.Prompt r -> r
      paysFor who p = case p of
        Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
          | Just d == who && Just player == who -> PaymentDecision.Pays
        Prompt.Search _ _ matches cap -> List.genericTake cap matches
        _ -> S.identityAnswer p
      payResponses = filter (\r -> case r of Response.ChoseToPay _ -> True; _ -> False)
      -- alice: five Swamps and a Rhystic Tutor, a Goblin Piker in her
      -- library. bob: two Islands. carol: two Forests.
      tutorBoard = do
        swamp <- S.printingOf s registry "Swamp"
        island <- S.printingOf s registry "Island"
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        tutor <- S.printingOf s registry "Rhystic Tutor"
        let lands = S.landsFor forest S.carol 2 (S.landsFor island S.bob 2 (S.landsFor swamp S.alice 5 S.threePlayerGame))
            (pikerId, stocked) = S.addLibraryCard piker S.alice lands
            (gs, tutorId) = S.handOne tutor stocked
            onStack = S.runPure S.identityAnswer gs (S.cast S.alice tutorId)
        pure (pikerId, onStack)
      -- bob: a Goblin Piker on the stack. alice: two Swamps and Dash Hopes,
      -- cast at the Piker, with its CR 601.2i cast trigger settled on top.
      dashBoard = do
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        dash <- S.printingOf s registry "Dash Hopes"
        let (_, withPiker) = S.spellOnStack piker S.bob (S.landsFor swamp S.alice 2 S.threePlayerGame)
            (gs, dashId) = S.handOne dash withPiker
            onStack = S.runPure S.identityAnswer gs (S.cast S.alice dashId >> Engine.settleForPriority)
        pure onStack
   in Spec.describe s "CR 118.12a unless any player pays" $ do
        Spec.it s "CR 118.12a bob alone pays, so alice does not search" $ do
          (pikerId, onStack) <- tutorBoard
          let ((_, after), transcript) = Replay.record (paysFor (Just S.bob)) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 118.12a: the Piker is still in alice's library" (Game.zoneMembers Zone.Library S.alice after) [pikerId]
          Spec.assertEqWith s "and her hand is empty" (S.handSize S.alice after) 0
          -- Every player is offered, in APNAP order, alice first.
          Spec.assertEqWith s "CR 101.4: alice declined, bob paid, carol declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines, Response.ChoseToPay PaymentDecision.Pays, Response.ChoseToPay PaymentDecision.Declines]
        -- The same board, differing only in bob's answer.
        Spec.it s "CR 118.12a nobody pays, so alice searches, unrevealed" $ do
          (_, onStack) <- tutorBoard
          let ((_, after), transcript) = Replay.record (paysFor Nothing) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 118.12a: the Piker left alice's library" (Game.zoneMembers Zone.Library S.alice after) []
          Spec.assertEqWith s "into her hand" (fmap (`S.soleFaceName` after) (Game.zoneMembers Zone.Hand S.alice after)) [CardName.MkCardName (Text.pack "Goblin Piker")]
          Spec.assertEqWith s "CR 701.23e: \"put that card into your hand\" reveals nothing" (S.revealsOf after) []
          Spec.assertEqWith s "all three declined" (payResponses transcript) (replicate 3 (Response.ChoseToPay PaymentDecision.Declines))
        -- A pair differing only in bob's answer; carol pays exactly when her
        -- prompt says bob did.
        Spec.it s "CR 101.4b a later payer is told what the payers before it answered" $ do
          onStack <- dashBoard
          let copying :: Bool -> Prompt.Prompt r -> r
              copying bobPays p = case p of
                Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ earlier
                  | d == player && player == S.bob -> if bobPays then PaymentDecision.Pays else PaymentDecision.Declines
                  | d == player && player == S.carol -> Maybe.fromMaybe PaymentDecision.Declines (lookup S.bob (Foldable.toList earlier))
                _ -> paysFor Nothing p
              paid = S.runPure (copying True) onStack Stack.resolveTop
              declined = S.runPure (copying False) onStack Stack.resolveTop
          Spec.assertEqWith s "CR 101.4b told bob paid, carol paid too" (fmap (`S.lifeOf` paid) [S.alice, S.bob, S.carol]) [Just 20, Just 15, Just 15]
          Spec.assertEqWith s "CR 101.4b told bob declined, carol declined" (fmap (`S.lifeOf` declined) [S.alice, S.bob, S.carol]) (replicate 3 (Just 20))

-- CR 118.12 / 118.12a over a cost printed as a CHOICE: Torment of Venom's
-- "Its controller loses 3 life unless they sacrifice another nonland permanent
-- of their choice or discard a card." bob controls the target and is the payer.
--
-- The answerer is a State log of every ChooseCost (how many options it offered)
-- and ChooseToPay (the cost it offered), answering ChooseCost by INDEX so a
-- mutation cannot be repaired by a search for the "right" option. Test-local
-- because the scenario harness matches a ChooseCost answer by its mana part
-- alone, and neither option here has one.
tormentSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tormentSpec s registry =
  let -- alice: four Swamps and Torment of Venom, cast at bob's Hill Giant. bob:
      -- a Swamp in hand and, when `withOther`, an Ornithopter -- the one other
      -- nonland permanent he could sacrifice.
      board withOther = do
        swamp <- S.printingOf s registry "Swamp"
        giant <- S.printingOf s registry "Hill Giant"
        thopter <- S.printingOf s registry "Ornithopter"
        torment <- S.printingOf s registry "Torment of Venom"
        let (target, g1) = S.addPermanent giant S.bob (S.landsFor swamp S.alice 4 S.threePlayerGame)
            (other, g2) = if withOther then S.addPermanent thopter S.bob g1 else (target, g1)
            (_, g3) = S.addHandCard swamp S.bob g2
            (gs, tormentId) = S.handOne torment g3
            aim :: Prompt.Prompt r -> r
            aim p = case p of
              Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just target) . Recipient.objectOf) sets
              _ -> S.identityAnswer p
        pure (other, S.runPure aim gs (S.cast S.alice tormentId))
      logging :: Int -> PaymentDecision.PaymentDecision -> Prompt.Prompt r -> State.State [Either Int (Cost.Type.Cost Keyword.Keyword)] r
      logging pick decision p = case p of
        Prompt.ChooseCost (Decider.MkDecider d) player _ candidates | d == player && player == S.bob -> do
          State.modify' (<> [Left (length candidates)])
          pure
            ( case drop pick candidates of
                chosen : _ -> chosen
                [] -> S.identityAnswer p
            )
        Prompt.ChooseToPay (Decider.MkDecider d) player _ _ cost _ | d == player && player == S.bob -> do
          State.modify' (<> [Right cost])
          pure decision
        _ -> pure (S.identityAnswer p)
      resolveWith pick decision onStack = State.runState (Engine.runGame (logging pick decision) onStack Stack.resolveTop) []
      discards cost = any (\c -> case c of CostComponent.DiscardCards {} -> True; _ -> False) (Cost.Type.components cost)
      outcome other gs = (S.onBattlefield other gs, S.handSize S.bob gs, S.lifeOf S.bob gs)
   in Spec.describe s "CR 118.12 a choice of costs" $ do
        -- A pair differing only in bob's ChooseCost answer.
        Spec.it s "CR 118.12a the payer picks which option to pay" $ do
          (thopter, onStack) <- board True
          let ((_, sacrificed), asked) = resolveWith 0 PaymentDecision.Pays onStack
              ((_, discarded), _) = resolveWith 1 PaymentDecision.Pays onStack
          Spec.assertEqWith s "picking the sacrifice: the Ornithopter is gone, the Swamp kept, no life lost" (outcome thopter sacrificed) (False, 1, Just 20)
          Spec.assertEqWith s "picking the discard: the Ornithopter stays, the Swamp is gone, no life lost" (outcome thopter discarded) (True, 0, Just 20)
          Spec.assertEqWith s "asked which of two options, then whether to pay" (fmap (either Just (const Nothing)) asked) [Just 2, Nothing]
        Spec.it s "CR 118.12a declining both options loses the life" $ do
          (thopter, onStack) <- board True
          let ((_, declined), _) = resolveWith 1 PaymentDecision.Declines onStack
          Spec.assertEqWith s "bob went 20 -> 17 and kept both" (outcome thopter declined) (True, 1, Just 17)
        -- The board above less the Ornithopter: the target is not "another"
        -- permanent, so only the discard is payable.
        Spec.it s "CR 118.3 an option the payer cannot pay is not offered" $ do
          (_, onStack) <- board False
          let ((_, after), asked) = resolveWith 0 PaymentDecision.Pays onStack
          Spec.assertEqWith s "CR 118.3 no ChooseCost, and the one offer is the discard" (fmap (either (const Nothing) (Just . discards)) asked) [Just True]
          Spec.assertEqWith s "bob discarded the Swamp and lost no life" (S.handSize S.bob after, S.lifeOf S.bob after) (0, Just 20)

-- CR 118.12a inside CR 608.2f's loop: Cleansing's "for each land, destroy that
-- land unless any player pays 1 life" is one offer PER LAND, each land's
-- destruction bought off by any one payment for THAT land. THREE SEATS and two
-- lands per non-caster, so a payment for one land can be seen not to save its
-- neighbour, and a seat can be seen paying for a land it does not control.
--
-- The answerer is a State log of every ChooseToPay, so the APNAP order of the
-- offers and what each one told its payer are read off what the engine asked.
cleansingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cleansingSpec s registry =
  let -- alice: three Plains and Cleansing in hand. bob: two Islands. carol: two
      -- Forests. Every land by id, so each offer is attributable to its land.
      board = do
        plains <- S.printingOf s registry "Plains"
        island <- S.printingOf s registry "Island"
        forest <- S.printingOf s registry "Forest"
        cleansing <- S.printingOf s registry "Cleansing"
        let withPlains = S.landsFor plains S.alice 3 S.threePlayerGame
            (islandA, g1) = S.addPermanent island S.bob withPlains
            (islandB, g2) = S.addPermanent island S.bob g1
            (forestC, g3) = S.addPermanent forest S.carol g2
            (forestD, g4) = S.addPermanent forest S.carol g3
            (gs, cleansingId) = S.handOne cleansing g4
            onStack = S.runPure S.identityAnswer gs (S.cast S.alice cleansingId)
        pure ((islandA, islandB, forestC, forestD), onStack)
      -- One ChooseToPay: who was asked, about which land, told what.
      logging ::
        (PlayerId.PlayerId -> Maybe ObjectId.ObjectId -> Seq.Seq (PlayerId.PlayerId, PaymentDecision.PaymentDecision) -> PaymentDecision.PaymentDecision) ->
        Prompt.Prompt r ->
        State.State [(PlayerId.PlayerId, Maybe ObjectId.ObjectId, [(PlayerId.PlayerId, PaymentDecision.PaymentDecision)])] r
      logging policy p = case p of
        Prompt.ChooseToPay (Decider.MkDecider d) player _ offer _ earlier | d == player -> do
          let land = case offer of
                PayOffer.ForMember member -> Recipient.objectOf member
                PayOffer.AtClause {} -> Nothing
          State.modify' (<> [(player, land, Foldable.toList earlier)])
          pure (policy player land earlier)
        _ -> pure (S.identityAnswer p)
      resolveWith policy onStack = State.runState (Engine.runGame (logging policy) onStack Stack.resolveTop) []
      -- Everything on the battlefield, in id order.
      permanentsOf gs = List.sort (concatMap (\pid -> Game.zoneMembers Zone.Battlefield pid gs) [S.alice, S.bob, S.carol])
      lives gs = fmap (`S.lifeOf` gs) [S.alice, S.bob, S.carol]
   in Spec.describe s "CR 118.12a an offer per member of a CR 608.2f loop" $ do
        Spec.it s "CR 118.12a each land is its own offer, and any player's payment saves only that land" $ do
          ((islandA, islandB, forestC, forestD), onStack) <- board
          -- bob pays for his Island A; carol pays for bob's Island B and her
          -- own Forest C. Nobody pays for Forest D or for any Plains.
          let policy player land _
                | player == S.bob && land == Just islandA = PaymentDecision.Pays
                | player == S.carol && land `elem` [Just islandB, Just forestC] = PaymentDecision.Pays
                | otherwise = PaymentDecision.Declines
              ((_, after), asked) = resolveWith policy onStack
          Spec.assertEqWith s "CR 118.12a the three lands somebody paid for stand, and every other land was destroyed" (permanentsOf after) (List.sort [islandA, islandB, forestC])
          Spec.assertEqWith s "CR 118.3b bob paid 1 life and carol 2" (lives after) [Just 20, Just 19, Just 18]
          Spec.assertEqWith s "CR 701.8a Forest D went to carol's graveyard though she paid for Forest C" (length (Game.zoneMembers Zone.Graveyard S.carol after), elem forestD (permanentsOf after)) (1, False)
          -- CR 101.4: every offer alice makes, then every one bob makes, then
          -- carol's -- seven lands each.
          Spec.assertEqWith s "CR 101.4 the offers go alice, bob, carol, each seat answering for every land" (fmap (\(who, _, _) -> who) asked) (replicate 7 S.alice <> replicate 7 S.bob <> replicate 7 S.carol)
          Spec.assertEqWith s "each offer named its land" (all (\(_, land, _) -> Maybe.isJust land) asked) True
          Spec.assertEqWith
            s
            "CR 101.4b carol, asked about Island A, was told alice declined and bob paid"
            [earlier | (who, land, earlier) <- asked, who == S.carol, land == Just islandA]
            [[(S.alice, PaymentDecision.Declines), (S.bob, PaymentDecision.Pays)]]
        -- A pair differing only in bob's answer for Island A: carol pays for a
        -- land exactly when her prompt says bob paid for it.
        Spec.it s "CR 101.4b a later seat knows what the seats before it answered for that land" $ do
          ((islandA, _, _, _), onStack) <- board
          let copying bobPays player land earlier
                | player == S.bob = if bobPays && land == Just islandA then PaymentDecision.Pays else PaymentDecision.Declines
                | player == S.carol = Maybe.fromMaybe PaymentDecision.Declines (lookup S.bob (Foldable.toList earlier))
                | otherwise = PaymentDecision.Declines
              ((_, paid), _) = resolveWith (copying True) onStack
              ((_, declined), _) = resolveWith (copying False) onStack
          Spec.assertEqWith s "CR 101.4b told bob paid for Island A, carol paid for it too" (lives paid) [Just 20, Just 19, Just 19]
          Spec.assertEqWith s "CR 101.4b told bob declined everything, carol paid for nothing" (lives declined) [Just 20, Just 20, Just 20]
          Spec.assertEqWith s "and with nobody paying, every land is gone" (permanentsOf declined) []
        -- Killing Wave: the payer is each creature's own controller, and the
        -- cost is the X its caster announced (CR 107.3a).
        Spec.it s "CR 118.12a Killing Wave asks each creature's controller, and a paid creature alone survives" $ do
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          wave <- S.printingOf s registry "Killing Wave"
          let (hers, g1) = S.addPermanent piker S.alice (S.landsFor swamp S.alice 3 S.threePlayerGame)
              (kept, g2) = S.addPermanent piker S.bob g1
              (lost, g3) = S.addPermanent piker S.bob g2
              (gs, waveId) = S.handOne wave g3
              xTwo :: Prompt.Prompt r -> r
              xTwo p = case p of
                Prompt.ChooseX {} -> 2
                _ -> S.identityAnswer p
              onStack = S.runPure xTwo gs (S.cast S.alice waveId)
              policy player creature _ = if player == S.bob && creature == Just kept then PaymentDecision.Pays else PaymentDecision.Declines
              ((_, after), asked) = resolveWith policy onStack
              standing = filter (`elem` [hers, kept, lost]) (permanentsOf after)
          Spec.assertEqWith s "CR 701.21a the Piker bob paid for stands, and the other two were sacrificed" standing [kept]
          Spec.assertEqWith s "CR 107.3a bob paid X = 2 life, once" (lives after) [Just 20, Just 18, Just 20]
          Spec.assertEqWith s "CR 101.4 alice answered for her Piker, then bob for each of his, and carol controls none" (fmap (\(who, creature, _) -> (who, creature)) asked) [(S.alice, Just hers), (S.bob, Just kept), (S.bob, Just lost)]

-- CR 607.2d: Brass Herald's "When this creature enters, reveal the top four
-- cards of your library. Put all creature cards of the chosen type revealed this
-- way into your hand and the rest on the bottom of your library in any order" is
-- linked to its "As this creature enters, choose a creature type", so the
-- resolution's own filter reads the type the Herald chose
-- (Resolve.Slots.effectContext through Pawl.Engine.SourceContext; Oracle checked
-- against Scryfall on 2026-10-01). The choice is stamped, as Pawl.TargetSpec's
-- From the Rubble does.
--
-- alice's library, top first: Goblin Piker, Hill Giant, Goblin Piker, Plains,
-- Island. A PAIR OF BOARDS differing only in the type chosen, each taking a
-- distinct number of cards: Goblin 2, Giant 1. The Island, never revealed, ends
-- on top either way.
brassHeraldSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
brassHeraldSpec s registry =
  Spec.it s "CR 607.2d Brass Herald puts only the revealed creature cards of the type it chose into its controller's hand" $ do
    herald <- S.printingOf s registry "Brass Herald"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    let (islandId, g0) = S.addLibraryCard island S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addLibraryCard plains S.alice g0
        (_, g2) = S.addLibraryCard piker S.alice g1
        (_, g3) = S.addLibraryCard giant S.alice g2
        (_, g4) = S.addLibraryCard piker S.alice g3
        (heraldId, g5) = S.entersWithTrigger herald S.alice g4
        resolved chosen =
          let chose = g5 {GameState.objects = Map.adjust (\o -> o {Object.chosenSubtype = Just chosen}) heraldId (GameState.objects g5)}
              placed = S.runPure S.identityAnswer chose Engine.placePendingTriggers
           in S.runPure S.identityAnswer placed Stack.resolveTop
        top g = Seq.lookup 0 =<< Map.lookup S.alice (GameState.library g)
        goblins = resolved Subtype.Goblin
        giants = resolved Subtype.Giant
    Spec.assertEqWith s "having chosen Goblin, both revealed Pikers go to hand and nothing else" (S.handSize S.alice goblins) 2
    Spec.assertEqWith s "having chosen Giant, only the Hill Giant does" (S.handSize S.alice giants) 1
    Spec.assertEqWith s "the rest went to the bottom, so the unrevealed Island is on top" (top goblins, top giants) (Just islandId, Just islandId)
    Spec.assertEqWith s "and the library keeps every card that did not go to hand" (fmap Seq.length (Map.lookup S.alice (GameState.library goblins)), fmap Seq.length (Map.lookup S.alice (GameState.library giants))) (Just 3, Just 4)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  brassHeraldSpec s registry
  cleansingSpec s registry
  tormentSpec s registry
  targetSpec s registry
  resolveSpec s registry
  wormsSpec s registry
  crabSpec s registry
  snagSpec s registry
  rhysticSpec s registry
