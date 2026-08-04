{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Resolve and Pawl.Engine.Target: targeting legality, spell resolution, and
-- the CR 608.2b fizzle.
module Pawl.ResolveSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
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
import qualified Pawl.Engine.Cast as Cast
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
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
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
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
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
      (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs)
      (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])
  Spec.it s "a departed player is not a legal target" $ do
    let gs = Departure.depart Departure.Type.Lost S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertBool
      s
      (not (Set.member (Recipient.ToPlayer S.bob) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs)))
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
              (Map.singleton slot True)
              (Map.singleton slot (Recipient.ToObject myr))
              (Effect.GainControl Duration.Indefinite (ObjectRef.InSlot slot))
        control =
          S.runPure S.identityAnswer board $
            Resolve.applyEffect
              S.noSource
              S.noSource
              S.bob
              (Map.singleton slot True)
              (Map.singleton slot (Recipient.ToObject myr))
              (Effect.GainControl Duration.Indefinite (ObjectRef.InSlot slot))
    Spec.assertEqWith s "no control effect is stored for a departed controller" (GameState.continuousEffects after) []
    Spec.assertEqWith s "and the Myr's controller is unchanged" (Projection.controllerOf myr after) (Just S.carol)
    Spec.assertEqWith s "the same call for a player still in the game DOES store one -- the guard is what did it" (length (GameState.continuousEffects control)) 1
    Spec.assertEqWith s "and takes control" (Projection.controllerOf myr control) (Just S.bob)
  Spec.it s "CR 608.2b a creature that left its zone is no longer legal" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gone)) "illegal once moved"
  Spec.it s "legalSets maps each slot to its legal recipients" $ do
    let specs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)
        gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith
      s
      "one slot, two players"
      (Target.legalSets Nothing S.noSource specs gs)
      (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
  Spec.it s "CR 115.4 CreatureTarget offers creatures but no players" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "just the creature"
      (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gs)
      (Set.singleton (Recipient.ToCreature oid))
  Spec.it s "CR 601.2c CreatureTarget has an empty legal set with no creatures" $ do
    Spec.assertBool
      s
      (Set.null (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures Nothing) (Setup.emptyGame S.bothPlayers)))
      "nothing to target"
  Spec.it s "CR 608.2b a creature that left is no longer a legal CreatureTarget" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertBool s (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gs) "legal while fielded"
    Spec.assertBool s (not (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gone)) "illegal once moved"
  Spec.it s "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (permId, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertBool
      s
      (Set.member (Recipient.ToObject permId) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.SpellsAndPermanents Nothing) gs))
      "the permanent is a legal object target"
  Spec.it s "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (permId, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (spellId, gs) = S.spellOnStack lightningBolt S.alice base
        legal = Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Spells Nothing) gs
    Spec.assertBool s (Set.member (Recipient.ToObject spellId) legal) "the stack spell is a legal target"
    Spec.assertBool s (not (Set.member (Recipient.ToObject permId) legal)) "the battlefield permanent is not a legal target"
  Spec.it s "LandTarget offers a land as an object target, not a creature or player" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
    Spec.assertBool s (Set.member (Recipient.ToObject landId) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs)) "the land is legal"
    Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs))) "no players"
  Spec.it s "CR 115: PlayerTarget is exactly the players still in the game" $ do
    let gs = Setup.emptyGame S.bothPlayers
        expected = Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]
    Spec.assertEqWith s "both players, no creatures" (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Players Nothing) gs) expected
  -- CR 115.1a / 700.2c: "target Wall" (Chaos Charm) restricts CreatureTarget to
  -- creatures whose PROJECTED subtypes include Wall. Wall of Stone (a real 0/8
  -- Creature - Wall, M4g) is the Wall; a Piker is the non-Wall control.
  Spec.it s "CR 115.1a / 700.2c \"target Wall\" offers a Wall creature but not a non-Wall creature" $ do
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    piker <- S.printingOf s registry "Goblin Piker"
    let (wallId, base) = S.addCreature wallOfStone S.bob (Setup.emptyGame S.bothPlayers)
        (pikerId, gs) = S.addCreature piker S.alice base
        slot = SlotName.MkSlotName (Text.pack "target")
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing S.noSource (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
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
        legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing S.noSource (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
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
        legal = Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))) gs
    Spec.assertEqWith s "exactly the artifact" legal (Set.singleton (Recipient.ToObject (S.artifactId gs)))
    Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) legal)) "no players"
  Spec.it s "CR 115.1a / 109.5 OpponentCreatureTarget excludes the source's controller's creatures" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (mine, gs1) = S.addCreature piker S.alice gs0
        (theirs, gs2) = S.addCreature warMammoth S.bob gs1
        legal = Target.legalRecipients (Just S.alice) mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs2
    Spec.assertEqWith s "only the opponent's creature" legal (Set.singleton (Recipient.ToCreature theirs))
    Spec.assertBool s (not (Set.member (Recipient.ToCreature mine) legal)) "not the source's controller's own"
  -- CR 115.1 / 109.5: "target OPPONENT". Until Ravenous Rats there was no
  -- card in the pool that narrowed a PLAYER target, so Target.legalRecipients
  -- kept every player unconditionally (#168). Three seats, so "an opponent"
  -- is a real set rather than the only other player.
  Spec.it s "CR 115.1 a Players pool narrowed by IsPlayer Opponent excludes the source's controller" $ do
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let (src, gs) = S.addCreature ravenousRats S.alice (Setup.emptyGame S.threePlayers)
        theSpec = TargetSpec.MkTargetSpec Pool.Players (Just (Filter.Type.IsPlayer PlayerRelation.Opponent))
        legal = Target.legalRecipients (Just S.alice) src theSpec gs
    Spec.assertEqWith
      s
      "exactly bob and carol, never alice"
      legal
      (Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol])
  -- The card itself, so the narrowing is proven through the real target spec
  -- the JSON carries rather than one hand-built in the test.
  Spec.it s "CR 115.1 Ravenous Rats' entry trigger may only target an opponent" $ do
    ravenousRats <- S.printingOf s registry "Ravenous Rats"
    let (src, gs) = S.addCreature ravenousRats S.bob (Setup.emptyGame S.threePlayers)
        -- The slot lives on the ENTRY TRIGGER, not the spell, so
        -- Card.allTargetSpecs (which covers the spell and the enchant slot)
        -- is the wrong door -- read the ability the card actually prints.
        specs = fmap (Modal.allTargetSpecs . TriggeredAbility.modal) (Face.triggeredAbilities (S.faceOf ravenousRats))
    case concatMap Map.elems specs of
      [theSpec] ->
        Spec.assertEqWith
          s
          "bob is excluded from his own Rats' trigger"
          (Target.legalRecipients (Just S.bob) src theSpec gs)
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        legal = Target.legalRecipients (Just S.alice) mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs3
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
      (Target.legalRecipients (Projection.controllerOf mine stolen) mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
      (Set.singleton (Recipient.ToCreature alsoTheirs))
    Spec.assertEqWith
      s
      "for bob's source, the two alice now controls"
      (Target.legalRecipients (Projection.controllerOf alsoTheirs stolen) alsoTheirs (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
      (Set.fromList [Recipient.ToCreature mine, Recipient.ToCreature theirs])
  -- P9 (#40): the reshaped TargetSpec = Pool + Maybe Filter reproduces the
  -- retired hand-carved specs as data. A black creature
  -- (Typhoid Rats, {B}) and a nonblack one (Goblin Piker, {1}{R}) exercise
  -- the Not (HasColor Black) filter that WAS NonblackCreatureTarget.
  Spec.it s "P9 Creatures + Not (HasColor Black) excludes a black creature" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
        (plainOid, gs) = S.addCreature piker S.alice gs1
        theSpec = TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))
        legal = Target.legalRecipients Nothing S.noSource theSpec gs
    Spec.assertBool s (not (Set.member (Recipient.ToCreature blackOid) legal)) "black creature illegal"
    Spec.assertBool s (Set.member (Recipient.ToCreature plainOid) legal) "nonblack creature legal"
  Spec.it s "P9 Creatures + Nothing narrows nothing" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
        (plainOid, gs) = S.addCreature piker S.alice gs1
        theSpec = TargetSpec.MkTargetSpec Pool.Creatures Nothing
        expectedAllCreatures = Set.fromList [Recipient.ToCreature blackOid, Recipient.ToCreature plainOid]
    Spec.assertEqWith s "all creatures legal" (Target.legalRecipients Nothing S.noSource theSpec gs) expectedAllCreatures
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
        specs = Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not Filter.Type.IsSource)))
    Spec.assertEqWith
      s
      "source excluded from its own set"
      (Target.legalSets Nothing srcId specs gs)
      (Map.singleton slot (Set.singleton (Recipient.ToCreature otherId)))
  -- The other half of the same claim: a slot carrying no Not IsSource does
  -- not exclude, so Prodigal Sorcerer may still ping itself (CR 115.4).
  Spec.it s "a slot without Not IsSource still admits the source (CR 115.4)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs) = S.addCreature piker S.alice gs0
        slot = SlotName.MkSlotName (Text.pack "target")
        specs = Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures Nothing)
    Spec.assertEqWith
      s
      "source is its own legal target"
      (Target.legalSets Nothing srcId specs gs)
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
    case S.spellTargetSpec terror of
      Nothing -> Spec.assertFailure s "Terror's printing carries no 'target' slot"
      Just theSpec -> do
        let gs0 = Setup.emptyGame S.bothPlayers
            (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
            (artifactOid, gs2) = S.addCreature darksteelMyr S.bob gs1
            (plainOid, gs) = S.addCreature piker S.alice gs2
            legal = Target.legalRecipients Nothing S.noSource theSpec gs
        Spec.assertBool s (not (Set.member (Recipient.ToCreature blackOid) legal)) "black creature illegal"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature artifactOid) legal)) "artifact creature illegal"
        Spec.assertBool s (Set.member (Recipient.ToCreature plainOid) legal) "nonblack, nonartifact creature legal"
  Spec.it s "Reprisal: PowerAtLeast 4 legality tracks a projected power pump" $ do
    reprisal <- S.printingOf s registry "Reprisal"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSpec reprisal of
      Nothing -> Spec.assertFailure s "Reprisal's printing carries no 'target' slot"
      Just theSpec -> do
        let gs0 = Setup.emptyGame S.bothPlayers
            (smallOid, gs) = S.addCreature piker S.bob gs0 -- power 2, {1}{R}
            legalBefore = Target.legalRecipients Nothing S.noSource theSpec gs
            pumped = S.withEffect smallOid (Modification.ModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0)) gs
            legalAfter = Target.legalRecipients Nothing S.noSource theSpec pumped
        Spec.assertBool s (not (Set.member (Recipient.ToCreature smallOid) legalBefore)) "power 2 is illegal (below the PowerAtLeast 4 floor)"
        Spec.assertBool s (Set.member (Recipient.ToCreature smallOid) legalAfter) "pumped to power 4 becomes legal"
  -- CR 508.1k: Kill Shot's IsAttacking narrowing, read off the committed card
  -- data. The defender is a creature in every other respect, so only combat
  -- status can be what separates the two.
  Spec.it s "Kill Shot: IsAttacking admits the attacker and rejects the untapped defender" $ do
    killShot <- S.printingOf s registry "Kill Shot"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSpec killShot of
      Nothing -> Spec.assertFailure s "Kill Shot's printing carries no 'target' slot"
      Just theSpec -> do
        let (board, mine, theirs) = S.combatBoardOf [piker] [piker]
            declared = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
            legal = Target.legalRecipients Nothing S.noSource theSpec declared
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
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid))
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
    Spec.assertEqWith s "slotsOf" (Resolve.slotsOf (Effect.ChangeText SubtypeFamily.CreatureType (Set.singleton Subtype.Wall) slot)) (Set.singleton slot)
  Spec.it s "CR 605 manaProduced reads AddMana, nothing else" $ do
    Spec.assertEqWith s "add mana" (ManaAbility.manaProduced (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green)))) (Just (ManaProduction.OfType (ManaType.Colored Color.Green)))
    Spec.assertEqWith s "add mana of any color" (ManaAbility.manaProduced (Effect.AddMana ManaProduction.AnyColor)) (Just ManaProduction.AnyColor)
    Spec.assertEqWith s "damage produces no mana" (ManaAbility.manaProduced (Effect.DealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "x"))) (Quantity.Literal 1))) Nothing
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
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              -- CR 700.2: Boil has one mode, and a directly-built stack object
              -- (bypassing Cast.castSpell) must stamp it chosen (mode 0), or
              -- Resolve.effectsOf/resolveSpell -- scoped to CHOSEN modes --
              -- would see no effects and no target specs at all.
              Object.bindings = Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing
            }
        g4 =
          g3
            { GameState.objects = Map.insert boilId boilObj (GameState.objects g3),
              GameState.stack = boilId : GameState.stack g3
            }
        resolve g = snd (Engine.runGamePure S.identityAnswer g (Resolve.resolveSpell boilId))
        onBattlefield oid g = Set.member oid (GameState.battlefield g)
        plain = resolve g4
        hacked = resolve (S.withEffectAt boilId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Island Subtype.Forest) g4)
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
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Map.empty,
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = Timestamp.MkTimestamp 0,
              Object.face = Nothing
            }
        g3 =
          g2
            { GameState.objects = Map.insert bloodMoonSpellId bmObj (GameState.objects g2),
              GameState.stack = bloodMoonSpellId : GameState.stack g2
            }
        hacked = S.withEffectAt bloodMoonSpellId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g3
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
        ability = case Face.activatedAbilities (S.faceOf prodigalSorcerer) of
          ab : _ -> ab
          [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime
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
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing
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
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter SearchDestination.BattlefieldTapped]) Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1))
            ActivationTiming.AnyTime
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing ts Nothing
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
    Spec.assertEqWith s "one permanent on the battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice resolved)) 1
    Spec.assertEqWith s "it is tapped" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice resolved) []
  Spec.it s "CR 701.23b Search may fail to find" $ do
    mountain <- S.printingOf s registry "Mountain"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addLibraryCard mountain S.alice base
        ability = ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter SearchDestination.BattlefieldTapped]) Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj = Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing ts Nothing
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure findNothing g4 Stack.resolveTop)
    Spec.assertEqWith s "nothing entered the battlefield" (GameState.battlefield resolved) Set.empty
  Spec.it s "CR 701.23a Search (And [HasCardType Land, HasSupertype Basic]) offers a basic land, not a nonland" $ do
    -- P9: the Search filter reads each library card through the PRINTED-card
    -- view (Projection.viewOfCard) -- a card in a library has no projection.
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
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter SearchDestination.BattlefieldTapped]) Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1))
            ActivationTiming.AnyTime
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing ts Nothing
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
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter SearchDestination.BattlefieldTapped]) Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1))
            ActivationTiming.AnyTime
        (abilId, g2) = Game.freshObjectId g1
        (ts, g3) = Game.freshTimestamp g2
        abilObj =
          Object.MkObject S.alice Nothing (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing Nothing Nothing ts Nothing
        g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
        resolved = snd (Engine.runGamePure (findForbidden pikerId) g4 Stack.resolveTop)
    Spec.assertEqWith s "the Piker was NOT fetched to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice resolved) 0
    Spec.assertBool s (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)) "it is still in the library"
    Spec.assertEqWith s "and nothing else was fetched either" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Mountain") S.alice resolved) 0
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
            (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.ExileAllGraveyards]) Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1))
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
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing
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
                  (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.ControlPlayerNextTurn slot]) (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Players Nothing)) Optionality.Mandatory))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.timing = ActivationTiming.AnyTime
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
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing
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
                  (Seq.singleton (Mode.MkMode (Seq.singleton Effect.RestartGame) Map.empty Optionality.Mandatory))
                  (ModeSelection.ChooseExactly 1),
              ActivatedAbility.timing = ActivationTiming.AnyTime
            }
        abilObj =
          Object.MkObject
            { Object.owner = S.bob,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfAbility aliceId ability,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.bob,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing
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
              Face.typeLine = Face.typeLine (S.faceOf lightningBolt),
              Face.power = Nothing,
              Face.toughness = Nothing,
              Face.loyalty = Nothing,
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.empty,
              Face.staticAbilities = [],
              Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage (ObjectRef.InSlot slot) (Quantity.Literal 3)]) Map.empty Optionality.Mandatory))
                  (ModeSelection.ChooseExactly 1),
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.characteristicPT = Nothing,
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.attackCosts = [],
              Face.mulliganAction = [],
              Face.openingHandAction = [],
              Face.additionalCosts = [],
              Face.alternativeCosts = [],
              Face.enchant = Nothing,
              Face.counterability = Counterability.Counterable
            }
        spellObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfToken card,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing
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
              Face.typeLine = Face.typeLine (S.faceOf lightningBolt),
              Face.power = Nothing,
              Face.toughness = Nothing,
              Face.loyalty = Nothing,
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.empty,
              Face.staticAbilities = [],
              Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage (ObjectRef.InSlot slot) (Quantity.Literal 3)]) Map.empty Optionality.Mandatory))
                  (ModeSelection.ChooseExactly 1),
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.characteristicPT = Nothing,
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.attackCosts = [],
              Face.mulliganAction = [],
              Face.openingHandAction = [],
              Face.additionalCosts = [],
              Face.alternativeCosts = [],
              Face.enchant = Nothing,
              Face.counterability = Counterability.Counterable
            }
        spellObj =
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfToken card,
              Object.zone = Zone.Stack,
              Object.tapped = TapState.Untapped,
              Object.damage = 0,
              Object.sickness = Sickness.Settled S.alice,
              Object.bindings = Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              Object.timestamp = ts,
              Object.face = Nothing
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice fogId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        combat = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False 0 Nothing DamageKind.Combat])
        spell = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False 0 Nothing DamageKind.Noncombat])
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
        cast = snd (Engine.runGamePure atBobAnswer gs2 (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure atBobAnswer gs2 (Cast.castSpell S.alice spellId))
        (_, cast1) = S.addHandCard piker S.bob cast
        before = S.lifeOf S.bob cast1
        after = snd (Engine.runGamePure atBobAnswer cast1 Stack.resolveTop)
    Spec.assertEqWith s "three damage" (S.lifeOf S.bob after) (fmap (subtract 3) before)
  Spec.it s "the same count with Relative You reads the caster's hand" $ do
    -- The direct contrast: the SAME Count shape (InZone Hand, Objects) that
    -- Sudden Impact scopes with PlayerRef.InSlot also serves Inner Calm,
    -- Outer Strength's PlayerRef.Relative You -- one shape, two
    -- perspectives, neither welded into a constructor.
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
        gs = fill S.alice 5 (fill S.bob 2 gs0)
        yourHand =
          Count.Type.MkCount
            (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
            (Filter.Type.And [])
            Aggregation.Objects
    Spec.assertEqWith
      s
      "Alice's five"
      (S.countOf (\oid -> Just (Projection.viewOfObject oid gs)) (Filter.MkContext (Just S.alice) Nothing) gs yourHand)
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
        cast = snd (Engine.runGamePure (atCreature wall) gs4 (Cast.castSpell S.alice spellId))
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
              (Map.singleton slot True)
              (Map.singleton slot (Recipient.ToCreature pikerId))
              (Effect.ModifyTarget Duration.UntilEndOfTurn m (ObjectRef.InSlot slot))
        refused = store (Modification.ModifyPowerToughness (Quantity.Literal 3) Quantity.Star)
        stored = store (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))
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
                (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.ControlPlayerNextTurn slot]) (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Players Nothing)) Optionality.Mandatory))
                (ModeSelection.ChooseExactly 1),
            ActivatedAbility.timing = ActivationTiming.AnyTime
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
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer target)) Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing
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

findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches -> case matches of
    m : _ -> Just m
    [] -> Nothing
  _ -> S.identityAnswer p

-- Names a card the search filter did NOT admit -- the lying interpreter #222 is
-- about. Parameterised so the test can point it at a specific nonland.
findForbidden :: ObjectId.ObjectId -> Prompt.Prompt r -> r
findForbidden wanted p = case p of
  Prompt.SearchLibrary {} -> Just wanted
  _ -> S.identityAnswer p

findNothing :: Prompt.Prompt r -> r
findNothing p = case p of
  Prompt.SearchLibrary {} -> Nothing
  _ -> S.identityAnswer p

-- Casts every castable spell (targets via lookupMin: creatures first),
-- otherwise passes. Drives the Bolt-vs-Bolt integration falsifier.
boltAnswer :: Prompt.Prompt r -> r
boltAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          A.Cast _ -> True
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
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.timestamp = Timestamp.MkTimestamp 0,
            Object.face = Nothing
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
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cancelId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (victimId, resolved)

-- Append a second card of `printing` to `pid`'s hand (handOne overwrites the hand,
-- so a second in-hand card must be appended, not re-inserted).
handAppend :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
handAppend printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      obj = Object.MkObject pid Nothing (Source.OfCard printing) Zone.Hand TapState.Untapped 0 (Sickness.Settled pid) Map.empty Map.empty Nothing Nothing Nothing (Timestamp.MkTimestamp 0) Nothing
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
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject victimId)) sets
        _ -> S.identityAnswer p
      castA = snd (Engine.runGamePure atVictim gs2 (Cast.castSpell S.alice cancelA))
      castB = snd (Engine.runGamePure atVictim castA (Cast.castSpell S.alice cancelB))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cancelId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the countered spell is not in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    Spec.assertEqWith s "the countered spell is exiled" (length (Game.zoneMembers Zone.Exile S.bob resolved)) 1

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
          Cast.castSpell S.alice hackId
          Cast.castSpell S.bob cancelId
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
        ((_, cast), castTranscript) = Replay.record hackToIsland gs (Cast.castSpell S.alice hackId)
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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
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
      onStack = S.runPure (aimAtCreature wraithId) g3 (Cast.castSpell S.alice turnToFrogId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            Cast.castSpell S.alice evolutionId
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
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Recipient.ToObject oid)) sets)
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
      onStack = S.runPure S.identityAnswer g3 (Cast.castSpell S.alice fodderId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            Cast.castSpell S.alice evolutionId
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
            Cast.castSpell S.alice evolutionId
            Stack.resolveTop
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan upkeep S.alice)
          (evolved {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      onStack = S.runPure S.identityAnswer begun Engine.settleForPriority
      after = S.runPure S.identityAnswer onStack Engine.priorityLoop
  pure (S.tokensOf after, after)

-- Aims every target slot at `oid` as a creature (Turn to Frog's Pool.Creatures
-- recipient shape); the board holds more than one creature, so the choice has to
-- be answered rather than forced by construction.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature oid)) sets
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
                Cast.castSpell S.alice spellId
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
        onStack = S.runPure S.identityAnswer g3 (Cast.castSpell S.alice secondId)
        spellId = case GameState.stack onStack of
          top : _ -> top
          [] -> ObjectId.MkObjectId 999
        -- The first Evolution replaces the second's every Wall with Frog.
        evolved = S.runPure (evolveAt spellId Subtype.Wall Subtype.Frog) onStack $ do
          Cast.castSpell S.alice firstId
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
    mapM_ (\oid -> Spec.assertEqWith s "named Goblin Token" (Projection.nameOf oid after) (CardName.MkCardName (Text.pack "Goblin Token"))) tokens

  -- And the point. BOTH halves of CR 612.2a: the type line, and the name those
  -- same words define.
  Spec.it s "CR 612.2a whole card: an evolved Dragon Fodder mints Elves, name and all" $ do
    (tokens, after) <- dragonFodderChain s registry (Just (Subtype.Goblin, Subtype.Elf))
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf oid after) (Set.singleton Subtype.Elf)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Token" (Projection.nameOf oid after) (CardName.MkCardName (Text.pack "Elf Token"))) tokens

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
    mapM_ (\oid -> Spec.assertEqWith s "named Faerie Rogue Token" (Projection.nameOf oid after) (CardName.MkCardName (Text.pack "Faerie Rogue Token"))) tokens

  Spec.it s "CR 612.2a whole card: an evolved Bitterblossom's trigger mints an Elf Rogue Token" $ do
    (tokens, after) <- bitterblossomChain s registry (Just (Subtype.Faerie, Subtype.Elf))
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf Rogue" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Elf, Subtype.Rogue])) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Rogue Token" (Projection.nameOf oid after) (CardName.MkCardName (Text.pack "Elf Rogue Token"))) tokens

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
          Cast.castSpell S.alice evolutionId
          Stack.resolveTop
    Spec.assertEqWith s "Creature -- Elf Warrior" (Projection.subtypesOf pikerId after) (Set.fromList [Subtype.Elf, Subtype.Warrior])
    Spec.assertEqWith s "and the name is untouched" (Projection.nameOf pikerId after) (CardName.MkCardName (Text.pack "Goblin Piker"))

-- The one activated ability of a printing that declares exactly one -- Prodigal
-- Sorcerer's {T}, which is all these fixtures reach for. Nothing for any other
-- printing, so a card that grew a second ability fails the case that names it
-- rather than silently picking whichever came first (Pawl.TargetSpec's
-- soleTargetSpec is the same shape for the same reason).
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
soleActivatedAbility p = case Face.activatedAbilities (S.faceOf p) of
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
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.alice)) sets
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
            cast = List.foldl' (\g oid -> S.runPure S.identityAnswer g (Cast.castSpell S.alice oid)) activated stifleIds
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
        cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice pikerId)
        -- The Piker resolves and enters; CR 603.3 then puts Aether Flash's
        -- trigger on the stack the next time a player would receive priority.
        entered = S.runPure S.identityAnswer cast Stack.resolveTop
        placed = S.runPure S.identityAnswer entered Engine.settleForPriority
        stifled = S.runPure S.identityAnswer placed (Cast.castSpell S.bob stifleId)
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
    Spec.assertEqWith s "no damage was ever dealt" (fmap DamageEvent.amount (Maybe.mapMaybe Event.damageOf (Foldable.toList (GameState.events after)))) []
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
        let castAll g oid = S.runPure S.identityAnswer g (Cast.castSpell S.alice oid)
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
  -- fizzle happened: with a single spec'd slot alone, fizzling and
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
        specs = Map.singleton targetSlot (TargetSpec.MkTargetSpec Pool.Creatures Nothing)
        (abilId, base4) = S.spellOnStack piker S.alice base3
        -- Mirrors Engine.placeOne's own construction: a real chosen
        -- target under `targetSlot`, plus the reserved self slot every
        -- placed trigger carries (Binding.setTriggerSource).
        bindings =
          Binding.setTriggerSource
            source
            (Binding.fromChoices (Map.singleton targetSlot (Recipient.ToCreature victim)) Nothing Set.empty)
        withBindings = base4 {GameState.objects = Map.adjust (\o -> o {Object.bindings = bindings}) abilId (GameState.objects base4)}
        -- Kill the sole real target before resolution: CR 608.2b makes it
        -- illegal (it's no longer a legal CreatureTarget), while the
        -- reserved slot -- never targeted -- stays vacuously legal.
        gone = S.runPure S.identityAnswer withBindings (Event.changeZone victim Zone.Graveyard)
        mode = Mode.MkMode (Seq.fromList [Effect.Destroy (ObjectRef.InSlot targetSlot) Regenerability.Regenerable Nothing, Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)]) specs Optionality.Mandatory
        run = Resolve.resolveModes abilId source [(ModeIndex.MkModeIndex 0, mode)]
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
            fmap (const (Recipient.ToPlayer S.bob)) sets
          Prompt.ChooseAction _ _ actions ->
            case filter (\a -> a == A.Cast oid) actions of
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
        wounded = S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True False 0 Nothing DamageKind.Combat)] gs
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
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (foeId, resolved)

-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p

-- atBobAnswer's creature counterpart: aim every target slot at one named
-- creature, rather than at whatever Set.lookupMin happens to offer first.
atCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
atCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature oid)) sets
  _ -> S.identityAnswer p

-- Add k cards of a printing to pid's hand (each a fresh Hand-zone object).
handCards :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
handCards printing pid k gs = List.foldl' (\g _ -> addOne g) gs [1 .. k]
  where
    addOne g =
      let (oid, g1) = Game.freshObjectId g
          obj = Object.MkObject pid Nothing (Source.OfCard printing) Zone.Hand TapState.Untapped 0 (Sickness.Settled pid) Map.empty Map.empty Nothing Nothing Nothing (Timestamp.MkTimestamp 0) Nothing
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
      began = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "the shielded creature survived Murder" (S.creaturesInPlay S.bob after) 1
  Spec.it s "CR 400.7 Unsummon returns a creature to its owner's hand" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (_, withPiker) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne unsummon withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
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
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure noDiscard gs (Cast.castSpell S.alice spellId))
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
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure noDiscard gs (Cast.castSpell S.alice spellId))
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
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure onlyLast gs (Cast.castSpell S.alice spellId))
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
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure sameTwice gs (Cast.castSpell S.alice spellId))
        after = snd (Engine.runGamePure sameTwice cast Stack.resolveTop)
    Spec.assertEqWith s "two distinct cards discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "one card left in bob's hand" (S.handSize S.bob after) 1

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
        cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        isDamage ev = case ev of
          GameEvent.DamageDealt _ -> True
          _ -> False
    Spec.assertEqWith s "bob drew two" (S.handSize S.bob after) 2
    Spec.assertEqWith s "and lost two life" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "alice, who cast it, lost none" (S.lifeOf S.alice after) (S.lifeOf S.alice gs)
    Spec.assertBool s (not (any isDamage (GameState.events after))) "no damage was dealt (CR 119.2)"
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
        cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob is at 0" (S.lifeOf S.bob after) (Just 0)
    Spec.assertEqWith s "and alice wins" (GameState.result (S.settleSba after)) (Just (Result.Won S.alice))

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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and her library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 10

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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
  Spec.it s "CR 122.2 Unsummon removes a counter-bearing creature's counters" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (victim, withFoe) = S.addCreature piker S.bob base
        withCounter = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
        (gs, spellId) = S.handOne unsummon withCounter
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- Total (no `head`): expect exactly one bounced card in hand, empty counters.
        handCounters = fmap (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h after)) (Game.zoneMembers Zone.Hand S.bob after)
    Spec.assertEqWith s "the bounced incarnation in hand has no counters" handCounters [Map.empty]

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
            (Map.singleton slot True)
            (Map.singleton slot (Recipient.ToCreature oid))
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
            (Map.singleton slot True)
            (Map.singleton slot (Recipient.ToCreature oid))
            (Effect.GainControl Duration.UntilEndOfTurn (ObjectRef.InSlot slot))
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
            (Map.singleton slot True)
            (Map.singleton slot (Recipient.ToCreature oid))
            (Effect.GainControl Duration.UntilEndOfTurn (ObjectRef.InSlot slot))
        after = snd (Engine.runGamePure S.identityAnswer settled run)
    Spec.assertEqWith s "alice controlled it before" (Projection.controllerOf oid settled) (Just S.alice)
    Spec.assertEqWith s "and still does" (Projection.controllerOf oid after) (Just S.alice)
    Spec.assertEqWith s "its settle under alice is untouched" (fmap Object.sickness (Game.lookupObject oid after)) (Just (Sickness.Settled S.alice))

gainPlayerCountersSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
gainPlayerCountersSpec s registry = Spec.describe s "GainPlayerCounters" $ do
  Spec.it s "CR 107.14 GainPlayerCounters gives the resolving controller energy" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        act = Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.GainPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2))
        after = S.runPure S.identityAnswer gs0 act
    Spec.assertEqWith s "alice has two energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2

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
  -- producer until Prologue to Phyresis (#267). The arm was implemented and
  -- unproven, which design.md section 4 says is not done.
  Spec.it s "CR 122.1 whole card: Prologue to Phyresis poisons the opponent, not the caster" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    prologueToPhyresis <- S.printingOf s registry "Prologue to Phyresis"
    let base = S.landsInPlay island 2
        (_, withLibrary) = S.addLibraryCard piker S.alice base
        handBefore = S.handSize S.alice withLibrary
        (gs, spellId) = S.handOne prologueToPhyresis withLibrary
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
  -- The counter placement rides Replacement.putCounters, so CR 614's counter
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
        afterCast = S.runPure proliferatesAll withSpell (Cast.castSpell S.alice spell)
        resolved = S.runPure proliferatesAll afterCast Stack.resolveTop
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the counter was proliferated" (S.counterOf CounterKind.PlusOnePlusOne creature resolved) 2
    -- The spell left the hand and one card was drawn, so the hand is level.
    Spec.assertEqWith s "drew a card" (length (Game.zoneMembers Zone.Hand S.alice resolved)) handBefore

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
    Map.mapMaybe
      (\legal -> if Set.member (Recipient.ToPlayer victim) legal then Just (Recipient.ToPlayer victim) else Set.lookupMin legal)
      sets
  _ -> S.identityAnswer p

-- A lying interpreter: names `wanted` for a sacrifice regardless of whether it
-- was offered. The only way to reach CR 701.21a's guard from a test, since the
-- candidate list is built from what the sacrificing player controls.
namesInstead :: ObjectId.ObjectId -> Prompt.Prompt r -> r
namesInstead wanted p = case p of
  Prompt.ChooseSacrifices {} -> Set.singleton wanted
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseSacrifices with `wanted`, when it is on offer. A pair of
-- tests differing only in this argument proves the ANSWER decides which permanent
-- is sacrificed, rather than the order the candidates are enumerated in.
sacrifices :: ObjectId.ObjectId -> Prompt.Prompt r -> r
sacrifices wanted p = case p of
  Prompt.ChooseSacrifices _ _ _ candidates _ ->
    if elem wanted candidates then Set.singleton wanted else Set.fromList (take 1 candidates)
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
        edict = Resolve.applyEffect src src S.alice (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))
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
        after = S.runPure (namesInstead hers) gs (Resolve.applyEffect src src S.alice (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
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
        act = Resolve.applyEffect src src S.alice (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))
        asked = State.execState (Engine.runGame countingAnswer gs act) 0
        after = S.runPure S.identityAnswer gs act
    Spec.assertEqWith s "nothing to choose" asked 0
    Spec.assertBool s (not (S.onBattlefield his after)) "but it still died"
  -- CR 609.3 again: a player with no creatures sacrifices nothing, and the
  -- edict simply does as much as it can -- which is nothing.
  Spec.it s "CR 609.3 an edict against an empty board does nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer gs (Resolve.applyEffect src src S.alice (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
    Spec.assertBool s (S.onBattlefield src after) "alice keeps hers"
  -- The gameplay-level proof: the real card, cast and resolved.
  Spec.it s "Diabolic Edict whole card: cast off two Swamps, bob sacrifices" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    diabolicEdict <- S.printingOf s registry "Diabolic Edict"
    let base = S.landsInPlay swamp 2
        (his, g1) = S.addCreature piker S.bob base
        (withSpell, spell) = S.handOne diabolicEdict g1
        afterCast = S.runPure (targetsPlayer S.bob) withSpell (Cast.castSpell S.alice spell)
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
    Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (GameState.events after)) "a BecameMonarch event was recorded"

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
            (Map.singleton slot True)
            (Map.singleton slot (Recipient.ToCreature oid))
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
            (Map.singleton slot True)
            (Map.singleton slot (Recipient.ToCreature oid))
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
            (Map.singleton slot True)
            (Map.singleton slot (Recipient.ToCreature oid))
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
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice spellId))
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
        Prompt.ChooseOptional (Decider.MkDecider d) player _ idx
          | d == S.alice && player == S.alice && idx == ModeIndex.MkModeIndex 0 ->
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
      afterCast = S.runPure S.identityAnswer withSpell (Cast.castSpell S.alice spell)
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
  -- card declares no target spec and the cast never raises a target prompt
  -- -- and CR 608.2b, which is about targets, has nothing to fizzle.
  Spec.it s "CR 115.10a Day of Judgment targets nothing: no target spec and no target prompt" $ do
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
        asked = State.execState (Engine.runGame countingAnswer withSpell (Cast.castSpell S.alice spell)) 0
    Spec.assertEqWith s "no target spec anywhere on the card" (Modal.allTargetSpecs (Face.spell (Card.combined card))) Map.empty
    Spec.assertEqWith s "and nothing was asked to target" asked 0
    -- The board still resolves the way the first test says it does, from the
    -- same cast -- so "targets nothing" is not "affects nothing".
    Spec.assertBool s (not (S.onBattlefield his (castDayOfJudgment plains dayOfJudgment g1))) "the creature still died"

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
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spell)
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
      afterCast = S.runPure S.identityAnswer withSpell (Cast.castSpell S.alice spell)
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
    case S.spellTargetSpec plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSpec -> do
        let (flierId, gs1) = S.addCreature birdMaiden S.bob (Setup.emptyGame S.bothPlayers)
            (groundId, gs) = S.addCreature piker S.bob gs1
            legal = Target.legalRecipients Nothing S.noSource theSpec gs
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
    case S.spellTargetSpec plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSpec -> do
        let (groundId, before) = S.addCreature piker S.alice (S.landsInPlay plains 3)
            (withSpell, spellId) = S.handOne spontaneousFlight before
            cast = snd (Engine.runGamePure S.identityAnswer withSpell (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        Spec.assertBool s (not (Set.member (Recipient.ToCreature groundId) (Target.legalRecipients Nothing S.noSource theSpec before))) "no flying, no offer"
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying groundId after) "the grant landed"
        Spec.assertBool s (Set.member (Recipient.ToCreature groundId) (Target.legalRecipients Nothing S.noSource theSpec after)) "and the grant makes it a legal target"
  -- The other direction, and the one that proves the read is not of the printed
  -- card: Humility (CR 613.1f, "all creatures lose all abilities") takes the
  -- flying off a creature that PRINTS it, and the offer goes with it.
  Spec.it s "CR 613.1f Humility strips the printed flying, and the offer goes with it" $ do
    plummet <- S.printingOf s registry "Plummet"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    humility <- S.printingOf s registry "Humility"
    case S.spellTargetSpec plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSpec -> do
        let (flierId, before) = S.addCreature birdMaiden S.bob (Setup.emptyGame S.bothPlayers)
            after = S.withHumility humility before
        Spec.assertBool s (Set.member (Recipient.ToCreature flierId) (Target.legalRecipients Nothing S.noSource theSpec before)) "legal while it flies"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying flierId after)) "Humility took the flying"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature flierId) (Target.legalRecipients Nothing S.noSource theSpec after))) "so it is no longer a legal target"
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
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure answerXTwo gs (Cast.castSpell S.alice spellId))
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
        cast = snd (Engine.runGamePure answerXTwo gs (Cast.castSpell S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure answerXTwo cast Stack.resolveTop))
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying maidenId after)) "Humility took the flying"
    Spec.assertEqWith s "three Forests paid {2}{G} all the same" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "no damage marked on the grounded Bird Maiden" (markedOn maidenId after) (Just 0)
    Spec.assertBool s (S.onBattlefield maidenId after) "so it survives"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  targetSpec s registry
  plummetSpec s registry
  corrosiveGaleSpec s registry
  resolveSpec s registry
  fizzleSpec s registry
  indestructibleSpec s registry
  zoneChangeSpec s registry
  drawCardSpec s registry
  loseLifeSpec s registry
  greatestSpec s registry
  counterSpec s registry
  magicalHackTimingSpec s registry
  artificialEvolutionSpec s registry
  stifleSpec s registry
  countersSpec s registry
  untapSpec s registry
  gainControlSpec s registry
  gainPlayerCountersSpec s registry
  proliferateSpec s registry
  playerSacrificesSpec s registry
  createEmblemSpec s registry
  becomeMonarchSpec s registry
  exileUntilMonarchSpec s registry
  actOfTreasonSpec s registry
  optionalEffectSpec s registry
  destroyAllSpec s registry
  trumpetBlastSpec s registry
  auraThiefSpec s registry
  baneOfProgressSpec s registry
