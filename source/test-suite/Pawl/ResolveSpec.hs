{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Resolve and Pawl.Target: targeting legality, spell resolution, and
-- the CR 608.2b fizzle.
module Pawl.ResolveSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Activate as Activate
import qualified Pawl.Binding as Binding
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Monarch as Monarch
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Replay as Replay
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
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
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
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
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

targetTests :: Registry.Type.Registry -> Tasty.TestTree
targetTests registry =
  Tasty.testGroup
    "Target"
    [ HU.testCase "CR 115.4 AnyTarget offers every creature and every playing player" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        HU.assertEqual
          "creature and both players"
          (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])
          (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs),
      HU.testCase "a departed player is not a legal target" $
        let gs = Departure.depart Departure.Type.Lost S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertBool
              "bob gone"
              (not (Set.member (Recipient.ToPlayer S.bob) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs))),
      HU.testCase "CR 800.4b an object does not change to the control of a player who has left the game" $ do
        -- CR 800.4b: "If an object would change to the control of a player who has
        -- left the game, it doesn't." Resolve.applyEffect takes the controller
        -- explicitly, which is what makes this testable: the effect is asked to
        -- resolve on behalf of a player who is no longer in the game.
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (myr, board) = S.addCreature darksteelMyr S.carol S.threePlayerGame
            gone = Departure.depart Departure.Type.Conceded S.bob board
            slot = SlotName.MkSlotName (Text.pack "target")
            after =
              S.runPure S.identityAnswer gone $
                Resolve.applyEffect
                  S.noSource
                  S.bob
                  Map.empty
                  (Map.singleton slot True)
                  (Map.singleton slot (Recipient.ToObject myr))
                  (Effect.GainControl Duration.Indefinite slot)
            control =
              S.runPure S.identityAnswer board $
                Resolve.applyEffect
                  S.noSource
                  S.bob
                  Map.empty
                  (Map.singleton slot True)
                  (Map.singleton slot (Recipient.ToObject myr))
                  (Effect.GainControl Duration.Indefinite slot)
        HU.assertEqual "no control effect is stored for a departed controller" [] (GameState.continuousEffects after)
        HU.assertEqual "and the Myr's controller is unchanged" (Just S.carol) (Projection.controllerOf myr after)
        HU.assertEqual "the same call for a player still in the game DOES store one -- the guard is what did it" 1 (length (GameState.continuousEffects control))
        HU.assertEqual "and takes control" (Just S.bob) (Projection.controllerOf myr control),
      HU.testCase "CR 608.2b a creature that left its zone is no longer legal" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
        HU.assertBool "legal while fielded" (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs)
        HU.assertBool "illegal once moved" (not (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gone)),
      HU.testCase "legalSets maps each slot to its legal recipients" $
        let specs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)
            gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual
              "one slot, two players"
              (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
              (Target.legalSets Nothing S.noSource specs gs),
      HU.testCase "CR 115.4 CreatureTarget offers creatures but no players" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        HU.assertEqual
          "just the creature"
          (Set.singleton (Recipient.ToCreature oid))
          (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gs),
      HU.testCase "CR 601.2c CreatureTarget has an empty legal set with no creatures" $
        HU.assertBool
          "nothing to target"
          (Set.null (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures Nothing) (Setup.emptyGame S.bothPlayers))),
      HU.testCase "CR 608.2b a creature that left is no longer a legal CreatureTarget" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
        HU.assertBool "legal while fielded" (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gs)
        HU.assertBool "illegal once moved" (not (Target.stillLegal Nothing S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gone)),
      HU.testCase "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (permId, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        HU.assertBool
          "the permanent is a legal object target"
          (Set.member (Recipient.ToObject permId) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.SpellsAndPermanents Nothing) gs)),
      HU.testCase "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $ do
        piker <- Registry.printing registry "Goblin Piker"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (permId, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            (spellId, gs) = S.spellOnStack lightningBolt S.alice base
            legal = Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Spells Nothing) gs
        HU.assertBool "the stack spell is a legal target" (Set.member (Recipient.ToObject spellId) legal)
        HU.assertBool "the battlefield permanent is not a legal target" (not (Set.member (Recipient.ToObject permId) legal)),
      HU.testCase "LandTarget offers a land as an object target, not a creature or player" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
        HU.assertBool "the land is legal" (Set.member (Recipient.ToObject landId) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs))
        HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs))),
      HU.testCase "CR 115: PlayerTarget is exactly the players still in the game" $
        let gs = Setup.emptyGame S.bothPlayers
            expected = Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]
         in HU.assertEqual "both players, no creatures" expected (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Players Nothing) gs),
      -- CR 115.1a / 700.2c: "target Wall" (Chaos Charm) restricts CreatureTarget to
      -- creatures whose PROJECTED subtypes include Wall. Wall of Stone (a real 0/8
      -- Creature - Wall, M4g) is the Wall; a Piker is the non-Wall control.
      HU.testCase "CR 115.1a / 700.2c \"target Wall\" offers a Wall creature but not a non-Wall creature" $ do
        wallOfStone <- Registry.printing registry "Wall of Stone"
        piker <- Registry.printing registry "Goblin Piker"
        let (wallId, base) = S.addCreature wallOfStone S.bob (Setup.emptyGame S.bothPlayers)
            (pikerId, gs) = S.addCreature piker S.alice base
            slot = SlotName.MkSlotName (Text.pack "target")
            legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing S.noSource (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
        HU.assertBool "the Wall is legal" (Set.member (Recipient.ToCreature wallId) legal)
        HU.assertBool "the non-Wall creature is not legal" (not (Set.member (Recipient.ToCreature pikerId) legal)),
      -- The same "target Wall", against a Wall that Ashaya animated into a land
      -- and Blood Moon then set to Mountain. CR 305.7 retires the land's OLD LAND
      -- TYPES and nothing else on the subtype axis, and its fourth sentence keeps
      -- the card types -- so the Wall is still a creature, still a Wall, and still
      -- a legal target. This is the gameplay-level half of Pawl.ProjectionSpec's
      -- "a Blood Moon'd creature-land keeps its creature types".
      HU.testCase "CR 305.7 an Ashaya-animated, Blood Moon'd Wall of Stone is still a legal \"target Wall\"" $ do
        wallOfStone <- Registry.printing registry "Wall of Stone"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (wallId, g1) = S.addCreature wallOfStone S.alice (Setup.emptyGame S.bothPlayers)
            (_, g2) = S.addCreature ashaya S.alice g1
            (_, gs) = S.addCreature bloodMoon S.alice g2
            slot = SlotName.MkSlotName (Text.pack "target")
            legal = Map.findWithDefault Set.empty slot (Target.legalSets Nothing S.noSource (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
        HU.assertBool "it really is a Mountain" (Set.member Subtype.Mountain (Projection.subtypesOf wallId gs))
        HU.assertBool "and still a creature (CR 305.7 removes no card types)" (Projection.isCreatureOf wallId gs)
        HU.assertBool "so \"target Wall\" still offers it" (Set.member (Recipient.ToCreature wallId) legal),
      HU.testCase "CR 115.1a ArtifactTarget is the battlefield's projected artifacts" $ do
        -- boardWithCreatureArtifactLand: alice has a Piker, a Mindslaver
        -- (Legendary Artifact) and a Mountain.
        piker <- Registry.printing registry "Goblin Piker"
        mindslaver <- Registry.printing registry "Mindslaver"
        mountain <- Registry.printing registry "Mountain"
        let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
            legal = Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))) gs
        HU.assertEqual "exactly the artifact" (Set.singleton (Recipient.ToObject (S.artifactId gs))) legal
        HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) legal)),
      HU.testCase "CR 115.1a / 109.5 OpponentCreatureTarget excludes the source's controller's creatures" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let gs0 = Setup.emptyGame S.bothPlayers
            (mine, gs1) = S.addCreature piker S.alice gs0
            (theirs, gs2) = S.addCreature warMammoth S.bob gs1
            legal = Target.legalRecipients (Just S.alice) mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs2
        HU.assertEqual "only the opponent's creature" (Set.singleton (Recipient.ToCreature theirs)) legal
        HU.assertBool "not the source's controller's own" (not (Set.member (Recipient.ToCreature mine) legal)),
      -- CR 115.1 / 109.5: "target OPPONENT". Until Ravenous Rats there was no
      -- card in the pool that narrowed a PLAYER target, so Target.legalRecipients
      -- kept every player unconditionally (#168). Three seats, so "an opponent"
      -- is a real set rather than the only other player.
      HU.testCase "CR 115.1 a Players pool narrowed by IsPlayer Opponent excludes the source's controller" $ do
        ravenousRats <- Registry.printing registry "Ravenous Rats"
        let (src, gs) = S.addCreature ravenousRats S.alice (Setup.emptyGame S.threePlayers)
            spec = TargetSpec.MkTargetSpec Pool.Players (Just (Filter.Type.IsPlayer PlayerRelation.Opponent))
            legal = Target.legalRecipients (Just S.alice) src spec gs
        HU.assertEqual
          "exactly bob and carol, never alice"
          (Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol])
          legal,
      -- The card itself, so the narrowing is proven through the real target spec
      -- the JSON carries rather than one hand-built in the test.
      HU.testCase "CR 115.1 Ravenous Rats' entry trigger may only target an opponent" $ do
        ravenousRats <- Registry.printing registry "Ravenous Rats"
        let (src, gs) = S.addCreature ravenousRats S.bob (Setup.emptyGame S.threePlayers)
            -- The slot lives on the ENTRY TRIGGER, not the spell, so
            -- Card.allTargetSpecs (which covers the spell and the enchant slot)
            -- is the wrong door -- read the ability the card actually prints.
            specs = fmap (Modal.allTargetSpecs . TriggeredAbility.modal) (Card.Type.triggeredAbilities (Printing.card ravenousRats))
        case concatMap Map.elems specs of
          [spec] ->
            HU.assertEqual
              "bob is excluded from his own Rats' trigger"
              (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.carol])
              (Target.legalRecipients (Just S.bob) src spec gs)
          _ -> HU.assertFailure "Ravenous Rats should declare exactly one target slot",
      -- The gameplay-level proof design.md section 4 asks for: an opcode is not
      -- done until a card exercises it end to end. Ravenous Rats enters, its
      -- trigger is placed and targeted from the narrowed set, and an OPPONENT
      -- loses a card from hand -- not alice, who cast it.
      HU.testCase "CR 115.1 Ravenous Rats' entry trigger makes an opponent discard, never its own controller" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        ravenousRats <- Registry.printing registry "Ravenous Rats"
        let base0 = S.landsInPlay swamp 2
            -- Both players hold a card, so "whose hand shrank" is a real question.
            (_, base1) = S.addHandCard piker S.bob base0
            (gs, spellId) = S.handOne ravenousRats base1
            aliceBefore = S.handSize S.alice gs
            bobBefore = S.handSize S.bob gs
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
        HU.assertEqual "bob discarded one" (bobBefore - 1) (S.handSize S.bob settled)
        HU.assertEqual "alice lost only the Rats she cast" (aliceBefore - 1) (S.handSize S.alice settled)
        HU.assertEqual "the Rats resolved onto the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Ravenous Rats") S.alice settled),
      HU.testCase "CR 806.1 at three seats a ControlledBy Opponent pool spans BOTH opponents' creatures" $ do
        -- Palace Jailer's second trigger targets a creature an opponent controls.
        -- At three seats that is a choice across two boards, and the engine must
        -- offer all of it. DISCRIMINATING: a relation resolved as "the next seat"
        -- offers only bob's, and carol is deliberately the far seat -- so an
        -- implementation that took one opponent fails on the set equality, not on
        -- a membership check that a superset would also satisfy.
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let gs0 = Setup.emptyGame S.threePlayers
            (mine, gs1) = S.addCreature piker S.alice gs0
            (bobs, gs2) = S.addCreature warMammoth S.bob gs1
            (carols, gs3) = S.addCreature piker S.carol gs2
            legal = Target.legalRecipients (Just S.alice) mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs3
        HU.assertEqual
          "exactly bob's and carol's, and nothing of alice's"
          (Set.fromList [Recipient.ToCreature bobs, Recipient.ToCreature carols])
          legal,
      HU.testCase "CR 613.1b OpponentCreatureTarget follows PROJECTED control, not ownership" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let gs0 = Setup.emptyGame S.bothPlayers
            (mine, gs1) = S.addCreature piker S.alice gs0
            (theirs, gs2) = S.addCreature warMammoth S.bob gs1
            (alsoTheirs, gs3) = S.addCreature typhoidRats S.bob gs2
            -- alice steals one of bob's creatures: it stops being "a creature an
            -- opponent controls" for alice's source, and becomes one for bob's.
            stolen = S.giveControl theirs S.alice gs3
        HU.assertEqual
          "for alice's source, only the creature still under bob's control"
          (Set.singleton (Recipient.ToCreature alsoTheirs))
          (Target.legalRecipients (Projection.controllerOf mine stolen) mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
        HU.assertEqual
          "for bob's source, the two alice now controls"
          (Set.fromList [Recipient.ToCreature mine, Recipient.ToCreature theirs])
          (Target.legalRecipients (Projection.controllerOf alsoTheirs stolen) alsoTheirs (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen),
      -- P9 (#40): the reshaped TargetSpec = Pool + Maybe Filter reproduces the
      -- retired hand-carved specs as data. A black creature
      -- (Typhoid Rats, {B}) and a nonblack one (Goblin Piker, {1}{R}) exercise
      -- the Not (HasColor Black) filter that WAS NonblackCreatureTarget.
      HU.testCase "P9 Creatures + Not (HasColor Black) excludes a black creature" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
            (plainOid, gs) = S.addCreature piker S.alice gs1
            spec = TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))
            legal = Target.legalRecipients Nothing S.noSource spec gs
        HU.assertBool "black creature illegal" (not (Set.member (Recipient.ToCreature blackOid) legal))
        HU.assertBool "nonblack creature legal" (Set.member (Recipient.ToCreature plainOid) legal),
      HU.testCase "P9 Creatures + Nothing narrows nothing" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
            (plainOid, gs) = S.addCreature piker S.alice gs1
            spec = TargetSpec.MkTargetSpec Pool.Creatures Nothing
            expectedAllCreatures = Set.fromList [Recipient.ToCreature blackOid, Recipient.ToCreature plainOid]
        HU.assertEqual "all creatures legal" expectedAllCreatures (Target.legalRecipients Nothing S.noSource spec gs),
      -- CR 601.2c "another" over a Creatures pool (#163). The pool tags its
      -- candidates ToCreature (CR 115.1a); a Not IsSource conjunct drops the
      -- source whatever tag the pool gave it, which the retired Exclusion field
      -- did not -- it deleted a ToObject recipient a Creatures pool never emits,
      -- so "another target creature" left the source legal.
      HU.testCase "another target creature excludes the source (CR 601.2c)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (srcId, gs1) = S.addCreature piker S.alice gs0
            (otherId, gs) = S.addCreature piker S.alice gs1
            slot = SlotName.MkSlotName (Text.pack "target")
            specs = Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not Filter.Type.IsSource)))
        HU.assertEqual
          "source excluded from its own set"
          (Map.singleton slot (Set.singleton (Recipient.ToCreature otherId)))
          (Target.legalSets Nothing srcId specs gs),
      -- The other half of the same claim: a slot carrying no Not IsSource does
      -- not exclude, so Prodigal Sorcerer may still ping itself (CR 115.4).
      HU.testCase "a slot without Not IsSource still admits the source (CR 115.4)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (srcId, gs) = S.addCreature piker S.alice gs0
            slot = SlotName.MkSlotName (Text.pack "target")
            specs = Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures Nothing)
        HU.assertEqual
          "source is its own legal target"
          (Map.singleton slot (Set.singleton (Recipient.ToCreature srcId)))
          (Target.legalSets Nothing srcId specs gs),
      -- Gate cards for P9 Task 5: Terror and Reprisal. Both cards' printed text
      -- ends "It can't be regenerated."; regeneration is not modelled (no
      -- regeneration shield to suppress), so that clause is a no-op and is
      -- omitted from data/cards/{terror,reprisal}.json -- regeneration clause
      -- omitted; not modelled (#113).
      HU.testCase "Terror: And of Not(HasColor Black) and Not(HasCardType Artifact) excludes black and artifact creatures" $ do
        terror <- Registry.printing registry "Terror"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        piker <- Registry.printing registry "Goblin Piker"
        case S.spellTargetSpec terror of
          Nothing -> HU.assertFailure "Terror's printing carries no 'target' slot"
          Just spec -> do
            let gs0 = Setup.emptyGame S.bothPlayers
                (blackOid, gs1) = S.addCreature typhoidRats S.bob gs0
                (artifactOid, gs2) = S.addCreature darksteelMyr S.bob gs1
                (plainOid, gs) = S.addCreature piker S.alice gs2
                legal = Target.legalRecipients Nothing S.noSource spec gs
            HU.assertBool "black creature illegal" (not (Set.member (Recipient.ToCreature blackOid) legal))
            HU.assertBool "artifact creature illegal" (not (Set.member (Recipient.ToCreature artifactOid) legal))
            HU.assertBool "nonblack, nonartifact creature legal" (Set.member (Recipient.ToCreature plainOid) legal),
      HU.testCase "Reprisal: PowerAtLeast 4 legality tracks a projected power pump" $ do
        reprisal <- Registry.printing registry "Reprisal"
        piker <- Registry.printing registry "Goblin Piker"
        case S.spellTargetSpec reprisal of
          Nothing -> HU.assertFailure "Reprisal's printing carries no 'target' slot"
          Just spec -> do
            let gs0 = Setup.emptyGame S.bothPlayers
                (smallOid, gs) = S.addCreature piker S.bob gs0 -- power 2, {1}{R}
                legalBefore = Target.legalRecipients Nothing S.noSource spec gs
                pumped = S.withEffect smallOid (Modification.ModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0)) gs
                legalAfter = Target.legalRecipients Nothing S.noSource spec pumped
            HU.assertBool "power 2 is illegal (below the PowerAtLeast 4 floor)" (not (Set.member (Recipient.ToCreature smallOid) legalBefore))
            HU.assertBool "pumped to power 4 becomes legal" (Set.member (Recipient.ToCreature smallOid) legalAfter),
      -- CR 508.1k: Kill Shot's IsAttacking narrowing, read off the committed card
      -- data. The defender is a creature in every other respect, so only combat
      -- status can be what separates the two.
      HU.testCase "Kill Shot: IsAttacking admits the attacker and rejects the untapped defender" $ do
        killShot <- Registry.printing registry "Kill Shot"
        piker <- Registry.printing registry "Goblin Piker"
        case S.spellTargetSpec killShot of
          Nothing -> HU.assertFailure "Kill Shot's printing carries no 'target' slot"
          Just spec -> do
            let (board, mine, theirs) = S.combatBoardOf [piker] [piker]
                declared = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
                legal = Target.legalRecipients Nothing S.noSource spec declared
            case (mine, theirs) of
              (attacker : _, defender : _) -> do
                HU.assertBool "the fixture really did attack" (Map.member attacker (Combat.Type.attackers (GameState.combat declared)))
                HU.assertBool "the attacker is legal" (Set.member (Recipient.ToCreature attacker) legal)
                HU.assertBool "the creature that stayed home is not" (not (Set.member (Recipient.ToCreature defender) legal))
              _ -> HU.assertFailure "fixture should have one creature a side"
    ]

resolveTests :: Registry.Type.Registry -> Tasty.TestTree
resolveTests registry =
  Tasty.testGroup
    "Resolve"
    [ HU.testCase "CR 608 a resolved spell's damage is Noncombat" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let base = S.landsInPlay mountain 1
            (_target, gs0) = S.addCreature piker S.bob base
            (gs1, spellId) = S.handOne lightningBolt gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice spellId))
            -- resolveTop applies the damage but does NOT run SBAs, so the event persists.
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual
          "the Bolt's damage event is Noncombat"
          [DamageKind.Noncombat]
          (fmap DamageEvent.kind (S.damageEventsOf resolved)),
      HU.testCase "CR 608.3 / 704.5g a resolved Bolt kills a Piker" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
            after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
        HU.assertEqual "stack empty" 0 (length (GameState.stack after))
        HU.assertEqual "no creature survives" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "Piker in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 608.2n the resolved Bolt is in its owner's graveyard" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "one card" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 120.3a a Bolt at a player drains life without marking" $ do
        -- No creature on the battlefield, so identityAnswer's lookupMin picks
        -- ToPlayer alice: a self-Bolt, which is legal Magic.
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "seventeen" (Just 17) (S.lifeOf S.alice after),
      HU.testCase "the resolved damage flows through the event funnel" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "one event of amount 3" [3] (fmap DamageEvent.amount (S.damageEventsOf after)),
      HU.testCase "resolving a Bolt conserves objects" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
        HU.assertEqual "conserved" (Game.objectCount cast) (Game.objectCount (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))),
      HU.testCase "CR 608.2b a Bolt whose only target died fizzles" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (base, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
            -- Kill the Piker while the Bolt is on the stack, as Bolt B will in
            -- the integration test, then check state-based actions.
            dead = S.settleSba (S.markDamage (S.pikerOf base) 3 cast)
            after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
        HU.assertEqual "Bolt in the graveyard, unresolved" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
        HU.assertEqual "no damage was dealt" [] (S.damageEventsOf after)
        HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 608.2b a fizzled spell applies none of its effects" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (base, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
            dead = S.settleSba (S.markDamage (S.pikerOf base) 3 cast)
            after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
        HU.assertEqual "life totals unchanged" (Just 20) (S.lifeOf S.alice after),
      -- The deterministic successor to the retired "instants happen" property: a
      -- Bolt cast in a game and resolved ends in its owner's graveyard.
      HU.testCase "a cast Bolt reaches its owner's graveyard" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, cast, _) = S.boltAtBobsPiker piker mountain lightningBolt
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "one card in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 612 slotsOf and textChangeSlots find a ChangeText slot" $ do
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let slot = SlotName.MkSlotName (Text.pack "target")
            card =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "T",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine = Card.Type.typeLine (Printing.card lightningBolt),
                  Card.Type.power = Nothing,
                  Card.Type.toughness = Nothing,
                  Card.Type.keywords = Set.empty,
                  Card.Type.colorIndicator = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.spell =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.singleton (Effect.ChangeText slot)) Map.empty Optionality.Mandatory))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.blockRequirements = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing,
                  Card.Type.counterability = Counterability.Counterable
                }
        HU.assertEqual "slotsOf" (Set.singleton slot) (Resolve.slotsOf (Effect.ChangeText slot))
        HU.assertEqual "textChangeSlots" [slot] (Resolve.textChangeSlots card),
      HU.testCase "CR 605 manaProduced reads AddMana, nothing else" $ do
        HU.assertEqual "add mana" (Just (ManaProduction.OfType (ManaType.Colored Color.Green))) (Resolve.manaProduced (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green))))
        HU.assertEqual "add mana of any color" (Just ManaProduction.AnyColor) (Resolve.manaProduced (Effect.AddMana ManaProduction.AnyColor))
        HU.assertEqual "damage produces no mana" Nothing (Resolve.manaProduced (Effect.DealDamage (SlotName.MkSlotName (Text.pack "x")) (Quantity.Literal 1))),
      HU.testCase "CR 612 resolve reads projected effects: a hacked 'becomes Swamp' resolves as Mountain" $ do
        -- The target is a Forest, so the assertion {Mountain} proves the rewrite:
        -- un-rewritten the effect is SetLandSubtype Swamp -> {Swamp}; rewritten
        -- (Swamp -> Mountain) it is SetLandSubtype Mountain -> {Mountain}.
        forest <- Registry.printing registry "Forest"
        landform <- Registry.printing registry "Landform"
        let base = S.landsInPlay forest 1
            targetLand = case Game.zoneMembers Zone.Battlefield S.alice base of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            slot = SlotName.MkSlotName (Text.pack "target")
            (landformId, g1) = Game.freshObjectId base
            landformObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfCard landform,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  -- CR 700.2: Landform has one mode; a directly-built stack object
                  -- (bypassing Cast.castSpell) must stamp it chosen (mode 0), or
                  -- Resolve.effectsOf/resolveSpell -- now scoped to CHOSEN modes --
                  -- would see no effects and no target specs at all.
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToObject targetLand)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = Timestamp.MkTimestamp 0
                }
            g2 =
              g1
                { GameState.objects = Map.insert landformId landformObj (GameState.objects g1),
                  GameState.stack = landformId : GameState.stack g1
                }
            -- A resolved Magical Hack already changed Swamp -> Mountain on the
            -- Landform spell (stored on the Landform's id).
            hacked = S.withEffectAt landformId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Swamp Subtype.Mountain) g2
            after = snd (Engine.runGamePure S.identityAnswer hacked (Resolve.resolveSpell landformId))
        -- Landform's own subtype does not matter; its EFFECT was rewritten to
        -- SetLandSubtype Mountain, so the target land ends up a Mountain.
        HU.assertEqual "target land became Mountain, not Swamp" (Set.singleton Subtype.Mountain) (Projection.subtypesOf targetLand after),
      HU.testCase "CR 612.1 a text change reaches a Filter carried by an effect" $ do
        -- Boil ("Destroy all Islands") is the first card whose effect selects by
        -- a BASIC LAND TYPE, so it is the first that can tell whether CR 612.1's
        -- "any words or symbols printed on that object" reaches inside an
        -- effect's Filter. The stored ChangeSubtypeWord is what a resolved
        -- Magical Hack leaves on the spell, exactly as the Landform case above.
        island <- Registry.printing registry "Island"
        forest <- Registry.printing registry "Forest"
        boil <- Registry.printing registry "Boil"
        let base = Setup.emptyGame S.bothPlayers
            (islandId, g1) = S.addCreature island S.alice base
            (forestId, g2) = S.addCreature forest S.alice g1
            (boilId, g3) = Game.freshObjectId g2
            boilObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfCard boil,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  -- CR 700.2, as the Landform case explains: a directly-built
                  -- stack object must stamp its one mode chosen.
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = Timestamp.MkTimestamp 0
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
        HU.assertBool "unhacked, the Island dies" (not (onBattlefield islandId plain))
        HU.assertBool "unhacked, the Forest lives" (onBattlefield forestId plain)
        -- And hacked, the word swap moves which lands the filter admits.
        HU.assertBool "hacked, the Forest dies" (not (onBattlefield forestId hacked))
        HU.assertBool "hacked, the Island lives" (onBattlefield islandId hacked),
      HU.testCase "CR 400.7 hacking Blood Moon on the stack is lost when it resolves" $ do
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature urborg S.alice base
            (bloodMoonSpellId, g2) = Game.freshObjectId g1
            bmObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfCard bloodMoon,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  Object.bindings = Map.empty,
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = Timestamp.MkTimestamp 0
                }
            g3 =
              g2
                { GameState.objects = Map.insert bloodMoonSpellId bmObj (GameState.objects g2),
                  GameState.stack = bloodMoonSpellId : GameState.stack g2
                }
            hacked = S.withEffectAt bloodMoonSpellId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g3
            after = snd (Engine.runGamePure S.identityAnswer hacked Stack.resolveTop)
        -- Blood Moon entered as a NEW object; the hack (locked to the spell id)
        -- no longer names it, so nonbasic lands are Mountains, not Islands.
        HU.assertEqual "hack lost: nonbasic land is Mountain" (Set.singleton Subtype.Mountain) (Projection.subtypesOf nonbasicId after),
      HU.testCase "CR 608.2n a resolving ability deals its damage and ceases" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            ability = case Card.Type.activatedAbilities (Printing.card prodigalSorcerer) of
              ab : _ -> ab
              [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime
            (abilId, g1) = Game.freshObjectId g0
            (ts, g2) = Game.freshTimestamp g1
            slot = SlotName.MkSlotName (Text.pack "target")
            abilObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfAbility srcId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = ts
                }
            g3 =
              g2
                { GameState.objects = Map.insert abilId abilObj (GameState.objects g2),
                  GameState.stack = abilId : GameState.stack g2
                }
            resolved = snd (Engine.runGamePure S.identityAnswer g3 Stack.resolveTop)
        HU.assertEqual "bob took 1" (Just 19) (S.lifeOf S.bob resolved)
        HU.assertEqual "ability object gone" Nothing (Game.lookupObject abilId resolved)
        HU.assertEqual "stack empty" [] (GameState.stack resolved),
      HU.testCase "CR 701.23 Search fetches a basic land to the battlefield tapped" $ do
        -- The fetched card gets a NEW object id (CR 400.7 changeZone), so assert by
        -- count/tapped-count, never by the library incarnation's id.
        mountain <- Registry.printing registry "Mountain"
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
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
        HU.assertEqual "one permanent on the battlefield" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
        HU.assertEqual "it is tapped" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "library empty" [] (Game.zoneMembers Zone.Library S.alice resolved),
      HU.testCase "CR 701.23b Search may fail to find" $ do
        mountain <- Registry.printing registry "Mountain"
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addLibraryCard mountain S.alice base
            ability = ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter SearchDestination.BattlefieldTapped]) Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj = Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findNothing g4 Stack.resolveTop)
        HU.assertEqual "nothing entered the battlefield" Set.empty (GameState.battlefield resolved),
      HU.testCase "CR 701.23a Search (And [HasCardType Land, HasSupertype Basic]) offers a basic land, not a nonland" $ do
        -- P9: the Search filter reads each library card through the PRINTED-card
        -- view (Projection.viewOfCard) -- a card in a library has no projection.
        -- With a Mountain (basic land) and a Piker (creature) both in the library,
        -- only the Mountain is a candidate: findFirst fetches it while the Piker
        -- stays put. The Piker is added SECOND, so it is the head of the library
        -- (Support.addLibraryCard prepends); a filter that matched everything would
        -- fetch the Piker and this test would fail.
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
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
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
        HU.assertEqual "the basic land is offered and fetched to the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Mountain") S.alice resolved)
        HU.assertBool "the nonland is not offered -- it remains in the library" (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)),
      -- #222: CR 701.23a's filter defines what the search may find. An
      -- interpreter that names a card the filter excluded must find nothing --
      -- "fails to find" is already a legal outcome, so rejecting needs no new
      -- branch. Same fixture as the test above, so the only variable is the answer.
      HU.testCase "#222 a search that names a card the filter excluded fetches nothing" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
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
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 (Sickness.Settled S.alice) (Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty Nothing ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure (findForbidden pikerId) g4 Stack.resolveTop)
        HU.assertEqual "the Piker was NOT fetched to the battlefield" 0 (S.countOnBattlefieldByName (Text.pack "Goblin Piker") S.alice resolved)
        HU.assertBool "it is still in the library" (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved))
        HU.assertEqual "and nothing else was fetched either" 0 (S.countOnBattlefieldByName (Text.pack "Mountain") S.alice resolved),
      HU.testCase "CR 603/608.2n Rest in Peace's ETB exiles graveyards and ceases" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
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
                  Object.source = Source.OfTrigger ripId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = ts
                }
            g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
            resolved = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
        HU.assertEqual "bob's graveyard is empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
        HU.assertEqual "ability ceased" Nothing (Game.lookupObject abilId resolved),
      HU.testCase "CR 103.5b ExileHandThenDraw exiles the whole hand, then draws that many" $ do
        mountain <- Registry.printing registry "Mountain"
        swamp <- Registry.printing registry "Swamp"
        let g0 = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addHandCard mountain S.alice g0
            (_, g2) = S.addHandCard swamp S.alice g1
            g3 = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.alice g)) g2 (replicate 5 ())
            after =
              S.runPure S.identityAnswer g3 $
                Resolve.applyEffect S.noSource S.alice Map.empty Map.empty Map.empty Effect.ExileHandThenDraw
        HU.assertEqual "the hand is refilled to the size it had" 2 (S.handSize S.alice after)
        HU.assertEqual "both old cards went to exile" 2 (length (Game.zoneMembers Zone.Exile S.alice after))
        HU.assertEqual "and the library is two shorter" 3 (length (Game.zoneMembers Zone.Library S.alice after)),
      HU.testCase "CR 723.1: Mindslaver's ability installs pending control, promoted next turn" $ do
        mindslaver <- Registry.printing registry "Mindslaver"
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
                  Object.source = Source.OfAbility srcId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = ts
                }
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = abilId : GameState.stack g3}
            resolved = snd (Engine.runGamePure S.identityAnswer g4 Stack.resolveTop)
            bobsTurn = snd (Engine.runGamePure S.identityAnswer resolved Engine.handoffTurn)
            afterBob = snd (Engine.runGamePure S.identityAnswer bobsTurn Engine.handoffTurn)
        HU.assertEqual "control pending for bob" (Just (Decider.MkDecider S.alice)) (Map.lookup S.bob (GameState.pendingControl resolved))
        HU.assertEqual "promoted on bob's turn" (Just (Decider.MkDecider S.alice)) (GameState.activeControl bobsTurn)
        HU.assertEqual "bob's decisions route to alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.bob bobsTurn)
        HU.assertEqual "control expired after bob's turn" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob afterBob),
      HU.testCase "CR 723.1a: a second player-controlling effect overwrites the first (last created wins)" $ do
        mindslaver <- Registry.printing registry "Mindslaver"
        let base = Setup.emptyGame S.bothPlayers
            -- First: alice controls bob.
            afterAlice = installControlBy mindslaver S.alice S.bob base
            -- Then: bob controls bob (CR 723.9 self-control), created LATER.
            afterBob = installControlBy mindslaver S.bob S.bob afterAlice
        HU.assertEqual "the first effect installed alice as bob's decider" (Just (Decider.MkDecider S.alice)) (Map.lookup S.bob (GameState.pendingControl afterAlice))
        HU.assertEqual "CR 723.1a: the later effect overwrites — bob's own control wins" (Just (Decider.MkDecider S.bob)) (Map.lookup S.bob (GameState.pendingControl afterBob)),
      HU.testCase "CR 727.1a: resolving a RestartGame ability restarts with its controller as starting player" $ do
        mountain <- Registry.printing registry "Mountain"
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
                  Object.source = Source.OfAbility aliceId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.bob,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = ts
                }
            g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
            after = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
        HU.assertEqual "the game restarted with bob as the starting player (CR 727.1a)" S.bob (GameState.activePlayer after)
        HU.assertEqual "alice's 8 cards all survived the restart, still hers (CR 727.2)" 8 (length (filter (\o -> Object.owner o == S.alice) (Map.elems (GameState.objects after))))
        HU.assertEqual "the resolving ability object ceased to exist (not a card)" Nothing (Game.lookupObject abilId after),
      HU.testCase "CR 729.1b: PlaySubgame binds the loser, a later DealDamage reads it (mid-resolution binding visible)" $ do
        lightningBolt <- Registry.printing registry "Lightning Bolt"
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
            card =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "Subgame Test Spell",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine = Card.Type.typeLine (Printing.card lightningBolt),
                  Card.Type.power = Nothing,
                  Card.Type.toughness = Nothing,
                  Card.Type.keywords = Set.empty,
                  Card.Type.colorIndicator = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.spell =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage slot (Quantity.Literal 3)]) Map.empty Optionality.Mandatory))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.blockRequirements = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing,
                  Card.Type.counterability = Counterability.Counterable
                }
            spellObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfToken card,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = ts
                }
            g3 = g2 {GameState.objects = Map.insert spellId spellObj (GameState.objects g2), GameState.stack = spellId : GameState.stack g2}
            after = snd (Engine.runGamePure S.identityAnswer g3 (Resolve.resolveSpellWith stubRunner spellId))
        HU.assertEqual "bob (the derived loser) lost 3 life to the follow-on DealDamage" (Just 17) (S.lifeOf S.bob after),
      HU.testCase "CR 729.1b: PlaySubgame's derived loser is drawn from the subgame roster, not the full main-game seating (a departed seat is never the loser)" $ do
        lightningBolt <- Registry.printing registry "Lightning Bolt"
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
            card =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "Subgame Test Spell (Three Seats, One Departed)",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine = Card.Type.typeLine (Printing.card lightningBolt),
                  Card.Type.power = Nothing,
                  Card.Type.toughness = Nothing,
                  Card.Type.keywords = Set.empty,
                  Card.Type.colorIndicator = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.spell =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage slot (Quantity.Literal 3)]) Map.empty Optionality.Mandatory))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.blockRequirements = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing,
                  Card.Type.counterability = Counterability.Counterable
                }
            spellObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfToken card,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled S.alice,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = ts
                }
            g3 = g2 {GameState.objects = Map.insert spellId spellObj (GameState.objects g2), GameState.stack = spellId : GameState.stack g2}
            after = snd (Engine.runGamePure S.identityAnswer g3 (Resolve.resolveSpellWith stubRunner spellId))
        HU.assertEqual "carol (a genuine subgame participant) lost 3 life to the follow-on DealDamage" (Just 17) (S.lifeOf S.carol after)
        HU.assertEqual "bob (departed before the subgame; never played it) was not named the loser and took no damage" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 111 Dragon Fodder creates two 1/1 Goblin tokens" $ do
        mountain <- Registry.printing registry "Mountain"
        dragonFodder <- Registry.printing registry "Dragon Fodder"
        let base = S.landsInPlay mountain 2
            (gs, spellId) = S.handOne dragonFodder base
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- Two Goblin tokens exist (count == 2 proves two distinct objects). The
        -- battlefield also holds alice's 2 Mountains, so filter by name/creature.
        HU.assertEqual "two Goblin tokens on the battlefield" 2 (S.countOnBattlefieldByName (Text.pack "Goblin") S.alice after)
        HU.assertEqual "alice controls two creatures (the tokens)" 2 (S.creaturesInPlay S.alice after)
        HU.assertEqual "Dragon Fodder went to the graveyard (CR 608.2n)" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
        -- The control leg for Hanweir Garrison's "tapped and attacking" riders
        -- (CombatSpec's PutOntoBattlefieldAttacking group): a Create that says
        -- neither takes CR 110.5b's default and joins no combat, so the riders
        -- are the effect's and not something every token gets.
        HU.assertEqual "CR 110.5b: the Goblins enter untapped" [TapState.Untapped, TapState.Untapped] (Maybe.mapMaybe (\oid -> fmap Object.tapped (Game.lookupObject oid after)) (S.tokensOf after))
        HU.assertEqual "and attacking nothing" Map.empty (Combat.Type.attackers (GameState.combat after)),
      HU.testCase "CR 615 Fog prevents combat damage but not spell damage (the gate)" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        fog <- Registry.printing registry "Fog"
        let base = S.landsInPlay forest 1
            (victim, gs0) = S.addCreature piker S.bob base
            (gs1, fogId) = S.handOne fog gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice fogId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            combat = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False 0 DamageKind.Combat])
            spell = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False 0 DamageKind.Noncombat])
        HU.assertEqual "Fog installed one replacement" 1 (length (GameState.replacements resolved))
        HU.assertEqual "combat damage prevented (the cancel shape)" (Just 0) (S.damageOf victim combat)
        -- The falsifier: a tag-blind Fog would also blunt this spell damage.
        HU.assertEqual "spell damage untouched (Noncombat)" (Just 2) (S.damageOf victim spell),
      -- Sudden Impact: "deals damage to target player equal to the number of
      -- cards in THAT player's hand." Cast through the real path (Cast.castSpell
      -- + resolveTop), not S.spellOnStack -- that helper sets Object.bindings =
      -- Map.empty and so does not fill the target slot the InSlot count reads.
      HU.testCase "Sudden Impact reads the TARGET's hand, not the caster's" $ do
        -- THE FALSIFIER for a perspective baked into the count: Alice holds
        -- five and Bob holds two, and Bob takes two. A count whose "you" were
        -- the resolving controller (Alice) would deal five instead.
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        suddenImpact <- Registry.printing registry "Sudden Impact"
        let gs0 = S.landsInPlay mountain 4
            fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
            gs1 = fill S.alice 5 (fill S.bob 2 gs0)
            (spellId, gs2) = S.addHandCard suddenImpact S.alice gs1
            cast = snd (Engine.runGamePure atBobAnswer gs2 (Cast.castSpell S.alice spellId))
            before = S.lifeOf S.bob cast
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        HU.assertEqual "two damage" (fmap (subtract 2) before) (S.lifeOf S.bob after),
      HU.testCase "CR 608.2h the number is read as the effect is applied, not as the spell is cast" $ do
        -- Bob's hand grows AFTER Sudden Impact is on the stack and BEFORE it
        -- resolves; the damage follows the hand size at resolution.
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        suddenImpact <- Registry.printing registry "Sudden Impact"
        let gs0 = S.landsInPlay mountain 4
            fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
            gs1 = fill S.bob 2 gs0
            (spellId, gs2) = S.addHandCard suddenImpact S.alice gs1
            cast = snd (Engine.runGamePure atBobAnswer gs2 (Cast.castSpell S.alice spellId))
            (_, cast1) = S.addHandCard piker S.bob cast
            before = S.lifeOf S.bob cast1
            after = snd (Engine.runGamePure atBobAnswer cast1 Stack.resolveTop)
        HU.assertEqual "three damage" (fmap (subtract 3) before) (S.lifeOf S.bob after),
      HU.testCase "the same count with Relative You reads the caster's hand" $ do
        -- The direct contrast: the SAME Count shape (InZone Hand, Objects) that
        -- Sudden Impact scopes with PlayerRef.InSlot also serves Inner Calm,
        -- Outer Strength's PlayerRef.Relative You -- one shape, two
        -- perspectives, neither welded into a constructor.
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            fill pid n g0 = List.foldl' (\g _ -> snd (S.addHandCard piker pid g)) g0 [1 .. (n :: Int)]
            gs = fill S.alice 5 (fill S.bob 2 gs0)
            yourHand =
              Count.Type.MkCount
                (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
                (Filter.Type.And [])
                Aggregation.Objects
        HU.assertEqual
          "Alice's five"
          (Just 5)
          (S.countOf (\oid -> Just (Projection.viewOfObject oid gs)) (Filter.MkContext (Just S.alice) Nothing) gs yourHand)
    ]

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
            Object.source = Source.OfAbility srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer target)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = ts
          }
      gs4 = gs3 {GameState.objects = Map.insert abilId abilObj (GameState.objects gs3), GameState.stack = abilId : GameState.stack gs3}
   in snd (Engine.runGamePure S.identityAnswer gs4 Stack.resolveTop)

-- CR 205.4c / 701.23a: a basic land card is one with the Land card type and the
-- Basic supertype -- Evolving Wilds' search filter, the printed-card predicate
-- that replaced CardCriterion.BasicLandCard.
basicLandFilter :: Filter.Type.Filter
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
            Object.source = Source.OfCard lightningBolt,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = Timestamp.MkTimestamp 0
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
      obj = Object.MkObject pid (Source.OfCard printing) Zone.Hand TapState.Untapped 0 (Sickness.Settled pid) Map.empty Map.empty Nothing (Timestamp.MkTimestamp 0)
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

counterTests :: Registry.Type.Registry -> Tasty.TestTree
counterTests registry =
  Tasty.testGroup
    "Counter"
    [ HU.testCase "CR 701.6 Cancel counters a spell into its owner's graveyard" $ do
        island <- Registry.printing registry "Island"
        cancel <- Registry.printing registry "Cancel"
        piker <- Registry.printing registry "Goblin Piker"
        let (_victimId, resolved) = cancelVictim island cancel piker
        HU.assertEqual "victim countered into bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
        HU.assertEqual "victim never resolved onto the battlefield" 0 (S.creaturesInPlay S.bob resolved)
        HU.assertEqual "Cancel in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved)),
      -- CR 113.6g: "an object's ability that states it can't be countered …
      -- functions on the stack", and CR 101.2 makes the "can't" win. The twin is
      -- the case directly above: the same Cancel, cast the same way at a spell
      -- that does not say it, DOES counter -- so this is the card's clause and
      -- not a broken Cancel.
      HU.testCase "CR 113.6g whole card: Cancel resolves but cannot counter Rending Volley" $ do
        island <- Registry.printing registry "Island"
        cancel <- Registry.printing registry "Cancel"
        rendingVolley <- Registry.printing registry "Rending Volley"
        let (victimId, resolved) = cancelVictim island cancel rendingVolley
        HU.assertBool "Rending Volley is still on the stack" (elem victimId (GameState.stack resolved))
        HU.assertEqual "and not in bob's graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
        -- CR 101.2 again, from the other side: the countering spell is not itself
        -- stopped. Cancel targeted legally (CR 113.6g grants no shroud), resolved,
        -- did nothing, and CR 608.2n put it into its owner's graveyard as the
        -- final part of that resolution.
        HU.assertEqual "Cancel resolved into alice's graveyard regardless" 1 (length (Game.zoneMembers Zone.Graveyard S.alice resolved)),
      HU.testCase "CR 608.2b a Cancel whose target already left the stack fizzles" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        cancel <- Registry.printing registry "Cancel"
        let after = racingCounters island piker cancel
        HU.assertEqual "the Piker moved exactly once, to bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "both Cancels in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after))
        HU.assertEqual "the Piker never hit the battlefield" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "stack cleared" 0 (length (GameState.stack after)),
      HU.testCase "CR 614 Cancel under Rest in Peace exiles the countered spell" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        cancel <- Registry.printing registry "Cancel"
        let (_, ripOut) = S.addCreature restInPeace S.alice (S.landsInPlay island 3)
            (_victimId, onStack) = S.spellOnStack piker S.bob ripOut
            (gs, cancelId) = S.handOne cancel onStack
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cancelId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "the countered spell is not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
        HU.assertEqual "the countered spell is exiled" 1 (length (Game.zoneMembers Zone.Exile S.bob resolved))
    ]

fizzleTests :: Registry.Type.Registry -> Tasty.TestTree
fizzleTests registry =
  Tasty.testGroup
    "Fizzle"
    [ HU.testCase "CR 608.2b Bolt-vs-Bolt through the priority loop: the second fizzles" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let after = snd (Engine.runGamePure boltAnswer (twoBoltState piker mountain lightningBolt) Engine.priorityLoop)
        HU.assertEqual "stack cleared" 0 (length (GameState.stack after))
        HU.assertEqual "Piker dead" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "both Bolts in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after))
        HU.assertEqual "the Piker in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "bob's life untouched: the fizzled Bolt hit nothing" (Just 20) (S.lifeOf S.bob after),
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
      HU.testCase "CR 608.2b the reserved trigger-source slot does not rescue a fizzle: the targetless Draw after the ability's only real target dies does not run" $ do
        piker <- Registry.printing registry "Goblin Piker"
        forest <- Registry.printing registry "Forest"
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
                (Binding.fromChoices (Map.singleton targetSlot (Recipient.ToCreature victim)) Map.empty Nothing Set.empty)
            withBindings = base4 {GameState.objects = Map.adjust (\o -> o {Object.bindings = bindings}) abilId (GameState.objects base4)}
            -- Kill the sole real target before resolution: CR 608.2b makes it
            -- illegal (it's no longer a legal CreatureTarget), while the
            -- reserved slot -- never targeted -- stays vacuously legal.
            gone = S.runPure S.identityAnswer withBindings (Event.changeZone victim Zone.Graveyard)
            mode = Mode.MkMode (Seq.fromList [Effect.Destroy (ObjectRef.InSlot targetSlot) Regenerability.Regenerable, Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1)]) specs Optionality.Mandatory
            run = Resolve.resolveModes abilId source [(ModeIndex.MkModeIndex 0, mode)]
            after = snd (Engine.runGamePure S.identityAnswer gone run)
        HU.assertEqual "the targetless Draw did not run: the ability fizzled" handBefore (S.handSize S.alice after),
      HU.testCase "CR 704.5a a Bolt can end the game mid-step" $ do
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
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
        HU.assertEqual "alice wins" (Just (Result.Won S.alice)) (GameState.result after)
        HU.assertEqual "the loop released priority" Nothing (GameState.priority after)
    ]

indestructibleTests :: Registry.Type.Registry -> Tasty.TestTree
indestructibleTests registry =
  Tasty.testGroup
    "Indestructible"
    [ HU.testCase "CR 704.5g an indestructible creature survives lethal marked damage" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (myrId, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
            -- Myr is 0/1; 3 marked damage is lethal (704.5g) but indestructible saves it.
            after = S.settleSba (S.markDamage myrId 3 gs)
        HU.assertEqual "Myr still on the battlefield" 1 (S.creaturesInPlay S.bob after)
        HU.assertEqual "Myr not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 704.5h an indestructible creature survives deathtouch" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (myrId, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
            -- Zero marked damage (so 704.5g is silent) plus a deathtouch event isolates
            -- the 704.5h path; indestructible must guard it too (CR 700.4).
            wounded = S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True False 0 DamageKind.Combat)] gs
            after = S.settleSba wounded
        HU.assertEqual "Myr survives deathtouch" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 704.5f indestructible does NOT save a creature with toughness <= 0" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (myrId, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
            -- A real -1/-1 counter drops Myr (0/1) to 0/0 (CR 122.1a); 704.5f is a
            -- put-into-graveyard, not a destroy, so indestructible does not apply
            -- (Myr's own reminder text).
            zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 myrId gs
            after = S.settleSba zeroed
        HU.assertEqual "Myr left the battlefield" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "Myr in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 704.5f regeneration does NOT save a creature with toughness <= 0" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (victim, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers) -- 2/1
        -- A real -1/-1 counter drops the toughness to 0 (CR 122.1a); 704.5f is a
        -- put-into-graveyard, not a destruction, so a regeneration shield cannot
        -- save it.
            zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs
            shielded = S.addRegenShield victim zeroed
            after = S.settleSba shielded
        HU.assertEqual "died despite the shield (704.5f is not a destruction)" 0 (S.creaturesInPlay S.bob after)
    ]

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

-- Add k cards of a printing to pid's hand (each a fresh Hand-zone object).
handCards :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
handCards printing pid k gs = List.foldl' (\g _ -> addOne g) gs [1 .. k]
  where
    addOne g =
      let (oid, g1) = Game.freshObjectId g
          obj = Object.MkObject pid (Source.OfCard printing) Zone.Hand TapState.Untapped 0 (Sickness.Settled pid) Map.empty Map.empty Nothing (Timestamp.MkTimestamp 0)
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

zoneChangeTests :: Registry.Type.Registry -> Tasty.TestTree
zoneChangeTests registry =
  Tasty.testGroup
    "ZoneChange"
    [ HU.testCase "CR 701.8 Murder destroys a normal creature into its owner's graveyard" $ do
        swamp <- Registry.printing registry "Swamp"
        murder <- Registry.printing registry "Murder"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, after) = castBlackRemovalAt swamp murder piker
        HU.assertEqual "no creature survives" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "Piker in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 700.4 Murder does nothing to an indestructible creature (destroy /= move)" $ do
        swamp <- Registry.printing registry "Swamp"
        murder <- Registry.printing registry "Murder"
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (_, after) = castBlackRemovalAt swamp murder darksteelMyr
        -- The falsifier: modelling Destroy as MoveToZone slot Graveyard would
        -- bury the Myr. It stays; the spell still resolved and was buried.
        HU.assertEqual "Myr still on the battlefield" 1 (S.creaturesInPlay S.bob after)
        HU.assertEqual "bob's graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "Murder in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 701.19a Murder is replaced by regeneration" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        murder <- Registry.printing registry "Murder"
        let base = S.landsInPlay swamp 3
            (victim, withFoe) = S.addCreature piker S.bob base
            shielded = S.addRegenShield victim withFoe
            (gs, spellId) = S.handOne murder shielded
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
        HU.assertEqual "the shielded creature survived Murder" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 400.7 Unsummon returns a creature to its owner's hand" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        unsummon <- Registry.printing registry "Unsummon"
        let base = S.landsInPlay island 1
            (_, withPiker) = S.addCreature piker S.bob base
            (gs, spellId) = S.handOne unsummon withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "no creature on the battlefield" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "a card in bob's hand (its owner)" 1 (S.handSize S.bob after)
        HU.assertEqual "Unsummon in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 701.19a regeneration does not save a bounced creature" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        unsummon <- Registry.printing registry "Unsummon"
        let base = S.landsInPlay island 1
            (victim, withFoe) = S.addCreature piker S.bob base
            shielded = S.addRegenShield victim withFoe
            (gs, spellId) = S.handOne unsummon shielded
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "the creature left the battlefield (bounce is not a destruction)" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "it is in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob after)),
      HU.testCase "CR 701.13 Angelic Edict exiles a target creature" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        angelicEdict <- Registry.printing registry "Angelic Edict"
        let base = S.landsInPlay plains 5
            (_, withPiker) = S.addCreature piker S.bob base
            (gs, spellId) = S.handOne angelicEdict withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "no creature on the battlefield" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "one card in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 115 Angelic Edict may exile an enchantment (non-creature permanent)" $ do
        plains <- Registry.printing registry "Plains"
        restInPeace <- Registry.printing registry "Rest in Peace"
        angelicEdict <- Registry.printing registry "Angelic Edict"
        let base = S.landsInPlay plains 5
            -- bob controls only Rest in Peace (an enchantment, not a creature), so
            -- it is the single legal CreatureOrEnchantmentTarget.
            (ripId, withRip) = S.addCreature restInPeace S.bob base
            (gs, spellId) = S.handOne angelicEdict withRip
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "the enchantment left the battlefield" Nothing (Game.lookupObject ripId after)
        HU.assertEqual "one card in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 121.1 Divination draws its controller two cards" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        divination <- Registry.printing registry "Divination"
        let base = S.landsInPlay island 3
            (_, g1) = S.addLibraryCard piker S.alice base
            (_, g2) = S.addLibraryCard piker S.alice g1
            (gs, spellId) = S.handOne divination g2
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "two cards drawn to hand" 2 (S.handSize S.alice after)
        HU.assertEqual "library emptied" [] (Game.zoneMembers Zone.Library S.alice after),
      HU.testCase "CR 121.4 a Draw that outruns the library records the loss" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        divination <- Registry.printing registry "Divination"
        let base = S.landsInPlay island 3
            (_, g1) = S.addLibraryCard piker S.alice base
            (gs, spellId) = S.handOne divination g1
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertBool "drewFromEmpty marked" (Set.member S.alice (GameState.drewFromEmpty after)),
      -- The card that proves Effect.Draw's recipient (#272): CR 121.1 says who
      -- draws, and here that is the player the spell TARGETS (CR 601.2c), not
      -- the controller who paid for it. Divination above is the same opcode
      -- pointed at `Relative You`; the two together are the falsifier for a
      -- Draw that always drew for its controller.
      HU.testCase "CR 121.1 Ancestral Recall draws three cards for the player it targets, not its controller" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        ancestralRecall <- Registry.printing registry "Ancestral Recall"
        let base = S.landsInPlay island 1
            withLib = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.bob g)) base [1 .. (4 :: Int)]
            (gs, spellId) = S.handOne ancestralRecall withLib
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        HU.assertEqual "three cards drawn to bob's hand" 3 (S.handSize S.bob after)
        HU.assertEqual "one card left in bob's library" 1 (length (Game.zoneMembers Zone.Library S.bob after))
        HU.assertEqual "alice drew nothing" 0 (S.handSize S.alice after),
      -- The card that proves Effect.Draw's `EachPlayer` arm (#276). Divination
      -- above draws for the controller alone and Ancestral Recall for one named
      -- player; Vision Skeins is the first Draw in the pool that reaches the
      -- whole table at once.
      HU.testCase "CR 121.1 Vision Skeins draws two cards for each player, its caster included" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        visionSkeins <- Registry.printing registry "Vision Skeins"
        let base = S.landsInPlay island 2
            withLibs = stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 base)
            (gs, spellId) = S.handOne visionSkeins withLibs
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "alice drew two" 2 (S.handSize S.alice after)
        HU.assertEqual "bob drew two as well" 2 (S.handSize S.bob after)
        HU.assertEqual "no draw outran a library" Set.empty (GameState.drewFromEmpty after),
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
      HU.testCase "CR 121.2c Vision Skeins draws for the active player first, then in turn order" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        visionSkeins <- Registry.printing registry "Vision Skeins"
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
        HU.assertEqual
          "bob (active) draws both of his, then carol, then the caster"
          [S.bob, S.bob, S.carol, S.carol, S.alice, S.alice]
          (drawersOf after)
        HU.assertEqual "and everyone holds two" [2, 2, 2] (fmap (\pid -> S.handSize pid after) [S.alice, S.bob, S.carol]),
      -- The card that proves Effect.Draw's `Relative Opponent` arm (#276), and
      -- the one shape no "you draw" card can stand in for: Master of the Feast's
      -- trigger is a DRAWBACK, drawing for everyone except the player who
      -- controls it (CR 109.5 makes "your upkeep" that controller's).
      HU.testCase "CR 121.1 Master of the Feast's upkeep trigger draws for the opponent, not its controller" $ do
        piker <- Registry.printing registry "Goblin Piker"
        masterOfTheFeast <- Registry.printing registry "Master of the Feast"
        let (_, board) = S.addCreature masterOfTheFeast S.alice (Setup.emptyGame S.bothPlayers)
            withLibs = stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board)
            onStack = settleAtAlicesUpkeep withLibs
            after = snd (Engine.runGamePure S.identityAnswer onStack Stack.resolveTop)
        HU.assertBool "the upkeep trigger really reached the stack" (not (null (GameState.stack onStack)))
        HU.assertEqual "bob drew" 1 (S.handSize S.bob after)
        HU.assertEqual "alice, who controls it, did not" 0 (S.handSize S.alice after)
        HU.assertEqual "and alice's library is untouched" 1 (length (Game.zoneMembers Zone.Library S.alice after)),
      -- The discriminator, and it needs a THIRD seat: at two players an
      -- `Opponent` arm that reached only ONE opponent is indistinguishable from
      -- one that reaches them all. CR 806.1: in a Free-for-All the players
      -- compete as individuals, so every other player is an opponent (CR 102.3's
      -- teammates are the one exception, and pawl has no teams, #175) and both
      -- of them draw.
      HU.testCase "CR 806.1 at three seats each opponent draws off Master of the Feast, and only opponents" $ do
        piker <- Registry.printing registry "Goblin Piker"
        masterOfTheFeast <- Registry.printing registry "Master of the Feast"
        let (_, board) = S.addCreature masterOfTheFeast S.alice S.threePlayerGame
            withLibs = stockLibrary piker S.carol 1 (stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board))
            after = snd (Engine.runGamePure S.identityAnswer (settleAtAlicesUpkeep withLibs) Stack.resolveTop)
        -- A drawer whose library was empty would draw no card and so record no
        -- zone change; this is what keeps the list below honest about that.
        HU.assertEqual "no draw outran a library" Set.empty (GameState.drewFromEmpty after)
        HU.assertEqual "both opponents drew, and the controller did not" [S.bob, S.carol] (drawersOf after),
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
      HU.testCase "CR 800.4a Vision Skeins does not draw for a player who has left the game" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        visionSkeins <- Registry.printing registry "Vision Skeins"
        let withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
            withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
            (gs0, spellId) = S.handOne visionSkeins withLibs
            gs = Departure.depart Departure.Type.Conceded S.carol gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "the two players still in the game drew, in APNAP order" [S.alice, S.alice, S.bob, S.bob] (drawersOf after)
        HU.assertEqual "and nothing was drawn against carol's departed library" Set.empty (GameState.drewFromEmpty after),
      HU.testCase "CR 701.17 Tome Scour mills five from a target player's library" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        tomeScour <- Registry.printing registry "Tome Scour"
        let base = S.landsInPlay island 1
            withLib = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.bob g)) base [1 .. (6 :: Int)]
            (gs, spellId) = S.handOne tomeScour withLib
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        HU.assertEqual "five milled to graveyard" 5 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "one card left in library" 1 (length (Game.zoneMembers Zone.Library S.bob after)),
      HU.testCase "CR 701.17b milling a short library mills fewer with no loss" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        tomeScour <- Registry.printing registry "Tome Scour"
        let base = S.landsInPlay island 1
            (_, g1) = S.addLibraryCard piker S.bob base
            (_, g2) = S.addLibraryCard piker S.bob g1
            (gs, spellId) = S.handOne tomeScour g2
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = S.settleSba (snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop))
        HU.assertEqual "two milled" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertBool "bob did not lose (milling is not drawing)" (not (Set.member S.bob (GameState.drewFromEmpty after))),
      HU.testCase "CR 701.9 Mind Rot discards two chosen cards from a hand of three" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        mindRot <- Registry.printing registry "Mind Rot"
        let base = S.landsInPlay swamp 3
            withHand = handCards piker S.bob 3 base
            (gs, spellId) = S.handOne mindRot withHand
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        HU.assertEqual "one card left in bob's hand" 1 (S.handSize S.bob after)
        HU.assertEqual "two cards in bob's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 609.3 a forced full-hand discard is not prompted" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        mindRot <- Registry.printing registry "Mind Rot"
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
        HU.assertEqual "bob's hand emptied" 0 (S.handSize S.bob after)
        HU.assertEqual "both cards discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      -- The three below are about the PROMPTED branch -- hand of three, discard
      -- two -- where the elision above does not apply and the answer is a real
      -- choice. Mind Rot is not "may", and CR 609.3's "as much as possible" caps
      -- nothing here (the hand is larger than the count), so every card an answer
      -- omits is one the player could have discarded.
      HU.testCase "CR 701.9b an empty ChooseDiscard answer still discards the full count" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        mindRot <- Registry.printing registry "Mind Rot"
        let base = S.landsInPlay swamp 3
            withHand = handCards piker S.bob 3 base
            (gs, spellId) = S.handOne mindRot withHand
            noDiscard q = case q of
              Prompt.ChooseDiscard {} -> []
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
              _ -> S.identityAnswer q
            cast = snd (Engine.runGamePure noDiscard gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure noDiscard cast Stack.resolveTop)
        HU.assertEqual "two discarded despite the answer naming none" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "one card left in bob's hand" 1 (S.handSize S.bob after),
      HU.testCase "CR 701.9b a valid pick is honoured and only the shortfall is completed" $ do
        -- Discriminating against "ignore the answer and take the first n": the
        -- answer names the LAST card in hand, which a first-n completion would
        -- leave behind.
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        mindRot <- Registry.printing registry "Mind Rot"
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
          [] -> HU.assertFailure "fixture should leave bob a hand to discard from"
          lastCard : _ -> do
            HU.assertEqual "two discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
            HU.assertBool "and the card the answer named is one of them" (List.notElem lastCard (Game.zoneMembers Zone.Hand S.bob after)),
      HU.testCase "CR 701.9b naming the same card twice fills one slot, not two" $ do
        -- ChooseDiscard is answered with a LIST, so unlike ChooseSacrifices'
        -- Set the duplicate has to be removed here or it discards one card short.
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        mindRot <- Registry.printing registry "Mind Rot"
        let base = S.landsInPlay swamp 3
            withHand = handCards piker S.bob 3 base
            (gs, spellId) = S.handOne mindRot withHand
            sameTwice q = case q of
              Prompt.ChooseDiscard _ _ ids _ -> concat (replicate 2 (take 1 ids))
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
              _ -> S.identityAnswer q
            cast = snd (Engine.runGamePure sameTwice gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure sameTwice cast Stack.resolveTop)
        HU.assertEqual "two distinct cards discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "one card left in bob's hand" 1 (S.handSize S.bob after)
    ]

drawCardTests :: Registry.Type.Registry -> Tasty.TestTree
drawCardTests registry =
  Tasty.testGroup
    "DrawCard"
    [ HU.testCase "CR 121.2 drawCard moves the top library card to hand" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (_, withCard) = S.addLibraryCard piker S.alice base
            after = S.runPure S.identityAnswer withCard (Event.drawCard S.alice)
        HU.assertEqual "one card in hand" 1 (S.handSize S.alice after)
        HU.assertEqual "library empty" [] (Game.zoneMembers Zone.Library S.alice after),
      HU.testCase "CR 121.3 drawing from an empty library records the failed draw" $
        let after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Event.drawCard S.alice)
         in HU.assertBool "drewFromEmpty marked" (Set.member S.alice (GameState.drewFromEmpty after))
    ]

loseLifeTests :: Registry.Type.Registry -> Tasty.TestTree
loseLifeTests registry =
  Tasty.testGroup
    "LoseLife"
    -- Both cases are Sign in Blood, the card that proves the opcode (#273): its
    -- two clauses share one target slot, so the player who draws is the player
    -- who loses life, and neither is aimed at the caster.
    [ -- The last assertion is the falsifier for a life loss spelled as damage.
      -- CR 119.2 makes damage a CAUSE of life loss, not a synonym for it, so
      -- this records no damage event for CR 614/615's replacement and
      -- prevention, infect's CR 120.3b diversion or CR 704.5h's deathtouch scan
      -- to read.
      HU.testCase "CR 119.3 Sign in Blood makes the player it targets draw two and lose two life" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        signInBlood <- Registry.printing registry "Sign in Blood"
        let base = S.landsInPlay swamp 2
            withLib = stockLibrary piker S.bob 3 base
            (gs, spellId) = S.handOne signInBlood withLib
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
            isDamage ev = case ev of
              GameEvent.DamageDealt _ -> True
              _ -> False
        HU.assertEqual "bob drew two" 2 (S.handSize S.bob after)
        HU.assertEqual "and lost two life" (fmap (subtract 2) (S.lifeOf S.bob gs)) (S.lifeOf S.bob after)
        HU.assertEqual "alice, who cast it, lost none" (S.lifeOf S.alice gs) (S.lifeOf S.alice after)
        HU.assertBool "no damage was dealt (CR 119.2)" (not (any isDamage (GameState.events after))),
      -- CR 704.5a: life lost without damage still reaches the state-based
      -- action -- the same check a CR 119.4 pay-life cost answers to. Bob is at
      -- two, so the second clause is lethal though nothing dealt damage.
      HU.testCase "CR 704.5a Sign in Blood's life loss can take a player to 0 and lose them the game" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        signInBlood <- Registry.printing registry "Sign in Blood"
        let base = S.landsInPlay swamp 2
            withLib = stockLibrary piker S.bob 3 base
            (gs0, spellId) = S.handOne signInBlood withLib
            gs = gs0 {GameState.players = Map.adjust (\pl -> pl {Player.life = 2}) S.bob (GameState.players gs0)}
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        HU.assertEqual "bob is at 0" (Just 0) (S.lifeOf S.bob after)
        HU.assertEqual "and alice wins" (Just (Result.Won S.alice)) (GameState.result (S.settleSba after))
    ]

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
greatestTests :: Registry.Type.Registry -> Tasty.TestTree
greatestTests registry =
  Tasty.testGroup
    "Greatest"
    [ HU.testCase "CR 202.3 One with the Machine draws the GREATEST mana value, not the count, the sum or the least" $ do
        island <- Registry.printing registry "Island"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        serumPowder <- Registry.printing registry "Serum Powder"
        mindslaver <- Registry.printing registry "Mindslaver"
        piker <- Registry.printing registry "Goblin Piker"
        oneWithTheMachine <- Registry.printing registry "One with the Machine"
        let base = S.landsInPlay island 4
            (_, withOne) = S.addCreature bonesplitter S.alice base
            (_, withTwo) = S.addCreature serumPowder S.alice withOne
            (_, withThree) = S.addCreature mindslaver S.alice withTwo
            withLib = stockLibrary piker S.alice 10 withThree
            (gs, spellId) = S.handOne oneWithTheMachine withLib
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- The spell left the hand as it was cast, so the hand size IS the draw.
        HU.assertEqual "alice drew six" 6 (S.handSize S.alice after),
      HU.testCase "CR 109.5 an opponent's larger artifact does not raise \"artifacts YOU control\"" $ do
        -- Bob's Mindslaver ({6}) is on the same battlefield and is the largest
        -- artifact in the game; Alice's own Bonesplitter ({1}) is the answer.
        island <- Registry.printing registry "Island"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        mindslaver <- Registry.printing registry "Mindslaver"
        piker <- Registry.printing registry "Goblin Piker"
        oneWithTheMachine <- Registry.printing registry "One with the Machine"
        let base = S.landsInPlay island 4
            (_, withMine) = S.addCreature bonesplitter S.alice base
            (_, withTheirs) = S.addCreature mindslaver S.bob withMine
            withLib = stockLibrary piker S.alice 10 withTheirs
            (gs, spellId) = S.handOne oneWithTheMachine withLib
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "alice drew one, not six" 1 (S.handSize S.alice after),
      HU.testCase "CR 205.2a a larger NONARTIFACT permanent does not raise \"ARTIFACTS you control\"" $ do
        -- Panglacial Wurm is {5}{G}{G} -- mana value 7, larger than any artifact
        -- in the pool -- and Alice controls it, so only the card-type conjunct
        -- keeps it out of the fold. Her four Islands are the same falsifier at
        -- mana value 0 (CR 202.1b / 202.3a), which no maximum could ever show.
        island <- Registry.printing registry "Island"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        panglacialWurm <- Registry.printing registry "Panglacial Wurm"
        piker <- Registry.printing registry "Goblin Piker"
        oneWithTheMachine <- Registry.printing registry "One with the Machine"
        let base = S.landsInPlay island 4
            (_, withArtifact) = S.addCreature bonesplitter S.alice base
            (_, withWurm) = S.addCreature panglacialWurm S.alice withArtifact
            withLib = stockLibrary piker S.alice 10 withWurm
            (gs, spellId) = S.handOne oneWithTheMachine withLib
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "alice drew one, not seven" 1 (S.handSize S.alice after),
      -- The empty matched set. No rule in the CR gives a maximum over nothing a
      -- value: CR 208.2a's "use 0 instead of that number" is scoped to a
      -- characteristic-defining ability (#65), and where the CR does want an
      -- empty maximum to be 0 it says so card-by-card (CR 714.2d, a Saga with no
      -- chapter abilities). So the fold answers Nothing -- undeterminable, the
      -- posture this codebase propagates everywhere -- and Resolve's Draw arm
      -- draws nothing for it.
      --
      -- OBSERVATIONALLY, Nothing and 0 are the same here, and the Gatherer
      -- ruling on Rishkar's Expertise ("if you control no creatures with power
      -- greater than 0 ... you draw no cards") is what this matches either way.
      -- Pawl.CountSpec pins the distinction where it IS visible, at the fold.
      HU.testCase "CR 208.2a / #65 controlling no artifacts draws nothing rather than substituting 0" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        oneWithTheMachine <- Registry.printing registry "One with the Machine"
        let base = S.landsInPlay island 4
            withLib = stockLibrary piker S.alice 10 base
            (gs, spellId) = S.handOne oneWithTheMachine withLib
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "alice drew nothing" 0 (S.handSize S.alice after)
        HU.assertEqual "and her library is untouched" 10 (length (Game.zoneMembers Zone.Library S.alice after))
    ]

countersTests :: Registry.Type.Registry -> Tasty.TestTree
countersTests registry =
  Tasty.testGroup
    "Counters"
    [ HU.testCase "CR 122.6 Battlegrowth puts a +1/+1 counter (gate)" $ do
        -- alice casts Battlegrowth on bob's Piker (2/1). After resolution the Piker
        -- is 3/2 and carries one +1/+1 counter.
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        battlegrowth <- Registry.printing registry "Battlegrowth"
        let base = S.landsInPlay forest 1
            (victim, withFoe) = S.addCreature piker S.bob base
            (gs, spellId) = S.handOne battlegrowth withFoe
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "power 3" (Just 3) (Projection.powerOf victim after)
        HU.assertEqual "toughness 2" (Just 2) (Projection.toughnessOf victim after),
      HU.testCase "CR 122 counter persists through cleanup (vs Giant Growth wearing off)" $ do
        -- After a cleanup step, the +1/+1 counter is still on the Piker.
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        battlegrowth <- Registry.printing registry "Battlegrowth"
        let base = S.landsInPlay forest 1
            (victim, withFoe) = S.addCreature piker S.bob base
            (gs, spellId) = S.handOne battlegrowth withFoe
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            afterCleanup = Expiry.dropAtCleanup resolved
        HU.assertEqual "still 3/2 after cleanup" (Just 3) (Projection.powerOf victim afterCleanup)
        HU.assertEqual "still 3/2 after cleanup" (Just 2) (Projection.toughnessOf victim afterCleanup),
      -- CR 122.1b: Spontaneous Flight is the one card where the two halves have
      -- DIFFERENT durations, which is what proves the flying is a counter rather
      -- than a second until-end-of-turn effect. The +2/+2 wears off at cleanup
      -- (CR 514.2); the flying counter does not.
      HU.testCase "CR 122.1b whole card: Spontaneous Flight pumps until EOT and grants flying for good" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        spontaneousFlight <- Registry.printing registry "Spontaneous Flight"
        let base = S.landsInPlay plains 3
            (target, withCreature) = S.addCreature piker S.alice base
            (gs, spellId) = S.handOne spontaneousFlight withCreature
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            afterCleanup = Expiry.dropAtCleanup resolved
        HU.assertBool "the Piker did not fly to begin with" (not (Projection.hasKeyword Keyword.Flying target withCreature))
        HU.assertEqual "pumped to 4/3" (Just 4) (Projection.powerOf target resolved)
        HU.assertEqual "pumped to 4/3" (Just 3) (Projection.toughnessOf target resolved)
        HU.assertBool "and it flies" (Projection.hasKeyword Keyword.Flying target resolved)
        -- The discriminator between a counter and another until-EOT effect.
        HU.assertEqual "the pump wore off" (Just 2) (Projection.powerOf target afterCleanup)
        HU.assertBool "the flying did not" (Projection.hasKeyword Keyword.Flying target afterCleanup),
      HU.testCase "CR 122.6 Instill Infection puts a -1/-1 counter and draws" $ do
        -- alice casts Instill Infection on bob's Piker; Piker becomes 1/0 and dies
        -- (704.5f); alice draws a card.
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        instillInfection <- Registry.printing registry "Instill Infection"
        forest <- Registry.printing registry "Forest"
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
        HU.assertEqual "Piker died to the -1/-1 counter (704.5f)" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "alice drew a card" (handBefore + 1) (S.handSize S.alice after),
      HU.testCase "CR 704.5q both counter kinds on one creature annihilate; net 2/1 survives" $ do
        -- Both counters on the same creature (placed directly); the SBA removes both.
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay forest 5
            (victim, withFoe) = S.addCreature piker S.alice base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs1
            after = S.settleSba gs2
        HU.assertEqual "creature survives (net 2/1)" 1 (S.creaturesInPlay S.alice after)
        HU.assertEqual "no counters remain" Map.empty (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject victim after)),
      HU.testCase "CR 122.2 Unsummon removes a counter-bearing creature's counters" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        unsummon <- Registry.printing registry "Unsummon"
        let base = S.landsInPlay island 1
            (victim, withFoe) = S.addCreature piker S.bob base
            withCounter = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
            (gs, spellId) = S.handOne unsummon withCounter
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            -- Total (no `head`): expect exactly one bounced card in hand, empty counters.
            handCounters = fmap (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h after)) (Game.zoneMembers Zone.Hand S.bob after)
        HU.assertEqual "the bounced incarnation in hand has no counters" [Map.empty] handCounters
    ]

untapTests :: Registry.Type.Registry -> Tasty.TestTree
untapTests registry =
  Tasty.testGroup
    "Untap"
    [ HU.testCase "CR 701.26b Untap untaps the slot's target" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            base = S.tapObject oid base0
            slot = SlotName.MkSlotName (Text.pack "target")
            run =
              Resolve.applyEffect
                oid
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.Untap (ObjectRef.InSlot slot))
            after = snd (Engine.runGamePure S.identityAnswer base run)
        HU.assertEqual "target is untapped" (Just TapState.Untapped) (fmap Object.tapped (Game.lookupObject oid after))
    ]

gainControlTests :: Registry.Type.Registry -> Tasty.TestTree
gainControlTests registry =
  Tasty.testGroup
    "GainControl"
    [ HU.testCase "GainControl gives the source's controller control until end of turn and re-Sicks (CR 302.6)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            slot = SlotName.MkSlotName (Text.pack "target")
            -- Apply as though a spell alice controls (controller = alice) resolved it.
            run =
              Resolve.applyEffect
                oid
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.GainControl Duration.UntilEndOfTurn slot)
            after = snd (Engine.runGamePure S.identityAnswer base run)
        HU.assertEqual "alice now controls it" (Just S.alice) (Projection.controllerOf oid after)
        HU.assertEqual "it is summoning sick for the new controller" (Just Sickness.Sick) (fmap Object.sickness (Game.lookupObject oid after))
        HU.assertEqual "control reverts after cleanup" (Just S.bob) (Projection.controllerOf oid (Expiry.dropAtCleanup after)),
      -- CR 302.6 asks whether control was CONTINUOUS. Gaining control of a
      -- permanent you already control interrupts nothing, so the clock must not
      -- reset. The sibling case above is the one where it must.
      --
      -- Isolated from haste on purpose: Act of Treason is the card that reaches
      -- this, and it grants haste, which would mask the difference on the ability
      -- path. Driving Effect.GainControl directly shows the sickness itself.
      HU.testCase "CR 302.6 GainControl does NOT re-Sick a permanent its controller already controlled" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            settled = S.runPure S.identityAnswer base (Engine.settleAll S.alice)
            slot = SlotName.MkSlotName (Text.pack "target")
            run =
              Resolve.applyEffect
                oid
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.GainControl Duration.UntilEndOfTurn slot)
            after = snd (Engine.runGamePure S.identityAnswer settled run)
        HU.assertEqual "alice controlled it before" (Just S.alice) (Projection.controllerOf oid settled)
        HU.assertEqual "and still does" (Just S.alice) (Projection.controllerOf oid after)
        HU.assertEqual "its settle under alice is untouched" (Just (Sickness.Settled S.alice)) (fmap Object.sickness (Game.lookupObject oid after))
    ]

gainPlayerCountersTests :: Registry.Type.Registry -> Tasty.TestTree
gainPlayerCountersTests registry =
  Tasty.testGroup
    "GainPlayerCounters"
    [ HU.testCase "CR 107.14 GainPlayerCounters gives the resolving controller energy" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            act = Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty (Effect.GainPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2))
            after = S.runPure S.identityAnswer gs0 act
        HU.assertEqual "alice has two energy" 2 (S.playerCounterOf PlayerCounterKind.Energy S.alice after)
    ]

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
  S.runPure answer gs (Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty Effect.Proliferate)

proliferateTests :: Registry.Type.Registry -> Tasty.TestTree
proliferateTests registry =
  Tasty.testGroup
    "Proliferate"
    [ -- CR 701.34a: "give each one additional counter of each kind that permanent
      -- or player already has." One more, never a doubling, and never a kind that
      -- was not already there.
      HU.testCase "CR 701.34a proliferate adds exactly one counter of a kind already there" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            gs = S.addCounter CounterKind.PlusOnePlusOne 2 src g0
            after = proliferate proliferatesAll src gs
        HU.assertEqual "two became three" 3 (S.counterOf CounterKind.PlusOnePlusOne src after),
      -- "each kind" is the clause a naive implementation drops: a creature holding
      -- both kinds gets one more of BOTH, not one of whichever was found first.
      --
      -- Holding both kinds at once is a state CR 704.5q would annihilate on the
      -- next state-based-action pass, which is exactly why this drives the opcode
      -- directly instead of resolving a spell: the question here is what
      -- Proliferate does to the counters it finds, not what survives afterwards.
      HU.testCase "CR 701.34a a permanent with two kinds gets one more of each" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            g1 = S.addCounter CounterKind.PlusOnePlusOne 1 src g0
            gs = S.addCounter CounterKind.MinusOneMinusOne 3 src g1
            after = proliferate proliferatesAll src gs
        HU.assertEqual "+1/+1 went up" 2 (S.counterOf CounterKind.PlusOnePlusOne src after)
        HU.assertEqual "-1/-1 went up too" 4 (S.counterOf CounterKind.MinusOneMinusOne src after),
      -- CR 701.34a: only permanents "that have a counter" are choosable, so a bare
      -- permanent is never offered and never gains a first counter this way.
      HU.testCase "CR 701.34a a permanent with no counters is not a candidate" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (bare, g1) = S.addCreature piker S.alice g0
            gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
            after = proliferate proliferatesAll src gs
        HU.assertEqual "the bare Piker gained nothing" 0 (S.counterOf CounterKind.PlusOnePlusOne bare after)
        HU.assertEqual "the countered one moved" 2 (S.counterOf CounterKind.PlusOnePlusOne src after),
      -- CR 102.2 / 109.5: `Relative Opponent` on GainPlayerCounters had no card
      -- producer until Prologue to Phyresis (#267). The arm was implemented and
      -- unproven, which design.md section 4 says is not done.
      HU.testCase "CR 122.1 whole card: Prologue to Phyresis poisons the opponent, not the caster" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        prologueToPhyresis <- Registry.printing registry "Prologue to Phyresis"
        let base = S.landsInPlay island 2
            (_, withLibrary) = S.addLibraryCard piker S.alice base
            handBefore = S.handSize S.alice withLibrary
            (gs, spellId) = S.handOne prologueToPhyresis withLibrary
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "bob is poisoned" 1 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
        HU.assertEqual "alice is not" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after)
        HU.assertEqual "and alice drew" (handBefore + 1) (S.handSize S.alice after),
      -- The discriminator, and it needs a THIRD seat: at two players `Relative
      -- Opponent` and `EachPlayer` differ only in whether the caster is included,
      -- which the case above catches -- but `Opponent` reaching only ONE of two
      -- opponents would still pass there. CR 806.1: in a Free-for-All the
      -- players compete as individuals, so every other player is an opponent and
      -- both must be poisoned. (CR 102.2 is the TWO-player rule, which is
      -- exactly what a third seat is here to get past.)
      HU.testCase "CR 806.1 at three seats every opponent is poisoned, and only opponents" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        prologueToPhyresis <- Registry.printing registry "Prologue to Phyresis"
        let (_, withLibrary) = S.addLibraryCard piker S.alice S.threePlayerGame
            -- Two Islands for the {1}{U}. S.landsInPlay builds its own two-seat
            -- game, so a three-seat board adds them one at a time instead.
            withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withLibrary [1 .. (2 :: Int)]
            (gs, spellId) = S.handOne prologueToPhyresis withMana
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- No separate "the fixture is payable" assertion: an unpayable cast is a
        -- no-op, so the poison counts below are what prove it resolved.
        HU.assertEqual "bob poisoned" 1 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
        HU.assertEqual "carol poisoned too" 1 (S.playerCounterOf PlayerCounterKind.Poison S.carol after)
        HU.assertEqual "alice untouched" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      -- CR 102.1 / CR 800.4a: an opponent is one of the OTHER people in the
      -- game, and carol is no longer one of them (#279). Poison on a departed
      -- player's record is not idle bookkeeping -- the proliferate case below
      -- reads Player.counters to build its candidate list, so this is the write
      -- that would put a non-player on the next prompt.
      HU.testCase "CR 800.4a Prologue to Phyresis does not poison a player who has left the game" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        prologueToPhyresis <- Registry.printing registry "Prologue to Phyresis"
        let (_, withLibrary) = S.addLibraryCard piker S.alice S.threePlayerGame
            withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withLibrary [1 .. (2 :: Int)]
            (gs0, spellId) = S.handOne prologueToPhyresis withMana
            gs = Departure.depart Departure.Type.Conceded S.carol gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "bob, still in the game, is poisoned" 1 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
        HU.assertEqual "carol, who left, is not" 0 (S.playerCounterOf PlayerCounterKind.Poison S.carol after)
        HU.assertEqual "and neither is the caster" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
      -- CR 701.34a: players carry counters too, and proliferate reaches them.
      HU.testCase "CR 701.34a proliferate adds to a player's poison and energy" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            g1 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g0
            gs = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice g1
            after = proliferate proliferatesAll src gs
        HU.assertEqual "bob's poison" 4 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
        HU.assertEqual "alice's energy" 2 (S.playerCounterOf PlayerCounterKind.Energy S.alice after),
      -- A player with no counters is not a candidate, the same clause the bare
      -- permanent above tests -- so proliferate never starts someone on poison.
      HU.testCase "CR 701.34a a player with no counters is not a candidate" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            gs = S.addPlayerCounter PlayerCounterKind.Poison 2 S.bob g0
            after = proliferate proliferatesAll src gs
        HU.assertEqual "alice stays clean" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
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
      HU.testCase "CR 800.4a a player who has left the game is not a proliferate candidate" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice S.threePlayerGame
            g1 = S.addPlayerCounter PlayerCounterKind.Poison 2 S.bob g0
            g2 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.carol g1
            gs = Departure.depart Departure.Type.Conceded S.carol g2
            after = proliferate proliferatesAll src gs
        HU.assertEqual "carol has left, so her poison does not move" 3 (S.playerCounterOf PlayerCounterKind.Poison S.carol after)
        HU.assertEqual "bob is still in the game, so his does" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob after),
      -- CR 701.34a: "any number" includes none. The discriminating twin of the
      -- first test -- same board, opposite answer -- so this fails if the engine
      -- proliferates for the player instead of asking.
      HU.testCase "CR 701.34a choosing nothing is legal and adds nothing" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            g1 = S.addCounter CounterKind.PlusOnePlusOne 2 src g0
            gs = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g1
            after = proliferate proliferatesNothing src gs
        HU.assertEqual "the creature is untouched" 2 (S.counterOf CounterKind.PlusOnePlusOne src after)
        HU.assertEqual "bob is untouched" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob after),
      -- The counter placement rides Event.putCounters, so CR 614's counter
      -- replacements get their opportunity -- proliferate is not a side door that
      -- bypasses Hardened Scales.
      HU.testCase "CR 614 Hardened Scales applies to the counter proliferate adds" $ do
        piker <- Registry.printing registry "Goblin Piker"
        hardenedScales <- Registry.printing registry "Hardened Scales"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addCreature hardenedScales S.alice g0
            gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
            after = proliferate proliferatesAll src gs
        HU.assertEqual "one proliferated counter became two" 3 (S.counterOf CounterKind.PlusOnePlusOne src after),
      -- Where the rules leave nothing to ask, do not ask: no permanent and no
      -- player holds a counter, so there is no choice to make.
      HU.testCase "CR 701.34a an empty candidate set raises no prompt" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            countingAnswer :: Prompt.Prompt r -> State.State Int r
            countingAnswer p = case p of
              Prompt.ChooseProliferate {} -> do
                State.modify (+ 1)
                pure (S.identityAnswer p)
              _ -> pure (S.identityAnswer p)
            asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty Effect.Proliferate)) 0
        HU.assertEqual "nobody has a counter: nothing to ask" 0 (asks gs)
        HU.assertEqual "someone does: one real decision" 1 (asks (S.addCounter CounterKind.PlusOnePlusOne 1 src gs)),
      -- The gameplay-level proof (design.md section 4): a real card, cast and
      -- resolved, doing both halves of its text.
      HU.testCase "Steady Progress whole card: proliferate, then draw a card" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        steadyProgress <- Registry.printing registry "Steady Progress"
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
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
        HU.assertEqual "the counter was proliferated" 2 (S.counterOf CounterKind.PlusOnePlusOne creature resolved)
        -- The spell left the hand and one card was drawn, so the hand is level.
        HU.assertEqual "drew a card" handBefore (length (Game.zoneMembers Zone.Hand S.alice resolved))
    ]

slotTarget :: SlotName.SlotName
slotTarget = SlotName.MkSlotName (Text.pack "target")

-- Diabolic Edict's "a creature of their choice".
creatureFilter :: Filter.Type.Filter
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

playerSacrificesTests :: Registry.Type.Registry -> Tasty.TestTree
playerSacrificesTests registry =
  Tasty.testGroup
    "PlayerSacrifices"
    [ -- CR 701.21a: "its controller moves it from the battlefield directly to its
      -- owner's graveyard." Diabolic Edict names a PLAYER, and that player picks.
      HU.testCase "Diabolic Edict: the targeted player chooses which of their creatures dies" $ do
        piker <- Registry.printing registry "Goblin Piker"
        rats <- Registry.printing registry "Typhoid Rats"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (hisPiker, g1) = S.addCreature piker S.bob g0
            (hisRats, gs) = S.addCreature rats S.bob g1
            edict = Resolve.applyEffect src S.alice Map.empty (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))
            keptRats = S.runPure (sacrifices hisPiker) gs edict
            keptPiker = S.runPure (sacrifices hisRats) gs edict
        HU.assertBool "choosing the Piker leaves the Rats" (S.onBattlefield hisRats keptRats)
        HU.assertBool "and the Piker is gone" (not (S.onBattlefield hisPiker keptRats))
        -- The discriminating twin: same board, same effect, opposite answer.
        HU.assertBool "choosing the Rats leaves the Piker" (S.onBattlefield hisPiker keptPiker)
        HU.assertBool "and the Rats are gone" (not (S.onBattlefield hisRats keptPiker))
        HU.assertBool "alice's own creature is never touched" (S.onBattlefield src keptRats),
      -- CR 701.21a: "A player can't sacrifice ... a permanent they don't control."
      -- The guard the whole issue is about, reached the only way it can be: an
      -- interpreter naming a permanent outside the offered set.
      --
      -- Bob controls TWO creatures on purpose. With one, candidates <= count and
      -- the prompt is elided, so the lying answerer is never consulted and the
      -- test passes without exercising anything -- which is what it did before
      -- review caught it.
      HU.testCase "CR 701.21a an answer naming a permanent the player does not control is refused" $ do
        piker <- Registry.printing registry "Goblin Piker"
        rats <- Registry.printing registry "Typhoid Rats"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (hers, g1) = S.addCreature piker S.alice g0
            (hisPiker, g2) = S.addCreature piker S.bob g1
            (hisRats, gs) = S.addCreature rats S.bob g2
            after = S.runPure (namesInstead hers) gs (Resolve.applyEffect src S.alice Map.empty (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
            bobsLeft = length (filter (`S.onBattlefield` after) [hisPiker, hisRats])
        HU.assertBool "alice's creature is untouched" (S.onBattlefield hers after)
        -- The edict still takes exactly one: an answer the engine refuses does not
        -- become an answer of "none". CR 609.3 caps it at what bob controls, and
        -- he controls two.
        HU.assertEqual "bob still lost exactly one of his own" 1 bobsLeft,
      -- Where the rules leave nothing to ask, don't prompt: one candidate is
      -- forced (CR 609.3 does as much as possible, which here is all of it).
      HU.testCase "CR 609.3 a lone creature is sacrificed without a prompt" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (his, gs) = S.addCreature piker S.bob g0
            countingAnswer :: Prompt.Prompt r -> State.State Int r
            countingAnswer p = case p of
              Prompt.ChooseSacrifices {} -> do
                State.modify (+ 1)
                pure (S.identityAnswer p)
              _ -> pure (S.identityAnswer p)
            act = Resolve.applyEffect src S.alice Map.empty (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))
            asked = State.execState (Engine.runGame countingAnswer gs act) 0
            after = S.runPure S.identityAnswer gs act
        HU.assertEqual "nothing to choose" 0 asked
        HU.assertBool "but it still died" (not (S.onBattlefield his after)),
      -- CR 609.3 again: a player with no creatures sacrifices nothing, and the
      -- edict simply does as much as it can -- which is nothing.
      HU.testCase "CR 609.3 an edict against an empty board does nothing" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs (Resolve.applyEffect src S.alice Map.empty (Map.singleton slotTarget True) (Map.singleton slotTarget (Recipient.ToPlayer S.bob)) (Effect.PlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
        HU.assertBool "alice keeps hers" (S.onBattlefield src after),
      -- The gameplay-level proof: the real card, cast and resolved.
      HU.testCase "Diabolic Edict whole card: cast off two Swamps, bob sacrifices" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        diabolicEdict <- Registry.printing registry "Diabolic Edict"
        let base = S.landsInPlay swamp 2
            (his, g1) = S.addCreature piker S.bob base
            (withSpell, spell) = S.handOne diabolicEdict g1
            afterCast = S.runPure (targetsPlayer S.bob) withSpell (Cast.castSpell S.alice spell)
            resolved = S.runPure (targetsPlayer S.bob) afterCast Stack.resolveTop
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
        HU.assertBool "bob's creature was sacrificed" (not (S.onBattlefield his resolved))
    ]

createEmblemTests :: Registry.Type.Registry -> Tasty.TestTree
createEmblemTests registry =
  Tasty.testGroup
    "CreateEmblem"
    [ HU.testCase "CR 114.2 CreateEmblem puts an emblem in the command zone under the resolver" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            act = Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (Printing.card piker))
            after = S.runPure S.identityAnswer gs0 act
            emblems = filter (\oid -> fmap Object.zone (Game.lookupObject oid after) == Just Zone.Command) (Set.toList (GameState.command after))
        HU.assertEqual "one emblem in command" 1 (Set.size (GameState.command after))
        HU.assertEqual "owned by the resolver" [Just S.alice] (fmap (\oid -> fmap Object.owner (Game.lookupObject oid after)) emblems)
    ]

becomeMonarchTests :: Registry.Type.Registry -> Tasty.TestTree
becomeMonarchTests registry =
  Tasty.testGroup
    "BecomeMonarch"
    [ HU.testCase "CR 725 BecomeMonarch TheController makes the resolver the monarch" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs0 (Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty (Effect.BecomeMonarch MonarchTarget.TheController))
        HU.assertEqual "alice is monarch" (Just S.alice) (GameState.monarch after)
        HU.assertBool "a BecameMonarch event was recorded" (elem (GameEvent.BecameMonarch S.alice) (GameState.events after))
    ]

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
exileUntilMonarchTests :: Registry.Type.Registry -> Tasty.TestTree
exileUntilMonarchTests registry =
  Tasty.testGroup
    "ExileUntilMonarch"
    [ -- Reachable at two seats: CR 603.3b lets alice order Palace Jailer's two
      -- entry triggers, so the exile can resolve BEFORE she becomes the monarch,
      -- while bob still holds the crown.
      HU.testCase "CR 725 an exile that resolves while an opponent is already the monarch does not return at once" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            base = base0 {GameState.monarch = Just S.bob}
            slot = SlotName.MkSlotName (Text.pack "target")
            exile =
              Resolve.applyEffect
                S.noSource
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.ExileUntilMonarch slot)
            exiled = snd (Engine.runGamePure S.identityAnswer base exile)
            settled = snd (Engine.runGamePure S.identityAnswer exiled Monarch.returnExiledForMonarch)
        HU.assertEqual "the watch was registered" 1 (Map.size (GameState.exiledUntilMonarch exiled))
        HU.assertEqual "bob is still the monarch, unchanged" (Just S.bob) (GameState.monarch settled)
        HU.assertEqual "nothing came back to the battlefield" 0 (Set.size (GameState.battlefield settled))
        HU.assertEqual "and the watch is still armed" 1 (Map.size (GameState.exiledUntilMonarch settled)),
      -- The whole arc, still two seats. The crown must actually CHANGE HANDS to an
      -- opponent before the creature comes back, and alice taking it herself in
      -- between must not discharge the watch.
      HU.testCase "CR 725 the exile returns when a NEW monarch is crowned who is an opponent" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            base = base0 {GameState.monarch = Just S.bob}
            slot = SlotName.MkSlotName (Text.pack "target")
            exile =
              Resolve.applyEffect
                S.noSource
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.ExileUntilMonarch slot)
            exiled = snd (Engine.runGamePure S.identityAnswer base exile)
            -- Palace Jailer's OTHER entry trigger: alice takes the crown. She is
            -- not her own opponent, so this must not return the creature.
            alicesCrown = snd (Engine.runGamePure S.identityAnswer exiled {GameState.monarch = Just S.alice} Monarch.returnExiledForMonarch)
            -- bob deals combat damage to the monarch (CR 725.3) and takes it back.
            bobsCrown = snd (Engine.runGamePure S.identityAnswer alicesCrown {GameState.monarch = Just S.bob} Monarch.returnExiledForMonarch)
        HU.assertEqual "alice holding the crown does not discharge the watch" 1 (Map.size (GameState.exiledUntilMonarch alicesCrown))
        HU.assertEqual "nor return the creature" 0 (Set.size (GameState.battlefield alicesCrown))
        HU.assertEqual "bob retaking it does return the creature" 1 (Set.size (GameState.battlefield bobsCrown))
        HU.assertEqual "and discharges the watch" 0 (Map.size (GameState.exiledUntilMonarch bobsCrown)),
      -- The crown VANISHING is not an opponent becoming the monarch. CR 725.1's
      -- ruling says the game keeps exactly one monarch once it has one, and the
      -- single way back to none is CR 725.4's last player standing leaving -- but
      -- the watch must not read "no monarch" as "not the controller" and fire.
      HU.testCase "CR 725.1 the crown vanishing is not an opponent becoming the monarch" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            base = base0 {GameState.monarch = Just S.bob}
            slot = SlotName.MkSlotName (Text.pack "target")
            exile =
              Resolve.applyEffect
                S.noSource
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.ExileUntilMonarch slot)
            exiled = snd (Engine.runGamePure S.identityAnswer base exile)
            noMonarch = snd (Engine.runGamePure S.identityAnswer exiled {GameState.monarch = Nothing} Monarch.returnExiledForMonarch)
        HU.assertEqual "the watch is still armed" 1 (Map.size (GameState.exiledUntilMonarch noMonarch))
        HU.assertEqual "and nothing returned" 0 (Set.size (GameState.battlefield noMonarch))
    ]

-- M4.5 P1 gate: Act of Treason strings GainControl + Untap + ModifyTarget
-- (GainKeyword Haste) together end to end -- cast, resolve, attack, revert.
actOfTreasonTests :: Registry.Type.Registry -> Tasty.TestTree
actOfTreasonTests registry =
  Tasty.testGroup
    "Act of Treason"
    [ HU.testCase "steal, untap, haste, attack, then revert" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        actOfTreason <- Registry.printing registry "Act of Treason"
        let base0 = S.landsInPlay mountain 3 -- alice: {R}{R}{R} for {2}{R}
            (oid, base1) = S.addCreature piker S.bob base0
            base = S.tapObject oid base1 -- start it tapped to prove the untap rider
            (gs1, spellId) = S.handOne actOfTreason base
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "alice controls the Piker" (Just S.alice) (Projection.controllerOf oid resolved)
        HU.assertEqual "the untap rider untapped it" (Just TapState.Untapped) (fmap Object.tapped (Game.lookupObject oid resolved))
        HU.assertBool "it has haste" (Projection.hasKeyword Keyword.Haste oid resolved)
        HU.assertBool "alice may attack with it this turn" (oid `elem` Combat.legalAttackers S.alice resolved)
        HU.assertBool "bob may not attack with it" (oid `notElem` Combat.legalAttackers S.bob resolved)
        HU.assertEqual "control reverts at cleanup" (Just S.bob) (Projection.controllerOf oid (Expiry.dropAtCleanup resolved))
    ]

-- CR 603.5 / 608.2d: an OPTIONAL effect -- "you may" -- decided as the ability
-- resolves, not as it is put on the stack.
--
-- Renewed Faith is the card: a {2}{W} instant with "You gain 6 life", Cycling
-- {1}{W}, and "When you cycle this card, you may gain 2 life". It targets
-- nothing, so nothing here can be passing on the targeting machinery: the only
-- new thing is whether the trigger's one effect happens.
optionalEffectTests :: Registry.Type.Registry -> Tasty.TestTree
optionalEffectTests registry =
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
        faith <- Registry.printing registry printing
        plains <- Registry.printing registry land
        let (g1, faithId) = S.handOne faith (S.landsInPlay plains 2)
        pure (g1 {GameState.priority = Just S.alice}, faithId)
      -- Deem Worthy in hand with four Mountains for its {3}{R} cycling, and one
      -- Goblin Piker on the battlefield as the only legal creature target.
      deemWorthyBoard = do
        worthy <- Registry.printing registry "Deem Worthy"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 4)
            (g1, worthyId) = S.handOne worthy g0
        pure (g1 {GameState.priority = Just S.alice}, worthyId, creature)
   in Tasty.testGroup
        "OptionalEffect"
        [ HU.testCase "CR 603.5 declining the may gains nothing, and the ability still resolves" $ do
            (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
            case Activate.abilitiesFor faithId gs of
              [ability] -> do
                let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice faithId ability)
                    placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
                    after = S.runPure S.identityAnswer placed Stack.resolveTop
                HU.assertEqual "the trigger is on the stack, above the draw" 2 (length (GameState.stack placed))
                HU.assertEqual "declining gains no life" (Just 20) (S.lifeOf S.alice after)
                -- CR 608.2n, not CR 608.2b: a declined "may" is not a fizzle.
                -- The ability resolved -- it just did nothing -- and leaving the
                -- stack is the last part of that resolution.
                HU.assertEqual "and the ability left the stack anyway -- it did not fizzle" 1 (length (GameState.stack after))
              abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
          HU.testCase "CR 603.5 whole card: cycling Renewed Faith and taking the may gains exactly 2" $ do
            (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
            case Activate.abilitiesFor faithId gs of
              [ability] -> do
                let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice faithId ability)
                    placed = S.runPure takeOptional cycled Engine.settleForPriority
                    after = S.runPure takeOptional placed Stack.resolveTop
                HU.assertEqual "the Faith is in the graveyard, cycled" 1 (length (Game.zoneMembers Zone.Graveyard S.alice cycled))
                HU.assertEqual "taking it gains exactly 2" (Just 22) (S.lifeOf S.alice after)
              abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
          -- The prompt itself, not just its consequence: recording the run puts
          -- the answer in the transcript, which is the only place a raised
          -- prompt is directly observable. Twinned with the mandatory control
          -- below, which must record NO such response.
          HU.testCase "CR 608.2d the choice is announced as a real prompt, and lands in the transcript" $ do
            (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
            case Activate.abilitiesFor faithId gs of
              [ability] -> do
                let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice faithId ability)
                    placed = S.runPure takeOptional cycled Engine.settleForPriority
                    (_, transcript) = Replay.record takeOptional placed Stack.resolveTop
                HU.assertEqual
                  "exactly one may was asked, and it was taken"
                  [Response.ChoseOptional OptionalDecision.Exercises]
                  (filter isOptionalResponse transcript)
              abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
          -- The control: Windcaller Aven's cycling trigger is the SAME shape one
          -- word short of a "may", and it must not be asked about at all.
          HU.testCase "CR 603.5 a mandatory cycling trigger raises no such prompt" $ do
            aven <- Registry.printing registry "Windcaller Aven"
            island <- Registry.printing registry "Island"
            piker <- Registry.printing registry "Goblin Piker"
            let (_, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
                (g1, avenId) = S.handOne aven g0
                gs = g1 {GameState.priority = Just S.alice}
            case Activate.abilitiesFor avenId gs of
              [ability] -> do
                let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice avenId ability)
                    placed = S.runPure takeOptional cycled Engine.settleForPriority
                    (_, transcript) = Replay.record takeOptional placed Stack.resolveTop
                HU.assertEqual "nothing was asked about a may" [] (filter isOptionalResponse transcript)
              abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
          -- The second card, and the one that puts a TARGET under the "may":
          -- Deem Worthy {4}{R} Instant, "Deem Worthy deals 7 damage to target
          -- creature. Cycling {3}{R}. When you cycle this card, you may have it
          -- deal 2 damage to target creature." The target is chosen as the
          -- trigger goes on the stack (CR 603.3d) and the option only on
          -- resolution (CR 603.5), which is the ordering a mode-selection
          -- encoding of "may" would have collapsed.
          HU.testCase "CR 603.5 whole card: cycling Deem Worthy and taking the may deals 2 to the target" $ do
            (gs, worthyId, piker) <- deemWorthyBoard
            case Activate.abilitiesFor worthyId gs of
              [ability] -> do
                let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice worthyId ability)
                    placed = S.runPure takeOptional cycled Engine.settleForPriority
                    taken = S.runPure takeOptional placed Stack.resolveTop
                    declined = S.runPure S.identityAnswer placed Stack.resolveTop
                HU.assertEqual "taking it marks 2 damage" (Just 2) (fmap Object.damage (Game.lookupObject piker taken))
                HU.assertEqual "declining marks none" (Just 0) (fmap Object.damage (Game.lookupObject piker declined))
              abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
          -- CR 608.2b before CR 603.5: with its only target gone, the ability
          -- "doesn't resolve. It's removed from the stack" -- so there is nothing
          -- left for the "may" to decide and the prompt is never raised. The
          -- engine does not ask a question whose answer cannot matter.
          HU.testCase "CR 608.2b a fizzled optional trigger is not asked about at all" $ do
            (gs, worthyId, piker) <- deemWorthyBoard
            case Activate.abilitiesFor worthyId gs of
              [ability] -> do
                let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice worthyId ability)
                    placed = S.runPure takeOptional cycled Engine.settleForPriority
                    gone = S.runPure S.identityAnswer placed (Event.changeZone piker Zone.Graveyard)
                    ((_, after), transcript) = Replay.record takeOptional gone Stack.resolveTop
                HU.assertEqual "the trigger left the stack" (length (GameState.stack placed) - 1) (length (GameState.stack after))
                HU.assertEqual "and no may was ever asked" [] (filter isOptionalResponse transcript)
              abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities))
        ]

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

destroyAllTests :: Registry.Type.Registry -> Tasty.TestTree
destroyAllTests registry =
  Tasty.testGroup
    "DestroyAll"
    [ -- CR 109.2: "Destroy all creatures" includes no "card" or "spell", so it
      -- means every CREATURE PERMANENT on the battlefield -- both players' and,
      -- pointedly, the caster's own. Nothing else on the battlefield is touched.
      HU.testCase "Day of Judgment destroys every creature, the caster's own included, and leaves noncreature permanents alone" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let (hers, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (his, g2) = S.addCreature piker S.bob g1
            (equipment, g3) = S.addCreature bonesplitter S.alice g2
            resolved = castDayOfJudgment plains dayOfJudgment g3
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
        HU.assertBool "bob's creature died" (not (S.onBattlefield his resolved))
        HU.assertBool "and so did alice's own" (not (S.onBattlefield hers resolved))
        HU.assertBool "the Equipment is not a creature and stands" (S.onBattlefield equipment resolved)
        HU.assertEqual "no creatures left at all" 0 (Set.size (Set.filter (`Projection.isCreatureOf` resolved) (GameState.battlefield resolved))),
      -- CR 702.12b: "A permanent with indestructible can't be destroyed." The
      -- mass form goes through Event.destroy exactly as the single-target form
      -- does, so it inherits that gate rather than bypassing it.
      HU.testCase "CR 702.12b an indestructible creature survives Day of Judgment" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let (myr, g1) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
            (his, g2) = S.addCreature piker S.bob g1
            resolved = castDayOfJudgment plains dayOfJudgment g2
        HU.assertBool "the Myr can't be destroyed" (S.onBattlefield myr resolved)
        HU.assertBool "the Piker can" (not (S.onBattlefield his resolved)),
      -- CR 701.19a: a regeneration shield "protects the permanent the next time
      -- it would be destroyed this turn". Day of Judgment says nothing about
      -- regeneration, so it carries Regenerability.Regenerable and the shield
      -- applies -- the creature is instead tapped and stays.
      HU.testCase "CR 701.19a a regeneration shield saves its creature from Day of Judgment" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let (shielded, g1) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            (bare, g2) = S.addCreature piker S.bob g1
            resolved = castDayOfJudgment plains dayOfJudgment (S.addRegenShield shielded g2)
        HU.assertBool "the shielded creature stands" (S.onBattlefield shielded resolved)
        HU.assertEqual "and CR 701.19a taps it" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject shielded resolved))
        HU.assertBool "its unshielded twin died" (not (S.onBattlefield bare resolved)),
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
      HU.testCase "CR 608.2f the affected set is fixed before the first destruction: March of the Machines and the Bonesplitter it animates die together" $ do
        plains <- Registry.printing registry "Plains"
        opalescence <- Registry.printing registry "Opalescence"
        march <- Registry.printing registry "March of the Machines"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let (opal, g1) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
            (animator, g2) = S.addCreature march S.alice g1
            (equipment, board) = S.addCreature bonesplitter S.alice g2
        HU.assertBool "setup: March is a creature via Opalescence" (Projection.isCreatureOf animator board)
        HU.assertBool "setup: the Bonesplitter is a creature via March" (Projection.isCreatureOf equipment board)
        HU.assertBool "setup: March is swept first" (animator < equipment)
        let resolved = castDayOfJudgment plains dayOfJudgment board
        HU.assertBool "March died" (not (S.onBattlefield animator resolved))
        HU.assertBool "and so did the Bonesplitter it animated" (not (S.onBattlefield equipment resolved))
        HU.assertBool "Opalescence animates each OTHER enchantment, so it was never a creature" (S.onBattlefield opal resolved),
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
      HU.testCase "CR 608.2f every victim's CR 702.12b gate is judged before any of them dies: the Walls of Ba Sing Se die, what they protect stands" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        walls <- Registry.printing registry "The Walls of Ba Sing Se"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let (granter, g1) = S.addCreature walls S.alice (Setup.emptyGame S.bothPlayers)
            (protected, g2) = S.addCreature piker S.alice g1
            (his, board) = S.addCreature piker S.bob g2
        HU.assertBool "setup: the Walls are swept before the creature they protect" (granter < protected)
        HU.assertBool "setup: the Walls do not benefit from their own grant" (not (Projection.hasKeyword Keyword.Indestructible granter board))
        HU.assertBool "setup: their controller's other creature does" (Projection.hasKeyword Keyword.Indestructible protected board)
        HU.assertBool "setup: the opponent's does not" (not (Projection.hasKeyword Keyword.Indestructible his board))
        let resolved = castDayOfJudgment plains dayOfJudgment board
        HU.assertBool "the Walls are destroyed" (not (S.onBattlefield granter resolved))
        HU.assertBool "the creature they protected stands" (S.onBattlefield protected resolved)
        HU.assertBool "and the opponent's creature, never protected, died" (not (S.onBattlefield his resolved)),
      -- The same board with the two permanents added in the other order, so the
      -- Walls are swept LAST. CR 608.2f leaves nothing for the sweep order to
      -- decide here, and that is the claim: the outcome is identical. This is the
      -- arrangement the sequential reading happens to get right, and it is worth
      -- pinning precisely because it is the one that would keep passing.
      HU.testCase "CR 608.2f the outcome does not depend on where the granter falls in the sweep order" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        walls <- Registry.printing registry "The Walls of Ba Sing Se"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let (protected, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (granter, board) = S.addCreature walls S.alice g1
        HU.assertBool "setup: the Walls are swept last this time" (protected < granter)
        let resolved = castDayOfJudgment plains dayOfJudgment board
        HU.assertBool "the Walls are destroyed" (not (S.onBattlefield granter resolved))
        HU.assertBool "the creature they protected stands" (S.onBattlefield protected resolved),
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
      HU.testCase "CR 608.2f a Rest in Peace dying in the sweep still exiles the cards the sweep puts into graveyards" $ do
        plains <- Registry.printing registry "Plains"
        opalescence <- Registry.printing registry "Opalescence"
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let (opal, g1) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
            (rip, g2) = S.addCreature restInPeace S.alice g1
            (his, board) = S.addCreature piker S.bob g2
        HU.assertBool "setup: Opalescence animates Rest in Peace" (Projection.isCreatureOf rip board)
        HU.assertBool "setup: Rest in Peace is swept before the Piker" (rip < his)
        let resolved = castDayOfJudgment plains dayOfJudgment board
        HU.assertBool "Rest in Peace died" (not (S.onBattlefield rip resolved))
        HU.assertBool "and so did the Piker" (not (S.onBattlefield his resolved))
        HU.assertEqual "the Piker was exiled, not buried" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
        HU.assertEqual "the Piker's card is in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob resolved))
        HU.assertEqual "and Rest in Peace exiled its own card too" 1 (length (Game.zoneMembers Zone.Exile S.alice resolved))
        HU.assertBool "Opalescence animates each OTHER enchantment, so it stands" (S.onBattlefield opal resolved),
      -- CR 115.10a: "Unless that object or player is identified by the word
      -- 'target' ... it's not a target." "All creatures" is not a target, so the
      -- card declares no target spec and the cast never raises a target prompt
      -- -- and CR 608.2b, which is about targets, has nothing to fizzle.
      HU.testCase "CR 115.10a Day of Judgment targets nothing: no target spec and no target prompt" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
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
        HU.assertEqual "no target spec anywhere on the card" Map.empty (Modal.allTargetSpecs (Card.Type.spell card))
        HU.assertEqual "and nothing was asked to target" 0 asked
        -- The board still resolves the way the first test says it does, from the
        -- same cast -- so "targets nothing" is not "affects nothing".
        HU.assertBool "the creature still died" (not (S.onBattlefield his (castDayOfJudgment plains dayOfJudgment g1)))
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Resolve" [targetTests registry, resolveTests registry, fizzleTests registry, indestructibleTests registry, zoneChangeTests registry, drawCardTests registry, loseLifeTests registry, greatestTests registry, counterTests registry, countersTests registry, untapTests registry, gainControlTests registry, gainPlayerCountersTests registry, proliferateTests registry, playerSacrificesTests registry, createEmblemTests registry, becomeMonarchTests registry, exileUntilMonarchTests registry, actOfTreasonTests registry, optionalEffectTests registry, destroyAllTests registry]
