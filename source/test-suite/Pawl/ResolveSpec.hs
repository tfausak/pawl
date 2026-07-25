{-# LANGUAGE GADTs #-}

-- Covers Pawl.Resolve and Pawl.Target: targeting legality, spell resolution, and
-- the CR 608.2b fizzle.
module Pawl.ResolveSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Count as Count
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Aggregation as Aggregation
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.MonarchTarget as MonarchTarget
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.Zone as Zone
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
          (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs),
      HU.testCase "a departed player is not a legal target" $
        let gs = Departure.depart Departure.Type.Lost S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertBool
              "bob gone"
              (not (Set.member (Recipient.ToPlayer S.bob) (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs))),
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
        HU.assertBool "legal while fielded" (Target.stillLegal S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gs)
        HU.assertBool "illegal once moved" (not (Target.stillLegal S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing) gone)),
      HU.testCase "legalSets maps each slot to its legal recipients" $
        let specs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)
            gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual
              "one slot, two players"
              (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
              (Target.legalSets S.noSource specs gs),
      HU.testCase "CR 115.4 CreatureTarget offers creatures but no players" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        HU.assertEqual
          "just the creature"
          (Set.singleton (Recipient.ToCreature oid))
          (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gs),
      HU.testCase "CR 601.2c CreatureTarget has an empty legal set with no creatures" $
        HU.assertBool
          "nothing to target"
          (Set.null (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Creatures Nothing) (Setup.emptyGame S.bothPlayers))),
      HU.testCase "CR 608.2b a creature that left is no longer a legal CreatureTarget" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
        HU.assertBool "legal while fielded" (Target.stillLegal S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gs)
        HU.assertBool "illegal once moved" (not (Target.stillLegal S.noSource (Recipient.ToCreature oid) (TargetSpec.MkTargetSpec Pool.Creatures Nothing) gone)),
      HU.testCase "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (permId, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        HU.assertBool
          "the permanent is a legal object target"
          (Set.member (Recipient.ToObject permId) (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.SpellsAndPermanents Nothing) gs)),
      HU.testCase "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $ do
        piker <- Registry.printing registry "Goblin Piker"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (permId, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            (spellId, gs) = S.spellOnStack lightningBolt S.alice base
            legal = Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Spells Nothing) gs
        HU.assertBool "the stack spell is a legal target" (Set.member (Recipient.ToObject spellId) legal)
        HU.assertBool "the battlefield permanent is not a legal target" (not (Set.member (Recipient.ToObject permId) legal)),
      HU.testCase "LandTarget offers a land as an object target, not a creature or player" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
        HU.assertBool "the land is legal" (Set.member (Recipient.ToObject landId) (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs))
        HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Land))) gs))),
      HU.testCase "CR 115: PlayerTarget is exactly the players still in the game" $
        let gs = Setup.emptyGame S.bothPlayers
            expected = Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]
         in HU.assertEqual "both players, no creatures" expected (Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Players Nothing) gs),
      -- CR 115.1a / 700.2c: "target Wall" (Chaos Charm) restricts CreatureTarget to
      -- creatures whose PROJECTED subtypes include Wall. Wall of Stone (a real 0/8
      -- Creature - Wall, M4g) is the Wall; a Piker is the non-Wall control.
      HU.testCase "CR 115.1a / 700.2c \"target Wall\" offers a Wall creature but not a non-Wall creature" $ do
        wallOfStone <- Registry.printing registry "Wall of Stone"
        piker <- Registry.printing registry "Goblin Piker"
        let (wallId, base) = S.addCreature wallOfStone S.bob (Setup.emptyGame S.bothPlayers)
            (pikerId, gs) = S.addCreature piker S.alice base
            slot = SlotName.MkSlotName (Text.pack "target")
            legal = Map.findWithDefault Set.empty slot (Target.legalSets S.noSource (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.HasSubtype Subtype.Wall)))) gs)
        HU.assertBool "the Wall is legal" (Set.member (Recipient.ToCreature wallId) legal)
        HU.assertBool "the non-Wall creature is not legal" (not (Set.member (Recipient.ToCreature pikerId) legal)),
      HU.testCase "CR 115.1a ArtifactTarget is the battlefield's projected artifacts" $ do
        -- boardWithCreatureArtifactLand: alice has a Piker, a Mindslaver
        -- (Legendary Artifact) and a Mountain.
        piker <- Registry.printing registry "Goblin Piker"
        mindslaver <- Registry.printing registry "Mindslaver"
        mountain <- Registry.printing registry "Mountain"
        let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
            legal = Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))) gs
        HU.assertEqual "exactly the artifact" (Set.singleton (Recipient.ToObject (S.artifactId gs))) legal
        HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) legal)),
      HU.testCase "CR 115.1a / 109.5 OpponentCreatureTarget excludes the source's controller's creatures" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        let gs0 = Setup.emptyGame S.bothPlayers
            (mine, gs1) = S.addCreature piker S.alice gs0
            (theirs, gs2) = S.addCreature warMammoth S.bob gs1
            legal = Target.legalRecipients mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs2
        HU.assertEqual "only the opponent's creature" (Set.singleton (Recipient.ToCreature theirs)) legal
        HU.assertBool "not the source's controller's own" (not (Set.member (Recipient.ToCreature mine) legal)),
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
            legal = Target.legalRecipients mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) gs3
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
          (Target.legalRecipients mine (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen)
        HU.assertEqual
          "for bob's source, the two alice now controls"
          (Set.fromList [Recipient.ToCreature mine, Recipient.ToCreature theirs])
          (Target.legalRecipients alsoTheirs (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))) stolen),
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
            legal = Target.legalRecipients S.noSource spec gs
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
        HU.assertEqual "all creatures legal" expectedAllCreatures (Target.legalRecipients S.noSource spec gs),
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
          (Target.legalSets srcId specs gs),
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
          (Target.legalSets srcId specs gs),
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
                legal = Target.legalRecipients S.noSource spec gs
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
                legalBefore = Target.legalRecipients S.noSource spec gs
                pumped = S.withEffect smallOid (Modification.ModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0)) gs
                legalAfter = Target.legalRecipients S.noSource spec pumped
            HU.assertBool "power 2 is illegal (below the PowerAtLeast 4 floor)" (not (Set.member (Recipient.ToCreature smallOid) legalBefore))
            HU.assertBool "pumped to power 4 becomes legal" (Set.member (Recipient.ToCreature smallOid) legalAfter)
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
                      (Seq.singleton (Mode.MkMode (Seq.singleton (Effect.ChangeText slot)) Map.empty))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing
                }
        HU.assertEqual "slotsOf" (Set.singleton slot) (Resolve.slotsOf (Effect.ChangeText slot))
        HU.assertEqual "textChangeSlots" [slot] (Resolve.textChangeSlots card),
      HU.testCase "CR 605 manaProduced reads AddMana, nothing else" $ do
        HU.assertEqual "add mana" (Just (ManaType.Colored Color.Green)) (Resolve.manaProduced (Effect.AddMana (ManaType.Colored Color.Green)))
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
                  Object.sickness = Sickness.Settled,
                  -- CR 700.2: Landform has one mode; a directly-built stack object
                  -- (bypassing Cast.castSpell) must stamp it chosen (mode 0), or
                  -- Resolve.effectsOf/resolveSpell -- now scoped to CHOSEN modes --
                  -- would see no effects and no target specs at all.
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToObject targetLand)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
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
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Map.empty,
                  Object.counters = Map.empty,
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
              [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1))
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
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
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
                (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter]) Map.empty)) (ModeSelection.ChooseExactly 1))
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj =
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled (Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
        HU.assertEqual "one permanent on the battlefield" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
        HU.assertEqual "it is tapped" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "library empty" [] (Game.zoneMembers Zone.Library S.alice resolved),
      HU.testCase "CR 701.23b Search may fail to find" $ do
        mountain <- Registry.printing registry "Mountain"
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addLibraryCard mountain S.alice base
            ability = ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter]) Map.empty)) (ModeSelection.ChooseExactly 1))
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj = Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled (Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty ts
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
                (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.Search basicLandFilter]) Map.empty)) (ModeSelection.ChooseExactly 1))
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj =
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled (Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0))) Map.empty ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
        HU.assertEqual "the basic land is offered and fetched to the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Mountain") S.alice resolved)
        HU.assertBool "the nonland is not offered -- it remains in the library" (elem pikerId (Game.zoneMembers Zone.Library S.alice resolved)),
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
                (Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.ExileAllGraveyards]) Map.empty)) (ModeSelection.ChooseExactly 1))
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
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
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
                      (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.ControlPlayerNextTurn slot]) (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Players Nothing))))
                      (ModeSelection.ChooseExactly 1)
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
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
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
                      (Seq.singleton (Mode.MkMode (Seq.singleton Effect.RestartGame) Map.empty))
                      (ModeSelection.ChooseExactly 1)
                }
            abilObj =
              Object.MkObject
                { Object.owner = S.bob,
                  Object.source = Source.OfAbility aliceId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
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
                      (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage slot (Quantity.Literal 3)]) Map.empty))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing
                }
            spellObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfToken card,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.timestamp = ts
                }
            g3 = g2 {GameState.objects = Map.insert spellId spellObj (GameState.objects g2), GameState.stack = spellId : GameState.stack g2}
            after = snd (Engine.runGamePure S.identityAnswer g3 (Resolve.resolveSpellWith stubRunner spellId))
        HU.assertEqual "bob (the derived loser) lost 3 life to the follow-on DealDamage" (Just 17) (S.lifeOf S.bob after),
      HU.testCase "CR 729.1b: PlaySubgame's derived loser is drawn from the subgame roster, not the full main-game seating (a departed seat is never the loser)" $ do
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        -- bob departed the MAIN game before this effect resolves, so bob was never
        -- seated for the subgame (Setup.subgameStateFrom seats only
        -- Departure.stillPlayingInOrder) -- only alice and carol played it. The
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
                      (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage slot (Quantity.Literal 3)]) Map.empty))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing
                }
            spellObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfToken card,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
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
        HU.assertEqual "Dragon Fodder went to the graveyard (CR 608.2n)" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 615 Fog prevents combat damage but not spell damage (the gate)" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        fog <- Registry.printing registry "Fog"
        let base = S.landsInPlay forest 1
            (victim, gs0) = S.addCreature piker S.bob base
            (gs1, fogId) = S.handOne fog gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice fogId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            combat = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False DamageKind.Combat])
            spell = S.runPure S.identityAnswer resolved (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False DamageKind.Noncombat])
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
          (Count.evaluate (\oid -> Just (Projection.viewOfObject oid gs)) (Filter.MkContext (Just S.alice) Nothing) gs yourHand)
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
                (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.ControlPlayerNextTurn slot]) (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Players Nothing))))
                (ModeSelection.ChooseExactly 1)
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
            Object.sickness = Sickness.Settled,
            Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer target)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
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
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
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
      obj = Object.MkObject pid (Source.OfCard printing) Zone.Hand TapState.Untapped 0 Sickness.Settled Map.empty Map.empty (Timestamp.MkTimestamp 0)
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
            run = Resolve.resolveEffects abilId source [Effect.Destroy targetSlot, Effect.Draw (Quantity.Literal 1)] specs
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
            wounded = S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True False DamageKind.Combat)] gs
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
          obj = Object.MkObject pid (Source.OfCard printing) Zone.Hand TapState.Untapped 0 Sickness.Settled Map.empty Map.empty (Timestamp.MkTimestamp 0)
       in g1
            { GameState.objects = Map.insert oid obj (GameState.objects g1),
              GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand g1)
            }

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
      HU.testCase "CR 120 Divination draws its controller two cards" $ do
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
      HU.testCase "CR 121.3 a Draw that outruns the library records the loss" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        divination <- Registry.printing registry "Divination"
        let base = S.landsInPlay island 3
            (_, g1) = S.addLibraryCard piker S.alice base
            (gs, spellId) = S.handOne divination g1
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertBool "drewFromEmpty marked" (Set.member S.alice (GameState.drewFromEmpty after)),
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
        HU.assertEqual "both cards discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
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
                (Effect.Untap slot)
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
        HU.assertEqual "control reverts after cleanup" (Just S.bob) (Projection.controllerOf oid (Expiry.dropAtCleanup after))
    ]

gainPlayerCountersTests :: Registry.Type.Registry -> Tasty.TestTree
gainPlayerCountersTests registry =
  Tasty.testGroup
    "GainPlayerCounters"
    [ HU.testCase "CR 107.14 GainPlayerCounters gives the resolving controller energy" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            act = Resolve.applyEffect src S.alice Map.empty Map.empty Map.empty (Effect.GainPlayerCounters PlayerCounterKind.Energy (Quantity.Literal 2))
            after = S.runPure S.identityAnswer gs0 act
        HU.assertEqual "alice has two energy" 2 (S.playerCounterOf PlayerCounterKind.Energy S.alice after)
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

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Resolve" [targetTests registry, resolveTests registry, fizzleTests registry, indestructibleTests registry, zoneChangeTests registry, drawCardTests registry, counterTests registry, countersTests registry, untapTests registry, gainControlTests registry, gainPlayerCountersTests registry, createEmblemTests registry, becomeMonarchTests registry, actOfTreasonTests registry]
