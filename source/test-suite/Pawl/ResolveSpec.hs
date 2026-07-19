{-# LANGUAGE GADTs #-}

-- Covers Pawl.Resolve and Pawl.Target: targeting legality, spell resolution, and
-- the CR 608.2b fizzle.
module Pawl.ResolveSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Decide as Decide
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AdditionalCost as AdditionalCost
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardCriterion as CardCriterion
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Append a stored continuous effect affecting exactly `oid`, at timestamp `ts`
-- (mirrors ProjectionSpec.withEffect; used to pre-stamp a hack on a stack spell).
withEffect :: ObjectId.ObjectId -> Timestamp.Timestamp -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffect oid ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

targetTests :: Cards.Cards -> Tasty.TestTree
targetTests cards =
  Tasty.testGroup
    "Target"
    [ HU.testCase "CR 115.4 AnyTarget offers every creature and every playing player" $
        let (oid, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "creature and both players"
              (Set.fromList [Recipient.ToCreature oid, Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])
              (Target.legalRecipients TargetSpec.AnyTarget gs),
      HU.testCase "a departed player is not a legal target" $
        let gs = Sba.depart S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertBool
              "bob gone"
              (not (Set.member (Recipient.ToPlayer S.bob) (Target.legalRecipients TargetSpec.AnyTarget gs))),
      HU.testCase "CR 608.2b a creature that left its zone is no longer legal" $
        let (oid, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            gone = Event.changeZone oid Zone.Graveyard gs
         in do
              HU.assertBool "legal while fielded" (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.AnyTarget gs)
              HU.assertBool "illegal once moved" (not (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.AnyTarget gone)),
      HU.testCase "legalSets maps each slot to its legal recipients" $
        let specs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget
            gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual
              "one slot, two players"
              (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]))
              (Target.legalSets specs gs),
      HU.testCase "CR 115.4 CreatureTarget offers creatures but no players" $
        let (oid, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "just the creature"
              (Set.singleton (Recipient.ToCreature oid))
              (Target.legalRecipients TargetSpec.CreatureTarget gs),
      HU.testCase "CR 601.2c CreatureTarget has an empty legal set with no creatures" $
        HU.assertBool
          "nothing to target"
          (Set.null (Target.legalRecipients TargetSpec.CreatureTarget (Setup.emptyGame S.bothPlayers))),
      HU.testCase "CR 608.2b a creature that left is no longer a legal CreatureTarget" $
        let (oid, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            gone = Event.changeZone oid Zone.Graveyard gs
         in do
              HU.assertBool "legal while fielded" (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.CreatureTarget gs)
              HU.assertBool "illegal once moved" (not (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.CreatureTarget gone)),
      HU.testCase "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $
        let (permId, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertBool
              "the permanent is a legal object target"
              (Set.member (Recipient.ToObject permId) (Target.legalRecipients TargetSpec.SpellOrPermanentTarget gs)),
      HU.testCase "LandTarget offers a land as an object target, not a creature or player" $
        let gs = S.mountainsInPlay cards 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
         in do
              HU.assertBool "the land is legal" (Set.member (Recipient.ToObject landId) (Target.legalRecipients TargetSpec.LandTarget gs))
              HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients TargetSpec.LandTarget gs))),
      HU.testCase "CR 115: PlayerTarget is exactly the players still in the game" $
        let gs = Setup.emptyGame S.bothPlayers
            expected = Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]
         in HU.assertEqual "both players, no creatures" expected (Target.legalRecipients TargetSpec.PlayerTarget gs)
    ]

resolveTests :: Cards.Cards -> Tasty.TestTree
resolveTests cards =
  Tasty.testGroup
    "Resolve"
    [ HU.testCase "CR 608.3 / 704.5g a resolved Bolt kills a Piker" $
        let (_, cast, _) = S.boltAtBobsPiker cards
            after = Sba.checkStateBasedActions (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack after))
              HU.assertEqual "no creature survives" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Piker in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 608.2n the resolved Bolt is in its owner's graveyard" $
        let (_, cast, _) = S.boltAtBobsPiker cards
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertEqual "one card" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 120.3a a Bolt at a player drains life without marking" $
        -- No creature on the battlefield, so identityAnswer's lookupMin picks
        -- ToPlayer alice: a self-Bolt, which is legal Magic.
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertEqual "seventeen" (Just 17) (S.lifeOf S.alice after),
      HU.testCase "the resolved damage flows through the event funnel" $
        let (_, cast, _) = S.boltAtBobsPiker cards
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertEqual "one event of amount 3" [3] (map DamageEvent.amount (GameState.damageEvents after)),
      HU.testCase "resolving a Bolt conserves objects" $
        let (_, cast, _) = S.boltAtBobsPiker cards
         in HU.assertEqual "conserved" (Game.objectCount cast) (Game.objectCount (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))),
      HU.testCase "CR 608.2b a Bolt whose only target died fizzles" $
        let (base, cast, _) = S.boltAtBobsPiker cards
            -- Kill the Piker while the Bolt is on the stack, as Bolt B will in
            -- the integration test, then check state-based actions.
            dead = Sba.checkStateBasedActions (S.markDamage (S.pikerOf base) 3 cast)
            after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
         in do
              HU.assertEqual "Bolt in the graveyard, unresolved" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
              HU.assertEqual "no damage was dealt" [] (GameState.damageEvents after)
              HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 608.2b a fizzled spell applies none of its effects" $
        let (base, cast, _) = S.boltAtBobsPiker cards
            dead = Sba.checkStateBasedActions (S.markDamage (S.pikerOf base) 3 cast)
            after = snd (Engine.runGamePure S.identityAnswer dead Stack.resolveTop)
         in HU.assertEqual "life totals unchanged" (Just 20) (S.lifeOf S.alice after),
      -- The deterministic successor to the retired "instants happen" property: a
      -- Bolt cast in a game and resolved ends in its owner's graveyard.
      HU.testCase "a cast Bolt reaches its owner's graveyard" $
        let (_, cast, _) = S.boltAtBobsPiker cards
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertEqual "one card in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 612 slotsOf and textChangeSlots find a ChangeText slot" $
        let slot = SlotName.MkSlotName (Text.pack "target")
            card =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "T",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine = Card.Type.typeLine (Printing.card (Cards.lightningBoltPrinting cards)),
                  Card.Type.power = Nothing,
                  Card.Type.toughness = Nothing,
                  Card.Type.keywords = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.effects = [Effect.ChangeText slot],
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.castingPermissions = [],
                  Card.Type.targetSpecs = Map.empty
                }
         in do
              HU.assertEqual "slotsOf" (Set.singleton slot) (Resolve.slotsOf (Effect.ChangeText slot))
              HU.assertEqual "textChangeSlots" [slot] (Resolve.textChangeSlots card),
      HU.testCase "CR 605 manaProduced reads AddMana, nothing else" $ do
        HU.assertEqual "add mana" (Just (ManaType.Colored Color.Green)) (Resolve.manaProduced (Effect.AddMana (ManaType.Colored Color.Green)))
        HU.assertEqual "damage produces no mana" Nothing (Resolve.manaProduced (Effect.DealDamage (SlotName.MkSlotName (Text.pack "x")) (Quantity.Literal 1))),
      HU.testCase "CR 612 resolve reads projected effects: a hacked 'becomes Swamp' resolves as Mountain" $
        -- The target is a Forest, so the assertion {Mountain} proves the rewrite:
        -- un-rewritten the effect is SetLandSubtype Swamp -> {Swamp}; rewritten
        -- (Swamp -> Mountain) it is SetLandSubtype Mountain -> {Mountain}.
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            targetLand = case Game.zoneMembers Zone.Battlefield S.alice base of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            slot = SlotName.MkSlotName (Text.pack "target")
            (landformId, g1) = Game.freshObjectId base
            landformObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfCard (Cards.landformPrinting cards),
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToObject targetLand)) Map.empty Nothing,
                  Object.timestamp = Timestamp.MkTimestamp 0
                }
            g2 =
              g1
                { GameState.objects = Map.insert landformId landformObj (GameState.objects g1),
                  GameState.stack = landformId : GameState.stack g1
                }
            -- A resolved Magical Hack already changed Swamp -> Mountain on the
            -- Landform spell (stored on the Landform's id).
            hacked = withEffect landformId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Swamp Subtype.Mountain) g2
            after = snd (Engine.runGamePure S.identityAnswer hacked (Resolve.resolveSpell landformId))
         in do
              -- Landform's own subtype does not matter; its EFFECT was rewritten to
              -- SetLandSubtype Mountain, so the target land ends up a Mountain.
              HU.assertEqual "target land became Mountain, not Swamp" (Set.singleton Subtype.Mountain) (Projection.subtypesOf targetLand after),
      HU.testCase "CR 400.7 hacking Blood Moon on the stack is lost when it resolves" $
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature (Cards.urborgPrinting cards) S.alice base
            (bloodMoonSpellId, g2) = Game.freshObjectId g1
            bmObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfCard (Cards.bloodMoonPrinting cards),
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Map.empty,
                  Object.timestamp = Timestamp.MkTimestamp 0
                }
            g3 =
              g2
                { GameState.objects = Map.insert bloodMoonSpellId bmObj (GameState.objects g2),
                  GameState.stack = bloodMoonSpellId : GameState.stack g2
                }
            hacked = withEffect bloodMoonSpellId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g3
            after = snd (Engine.runGamePure S.identityAnswer hacked Stack.resolveTop)
         in -- Blood Moon entered as a NEW object; the hack (locked to the spell id)
            -- no longer names it, so nonbasic lands are Mountains, not Islands.
            HU.assertEqual "hack lost: nonbasic land is Mountain" (Set.singleton Subtype.Mountain) (Projection.subtypesOf nonbasicId after),
      HU.testCase "CR 608.2n a resolving ability deals its damage and ceases" $
        let (srcId, g0) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            ability = case Card.Type.activatedAbilities (Printing.card (Cards.prodigalSorcererPrinting cards)) of
              ab : _ -> ab
              [] -> ActivatedAbility.MkActivatedAbility (AbilityCost.MkAbilityCost Nothing []) [] Map.empty
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
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing,
                  Object.timestamp = ts
                }
            g3 =
              g2
                { GameState.objects = Map.insert abilId abilObj (GameState.objects g2),
                  GameState.stack = abilId : GameState.stack g2
                }
            resolved = snd (Engine.runGamePure S.identityAnswer g3 Stack.resolveTop)
         in do
              HU.assertEqual "bob took 1" (Just 19) (S.lifeOf S.bob resolved)
              HU.assertEqual "ability object gone" Nothing (Game.lookupObject abilId resolved)
              HU.assertEqual "stack empty" [] (GameState.stack resolved),
      HU.testCase "CR 701.23 Search fetches a basic land to the battlefield tapped" $
        -- The fetched card gets a NEW object id (CR 400.7 changeZone), so assert by
        -- count/tapped-count, never by the library incarnation's id.
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addLibraryCard (Cards.mountainPrinting cards) S.alice base
            ability =
              ActivatedAbility.MkActivatedAbility
                (AbilityCost.MkAbilityCost Nothing [])
                [Effect.Search CardCriterion.BasicLandCard]
                Map.empty
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj =
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled Map.empty ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
         in do
              HU.assertEqual "one permanent on the battlefield" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
              HU.assertEqual "it is tapped" 1 (S.tappedCount S.alice resolved)
              HU.assertEqual "library empty" [] (Game.zoneMembers Zone.Library S.alice resolved),
      HU.testCase "CR 701.23b Search may fail to find" $
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addLibraryCard (Cards.mountainPrinting cards) S.alice base
            ability = ActivatedAbility.MkActivatedAbility (AbilityCost.MkAbilityCost Nothing []) [Effect.Search CardCriterion.BasicLandCard] Map.empty
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj = Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled Map.empty ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findNothing g4 Stack.resolveTop)
         in HU.assertEqual "nothing entered the battlefield" Set.empty (GameState.battlefield resolved),
      HU.testCase "CR 603/608.2n Rest in Peace's ETB exiles graveyards and ceases" $
        let g0 = Setup.emptyGame S.bothPlayers
            (ripId, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice g0
            (deadId, g2) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob g1
            -- move the Piker into bob's graveyard
            g3 = Event.changeZone deadId Zone.Graveyard g2
            ability = TriggeredAbility.MkTriggeredAbility TriggerCondition.SelfEnters [Effect.ExileAllGraveyards] Map.empty
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
                  Object.bindings = Map.empty,
                  Object.timestamp = ts
                }
            g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
            resolved = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
         in do
              HU.assertEqual "bob's graveyard is empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
              HU.assertEqual "ability ceased" Nothing (Game.lookupObject abilId resolved),
      HU.testCase "CR 723.1: Mindslaver's ability installs pending control, promoted next turn" $
        let g0 = Setup.emptyGame S.bothPlayers
            (srcId, g1) = S.addCreature (Cards.mindslaverPrinting cards) S.alice g0
            slot = SlotName.MkSlotName (Text.pack "target")
            ability =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost =
                    AbilityCost.MkAbilityCost
                      { AbilityCost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                        AbilityCost.additional = [AdditionalCost.TapSelf, AdditionalCost.SacrificeSelf]
                      },
                  ActivatedAbility.effects = [Effect.ControlPlayerNextTurn slot],
                  ActivatedAbility.targetSpecs = Map.singleton slot TargetSpec.PlayerTarget
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
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing,
                  Object.timestamp = ts
                }
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = abilId : GameState.stack g3}
            resolved = snd (Engine.runGamePure S.identityAnswer g4 Stack.resolveTop)
            bobsTurn = snd (Engine.runGamePure S.identityAnswer resolved Engine.handoffTurn)
            afterBob = snd (Engine.runGamePure S.identityAnswer bobsTurn Engine.handoffTurn)
         in do
              HU.assertEqual "control pending for bob" (Just (Decider.MkDecider S.alice)) (Map.lookup S.bob (GameState.pendingControl resolved))
              HU.assertEqual "promoted on bob's turn" (Just (Decider.MkDecider S.alice)) (GameState.activeControl bobsTurn)
              HU.assertEqual "bob's decisions route to alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.bob bobsTurn)
              HU.assertEqual "control expired after bob's turn" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob afterBob)
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
twoBoltState :: Cards.Cards -> GameState.GameState
twoBoltState cards =
  let (_, withPiker) = S.addPiker cards S.bob (S.mountainsInPlay cards 2)
      (gs1, _oid1) = S.handOne (Cards.lightningBoltPrinting cards) withPiker
      (oid2, gs2) = Game.freshObjectId gs1
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.source = Source.OfCard (Cards.lightningBoltPrinting cards),
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.timestamp = Timestamp.MkTimestamp 0
          }
   in gs2
        { GameState.objects = Map.insert oid2 obj (GameState.objects gs2),
          -- handOne already put oid1 in hand; ADD the second Bolt, oid2.
          GameState.hand = Map.adjust (oid2 Seq.<|) S.alice (GameState.hand gs2)
        }

fizzleTests :: Cards.Cards -> Tasty.TestTree
fizzleTests cards =
  Tasty.testGroup
    "Fizzle"
    [ HU.testCase "CR 608.2b Bolt-vs-Bolt through the priority loop: the second fizzles" $
        let after = snd (Engine.runGamePure boltAnswer (twoBoltState cards) Engine.priorityLoop)
         in do
              HU.assertEqual "stack cleared" 0 (length (GameState.stack after))
              HU.assertEqual "Piker dead" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "both Bolts in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after))
              HU.assertEqual "the Piker in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "bob's life untouched: the fizzled Bolt hit nothing" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 704.5a a Bolt can end the game mid-step" $
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            lowBob =
              gs {GameState.players = Map.adjust (\pl -> pl {Player.life = 3}) S.bob (GameState.players gs)}
            atBob :: Prompt.Prompt r -> r
            atBob p = case p of
              Prompt.ChooseTargets _ _ _ sets ->
                Map.map (const (Recipient.ToPlayer S.bob)) sets
              Prompt.ChooseAction _ _ actions ->
                case filter (\a -> a == A.Cast oid) actions of
                  h : _ -> h
                  [] -> A.Pass
              _ -> S.identityAnswer p
            after = snd (Engine.runGamePure atBob lowBob Engine.priorityLoop)
         in do
              HU.assertEqual "alice wins" (Just (Result.Won S.alice)) (GameState.result after)
              HU.assertEqual "the loop released priority" Nothing (GameState.priority after)
    ]

indestructibleTests :: Cards.Cards -> Tasty.TestTree
indestructibleTests cards =
  Tasty.testGroup
    "Indestructible"
    [ HU.testCase "CR 704.5g an indestructible creature survives lethal marked damage" $
        let (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            -- Myr is 0/1; 3 marked damage is lethal (704.5g) but indestructible saves it.
            after = Sba.checkStateBasedActions (S.markDamage myrId 3 gs)
         in do
              HU.assertEqual "Myr still on the battlefield" 1 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Myr not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 704.5h an indestructible creature survives deathtouch" $
        let (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            -- Zero marked damage (so 704.5g is silent) plus a deathtouch event isolates
            -- the 704.5h path; indestructible must guard it too (CR 700.4).
            wounded = gs {GameState.damageEvents = [DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True]}
            after = Sba.checkStateBasedActions wounded
         in HU.assertEqual "Myr survives deathtouch" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 704.5f indestructible does NOT save a creature with toughness <= 0" $
        let (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            -- A test-local -0/-1 drops Myr (0/1) to 0/0; 704.5f is a put-into-graveyard,
            -- not a destroy, so indestructible does not apply (Myr's own reminder text).
            zeroed = withEffect myrId (Timestamp.MkTimestamp 5) (Modification.ModifyPowerToughness (Quantity.Literal 0) (Quantity.Literal (-1))) gs
            after = Sba.checkStateBasedActions zeroed
         in do
              HU.assertEqual "Myr left the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Myr in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
    ]

-- alice controls `n` Swamps and holds `printing` in a main phase with priority;
-- bob controls one `foe`. Returns (foe's id, post-cast-and-resolve state).
castBlackRemovalAt :: Cards.Cards -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
castBlackRemovalAt cards printing foe =
  let base = S.landsInPlay (Cards.swampPrinting cards) 3
      (foeId, withFoe) = S.addCreature foe S.bob base
      (gs, spellId) = S.handOne printing withFoe
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (foeId, resolved)

zoneChangeTests :: Cards.Cards -> Tasty.TestTree
zoneChangeTests cards =
  Tasty.testGroup
    "ZoneChange"
    [ HU.testCase "CR 701.7 Murder destroys a normal creature into its owner's graveyard" $
        let (_, after) = castBlackRemovalAt cards (Cards.murderPrinting cards) (Cards.pikerPrinting cards)
         in do
              HU.assertEqual "no creature survives" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Piker in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 700.4 Murder does nothing to an indestructible creature (destroy /= move)" $
        let (_, after) = castBlackRemovalAt cards (Cards.murderPrinting cards) (Cards.darksteelMyrPrinting cards)
         in do
              -- The falsifier: modelling Destroy as MoveToZone slot Graveyard would
              -- bury the Myr. It stays; the spell still resolved and was buried.
              HU.assertEqual "Myr still on the battlefield" 1 (S.creaturesInPlay S.bob after)
              HU.assertEqual "bob's graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "Murder in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 400.7 Unsummon returns a creature to its owner's hand" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, withPiker) = S.addPiker cards S.bob base
            (gs, spellId) = S.handOne (Cards.unsummonPrinting cards) withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "no creature on the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "a card in bob's hand (its owner)" 1 (S.handSize S.bob after)
              HU.assertEqual "Unsummon in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Resolve" [targetTests cards, resolveTests cards, fizzleTests cards, indestructibleTests cards, zoneChangeTests cards]
