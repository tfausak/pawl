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
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Damage as Damage
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
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Power as Power
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
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TypeLine as TypeLine
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
      HU.testCase "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $
        let (permId, base) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            (spellId, gs) = S.spellOnStack (Cards.lightningBoltPrinting cards) S.alice base
            legal = Target.legalRecipients TargetSpec.SpellTarget gs
         in do
              HU.assertBool "the stack spell is a legal target" (Set.member (Recipient.ToObject spellId) legal)
              HU.assertBool "the battlefield permanent is not a legal target" (not (Set.member (Recipient.ToObject permId) legal)),
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
         in HU.assertEqual "both players, no creatures" expected (Target.legalRecipients TargetSpec.PlayerTarget gs),
      -- CR 115.4 / 700.2c: "target Wall" (Chaos Charm) restricts CreatureTarget to
      -- creatures whose PROJECTED subtypes include Wall. Wall of Stone (Task 5) is
      -- not landed yet, so this is a hand-built minimal Wall Card/Printing --
      -- 0/8, Creature - Wall, an empty mode -- just enough to sit on the
      -- battlefield and carry the subtype.
      HU.testCase "CR 115.4 / 700.2c WallTarget offers a Wall creature but not a non-Wall creature" $
        let wallCard =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "Test Wall",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine =
                    TypeLine.MkTypeLine
                      { TypeLine.supertypes = Set.empty,
                        TypeLine.types = Set.singleton CardType.Creature,
                        TypeLine.subtypes = Set.singleton Subtype.Wall
                      },
                  Card.Type.power = Just (Power.MkPower (Quantity.Literal 0)),
                  Card.Type.toughness = Just (Toughness.MkToughness (Quantity.Literal 8)),
                  Card.Type.keywords = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.spell =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode Seq.empty Map.empty))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.castingPermissions = []
                }
            wallPrinting = Printing.MkPrinting wallCard
            (wallId, base) = S.addCreature wallPrinting S.bob (Setup.emptyGame S.bothPlayers)
            (pikerId, gs) = S.addPiker cards S.alice base
            slot = SlotName.MkSlotName (Text.pack "target")
            legal = Map.findWithDefault Set.empty slot (Target.legalSets (Map.singleton slot TargetSpec.WallTarget) gs)
         in do
              HU.assertBool "the Wall is legal" (Set.member (Recipient.ToCreature wallId) legal)
              HU.assertBool "the non-Wall creature is not legal" (not (Set.member (Recipient.ToCreature pikerId) legal))
    ]

resolveTests :: Cards.Cards -> Tasty.TestTree
resolveTests cards =
  Tasty.testGroup
    "Resolve"
    [ HU.testCase "CR 608 a resolved spell's damage is Noncombat" $
        let base = S.landsInPlay (Cards.mountainPrinting cards) 1
            (_target, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs1, spellId) = S.handOne (Cards.lightningBoltPrinting cards) gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice spellId))
            -- resolveTop applies the damage but does NOT run SBAs, so the event persists.
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertEqual
              "the Bolt's damage event is Noncombat"
              [DamageKind.Noncombat]
              (map DamageEvent.kind (GameState.damageEvents resolved)),
      HU.testCase "CR 608.3 / 704.5g a resolved Bolt kills a Piker" $
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
                  Card.Type.spell =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.singleton (Effect.ChangeText slot)) Map.empty))
                      (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.castingPermissions = []
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
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToObject targetLand)) Map.empty Nothing Set.empty,
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
                  Object.counters = Map.empty,
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
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing Set.empty,
                  Object.counters = Map.empty,
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
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled Map.empty Map.empty ts
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
            abilObj = Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled Map.empty Map.empty ts
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
                  Object.counters = Map.empty,
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
                  Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.bob)) Map.empty Nothing Set.empty,
                  Object.counters = Map.empty,
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
              HU.assertEqual "control expired after bob's turn" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob afterBob),
      HU.testCase "CR 111 Dragon Fodder creates two 1/1 Goblin tokens" $
        let base = S.landsInPlay (Cards.mountainPrinting cards) 2
            (gs, spellId) = S.handOne (Cards.dragonFodderPrinting cards) base
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              -- Two Goblin tokens exist (count == 2 proves two distinct objects). The
              -- battlefield also holds alice's 2 Mountains, so filter by name/creature.
              HU.assertEqual "two Goblin tokens on the battlefield" 2 (S.countOnBattlefieldByName (Text.pack "Goblin") S.alice after)
              HU.assertEqual "alice controls two creatures (the tokens)" 2 (S.creaturesInPlay S.alice after)
              HU.assertEqual "Dragon Fodder went to the graveyard (CR 608.2n)" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 615 Fog prevents combat damage but not spell damage (the gate)" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victim, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs1, fogId) = S.handOne (Cards.fogPrinting cards) gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice fogId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            combat = Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Combat] resolved
            spell = Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Noncombat] resolved
         in do
              HU.assertEqual "Fog installed one prevention" 1 (length (GameState.preventions resolved))
              HU.assertEqual "combat damage prevented (the cancel shape)" (Just 0) (S.damageOf victim combat)
              -- The falsifier: a tag-blind Fog would also blunt this spell damage.
              HU.assertEqual "spell damage untouched (Noncombat)" (Just 2) (S.damageOf victim spell)
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
cancelVictim :: Cards.Cards -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
cancelVictim cards victim =
  let base = S.landsInPlay (Cards.islandPrinting cards) 3
      (victimId, onStack) = S.spellOnStack victim S.bob base
      (gs, cancelId) = S.handOne (Cards.cancelPrinting cards) onStack
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
racingCounters :: Cards.Cards -> GameState.GameState
racingCounters cards =
  let base = S.landsInPlay (Cards.islandPrinting cards) 6
      (victimId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob base
      (gs1, cancelA) = S.handOne (Cards.cancelPrinting cards) onStack
      (cancelB, gs2) = handAppend (Cards.cancelPrinting cards) S.alice gs1
      atVictim :: Prompt.Prompt r -> r
      atVictim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToObject victimId)) sets
        _ -> S.identityAnswer p
      castA = snd (Engine.runGamePure atVictim gs2 (Cast.castSpell S.alice cancelA))
      castB = snd (Engine.runGamePure atVictim castA (Cast.castSpell S.alice cancelB))
      r1 = snd (Engine.runGamePure atVictim castB Stack.resolveTop) -- B counters the Piker
      r2 = snd (Engine.runGamePure atVictim r1 Stack.resolveTop) -- A fizzles
   in r2

counterTests :: Cards.Cards -> Tasty.TestTree
counterTests cards =
  Tasty.testGroup
    "Counter"
    [ HU.testCase "CR 701.6 Cancel counters a spell into its owner's graveyard" $
        let (_victimId, resolved) = cancelVictim cards (Cards.pikerPrinting cards)
         in do
              HU.assertEqual "victim countered into bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
              HU.assertEqual "victim never resolved onto the battlefield" 0 (S.creaturesInPlay S.bob resolved)
              HU.assertEqual "Cancel in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
              HU.assertEqual "stack empty" 0 (length (GameState.stack resolved)),
      HU.testCase "CR 608.2b a Cancel whose target already left the stack fizzles" $
        let after = racingCounters cards
         in do
              HU.assertEqual "the Piker moved exactly once, to bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "both Cancels in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after))
              HU.assertEqual "the Piker never hit the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "stack cleared" 0 (length (GameState.stack after)),
      HU.testCase "CR 614 Cancel under Rest in Peace exiles the countered spell" $
        let (_, ripOut) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (S.landsInPlay (Cards.islandPrinting cards) 3)
            (_victimId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob ripOut
            (gs, cancelId) = S.handOne (Cards.cancelPrinting cards) onStack
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cancelId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "the countered spell is not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
              HU.assertEqual "the countered spell is exiled" 1 (length (Game.zoneMembers Zone.Exile S.bob resolved))
    ]

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
            wounded = gs {GameState.damageEvents = [DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True DamageKind.Combat]}
            after = Sba.checkStateBasedActions wounded
         in HU.assertEqual "Myr survives deathtouch" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 704.5f indestructible does NOT save a creature with toughness <= 0" $
        let (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            -- A real -1/-1 counter drops Myr (0/1) to 0/0 (CR 122.1a); 704.5f is a
            -- put-into-graveyard, not a destroy, so indestructible does not apply
            -- (Myr's own reminder text).
            zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 myrId gs
            after = Sba.checkStateBasedActions zeroed
         in do
              HU.assertEqual "Myr left the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "Myr in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 704.5f regeneration does NOT save a creature with toughness <= 0" $
        let (victim, gs) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers) -- 2/1
        -- A real -1/-1 counter drops the toughness to 0 (CR 122.1a); 704.5f is a
        -- put-into-graveyard, not a destruction, so a regeneration shield cannot
        -- save it.
            zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs
            shielded = S.addRegenShield victim zeroed
            after = Sba.checkStateBasedActions shielded
         in HU.assertEqual "died despite the shield (704.5f is not a destruction)" 0 (S.creaturesInPlay S.bob after)
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

-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToPlayer S.bob)) sets
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
      HU.testCase "CR 701.19a Murder is replaced by regeneration" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 3
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            shielded = S.addRegenShield victim withFoe
            (gs, spellId) = S.handOne (Cards.murderPrinting cards) shielded
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = Sba.checkStateBasedActions (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
         in HU.assertEqual "the shielded creature survived Murder" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 400.7 Unsummon returns a creature to its owner's hand" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, withPiker) = S.addPiker cards S.bob base
            (gs, spellId) = S.handOne (Cards.unsummonPrinting cards) withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "no creature on the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "a card in bob's hand (its owner)" 1 (S.handSize S.bob after)
              HU.assertEqual "Unsummon in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 701.19a regeneration does not save a bounced creature" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            shielded = S.addRegenShield victim withFoe
            (gs, spellId) = S.handOne (Cards.unsummonPrinting cards) shielded
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "the creature left the battlefield (bounce is not a destruction)" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "it is in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob after)),
      HU.testCase "CR 701.10 Angelic Edict exiles a target creature" $
        let base = S.landsInPlay (Cards.plainsPrinting cards) 5
            (_, withPiker) = S.addPiker cards S.bob base
            (gs, spellId) = S.handOne (Cards.angelicEdictPrinting cards) withPiker
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "no creature on the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "one card in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 115 Angelic Edict may exile an enchantment (non-creature permanent)" $
        let base = S.landsInPlay (Cards.plainsPrinting cards) 5
            -- bob controls only Rest in Peace (an enchantment, not a creature), so
            -- it is the single legal CreatureOrEnchantmentTarget.
            (ripId, withRip) = S.addCreature (Cards.restInPeacePrinting cards) S.bob base
            (gs, spellId) = S.handOne (Cards.angelicEdictPrinting cards) withRip
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "the enchantment left the battlefield" Nothing (Game.lookupObject ripId after)
              HU.assertEqual "one card in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 120 Divination draws its controller two cards" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 3
            (_, g1) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            (_, g2) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice g1
            (gs, spellId) = S.handOne (Cards.divinationPrinting cards) g2
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "two cards drawn to hand" 2 (S.handSize S.alice after)
              HU.assertEqual "library emptied" [] (Game.zoneMembers Zone.Library S.alice after),
      HU.testCase "CR 121.3 a Draw that outruns the library records the loss" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 3
            (_, g1) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            (gs, spellId) = S.handOne (Cards.divinationPrinting cards) g1
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertBool "drewFromEmpty marked" (Set.member S.alice (GameState.drewFromEmpty after)),
      HU.testCase "CR 701.13 Tome Scour mills five from a target player's library" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            withLib = List.foldl' (\g _ -> snd (S.addLibraryCard (Cards.pikerPrinting cards) S.bob g)) base [1 .. (6 :: Int)]
            (gs, spellId) = S.handOne (Cards.tomeScourPrinting cards) withLib
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "five milled to graveyard" 5 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "one card left in library" 1 (length (Game.zoneMembers Zone.Library S.bob after)),
      HU.testCase "CR 701.13b milling a short library mills fewer with no loss" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob base
            (_, g2) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob g1
            (gs, spellId) = S.handOne (Cards.tomeScourPrinting cards) g2
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = Sba.checkStateBasedActions (snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop))
         in do
              HU.assertEqual "two milled" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertBool "bob did not lose (milling is not drawing)" (not (Set.member S.bob (GameState.drewFromEmpty after))),
      HU.testCase "CR 701.8 Mind Rot discards two chosen cards from a hand of three" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 3
            withHand = handCards (Cards.pikerPrinting cards) S.bob 3 base
            (gs, spellId) = S.handOne (Cards.mindRotPrinting cards) withHand
            cast = snd (Engine.runGamePure atBobAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "one card left in bob's hand" 1 (S.handSize S.bob after)
              HU.assertEqual "two cards in bob's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 701.8b a forced full-hand discard is not prompted" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 3
            withHand = handCards (Cards.pikerPrinting cards) S.bob 2 base
            (gs, spellId) = S.handOne (Cards.mindRotPrinting cards) withHand
            -- Answer ChooseDiscard with [] so a prompt would discard nothing;
            -- aim the spell at bob.
            noDiscard q = case q of
              Prompt.ChooseDiscard {} -> []
              Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToPlayer S.bob)) sets
              _ -> S.identityAnswer q
            cast = snd (Engine.runGamePure noDiscard gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure noDiscard cast Stack.resolveTop)
         in do
              -- Elision (hand == count): the whole hand is discarded without asking.
              HU.assertEqual "bob's hand emptied" 0 (S.handSize S.bob after)
              HU.assertEqual "both cards discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.bob after))
    ]

drawCardTests :: Cards.Cards -> Tasty.TestTree
drawCardTests cards =
  Tasty.testGroup
    "DrawCard"
    [ HU.testCase "CR 121.2 drawCard moves the top library card to hand" $
        let base = Setup.emptyGame S.bothPlayers
            (_, withCard) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            after = Event.drawCard S.alice withCard
         in do
              HU.assertEqual "one card in hand" 1 (S.handSize S.alice after)
              HU.assertEqual "library empty" [] (Game.zoneMembers Zone.Library S.alice after),
      HU.testCase "CR 121.3 drawing from an empty library records the failed draw" $
        let after = Event.drawCard S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertBool "drewFromEmpty marked" (Set.member S.alice (GameState.drewFromEmpty after))
    ]

countersTests :: Cards.Cards -> Tasty.TestTree
countersTests cards =
  Tasty.testGroup
    "Counters"
    [ HU.testCase "CR 122.6 Battlegrowth puts a +1/+1 counter (gate)" $
        -- alice casts Battlegrowth on bob's Piker (2/1). After resolution the Piker
        -- is 3/2 and carries one +1/+1 counter.
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs, spellId) = S.handOne (Cards.battlegrowthPrinting cards) withFoe
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "power 3" (Just 3) (Projection.powerOf victim after)
              HU.assertEqual "toughness 2" (Just 2) (Projection.toughnessOf victim after),
      HU.testCase "CR 122 counter persists through cleanup (vs Giant Growth wearing off)" $
        -- After a cleanup step, the +1/+1 counter is still on the Piker.
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs, spellId) = S.handOne (Cards.battlegrowthPrinting cards) withFoe
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            afterCleanup = Projection.dropEndOfTurnEffects resolved
         in do
              HU.assertEqual "still 3/2 after cleanup" (Just 3) (Projection.powerOf victim afterCleanup)
              HU.assertEqual "still 3/2 after cleanup" (Just 2) (Projection.toughnessOf victim afterCleanup),
      HU.testCase "CR 122.6 Instill Infection puts a -1/-1 counter and draws" $
        -- alice casts Instill Infection on bob's Piker; Piker becomes 1/0 and dies
        -- (704.5f); alice draws a card.
        let base = S.landsInPlay (Cards.swampPrinting cards) 4
            (_, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            -- Baseline before Instill Infection itself enters alice's hand: casting
            -- moves that same card from hand to the stack, so measuring after it is
            -- already there would net the draw against the spell's own departure.
            handBefore = S.handSize S.alice withFoe
            (gs0, spellId) = S.handOne (Cards.instillInfectionPrinting cards) withFoe
            -- put a card in alice's library so the draw has something to find.
            (_, gs) = S.addLibraryCard (Cards.forestPrinting cards) S.alice gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = Sba.checkStateBasedActions (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
         in do
              HU.assertEqual "Piker died to the -1/-1 counter (704.5f)" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "alice drew a card" (handBefore + 1) (S.handSize S.alice after),
      HU.testCase "CR 704.5q both counter kinds on one creature annihilate; net 2/1 survives" $
        -- Both counters on the same creature (placed directly); the SBA removes both.
        let base = S.landsInPlay (Cards.forestPrinting cards) 5
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs1
            after = Sba.checkStateBasedActions gs2
         in do
              HU.assertEqual "creature survives (net 2/1)" 1 (S.creaturesInPlay S.alice after)
              HU.assertEqual "no counters remain" Map.empty (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject victim after)),
      HU.testCase "CR 122.2 Unsummon removes a counter-bearing creature's counters" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            withCounter = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
            (gs, spellId) = S.handOne (Cards.unsummonPrinting cards) withCounter
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            -- Total (no `head`): expect exactly one bounced card in hand, empty counters.
            handCounters = map (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h after)) (Game.zoneMembers Zone.Hand S.bob after)
         in HU.assertEqual "the bounced incarnation in hand has no counters" [Map.empty] handCounters
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Resolve" [targetTests cards, resolveTests cards, fizzleTests cards, indestructibleTests cards, zoneChangeTests cards, drawCardTests cards, counterTests cards, countersTests cards]
