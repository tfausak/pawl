{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Combat: attack/block legality, combat damage, and the combat
-- keywords (flying, reach, defender, vigilance, haste, first/double strike).
module Pawl.CombatSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Combat as Combat
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.Expiry as Expiry
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

combatDamageTests :: Cards.Cards -> Tasty.TestTree
combatDamageTests cards =
  Tasty.testGroup
    "CombatDamage"
    [ HU.testCase "CR 510.1b an unblocked attacker damages the defending player" $
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 1 0
            after = S.fightWith S.aggressiveAnswer gs
         in -- A Piker is a 2/1, and bob starts at 20.
            HU.assertEqual "bob took 2" (Just 18) (S.lifeOf S.bob after),
      HU.testCase "CR 509 a blocked attacker does not damage the player" $
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            after = S.fightWith S.aggressiveAnswer gs
         in HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after),
      HU.testCase "CR 510.1c a single blocker takes all the damage, unprompted" $
        -- If the engine wrongly prompts here, this interpreter answers with an
        -- empty division, which is illegal (it does not total the attacker's
        -- power), so it is rejected and the blocker takes 0 -- and the assertion
        -- below fails. That is why this proves "unprompted" without an `error`,
        -- which the no-partial-functions rule forbids anyway.
        let (gs, _, theirs) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            noAssign :: Prompt.Prompt r -> r
            noAssign p = case p of
              Prompt.AssignCombatDamage {} -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.fightWith noAssign gs
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              b : _ -> HU.assertEqual "took 2" (Just 2) (S.damageOf b after),
      HU.testCase "CR 510.2 a 2/1 trade kills BOTH creatures" $
        -- The simultaneity test. Sequential damage kills only one, because the
        -- blocker would be in the graveyard before it dealt its damage.
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
         in do
              HU.assertEqual "alice's is dead" 0 (S.creaturesInPlay S.alice after)
              HU.assertEqual "bob's is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 510.1c a free division of 2 across two blockers kills both" $
        let (gs, _, theirs) = S.combatBoard (Cards.pikerPrinting cards) 1 2
            split :: Prompt.Prompt r -> r
            split p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 1)) (filter S.isCreatureRecipient (Map.keys thresholds)))
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith split gs)
         in do
              HU.assertEqual "both blockers dead" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "expected two blockers" 2 (length theirs),
      HU.testCase "CR 510.1c the same 2 damage on one blocker kills only it" $
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 1 2
            dump :: Prompt.Prompt r -> r
            dump p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter S.isCreatureRecipient (Map.keys thresholds) of
                  r : _ -> Map.singleton r n
                  [] -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith dump gs)
         in HU.assertEqual "one blocker survives" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 510.1e an illegal division is rejected and deals nothing" $
        -- Not a reachable game state: this is the engine's defense against a
        -- broken interpreter. See the spec, section 3.
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 1 2
            cheat :: Prompt.Prompt r -> r
            cheat p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 99)) (filter S.isCreatureRecipient (Map.keys thresholds)))
              _ -> S.aggressiveAnswer p
            after = S.settleSba (S.fightWith cheat gs)
         in HU.assertEqual "both blockers survive" 2 (S.creaturesInPlay S.bob after),
      -- The deterministic successor to the retired "combat happens" property: an
      -- unblocked 2/1 attacker reduces the defender's life by its power.
      HU.testCase "combat deals damage to the defending player" $
        let (gs, _, _) = S.combatBoardOf [Cards.pikerPrinting cards] []
            after = S.runCombat S.aggressiveAnswer gs
         in HU.assertEqual "defender took two" (Just 18) (S.lifeOf S.bob after)
    ]

declaredAttackers :: GameState.GameState -> [ObjectId.ObjectId]
declaredAttackers gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

declareTests :: Cards.Cards -> Tasty.TestTree
declareTests cards =
  Tasty.testGroup
    "Declare"
    [ HU.testCase "CR 508.1f declaring an attacker taps it" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
         in do
              HU.assertEqual "one attacker" mine (declaredAttackers after)
              HU.assertEqual "tapped" 1 (S.tappedCount S.alice after),
      HU.testCase "CR 508.1 attackers attack the defending player" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ ->
                HU.assertEqual
                  "attacking bob"
                  (Just (AttackTarget.OfPlayer S.bob))
                  (Map.lookup oid (Combat.Type.attackers (GameState.combat after))),
      HU.testCase "an illegal attacker in the answer is dropped" $
        -- The interpreter names bob's creature. It is not alice's to attack with.
        let (gs, _, theirs) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            liar :: Prompt.Prompt r -> r
            liar p = case p of
              Prompt.DeclareAttackers {} -> theirs
              _ -> S.aggressiveAnswer p
            after = snd (Engine.runGamePure liar gs (Combat.declareAttackers S.alice))
         in HU.assertEqual "nothing attacks" [] (declaredAttackers after),
      HU.testCase "CR 509.1 a blocker is recorded against the attacker it blocks" $
        let (gs, mine, theirs) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              attacker : _ ->
                HU.assertEqual "blocked by bob's creature" (Set.fromList theirs) (Combat.blockersOf attacker after),
      HU.testCase "an unblocked attacker has no blockers" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 1 0
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              attacker : _ -> HU.assertBool "unblocked" (not (Combat.isBlocked attacker after)),
      HU.testCase "no legal attackers means no prompt and no attacks" $
        -- combatBoard 0 1 gives alice nothing. A prompt here would be the engine
        -- asking a question with exactly one answer.
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 0 1
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
         in HU.assertEqual "nothing attacks" [] (declaredAttackers after),
      -- The end-to-end summoning sickness scenario the spec names: a creature
      -- that just arrived cannot attack, and the SAME creature can once its
      -- controller's untap step has settled it. The halves are tested in Tasks 1
      -- and 4; this proves they compose.
      HU.testCase "CR 302.6 a creature cannot attack the turn it arrives, and can after untapping" $
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 1 1
            arrived = justArrived gs
            sameTurn = snd (Engine.runGamePure S.aggressiveAnswer arrived (Combat.declareAttackers S.alice))
            nextTurn =
              snd
                . Engine.runGamePure S.aggressiveAnswer arrived
                $ do
                  Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap)
                  Combat.declareAttackers S.alice
         in do
              HU.assertEqual "cannot attack the turn it arrives" [] (declaredAttackers sameTurn)
              HU.assertEqual "can attack after untapping" 1 (length (declaredAttackers nextTurn))
    ]

defenderTests :: Cards.Cards -> Tasty.TestTree
defenderTests cards =
  Tasty.testGroup
    "Defender"
    [ HU.testCase "CR 702.3b a creature with defender can't attack" $
        let (gs, mine, _) = S.combatBoardOf [Cards.ogreSentryPrinting cards] [Cards.pikerPrinting cards]
         in case mine of
              [] -> HU.assertFailure "fixture should have one creature"
              oid : _ -> HU.assertBool "can't attack" (not (Combat.canAttack S.alice oid gs)),
      HU.testCase "CR 702.3b a creature with defender is not offered as a legal attacker" $
        let (gs, _, _) = S.combatBoardOf [Cards.ogreSentryPrinting cards] [Cards.pikerPrinting cards]
         in HU.assertEqual "none" [] (Combat.legalAttackers S.alice gs),
      HU.testCase "CR 702.3b defender does not stop it blocking" $
        -- 702.3b says "can't attack" and nothing else. A defender that could not
        -- block would be a Wall in the pre-2004 sense, and that is not the rule.
        let (gs, _, theirs) = S.combatBoardOf [Cards.pikerPrinting cards] [Cards.ogreSentryPrinting cards]
         in case theirs of
              [] -> HU.assertFailure "fixture should have one blocker"
              oid : _ -> HU.assertBool "may block" (Combat.canBlock S.bob oid gs),
      HU.testCase "a creature without defender is still offered" $
        -- The control. If defender were implemented as "nothing may attack", the
        -- test above would pass and this one would fail.
        let (gs, mine, _) = S.combatBoardOf [Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
         in HU.assertEqual "one" mine (Combat.legalAttackers S.alice gs),
      HU.testCase "CR 702.3b a defender is skipped but its neighbor still attacks" $
        let (gs, mine, _) = S.combatBoardOf [Cards.ogreSentryPrinting cards, Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
         in case mine of
              [_, piker] -> HU.assertEqual "only the piker" [piker] (Combat.legalAttackers S.alice gs)
              _ -> HU.assertFailure "fixture should have two creatures"
    ]

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Re-sicken alice's creatures, as though they had just resolved this turn.
justArrived :: GameState.GameState -> GameState.GameState
justArrived gs =
  let sicken o = if Object.owner o == S.alice then o {Object.sickness = Sickness.Sick} else o
   in gs {GameState.objects = fmap sicken (GameState.objects gs)}

hasteTests :: Cards.Cards -> Tasty.TestTree
hasteTests cards =
  Tasty.testGroup
    "Haste"
    [ HU.testCase "CR 702.10b a creature with haste attacks the turn it arrives" $
        let (gs, _, _) = S.combatBoardOf [Cards.goblinChariotPrinting cards] [Cards.pikerPrinting cards]
            after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
         in HU.assertEqual "attacks" 1 (length (declaredAttackers after)),
      HU.testCase "CR 302.6 the same creature without haste cannot" $
        -- The control. Goblin Chariot and Goblin Piker are both 2/2-ish Goblin
        -- Warriors; the ONLY difference the engine can see is the keyword.
        let (gs, _, _) = S.combatBoardOf [Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
            after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
         in HU.assertEqual "cannot attack" [] (declaredAttackers after),
      HU.testCase "CR 702.10b haste is not needed once the creature has settled" $
        let (gs, mine, _) = S.combatBoardOf [Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
         in HU.assertEqual "attacks" mine (declaredAttackers after),
      HU.testCase "CR 702.10b a hasty creature and a sick one, in the same declaration" $
        -- Both sick; only the Chariot may attack. A blanket "sickness ignored"
        -- bug would let both through.
        let (gs, mine, _) = S.combatBoardOf [Cards.goblinChariotPrinting cards, Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
            after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
         in case mine of
              [chariot, _] -> HU.assertEqual "only the chariot" [chariot] (declaredAttackers after)
              _ -> HU.assertFailure "fixture should have two creatures"
    ]

controlChangeSicknessTests :: Cards.Cards -> Tasty.TestTree
controlChangeSicknessTests cards =
  Tasty.testGroup
    "ControlChangeSickness"
    [ -- SYNTHETIC (labeled crutch, spec §4): a "steal until end of turn, no haste"
      -- effect. A real card would grant haste (masking CR 302.6) or be an Aura
      -- (Attach, out of M4.5 scope). Retired by the Auras / Control Magic phase (#33).
      HU.testCase "CR 302.6 a creature that just changed control is summoning sick (no haste)" $
        let (oid, base) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            slot = SlotName.MkSlotName (Text.pack "target")
            steal =
              Resolve.applyEffect
                oid
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.GainControl Duration.UntilEndOfTurn slot)
            after = snd (Engine.runGamePure S.identityAnswer base steal)
         in do
              HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf oid after)
              HU.assertBool "but it is summoning sick, so it cannot attack this turn" (not (Combat.canAttack S.alice oid after))
    ]

-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
      after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
   in (after, ours, yours)

-- CR 702.36: grant fear to `oid` with a stored continuous effect. No card in the
-- pool has PRINTED fear (Aphotic Wisps grants it at instant speed, which combat
-- fixtures cannot reach mid-step), so this is the M2c granted-keyword posture.
withFear :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withFear oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = oid,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Fear,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

evasionTests :: Cards.Cards -> Tasty.TestTree
evasionTests cards =
  Tasty.testGroup
    "Evasion"
    [ HU.testCase "CR 702.9b a declaration in which a ground creature blocks a flier is illegal" $
        let (gs, mine, theirs) = attacking [Cards.birdMaidenPrinting cards] [Cards.pikerPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.17b a reach creature may block a flier" $
        -- THE FALSIFIER. Fails against any implementation that asks "does the
        -- blocker have flying?"
        let (gs, mine, theirs) = attacking [Cards.birdMaidenPrinting cards] [Cards.nimbleBirdstickerPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a ground creature" $
        -- The asymmetry: 702.9b's second sentence. Fails if flying is implemented
        -- as a symmetric predicate.
        let (gs, mine, theirs) = attacking [Cards.pikerPrinting cards] [Cards.birdMaidenPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a flier" $
        let (gs, mine, theirs) = attacking [Cards.birdMaidenPrinting cards] [Cards.birdMaidenPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 509.1a a ground creature is still a legal blocker while a flier attacks" $
        -- 509.1a is about the blocker ALONE: it can block SOMETHING. This test
        -- fails if evasion is wrongly implemented as a filter on the candidates.
        let (gs, _, theirs) = attacking [Cards.birdMaidenPrinting cards] [Cards.pikerPrinting cards]
         in HU.assertEqual "still offered" theirs (Combat.legalBlockers S.bob gs),
      HU.testCase "CR 509.1b an illegal declaration is rejected WHOLE, not repaired" $
        -- aggressiveAnswer blocks the first attacker with EVERYTHING, so bob
        -- declares the reach creature (legal) AND the Piker (illegal) on the
        -- flier. Neither may block. A per-pair filter would drop the Piker and
        -- let the Birdsticker's block stand -- which is what M1b does today, and
        -- is unsound: under menace, dropping one blocker from a pair manufactures
        -- an illegal single block.
        let (gs, _, _) = S.combatBoardOf [Cards.birdMaidenPrinting cards] [Cards.nimbleBirdstickerPrinting cards, Cards.pikerPrinting cards]
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
         in case Map.keys (Combat.Type.attackers (GameState.combat after)) of
              [] -> HU.assertFailure "fixture should have an attacker"
              a : _ -> HU.assertEqual "nobody blocks" Set.empty (Combat.blockersOf a after),
      HU.testCase "CR 509.1b a wholly legal declaration is accepted" $
        -- The control for the test above: with only the reach creature, the same
        -- interpreter produces a legal declaration and the block stands.
        let (gs, _, theirs) = S.combatBoardOf [Cards.birdMaidenPrinting cards] [Cards.nimbleBirdstickerPrinting cards]
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
         in case Map.keys (Combat.Type.attackers (GameState.combat after)) of
              [] -> HU.assertFailure "fixture should have an attacker"
              a : _ -> HU.assertEqual "the reach creature blocks" (Set.fromList theirs) (Combat.blockersOf a after),
      HU.testCase "CR 509.1a a Mountain is not a legal blocker, flier or no flier" $
        -- The classification, from the other side: `canBlock` asks
        -- is-it-a-creature, never which card it is. M1b (tests cards) "a land may not
        -- attack" but never that a land may not BLOCK, so this closes a real gap
        -- rather than restating one.
        let (gs, mine, _) = attacking [Cards.birdMaidenPrinting cards] []
            withLand = snd (S.addCreature (Cards.mountainPrinting cards) S.bob gs)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              _ : _ -> HU.assertEqual "no legal blockers" [] (Combat.legalBlockers S.bob withLand),
      HU.testCase "CR 702.9b a flier connects past an untapped ground creature, in a real combat" $
        -- The integration case, and it is precise rather than vacuous. WITH
        -- flying: nothing may block, bob takes 1, and both creatures live.
        -- WITHOUT flying: the Piker blocks, bob takes 0, and the two TRADE (Bird
        -- Maiden is 1/2, Piker is 2/1). All three assertions distinguish them.
        let (gs, _, _) = S.combatBoardOf [Cards.birdMaidenPrinting cards] [Cards.pikerPrinting cards]
            after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
         in do
              HU.assertEqual "bob took 1" (Just 19) (S.lifeOf S.bob after)
              HU.assertEqual "the flier lives" 1 (S.creaturesInPlay S.alice after)
              HU.assertEqual "the would-be blocker lives" 1 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.36b a red creature may not block a creature with fear" $
        let (gs0, mine, theirs) = attacking [Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0)))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b a black creature may block a creature with fear" $
        let (gs0, mine, theirs) = attacking [Cards.pikerPrinting cards] [Cards.typhoidRatsPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b an ARTIFACT creature may block a creature with fear" $
        -- THE FALSIFIER for reading 702.36b as a colour test alone: Darksteel Myr
        -- is a colourless artifact creature and blocks legally.
        let (gs0, mine, theirs) = attacking [Cards.pikerPrinting cards] [Cards.darksteelMyrPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b a devoid creature with a black mana cost may not block a creature with fear" $
        -- THE FALSIFIER for reading the blocker's PRINTED colour: the Devoid
        -- Drone's mana cost is {1}{B}, but CR 702.114a makes it colourless (not
        -- black), so it is not a legal blocker of a fear attacker. Fails against
        -- any implementation that reads the blocker's printed colour rather than
        -- its projected colour.
        let (gs0, mine, theirs) = attacking [Cards.pikerPrinting cards] [Cards.devoidDronePrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "illegal" (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0)))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.36b fear restricts being blocked, never blocking" $
        -- The 702.9b asymmetry, restated for fear: a fear creature blocking a
        -- plain attacker is legal.
        let (gs0, mine, theirs) = attacking [Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear b gs0))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker"
    ]

vigilanceTests :: Cards.Cards -> Tasty.TestTree
vigilanceTests cards =
  Tasty.testGroup
    "Vigilance"
    [ HU.testCase "CR 702.20b attacking doesn't tap a creature with vigilance, but does tap its neighbor" $
        -- Both creatures in ONE declaration, so a blanket "nothing taps" bug
        -- cannot pass: the Piker must still tap.
        let (gs, mine, _) = S.combatBoardOf [Cards.windseekerCentaurPrinting cards, Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
         in case mine of
              [centaur, piker] -> do
                HU.assertEqual "both attacking" 2 (length (declaredAttackers after))
                HU.assertEqual "the centaur is untapped" (Just TapState.Untapped) (tapStateOf centaur after)
                HU.assertEqual "the piker is tapped" (Just TapState.Tapped) (tapStateOf piker after)
              _ -> HU.assertFailure "fixture should have two attackers",
      HU.testCase "CR 702.20b vigilance still attacks" $
        -- Vigilance is not a legality question: the creature is declared as an
        -- attacker exactly as normal. It simply skips CR 508.1f's tap.
        let (gs, mine, _) = S.combatBoardOf [Cards.windseekerCentaurPrinting cards] [Cards.pikerPrinting cards]
            after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
         in HU.assertEqual "attacking" mine (declaredAttackers after),
      HU.testCase "CR 702.20b an untapped vigilant attacker can still be blocked" $
        -- It is attacking, so it is in the Combat record, tapped or not.
        let (gs, mine, theirs) = S.combatBoardOf [Cards.windseekerCentaurPrinting cards] [Cards.pikerPrinting cards]
            steps = do
              Combat.declareAttackers S.alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              attacker : _ -> HU.assertEqual "blocked" (Set.fromList theirs) (Combat.blockersOf attacker after)
    ]

combatLegalityTests :: Cards.Cards -> Tasty.TestTree
combatLegalityTests cards =
  Tasty.testGroup
    "CombatLegality"
    [ HU.testCase "a Settled untapped creature may attack" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 1 0
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ -> HU.assertBool "may attack" (Combat.canAttack S.alice oid gs),
      HU.testCase "CR 302.6 a summoning sick creature may not attack" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 1 0
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ ->
                let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
                 in HU.assertBool "may not attack" (not (Combat.canAttack S.alice oid sick)),
      HU.testCase "CR 508.1a a tapped creature may not attack" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 1 0
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              oid : _ ->
                let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
                 in HU.assertBool "may not attack" (not (Combat.canAttack S.alice oid tapped)),
      HU.testCase "a land may not attack" $
        let gs = (S.landsInPlay (Cards.mountainPrinting cards) 1) {GameState.activePlayer = S.alice}
         in case Game.zoneMembers Zone.Battlefield S.alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> HU.assertBool "may not attack" (not (Combat.canAttack S.alice oid gs)),
      HU.testCase "you may not attack with a creature you do not control" $
        let (gs, _, theirs) = S.combatBoard (Cards.pikerPrinting cards) 1 1
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              oid : _ -> HU.assertBool "not alice's" (not (Combat.canAttack S.alice oid gs)),
      -- CR 302.6 restricts attacking and tap abilities. It says NOTHING about
      -- blocking, and getting this wrong is the classic beginner bug.
      HU.testCase "CR 302.6 a summoning sick creature MAY block" $
        let (gs, _, theirs) = S.combatBoard (Cards.pikerPrinting cards) 1 1
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              oid : _ ->
                let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
                 in HU.assertBool "may block" (Combat.canBlock S.bob oid sick),
      HU.testCase "CR 509.1a a tapped creature may not block" $
        let (gs, _, theirs) = S.combatBoard (Cards.pikerPrinting cards) 1 1
         in case theirs of
              [] -> HU.assertFailure "fixture should have a blocker"
              oid : _ ->
                let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
                 in HU.assertBool "may not block" (not (Combat.canBlock S.bob oid tapped)),
      HU.testCase "legalAttackers lists exactly the active player's creatures" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 2 3
         in HU.assertEqual "two" mine (Combat.legalAttackers S.alice gs),
      HU.testCase "CR 508.1a a player can attack with a creature they control but do not own" $
        let (oid, base) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            gs0 = S.giveControl oid S.alice base
         in do
              HU.assertBool "alice may attack with it" (elem oid (Combat.legalAttackers S.alice gs0))
              HU.assertBool "bob may not (not the controller, not active)" (notElem oid (Combat.legalAttackers S.bob gs0)),
      HU.testCase "the defending player is the non-active player" $
        let (gs, _, _) = S.combatBoard (Cards.pikerPrinting cards) 1 1
         in HU.assertEqual "bob defends" [S.bob] (Combat.defendingPlayers gs),
      HU.testCase "combat starts empty and clears" $
        let (gs, mine, _) = S.combatBoard (Cards.pikerPrinting cards) 1 0
            busy = case mine of
              [] -> gs
              oid : _ ->
                gs
                  { GameState.combat =
                      Combat.Type.MkCombat
                        { Combat.Type.attackers = Map.singleton oid (AttackTarget.OfPlayer S.bob),
                          Combat.Type.blockers = Map.empty,
                          Combat.Type.struckFirst = Nothing
                        }
                  }
         in do
              HU.assertEqual "starts empty" Map.empty (Combat.Type.attackers (GameState.combat gs))
              HU.assertEqual "clears" Map.empty (Combat.Type.attackers (GameState.combat (Combat.clearCombat busy)))
    ]

keywordTests :: Cards.Cards -> Tasty.TestTree
keywordTests cards =
  let gs0 = Setup.emptyGame S.bothPlayers
      -- Each M2a printing carries exactly its one keyword and no other.
      carriesOnly (printing, keyword) =
        let (oid, gs) = S.addCreature printing S.alice gs0
            name = Text.unpack (Card.Type.name (Printing.card printing))
         in HU.testCase (name <> " carries exactly " <> show keyword) $ do
              HU.assertEqual "keywords" (Set.singleton keyword) (Projection.keywordsOf oid gs)
              HU.assertBool "hasKeyword" (Projection.hasKeyword keyword oid gs)
   in Tasty.testGroup
        "Keyword"
        ( fmap
            carriesOnly
            [ (Cards.birdMaidenPrinting cards, Keyword.Flying),
              (Cards.nimbleBirdstickerPrinting cards, Keyword.Reach),
              (Cards.ogreSentryPrinting cards, Keyword.Defender),
              (Cards.windseekerCentaurPrinting cards, Keyword.Vigilance),
              (Cards.goblinChariotPrinting cards, Keyword.Haste)
            ]
            <> [ HU.testCase "a Piker has no keywords" $
                   let (oid, gs) = S.addCreature (Cards.pikerPrinting cards) S.alice gs0
                    in do
                         HU.assertEqual "none" Set.empty (Projection.keywordsOf oid gs)
                         HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying oid gs)),
                 HU.testCase "a Mountain has no keywords" $
                   let gs = S.landsInPlay (Cards.mountainPrinting cards) 1
                    in case Game.zoneMembers Zone.Battlefield S.alice gs of
                         [] -> HU.assertFailure "fixture should have one Mountain"
                         oid : _ -> HU.assertEqual "none" Set.empty (Projection.keywordsOf oid gs),
                 HU.testCase "an unknown id has no keywords" $
                   HU.assertEqual "none" Set.empty (Projection.keywordsOf (ObjectId.MkObjectId 999) gs0),
                 -- Flying is on Bird Maiden and NOT on Nimble Birdsticker. If this
                 -- passes while the reach case above also passes, the two keywords
                 -- are genuinely distinct rather than one flag.
                 HU.testCase "reach is not flying" $
                   let (oid, gs) = S.addCreature (Cards.nimbleBirdstickerPrinting cards) S.alice gs0
                    in HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying oid gs))
               ]
        )

-- Run whole steps until the first-strike combat damage step has been dealt
-- (struckFirst is set) or combat ends, so a test can observe the board BETWEEN
-- the two combat damage steps.
runToFirstStrikeDone :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToFirstStrikeDone answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (Combat.Type.struckFirst (GameState.combat g))
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

firstStrikeTests :: Cards.Cards -> Tasty.TestTree
firstStrikeTests cards =
  Tasty.testGroup
    "FirstStrike"
    [ HU.testCase "CR 702.7b a first striker kills a vanilla blocker and lives" $
        -- The tiger (2/1 first strike) kills the Piker (2/1) in the first-strike
        -- step; the SBA between steps buries it before it can deal, so the tiger
        -- survives at zero damage.
        let (gs, _, _) = S.combatBoardOf [Cards.sabretoothTigerPrinting cards] [Cards.pikerPrinting cards]
            after = S.runCombat S.aggressiveAnswer gs
         in do
              HU.assertEqual "the blocker is dead" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "the first striker lives" 1 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 510.2 the control: two vanilla 2/1s trade" $
        -- With a Piker in the tiger's place there is one combat damage step and
        -- both die. So first strike is the sole cause above.
        let (gs, _, _) = S.combatBoardOf [Cards.pikerPrinting cards] [Cards.pikerPrinting cards]
            after = S.runCombat S.aggressiveAnswer gs
         in do
              HU.assertEqual "alice's is dead" 0 (S.creaturesInPlay S.alice after)
              HU.assertEqual "bob's is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.4b a double striker deals twice to an unblocked player" $
        -- The raptor (2/1 double strike) deals 2 in each step: bob loses 4.
        let (gs, _, _) = S.combatBoardOf [Cards.ridgetopRaptorPrinting cards] []
            after = S.runCombat S.aggressiveAnswer gs
         in HU.assertEqual "bob took 4" (Just 16) (S.lifeOf S.bob after),
      HU.testCase "CR 702.7b the control: a first striker deals once to a player" $
        let (gs, _, _) = S.combatBoardOf [Cards.sabretoothTigerPrinting cards] []
            after = S.runCombat S.aggressiveAnswer gs
         in HU.assertEqual "bob took 2" (Just 18) (S.lifeOf S.bob after),
      HU.testCase "CR 510.1b the control: a vanilla creature deals once to a player" $
        let (gs, _, _) = S.combatBoardOf [Cards.pikerPrinting cards] []
            after = S.runCombat S.aggressiveAnswer gs
         in HU.assertEqual "bob took 2" (Just 18) (S.lifeOf S.bob after),
      HU.testCase "CR 510.4 double strike kills a 3/3 across two steps; first strike does not" $
        -- The raptor deals 2 + 2 = 4 to the Ogre (3/3), killing it. A first
        -- striker deals 2 once, and the Ogre lives.
        let raptorVs = S.combatBoardOf [Cards.ridgetopRaptorPrinting cards] [Cards.ogreSentryPrinting cards]
            tigerVs = S.combatBoardOf [Cards.sabretoothTigerPrinting cards] [Cards.ogreSentryPrinting cards]
            afterRaptor = S.runCombat S.aggressiveAnswer (frst raptorVs)
            afterTiger = S.runCombat S.aggressiveAnswer (frst tigerVs)
         in do
              HU.assertEqual "double strike kills the Ogre" 0 (S.creaturesInPlay S.bob afterRaptor)
              HU.assertEqual "first strike leaves the Ogre" 1 (S.creaturesInPlay S.bob afterTiger),
      HU.testCase "CR 510.4 a striker killed in the first step does not deal in the second" $
        -- Raptor (double strike) and tiger (first strike) each block-kill the
        -- other in the first step. Neither is "remaining" for the second step, so
        -- no second-wave damage; both are simply dead.
        let (gs, _, _) = S.combatBoardOf [Cards.ridgetopRaptorPrinting cards] [Cards.sabretoothTigerPrinting cards]
            after = S.runCombat S.aggressiveAnswer gs
         in do
              HU.assertEqual "attacker dead" 0 (S.creaturesInPlay S.alice after)
              HU.assertEqual "blocker dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 510.4 the mixed board: first strike once, vanilla once, double strike twice" $
        -- Tiger (first strike), raptor (double strike) and Piker (vanilla) all
        -- attack unblocked. First-strike step: tiger 2 + raptor 2 = 4. Second
        -- step: raptor 2 + Piker 2 = 4. bob: 20 - 8 = 12. The naive "strikers in
        -- step one, everyone else in step two" drops the raptor's second hit and
        -- lands bob at 14.
        let (gs, _, _) = S.combatBoardOf [Cards.sabretoothTigerPrinting cards, Cards.ridgetopRaptorPrinting cards, Cards.pikerPrinting cards] []
            mid = runToFirstStrikeDone S.aggressiveAnswer gs
            after = S.runCombat S.aggressiveAnswer gs
         in do
              HU.assertEqual "after the first-strike step, bob took 4" (Just 16) (S.lifeOf S.bob mid)
              HU.assertEqual "after both steps, bob took 8" (Just 12) (S.lifeOf S.bob after)
    ]

-- The state out of a combatBoardOf triple.
frst :: (a, b, c) -> a
frst (a, _, _) = a

m2bExitTests :: Cards.Cards -> Tasty.TestTree
m2bExitTests cards =
  Tasty.testGroup
    "M2bExit"
    [ HU.testCase "the milestone: first strike breaks the trade, double strike doubles the hit, no attacker no damage" $
        let trade = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [Cards.sabretoothTigerPrinting cards] [Cards.pikerPrinting cards]))
            doubled = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [Cards.ridgetopRaptorPrinting cards] []))
            quiet = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [] []))
         in do
              HU.assertEqual "first striker lives" 1 (S.creaturesInPlay S.alice trade)
              HU.assertEqual "its would-be killer is dead" 0 (S.creaturesInPlay S.bob trade)
              HU.assertEqual "double striker deals 4" (Just 16) (S.lifeOf S.bob doubled)
              HU.assertEqual "an attacker-less turn deals nothing" (Just 20) (S.lifeOf S.bob quiet)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Combat"
    [ combatLegalityTests cards,
      declareTests cards,
      combatDamageTests cards,
      keywordTests cards,
      firstStrikeTests cards,
      m2bExitTests cards,
      defenderTests cards,
      vigilanceTests cards,
      hasteTests cards,
      evasionTests cards,
      controlChangeSicknessTests cards
    ]
