-- Covers Pawl.PlayerEffect and Pawl.Cost, plus the four types they case on
-- (Pawl.Type.PlayerEffect, PlayerScope, SpellCriterion, PlayerStaticAbility) and
-- the stored carrier Pawl.Type.ActivePlayerEffect. CR 613.10/613.11: the
-- continuous effects that affect PLAYERS and the RULES OF THE GAME, which sit
-- outside the CR 613 layer system entirely.
--
-- The five gate cards: Rule of Law, Thalia Guardian of Thraben, Sapphire
-- Medallion, Reliquary Tower and Silence.
module Pawl.PlayerEffectSpec where

import qualified Pawl.Action as Action
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as Action.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Rule of Law {2}{W} Enchantment: "Each player can't cast more than one spell
-- each turn." alice has nine untapped Plains (mana is never the reason a cast is
-- unavailable) and three Rule of Law cards in hand, in her own precombat main
-- phase with an empty stack.
ruleOfLawBoard :: Cards.Cards -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ruleOfLawBoard cards =
  let base = S.landsInPlay (Cards.plainsPrinting cards) 9
      (a, gs1) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice base
      (b, gs2) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice gs1
      (c, gs3) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice gs2
   in ( a,
        b,
        c,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

ruleOfLawTests :: Cards.Cards -> Tasty.TestTree
ruleOfLawTests cards =
  let resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      (a, b, _, board) = ruleOfLawBoard cards
      -- Cast Rule of Law itself, and let it resolve onto the battlefield.
      afterFirst = resolveAll (S.runPure S.identityAnswer board (Cast.castSpell S.alice a))
   in Tasty.testGroup
        "RuleOfLaw"
        [ HU.testCase "before any spell is cast, both cards are castable" $
            do
              HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice board))
              HU.assertBool "a offered" (elem (Action.Type.Cast a) (Action.legalActions S.alice board))
              HU.assertBool "b offered" (elem (Action.Type.Cast b) (Action.legalActions S.alice board)),
          -- Ruling: "Rule of Law looks at the entire turn to see if a player has
          -- cast a spell, even if Rule of Law wasn't on the battlefield when that
          -- spell was cast. Notably, you can't cast Rule of Law and then cast
          -- another spell during the same turn." THE FALSIFIER: the spell that
          -- used up the allowance is Rule of Law itself, cast BEFORE the effect
          -- existed. Any per-effect watermark or counter fails here.
          HU.testCase "CR 601.3 casting Rule of Law itself uses up the turn's one spell" $
            do
              HU.assertBool "alice is now prohibited" (PlayerEffect.prohibitsCasting S.alice afterFirst)
              HU.assertEqual
                "no cast is offered at all"
                []
                (filter isCast (Action.legalActions S.alice afterFirst)),
          -- The limit is counted PER PLAYER: bob has cast nothing this turn, so
          -- EachPlayer does not prohibit him.
          HU.testCase "CR 109.5 the EachPlayer scope still counts each player's own casts" $
            HU.assertBool "bob is not prohibited" (not (PlayerEffect.prohibitsCasting S.bob afterFirst)),
          -- CR 608.2i: the log is cleared at turn handoff, so "this turn" is
          -- exactly the log's own extent.
          HU.testCase "CR 608.2i the restriction lifts on the next turn" $
            let handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
                nextOwnTurn =
                  (handoff (handoff afterFirst))
                    { GameState.phase = Phase.PrecombatMain,
                      GameState.priority = Just S.alice
                    }
             in do
                  HU.assertEqual "alice is active again" S.alice (GameState.activePlayer nextOwnTurn)
                  HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice nextOwnTurn))
                  HU.assertBool "b offered again" (elem (Action.Type.Cast b) (Action.legalActions S.alice nextOwnTurn)),
          -- Ruling: "If you cast a spell that was countered, you can't cast
          -- another spell during the same turn." The counted event is the CAST.
          HU.testCase "CR 601.2i a countered spell still counted" $
            let (_, x, _, plain) = ruleOfLawBoard cards
                onBoard = snd (S.addCreature (Cards.ruleOfLawPrinting cards) S.alice plain)
                cast = S.runPure S.identityAnswer onBoard (Cast.castSpell S.alice x)
             in case GameState.stack cast of
                  [] -> HU.assertFailure "expected the spell on the stack"
                  top : _ ->
                    let countered = S.runPure S.identityAnswer cast (Event.counter top)
                     in do
                          HU.assertEqual "the stack is empty again" [] (GameState.stack countered)
                          HU.assertBool "still prohibited" (PlayerEffect.prohibitsCasting S.alice countered),
          -- The effect is RE-DERIVED from the battlefield on every read, so there
          -- is no stored state to unwind when its source leaves.
          HU.testCase "CR 604.2 destroying Rule of Law lifts the restriction in the same turn" $
            let (_, _, z, plain) = ruleOfLawBoard cards
                (rol, onBoard) = S.addCreature (Cards.ruleOfLawPrinting cards) S.alice plain
                castOne = S.withEvent (GameEvent.SpellCast S.alice) onBoard
                gone = S.runPure S.identityAnswer castOne (Event.destroy rol)
             in do
                  HU.assertBool "prohibited while it stands" (PlayerEffect.prohibitsCasting S.alice castOne)
                  HU.assertBool "not prohibited once it is gone" (not (PlayerEffect.prohibitsCasting S.alice gone))
                  HU.assertBool "and a cast is offered again" (elem (Action.Type.Cast z) (Action.legalActions S.alice gone)),
          -- CR 601.3's prohibit half applies to EVERY cast, including a
          -- Panglacial Wurm cast from the library: the Panglacial permission
          -- (Cast.permitsCastWhileSearching) excepts only the timing half, not
          -- the prohibition half. Seven Forests pay the Wurm's {5}{G}{G}.
          HU.testCase "CR 601.3 Rule of Law also prohibits casting Panglacial Wurm from the library" $
            let base = S.landsInPlay (Cards.forestPrinting cards) 7
                (_, withRuleOfLaw) = S.addCreature (Cards.ruleOfLawPrinting cards) S.alice base
                (_, gs) = S.addLibraryCard (Cards.panglacialWurmPrinting cards) S.alice withRuleOfLaw
                castOne = S.withEvent (GameEvent.SpellCast S.alice) gs
             in do
                  -- Positive control: without it, the negative assertion below
                  -- could pass merely because the Wurm was never offered at all.
                  HU.assertEqual
                    "before any cast, the Wurm is offered from the library"
                    1
                    (length (Cast.castableWhileSearching S.alice gs))
                  HU.assertEqual
                    "Rule of Law's one-spell limit blocks the library cast too"
                    []
                    (Cast.castableWhileSearching S.alice castOne)
        ]

isCast :: Action.Type.Action -> Bool
isCast action = case action of
  Action.Type.Cast _ -> True
  Action.Type.Play _ -> False
  Action.Type.Activate _ _ -> False
  Action.Type.Pass -> False

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.PlayerEffectSpec" [ruleOfLawTests cards]
