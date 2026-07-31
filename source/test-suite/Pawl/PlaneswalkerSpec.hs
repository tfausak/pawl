-- Covers CR 306, the planeswalker card type, across the four modules it reaches:
-- Pawl.Engine.Projection's intrinsic CR 306.5b enters-with replacement and
-- Pawl.Engine.Replacement's EntryR arm that carries it out, Pawl.Engine.Cost's
-- two CR 606.4 loyalty cost components, Pawl.Engine.Activate's CR 606.3 gate, and
-- Pawl.Engine.Sba's CR 704.5i zero-loyalty state-based action.
--
-- Jace Beleren is the whole proof: {1}{U}{U} Legendary Planeswalker -- Jace, with
-- printed loyalty 3 and three loyalty abilities (+2, -1, -10). Its -10 is what
-- makes CR 606.6 observable at 3 loyalty, and three -1s across three of alice's
-- turns are what drive it to 0 for CR 704.5i.
module Pawl.PlaneswalkerSpec where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Jace Beleren's abilities in printed order: +2, -1, -10. Indexed rather than
-- taken with a `head` the way every other spec's `theAbility` is, because this is
-- the first card in the pool with more than one activated ability and CR 606.6 is
-- a claim about WHICH of them is offered.
abilityAt :: Int -> Printing.Printing -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilityAt i p = take 1 (drop i (Card.Type.activatedAbilities (Printing.card p)))

-- The Activate action for one of Jace's abilities, as a one-or-zero-element list
-- so a fixture that finds no such ability produces an assertion failure rather
-- than a partial pattern.
activation :: ObjectId.ObjectId -> Int -> Printing.Printing -> [A.Action]
activation oid i p = fmap (A.Activate oid) (abilityAt i p)

plusTwo, minusOne, minusTen :: Int
plusTwo = 0
minusOne = 1
minusTen = 2

-- Activate one of Jace's abilities and let it resolve.
useAbility :: Int -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
useAbility i p oid gs = case abilityAt i p of
  ability : _ -> S.runPure S.identityAnswer gs (do Activate.activateAbility S.alice oid ability; Stack.resolveTop)
  [] -> gs

-- alice with three Islands untapped and Jace Beleren in hand, in her precombat
-- main phase with priority -- CR 306.1's window ("during a main phase of their
-- turn when the stack is empty") and enough mana for {1}{U}{U}.
--
-- Jace is then cast and resolved through the ordinary path, so the loyalty
-- counters come from CR 306.5b's replacement rather than from a fixture.
jaceOnBattlefield :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
jaceOnBattlefield island jace =
  let (gs, handId) = S.handOne jace (stockLibraries island (S.landsInPlay island 3))
      after = S.runPure S.identityAnswer gs (do Cast.castSpell S.alice handId; Stack.resolveTop)
   in (theJace after, after)

-- Four cards in each library. Jace's abilities all draw, and CR 704.5b would end
-- the game on an empty one -- so the libraries are stocked to keep every
-- assertion about loyalty and about who drew from resting on a deck-out.
stockLibraries :: Printing.Printing -> GameState.GameState -> GameState.GameState
stockLibraries island base =
  List.foldl' (\gs pid -> snd (S.addLibraryCard island pid gs)) base (concat (replicate 4 [S.alice, S.bob]))

-- The planeswalker on the battlefield, found by name because CR 400.7 mints a new
-- object as the spell moves and the hand's id does not survive the cast.
theJace :: GameState.GameState -> ObjectId.ObjectId
theJace gs =
  let isJace oid = fmap Card.Type.name (Game.cardOf oid gs) == Just (Text.pack "Jace Beleren")
   in case filter isJace (Set.toList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> S.noSource

-- Hand the turn round the table until alice is active again, and put her back in
-- her precombat main phase with priority. Two handoffs in a two-player game.
--
-- CR 606.3's limit is "that turn", and Pawl.Engine.Engine.beginTurnOf clears the
-- CR 608.2i log the limit is read out of -- so this is also what proves the limit
-- expires rather than merely existing.
alicesNextTurn :: GameState.GameState -> GameState.GameState
alicesNextTurn gs =
  let bobs = S.runPure S.identityAnswer gs Engine.handoffTurn
      back = S.runPure S.identityAnswer bobs Engine.handoffTurn
   in back {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}

tests :: Registry.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Planeswalker"
    [ HU.testCase "CR 306.5b Jace Beleren enters with three loyalty counters" $ do
        island <- Registry.printing registry "Island"
        jace <- Registry.printing registry "Jace Beleren"
        let (jaceId, after) = jaceOnBattlefield island jace
        HU.assertBool "on the battlefield" (Set.member jaceId (GameState.battlefield after))
        HU.assertEqual "loyalty 3" 3 (S.counterOf CounterKind.Loyalty jaceId after),
      HU.testCase "CR 606.4 the +2 adds two loyalty counters and each player draws" $ do
        island <- Registry.printing registry "Island"
        jace <- Registry.printing registry "Jace Beleren"
        let (jaceId, board) = jaceOnBattlefield island jace
            after = useAbility plusTwo jace jaceId board
        HU.assertEqual "loyalty 3 + 2" 5 (S.counterOf CounterKind.Loyalty jaceId after)
        HU.assertEqual "alice drew" 1 (S.handSize S.alice after)
        HU.assertEqual "bob drew too" 1 (S.handSize S.bob after),
      HU.testCase "CR 606.4 the -1 removes a loyalty counter and the targeted player draws" $ do
        island <- Registry.printing registry "Island"
        jace <- Registry.printing registry "Jace Beleren"
        let (jaceId, board) = jaceOnBattlefield island jace
            after = useAbility minusOne jace jaceId board
        HU.assertEqual "loyalty 3 - 1" 2 (S.counterOf CounterKind.Loyalty jaceId after)
        HU.assertEqual "exactly one player drew exactly one card" 1 (S.handSize S.alice after + S.handSize S.bob after),
      HU.testCase "CR 606.6 the -10 is not offered at 3 loyalty, while the other two are" $ do
        island <- Registry.printing registry "Island"
        jace <- Registry.printing registry "Jace Beleren"
        let (jaceId, board) = jaceOnBattlefield island jace
            offered = Action.legalActions S.alice board
            isOffered i = not (null (activation jaceId i jace)) && all (`elem` offered) (activation jaceId i jace)
        HU.assertBool "the +2 is offered" (isOffered plusTwo)
        HU.assertBool "the -1 is offered" (isOffered minusOne)
        HU.assertBool "the -10 is NOT offered" (not (isOffered minusTen))
        HU.assertBool
          "and it is not activatable either"
          (not (any (\ab -> Activate.activatable S.alice jaceId ab board) (abilityAt minusTen jace))),
      HU.testCase "CR 606.3 a second loyalty ability is not offered in the same turn" $ do
        island <- Registry.printing registry "Island"
        jace <- Registry.printing registry "Jace Beleren"
        let (jaceId, board) = jaceOnBattlefield island jace
            after = useAbility plusTwo jace jaceId board
            offered = Action.legalActions S.alice after
        HU.assertEqual "the +2 resolved" 5 (S.counterOf CounterKind.Loyalty jaceId after)
        HU.assertBool "the +2 is not offered again" (all (`notElem` offered) (activation jaceId plusTwo jace))
        HU.assertBool "and neither is the -1" (all (`notElem` offered) (activation jaceId minusOne jace))
        HU.assertBool
          "but the limit expires with the turn"
          (all (`elem` Action.legalActions S.alice (alicesNextTurn after)) (activation jaceId plusTwo jace)),
      HU.testCase "CR 606.3 a loyalty ability is not offered on an opponent's turn" $ do
        island <- Registry.printing registry "Island"
        jace <- Registry.printing registry "Jace Beleren"
        let (jaceId, board) = jaceOnBattlefield island jace
            theirTurn = board {GameState.activePlayer = S.bob, GameState.priority = Just S.alice}
        HU.assertBool
          "the +2 is not offered"
          (all (`notElem` Action.legalActions S.alice theirTurn) (activation jaceId plusTwo jace)),
      HU.testCase "CR 704.5i three -1s across three turns bury Jace in his owner's graveyard" $ do
        island <- Registry.printing registry "Island"
        jace <- Registry.printing registry "Jace Beleren"
        let (jaceId, board) = jaceOnBattlefield island jace
            turn gs = S.settleSba (alicesNextTurn (useAbility minusOne jace jaceId gs))
            afterOne = turn board
            afterTwo = turn afterOne
            afterThree = turn afterTwo
        HU.assertEqual "loyalty 2 after one activation" 2 (S.counterOf CounterKind.Loyalty jaceId afterOne)
        HU.assertEqual "loyalty 1 after two" 1 (S.counterOf CounterKind.Loyalty jaceId afterTwo)
        HU.assertBool "off the battlefield after three" (not (Set.member jaceId (GameState.battlefield afterThree)))
        HU.assertEqual "in its owner's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice afterThree))
    ]
