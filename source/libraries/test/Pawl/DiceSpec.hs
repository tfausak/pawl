{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 706 ROLLING A DIE -- Pawl.Types.RollDie, Effect.RollDie's arm in
-- Pawl.Engine.Resolve, and the Pawl.Types.Prompt / Pawl.Types.Response pair the
-- roll is externalised through. The transcript legs live in Pawl.ReplaySpec
-- with the other randomness prompts.
--
-- FIVE FIXTURES. Ancient Copper Dragon ("Flying /
-- Whenever this creature deals combat damage to a player, roll a d20. You create
-- a number of Treasure tokens equal to the result") is CR 706.4's, the result
-- read straight into a count; Djinni Windseer ("Flying / When this creature
-- enters, roll a d20. / 1-9 | Scry 1. / 10-19 | Scry 2. / 20 | Scry 3.") is CR
-- 706.3's results table, where the result selects an effect instead; Diviner's
-- Portent ("Roll a d20 and add the number of cards in your hand. / 1-14 | Draw X
-- cards. / 15+ | Scry X, then draw X cards.") is CR 706.2's modifier, printed in
-- the instruction that ordered the roll. Between them that is the whole of CR
-- 706 this file can reach with ONE die; Valiant Endeavor ("Roll two d6 and
-- choose one result. Destroy each creature with power greater than or equal to
-- that result. Then create a number of 2/2 white Knight creature tokens with
-- vigilance equal to the other result") is the fourth fixture and CR 706.1's
-- count, CR 706.4's choice among the results and the "other result" beside it.
-- CR 614.1a's replacement over the roll is the fifth fixture, Pixie Guide, at the
-- bottom of this file -- the ignore of CR 706.6 rides it, no instruction in
-- data\/cards\/ printing one of its own.
-- Left out: no reroll and no modifier from another source (#2083), no "Roll
-- again" (#2124), and no reading that takes the results as a set (#3243). CR
-- 706.1's roll does record its event, but the trigger reading it lives in
-- Pawl.EventTriggerSpec beside the other condition cases.
--
-- THE ASSERTED QUANTITY on the DRAGON's boards is how many Treasure tokens alice
-- controls once combat damage has been dealt. It is the roll's result made
-- visible: nothing else on this board mints a token, bob controls nothing, and
-- the Treasure's own mana ability is never activated. Tokens enter under their
-- creator's control (CR 111.2) and alice is the only creator, so there is no
-- second trace to the same number.
--
-- FOUR BOARDS differing in ONE thing -- the number the interpreter answers with:
-- 13, 7, 20 and 21. 13 is the primary pin because it is none of the values a
-- wrong implementation could produce by accident: not 1 (Replay.defaultAnswer,
-- which S.identityAnswer falls through to), not 20 (the die's size), not 6 or 5
-- (the dragon's power and toughness) and not 0. The 7 leg falsifies a hard-coded
-- 13. The 20/21 pair straddles CR 706.1a's upper bound, which is the only place
-- an off-by-one on the face count is visible: 20 IS an outcome of a d20, and 21
-- is not.
--
-- TWO SEATS, not three: the roll's result is read by a Create scoped to "you",
-- so no clause here ranges over opponents, and a third seat would only add a
-- defending-player question. Bob controls nothing, because a blocker would keep
-- the combat damage off him and the trigger would never fire at all.
--
-- THE ASSERTED QUANTITY on the WINDSEER's boards is the identity of the top card
-- of alice's library after the trigger resolves, over a library of six cards
-- interned from six different printings. Every scry prompt is answered by
-- bottoming the whole look, so scry N moves exactly N cards off the top and the
-- top card names N: card 2 for scry 1, card 3 for scry 2, card 4 for scry 3, and
-- card 1 for a table that fired nothing at all. GameEvent.Scried carries a
-- PlayerId and no count, so library order is the only gameplay-level quantity
-- that can tell the striations apart.
--
-- SIX CARDS rather than four so that a table firing two striations at once is
-- distinguishable rather than clamped by a short library: dropping the 10-19
-- band's upper endpoint makes a roll of 20 scry 2 AND scry 3, which is five cards
-- bottomed and card 6 on top.
--
-- THE ASSERTED QUANTITY on the PORTENT's boards is the same top card, for the
-- same reason, over the same six-card library -- but reached by a CAST rather
-- than an enters trigger, since the Portent is an instant. Hand size is NOT the
-- reading: X is announced as 1 and both striations draw X, so the hand is one
-- larger under either implementation of the modifier. Only the scry the 15+ band
-- performs moves the library, so the top card is what separates the bands.
module Pawl.DiceSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  rollDieSpec s registry
  resultsTableSpec s registry
  modifierSpec s registry
  severalDiceSpec s registry
  dieRollRSpec s registry

treasure :: CardName.CardName
treasure = CardName.MkCardName (Text.pack "Treasure Token")

knight :: CardName.CardName
knight = CardName.MkCardName (Text.pack "Knight Token")

-- Pins BOTH questions this combat asks: alice attacks bob (CR 508.1), and the
-- d20 comes up `n`. The roll is answered by CONSTANT rather than by anything
-- derived from the prompt, so the engine cannot repair the answer after a
-- mutation, and never by 1, which is what Replay.defaultAnswer would supply
-- unasked.
rollAnswer :: Natural.Natural -> Prompt.Prompt r -> r
rollAnswer n p = case p of
  Prompt.RollDie _ -> n
  _ -> S.attackTo S.bob p

-- The same combat under an answerer that RECORDS what the roll prompt offered,
-- since the offer is not readable off the resulting board. S.runCombat's loop,
-- one monad up, because the trigger resolves in the combat damage step rather
-- than the step the fixture starts in.
offeredSides :: GameState.GameState -> [Natural.Natural]
offeredSides board =
  let logging :: Prompt.Prompt r -> State.State [Natural.Natural] r
      logging p = case p of
        Prompt.RollDie sides -> do
          State.modify' (sides :)
          pure (rollAnswer 13 p)
        _ -> pure (rollAnswer 13 p)
      go n gs =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result gs) || not (S.inCombatPhase (GameState.phase gs))
          then pure gs
          else do
            (_, next) <- Engine.runGame logging gs Engine.runStep
            go (n - 1) next
   in reverse (State.execState (go (24 :: Int) board) [])

rollDieSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rollDieSpec s registry = Spec.describe s "RollDie" $ do
  Spec.it s "CR 706.4 the result of the roll is the number of Treasure tokens" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    let (board, _, _) = S.combatBoardOf [dragon] []
        after n = S.runCombat (rollAnswer n) board
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: one Treasure per pip of the roll.
    Spec.assertEqWith
      s
      "CR 706.4: thirteen Treasures for a roll of thirteen"
      (S.countOnBattlefieldByName treasure S.alice (after 13))
      13
    -- The paired board, one thing different: the same combat with the die
    -- pinned to seven. Falsifies any implementation that hard-codes the pin
    -- above, or that reads the count from anywhere but the roll.
    Spec.assertEqWith
      s
      "and seven Treasures for a roll of seven"
      (S.countOnBattlefieldByName treasure S.alice (after 7))
      7
  Spec.it s "CR 706.1a a dN's outcomes are numbered from 1 to N, both ends included" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    let (board, _, _) = S.combatBoardOf [dragon] []
        after n = S.runCombat (rollAnswer n) board
    -- The top face IS an outcome, so it is admitted: a range check written
    -- `< sides` instead of `<= sides` refuses it and falls back to the floor.
    Spec.assertEqWith
      s
      "CR 706.1a: 20 is an outcome of a d20, so twenty Treasures"
      (S.countOnBattlefieldByName treasure S.alice (after 20))
      20
    -- And the range is CLOSED above: an answer no d20 could show is refused and
    -- CR 706.1a's floor stands, the instruction being mandatory. An engine that
    -- trusted the answer mints 21.
    Spec.assertEqWith
      s
      "CR 706.1a: 21 is not an outcome of a d20, so the floor stands"
      (S.countOnBattlefieldByName treasure S.alice (after 21))
      1
  Spec.it s "CR 706.1 the engine offers the die and never rolls it" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    let (board, _, _) = S.combatBoardOf [dragon] []
    -- Supporting, and in its own case so it cannot stand in for the counts
    -- above: what the engine ASKED is the visible half of "offer and filter
    -- back", and it asked once, for a twenty-sided die.
    Spec.assertEqWith s "asked once, offering twenty sides" (offeredSides board) [20]

-- Six cards on alice's library from six DIFFERENT printings, top-first, with
-- Djinni Windseer entering under her and its CR 603.6a trigger pending. The
-- printings differ so that no two library positions can be confused for each
-- other; the returned ids are the library reading top-first.
--
-- addLibraryCard puts its card ON TOP, so the deck is dealt deepest-first.
tableBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m ([ObjectId.ObjectId], GameState.GameState)
tableBoard s registry = do
  djinni <- S.printingOf s registry "Djinni Windseer"
  deck <- traverse (S.printingOf s registry) ["Goblin Piker", "Bird Maiden", "Mountain", "Forest", "Island", "Plains"]
  let deal (acc, gs) printing = let (oid, gs2) = S.addLibraryCard printing S.alice gs in (oid : acc, gs2)
      (ids, stocked) = List.foldl' deal ([], Setup.emptyGame S.bothPlayers) (reverse deck)
      (_, entered) = S.entersWithTrigger djinni S.alice stocked
  pure (ids, entered)

-- Pins the d20 to `n` and BOTTOMS the whole look of every scry, so the number of
-- cards that leave the top is the scry's count and nothing else. Bottoming the
-- offered list is not a search for a legal answer -- it names whatever it was
-- shown -- so a mutation that changed which striation fired changes the library
-- rather than being repaired by the answerer.
tableAnswer :: Natural.Natural -> Prompt.Prompt r -> r
tableAnswer n p = case p of
  Prompt.RollDie _ -> n
  Prompt.ChooseScry _ _ looked -> (looked, [])
  _ -> S.identityAnswer p

-- The pending enters trigger, put on the stack and not yet resolved.
placeTable :: Natural.Natural -> GameState.GameState -> GameState.GameState
placeTable n board = S.runPure (tableAnswer n) board Engine.placePendingTriggers

-- And resolved, under the same answerer, so the roll and the scry it selects are
-- the same run.
runTable :: Natural.Natural -> GameState.GameState -> GameState.GameState
runTable n board = S.runPure (tableAnswer n) (placeTable n board) Stack.resolveTop

-- Alice's library, top-first.
tableLibrary :: GameState.GameState -> [ObjectId.ObjectId]
tableLibrary = Game.zoneMembers Zone.Library S.alice

resultsTableSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
resultsTableSpec s registry = Spec.describe s "ResultsTable" $ do
  -- CR 706.3a's three striation forms on one card, read through the one thing
  -- that separates them: how many cards a scry took off the top. Card 1 on top
  -- is the vacuous board -- the table fired nothing -- and it is a DIFFERENT
  -- value from all three, so "the gate never held" cannot pass as any leg.
  Spec.it s "CR 706.3a the result selects one striation of the results table" $ do
    (ids, board) <- tableBoard s registry
    case ids of
      [_, second, third, fourth, _, _] -> do
        -- 10-19, the closed range in the middle: scry 2, so the third card is on
        -- top. First because the middle band is the one a half-open reading of
        -- CR 706.3a gets wrong in both directions.
        Spec.assertEqWith s "CR 706.3a: a roll of 10 is in 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (runTable 10 board))) (Just third)
        -- 1-9, the other closed range: scry 1, so the second card is on top.
        Spec.assertEqWith s "CR 706.3a: a roll of 9 is in 1-9, so scry 1" (Maybe.listToMaybe (tableLibrary (runTable 9 board))) (Just second)
        -- The single number: scry 3, so the fourth card is on top. A REGRESSION
        -- FENCE on this striation's own shape rather than a proof of it: 20 is a
        -- d20's top face, so a `20+` reading agrees with the printed `20` on
        -- every outcome, and swapping the card's Exactly for an AtLeast leaves
        -- this assertion green. Windseer's own instruction prints no modifier, so
        -- it becomes discriminable here only alongside one reaching the roll from
        -- ANOTHER source (#2083); the single-endpoint form itself is proved on
        -- Diviner's Portent below, whose printed modifier pushes a natural 20
        -- past the face count. What the assertion DOES prove is that 20 selects
        -- this striation and not the 10-19 band above it.
        Spec.assertEqWith s "CR 706.3a: a roll of 20 is the 20 striation, so scry 3" (Maybe.listToMaybe (tableLibrary (runTable 20 board))) (Just fourth)
      _ -> Spec.assertFailure s "expected six library cards"
  -- Both endpoints of a printed N1-N2 belong to it (CR 706.3a). 9 and 10 above
  -- are the adjacent pair that separates the two bands; these are the far ends,
  -- which an off-by-one at the other endpoint moves.
  Spec.it s "CR 706.3a a closed range includes both of its endpoints" $ do
    (ids, board) <- tableBoard s registry
    case ids of
      [_, second, third, _, _, _] -> do
        -- Moving this endpoint UP to 2 is visible here; deleting the test
        -- altogether is not, 1 being the die's floor. Same shape as the 20
        -- striation above, at the other end.
        Spec.assertEqWith s "CR 706.3a: 1 is the low end of 1-9, so scry 1" (Maybe.listToMaybe (tableLibrary (runTable 1 board))) (Just second)
        Spec.assertEqWith s "CR 706.3a: 19 is the high end of 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (runTable 19 board))) (Just third)
      _ -> Spec.assertFailure s "expected six library cards"
  -- The roll, the striations and the table are ONE ability, so entering puts one
  -- object on the stack and one resolution runs the whole table. Four striations
  -- transcribed as four abilities would put four there.
  Spec.it s "CR 706.3b the roll and its table are one ability" $ do
    (_, board) <- tableBoard s registry
    Spec.assertEqWith s "CR 706.3b: one ability on the stack, not one per striation" (length (GameState.stack (placeTable 9 board))) 1
    Spec.assertEqWith s "and it is gone after one resolution" (length (GameState.stack (runTable 9 board))) 0
  -- The fixture pin, in its own case so it cannot stand in for a striation
  -- above: the library really is the six cards in the order those assertions
  -- index, and CR 701.22a moves no card OUT of it, which is what makes "the top
  -- card" the whole reading.
  Spec.it s "CR 701.22a the fixture is six cards and the scry keeps them all" $ do
    (ids, board) <- tableBoard s registry
    Spec.assertEqWith s "six cards, top-first" (tableLibrary board) ids
    Spec.assertEqWith s "CR 701.22a: the library still holds all six" (length (tableLibrary (runTable 9 board))) 6

-- alice holds Diviner's Portent plus `others` cards of one filler printing, has
-- four untapped Islands, and a library of six cards from six DIFFERENT printings
-- so no two library positions can be confused. The returned ids are the library
-- reading top-first; the second component is the Portent in hand.
--
-- `others` IS the modifier: the Portent leaves the hand for the stack when it is
-- cast (CR 601.2a), so the hand at resolution holds exactly those. handOne
-- REPLACES the hand, so it runs before the filler is appended.
--
-- Four Islands, because X is announced as 1 on every leg and {X}{U}{U}{U} at
-- X=1 is four mana.
portentBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
portentBoard s registry others = do
  portent <- S.printingOf s registry "Diviner's Portent"
  island <- S.printingOf s registry "Island"
  filler <- S.printingOf s registry "Lightning Bolt"
  deck <- traverse (S.printingOf s registry) ["Goblin Piker", "Bird Maiden", "Mountain", "Forest", "Swamp", "Plains"]
  let (held, spell) = S.handOne portent (S.landsInPlay island 4)
      deal (acc, gs) printing = let (oid, gs2) = S.addLibraryCard printing S.alice gs in (oid : acc, gs2)
      (ids, stocked) = List.foldl' deal ([], held) (reverse deck)
      pad gs _ = snd (S.addHandCard filler S.alice gs)
  pure (ids, spell, List.foldl' pad stocked [1 .. others])

-- Pins every question this cast asks: X is 1, the d20 comes up `n`, and every
-- scry bottoms its WHOLE look, so the number of cards leaving the top is the
-- scry's count and nothing else. ChooseX gets its own arm rather than falling
-- through to S.identityAnswer, which reaches Replay.defaultAnswer and would
-- announce a value the test never chose.
portentAnswer :: Natural.Natural -> Prompt.Prompt r -> r
portentAnswer n p = case p of
  Prompt.RollDie _ -> n
  Prompt.ChooseX {} -> 1
  Prompt.ChooseScry _ _ looked -> (looked, [])
  _ -> S.identityAnswer p

-- Cast the Portent and resolve it, one run under one answerer.
runPortent :: Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
runPortent n spell board = S.runPure (portentAnswer n) board (S.cast S.alice spell >> Stack.resolveTop)

-- The same cast under an answerer that RECORDS what each roll prompt offered,
-- since the offer is not readable off the resulting board.
portentOffers :: ObjectId.ObjectId -> GameState.GameState -> [Natural.Natural]
portentOffers spell board =
  let logging :: Prompt.Prompt r -> State.State [Natural.Natural] r
      logging p = case p of
        Prompt.RollDie sides -> do
          State.modify' (sides :)
          pure (portentAnswer 14 p)
        _ -> pure (portentAnswer 14 p)
   in reverse (State.execState (Engine.runGame logging board (S.cast S.alice spell >> Stack.resolveTop)) [])

modifierSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
modifierSpec s registry = Spec.describe s "RollDieModifier" $ do
  -- CR 706.2's first sentence, on the only card in data/cards/ whose roll
  -- instruction prints a modifier. 14 is the primary pin because it is the printed boundary of the 1-14 band:
  -- the modifier is the only thing that can move a natural 14 out of it. It is
  -- also none of the values a wrong implementation reaches by accident -- not 1
  -- (Replay.defaultAnswer, which S.identityAnswer falls through to), not 20 (the
  -- die's size), not 15 (the other band's endpoint) and not 0.
  Spec.it s "CR 706.2 the instruction's modifier is added to the natural result" $ do
    (ids, spell, board) <- portentBoard s registry 1
    case ids of
      [_, _, third, _, _, _] ->
        -- THE GAMEPLAY ASSERTION, and the only one in this case so nothing can
        -- absorb a mutation. A natural 14 plus a hand of one is 15, which is the
        -- 15+ band: scry 1 bottoms card 1 and draw 1 takes card 2, leaving card
        -- 3 on top. An engine that dropped the modifier reads 14, fires the 1-14
        -- band instead, draws card 1 and leaves card 2 on top.
        Spec.assertEqWith
          s
          "CR 706.2: 14 plus a hand of one is 15, so the 15+ band scried"
          (Maybe.listToMaybe (tableLibrary (runPortent 14 spell board)))
          (Just third)
      _ -> Spec.assertFailure s "expected six library cards"
  -- The pair that proves the modifier is the HAND COUNT and not a constant:
  -- SAME die, different hand, different band. It is also the guard against a
  -- Quantity evaluated against a context whose fields were never filled: that
  -- answers 0 without raising, which is indistinguishable from an empty hand on
  -- one board and makes both boards agree.
  Spec.it s "CR 706.2 the modifier is the number the instruction names" $ do
    (ids, spell, small) <- portentBoard s registry 1
    (bigIds, bigSpell, big) <- portentBoard s registry 4
    case (ids, bigIds) of
      ([_, second, _, _, _, _], [_, _, bigThird, _, _, _]) -> do
        Spec.assertEqWith
          s
          "CR 706.2: 11 plus a hand of one is 12, so the 1-14 band drew without scrying"
          (Maybe.listToMaybe (tableLibrary (runPortent 11 spell small)))
          (Just second)
        Spec.assertEqWith
          s
          "CR 706.2: the same 11 plus a hand of four is 15, so the 15+ band scried"
          (Maybe.listToMaybe (tableLibrary (runPortent 11 bigSpell big)))
          (Just bigThird)
      _ -> Spec.assertFailure s "expected six library cards"
  -- CR 706.1a bounds the NATURAL result at 1..N; CR 706.2 adds the modifier
  -- afterwards and no rule bounds the sum. A natural 20 with a hand of five is a
  -- result of 25, past the die's own top face. An engine that reused the 1..N
  -- filter on the SUM falls to CR 706.1a's floor of 1 and fires the 1-14 band.
  Spec.it s "CR 706.1a the face is bounded by the die and the result is not" $ do
    (ids, spell, board) <- portentBoard s registry 5
    case ids of
      [_, _, third, _, _, _] ->
        Spec.assertEqWith
          s
          "CR 706.2: a natural 20 plus a hand of five is a result of 25"
          (Maybe.listToMaybe (tableLibrary (runPortent 20 spell board)))
          (Just third)
      _ -> Spec.assertFailure s "expected six library cards"
  -- The fixture pins, in their own case so they cannot stand in for a band
  -- above: the spell really resolved, and the library really lost exactly the
  -- one card X drew rather than being clamped short.
  Spec.it s "CR 608.2 the spell resolved and the library is not short" $ do
    (ids, spell, board) <- portentBoard s registry 1
    let after = runPortent 14 spell board
    Spec.assertEqWith s "six cards, top-first" (tableLibrary board) ids
    -- Before the stack reading, which a cast that never happened also satisfies.
    Spec.assertEqWith s "one card drawn out of six" (length (tableLibrary after)) 5
    Spec.assertEqWith s "nothing left on the stack" (length (GameState.stack after)) 0
  -- Supporting, and in its own case: the modifier is the engine's arithmetic, so
  -- it did not become a second roll or a bigger die.
  Spec.it s "CR 706.1 a modified roll is still one roll of the printed die" $ do
    (_, spell, board) <- portentBoard s registry 1
    Spec.assertEqWith s "asked once, offering twenty sides" (portentOffers spell board) [20]

-- alice holds Valiant Endeavor with six untapped Plains, and BOB controls the
-- two creatures the destruction judges: Bird Maiden (power 1) and Barkhide
-- Mauler (power 4). The two powers straddle every result the cases below choose,
-- so which creature survives IS the number the roller chose.
--
-- Under bob so that the Knight tokens, which arrive under alice (CR 111.2), are
-- the only things she owns on the battlefield -- S.countOnBattlefieldByName
-- indexes by owner, so a victim of hers would be counted while it lived.
endeavorBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
endeavorBoard s registry = do
  endeavor <- S.printingOf s registry "Valiant Endeavor"
  plains <- S.printingOf s registry "Plains"
  maiden <- S.printingOf s registry "Bird Maiden"
  mauler <- S.printingOf s registry "Barkhide Mauler"
  let (held, spell) = S.handOne endeavor (S.landsInPlay plains 6)
      (weak, withWeak) = S.addPermanent maiden S.bob held
      (strong, withBoth) = S.addPermanent mauler S.bob withWeak
  pure (spell, weak, strong, withBoth)

-- Pins both questions the resolution asks: each die comes up the next number of
-- `rolls`, in the order they are rolled, and the roller chooses the result at
-- `index`.
--
-- STATEFUL, because the two roll prompts are structurally identical: a pure
-- @Prompt r -> r@ cannot tell them apart, so it answers both the same number and
-- the two results coincide whatever the engine did. Six for a roll the script
-- did not plan, which is the die's top face rather than CR 706.1a's floor, so a
-- third die shows up as a number no assertion here expects.
endeavorAnswer :: Natural.Natural -> Prompt.Prompt r -> State.State [Natural.Natural] r
endeavorAnswer index p = case p of
  Prompt.RollDie _ -> do
    rolls <- State.get
    case rolls of
      h : t -> do
        State.put t
        pure h
      [] -> pure 6
  Prompt.ChooseDieResult {} -> pure index
  _ -> pure (S.identityAnswer p)

-- Cast the Endeavor and resolve it, one run under one answerer.
runEndeavor :: [Natural.Natural] -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
runEndeavor rolls index spell board =
  snd (State.evalState (Engine.runGame (endeavorAnswer index) board (S.cast S.alice spell >> Stack.resolveTop)) rolls)

-- The same cast under an answerer that RECORDS what each roll prompt offered and
-- what each choice prompt was shown, neither being readable off the board.
endeavorPrompts :: [Natural.Natural] -> ObjectId.ObjectId -> GameState.GameState -> ([Natural.Natural], [[Natural.Natural]])
endeavorPrompts rolls spell board =
  let logging :: Prompt.Prompt r -> State.State ([Natural.Natural], [[Natural.Natural]], [Natural.Natural]) r
      logging p = case p of
        Prompt.RollDie sides -> do
          (seen, asked, scripted) <- State.get
          case scripted of
            h : t -> do
              State.put (sides : seen, asked, t)
              pure h
            [] -> do
              State.put (sides : seen, asked, [])
              pure 6
        Prompt.ChooseDieResult _ _ _ candidates -> do
          State.modify' (\(seen, asked, scripted) -> (seen, NonEmpty.toList candidates : asked, scripted))
          pure 0
        _ -> pure (S.identityAnswer p)
      (offers, choices, _) = State.execState (Engine.runGame logging board (S.cast S.alice spell >> Stack.resolveTop)) ([], [], rolls)
   in (reverse offers, reverse choices)

severalDiceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
severalDiceSpec s registry = Spec.describe s "RollSeveralDice" $ do
  -- CR 706.1's other half. The pair of boards differs in ONE thing -- which of
  -- the two results the roller chose -- and every reading of the resolution
  -- moves with it: 5 and 2 are distinct, so are the creatures' powers, and the
  -- token count is the result the roller did NOT choose.
  Spec.it s "CR 706.4 the roller chooses one result and the other is the other" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: choosing the 2 leaves the 5 as "the other result", so five
    -- Knights. An engine that bound the chosen result twice mints two, one that
    -- rolled a single die mints none, and one that ignored the choice mints two.
    Spec.assertEqWith
      s
      "CR 706.4: choosing the second die's 2 leaves the first die's 5 to count"
      (S.countOnBattlefieldByName knight S.alice (runEndeavor [5, 2] 1 spell board))
      5
    -- The paired board, one thing different: the SAME two rolls with the first
    -- result chosen instead. Falsifies an engine that binds the results by
    -- position rather than by the answer.
    Spec.assertEqWith
      s
      "CR 706.4: choosing the first die's 5 leaves the second die's 2 to count"
      (S.countOnBattlefieldByName knight S.alice (runEndeavor [5, 2] 0 spell board))
      2
  -- The chosen result read as a BOUND rather than as a count, off the same pair
  -- of boards: CR 208.1 against the number the roller chose.
  Spec.it s "CR 706.4 the destruction judges power against the chosen result" $ do
    (spell, weak, strong, board) <- endeavorBoard s registry
    let chose2 = runEndeavor [5, 2] 1 spell board
        chose5 = runEndeavor [5, 2] 0 spell board
    Spec.assertBool s (not (S.onBattlefield strong chose2)) "power 4 is at least the chosen 2, so it is destroyed"
    Spec.assertBool s (S.onBattlefield weak chose2) "power 1 is not, so it survives"
    -- The same two creatures under the other choice: 4 is less than 5, so
    -- nothing dies. A filter comparing the wrong way, or against the other
    -- result, separates these two boards.
    Spec.assertBool s (S.onBattlefield strong chose5) "power 4 is less than the chosen 5, so it survives"
    Spec.assertBool s (S.onBattlefield weak chose5) "and so does power 1"
  -- CR 706.1a's own boundary, on the "greater than or equal" the card prints:
  -- the creature whose power EQUALS the chosen result is destroyed, which a
  -- strict comparison spares.
  Spec.it s "CR 208.1 a power equal to the result is destroyed" $ do
    (spell, weak, strong, board) <- endeavorBoard s registry
    let chose4 = runEndeavor [1, 4] 1 spell board
    Spec.assertBool s (not (S.onBattlefield strong chose4)) "power 4 equals the chosen 4, so it is destroyed"
    Spec.assertBool s (S.onBattlefield weak chose4) "power 1 is below it, so it survives"
  -- The choice is ELIDED where it is not one. Two dice showing the same number
  -- leave both slots holding that number whichever is named, so the engine asks
  -- nothing -- and the resolution still reads both results.
  Spec.it s "CR 706.4 two equal results are not a choice" $ do
    (spell, _, strong, board) <- endeavorBoard s registry
    let (offers, choices) = endeavorPrompts [3, 3] spell board
    Spec.assertEqWith s "nothing was asked to choose" choices []
    Spec.assertEqWith s "asked twice, offering six sides each" offers [6, 6]
    let after = runEndeavor [3, 3] 0 spell board
    Spec.assertEqWith s "CR 706.4: the other result is the other 3" (S.countOnBattlefieldByName knight S.alice after) 3
    Spec.assertBool s (not (S.onBattlefield strong after)) "and power 4 is at least 3, so it is destroyed"
  -- Supporting, and in its own case so it cannot stand in for a count above:
  -- what the engine ASKED. Two dice of six sides, and one choice offering both
  -- results in roll order.
  Spec.it s "CR 706.1 the count is how many dice the instruction offers" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    let (offers, choices) = endeavorPrompts [5, 2] spell board
    Spec.assertEqWith s "asked twice, offering six sides each" offers [6, 6]
    Spec.assertEqWith s "and offered both results, in roll order" choices [[5, 2]]

-- CR 614.1a over CR 706.1, and CR 706.6 behind it: Pixie Guide's "if you would
-- roll one or more dice, instead roll that many dice plus one and ignore the
-- lowest roll". The Thumb group in Pawl.CoinSpec one rule over, and the same
-- shape -- the Guide is the ONE thing that differs between the legs, so every
-- reading that moves is the replacement's doing.
--
-- TWO FIXTURES, because the sentence says "one or more dice" and the two halves
-- of that are separately breakable. Ancient Copper Dragon rolls ONE d20 and reads
-- the result straight into a token count, which is where "plus one, ignore the
-- lowest" reads as "take the higher"; Valiant Endeavor rolls TWO d6 and reads
-- BOTH results, which is where an engine that added the die without ignoring one
-- is visible -- three results leave no "other result" to count, so it mints
-- nothing.
--
-- NOT LEGENDARY, unlike Krark's Thumb, so a board may carry two Guides -- which
-- is what puts CR 614.5's read-forward under a gameplay assertion rather than a
-- prompt tally: the second row applies to the modified event, so four dice are
-- thrown and the TWO lowest go.
--
-- STATE-THREADED answerers throughout: the dice of one instruction are
-- structurally identical Prompt.RollDie questions, and a pure answerer cannot
-- tell them apart -- it would answer every die the same number, which is exactly
-- the board on which ignoring the lowest changes nothing.
dieRollRSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dieRollRSpec s registry = Spec.describe s "DieRollR" $ do
  Spec.it s "CR 614.1a Pixie Guide rolls one more die and CR 706.6 ignores the lowest" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    guide <- S.printingOf s registry "Pixie Guide"
    let (bare, _, _) = S.combatBoardOf [dragon] []
        guarded = snd (S.addPermanent guide S.alice bare)
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: two dice came up 7 and 13, the 7 was ignored, and the Dragon
    -- reads the 13 into thirteen Treasures. An engine that ignored the HIGHEST
    -- mints 7, one that ignored NEITHER reads the first die and mints 7 too, and
    -- one that never added the die never asks for the 13 at all.
    Spec.assertEqWith
      s
      "CR 706.6: the 7 is ignored, so the result is the 13"
      (S.countOnBattlefieldByName treasure S.alice (fst (guideCombat [7, 13] guarded)))
      13
    -- The same two faces in the other order, so the kept roll is the SECOND die
    -- under one reading and the FIRST under the other: an engine that kept a
    -- fixed position rather than the higher number separates these two boards.
    Spec.assertEqWith
      s
      "CR 706.6: the lowest goes whichever die showed it"
      (S.countOnBattlefieldByName treasure S.alice (fst (guideCombat [13, 7] guarded)))
      13
    -- The paired board, one thing different: no Guide. The same pinned faces, and
    -- the second is never reached.
    Spec.assertEqWith
      s
      "CR 706.1: without the Guide the instruction's own one die settles it"
      (S.countOnBattlefieldByName treasure S.alice (fst (guideCombat [7, 13] bare)))
      7
  Spec.it s "CR 706.1 the Guide adds a die to the instruction" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    guide <- S.printingOf s registry "Pixie Guide"
    let (bare, _, _) = S.combatBoardOf [dragon] []
        guarded = snd (S.addPermanent guide S.alice bare)
    -- Supporting, and in its own case so it cannot stand in for the counts
    -- above: what the engine ASKED. Two twenty-sided dice under the Guide, one
    -- without -- and the SIZE is untouched, which is the half of rule 706.1 the
    -- replacement says nothing about.
    Spec.assertEqWith s "CR 706.1a: two d20 under the Guide" (snd (guideCombat [7, 13] guarded)) [20, 20]
    Spec.assertEqWith s "CR 706.1a: one d20 without it" (snd (guideCombat [7, 13] bare)) [20]
  Spec.it s "CR 706.6 an ignored roll is not the other result" $ do
    (spell, weak, strong, board) <- endeavorBoard s registry
    guide <- S.printingOf s registry "Pixie Guide"
    let guarded = snd (S.addPermanent guide S.alice board)
    -- THE GAMEPLAY ASSERTION: three d6 came up 1, 3 and 5, the 1 is ignored, and
    -- what the Endeavor reads is the two that are left -- the roller chose the 3,
    -- so "the other result" is the 5 and five Knights arrive. An engine that
    -- added the die without ignoring one leaves THREE results, where the card's
    -- "other result" is not one number and the slot stays unbound: no Knights at
    -- all.
    Spec.assertEqWith
      s
      "CR 706.6: the ignored 1 is not the other result, so the 5 is"
      (S.countOnBattlefieldByName knight S.alice (runEndeavor [1, 3, 5] 0 spell guarded))
      5
    -- The chosen result moves with the ignore too: the 3 is what the destruction
    -- judges, so power 4 dies and power 1 lives. Without the Guide the same
    -- script leaves the 1 chosen, and BOTH creatures die -- two boards apart.
    let guarded3 = runEndeavor [1, 3, 5] 0 spell guarded
    Spec.assertBool s (not (S.onBattlefield strong guarded3)) "power 4 is at least the chosen 3, so it is destroyed"
    Spec.assertBool s (S.onBattlefield weak guarded3) "power 1 is below it, so it survives"
    -- The paired board, one thing different: no Guide, so two dice and no ignore.
    Spec.assertEqWith
      s
      "CR 706.1: without the Guide the 1 is chosen and the 3 is the other"
      (S.countOnBattlefieldByName knight S.alice (runEndeavor [1, 3, 5] 0 spell board))
      3
  Spec.it s "CR 706.6 rolls tied for the lowest leave nothing to ask" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    guide <- S.printingOf s registry "Pixie Guide"
    let guarded = snd (S.addPermanent guide S.alice board)
        (offers, choices) = endeavorPrompts [4, 4, 4] spell guarded
    -- Three dice all showing 4: rule 706.6's second sentence gives the roller the
    -- tie-break, and every way of breaking it leaves the same two 4s -- so no
    -- board can tell the answers apart and the engine asks nothing. CR 706.4's
    -- choice among what is left is elided for its own reason, the two being equal.
    Spec.assertEqWith s "CR 706.1: three dice under the Guide" offers [6, 6, 6]
    Spec.assertEqWith s "nothing was asked to choose" choices []
    -- And exactly ONE of the tied rolls went: two 4s are left, so the other
    -- result is a 4 and four Knights arrive. An implementation that dropped every
    -- copy of the lowest leaves one result and mints none.
    Spec.assertEqWith
      s
      "CR 706.6: one of the tied 4s is ignored, so the other result is the other 4"
      (S.countOnBattlefieldByName knight S.alice (runEndeavor [4, 4, 4] 0 spell guarded))
      4
  Spec.it s "CR 614.5 a second Guide adds a second die and a second ignore" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    guide <- S.printingOf s registry "Pixie Guide"
    let one = snd (S.addPermanent guide S.alice board)
        two = snd (S.addPermanent guide S.alice one)
    -- THE GAMEPLAY ASSERTION: each row gets its own CR 614.5 opportunity, the
    -- second on the event the first produced, so four dice are thrown and the two
    -- lowest go -- leaving the 4 and the 6, the 4 chosen and the 6 counted. An
    -- engine that spent both rows on the original event throws three, and one
    -- that added two dice while ignoring one leaves three results and no "other".
    Spec.assertEqWith
      s
      "CR 614.5: four dice, the 1 and the 2 ignored, so the other result is the 6"
      (S.countOnBattlefieldByName knight S.alice (runEndeavor [1, 2, 4, 6] 0 spell two))
      6
    -- The paired board, one thing different: ONE Guide. Three dice, one ignore,
    -- so the 2 is chosen and the 4 is counted.
    Spec.assertEqWith
      s
      "CR 706.6: one Guide ignores only the 1"
      (S.countOnBattlefieldByName knight S.alice (runEndeavor [1, 2, 4, 6] 0 spell one))
      4
    -- Supporting: the dice really were thrown, four of them.
    Spec.assertEqWith s "CR 706.1: four dice under two Guides" (fst (endeavorPrompts [1, 2, 4, 6] spell two)) [6, 6, 6, 6]

-- The Dragon's combat under an answerer that hands out the given faces IN ORDER
-- to CR 706.1's rolls and RECORDS what each roll prompt offered. `offeredSides`
-- above one monad up, and for its reason: the roll happens in the combat damage
-- step rather than the step the fixture starts in.
--
-- Twenty for a roll the script did not plan, which is the die's TOP face: a
-- surplus die then shows up as the largest number on the board rather than as CR
-- 706.1a's floor, which is also what Replay.defaultAnswer would have supplied.
guideCombat :: [Natural.Natural] -> GameState.GameState -> (GameState.GameState, [Natural.Natural])
guideCombat rolls board =
  let answering :: Prompt.Prompt r -> State.State ([Natural.Natural], [Natural.Natural]) r
      answering p = case p of
        Prompt.RollDie sides -> do
          (pending, offers) <- State.get
          case pending of
            face : rest -> do
              State.put (rest, sides : offers)
              pure face
            [] -> do
              State.put ([], sides : offers)
              pure 20
        _ -> pure (S.attackTo S.bob p)
      go n gs =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result gs) || not (S.inCombatPhase (GameState.phase gs))
          then pure gs
          else do
            (_, next) <- Engine.runGame answering gs Engine.runStep
            go (n - 1) next
      (settled, (_, seen)) = State.runState (go (24 :: Int) board) (rolls, [])
   in (settled, reverse seen)
