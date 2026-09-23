{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 706 ROLLING A DIE -- Pawl.Types.RollDie, Effect.RollDie's arm in
-- Pawl.Engine.Resolve, and the Pawl.Types.Prompt / Pawl.Types.Response pair the
-- roll is externalised through. The transcript legs live in Pawl.ReplaySpec
-- with the other randomness prompts.
--
-- EIGHT FIXTURES. Ancient Copper Dragon ("Flying /
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
-- CR 706.2b's first step is the SIXTH fixture, Clam-I-Am, at the bottom of this
-- file -- a reroll offered to the roller by a permanent the instruction knows
-- nothing about. CR 706.2a's cost on such a modifier is the SEVENTH, Wall of
-- Fortune, below it -- the only printing whose reroll charges anything and the
-- only one reaching a roll its own controller did not make.
-- CR 706.2b's second step is the EIGHTH, Night Shift of the Living Dead, at the
-- very bottom -- a modifier from another source that increases or decreases the
-- result, with a life cost and a once-each-turn budget.
-- Left out: no "Roll again" (#2124), and no reading that takes the results as a
-- set (#3243). CR 706.1's roll does record its event, but the trigger
-- reading it lives in Pawl.EventTriggerSpec beside the other condition cases.
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

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RollAdjustment as RollAdjustment
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  rollDieSpec s registry
  resultsTableSpec s registry
  modifierSpec s registry
  severalDiceSpec s registry
  dieRollRSpec s registry
  rerollSpec s registry
  costedRerollSpec s registry
  activatedRerollSpec s registry
  nightShiftSpec s registry
  deckSpec s registry

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
        -- this assertion green. Windseer's own instruction prints no modifier;
        -- the IncreaseOrDecrease group's "a result shifted past the die's top
        -- face" case proves the exact form under Night Shift of the Living
        -- Dead's 21, and the single-endpoint form is proved on Diviner's Portent
        -- below. What the assertion DOES prove is that 20 selects this striation
        -- and not the 10-19 band above it.
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

-- CR 706.2 / 706.2b's first step: Clam-I-Am's "if you roll a 3 on a six-sided
-- die, you may reroll that die". The Pixie Guide group just above one clause of
-- rule 706 over, and the same shape -- the Clam is the ONE thing that differs
-- between the paired boards, so every reading that moves is the modifier's
-- doing.
--
-- VALIANT ENDEAVOR is the fixture, because the Clam names a SIX-sided die and
-- the Endeavor is the only d6 in data\/cards\/. Its "other result" is what the
-- assertions read: the die the reroll touched is the one the roller does NOT
-- choose, so the Knight count IS the number the second throw produced.
--
-- ONE SCRIPT ACROSS THE PAIR -- 3, then 6, then 2, in that order -- rather than a
-- script per board. Under the Clam the first die's 3 is rerolled into the 6 and
-- the second die is the 2; without it the 3 and the 6 are the two dice and the 2
-- is never reached. So the boards differ in the Clam alone, and an engine that
-- offered no reroll reads the same numbers in a different order.
--
-- THE ANCIENT COPPER DRAGON's d20 is the negative leg, and it has to be a
-- different card: rule 706.1a makes the die's size the whole description of a
-- die, and a d20 that comes up 3 is the board on which the Clam's "six-sided"
-- narrowing is visible.
rerollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rerollSpec s registry = Spec.describe s "Reroll" $ do
  Spec.it s "CR 706.2b a reroll replaces the natural result" $ do
    (spell, weak, strong, board) <- endeavorBoard s registry
    clam <- S.printingOf s registry "Clam-I-Am"
    let clammed = snd (S.addPermanent clam S.alice board)
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: the first die's 3 was thrown away and came back a 6, the roller
    -- chose the second die's 2, so "the other result" is the 6 and six Knights
    -- arrive. An engine that never offered the reroll reads the 3 and the 6 as
    -- its two dice and mints three.
    Spec.assertEqWith
      s
      "CR 706.2b: the rerolled die's 6 is the other result"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell clammed))
      6
    -- The paired board, one thing different: no Clam. The SAME script, and the 2
    -- is never reached.
    Spec.assertEqWith
      s
      "CR 706.1: without the Clam the 3 stands and the 6 is the second die"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell board))
      3
    -- The chosen result moves with the reroll too, read as a bound rather than a
    -- count: under the Clam the roller chose a 2, so power 4 dies; without it
    -- they chose the 6 and nothing does.
    let clammed2 = runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell clammed
    Spec.assertBool s (not (S.onBattlefield strong clammed2)) "power 4 is at least the chosen 2, so it is destroyed"
    Spec.assertBool s (S.onBattlefield weak clammed2) "power 1 is below it, so it survives"
  Spec.it s "CR 706.2a the reroll is the roller's to decline" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    clam <- S.printingOf s registry "Clam-I-Am"
    let clammed = snd (S.addPermanent clam S.alice board)
    -- The same board and the same script as the case above, one thing different:
    -- the roller says no. CR 706.2a makes the modifier optional, so declining
    -- leaves the natural 3 standing and the 6 is the second die -- the reading an
    -- engine that applied the modifier unasked cannot produce.
    Spec.assertEqWith
      s
      "CR 706.2a: a declined reroll leaves the natural result"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Declines] 1 spell clammed))
      3
  Spec.it s "CR 706.2a two free offers to the same player are one question" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    clam <- S.printingOf s registry "Clam-I-Am"
    let clammed = snd (S.addPermanent clam S.alice (snd (S.addPermanent clam S.alice board)))
    -- Two Clams state two modifiers over the same 3, and neither states a cost,
    -- so the two offers are the same question put to the same player: either
    -- accepted throws the same die, and no board can tell which Clam was taken.
    -- Where the rules leave nothing to ask, don't prompt -- so ONE offer is
    -- raised and a decline is a decline of both. The costed case is the opposite
    -- reading and has its own group below: a stated cost is something the payer
    -- can tell apart.
    Spec.assertEqWith
      s
      "CR 706.2a: two Clams raise one offer"
      (snd (rerollPrompts [3, 6, 2] [OptionalDecision.Declines] spell clammed))
      [3]
    -- Supporting, and in its own assertion: the one offer really is live, so the
    -- reading above is an elision rather than a modifier that never applied.
    Spec.assertEqWith
      s
      "CR 706.2b: accepting that one offer rerolls the die"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell clammed))
      6
  Spec.it s "CR 706.2 a rerolled die that repeats the number is offered again" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    clam <- S.printingOf s registry "Clam-I-Am"
    let clammed = snd (S.addPermanent clam S.alice board)
    -- The Clam's gate is on the NATURAL result, and a reroll produces one: the
    -- first die comes up 3, the reroll comes up 3 again, and the third throw is
    -- the 5 the instruction keeps. An engine that offered the reroll once per die
    -- stops at the second 3 and mints three Knights.
    Spec.assertEqWith
      s
      "CR 706.2: a second 3 is a third throw, and the 5 is the other result"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 3, 5, 2] [OptionalDecision.Exercises, OptionalDecision.Exercises] 1 spell clammed))
      5
    -- Supporting, and in its own assertion: the offer really was raised twice,
    -- and both times over a 3.
    Spec.assertEqWith
      s
      "CR 706.2b: both offers carried the natural 3"
      (snd (rerollPrompts [3, 3, 5, 2] [OptionalDecision.Exercises, OptionalDecision.Exercises] spell clammed))
      [3, 3]
  Spec.it s "CR 706.2 the offer is gated on the number the card names" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    clam <- S.printingOf s registry "Clam-I-Am"
    let clammed = snd (S.addPermanent clam S.alice board)
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: two d6 come up 2 and 5, neither of them the 3 the Clam names, so
    -- the roller chooses the 2 and the 5 is the other result. Every reroll on
    -- offer is ACCEPTED in this script, so an engine whose gate is wider in
    -- EITHER direction takes the 1 waiting behind them: one that matches every
    -- number rerolls the 2 and mints six, and one that matches 3 and up rerolls
    -- the 5 and mints one.
    Spec.assertEqWith
      s
      "CR 706.2: neither die shows the Clam's 3, so both stand"
      (S.countOnBattlefieldByName knight S.alice (runReroll [2, 5, 1] [OptionalDecision.Exercises, OptionalDecision.Exercises] 0 spell clammed))
      5
    -- Supporting, and in its own assertion: two dice were thrown and nothing was
    -- offered, which separates a gate that matched from a reroll the roller
    -- happened to decline.
    let (offers, naturals) = rerollPrompts [2, 5, 1] [OptionalDecision.Exercises, OptionalDecision.Exercises] spell clammed
    Spec.assertEqWith s "CR 706.1: two d6 were thrown" offers [6, 6]
    Spec.assertEqWith s "and no reroll was offered" naturals []
  Spec.it s "CR 706.1a the offer is gated on the die the card names" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    clam <- S.printingOf s registry "Clam-I-Am"
    let (bare, _, _) = S.combatBoardOf [dragon] []
        clammed = snd (S.addPermanent clam S.alice bare)
    -- THE GAMEPLAY ASSERTION: a d20 that came up 3 is not the Clam's six-sided
    -- die, so the 3 stands and the Dragon mints three Treasures. An engine that
    -- matched on the number alone rerolls into the 20 the script supplies next
    -- and mints twenty.
    Spec.assertEqWith
      s
      "CR 706.1a: a d20's 3 is not a six-sided die's 3"
      (S.countOnBattlefieldByName treasure S.alice (fst (clamCombat [3, 20] clammed)))
      3

-- Answers all three questions one Endeavor under a CR 706.2 reroll asks: each
-- die comes up the next number of `rolls`, each reroll offer takes the next
-- answer of `decisions`, and the roller chooses the result at `index`. Shared by
-- the Clam-I-Am group above and the Wall of Fortune group below -- neither the
-- modifier nor its payer is anything this answerer reads.
--
-- STATEFUL for endeavorAnswer's reason, which the reroll sharpens: the first
-- die's two throws are the same Prompt.RollDie question, so a pure answerer
-- cannot tell a die from its own reroll.
--
-- Six for a roll the script did not plan and Declines for an offer it did not
-- plan: a surplus throw shows up as the die's top face rather than as CR
-- 706.1a's floor, and a surplus offer stops rather than looping.
rerollAnswer :: Natural.Natural -> Prompt.Prompt r -> State.State ([Natural.Natural], [OptionalDecision.OptionalDecision]) r
rerollAnswer index p = case p of
  Prompt.RollDie _ -> do
    (rolls, decisions) <- State.get
    case rolls of
      h : t -> do
        State.put (t, decisions)
        pure h
      [] -> pure 6
  Prompt.RerollDie {} -> do
    (rolls, decisions) <- State.get
    case decisions of
      h : t -> do
        State.put (rolls, t)
        pure h
      [] -> pure OptionalDecision.Declines
  Prompt.ChooseDieResult {} -> pure index
  _ -> pure (S.identityAnswer p)

-- Cast the Endeavor and resolve it, one run under one answerer.
runReroll :: [Natural.Natural] -> [OptionalDecision.OptionalDecision] -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
runReroll rolls decisions index spell board =
  snd (State.evalState (Engine.runGame (rerollAnswer index) board (S.cast S.alice spell >> Stack.resolveTop)) (rolls, decisions))

-- The same cast under an answerer that RECORDS what each roll prompt offered and
-- what natural result each reroll offer carried, neither being readable off the
-- board.
rerollPrompts :: [Natural.Natural] -> [OptionalDecision.OptionalDecision] -> ObjectId.ObjectId -> GameState.GameState -> ([Natural.Natural], [Natural.Natural])
rerollPrompts rolls decisions spell board =
  let logging :: Prompt.Prompt r -> State.State ([Natural.Natural], [Natural.Natural], ([Natural.Natural], [OptionalDecision.OptionalDecision])) r
      logging p = case p of
        Prompt.RollDie sides -> do
          (seen, asked, scripted) <- State.get
          let (answer, next) = State.runState (rerollAnswer 0 p) scripted
          State.put (sides : seen, asked, next)
          pure answer
        Prompt.RerollDie _ _ natural _ -> do
          (seen, asked, scripted) <- State.get
          let (answer, next) = State.runState (rerollAnswer 0 p) scripted
          State.put (seen, natural : asked, next)
          pure answer
        _ -> do
          scripted <- fmap (\(_, _, x) -> x) State.get
          let (answer, next) = State.runState (rerollAnswer 0 p) scripted
          State.modify' (\(seen, asked, _) -> (seen, asked, next))
          pure answer
      (offers, naturals, _) = State.execState (Engine.runGame logging board (S.cast S.alice spell >> Stack.resolveTop)) ([], [], (rolls, decisions))
   in (reverse offers, reverse naturals)

-- The Dragon's combat under the Clam, guideCombat's shape and for its reason:
-- the roll happens in the combat damage step rather than the step the fixture
-- starts in. Every reroll offer is ACCEPTED, so a board that wrongly raised one
-- takes the script's next number rather than quietly declining back to the same
-- reading.
clamCombat :: [Natural.Natural] -> GameState.GameState -> (GameState.GameState, [Natural.Natural])
clamCombat rolls board =
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
        Prompt.RerollDie {} -> pure OptionalDecision.Exercises
        _ -> pure (S.attackTo S.bob p)
      go n gs =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result gs) || not (S.inCombatPhase (GameState.phase gs))
          then pure gs
          else do
            (_, next) <- Engine.runGame answering gs Engine.runStep
            go (n - 1) next
      (settled, (_, seen)) = State.runState (go (24 :: Int) board) (rolls, [])
   in (settled, reverse seen)

-- CR 706.2a's OTHER half: "Modifiers may be optional and\/or have associated
-- costs." Wall of Fortune ("Defender \/ You may tap an untapped Wall you control
-- to have any player reroll a die that player rolled") is the whole producer --
-- the only printing whose reroll charges anything, and the only one whose
-- modifier reaches a roll the modifier's own controller did not make.
--
-- THE SAME ENDEAVOR FIXTURE and the same 3-6-2 script as the Clam group above,
-- deliberately: the Wall states no die size and no natural result, so the two
-- modifiers differ in the COST alone and the Clam's numbers carry over. Six
-- Knights means the reroll happened, three means it did not.
--
-- THE COST PROVES ITSELF by tapping the only Wall on the board. The Wall is a
-- Wall, so it pays for itself; having paid it is tapped, and its own filter no
-- longer admits anything -- which is why ONE Exercises is the whole script even
-- though the Wall gates on no number and would otherwise be offered over every
-- throw. An engine that charged nothing offers the second throw's 6 a reroll
-- too.
--
-- THREE BOARDS differing in one thing: no Wall, an untapped Wall, and a Wall
-- already tapped. The third is the cost's own leg -- the modifier is in force
-- and matches the die, and only CR 118.3 -- no paying a cost without the
-- resources to pay it fully -- keeps the reroll off the board.
--
-- THE SEAT is the fourth case and needs the second player: BOB's Wall over
-- ALICE's roll. Rule 109.5 puts the "may" and the payment on the Wall's
-- controller, so bob is asked and bob's Wall taps, and the die alice rolled is
-- the one thrown again.
costedRerollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
costedRerollSpec s registry = Spec.describe s "Costed reroll" $ do
  Spec.it s "CR 706.2a a reroll that carries a cost" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    wall <- S.printingOf s registry "Wall of Fortune"
    let (walled, withWall) = S.addPermanent wall S.alice board
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: the first die's 3 was thrown away for the price of one tap and
    -- came back a 6, the roller chose the second die's 2, so "the other result"
    -- is the 6 and six Knights arrive.
    Spec.assertEqWith
      s
      "CR 706.2a: the paid-for reroll's 6 is the other result"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell withWall))
      6
    -- The paired board, one thing different: no Wall. The SAME script, and the 2
    -- is never reached.
    Spec.assertEqWith
      s
      "CR 706.1: without the Wall the 3 stands and the 6 is the second die"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell board))
      3
    -- Supporting, and in its own assertion: the cost was actually charged. A
    -- board that rerolled for free leaves the Wall untapped.
    Spec.assertBool
      s
      (Game.isTapped walled (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell withWall))
      "CR 706.2a: the Wall paid for the reroll and is tapped"
    Spec.assertBool
      s
      (not (Game.isTapped walled (runReroll [3, 6, 2] [OptionalDecision.Declines] 1 spell withWall)))
      "and a declined offer charges nothing"
  Spec.it s "CR 706.2a an unpayable cost is not offered" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    wall <- S.printingOf s registry "Wall of Fortune"
    let (walled, withWall) = S.addPermanent wall S.alice board
        spent = S.tapObject walled withWall
    -- THE GAMEPLAY ASSERTION: the Wall is the only Wall and it is already
    -- tapped, so "an untapped Wall you control" admits nothing and the modifier
    -- -- in force, and matching a die it narrows in no way -- charges a cost
    -- nobody can pay. The 3 stands. Every Exercises in this script is accepted,
    -- so an engine that offered the reroll anyway mints six.
    Spec.assertEqWith
      s
      "CR 706.2a: no untapped Wall, so no offer"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises, OptionalDecision.Exercises] 1 spell spent))
      3
    -- Supporting, and in its own assertion: the offer was never raised at all,
    -- which separates a gate that refused from a reroll the answerer declined.
    Spec.assertEqWith
      s
      "and no reroll was offered"
      (snd (rerollPrompts [3, 6, 2] [OptionalDecision.Exercises, OptionalDecision.Exercises] spell spent))
      []
  Spec.it s "CR 109.5 the offer and its cost belong to the modifier's controller" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    wall <- S.printingOf s registry "Wall of Fortune"
    let (walled, withWall) = S.addPermanent wall S.bob board
    -- THE GAMEPLAY ASSERTION: the Wall is BOB's and the roll is ALICE's, which
    -- is the seat Clam-I-Am's "you" cannot show. Rule 706.2a's cost is bob's to
    -- pay, so bob's Wall taps, and the die alice rolled comes back a 6.
    Spec.assertEqWith
      s
      "CR 706.2: an opponent's Wall rerolls alice's die"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell withWall))
      6
    Spec.assertBool
      s
      (Game.isTapped walled (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell withWall))
      "CR 109.5: bob's Wall is what paid"
    -- Supporting, and in its own assertion: WHO was asked, which neither count
    -- above can show -- both read the same had the engine put the offer to the
    -- roller and charged bob's Wall anyway.
    Spec.assertEqWith
      s
      "CR 109.5: bob is the seat the offer was put to"
      (rerollSeats [3, 6, 2] [OptionalDecision.Exercises] spell withWall)
      [S.bob]
  Spec.it s "CR 706.2b a reroll is a roll by the player who throws it" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    wall <- S.printingOf s registry "Wall of Fortune"
    let (_, withWall) = S.addPermanent wall S.bob board
        rolls p after = length (filter (== GameEvent.DiceRolled p) (S.eventsOf after))
    -- Pippa, Duchess of Dice's ruling makes a reroll trigger "whenever you roll
    -- a die". Bob's Wall has ALICE reroll, so alice has rolled twice -- the
    -- instruction's roll and the reroll -- and bob not at all. The paired run
    -- declines, and leaves the one roll.
    Spec.assertEqWith s "the reroll is alice's second roll" (rolls S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell withWall)) 2
    Spec.assertEqWith s "and bob, who paid, did not roll" (rolls S.bob (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell withWall)) 0
    Spec.assertEqWith s "a declined reroll is no roll" (rolls S.alice (runReroll [3, 6, 2] [OptionalDecision.Declines] 1 spell withWall)) 1
  Spec.it s "CR 706.2a each costed modifier is its own offer" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    wall <- S.printingOf s registry "Wall of Fortune"
    let (first_, withFirst) = S.addPermanent wall S.alice board
        (second_, withBoth) = S.addPermanent wall S.alice withFirst
        after = runReroll [3, 6, 2] [OptionalDecision.Declines, OptionalDecision.Exercises] 1 spell withBoth
    -- THE GAMEPLAY ASSERTION: two Walls state two modifiers, each with its own
    -- cost to pay, so declining the first leaves the second still to be offered
    -- -- and the reroll taken off it produces the same 6. An engine that read
    -- the first decline as the answer for every modifier in force leaves the 3
    -- standing and mints three.
    Spec.assertEqWith
      s
      "CR 706.2a: the second Wall's offer still stands after the first is declined"
      (S.countOnBattlefieldByName knight S.alice after)
      6
    -- Supporting, and in its own assertion: ONE Wall paid. Rule 706.2b applies
    -- one modifier to a roll, not both.
    Spec.assertEqWith
      s
      "CR 706.2b: exactly one Wall was tapped"
      (length (filter (\oid -> Game.isTapped oid after) [first_, second_]))
      1
    -- And in its own assertion again: the offers really were two, over the same
    -- natural 3, rather than one offer asked twice by a loop.
    Spec.assertEqWith
      s
      "CR 706.2a: both Walls offered over the natural 3"
      (take 2 (snd (rerollPrompts [3, 6, 2] [OptionalDecision.Declines, OptionalDecision.Exercises] spell withBoth)))
      [3, 3]

-- The same cast under an answerer that records WHICH player each reroll offer
-- was put to. rerollPrompts' shape, and separate from it because the seat is a
-- different question from the natural result: the Clam group reads the number
-- and this one reads the player.
rerollSeats :: [Natural.Natural] -> [OptionalDecision.OptionalDecision] -> ObjectId.ObjectId -> GameState.GameState -> [PlayerId.PlayerId]
rerollSeats rolls decisions spell board =
  let logging :: Prompt.Prompt r -> State.State ([PlayerId.PlayerId], ([Natural.Natural], [OptionalDecision.OptionalDecision])) r
      logging p = do
        (asked, scripted) <- State.get
        let (answer, next) = State.runState (rerollAnswer 0 p) scripted
        State.put (case p of Prompt.RerollDie _ pid _ _ -> pid : asked; _ -> asked, next)
        pure answer
      (seats, _) = State.execState (Engine.runGame logging board (S.cast S.alice spell >> Stack.resolveTop)) ([], (rolls, decisions))
   in reverse seats

-- Goblin Bookie's "{R}, {T}: Reflip any coin or reroll any die. (Activate only
-- any time it makes sense.)", read as a window inside CR 706.2's modification
-- step where the ability is activated and resolves at once. The Endeavor
-- fixture and the 3-6-2 script of the Wall group above: six Knights means the
-- reroll happened, three that it did not.
--
-- BOB's Bookie over ALICE's roll, since "any die" reaches another player's,
-- and a Mountain beside it for the {R}. The paired boards each take away one
-- thing an ACTIVATED ability needs and a static modifier would not: the mana
-- (CR 602.2b), and a creature's settle before paying {T} (CR 302.6).
--
-- Not transcribed: the "reflip any coin" half (#4017). Stricter than printed:
-- bob can reflip nothing.
activatedRerollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
activatedRerollSpec s registry = Spec.describe s "Activated reroll" $ do
  Spec.it s "CR 706.2b Goblin Bookie rerolls another player's die" $ do
    (spell, _, _, board, bookie, mountain) <- bookieBoard s registry
    let after = runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell board
    -- THE GAMEPLAY ASSERTION: bob activated the Bookie inside alice's roll and
    -- the 3 came back a 6.
    Spec.assertEqWith
      s
      "CR 706.2b: the Bookie's reroll turns the 3 into a 6"
      (S.countOnBattlefieldByName knight S.alice after)
      6
    Spec.assertBool s (Game.isTapped bookie after && Game.isTapped mountain after) "CR 602.2b: bob paid {R} and {T}"
    Spec.assertEqWith s "CR 109.5: bob is the seat the offer was put to" (rerollSeats [3, 6, 2] [OptionalDecision.Exercises] spell board) [S.bob]
    -- Pippa, Duchess of Dice's ruling: the reroll is the rerolling player's roll.
    Spec.assertEqWith
      s
      "a reroll of alice's die is bob's roll"
      (length (filter (== GameEvent.DiceRolled S.bob) (S.eventsOf after)))
      1
    -- The paired run: the same board with the offer declined.
    Spec.assertEqWith
      s
      "and a declined offer leaves the 3"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Declines] 1 spell board))
      3
  Spec.it s "CR 602.2b the window asks what a priority activation asks" $ do
    (spell, _, _, board, bookie, mountain) <- bookieBoard s registry
    let unpaid = S.tapObject mountain board
        sick = board {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) bookie (GameState.objects board)}
    -- THE GAMEPLAY ASSERTIONS: every Exercises in the script is accepted, so an
    -- engine that offered the Bookie anyway mints six. The sick Bookie is the
    -- gate's proof; the tapped Mountain would also be refused by the payment.
    Spec.assertEqWith
      s
      "CR 118.3: no {R} to pay, so no reroll"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell unpaid))
      3
    Spec.assertEqWith
      s
      "CR 302.6: a summoning-sick Bookie cannot pay {T}"
      (S.countOnBattlefieldByName knight S.alice (runReroll [3, 6, 2] [OptionalDecision.Exercises] 1 spell sick))
      3
    Spec.assertEqWith s "and neither was offered" (rerollSeats [3, 6, 2] [OptionalDecision.Exercises] spell unpaid <> rerollSeats [3, 6, 2] [OptionalDecision.Exercises] spell sick) []
  Spec.it s "CR 117.1b the Bookie cannot be activated at priority" $ do
    (_, _, _, board, bookie, _) <- bookieBoard s registry
    -- No die is being rolled, so it never makes sense; with the mana and the
    -- untapped Bookie that the first case paid with.
    Spec.assertBool
      s
      (not (any (\ability -> Activatable.activatable S.bob bookie ability board) (Activatable.abilitiesFor bookie board)))
      "the Bookie's ability is not activatable outside a roll"
    Spec.assertBool s (not (null (Activatable.abilitiesFor bookie board))) "and the Bookie has the ability"

-- The Endeavor board plus bob's Goblin Bookie and a Mountain to pay its {R}.
bookieBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
bookieBoard s registry = do
  (spell, weak, strong, board) <- endeavorBoard s registry
  bookiePrinting <- S.printingOf s registry "Goblin Bookie"
  mountainPrinting <- S.printingOf s registry "Mountain"
  let (bookie, withBookie) = S.addPermanent bookiePrinting S.bob board
      (mountain, withBoth) = S.addPermanent mountainPrinting S.bob withBookie
  pure (spell, weak, strong, withBoth, bookie, mountain)

-- CR 706.2's third sentence, "Modifiers may also come from other sources", in
-- rule 706.2b's SECOND bucket: Night Shift of the Living Dead ("After you roll a
-- die, you may pay 1 life. If you do, increase or decrease the result by 1. Do
-- this only once each turn. / Whenever you roll a 6, create a 2/2 black Zombie
-- Employee creature token."). Its first ability is the modifier, with CR
-- 706.2a's optional cost; its second reads the RESULT, which CR 706.2's last
-- sentence makes the number after every modifier -- so a 5 shifted up mints a
-- Zombie and a 6 shifted down does not.
--
-- THE SAME ENDEAVOR FIXTURE as the reroll groups above: two d6, the roller
-- picks the destroying result and "the other result" counts Knights. Two
-- readings, then: the Knights say which number the unchosen die ended on, and
-- the Zombie Employee tokens say whether a 6 was among the final results.
--
-- CR 603.3 is why nightShiftRun drains the stack after the Endeavor resolves:
-- "whenever you roll a 6" triggers during that resolution and reaches the stack
-- only at the next placement.
nightShiftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nightShiftSpec s registry = Spec.describe s "IncreaseOrDecrease" $ do
  Spec.it s "CR 706.2 an increase from another source is part of the result" $ do
    (spell, weak, strong, board) <- endeavorBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let shifted = snd (S.addPermanent shift S.alice board)
        (after, shown) = nightShiftRun [5, 2] [] [Just (0, RollAdjustment.Increase)] 1 spell shifted
    -- THE GAMEPLAY ASSERTIONS, first so nothing ahead of them can absorb a
    -- mutation: the first die's 5 was pushed to 6, so the other result is six
    -- Knights and "whenever you roll a 6" mints a Zombie Employee. An engine that
    -- never offered the modifier reads 5 and mints no Zombie.
    Spec.assertEqWith s "CR 706.2: the shifted 6 is the other result" (S.countOnBattlefieldByName knight S.alice after) 6
    Spec.assertEqWith s "CR 706.2: the result after the modifier is a 6, so the Zombie arrives" (S.countOnBattlefieldByName zombieEmployee S.alice after) 1
    Spec.assertEqWith s "CR 706.2a: the modifier cost 1 life" (S.lifeOf S.alice after) (Just 19)
    Spec.assertBool s (not (S.onBattlefield strong after)) "the chosen 2 still destroys power 4"
    Spec.assertBool s (S.onBattlefield weak after) "and power 1 survives it"
    -- Supporting: both results were shown before the choice (the printed
    -- ruling), and only once.
    Spec.assertEqWith s "CR 706.2b: one offer, showing both dice" shown [[5, 2]]
    -- The paired board, one thing different: no Night Shift.
    let (bare, _) = nightShiftRun [5, 2] [] [Just (0, RollAdjustment.Increase)] 1 spell board
    Spec.assertEqWith s "CR 706.1: without it the 5 stands" (S.countOnBattlefieldByName knight S.alice bare) 5
  Spec.it s "CR 706.2a the modifier is the roller's to decline" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let shifted = snd (S.addPermanent shift S.alice board)
        (after, _) = nightShiftRun [5, 2] [] [Nothing] 1 spell shifted
    Spec.assertEqWith s "CR 706.2a: a declined modifier leaves the 5" (S.countOnBattlefieldByName knight S.alice after) 5
    Spec.assertEqWith s "and no 6 was rolled" (S.countOnBattlefieldByName zombieEmployee S.alice after) 0
    Spec.assertEqWith s "and no life was paid" (S.lifeOf S.alice after) (Just 20)
  Spec.it s "CR 706.2 a decrease moves a 6 off the number the trigger reads" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let shifted = snd (S.addPermanent shift S.alice board)
        (lowered, _) = nightShiftRun [6, 2] [] [Just (0, RollAdjustment.Decrease)] 1 spell shifted
        (kept, _) = nightShiftRun [6, 2] [] [Nothing] 1 spell shifted
    -- The natural 6 is not what "whenever you roll a 6" reads: the result is.
    Spec.assertEqWith s "CR 706.2: a natural 6 decreased to 5 mints no Zombie" (S.countOnBattlefieldByName zombieEmployee S.alice lowered) 0
    Spec.assertEqWith s "and the other result is the 5" (S.countOnBattlefieldByName knight S.alice lowered) 5
    Spec.assertEqWith s "CR 706.2: the same 6 left alone mints one" (S.countOnBattlefieldByName zombieEmployee S.alice kept) 1
  Spec.it s "CR 706.2 the roller picks which die to shift" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let shifted = snd (S.addPermanent shift S.alice board)
        (after, _) = nightShiftRun [2, 5] [] [Just (1, RollAdjustment.Increase)] 0 spell shifted
    -- The SECOND die is named, so the 5 becomes the 6; an engine that shifted
    -- the first die whatever the answer makes the 2 a 3 and mints five Knights.
    Spec.assertEqWith s "CR 706.2: the second die's 6 is the other result" (S.countOnBattlefieldByName knight S.alice after) 6
    Spec.assertEqWith s "and it is a 6 for the trigger" (S.countOnBattlefieldByName zombieEmployee S.alice after) 1
  Spec.it s "CR 706.2a the modifier is taken only once each turn" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    endeavor <- S.printingOf s registry "Valiant Endeavor"
    plains <- S.printingOf s registry "Plains"
    -- Six more Plains, so the second Endeavor is paid for as the first was.
    let mana = List.foldl' (\gs _ -> snd (S.addPermanent plains S.alice gs)) (snd (S.addPermanent shift S.alice board)) [1 .. 6 :: Int]
        (twice, second) = S.handOne endeavor mana
        answers = [Just (0 :: Natural.Natural, RollAdjustment.Increase), Just (0, RollAdjustment.Increase)]
        (once, shownOnce) = nightShiftRun [5, 2] [] answers 1 spell twice
        -- The second Endeavor rolls 5 and 3 and destroys with the 3, so the
        -- first cast's 2/2 Knights and Zombie survive it.
        (sameTurn, shownSame) = nightShiftRun [5, 3] [] answers 1 second once
        (nextTurn, shownNext) = nightShiftRun [5, 3] [] answers 1 second (Engine.beginTurnOf S.alice (Engine.beginTurnOf S.bob once))
    -- THE GAMEPLAY ASSERTION: the budget is spent, so the second roll's 5
    -- stands: five more Knights on top of the first six, and still one Zombie.
    Spec.assertEqWith s "CR 706.2a: the same turn's second roll is not shifted" (S.countOnBattlefieldByName knight S.alice sameTurn) 11
    Spec.assertEqWith s "and mints no second Zombie" (S.countOnBattlefieldByName zombieEmployee S.alice sameTurn) 1
    Spec.assertEqWith s "the first roll was offered" shownOnce [[5, 2]]
    Spec.assertEqWith s "and the second was not" shownSame []
    -- The next turn's roll has its budget back: its shifted 6 mints a second
    -- Zombie, and the offer was raised.
    Spec.assertEqWith s "a new turn's shifted 6 mints a second Zombie" (S.countOnBattlefieldByName zombieEmployee S.alice nextTurn) 2
    Spec.assertEqWith s "a new turn offers it again" shownNext [[5, 3]]
  -- CR 706.3a's single-number striation against a result the die cannot
  -- show: Djinni Windseer's "20 | Scry 3." under a natural 20 shifted to 21. The
  -- table's `20` is an exact number, so 21 fires nothing and alice's top card
  -- is the first; a `20+` reading scries 3. The decrease leg is the pair: 19 is
  -- in 10-19 and scries 2.
  Spec.it s "CR 706.3a a result shifted past the die's top face fires no single-number striation" $ do
    (ids, board) <- tableBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    let shifted = snd (S.addPermanent shift S.alice board)
        shiftAnswer :: RollAdjustment.RollAdjustment -> Prompt.Prompt r -> r
        shiftAnswer direction p = case p of
          Prompt.AdjustDieRoll {} -> Just (0, direction)
          _ -> tableAnswer 20 p
        run direction = S.runPure (shiftAnswer direction) (S.runPure (shiftAnswer direction) shifted Engine.placePendingTriggers) Stack.resolveTop
    case ids of
      [first, _, third, _, _, _] -> do
        Spec.assertEqWith s "CR 706.3a: 21 is not 20, so nothing is scried" (Maybe.listToMaybe (tableLibrary (run RollAdjustment.Increase))) (Just first)
        Spec.assertEqWith s "CR 706.3a: 19 is in 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (run RollAdjustment.Decrease))) (Just third)
      _ -> Spec.assertFailure s "the fixture library is six cards"
  -- CR 706.2b's two steps in order, on the pair of producers: Clam-I-Am's
  -- reroll ("If you roll a 3 on a six-sided die, you may reroll that die") is
  -- the first step, Night Shift's shift the second. Every reroll offer in this
  -- script is ACCEPTED.
  Spec.it s "CR 706.2b rerolls come before increases and decreases" $ do
    (spell, _, _, board) <- endeavorBoard s registry
    shift <- S.printingOf s registry "Night Shift of the Living Dead"
    clam <- S.printingOf s registry "Clam-I-Am"
    let both = snd (S.addPermanent clam S.alice (snd (S.addPermanent shift S.alice board)))
        -- A natural 2 shifted UP to 3 lands on the Clam's number after step
        -- one is over, so it is NOT rerolled: the other result is the 3. An
        -- engine that ran the rerolls again after the shift throws the 1
        -- waiting in the script and mints one Knight.
        (shiftedOnto, _) = nightShiftRun [2, 5, 1] [OptionalDecision.Exercises] [Just (0, RollAdjustment.Increase)] 1 spell both
        -- A natural 3 IS rerolled, before the shift is offered: the shift sees
        -- the rerolled 1, not the 3.
        (_, shownAfterReroll) = nightShiftRun [3, 1, 5] [OptionalDecision.Exercises] [Nothing] 1 spell both
    Spec.assertEqWith s "CR 706.2b: a 3 made by the shift is not rerolled" (S.countOnBattlefieldByName knight S.alice shiftedOnto) 3
    Spec.assertEqWith s "CR 706.2b: the shift is offered over the rerolled die" shownAfterReroll [[1, 5]]

zombieEmployee :: CardName.CardName
zombieEmployee = CardName.MkCardName (Text.pack "Zombie Employee Token")

-- Answers every question one Endeavor under Night Shift asks, rerollAnswer's
-- script extended by the shift: each die comes up the next of `rolls`, each
-- reroll offer takes the next of `rerolls`, each shift offer the next of
-- `shifts`, and the roller chooses the result at `index`. Records the results
-- each shift offer SHOWED, which the board cannot say.
--
-- Declines for an unplanned shift offer, and six for an unplanned throw,
-- rerollAnswer's reasons.
nightShiftAnswer :: Natural.Natural -> Prompt.Prompt r -> State.State ([Natural.Natural], [OptionalDecision.OptionalDecision], [Maybe (Natural.Natural, RollAdjustment.RollAdjustment)], [[Integer]]) r
nightShiftAnswer index p = case p of
  Prompt.RollDie _ -> do
    (rolls, rerolls, shifts, shown) <- State.get
    case rolls of
      h : t -> State.put (t, rerolls, shifts, shown) >> pure h
      [] -> pure 6
  Prompt.RerollDie {} -> do
    (rolls, rerolls, shifts, shown) <- State.get
    case rerolls of
      h : t -> State.put (rolls, t, shifts, shown) >> pure h
      [] -> pure OptionalDecision.Declines
  Prompt.AdjustDieRoll _ _ results _ _ -> do
    (rolls, rerolls, shifts, shown) <- State.get
    case shifts of
      h : t -> State.put (rolls, rerolls, t, NonEmpty.toList results : shown) >> pure h
      [] -> State.put (rolls, rerolls, [], NonEmpty.toList results : shown) >> pure Nothing
  Prompt.ChooseDieResult {} -> pure index
  _ -> pure (S.identityAnswer p)

-- Cast the Endeavor, resolve it, then place and drain whatever triggered (CR
-- 603.3) under the same script. Returns the board and the results each shift
-- offer showed, in order.
nightShiftRun :: [Natural.Natural] -> [OptionalDecision.OptionalDecision] -> [Maybe (Natural.Natural, RollAdjustment.RollAdjustment)] -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [[Integer]])
nightShiftRun rolls rerolls shifts index spell board =
  let drain :: Int -> Game.Type.Game ()
      drain n = do
        gs <- State.get
        Monad.unless (n <= 0 || null (GameState.stack gs)) (Stack.resolveTop >> drain (n - 1))
      script = S.cast S.alice spell >> Stack.resolveTop >> Engine.placePendingTriggers >> drain 8
      ((_, after), (_, _, _, shown)) = State.runState (Engine.runGame (nightShiftAnswer index) board script) (rolls, rerolls, shifts, [])
   in (after, reverse shown)

-- CR 706.2's last sentence against a NEGATIVE instruction modifier: The Deck of
-- Many Things ("{2}, {T}: Roll a d20 and subtract the number of cards in your
-- hand. If the result is 0 or less, discard your hand. / 1-9 | Return a card at
-- random from your graveyard to your hand. / 10-19 | Draw two cards.") under
-- Night Shift of the Living Dead. The result is the number after EVERY
-- modifier, so a natural 3 with four cards in hand, shifted up, is 3 - 4 + 1 =
-- 0: the hand is discarded and no striation fires. An engine that clamped the
-- instruction's sum before the shift reads max 0 (-1) + 1 = 1, keeps the hand
-- and returns the graveyard card on top of it.
--
-- Not implemented: the Deck's 20 striation, which the card file leaves out
-- (#4015). Stricter than printed, and no roll here reaches 20.
deckSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
deckSpec s registry = Spec.describe s "UnclampedShift" $ do
  Spec.it s "CR 706.2 a shift applies to the unclamped sum" $ do
    zeroed <- deckBoard s registry 4
    paired <- deckBoard s registry 2
    -- THE GAMEPLAY ASSERTION: four cards in hand, 3 - 4 + 1 = 0, so the hand is
    -- discarded and nothing comes back from the graveyard.
    Spec.assertEqWith s "CR 706.2: 3 - 4 + 1 is 0, so the hand is discarded" (length (Game.zoneMembers Zone.Hand S.alice (runDeck zeroed))) 0
    Spec.assertEqWith s "CR 706.2a: the shift was paid for" (S.lifeOf S.alice (runDeck zeroed)) (Just 19)
    -- The paired board, one thing different: two cards in hand, so 3 - 2 + 1 =
    -- 2 lands in 1-9 and the graveyard card joins the two kept.
    Spec.assertEqWith s "CR 706.3a: 3 - 2 + 1 is 2, so a card returns" (length (Game.zoneMembers Zone.Hand S.alice (runDeck paired))) 3

-- The Deck and Night Shift under alice, two Plains to pay {2}, `held` cards
-- in hand and one in the graveyard.
deckBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m (ObjectId.ObjectId, [ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)], GameState.GameState)
deckBoard s registry held = do
  deck <- S.printingOf s registry "The Deck of Many Things"
  shift <- S.printingOf s registry "Night Shift of the Living Dead"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let (deckId, withDeck) = S.addPermanent deck S.alice (S.landsInPlay plains 2)
      shifted = snd (S.addPermanent shift S.alice withDeck)
      stocked = snd (S.addGraveyardCard piker S.alice shifted)
      dealt = List.foldl' (\gs _ -> snd (S.addHandCard piker S.alice gs)) stocked [1 .. held]
  pure (deckId, Face.activatedAbilities (S.combinedFace deck), dealt)

-- Activate the Deck and resolve it: the d20 comes up 3 and every shift offer
-- takes the first die up.
runDeck :: (ObjectId.ObjectId, [ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)], GameState.GameState) -> GameState.GameState
runDeck (deckId, abilities, board) =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.RollDie _ -> 3
        Prompt.AdjustDieRoll {} -> Just (0, RollAdjustment.Increase)
        _ -> S.identityAnswer p
   in S.runPure answer board (mapM_ (Activate.activateAbility S.alice deckId) (take 1 abilities) >> Stack.resolveTop)
