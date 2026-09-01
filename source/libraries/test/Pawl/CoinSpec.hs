{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 705 FLIPPING A COIN -- Pawl.Types.FlipCoin, Effect.FlipCoin's arm
-- in Pawl.Engine.Resolve, and the Pawl.Types.Prompt / Pawl.Types.Response pairs
-- the call and the flip are externalised through. The transcript legs live in
-- Pawl.ReplaySpec with the other randomness prompts.
--
-- NOT the flip's GameEvent, which Pawl.EventTriggerSpec's PlayerWinsCoinFlip
-- group proves with Tavern Scoundrel: what a trigger sees is a rule 603 question
-- rather than a rule 705 one, and Winter Sky watches nothing.
--
-- NOT rule 705.2's FIRST sentence on its own -- the flip nobody wins, which is
-- an entry replacement rather than an effect (Molten Sentry), and is proved in
-- Pawl.ReplacementSpec beside the rest of the CR 614.1c family. It appears here
-- once, in the CR 705.3 group, because rule 705.3 is the one rule both roads
-- have to obey and Pawl.Engine.Coin is the one road they share.
--
-- Its own module rather than a group in Pawl.DiceSpec: CR 705 and CR 706 are
-- different rules sharing no type, no prompt and no effect. A coin has no size,
-- no results table and no modifier; a die has no call and no winner.
--
-- ONE FIXTURE. Winter Sky ({R} Sorcery, "Flip a coin. If you win the flip,
-- Winter Sky deals 1 damage to each creature and each player. If you lose the
-- flip, each player draws a card.") is CR 705.2's win/lose reading with both
-- branches spelled in opcodes that already existed, so the flip is the only new
-- thing the board can be reading.
--
-- FOUR LEGS, the whole truth table of (face, call): (Heads, Heads) and (Tails,
-- Tails) match and so win; (Heads, Tails) and (Tails, Heads) do not and so lose.
-- An implementation that reads only the FACE is red on (Heads, Tails); one that
-- reads only the CALL is red on (Tails, Heads); one that hard-codes a win is red
-- on both losing legs. (Heads, Tails) is the PRIMARY leg because
-- Replay.defaultAnswer answers Heads to both prompts, so a run that asked
-- neither one produces a WIN -- every S.identityAnswer descendant falls through
-- to that default silently, and the losing legs are the ones such a run cannot
-- reach.
--
-- THE ASSERTED QUANTITIES, per leg: alice's life, bob's life, alice's creature
-- count, bob's creature count, alice's hand size, bob's hand size, and the depth
-- of the stack. Each column earns its place by separating a pair of readings
-- that another column cannot:
--
--   * Life and creature counts separate a WIN from everything else.
--   * HAND SIZE is the only column that separates a LOST flip from a gate that
--     never held at all -- an unbound slot and a misspelled slot name both leave
--     life and creatures exactly where a loss leaves them.
--   * Bob's column beside alice's separates "each player" and "each creature"
--     from a sweep miswritten as the controller's own.
--   * The stack's depth keeps a Winter Sky that never resolved from passing as a
--     lost flip.
--
-- THE BOARD. Two seats: "each player" appears in both branches, so one seat
-- cannot tell it from "you". Not three -- nothing here ranges over opponents.
-- Distinct life totals (20 and 17) and distinct creature counts (one and two) so
-- no numeric coincidence can make a wrong seat read right.
--
-- Alice and bob each control a Goblin Piker (2/1), which 1 damage kills through
-- CR 704.5g, so the damage becomes a board count rather than a marker nothing
-- reads. Bob ALSO controls a Bird Maiden (1/2), which survives: that is what
-- keeps "deals 1 damage to each creature" from reading the same as a wipe.
--
-- Two cards in each library, so CR 104.3c never decks a seat and replaces a hand
-- size with a loss; one card in alice's hand and none in bob's, so the two hand
-- columns stay distinct in both legs.
module Pawl.CoinSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Coin" $ do
  flipCoinSpec s registry
  statedFlipSpec s registry

-- Set a seat's life directly, so the two seats start on different numbers and
-- neither can be read for the other.
atLife :: PlayerId.PlayerId -> Integer -> GameState.GameState -> GameState.GameState
atLife pid n gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}

-- Pins BOTH of CR 705.2's questions by CONSTANT rather than by anything derived
-- from the prompt, so the engine cannot repair the answer after a mutation, and
-- never by whatever Replay.defaultAnswer would supply unasked -- which is Heads
-- for both, and so a WIN.
flipAnswer :: CoinFace.CoinFace -> CoinFace.CoinFace -> Prompt.Prompt r -> r
flipAnswer face called p = case p of
  Prompt.FlipCoin -> face
  Prompt.CallCoin {} -> called
  _ -> S.identityAnswer p

-- Winter Sky in alice's hand with one untapped Mountain to pay for it, over the
-- board described at the top. CAST rather than planted on the stack: CR 601.2b's
-- mode selection happens as the spell is cast, and a hand-built stack object
-- carries no chosen mode and so resolves to nothing at all.
coinBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, GameState.GameState)
coinBoard s registry = do
  sky <- S.printingOf s registry "Winter Sky"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  let (_, gs1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      (_, gs2) = S.addCreature piker S.bob gs1
      (_, gs3) = S.addCreature maiden S.bob gs2
      (_, gs4) = S.addLibraryCard mountain S.alice gs3
      (_, gs5) = S.addLibraryCard mountain S.alice gs4
      (_, gs6) = S.addLibraryCard mountain S.bob gs5
      (_, gs7) = S.addLibraryCard mountain S.bob gs6
      -- handOne REPLACES alice's hand, so the spare card goes in after it.
      (gs8, skyId) = S.handOne sky (S.landsFor mountain S.alice 1 gs7)
      (_, gs9) = S.addHandCard mountain S.alice gs8
  pure (skyId, atLife S.bob 17 gs9)

-- Cast Winter Sky and resolve it under a pinned (face, call), then settle CR
-- 704.5g so the damage reads as a board count.
after :: CoinFace.CoinFace -> CoinFace.CoinFace -> (ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
after face called (skyId, board) =
  S.settleSba (S.runPure (flipAnswer face called) board (S.cast S.alice skyId >> Stack.resolveTop))

-- What resolution ASKED, in order: Nothing for CR 705.1's flip, which names no
-- seat, and Just the seat for CR 705.2's call. Not readable off the resulting
-- board, so it takes a State-logging answerer.
asked :: (ObjectId.ObjectId, GameState.GameState) -> [Maybe PlayerId.PlayerId]
asked (skyId, board) =
  let logging :: Prompt.Prompt r -> State.State [Maybe PlayerId.PlayerId] r
      logging p = case p of
        Prompt.FlipCoin -> do
          State.modify' (Nothing :)
          pure (flipAnswer CoinFace.Heads CoinFace.Tails p)
        Prompt.CallCoin _ pid -> do
          State.modify' (Just pid :)
          pure (flipAnswer CoinFace.Heads CoinFace.Tails p)
        _ -> pure (flipAnswer CoinFace.Heads CoinFace.Tails p)
      run = Engine.runGame logging board (S.cast S.alice skyId >> Stack.resolveTop)
   in reverse (State.execState run [])

-- Every gameplay-level column of one leg, in the order the header lists them.
reading :: GameState.GameState -> (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
reading gs =
  ( S.lifeOf S.alice gs,
    S.lifeOf S.bob gs,
    S.creaturesInPlay S.alice gs,
    S.creaturesInPlay S.bob gs,
    S.handSize S.alice gs,
    S.handSize S.bob gs,
    length (GameState.stack gs)
  )

-- A won flip: 1 damage to each player and each creature, so both Pikers die and
-- bob's Bird Maiden survives, and neither player draws.
won :: (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
won = (Just 19, Just 16, 0, 1, 1, 0, 0)

-- A lost flip: nothing is damaged and each player draws one, which is the only
-- reading in which the hand columns move.
lost :: (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
lost = (Just 20, Just 17, 1, 2, 2, 1, 0)

flipCoinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flipCoinSpec s registry = Spec.describe s "FlipCoin" $ do
  Spec.it s "CR 705.2 a call the coin does not match loses the flip" $ do
    board <- coinBoard s registry
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation, and on the one leg Replay.defaultAnswer cannot reach: the coin
    -- came up heads, the player called tails, so the call does not match and the
    -- flip is lost.
    Spec.assertEqWith
      s
      "CR 705.2: heads flipped against a call of tails is a lost flip"
      (reading (after CoinFace.Heads CoinFace.Tails board))
      lost
    -- The mirror, one thing different: the same mismatch the other way round.
    -- Falsifies an implementation that reads only the call.
    Spec.assertEqWith
      s
      "CR 705.2: tails flipped against a call of heads is a lost flip too"
      (reading (after CoinFace.Tails CoinFace.Heads board))
      lost
  Spec.it s "CR 705.2 a call the coin matches wins the flip" $ do
    board <- coinBoard s registry
    Spec.assertEqWith
      s
      "CR 705.2: heads flipped against a call of heads is a won flip"
      (reading (after CoinFace.Heads CoinFace.Heads board))
      won
    -- The other matching pair. Falsifies an implementation that reads only the
    -- face -- "heads wins" agrees with the line above and disagrees here.
    Spec.assertEqWith
      s
      "CR 705.2: tails flipped against a call of tails is a won flip too"
      (reading (after CoinFace.Tails CoinFace.Tails board))
      won
  Spec.it s "CR 705.2 only the flipping player calls, and calls before the coin comes up" $ do
    board <- coinBoard s registry
    -- Supporting, and in its own case so it cannot stand in for the four legs
    -- above: the call is asked once and of ALICE, CR 705.2's "only the player
    -- who flips the coin ... no other players are involved", and it is asked
    -- BEFORE the face, since calling with the face already known is a different
    -- game. Bob is never asked.
    Spec.assertEqWith s "the call, of alice, then the flip" (asked board) [Just S.alice, Nothing]

-- CR 705.3's producer is Edgar, King of Figaro ({4}{U}{U} Legendary Creature --
-- Human Artificer Noble 4/5, "Two-Headed Coin -- The first time you flip one or
-- more coins each turn, those coins come up heads and you win those flips";
-- name, cost, type line and Oracle text checked against api.scryfall.com
-- 2026-09-01). It states BOTH halves of the rule at once, which is why one card
-- can prove them together.
--
-- THE FIXTURE is the Winter Sky board above with one creature added under alice.
-- `withEdgar` decides WHICH creature, and that is the only difference between
-- the two boards: Edgar, or a Bird Maiden (1/2), which is the same shape for
-- every column `reading` looks at -- one more creature under alice that survives
-- 1 damage. A negative built by leaving the seat empty instead would differ in
-- two things.
--
-- `skies` is how many Winter Skys sit in alice's hand, which is what the "first
-- time each turn" narrowing needs: one flip cannot tell "the first" from "every".
statedBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> Int -> m ([ObjectId.ObjectId], GameState.GameState)
statedBoard s registry withEdgar skies = do
  sky <- S.printingOf s registry "Winter Sky"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  edgar <- S.printingOf s registry "Edgar, King of Figaro"
  let (_, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addCreature piker S.bob g1
      (_, g3) = S.addCreature maiden S.bob g2
      (_, g4) = S.addCreature (if withEdgar then edgar else maiden) S.alice g3
      -- Three cards apiece so a lost flip's draw never decks a seat (CR 104.3c)
      -- and replaces a hand size with a loss.
      stocked = repeatedly (snd . S.addLibraryCard mountain S.alice) 3 (repeatedly (snd . S.addLibraryCard mountain S.bob) 3 g4)
      landed = S.landsFor mountain S.alice (2 * skies + 2) stocked
      addSky (ids, g) = let (i, g') = S.addHandCard sky S.alice g in (i : ids, g')
      (skyIds, handed) = repeatedly addSky skies ([], landed)
   in pure (skyIds, atLife S.bob 17 handed)

-- Apply `f` `n` times.
repeatedly :: (a -> a) -> Int -> a -> a
repeatedly f n x = if n <= 0 then x else repeatedly f (n - 1) (f x)

-- Cast each Winter Sky in turn and resolve it, under the ONE pinned answer this
-- group uses: the coin comes up TAILS and alice calls HEADS. That pair loses on
-- its own -- it is the primary leg of the group above -- so every win below is
-- one rule 705.3 stated rather than one the coin produced, and neither the face
-- nor the call can be the thing the board is reading.
afterSkies :: [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
afterSkies skyIds board =
  S.settleSba (foldl (\g i -> S.runPure (flipAnswer CoinFace.Tails CoinFace.Heads) g (S.cast S.alice i >> Stack.resolveTop)) board skyIds)

-- One Winter Sky won: 1 damage to each player and each creature, so both Pikers
-- die, alice's extra creature and bob's Bird Maiden survive, and nobody draws.
wonWithHelper :: (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
wonWithHelper = (Just 19, Just 16, 1, 1, 0, 0, 0)

-- The same Winter Sky lost: nothing damaged, each player draws one.
lostWithHelper :: (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
lostWithHelper = (Just 20, Just 17, 2, 2, 1, 1, 0)

-- Two Winter Skys under Edgar: the FIRST flip is won and the second is not, so
-- the damage sentence runs once and the draw sentence runs once. An
-- implementation that ignored "the first time ... each turn" would win both,
-- which is 18 and 15 with bob's Bird Maiden dead to the second point of damage
-- and no draws at all -- no column of this reading survives that.
firstOnly :: (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
firstOnly = (Just 19, Just 16, 1, 1, 1, 1, 0)

-- alice's Molten Sentry ({3}{R} Creature -- Elemental */*, "As this creature
-- enters, flip a coin. If the coin comes up heads, this creature enters as a 5/2
-- creature with haste. If it comes up tails, this creature enters as a 2/5
-- creature with defender") in hand, over a Tavern Scoundrel ("Whenever you win a
-- coin flip, create two Treasure tokens") and one more creature under alice --
-- Edgar or the same Bird Maiden stand-in the Winter Sky boards use.
sentryBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, GameState.GameState)
sentryBoard s registry withEdgar = do
  mountain <- S.printingOf s registry "Mountain"
  scoundrel <- S.printingOf s registry "Tavern Scoundrel"
  sentry <- S.printingOf s registry "Molten Sentry"
  maiden <- S.printingOf s registry "Bird Maiden"
  edgar <- S.printingOf s registry "Edgar, King of Figaro"
  let (_, g1) = S.addCreature scoundrel S.alice (S.landsInPlay mountain 6)
      (_, g2) = S.addCreature (if withEdgar then edgar else maiden) S.alice g1
      (spellId, g3) = S.addHandCard sentry S.alice g2
   in pure
        ( spellId,
          g3
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        )

-- Cast the Sentry, resolve it, then run the place/resolve cycle twice so a flip
-- the Scoundrel watches has room to pay out -- CR 603.3 puts the trigger on the
-- stack at the next priority, and the flip happens inside the entry replacement.
-- The coin is pinned to TAILS, so a heads face below is one rule 705.3 stated.
runSentry :: GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
runSentry board spellId =
  let answer :: Prompt.Prompt r -> r
      answer = flipAnswer CoinFace.Tails CoinFace.Tails
      drain n g =
        if n <= (0 :: Int) || null (GameState.stack g)
          then g
          else drain (n - 1) (S.runPure answer g Stack.resolveTop)
      cycleOnce g = drain 8 (S.runPure answer g Engine.placePendingTriggers)
      resolved = S.runPure answer board (S.cast S.alice spellId >> Stack.resolveTop)
   in cycleOnce (cycleOnce resolved)

-- The newest battlefield object whose printed card has this name.
newestNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

statedFlipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
statedFlipSpec s registry = Spec.describe s "StateCoinFlip (CR 705.3)" $ do
  Spec.it s "CR 705.3 a stated win beats a call the coin did not match" $ do
    (withEdgar, edgarBoard) <- statedBoard s registry True 1
    (withHelper, plainBoard) <- statedBoard s registry False 1
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: tails against a call of heads is a LOST flip everywhere else in
    -- this module, and under Edgar it is won -- "this can cause a player to win
    -- a flip that couldn't otherwise be won".
    Spec.assertEqWith
      s
      "CR 705.3: Edgar states the win, so Winter Sky's damage sentence runs"
      (reading (afterSkies withEdgar edgarBoard))
      wonWithHelper
    -- The pair, one thing different: the same board with a vanilla creature in
    -- Edgar's seat, so every column starts where the line above started.
    Spec.assertEqWith
      s
      "CR 705.2: with nobody stating a result the same flip is lost"
      (reading (afterSkies withHelper plainBoard))
      lostWithHelper
  Spec.it s "CR 705.3 Edgar's statement is spent on the turn's first flip" $ do
    (skyIds, board) <- statedBoard s registry True 2
    Spec.assertEqWith
      s
      "the first flip is won and the second is not"
      (reading (afterSkies skyIds board))
      firstOnly
  Spec.it s "CR 705.3 reaches the flip CR 705.2 leaves winnerless" $ do
    (edgarSpell, edgarBoard) <- sentryBoard s registry True
    (plainSpell, plainBoard) <- sentryBoard s registry False
    let underEdgar = runSentry edgarBoard edgarSpell
        underHelper = runSentry plainBoard plainSpell
    -- Edgar's own ruling: its ability "can cause you to win coin flips that
    -- would ordinarily have no winner". The Scoundrel is the discrimination --
    -- two Treasures against none -- and it is asserted FIRST, because the P/T
    -- below would also move if only the stated FACE had landed.
    Spec.assertEqWith
      s
      "CR 705.3: Molten Sentry's winnerless flip is won, so the Scoundrel mints two Treasures"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Treasure Token")) S.alice underEdgar)
      2
    Spec.assertEqWith
      s
      "CR 705.2: with nobody stating a result that same flip has no winner"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Treasure Token")) S.alice underHelper)
      0
    -- The other half of the same statement, on the same two boards: the coin was
    -- pinned to TAILS, so a 5/2 with haste is the stated FACE and a 2/5 with
    -- defender is the one the coin actually came up.
    case (newestNamed (CardName.MkCardName (Text.pack "Molten Sentry")) underEdgar, newestNamed (CardName.MkCardName (Text.pack "Molten Sentry")) underHelper) of
      (Just stated, Just actual) -> do
        Spec.assertEqWith s "CR 705.3: the stated heads picks the 5/2" (S.powerToughnessOf stated underEdgar) (Just (5, 2))
        Spec.assertEqWith s "CR 705.1: the actual tails picks the 2/5" (S.powerToughnessOf actual underHelper) (Just (2, 5))
      _ -> Spec.assertFailure s "Molten Sentry did not reach the battlefield"
