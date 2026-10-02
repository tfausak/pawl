{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 705 FLIPPING A COIN -- Pawl.Types.FlipCoin (both of its tallies),
-- Pawl.Types.CoinReading, Pawl.Engine.Coin, Pawl.Engine.Event.flipOneCoin and
-- the CR 614.1a replacement over it (Pawl.Types.CoinFlipR, Krark's Thumb),
-- Pawl.Types.StatedFlip, Effect.FlipCoin's arm in Pawl.Engine.Resolve, and the
-- Pawl.Types.Prompt / Pawl.Types.Response pairs the call and the flip are
-- externalised through. The transcript legs live in
-- Pawl.ReplaySpec with the other randomness prompts.
--
-- NOT the flip's GameEvent, which Pawl.EventTriggerSpec's PlayerWinsCoinFlip
-- group proves with Tavern Scoundrel: what a trigger sees is a rule 603 question
-- rather than a rule 705 one, and Winter Sky watches nothing.
--
-- NOT rule 705.2's first sentence as an ENTRY REPLACEMENT (Molten Sentry), which
-- is proved in Pawl.ReplacementSpec beside the rest of the CR 614.1c family. It
-- appears here twice: in the CR 705.3 group, because rule 705.3 is the one rule
-- both roads have to obey and Pawl.Engine.Event.flipOneCoin is the one road they
-- share, and
-- as an EFFECT in the face-reading group below (Odds), which is the same
-- sentence on the other road.
--
-- Its own module rather than a group in Pawl.DiceSpec: CR 705 and CR 706 are
-- different rules sharing no type, no prompt and no effect. A coin has no size,
-- no results table and no modifier; a die has no call and no winner.
--
-- THREE FIXTURES, one per group; the two below Winter Sky are documented where
-- they are built. Winter Sky ({R} Sorcery, "Flip a coin. If you win the flip,
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
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Coin" $ do
  flipCoinSpec s registry
  faceReadingSpec s registry
  missesSpec s registry

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
  let (_, gs1) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
      (_, gs2) = S.addPermanent piker S.bob gs1
      (_, gs3) = S.addPermanent maiden S.bob gs2
      (_, gs4) = S.addLibraryCard mountain S.alice gs3
      (_, gs5) = S.addLibraryCard mountain S.alice gs4
      (_, gs6) = S.addLibraryCard mountain S.bob gs5
      (_, gs7) = S.addLibraryCard mountain S.bob gs6
      -- handOne REPLACES alice's hand, so the spare card goes in after it.
      (gs8, skyId) = S.handOne sky (S.landsFor mountain S.alice 1 gs7)
      (_, gs9) = S.addHandCard mountain S.alice gs8
  pure (skyId, atLife S.bob 17 gs9)

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

flipCoinSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
flipCoinSpec s registry = Spec.describe s "FlipCoin" $ do
  Spec.it s "CR 705.2 only the flipping player calls, and calls before the coin comes up" $ do
    board <- coinBoard s registry
    -- Supporting, and in its own case so it cannot stand in for the four legs
    -- above: the call is asked once and of ALICE, CR 705.2's "only the player
    -- who flips the coin ... no other players are involved", and it is asked
    -- BEFORE the face, since calling with the face already known is a different
    -- game. Bob is never asked.
    Spec.assertEqWith s "the call, of alice, then the flip" (asked board) [Just S.alice, Nothing]

-- The `i`th element, or a fallback past the end. The fallbacks are the answers
-- that LOSE a flip -- a tails coin against a call of heads -- so a run that
-- flipped more coins than the case pinned cannot inflate the tally.
atIndex :: [CoinFace.CoinFace] -> CoinFace.CoinFace -> Int -> CoinFace.CoinFace
atIndex xs fallback i = Maybe.fromMaybe fallback (Maybe.listToMaybe (drop i xs))

-- CR 705.2's FIRST sentence as an effect rather than an entry replacement: Odds
-- (the left half of Odds // Ends, {U}{R} Instant, "Flip a coin. If it comes up
-- heads, counter target instant or sorcery spell. If it comes up tails, copy
-- that spell and you may choose new targets for the copy"; name, cost, type
-- line and Oracle text read off the card_faces array at api.scryfall.com
-- 2026-09-01). Its two branches read the FACE, and no player wins or loses.
--
-- Lightning Bolt is the spell it answers, so the two branches are three damage
-- apart in each direction: countered leaves bob at 20, copied takes him to 14.
-- Two seats are enough -- the copy keeps the Bolt's target, so no third seat has
-- a role.
--
-- Three lands, exactly both costs: two Mountains and an Island.
oddsBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
oddsBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  bolt <- S.printingOf s registry "Lightning Bolt"
  odds <- S.printingOf s registry "Odds"
  let lands = S.landsFor island S.alice 1 (S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers))
      (withBolt, boltId) = S.handOne bolt lands
      (oddsId, board) = S.addHandCard odds S.alice withBolt
  pure (boltId, oddsId, board)

-- Answer a ChooseTargets by FILTERING the offered set down to one recipient,
-- never by building one: CR 608.2b re-reads what was chosen, and a hand-built
-- Recipient.ToObject of the same permanent is a different recipient that the
-- re-read drops with no error.
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (== recipient) offered) sets
  _ -> S.identityAnswer p

-- alice casts Lightning Bolt at bob and then -- CR 117.3c, still holding
-- priority -- Odds at the Bolt, and the whole stack resolves under a coin pinned
-- to `face`.
afterOdds :: CoinFace.CoinFace -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
afterOdds face (boltId, oddsId, board) =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.FlipCoin -> face
        -- CR 707.10c's re-target prompt, which the copy raises because Odds says
        -- "you may choose new targets for the copy". Pinned back onto bob rather
        -- than left to the fallback, so the copy deals its damage where the Bolt
        -- would have.
        Prompt.ChooseTargets {} -> pinTarget (Recipient.ToPlayer S.bob) p
        _ -> S.identityAnswer p
      cast1 = S.runPure (pinTarget (Recipient.ToPlayer S.bob)) board (Cast.castSpell S.manaPerformer S.alice boltId boltName Facing.FaceUp)
      drain n g = if n <= (0 :: Int) || null (GameState.stack g) then g else drain (n - 1) (S.runPure answer g Stack.resolveTop)
   in case GameState.stack cast1 of
        [] -> cast1
        boltSpell : _ ->
          let cast2 = S.runPure (pinTarget (Recipient.ToObject boltSpell)) cast1 (Cast.castSpell S.manaPerformer S.alice oddsId oddsName Facing.FaceUp)
           in S.settleSba (drain 8 cast2)

-- How many CR 705.2 CALLS the whole run asked. Not readable off the board, so it
-- takes a State-counting answerer.
oddsCalls :: CoinFace.CoinFace -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState) -> Int
oddsCalls face (boltId, oddsId, board) =
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.CallCoin {} -> do
          State.modify' (+ 1)
          pure (S.identityAnswer p)
        Prompt.FlipCoin -> pure face
        _ -> pure (S.identityAnswer p)
      drain :: Int -> GameState.GameState -> State.State Int GameState.GameState
      drain n g =
        if n <= 0 || null (GameState.stack g)
          then pure g
          else do
            (_, next) <- Engine.runGame counting g Stack.resolveTop
            drain (n - 1) next
      cast1 = S.runPure (pinTarget (Recipient.ToPlayer S.bob)) board (Cast.castSpell S.manaPerformer S.alice boltId boltName Facing.FaceUp)
   in case GameState.stack cast1 of
        [] -> 0
        boltSpell : _ ->
          let cast2 = S.runPure (pinTarget (Recipient.ToObject boltSpell)) cast1 (Cast.castSpell S.manaPerformer S.alice oddsId oddsName Facing.FaceUp)
           in State.execState (drain 8 cast2) 0

boltName :: CardName.CardName
boltName = CardName.MkCardName (Text.pack "Lightning Bolt")

oddsName :: CardName.CardName
oddsName = CardName.MkCardName (Text.pack "Odds")

faceReadingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
faceReadingSpec s registry = Spec.describe s "FlipCoin read for its face (CR 705.2)" $ do
  Spec.it s "CR 705.2 an effect that cares only about the face reads the face" $ do
    board <- oddsBoard s registry
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation, and on the TAILS leg, which is the one leg the win/lose reading
    -- cannot reach the same answer on: Replay.defaultAnswer calls and flips
    -- heads, so a flip read as a WIN would counter the Bolt on both legs.
    Spec.assertEqWith
      s
      "CR 705.2: tails copies the Bolt, so bob takes it twice"
      (S.lifeOf S.bob (afterOdds CoinFace.Tails board))
      (Just 14)
    -- The pair, one thing different: the same board and the same answers with
    -- the coin the other way up.
    Spec.assertEqWith
      s
      "CR 705.2: heads counters the Bolt, so bob takes nothing"
      (S.lifeOf S.bob (afterOdds CoinFace.Heads board))
      (Just 20)
  Spec.it s "CR 705.2 no call is made for a flip nobody wins" $ do
    board <- oddsBoard s registry
    -- "No player wins or loses a coin flip for this kind of effect", so there is
    -- nothing to call and nothing for a CR 723 controller to usurp. Not readable
    -- off the board -- both legs above leave the same board whether or not a
    -- call was asked and thrown away.
    Spec.assertEqWith s "CR 705.2: Odds asks no call" (oddsCalls CoinFace.Tails board) 0

-- CR 705.2's OTHER tally: Mutalith Vortex Beast ({4}{U}{R} Creature -- Mutant
-- Beast 6/6, Trample, "Warp Vortex -- When this creature enters, flip a coin for
-- each opponent you have. For each flip you win, draw a card. For each flip you
-- lose, this creature deals 3 damage to that player."; Oracle text via
-- api.scryfall.com 2026-09-02). Its ruling ties each flip to one opponent
-- ("deals 3 damage to the appropriate player for each lost flip"), so the card
-- is a ForEach over the opponents with a one-coin flip in the body, reading the
-- flip's wins for the draw and its losses for the damage.
--
-- THREE SEATS, because the card ranges over opponents and two flips are what
-- separates the lost tally from the won one: with bob's flip lost and carol's
-- won, the losses reaching bob and NOT carol is a reading neither the wins
-- tally nor the coin count produces. Distinct life totals (20 and 16) so no
-- numeric coincidence can make the wrong seat read right. Two cards in alice's
-- library, since the won flip draws.
--
-- The Beast enters via S.entersWithTrigger rather than a cast: its six mana buy
-- nothing here, and the trigger placed off the hand-built enters event is the
-- same ability a cast would place.
mutalithBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
mutalithBoard s registry = do
  beast <- S.printingOf s registry "Mutalith Vortex Beast"
  maiden <- S.printingOf s registry "Bird Maiden"
  let stocked = snd (S.addLibraryCard maiden S.alice (snd (S.addLibraryCard maiden S.alice S.threePlayerGame)))
  pure (snd (S.entersWithTrigger beast S.alice (atLife S.carol 16 stocked)))

-- Every call is heads; the faces are pinned by index, in the order the loop
-- visits the opponents (APNAP from alice: bob, then carol). A pure answerer
-- cannot tell the two flips apart, so this one counts them.
mutalithAnswer :: [CoinFace.CoinFace] -> Prompt.Prompt r -> State.State Int r
mutalithAnswer faces p = case p of
  Prompt.CallCoin {} -> pure CoinFace.Heads
  Prompt.FlipCoin -> do
    i <- State.get
    State.put (i + 1)
    pure (atIndex faces CoinFace.Tails i)
  _ -> pure (S.identityAnswer p)

-- Place the enters trigger and resolve it under the pinned faces.
afterMutalith :: [CoinFace.CoinFace] -> GameState.GameState -> GameState.GameState
afterMutalith faces board =
  let run :: GameState.GameState -> Game.Type.Game a -> GameState.GameState
      run g game = snd (State.evalState (Engine.runGame (mutalithAnswer faces) g game) 0)
   in S.settleSba (run (run board Engine.placePendingTriggers) Stack.resolveTop)

missesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
missesSpec s registry = Spec.describe s "FlipCoin tallies the flips lost (CR 705.2)" $ do
  Spec.it s "CR 705.2 a lost flip reaches its loser and a won one does not" $ do
    board <- mutalithBoard s registry
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation. Bob's coin is tails against a call of heads (lost), carol's is
    -- heads (won): three damage to bob and none to carol. The losses bound to
    -- the WON tally would deal it to carol instead; bound to the coin COUNT it
    -- would deal it to both; left unbound it would reach nobody.
    Spec.assertEqWith
      s
      "CR 705.2: the lost flip deals 3 to bob and the won one deals nothing to carol"
      (let settled = afterMutalith [CoinFace.Tails, CoinFace.Heads] board in (S.lifeOf S.bob settled, S.lifeOf S.carol settled))
      (Just 17, Just 16)
    -- Supporting: the won flip drew its card and the lost one did not.
    Spec.assertEqWith
      s
      "CR 705.2: one flip won, so one card drawn"
      (S.handSize S.alice (afterMutalith [CoinFace.Tails, CoinFace.Heads] board))
      1
  Spec.it s "CR 705.2 each lost flip is paid once and none when none is lost" $ do
    board <- mutalithBoard s registry
    -- Both lost: both opponents take 3, and nothing is drawn.
    Spec.assertEqWith
      s
      "CR 705.2: two flips lost, so 3 damage to each opponent"
      (let settled = afterMutalith [CoinFace.Tails, CoinFace.Tails] board in (S.lifeOf S.bob settled, S.lifeOf S.carol settled, S.handSize S.alice settled))
      (Just 17, Just 13, 0)
    -- The pair, one thing different: both won, so nobody is damaged and two
    -- cards are drawn.
    Spec.assertEqWith
      s
      "CR 705.2: no flip lost, so no damage and two cards drawn"
      (let settled = afterMutalith [CoinFace.Heads, CoinFace.Heads] board in (S.lifeOf S.bob settled, S.lifeOf S.carol settled, S.handSize S.alice settled))
      (Just 20, Just 16, 2)
