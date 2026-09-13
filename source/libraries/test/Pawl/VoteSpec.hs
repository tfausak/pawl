{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- CR 701.38's vote: Pawl.Engine.Resolve.Effect's Effect.Vote arm, the
-- Pawl.Types.Vote payload it reads, Prompt.ChooseVote and Prompt.ChooseVoteWord,
-- Pawl.Engine.PlayerEffect's votesAllowed, and Pawl.Engine.Game's turnOrderFrom,
-- which rule 701.38a's "starting with a specified player and proceeding in turn
-- order" is the second reader of (CR 101.4 is the first).
--
-- The object vote's producer is Council's Judgment {1}{W}{W} Sorcery: "Will of
-- the council -- Starting with you, each player votes for a nonland permanent
-- you don't control. Exile each permanent with the most votes or tied for most
-- votes."
--
-- The word vote's is Plea for Power {3}{U} Sorcery: "Will of the council --
-- Starting with you, each player votes for time or knowledge. If time gets more
-- votes, take an extra turn after this one. If knowledge gets more votes or the
-- vote is tied, draw three cards." Rule 701.38d's extra vote is Brago's
-- Representative {2}{W} Creature -- Human Advisor: "While voting, you get an
-- additional vote."
--
-- THREE SEATS, because on two a tally cannot have a strict winner and a strict
-- loser at once, and the exile could follow the candidate list rather than the
-- count without the board telling the difference. Three seats also make three
-- ballots, which cannot TIE over two words -- so the tie half of Plea for
-- Power's second sentence is reachable only with Brago's Representative's fourth
-- ballot, which is why the two sit in one group.
module Pawl.VoteSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Vote" $ do
  councilsJudgmentSpec s registry
  pleaForPowerSpec s registry

-- Alice's Wall of Stone, a Mountain and a Goblin Piker under bob, Typhoid Rats
-- under carol, three Plains for alice to cast with, and Council's Judgment in her
-- hand. Returns the Wall, the Mountain, the Piker, the Rats, the spell and that
-- state.
--
-- The four permanents are placed first, so ObjectId order runs Wall, Mountain,
-- Piker, Rats -- and that is the order
-- Pawl.Engine.Resolve.Slots.battlefieldMatching offers the choices in, the lands
-- behind them matching nothing.
judgmentBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
judgmentBoard s registry = do
  wall <- S.printingOf s registry "Wall of Stone"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  plains <- S.printingOf s registry "Plains"
  judgment <- S.printingOf s registry "Council's Judgment"
  let (aWall, g1) = S.addPermanent wall S.alice S.threePlayerGame
      (bMountain, g2) = S.addPermanent mountain S.bob g1
      (bPiker, g3) = S.addPermanent piker S.bob g2
      (cRats, g4) = S.addPermanent rats S.carol g3
      (g5, spell) = S.handOne judgment (S.landsFor plains S.alice 3 g4)
  pure (aWall, bMountain, bPiker, cRats, spell, g5)

-- The card names in one player's copy of a zone. CR 400.7 mints a fresh
-- incarnation at the destination, so the exiled permanent is read by NAME rather
-- than by the ObjectId it had on the battlefield.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Answers Prompt.ChooseVote with the choice pinned for the SEAT the prompt names
-- and records, in the order they were asked, which seat was asked and what it was
-- offered. Everything else defers to S.identityAnswer.
--
-- Keyed by seat, and stateful rather than pure, because every ballot in one vote
-- is a structurally identical prompt: a pure answerer would give all three seats
-- the same answer and the tally could not discriminate.
votingFor ::
  [(PlayerId.PlayerId, ObjectId.ObjectId)] ->
  Prompt.Prompt r ->
  State.State [(PlayerId.PlayerId, [ObjectId.ObjectId])] r
votingFor pins p = case p of
  Prompt.ChooseVote _ pid _ offered -> do
    State.modify (<> [(pid, NonEmpty.toList offered)])
    pure $ case lookup pid pins of
      Just oid -> oid
      Nothing -> S.identityAnswer p
  _ -> pure (S.identityAnswer p)

councilsJudgmentSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
councilsJudgmentSpec s registry = Spec.describe s "Council's Judgment" $ do
  -- Two seats name bob's Piker and one names carol's Rats, so one resolution has
  -- a strict winner and a strict loser: the exile that takes the Piker has to
  -- leave the Rats alone, which is what says it followed the count rather than
  -- the candidate list.
  Spec.it s "CR 701.38a the permanent with the most votes is exiled and the rest are not" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    (aWall, bMountain, bPiker, cRats, spell, gs) <- judgmentBoard s registry
    let pins = [(S.alice, bPiker), (S.bob, cRats), (S.carol, bPiker)]
        ((_, after), asked) = State.runState (Engine.runGame (votingFor pins) gs (S.cast S.alice spell >> Stack.resolveTop)) []
    Spec.assertEqWith s "bob's Piker, whom alice and carol both voted for, is the one card in exile" (namesIn Zone.Exile S.bob after) [Just (S.printingName piker)]
    Spec.assertBool s (S.onBattlefield cRats after) "carol's Rats, with the one vote bob cast, is still on the battlefield"
    -- Rule 701.38a's ORDER and rule 701.38b's list of choices in one read: alice
    -- is the "specified player" the printed sentence names, the other two follow
    -- her in turn order, and all three are offered the same two permanents --
    -- alice's own Wall failing "you don't control", bob's Mountain failing
    -- "nonland", and neither leaving the battlefield.
    Spec.assertEqWith
      s
      "each seat votes once, starting with alice and proceeding in turn order, between the two nonland permanents alice does not control"
      asked
      [(S.alice, [bPiker, cRats]), (S.bob, [bPiker, cRats]), (S.carol, [bPiker, cRats])]
    Spec.assertBool s (S.onBattlefield aWall after && S.onBattlefield bMountain after) "neither permanent the filter excludes left the battlefield"

-- Alice's four Islands, Plea for Power in her hand, and three Mountains in her
-- library so the printed "draw three cards" has cards to draw (CR 104.3c).
-- `representatives` many Brago's Representatives under her control. Returns the
-- spell and that state.
pleaBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Int ->
  m (ObjectId.ObjectId, GameState.GameState)
pleaBoard s registry representatives = do
  island <- S.printingOf s registry "Island"
  mountain <- S.printingOf s registry "Mountain"
  plea <- S.printingOf s registry "Plea for Power"
  representative <- S.printingOf s registry "Brago's Representative"
  let g1 = S.landsFor island S.alice 4 S.threePlayerGame
      g2 = List.foldl' (\gs _ -> snd (S.addPermanent representative S.alice gs)) g1 [1 .. representatives]
      g3 = List.foldl' (\gs _ -> snd (S.addLibraryCard mountain S.alice gs)) g2 [1 .. 3 :: Int]
      (g4, spell) = S.handOne plea g3
  pure (spell, g4)

-- Answers Prompt.ChooseVoteWord from a list of ballots consumed in the order the
-- prompts are raised, and records which seat each prompt named and what it was
-- offered. Everything else defers to S.identityAnswer.
--
-- Consumed BY POSITION rather than keyed by seat, which is what lets one seat's
-- two ballots (CR 701.38d) go to different words -- a seat-keyed answerer would
-- give them both the same answer and the tie could not be built.
votingWords ::
  Prompt.Prompt r ->
  State.State ([SlotName.SlotName], [(PlayerId.PlayerId, [SlotName.SlotName])]) r
votingWords p = case p of
  Prompt.ChooseVoteWord _ pid _ offered -> do
    (pending, asked) <- State.get
    State.put (drop 1 pending, asked <> [(pid, NonEmpty.toList offered)])
    pure (case pending of chosen : _ -> chosen; [] -> S.identityAnswer p)
  _ -> pure (S.identityAnswer p)

word :: String -> SlotName.SlotName
word = SlotName.MkSlotName . Text.pack

-- How many cards are in alice's hand, and whom the extra turns queued are for:
-- the two observable halves of Plea for Power's two gated clauses.
outcome :: GameState.GameState -> (Int, [PlayerId.PlayerId])
outcome gs = (length (Game.zoneMembers Zone.Hand S.alice gs), fmap ExtraTurn.taker (GameState.extraTurns gs))

pleaForPowerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
pleaForPowerSpec s registry = Spec.describe s "Plea for Power" $ do
  -- Two knowledge ballots against one time ballot, so the printed "if knowledge
  -- gets more votes" holds and "if time gets more votes" does not. The draw is
  -- what says a clause read the WORD's tally: nothing else on this board tells
  -- the two sentences apart.
  Spec.it s "CR 701.38b a clause gated on a word's tally happens and the clause gated on the other does not" $ do
    (spell, gs) <- pleaBoard s registry 0
    let ballots = [word "time", word "knowledge", word "knowledge"]
        ((_, after), (_, asked)) = State.runState (Engine.runGame votingWords gs (S.cast S.alice spell >> Stack.resolveTop)) (ballots, [])
    Spec.assertEqWith s "knowledge won, so alice drew three cards and took no extra turn" (outcome after) (3, [])
    -- Rule 701.38a's order and rule 701.38b's list in one read: alice is the
    -- "specified player", the other two follow her in turn order, and all three
    -- are offered the same two words in printed order.
    Spec.assertEqWith
      s
      "each seat votes once, starting with alice and proceeding in turn order, between the two printed words"
      asked
      [(S.alice, [word "time", word "knowledge"]), (S.bob, [word "time", word "knowledge"]), (S.carol, [word "time", word "knowledge"])]
  -- The same board with the ballots swapped, so the OTHER clause happens. A pair
  -- differing in exactly one thing: without it a green first case could mean the
  -- draw clause is ungated.
  Spec.it s "CR 701.38b the other word winning runs the other clause instead" $ do
    (spell, gs) <- pleaBoard s registry 0
    let ballots = [word "time", word "time", word "knowledge"]
        ((_, after), _) = State.runState (Engine.runGame votingWords gs (S.cast S.alice spell >> Stack.resolveTop)) (ballots, [])
    Spec.assertEqWith s "time won, so alice took an extra turn and drew nothing" (outcome after) (0, [S.alice])
  -- CR 701.38d's extra vote makes FOUR ballots, which is the only way two words
  -- tie at this table. Alice's two go to different words, which Brago's
  -- Representative's reminder text allows, so the tally is two all and the "or
  -- the vote is tied" is what admits the draw.
  Spec.it s "CR 701.38d an additional vote is a second ballot for that seat, taken before the next seat votes" $ do
    (spell, gs) <- pleaBoard s registry 1
    let ballots = [word "time", word "knowledge", word "time", word "knowledge"]
        ((_, after), (_, asked)) = State.runState (Engine.runGame votingWords gs (S.cast S.alice spell >> Stack.resolveTop)) (ballots, [])
    Spec.assertEqWith s "the vote is two all, so alice drew three cards and took no extra turn" (outcome after) (3, [])
    Spec.assertEqWith
      s
      "alice is asked twice in a row and the other two seats once each"
      (fmap fst asked)
      [S.alice, S.alice, S.bob, S.carol]
