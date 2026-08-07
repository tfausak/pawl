module Pawl.Engine.Mulligan where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Numeric.Natural
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Decider as Decider
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HandActionIndex as HandActionIndex
import Pawl.Types.HandActionPerformer (HandActionPerformer)
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- CR 103.5: the starting hand size. Deliberately NOT shared with CR 402.2's
-- maximum hand size (PlayerEffect.defaultMaximumHandSize), a different seven
-- the rules keep apart.
openingHand :: Int
openingHand = 7

-- Ask the interpreter to shuffle this player's library (CR 103.3 / 701.24).
shuffleLibrary :: PlayerId -> Game ()
shuffleLibrary pid = do
  gs <- State.get
  let ids = Game.zoneMembers Zone.Library pid gs
  answer <- Game.ask (Prompt.Shuffle ids)
  let shuffled = Game.honourShuffle ids answer
  -- modify' rather than putting `gs` back: it was read before the prompt, and a
  -- prompt may write state -- Game.choose writes GameState.lastChoice. This one
  -- does not (Prompt.Shuffle is bare, being randomness rather than a choice),
  -- so this is defending the invariant rather than fixing a live bug.
  State.modify' (\g -> g {GameState.library = Map.insert pid (Seq.fromList shuffled) (GameState.library g)})

-- CR 103.5: draw opening hands, then run the declaration/mulligan/bottom round
-- loop to completion. Assumes each player's library is already built and
-- shuffled. The per-player mulligan count is a local Map (absent = 0) read only
-- by this loop, so it never enters GameState. `owners` is in turn order.
openingHands :: HandActionPerformer -> [PlayerId] -> Game ()
openingHands perform owners = do
  -- CR 103.5 sentence 1: every player draws a full opening hand. A short
  -- library sets drewFromEmpty here, and the flag survives the loop -- CR 727.3
  -- / 729.3.
  Monad.forM_ owners (Monad.replicateM_ openingHand . Event.drawCard)
  mulliganRounds perform Map.empty owners
  -- CR 103.6: the opening-hand window, after the WHOLE CR 103.5 process.
  openingHandActions perform owners

-- CR 103.5b / CR 103.6: every action the cards in this player's hand grant from
-- the window `field` names, keyed by the granting card AND which of that card's
-- actions it is, and paired with the effects that action performs. A card
-- printing two actions contributes two entries, in printed order -- nothing in
-- CR 103 caps the number, and the two are different offers. A CLASSIFICATION,
-- not an identity test: this asks whether the card declares an action, never
-- which card it is.
--
-- Read straight off the face (Game.faceOf) and never through the projection --
-- the Face.castingPermissions precedent: these abilities function in the HAND
-- (CR 113.6), which pawl's projection does not reach (#160).
actionsFor :: (Face.Face Card.Card -> [[Effect Card.Card]]) -> PlayerId -> GameState.GameState -> [((ObjectId, HandActionIndex.HandActionIndex), [Effect Card.Card])]
actionsFor field pid gs =
  let withActions oid = case Game.faceOf oid gs of
        Nothing -> []
        Just face -> zipWith (\i effects -> ((oid, HandActionIndex.MkHandActionIndex i), effects)) [0 ..] (field face)
   in concatMap withActions (Game.zoneMembers Zone.Hand pid gs)

-- The shared CR 103.5b / CR 103.6 loop: offer this player every action their
-- hand grants through `field`, on the `question` channel, until they decline or
-- none is left. Performing one is never a mulligan and never a cost. Both rules
-- let a player act more than once, which is why this recurses rather than asking
-- once.
--
-- Terminates even against an interpreter that never declines: every action in
-- the pool moves its card out of the hand (CR 103.6a onto the battlefield, CR
-- 103.5b's into exile), and a card leaving takes ALL of its entries with it, so
-- the candidate list strictly shrinks. An action that leaves its card in the
-- hand, and so can be taken again in the same window, is not supported (#801).
handWindow ::
  (Face.Face Card.Card -> [[Effect Card.Card]]) ->
  (Decider.Decider -> PlayerId -> [(ObjectId, HandActionIndex.HandActionIndex)] -> Prompt.Prompt (Maybe (ObjectId, HandActionIndex.HandActionIndex))) ->
  HandActionPerformer ->
  PlayerId ->
  Game ()
handWindow field question perform pid = do
  candidates <- State.gets (actionsFor field pid)
  case candidates of
    -- Where the rules leave nothing to ask, don't prompt.
    [] -> pure ()
    _ -> do
      decider <- State.gets (Decide.deciderFor pid)
      answer <- Game.choose (question decider pid (fmap fst candidates))
      case answer of
        Nothing -> pure ()
        -- The card AND the index, because a card granting two actions leaves the
        -- card alone ambiguous, and picking either one for the player would be
        -- the engine making a choice.
        Just key -> case lookup key candidates of
          -- A key that was not offered: validated by MEMBERSHIP, the
          -- Action.Activate posture, which keeps this total with no partial
          -- lookup and no way for an interpreter to conjure an action.
          Nothing -> pure ()
          Just effects -> do
            perform (fst key) pid effects
            handWindow field question perform pid

-- CR 103.6: the starting player acts first, then each other player in turn
-- order, which is exactly the order `owners` arrives in. A player who has left
-- the game is not here to act -- the rebuild paths derive `owners` from
-- Game.stillPlayingInOrder, so they get no window and no opening hand.
openingHandActions :: HandActionPerformer -> [PlayerId] -> Game ()
openingHandActions perform owners =
  Monad.forM_ owners (handWindow Face.openingHandActions Prompt.OpeningHandAction perform)

-- CR 103.5: repeat the declare-all-then-take-all round until no still-deciding
-- player mulligans. `deciding` is the players who have NOT yet kept -- keeping
-- is terminal, so a player who keeps drops out of the pool and is never asked
-- again.
mulliganRounds :: HandActionPerformer -> Map.Map PlayerId Numeric.Natural.Natural -> [PlayerId] -> Game ()
mulliganRounds perform counts deciding = do
  -- CR 103.5: the starting player declares first, then each other in turn order
  -- (turn order is preserved: `deciding` is filtered from the original
  -- `owners`). A player whose hand is already zero cannot mulligan (final
  -- sentence) and is treated as Keep without being asked -- which also drops
  -- them from the pool.
  decisions <- Monad.forM deciding $ \pid -> do
    -- CR 103.5b: the window comes FIRST -- the action is taken "at a time they
    -- would declare", and the declaration follows it. Reading the hand size
    -- after it is load-bearing: an action that empties the hand makes this a
    -- forced keep under CR 103.5's final sentence.
    handWindow Face.mulliganActions Prompt.MulliganAction perform pid
    handSize <- State.gets (length . Game.zoneMembers Zone.Hand pid)
    if handSize <= 0
      then pure (pid, MulliganDecision.Keep)
      else do
        decider <- State.gets (Decide.deciderFor pid)
        offer <- State.gets (offerFor counts pid)
        decision <- Game.choose (Prompt.DeclareMulligan decider pid offer)
        pure (pid, decision)
  let mulliganers = fmap fst (filter (\(_, d) -> d == MulliganDecision.Mulligan) decisions)
  case mulliganers of
    -- CR 103.5: a round with no mulligans ends the process; the remaining hands
    -- are opening hands.
    [] -> pure ()
    _ -> do
      -- CR 103.5 takes every mulligan simultaneously. pawl is sequential;
      -- because a hand is hidden information, applying them in turn order is
      -- observably equivalent.
      counts' <- Monad.foldM takeMulligan counts mulliganers
      -- Kept players have dropped out; only this round's mulliganers decide
      -- again.
      mulliganRounds perform counts' mulliganers

-- CR 103.5c / CR 800.6: how many of a player's mulligans are free -- do not
-- count toward the cards bottomed or the mulligans allowed.
--
-- CR 800.1: a multiplayer game BEGINS with more than two players, rather than
-- currently having them. GameState.turnOrder is the permanent seating roster,
-- so counting seats answers that directly and a game that drops to two players
-- by departure keeps its free mulligan. A rebuilt game (CR 727.1, CR 729.2) is
-- seated from the players who were in the game it came from, so it answers for
-- itself.
--
-- A Natural rather than a Bool, because the caller subtracts this. CR 103.5c
-- grants the same allowance to any Brawl game (CR 903.12g), which pawl has no
-- way to know it is playing (#174), so the seat count is the only cause today;
-- a format layer redefines this function rather than chasing call sites.
freeMulligans :: GameState.GameState -> Numeric.Natural.Natural
freeMulligans gs = if length (GameState.turnOrder gs) > 2 then 1 else 0

-- CR 103.5 / 103.5c: what this player is told at their declaration -- the raw
-- number of mulligans they have taken, and how many cards taking one more would
-- put on the bottom of their library.
--
-- THE one place that turns a raw count into a cost. Both readers go through it:
-- Prompt.DeclareMulligan reports it, and takeMulligan bottoms by it, so what a
-- player is promised and what the mulligan actually costs cannot drift (#176).
offerFor :: Map.Map PlayerId Numeric.Natural.Natural -> PlayerId -> GameState.GameState -> MulliganOffer.MulliganOffer
offerFor counts pid gs =
  let taken = Map.findWithDefault 0 pid counts
      free = freeMulligans gs
      count = taken + 1
   in MulliganOffer.MkMulliganOffer
        { MulliganOffer.taken = taken,
          -- CR 103.5c: the free mulligans do not count. Natural subtraction is
          -- partial below zero, so the comparison is explicit rather than
          -- clamped after the fact.
          MulliganOffer.bottomCount = if count > free then count - free else 0
        }

-- CR 103.5: one player takes a mulligan -- shuffle the hand back, redraw a full
-- hand, then bottom one card per mulligan taken, in the player's chosen order,
-- less the free allowance (CR 103.5c). The RAW count is what goes back into the
-- map and what Prompt.DeclareMulligan reports, since it is what CR 103.5c
-- subtracts FROM.
takeMulligan :: Map.Map PlayerId Numeric.Natural.Natural -> PlayerId -> Game (Map.Map PlayerId Numeric.Natural.Natural)
takeMulligan counts pid = do
  handIds <- State.gets (Game.zoneMembers Zone.Hand pid)
  Monad.forM_ handIds (\oid -> Event.changeZone oid Zone.Library)
  shuffleLibrary pid
  Monad.replicateM_ openingHand (Event.drawCard pid)
  -- The SAME offer the declaration prompt reported -- offerFor is a pure
  -- function of `counts`, `pid` and the seat roster, none of which the redraw
  -- touches -- so this bottoms exactly what the player was promised.
  offer <- State.gets (offerFor counts pid)
  let count = MulliganOffer.taken offer + 1
      counted = MulliganOffer.bottomCount offer
  newHand <- State.gets (Game.zoneMembers Zone.Hand pid)
  let n = min counted (Natural.length newHand)
  bottomChosen <-
    if n > 0 && length newHand >= 2
      then do
        decider <- State.gets (Decide.deciderFor pid)
        answer <- Game.choose (Prompt.Bottom decider pid newHand n)
        -- CR 103.5: the cards bottomed come from THIS hand, and there are
        -- exactly `n` of them. Filtered, not trusted (#222); a short answer is
        -- topped up from the front of the hand. nub, not just filter: an answer
        -- naming one card twice would bottom it twice, and the second
        -- changeZone is a no-op on an id that has already moved, so the hand
        -- would end up one card too big rather than visibly wrong.
        let kept = List.genericTake n (List.nub (filter (\oid -> List.elem oid newHand) answer))
            topUp = List.genericTake (n - Natural.length kept) (filter (\oid -> List.notElem oid kept) newHand)
        pure (kept <> topUp)
      else -- CR 103.5: with nothing to bottom (a free mulligan, CR 103.5c) or a
      -- hand of 0 or 1, there is exactly one possible outcome; where the rules
      -- leave nothing to ask, don't prompt -- bottom whatever `n` names.
        pure (List.genericTake n newHand)
  Monad.forM_ bottomChosen (\oid -> Event.changeZone oid Zone.Library)
  pure (Map.insert pid count counts)
