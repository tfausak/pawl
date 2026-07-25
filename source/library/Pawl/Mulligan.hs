module Pawl.Mulligan where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Numeric.Natural
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Type.Card as Card
import Pawl.Type.Effect (Effect)
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import Pawl.Type.MulliganPerformer (MulliganPerformer)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Zone as Zone

-- CR 103.5: the starting hand size, "normally seven." Deliberately NOT shared
-- with CR 402.2's maximum hand size (PlayerEffect.defaultMaximumHandSize), which
-- is a different seven the rules keep apart.
openingHand :: Int
openingHand = 7

-- Ask the interpreter to shuffle this player's library (CR 103.3 / 701.24).
shuffleLibrary :: PlayerId -> Game ()
shuffleLibrary pid = do
  gs <- State.get
  let ids = Game.zoneMembers Zone.Library pid gs
  shuffled <- Trans.lift (Program.prompt (Prompt.Shuffle ids))
  State.put gs {GameState.library = Map.insert pid (Seq.fromList shuffled) (GameState.library gs)}

-- CR 103.5: draw opening hands, then run the declaration/mulligan/bottom round
-- loop to completion. Assumes each player's library is already built and
-- shuffled. The per-player mulligan count is a local Map (absent = 0): the only
-- reader is this loop, which needs it for the number of cards bottomed (CR 103.5)
-- less the free allowance (CR 103.5c), so it never enters GameState. `owners` is
-- in turn order (starting player first).
openingHands :: MulliganPerformer -> [PlayerId] -> Game ()
openingHands perform owners = do
  -- CR 103.5 sentence 1: every player draws a full opening hand. A short library
  -- sets drewFromEmpty here; the flag survives the loop, which is what CR 727.3 /
  -- 729.3's "regardless of any mulligans" means.
  Monad.forM_ owners (Monad.replicateM_ openingHand . Event.drawCard)
  mulliganRounds perform Map.empty owners

-- CR 103.5b: the cards in this player's hand that grant an action they may take
-- at their mulligan declaration, each paired with the effects that action
-- performs. A CLASSIFICATION, not an identity test: this asks whether the card
-- declares an action, never which card it is.
--
-- Read straight off the card (Game.cardOf) and never through the projection --
-- the Card.castingPermissions precedent: the ability functions in the HAND (CR
-- 113.6), where the CR 613 layer system does not reach.
actionsFor :: PlayerId -> GameState.GameState -> [(ObjectId, [Effect Card.Card])]
actionsFor pid gs =
  let withAction oid = case Game.cardOf oid gs of
        Nothing -> Nothing
        Just card -> case Card.mulliganAction card of
          [] -> Nothing
          effects -> Just (oid, effects)
   in Maybe.mapMaybe withAction (Game.zoneMembers Zone.Hand pid gs)

-- CR 103.5b: offer this player every action their hand grants "any time [they]
-- could mulligan", repeatedly, until they decline or none is left. Performing
-- one is NOT taking a mulligan -- nothing is shuffled or bottomed and `counts`
-- is untouched -- so the caller's declaration still follows (CR 103.5b's last
-- sentence). Nothing in CR 103.5b or on the card limits a player to one action,
-- and a hand that redraws into a second granting card may use it here too.
--
-- Terminates even against an interpreter that never declines: each action moves
-- at least one card out of the hand for the rest of the game, so the deck
-- strictly shrinks, and an empty library redraws nothing -- leaving a hand with
-- no candidate, which ends the loop.
mulliganWindow :: MulliganPerformer -> PlayerId -> Game ()
mulliganWindow perform pid = do
  candidates <- State.gets (actionsFor pid)
  case candidates of
    -- Where the rules leave nothing to ask, don't prompt.
    [] -> pure ()
    _ -> do
      decider <- State.gets (Decide.deciderFor pid)
      answer <- Trans.lift (Program.prompt (Prompt.MulliganAction decider pid (fmap fst candidates)))
      case answer of
        Nothing -> pure ()
        Just oid -> case lookup oid candidates of
          -- An id that was not offered: validated by MEMBERSHIP, the
          -- Action.Activate posture, which keeps this total with no partial
          -- lookup and no way for an interpreter to conjure an action.
          Nothing -> pure ()
          Just effects -> do
            perform oid pid effects
            mulliganWindow perform pid

-- CR 103.5: repeat the declare-all-then-take-all round until no still-deciding
-- player mulligans. `deciding` is the players who have NOT yet kept -- keeping is
-- terminal (CR 103.5: "that player may not take any further mulligans"), so a
-- player who keeps drops out of the pool and is never asked again; only the
-- mulliganers of a round remain to decide in the next one.
mulliganRounds :: MulliganPerformer -> Map.Map PlayerId Numeric.Natural.Natural -> [PlayerId] -> Game ()
mulliganRounds perform counts deciding = do
  -- CR 103.5: the starting player declares first, then each other in turn order
  -- (turn order is preserved: `deciding` is filtered from the original `owners`).
  -- A player whose hand is already zero cannot mulligan (final sentence) and is
  -- treated as Keep without being asked -- which also drops them from the pool.
  decisions <- Monad.forM deciding $ \pid -> do
    -- CR 103.5b: the window comes FIRST -- the action is taken "at a time they
    -- would declare", and the declaration follows it. Reading the hand size
    -- after it is load-bearing: an action that empties the hand makes this a
    -- forced keep under CR 103.5's final sentence.
    mulliganWindow perform pid
    handSize <- State.gets (length . Game.zoneMembers Zone.Hand pid)
    if handSize <= 0
      then pure (pid, MulliganDecision.Keep)
      else do
        decider <- State.gets (Decide.deciderFor pid)
        let taken = Map.findWithDefault 0 pid counts
        decision <- Trans.lift (Program.prompt (Prompt.DeclareMulligan decider pid taken))
        pure (pid, decision)
  let mulliganers = fmap fst (filter (\(_, d) -> d == MulliganDecision.Mulligan) decisions)
  case mulliganers of
    -- CR 103.5: a round with no mulligans ends the process; the remaining hands
    -- are opening hands.
    [] -> pure ()
    _ -> do
      -- CR 103.5: "all players who decided to take mulligans do so at the same
      -- time." pawl is sequential; because a hand is hidden information, applying
      -- them in turn order is observably equivalent to simultaneity.
      counts' <- Monad.foldM takeMulligan counts mulliganers
      -- Kept players have dropped out; only this round's mulliganers decide again.
      mulliganRounds perform counts' mulliganers

-- CR 103.5c / CR 800.6: how many of a player's mulligans are free -- do not count
-- toward the number of cards that player puts on the bottom of their library, or
-- toward the number of mulligans they may take. CR 800.6: "In a multiplayer game,
-- the first mulligan a player takes doesn't count toward the number of cards that
-- player will put on the bottom of their library or the number of mulligans that
-- player may take."
--
-- CR 800.1: "A multiplayer game is a game that begins with more than two
-- players." BEGINS with, not currently has. GameState.turnOrder is the permanent
-- seating roster (see Pawl.Type.GameState), so counting seats answers that
-- directly, and a game that drops to two players by departure keeps its free
-- mulligan. A rebuilt game (CR 727.1 restart, CR 729.2 subgame) is seated from
-- the players who were in the game it came from, so it answers for itself.
--
-- A Natural rather than a Bool: the caller subtracts this, and a Bool would not
-- say WHAT to subtract. CR 103.5c grants the same allowance for a second,
-- independent reason -- any Brawl game (CR 903.12g) -- which pawl has no way to
-- know it is playing (#174), so the seat count is the only cause today. A
-- format layer redefines this function rather than chasing call sites.
freeMulligans :: GameState.GameState -> Numeric.Natural.Natural
freeMulligans gs = if length (GameState.turnOrder gs) > 2 then 1 else 0

-- CR 103.5: one player takes a mulligan -- shuffle the hand back, redraw a full
-- hand, then bottom "a number of those cards equal to the number of times that
-- player has taken a mulligan", in the player's chosen order, less the free
-- allowance (CR 103.5c, freeMulligans). The RAW count is what goes back into the
-- map and what Prompt.DeclareMulligan reports: it is the number of mulligans
-- taken, which is what CR 103.5c subtracts FROM.
takeMulligan :: Map.Map PlayerId Numeric.Natural.Natural -> PlayerId -> Game (Map.Map PlayerId Numeric.Natural.Natural)
takeMulligan counts pid = do
  handIds <- State.gets (Game.zoneMembers Zone.Hand pid)
  Monad.forM_ handIds (\oid -> Event.changeZone oid Zone.Library)
  shuffleLibrary pid
  Monad.replicateM_ openingHand (Event.drawCard pid)
  free <- State.gets freeMulligans
  let count = Map.findWithDefault 0 pid counts + 1
      -- CR 103.5c: the free mulligans do not count. Natural subtraction is
      -- partial below zero, so the comparison is explicit rather than clamped
      -- after the fact.
      counted = if count > free then count - free else 0
  newHand <- State.gets (Game.zoneMembers Zone.Hand pid)
  let n = min (fromIntegral counted) (length newHand)
  bottomChosen <-
    if n > 0 && length newHand >= 2
      then do
        decider <- State.gets (Decide.deciderFor pid)
        Trans.lift (Program.prompt (Prompt.Bottom decider pid newHand (fromIntegral n)))
      else -- CR 103.5: with nothing to bottom (a free mulligan, CR 103.5c) or a
      -- hand of 0 or 1, there is exactly one possible outcome; where the rules
      -- leave nothing to ask, don't prompt -- bottom whatever `n` names.
        pure (take n newHand)
  Monad.forM_ bottomChosen (\oid -> Event.changeZone oid Zone.Library)
  pure (Map.insert pid count counts)
