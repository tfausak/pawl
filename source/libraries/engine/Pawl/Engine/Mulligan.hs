module Pawl.Engine.Mulligan where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Numeric.Natural
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Decider as Decider
import Pawl.Types.Effect (Effect)
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.HandActionPerformer (HandActionPerformer)
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

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
  answer <- Trans.lift (Program.prompt (Prompt.Shuffle ids))
  let shuffled = Game.honourShuffle ids answer
  State.put gs {GameState.library = Map.insert pid (Seq.fromList shuffled) (GameState.library gs)}

-- CR 103.5: draw opening hands, then run the declaration/mulligan/bottom round
-- loop to completion. Assumes each player's library is already built and
-- shuffled. The per-player mulligan count is a local Map (absent = 0): the only
-- reader is this loop, which needs it for the number of cards bottomed (CR 103.5)
-- less the free allowance (CR 103.5c), so it never enters GameState. `owners` is
-- in turn order (starting player first).
openingHands :: HandActionPerformer -> [PlayerId] -> Game ()
openingHands perform owners = do
  -- CR 103.5 sentence 1: every player draws a full opening hand. A short library
  -- sets drewFromEmpty here; the flag survives the loop, which is what CR 727.3 /
  -- 729.3's "regardless of any mulligans" means.
  Monad.forM_ owners (Monad.replicateM_ openingHand . Event.drawCard)
  mulliganRounds perform Map.empty owners
  -- CR 103.6: the opening-hand window, after the WHOLE CR 103.5 process.
  openingHandActions perform owners

-- CR 103.5b / CR 103.6: the cards in this player's hand that grant an action
-- from the window `field` names, each paired with the effects that action
-- performs. A CLASSIFICATION, not an identity test: this asks whether the card
-- declares an action, never which card it is.
--
-- Read straight off the card (Game.cardOf) and never through the projection --
-- the Card.castingPermissions precedent: these abilities function in the HAND
-- (CR 113.6), where the CR 613 layer system does not reach.
actionsFor :: (Card.Card -> [Effect Card.Card]) -> PlayerId -> GameState.GameState -> [(ObjectId, [Effect Card.Card])]
actionsFor field pid gs =
  let withAction oid = case Game.cardOf oid gs of
        Nothing -> Nothing
        Just card -> case field card of
          [] -> Nothing
          effects -> Just (oid, effects)
   in Maybe.mapMaybe withAction (Game.zoneMembers Zone.Hand pid gs)

-- The shared CR 103.5b / CR 103.6 loop: offer this player every action their
-- hand grants through `field`, on the `ask` channel, until they decline or none
-- is left. Performing one is never a mulligan and never a cost -- it is the
-- action itself. Both rules let a player act more than once (CR 103.5b sets no
-- limit; CR 103.6 says "any such actions in any order"), which is why this
-- recurses rather than asking once.
--
-- Terminates even against an interpreter that never declines: every action in
-- the pool moves its card out of the hand -- CR 103.6a puts it onto the
-- battlefield, CR 103.5b's exiles it -- so the candidate list strictly shrinks.
--
-- Passing the prompt constructor needs no extension: `ask`'s result type is the
-- fixed `Prompt (Maybe ObjectId)`, not a polymorphic one.
handWindow ::
  (Card.Card -> [Effect Card.Card]) ->
  (Decider.Decider -> PlayerId -> [ObjectId] -> Prompt.Prompt (Maybe ObjectId)) ->
  HandActionPerformer ->
  PlayerId ->
  Game ()
handWindow field ask perform pid = do
  candidates <- State.gets (actionsFor field pid)
  case candidates of
    -- Where the rules leave nothing to ask, don't prompt.
    [] -> pure ()
    _ -> do
      decider <- State.gets (Decide.deciderFor pid)
      answer <- Trans.lift (Program.prompt (ask decider pid (fmap fst candidates)))
      case answer of
        Nothing -> pure ()
        Just oid -> case lookup oid candidates of
          -- An id that was not offered: validated by MEMBERSHIP, the
          -- Action.Activate posture, which keeps this total with no partial
          -- lookup and no way for an interpreter to conjure an action.
          Nothing -> pure ()
          Just effects -> do
            perform oid pid effects
            handWindow field ask perform pid

-- CR 103.6: "Once the mulligan process (see rule 103.5) is complete, the
-- starting player may take any such actions in any order. Then each other player
-- in turn order may do the same." `owners` is already in turn order with the
-- starting player first, which is exactly that order.
--
-- A player who has left the game is not here to act: the rebuild paths derive
-- `owners` from Game.stillPlayingInOrder, so they get no window, exactly as
-- they get no opening hand.
openingHandActions :: HandActionPerformer -> [PlayerId] -> Game ()
openingHandActions perform owners =
  Monad.forM_ owners (handWindow Card.openingHandAction Prompt.OpeningHandAction perform)

-- CR 103.5: repeat the declare-all-then-take-all round until no still-deciding
-- player mulligans. `deciding` is the players who have NOT yet kept -- keeping is
-- terminal (CR 103.5: "that player may not take any further mulligans"), so a
-- player who keeps drops out of the pool and is never asked again; only the
-- mulliganers of a round remain to decide in the next one.
mulliganRounds :: HandActionPerformer -> Map.Map PlayerId Numeric.Natural.Natural -> [PlayerId] -> Game ()
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
    handWindow Card.mulliganAction Prompt.MulliganAction perform pid
    handSize <- State.gets (length . Game.zoneMembers Zone.Hand pid)
    if handSize <= 0
      then pure (pid, MulliganDecision.Keep)
      else do
        decider <- State.gets (Decide.deciderFor pid)
        offer <- State.gets (offerFor counts pid)
        decision <- Trans.lift (Program.prompt (Prompt.DeclareMulligan decider pid offer))
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
-- seating roster (see Pawl.Types.GameState), so counting seats answers that
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
  -- The SAME offer the declaration prompt reported (offerFor is a pure function
  -- of `counts`, `pid` and the seat roster, none of which the redraw above
  -- touches), so this bottoms exactly what the player was promised.
  offer <- State.gets (offerFor counts pid)
  let count = MulliganOffer.taken offer + 1
      counted = MulliganOffer.bottomCount offer
  newHand <- State.gets (Game.zoneMembers Zone.Hand pid)
  let n = min counted (Natural.length newHand)
  bottomChosen <-
    if n > 0 && length newHand >= 2
      then do
        decider <- State.gets (Decide.deciderFor pid)
        answer <- Trans.lift (Program.prompt (Prompt.Bottom decider pid newHand n))
        -- CR 103.5: the cards bottomed come from THIS hand, and there are exactly
        -- `n` of them. Filtered, not trusted (#222); a short answer is topped up
        -- from the front of the hand rather than bottoming too few.
        -- nub, not just filter: an answer naming one card twice would otherwise
        -- bottom it twice, and the second changeZone is a no-op on an id that has
        -- already moved -- so the hand would end up one card too big rather than
        -- visibly wrong.
        let kept = List.genericTake n (List.nub (filter (\oid -> List.elem oid newHand) answer))
            topUp = List.genericTake (n - Natural.length kept) (filter (\oid -> List.notElem oid kept) newHand)
        pure (kept <> topUp)
      else -- CR 103.5: with nothing to bottom (a free mulligan, CR 103.5c) or a
      -- hand of 0 or 1, there is exactly one possible outcome; where the rules
      -- leave nothing to ask, don't prompt -- bottom whatever `n` names.
        pure (List.genericTake n newHand)
  Monad.forM_ bottomChosen (\oid -> Event.changeZone oid Zone.Library)
  pure (Map.insert pid count counts)
