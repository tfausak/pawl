-- CR 705: flipping a coin. The ONE road every flip in the engine takes, and the
-- only place CR 705.3's stated result is applied.
--
-- Its own module rather than a function in Pawl.Engine.Resolve, because there are
-- two writers and neither can hold the funnel: Resolve's Effect.FlipCoin arm is
-- CR 705.2's win/lose flip (Winter Sky), and Pawl.Engine.Event's
-- EntryRewrite.ChoiceByCoinFlip arm is CR 705.2's first-sentence one (Molten
-- Sentry), which is an entry replacement and so cannot live in Resolve. One
-- road is what keeps rule 705.3 from having to be written twice.
--
-- Nothing here decides who WINS: rule 705.2's comparison of the call against the
-- face belongs to the effect that asked for a call, and this module answers only
-- the two questions rule 705.3 lets an effect state.
module Pawl.Engine.Coin where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.StatedFlip as StatedFlip

-- | CR 705.1's flip, asked of the INTERPRETER: nobody decides how a coin lands,
-- so this goes through Game.ask and never Game.choose.
--
-- Answers what CR 705.3 lets an effect state about the flip: the face to use --
-- the actual one when no effect states another -- and whether the flipper is
-- stated to WIN it. The caller decides what a win means for its own kind of
-- flip; a flip that CR 705.2's first sentence leaves winnerless still takes the
-- stated win, since Edgar, King of Figaro's ruling says its ability "can cause
-- you to win coin flips that would ordinarily have no winner".
--
-- The coin is flipped even when a statement will discard the result: rule 705.3
-- says to ignore the actual result, not to skip the flip, and an interpreter
-- replaying a transcript must be asked the same questions either way.
--
-- Takes a Maybe seat because the entry road's flipper comes from
-- Projection.controllerOf, which is a Maybe. No seat means no statement can
-- apply -- rule 705.3's statements are all about a particular player.
flipCoin :: Maybe PlayerId -> Game (CoinFace.CoinFace, Bool)
flipCoin mFlipper = do
  gs <- State.get
  actual <- Game.ask Prompt.FlipCoin
  let stated = foldMap (\pid -> statedFor pid gs) mFlipper
  pure (Maybe.fromMaybe actual (statedFace stated), any StatedFlip.wins stated)

-- | The CR 705.3 statements that apply to a flip `pid` is about to make: every
-- statement in force, less the ones Edgar's "the FIRST time you flip one or more
-- coins each turn" has already been spent on.
--
-- Read BEFORE the flip is recorded, which is what makes the count below the
-- flips that came earlier in the turn rather than this one.
statedFor :: PlayerId -> GameState -> [StatedFlip.StatedFlip]
statedFor pid gs =
  let spent = flipsThisTurn pid gs > 0
   in filter (\statement -> not (spent && StatedFlip.firstEachTurn statement)) (PlayerEffect.statedFlips pid gs)

-- | CR 705.3's stated result, out of however many statements apply. The LAST
-- one, in the timestamp order Pawl.Engine.PlayerEffect.statedFlips returns:
-- rule 705.3 gives no order of its own, and a later effect overriding an earlier
-- one is what CR 613.7a's tie-break does everywhere else a stored effect can
-- disagree with another.
--
-- UNPROVEN by any board, and it cannot be until a second card states a face:
-- Edgar, King of Figaro is the only producer, and two Edgars state the same
-- face.
statedFace :: [StatedFlip.StatedFlip] -> Maybe CoinFace.CoinFace
statedFace = Maybe.listToMaybe . reverse . Maybe.mapMaybe StatedFlip.face

-- | How many CR 705.1 flips `pid` has already made this turn. A fold over the
-- whole event log, which is exactly "this turn" for
-- Pawl.Engine.PlayerEffect.castsThisTurn's reason: Engine.handoffTurn clears the
-- log at the handoff and no reader ever drains it.
--
-- Counts BOTH roads, because both record a CR 705.1 flip: a Molten Sentry that
-- entered earlier in the turn is a coin this player flipped, and spends Edgar's
-- one statement just as Winter Sky's would.
flipsThisTurn :: PlayerId -> GameState -> Int
flipsThisTurn pid gs =
  let flipped logged = case LoggedEvent.event logged of
        GameEvent.CoinFlipped flipped_ -> CoinFlipped.flipper flipped_ == pid
        _ -> False
   in length (filter flipped (Foldable.toList (GameState.events gs)))
