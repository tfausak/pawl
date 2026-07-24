module Pawl.Mulligan where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameState as GameState
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

-- CR 103.5: each player draws their opening hand. TODO in Task 3: run the
-- declaration/mulligan round loop. For now this reproduces the pre-mulligan
-- behavior exactly -- an unconditional `openingHand`-card draw per player.
openingHands :: [PlayerId] -> Game ()
openingHands owners =
  Monad.forM_ owners $ \pid ->
    Monad.replicateM_ openingHand (Event.drawCard pid)
