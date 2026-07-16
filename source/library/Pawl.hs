-- The public surface of the engine. Bare @module@ re-exports trip
-- -Wmissing-import-lists, so the entry points are listed explicitly; that also
-- keeps this module an intentional API rather than whatever happens to be in
-- scope. Everything else stays reachable via its own @Pawl.*@ module.
module Pawl
  ( Engine.playFrom,
    Engine.playGame,
    Engine.runGame,
    Engine.runGamePure,
    Replay.record,
    Replay.replay,
    Setup.deckSize,
    Setup.emptyGame,
    Setup.newGame,
    Setup.openingHand,
    Setup.startingLife,
  )
where

import qualified Pawl.Engine as Engine
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
