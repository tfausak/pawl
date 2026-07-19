module Pawl.Stack where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Resolve as Resolve
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone

-- CR 608.3: a resolving permanent spell becomes a permanent on the battlefield;
-- anything else resolves its effects and then goes to its owner's graveyard
-- (Resolve.resolveSpell -- the CR 608.2 executor).
--
-- THE INVARIANT: this dispatches on a CLASSIFICATION -- is-it-a-permanent, read
-- off the type line -- and never on the card's identity. There is no
-- `case card of Piker -> ...` here and there must never be one; that is the
-- fusion of the closed and open halves that sinks the project. The same shape as
-- is-it-a-mana-ability. Zero opcodes.
resolveTop :: Game ()
resolveTop = do
  gs <- State.get
  case GameState.stack gs of
    [] -> pure ()
    oid : rest -> case Game.lookupObject oid gs of
      -- A stack id that does not resolve is a bug elsewhere; drop it rather than
      -- wedging the loop.
      Nothing -> State.put gs {GameState.stack = rest}
      Just obj -> case Object.source obj of
        Source.OfCard printing ->
          if Card.isPermanent (Printing.card printing)
            then State.modify' (Game.changeZone oid Zone.Battlefield)
            else Resolve.resolveSpell oid
