module Pawl.Stack where

import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone

-- CR 608.3: a resolving permanent spell becomes a permanent on the battlefield;
-- anything else goes to its owner's graveyard.
--
-- THE INVARIANT: this dispatches on a CLASSIFICATION -- is-it-a-permanent, read
-- off the type line -- and never on the card's identity. There is no
-- `case card of Piker -> ...` here and there must never be one; that is the
-- fusion of the closed and open halves that sinks the project. The same shape as
-- is-it-a-mana-ability. Zero opcodes.
--
-- Non-permanent spells cannot exist in M1a (nothing is an instant or sorcery
-- yet), so that branch is unreachable today. It is written anyway because it is
-- the rule, and because leaving it out would make this a partial function.
resolveTop :: GameState -> GameState
resolveTop gs = case GameState.stack gs of
  [] -> gs
  oid : rest -> case Game.lookupObject oid gs of
    -- A stack id that does not resolve is a bug elsewhere; drop it rather than
    -- wedging the loop.
    Nothing -> gs {GameState.stack = rest}
    Just obj -> case Object.source obj of
      Source.OfCard printing ->
        let destination =
              if Card.isPermanent (Printing.card printing)
                then Zone.Battlefield
                else Zone.Graveyard
         in Game.changeZone oid destination gs
