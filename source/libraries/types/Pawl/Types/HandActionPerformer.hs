module Pawl.Types.HandActionPerformer where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Game as Game
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 103.5b / CR 103.6: how the closed half performs the effects of an action a
-- card grants from a player's HAND before the game begins -- the card that
-- granted it, the player taking it, and the effects themselves.
--
-- Both windows use it: the mulligan-declaration window (CR 103.5b) and the
-- opening-hand window (CR 103.6), the second of which is explicitly not a
-- mulligan, which is why the name is no longer MulliganPerformer.
--
-- A PARAMETER rather than an import because Pawl.Engine.Resolve sits ABOVE
-- Pawl.Engine.Mulligan in the module graph (Effect.RestartGame -> Setup.restartGame ->
-- startGameFromCards -> openingHands), so Pawl.Engine.Mulligan importing Pawl.Engine.Resolve
-- would close a cycle. That cycle is a fact about the RULES, not the layout: an
-- opcode restarts a game, a game start draws opening hands, and drawing opening
-- hands performs opcodes. The precedent is Resolve.resolveSpellWith, which takes
-- its subgame runner the same way for the same reason.
--
-- Deliberately has NO default: "no subgame runner" is a real state of the world
-- (Resolve.noSubgame), but "no mulligan performer" is not -- it would silently
-- disable every CR 103.5b card at whichever call site forgot one.
type HandActionPerformer = ObjectId.ObjectId -> PlayerId.PlayerId -> [Effect.Effect Card.Card] -> Game.Game ()
