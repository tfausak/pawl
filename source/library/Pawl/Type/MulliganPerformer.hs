module Pawl.Type.MulliganPerformer where

import Pawl.Type.Card (Card)
import Pawl.Type.Effect (Effect)
import Pawl.Type.Game (Game)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 103.5b: how the closed half performs a mulligan-window action's effects --
-- the hand card that granted the action, the player taking it, and the effects
-- themselves.
--
-- A PARAMETER rather than an import because Pawl.Resolve sits ABOVE
-- Pawl.Mulligan in the module graph (Effect.RestartGame -> Setup.restartGame ->
-- startGameFromCards -> openingHands), so Pawl.Mulligan importing Pawl.Resolve
-- would close a cycle. That cycle is a fact about the RULES, not the layout: an
-- opcode restarts a game, a game start draws opening hands, and drawing opening
-- hands performs opcodes. The precedent is Resolve.resolveSpellWith, which takes
-- its subgame runner the same way for the same reason.
--
-- Deliberately has NO default: "no subgame runner" is a real state of the world
-- (Resolve.noSubgame), but "no mulligan performer" is not -- it would silently
-- disable every CR 103.5b card at whichever call site forgot one.
type MulliganPerformer = ObjectId -> PlayerId -> [Effect Card] -> Game ()
