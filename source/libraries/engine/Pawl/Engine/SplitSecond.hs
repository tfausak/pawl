-- CR 702.61: split second, a keyword that restricts what OTHER players may do.
-- CR 702.61a states it as a static ability of a spell on the stack, so it is a
-- rules-modifying
-- continuous effect (CR 611.1's third clause) rather than an ability object, and
-- nothing is minted from it.
--
-- Its own module for Pawl.Engine.Defender's reason: two halves of the engine ask
-- it and neither imports the other. Pawl.Engine.Cast gates CR 601.3 on it and
-- Pawl.Engine.Activate gates CR 602.5 on it, so the question has to live under
-- both.
--
-- The posture Pawl.Engine.CombatRestriction and Pawl.Engine.PlayerEffect take:
-- gathered LIVE on every read and never captured, so CR 702.61a's "as long as
-- this spell is on the stack" needs nothing to unwind -- the spell resolving, or
-- being countered, lifts the restriction by leaving the stack.
module Pawl.Engine.SplitSecond where

import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword

-- CR 702.61a: is a spell with split second on the stack right now?
--
-- A MEMBERSHIP question and never a tally, which is CR 702.61c -- multiple
-- instances are redundant, so one is as good as three and no card need print
-- two.
--
-- No player is named, because CR 702.61a names none: the restriction reaches
-- every player, the controller of the split-second spell included. That the
-- spell itself is not stopped is the rule's "other spells" limb holding by
-- construction -- it is already on the stack by the time this answers True.
--
-- Read off the PRINTED face (Game.faceOf), which is what Pawl.Engine.Event's CR
-- 113.6g counterability gate does with the other static ability that functions
-- on the stack. A spell cast face down is measured by CR 708.4 against CR
-- 708.2a's no-text characteristics, which faceOf already answers.
--
-- Not implemented: a spell that GAINS split second, which Molten Disaster and
-- Shadow the Hedgehog print (#1205).
inForce :: GameState -> Bool
inForce gs =
  let onStack oid = maybe False (Set.member Keyword.SplitSecond . Face.keywords) (Game.faceOf oid gs)
   in any onStack (GameState.stack gs)
