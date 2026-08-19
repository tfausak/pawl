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

import qualified Data.Map as Map
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword

-- CR 702.61a: is a spell with split second on the stack right now?
--
-- A MEMBERSHIP question and never a tally, which is CR 702.61c -- multiple
-- instances are redundant, so one is as good as three and no card need print
-- two. Map.member rather than a count for exactly that reason.
--
-- No player is named, because CR 702.61a names none: the restriction reaches
-- every player, the controller of the split-second spell included. That the
-- spell itself is not stopped is the rule's "other spells" limb holding by
-- construction -- it is already on the stack by the time this answers True.
--
-- Read off the CR 613 PROJECTION rather than the printed face, so a spell that
-- was GRANTED split second is seen -- Molten Disaster's "if this spell was
-- kicked, it has split second", which Pawl.Engine.Projection gathers from the
-- stack under CR 113.6. A spell cast face down is measured by CR 708.4 against
-- CR 708.2a's no-text characteristics, which the projection seeds from
-- Game.faceOf and so answers unchanged.
--
-- Not implemented: split second granted from OUTSIDE the spell -- Shadow the
-- Hedgehog's "each spell you cast has split second if mana from an artifact was
-- spent to cast it" (#1284). The projection would read such a grant; nothing can
-- yet write one.
inForce :: GameState -> Bool
inForce gs =
  let onStack oid = Map.member Keyword.SplitSecond (Projection.keywordsOf oid gs)
   in any onStack (GameState.stack gs)
