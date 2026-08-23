module Pawl.Types.BattlefieldCandidate where

import qualified Pawl.Types.PlayerId as PlayerId

-- | One permanent as CR 603.10's trigger scan needs to see it: who controlled it
-- (CR 603.3a) and what it looked like at that moment. Pawl.Engine.Event's
-- battlefieldCandidates builds these, and
-- Pawl.Types.GameState.battlefieldWhenTriggered files them per event group with
-- the reasoning for why a sample is needed at all.
--
-- A record rather than a pair, so neither half can be taken for the other; both
-- were unnamed before, and the controller is the one the live board most often
-- gets wrong.
--
-- PARAMETRIC in what is carried beside the controller, because the scan narrows
-- it: the stored form holds projected characteristics, and the scan maps those
-- to the abilities that function on the battlefield. Neither field is strict --
-- GameState.battlefieldWhenTriggered's entries are thunks over the state at that
-- moment, and forcing one here would cost the projection the laziness exists to
-- avoid.
data BattlefieldCandidate a = MkBattlefieldCandidate
  { controller :: PlayerId.PlayerId,
    characteristics :: a
  }
  deriving (Eq, Ord, Show)

-- | Maps what is carried beside the controller, which is what the trigger scan
-- does to narrow characteristics to abilities. Written out rather than derived,
-- so the module needs no extension, and it is the same mapping the pair this
-- replaced got from its own Functor.
instance Functor BattlefieldCandidate where
  fmap f candidate = candidate {characteristics = f (characteristics candidate)}
