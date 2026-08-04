module Pawl.Types.EntryRiders where

import qualified Pawl.Types.TapState as TapState

-- | What an effect says about a permanent AS IT ENTERS the battlefield, beyond the
-- permanent's own text -- Hanweir Garrison's "that are tapped and attacking",
-- and Meandering Towershell's "return it to the battlefield tapped and
-- attacking".
--
-- Carried by the OPCODE (Create, MoveToZone) and not by the entering object,
-- because neither is one of its characteristics (CR 109.3, CR 111.3). Two tokens
-- with the same text can enter differently, and one printed card can be returned
-- tapped by one effect and untapped by another.
--
-- Meaningful only for a BATTLEFIELD entry (CR 110.5d); a MoveToZone naming any
-- other zone carries the default and Pawl.Engine.Resolve applies neither rider.
--
-- Two independent riders, not one flag, because the rules make them independent.
-- Tapped is CR 110.5's status category, defaulted by CR 110.5b. Attacking is not
-- a status at all but combat state (CR 506.3, CR 508.4), and a creature put onto
-- the battlefield attacking is NOT tapped by that fact -- CR 508.1f taps only
-- creatures declared as attackers, and CR 508.4c exempts these from declaration.
--
-- `attacking` is a Bool rather than an AttackTarget because the effect does not
-- say WHAT the creature attacks; whom it attacks is chosen as it enters, by
-- Pawl.Engine.Combat.putOntoBattlefieldAttacking. CR 508.4's parenthetical case
-- of an effect that does specify has no card in the pool.
data EntryRiders = MkEntryRiders
  { tapped :: TapState.TapState,
    attacking :: Bool
  }
  deriving (Eq, Ord, Show)
