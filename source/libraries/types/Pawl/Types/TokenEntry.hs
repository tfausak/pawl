module Pawl.Types.TokenEntry where

import Pawl.Types.TapState (TapState)

-- What a token-creating effect says about the tokens AS THEY ENTER, beyond their
-- text -- Hanweir Garrison's "that are tapped and attacking".
--
-- Carried by the CREATE opcode and not by the token's embedded card, because
-- neither of these is one of the token's characteristics: CR 111.3 makes the
-- creating effect's defined values the token's "text", and CR 109.3's list of
-- characteristics excludes both of these by name ("characteristics don't include
-- whether a permanent is tapped, a spell's target, an object's owner or
-- controller"). Two tokens with the same text can enter differently, which is
-- the whole reason this is a separate field.
--
-- Two independent riders, not one flag, because the rules make them independent.
-- Tapped is CR 110.5's status category, defaulted by CR 110.5b ("permanents enter
-- the battlefield untapped ... unless a spell or ability says otherwise") --
-- which is the sentence "that are tapped" overrides. Attacking is not a status at
-- all (CR 110.5 lists four categories and this is not among them) but combat
-- state (CR 506.3, CR 508.4), and a creature put onto the battlefield attacking
-- is NOT tapped by that fact: CR 508.1f taps only creatures DECLARED as
-- attackers, and CR 508.4c exempts these from the declaration's rules entirely.
--
-- `attacking` is a Bool rather than an AttackTarget because the effect does not
-- say WHAT the tokens attack. CR 508.4's parenthetical -- "unless the effect that
-- put it onto the battlefield specifies what it's attacking" -- is the other
-- case, and no card in the pool is it; whom they attack is chosen as they enter,
-- by Pawl.Combat.putOntoBattlefieldAttacking.
data TokenEntry = MkTokenEntry
  { tapped :: TapState,
    attacking :: Bool
  }
  deriving (Eq, Ord, Show)
