module Pawl.Types.EntryRiders where

import qualified Pawl.Types.TapState as TapState

-- | What an effect says about a permanent AS IT ENTERS the battlefield, beyond the
-- permanent's own text -- Hanweir Garrison's "that are tapped and attacking",
-- and Meandering Towershell's "return it to the battlefield tapped and
-- attacking".
--
-- Carried by the OPCODE (Create, MoveToZone) and not by the entering object,
-- because neither of these is one of its characteristics: CR 111.3 makes a
-- token-creating effect's defined values the token's "text", and CR 109.3's list
-- of characteristics excludes both of these by name ("characteristics don't
-- include whether a permanent is tapped, a spell's target, an object's owner or
-- controller"). Two tokens with the same text can enter differently, which is
-- the whole reason this is a separate field; and one printed card can be
-- returned to the battlefield tapped by one effect and untapped by another.
--
-- Meaningful only for a BATTLEFIELD entry. CR 110.5d: "Only permanents have
-- status. Cards not on the battlefield do not ... cards not on the battlefield
-- are neither tapped nor untapped." Attacking is not a status at all but combat
-- state, and equally battlefield-only. So a MoveToZone naming any other zone
-- carries the default, and Pawl.Engine.Resolve applies neither rider there.
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
-- say WHAT the creature attacks. CR 508.4's parenthetical -- "unless the effect
-- that put it onto the battlefield specifies what it's attacking" -- is the
-- other case, and no card in the pool is it; whom they attack is chosen as they
-- enter, by Pawl.Engine.Combat.putOntoBattlefieldAttacking. Meandering
-- Towershell's own ruling is explicit that this is a choice and not a memory:
-- "you choose which opponent or opposing planeswalker it's attacking. It doesn't
-- have to attack the same opponent ... that it was when it was exiled."
data EntryRiders = MkEntryRiders
  { tapped :: TapState.TapState,
    attacking :: Bool
  }
  deriving (Eq, Ord, Show)
