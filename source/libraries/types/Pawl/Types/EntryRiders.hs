module Pawl.Types.EntryRiders where

import qualified Pawl.Types.TapState as TapState

-- | What an effect says about a permanent AS IT ENTERS the battlefield, beyond the
-- permanent's own text -- Hanweir Garrison's "that are tapped and attacking",
-- Meandering Towershell's "return it to the battlefield tapped and attacking",
-- and Befriending the Moths' "return it to the battlefield transformed".
--
-- Carried by the OPCODE (Create, MoveToZone) and not by the entering object,
-- because neither is one of its characteristics (CR 109.3, CR 111.3). Two tokens
-- with the same text can enter differently, and one printed card can be returned
-- tapped by one effect and untapped by another.
--
-- Meaningful only for a BATTLEFIELD entry (CR 110.5d, CR 712.14a); a MoveToZone
-- naming any other zone carries the default and no rider is applied to it.
--
-- Three independent riders, not one flag, because the rules make them
-- independent. Tapped is CR 110.5's status category, defaulted by CR 110.5b.
-- Attacking is not a status at all but combat state (CR 506.3, CR 508.4), and a
-- creature put onto the battlefield attacking is NOT tapped by that fact -- CR
-- 508.1f taps only creatures declared as attackers, and CR 508.4c exempts these
-- from declaration. Transformed is neither: CR 712.14a makes it a question of
-- WHICH FACE the card enters showing, which CR 712.14 otherwise answers with the
-- front one.
--
-- `attacking` is a Bool rather than an AttackTarget because the effect does not
-- say WHAT the creature attacks; whom it attacks is chosen as it enters, by
-- Pawl.Engine.Combat.putOntoBattlefieldAttacking. CR 508.4's parenthetical case
-- of an effect that does specify has no card in the pool.
--
-- `transformed` is a Bool rather than a face name for the reason CR 712.14a
-- states: "If a spell or ability puts a double-faced card onto the battlefield
-- 'transformed' or 'converted', it enters the battlefield with its BACK face
-- up." The effect names no face -- which face that is falls out of the card's
-- layout (Pawl.Engine.Card.backFace), and Pawl.Engine.Event.changeZoneEntering
-- is where the rule is applied. That is also what keeps this rider clear of CR
-- 712.11b's choice of face when casting a modal double-faced card: a choice is
-- offered to the player as a list of castable faces, while this is an
-- instruction the effect carries and the player is never asked about.
--
-- Meaningful only for a MoveToZone of a CARD, which is CR 712.14a's own scope --
-- a token is not a card (CR 111.1), and no rule puts a double-faced token onto
-- the battlefield transformed (CR 707.8a decides a copy token's face by copy
-- rules instead). So Pawl.Engine.Resolve's Create arm does not read this rider,
-- and Pawl.CardSpec's corpus lint holds that no Create in the pool sets it.
data EntryRiders = MkEntryRiders
  { tapped :: TapState.TapState,
    attacking :: Bool,
    transformed :: Bool
  }
  deriving (Eq, Ord, Show)
