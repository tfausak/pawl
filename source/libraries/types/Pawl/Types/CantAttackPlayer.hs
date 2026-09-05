module Pawl.Types.CantAttackPlayer where

import Data.Set (Set)
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | CR 508.1c's PAIRWISE restriction: which creatures are restricted, whose side
-- of the board they may not attack, which of CR 506.3's attackable things on that
-- side are barred, and CR 508.1c's "unless" gate. Blazing Archon's "creatures
-- can't attack you" and Vow of Flight's "can't attack you or planeswalkers you
-- control".

-- Its own record rather than Pawl.Types.AffectedUnless plus a field, for
-- Pawl.Types.CantBeBlockedBy's reason: 'affected' names the creatures restricted
-- and 'defenders' names the players they may not be announced against, so the
-- two halves are not interchangeable and the second key is not a second
-- @affected@.
--
-- A Pawl.Types.PlayerScope and not a Pawl.Types.Filter over players, because CR
-- 109.5 is the whole of what the printed sentence needs: "you" is the source's
-- controller. Pawl.Types.RequiredDefender says why the REQUIREMENT side could
-- not take this type -- Public Enemy's object is the enchanted creature's
-- controller, which is not CR 109.5's "you" -- and that asymmetry is why the two
-- axes carry different types rather than sharing one.
data CantAttackPlayer = MkCantAttackPlayer
  { affected :: Affected.Affected,
    defenders :: PlayerScope.PlayerScope,
    -- | CR 506.3: which announcements aimed at a player in 'defenders' are
    -- barred -- the seat itself, their planeswalkers, the battles they protect.
    -- Blazing Archon writes OfPlayer alone, Vow of Flight adds OfPlaneswalker;
    -- no card in data/cards writes OfBattle (Scryfall @o:/can't attack
    -- .*battles/@, 2026-09-01, no hit), so that arm is a regression fence rather
    -- than a proven behaviour. The empty set bars nothing, which the codec
    -- accepts as Pawl.Codec.TypeLine accepts an empty required set.
    kinds :: Set AttackTargetKind.AttackTargetKind,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition,
    -- | CR 116.2d: the name this face gives the ability stating the restriction,
    -- so that face's own SpecialAction.IgnoreThisUntilEndOfTurn can say WHICH
    -- effect a payment ignores -- "the effect from that ability", singular.
    -- Pawl.Types.PlayerStaticAbility.name on the player axis, with the same
    -- meaning and joined to the grant by the same dataflow lint in
    -- Pawl.AbilitySlotLintSpec.
    --
    -- Nothing for the overwhelming majority, which grant no such permission and
    -- so have nothing to be referred to by. Volrath's Curse's one sentence
    -- declares BOTH halves of "can't attack or block" under one name, which is
    -- what makes one payment cover both.
    name :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
