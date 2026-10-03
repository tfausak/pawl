module Pawl.Types.GrantedAbility where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.RuleAbilities as RuleAbilities
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | One whole quoted ability: what a CR 613.1f layer-6 grant hands to another
-- object -- Presence of Gond's "'{T}: Create a 1/1 green Elf Warrior creature
-- token'", Sixth Sense's "'Whenever this creature deals combat damage to a
-- player, you may draw a card'" -- and what CR 708.2 lists for a face-down
-- object (Pawl.Types.FaceDownCharacteristics' abilities).
--
-- The KIND of ability is CR 113.3's classification, so casing on this
-- constructor is the closed half reading a rulebook category and not the open
-- half's identity -- the same standing Pawl.Types.Keyword has. Which arm it is
-- decides only which ProjectedCharacteristics list the projection appends to;
-- nothing downstream learns the ability came from a grant.
--
-- Parametric in `card` for the reason ActivatedAbility and TriggeredAbility are,
-- and it is what Pawl.Types.Modification's own variable is instantiated at.
--
-- THIS is where the ability knot is tied, the way Pawl.Types.Card ties the card
-- one: every arm instantiates its ability variable at this very type, so an
-- ability granted by a continuous effect may itself grant an ability. Every
-- module on the path -- Effect, ModifyTarget, Clause, Mode, Modal,
-- ActivatedAbility, TriggeredAbility, StaticAbility -- stays parametric so that
-- none of them has to import this one, which is the cycle the variable exists
-- to open.
data GrantedAbility card
  = Activated (ActivatedAbility.ActivatedAbility card (GrantedAbility card))
  | Triggered (TriggeredAbility.TriggeredAbility card (GrantedAbility card))
  | -- | CR 113.3d.
    Static (StaticAbility.StaticAbility (GrantedAbility card))
  | -- | CR 613.11: rule-affecting static abilities, Chomping Kavu's "can't be
    -- blocked by creatures with power 2 or less".
    Rules RuleAbilities.RuleAbilities
  | -- | CR 113.3d / 614.1a: a static ability whose effect is a replacement effect.
    Replacement (PrintedReplacement.PrintedReplacement card (GrantedAbility card) (Effect.Effect card (GrantedAbility card)))
  | -- | CR 113.3d / 613.10: a static ability that affects players, Nerd Rage's
    -- "You have no maximum hand size".
    Player PlayerStaticAbility.PlayerStaticAbility
  | -- | CR 113.3d / 601.2f: a static ability reducing its own object's cost to
    -- cast, Richlau, Headmaster's "This spell costs {1} less to cast".
    SelfCostReduction CostReduction.CostReduction
  | -- | CR 113.6d / 118.9: an alternative cost its own object may be cast for,
    -- Mine Security's perpetual "You may pay {0} rather than pay this spell's
    -- mana cost".
    SelfAlternativeCost AlternativeCost.AlternativeCost
  deriving (Eq, Ord, Show)
