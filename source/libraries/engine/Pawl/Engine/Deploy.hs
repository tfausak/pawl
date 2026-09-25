-- | CR 804: the deploy creatures option.
module Pawl.Engine.Deploy where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Pawl.Types.ActivatedAbility (ActivatedAbility)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Activator as Activator
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GiveControl as GiveControl
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSlot as TargetSlot

-- | CR 804.2: does this permanent have the deploy ability, given whether the
-- game uses the option (GameSettings.deployCreatures)? Every creature does. An
-- ability of the creature like CR 305.6's intrinsic one, so CR 613.1f's "loses
-- all abilities" takes it too (Pawl.Engine.Subtype.intrinsicManaAbilityOf's
-- reading).
--
-- Asked of a permanent's characteristics only: CR 804.2's "creature" is a
-- creature on the battlefield (CR 109.2), so no reader asks it of a card or a
-- spell.
grants :: Bool -> PC.ProjectedCharacteristics -> Bool
grants optionOn pc =
  optionOn
    && Set.member CardType.Creature (PC.cardTypes pc)
    && not (PC.lostAllAbilities pc)

-- | CR 804.2: "{T}: Target teammate gains control of this creature. Activate
-- only as a sorcery."
ability :: ActivatedAbility Card (GrantedAbility.GrantedAbility Card)
ability =
  let slot = TargetSlot.required Pool.Players (Just (Filter.IsPlayer PlayerRelation.Teammate))
      effect = Effect.GiveControl (GiveControl.MkGiveControl (PlayerRef.InSlot teammate) (ObjectRef.EachMatching Filter.IsSource))
   in ActivatedAbility.MkActivatedAbility
        { ActivatedAbility.cost = Cost.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.TapThis],
          ActivatedAbility.modal =
            Modal.MkModal
              (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) (Map.singleton teammate slot)))
              (ModeSelection.ChooseExactly 1),
          ActivatedAbility.maximumX = [],
          ActivatedAbility.minimumX = 0,
          -- CR 307.5's sorcery timing.
          ActivatedAbility.restrictions = [ActivationRestriction.SorcerySpeed],
          ActivatedAbility.activator = Activator.Controller,
          ActivatedAbility.condition = Nothing,
          ActivatedAbility.name = Nothing,
          ActivatedAbility.keyword = Nothing
        }

-- The slot CR 804.2's one target is chosen into.
teammate :: SlotName.SlotName
teammate = SlotName.MkSlotName (Text.pack "teammate")
