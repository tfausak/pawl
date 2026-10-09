-- | Builders for the abilities the engine writes itself -- a keyword's
-- (CR 702), a designation's (CR 725, 726) or the rulebook's own (CR 728.1,
-- 901.8) -- rather than reads off a card. Its only imports are types, so
-- Pawl.Engine.Keyword and the modules beneath it can use it.
module Pawl.Engine.Mint where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Types.Clause as Clause
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.TargetSlot (TargetSlot)
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | One mode, selected outright, of one mandatory clause holding these effects
-- in order (CR 608.2c): an ability that is not modal (CR 700.2) and chooses
-- nothing but its targets.
oneMode :: Seq.Seq (Effect card ability) -> Modal.Modal card ability
oneMode = oneModeTargeting Map.empty

-- | `oneMode`, with these target slots (CR 115.1).
oneModeTargeting :: Map.Map SlotName TargetSlot -> Seq.Seq (Effect card ability) -> Modal.Modal card ability
oneModeTargeting slots effects = oneModeOf slots (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing effects))

-- | One mode, selected outright, of these clauses and target slots: the
-- general form of `oneModeTargeting`, for a clause that is optional, gated or
-- one of several.
oneModeOf :: Map.Map SlotName TargetSlot -> Seq.Seq (Clause.Clause card ability) -> Modal.Modal card ability
oneModeOf slots clauses = Modal.MkModal (Seq.singleton (Mode.MkMode clauses slots)) (ModeSelection.ChooseExactly 1)

-- | A triggered ability (CR 603.1) of one `oneMode`, with no intervening "if"
-- (CR 603.4), no name and no "only once" rider.
trigger :: TriggerCondition -> Seq.Seq (Effect card ability) -> TriggeredAbility.TriggeredAbility card ability
trigger condition effects =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal = oneMode effects,
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.name = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
