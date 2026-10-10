-- | Builders for the abilities the engine writes itself rather than reads off a
-- card: a keyword's, a designation's, the rulebook's own. Its only imports are
-- types, so Pawl.Engine.Keyword and the modules beneath it can use it.
module Pawl.Engine.Mint where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Clause as Clause
import Pawl.Types.Condition (Condition)
import Pawl.Types.Effect (Effect)
import Pawl.Types.Filter (Filter)
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import Pawl.Types.Optionality (Optionality)
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StepBegins as StepBegins
import Pawl.Types.TargetSlot (TargetSlot)
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope

-- | One clause (CR 608.2e) of these effects in order (CR 608.2c), with this
-- optionality and nothing else: no "if you do", no "if", no "otherwise", no
-- "unless".
clause :: Optionality -> Seq.Seq (Effect card ability) -> Clause.Clause card ability
clause optionality = Clause.MkClause Nothing Nothing Nothing optionality Nothing

-- | A `clause` its controller must follow.
mandatory :: Seq.Seq (Effect card ability) -> Clause.Clause card ability
mandatory = clause Optionality.Mandatory

-- | A `mandatory` clause that runs only if this condition holds as it resolves:
-- a printed "if" scoped to one clause (CR 608.2c).
mandatoryIf :: Condition -> Seq.Seq (Effect card ability) -> Clause.Clause card ability
mandatoryIf condition = Clause.MkClause Nothing (Just condition) Nothing Optionality.Mandatory Nothing

-- | A `clause` its controller may decline as it resolves: "you may" (CR 603.5).
youMay :: Seq.Seq (Effect card ability) -> Clause.Clause card ability
youMay = clause (Optionality.Optional (PlayerRef.Relative PlayerRelation.You))

-- | One mode, selected outright, of one mandatory clause holding these effects
-- in order (CR 608.2c): an ability that is not modal (CR 700.2) and chooses
-- nothing but its targets.
oneMode :: Seq.Seq (Effect card ability) -> Modal.Modal card ability
oneMode = oneModeTargeting Map.empty

-- | `oneMode`, with these target slots (CR 115.1).
oneModeTargeting :: Map.Map SlotName TargetSlot -> Seq.Seq (Effect card ability) -> Modal.Modal card ability
oneModeTargeting slots = oneModeOf slots . Seq.singleton . mandatory

-- | One mode, selected outright, of these clauses and target slots: the
-- general form of `oneModeTargeting`, for a clause that is optional, gated or
-- one of several.
oneModeOf :: Map.Map SlotName TargetSlot -> Seq.Seq (Clause.Clause card ability) -> Modal.Modal card ability
oneModeOf slots clauses = Modal.MkModal (Seq.singleton (Mode.MkMode clauses slots)) (ModeSelection.ChooseExactly 1)

-- | A triggered ability (CR 603.1) of one `oneMode`, with no intervening "if"
-- (CR 603.4), no name and no "only once" rider.
trigger :: TriggerCondition -> Seq.Seq (Effect card ability) -> TriggeredAbility.TriggeredAbility card ability
trigger condition = triggerOf condition Nothing . oneMode

-- | `trigger`, with this intervening "if" (CR 603.4).
triggerIf :: TriggerCondition -> Condition -> Seq.Seq (Effect card ability) -> TriggeredAbility.TriggeredAbility card ability
triggerIf condition intervening = triggerOf condition (Just intervening) . oneMode

-- | A triggered ability (CR 603.1) of this modal and intervening "if" (CR
-- 603.4), with no name and no "only once" rider: the general form of
-- `trigger` and `triggerIf`.
triggerOf :: TriggerCondition -> Maybe Condition -> Modal.Modal card ability -> TriggeredAbility.TriggeredAbility card ability
triggerOf condition intervening modal =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal = modal,
      TriggeredAbility.intervening = intervening,
      TriggeredAbility.name = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

-- | "At the beginning of your upkeep" (CR 503.1): ControllersTurn with the
-- ability's controller as "you".
yourUpkeep :: TriggerCondition
yourUpkeep = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) Nothing TurnScope.ControllersTurn)

-- | "Whenever ... casts a spell" (CR 601.2i), the filter naming whose and
-- which: from any zone, on every occurrence rather than one counted one, and
-- never for a copy.
spellCast :: Filter Keyword -> TriggerCondition
spellCast filter_ =
  TriggerCondition.SpellCast
    SpellCast.MkSpellCast
      { SpellCast.filter = filter_,
        SpellCast.scope = TurnScope.EachTurn,
        SpellCast.zone = Nothing,
        SpellCast.ordinal = Nothing,
        SpellCast.phase = Nothing,
        SpellCast.copies = False
      }

-- | "Whenever this creature deals combat damage to a player". A PLAYER and not
-- an opponent: rules 702.70a, 702.99a, 702.112a and 702.115a all word it so,
-- where Akki Lavarunner and Questing Beast print "to an opponent".
dealsCombatDamageToAPlayer :: TriggerCondition
dealsCombatDamageToAPlayer = TriggerCondition.SelfDealsCombatDamageToPlayer PlayerRelation.AnyPlayer
