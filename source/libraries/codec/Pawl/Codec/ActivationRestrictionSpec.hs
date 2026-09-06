module Pawl.Codec.ActivationRestrictionSpec where

import qualified Pawl.Codec.ActivationRestriction as ActivationRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.DuringPhase as DuringPhase
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivationRestriction" $ do
  Spec.it s "SorcerySpeed" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ActivationRestriction.SorcerySpeed
      " {\"type\":\"SorcerySpeed\"} "
  -- A stepped window (CR 511.1) beside a phase one (CR 500.1):
  -- Pawl.Types.PhaseSelector spans both, so the arm has to carry both.
  Spec.it s "DuringPhase, Desert's end-of-combat rider" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ( ActivationRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat),
              DuringPhase.scope = TurnScope.EachTurn
            }
      )
      " {\"type\":\"DuringPhase\",\"value\":{\"window\":{\"type\":\"Step\",\"value\":{\"type\":\"Combat\",\"value\":{\"type\":\"EndOfCombat\"}}},\"scope\":{\"type\":\"EachTurn\"}}} "
  -- The arm's second axis: the SAME window under each scope, so a codec that
  -- dropped the scope would collapse this and the previous case into one.
  Spec.it s "DuringPhase, Llanowar Augur's controller's-turn upkeep" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ( ActivationRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep),
              DuringPhase.scope = TurnScope.ControllersTurn
            }
      )
      " {\"type\":\"DuringPhase\",\"value\":{\"window\":{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}}},\"scope\":{\"type\":\"ControllersTurn\"}}} "
  -- The PhaseSelector's stepless arm: a phase that HAS steps, named whole.
  Spec.it s "DuringPhase, Jade Statue's combat-phase rider" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ( ActivationRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.CombatPhase,
              DuringPhase.scope = TurnScope.EachTurn
            }
      )
      " {\"type\":\"DuringPhase\",\"value\":{\"window\":{\"type\":\"CombatPhase\"},\"scope\":{\"type\":\"EachTurn\"}}} "
  -- CR 102.1 with no window beside it, which is the arm DuringPhase above cannot
  -- reach: Lavinia, Foil to Conspiracy's "Activate only during an opponent's
  -- turn". Rendered payload-tagged, so a decoder cannot confuse it with the
  -- windowed arm.
  Spec.it s "DuringTurn, Lavinia's opponent's-turn rider" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      (ActivationRestriction.DuringTurn TurnScope.OpponentsTurn)
      " {\"type\":\"DuringTurn\",\"value\":{\"type\":\"OpponentsTurn\"}} "
  -- CR 602.5's second clause, and the arm that made this type a list: Kongming's
  -- Contraptions prints it beside the DuringPhase above.
  Spec.it s "AttackedThisStep" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ActivationRestriction.AttackedThisStep
      " {\"type\":\"AttackedThisStep\"} "
  -- Trap Runner's "activate only during combat after blockers are declared"
  -- (CR 506.7b through CR 506.7g).
  Spec.it s "AfterBlockersDeclared" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ActivationRestriction.AfterBlockersDeclared
      " {\"type\":\"AfterBlockersDeclared\"} "
  -- Save Point's "activate only during combat before combat damage has been
  -- dealt" (CR 506.7 through CR 506.7g).
  Spec.it s "BeforeCombatDamage" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ActivationRestriction.BeforeCombatDamage
      " {\"type\":\"BeforeCombatDamage\"} "
  -- CR 602.5 over a fact about the board rather than a window: Barbarian Ring's
  -- "Activate only if there are seven or more cards in your graveyard".
  Spec.it s "OnlyIf, Barbarian Ring's threshold rider" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ( ActivationRestriction.OnlyIf
          ( Condition.Compares
              Compares.MkCompares
                { Compares.measured =
                    Quantity.Count
                      Count.MkCount
                        { Count.aggregation = Aggregation.Members,
                          Count.filter = Filter.And [],
                          Count.scope = Scope.InZone (InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.You))
                        },
                  Compares.comparison = Comparison.AtLeast,
                  Compares.threshold = Quantity.Literal 7
                }
          )
      )
      " {\"type\":\"OnlyIf\",\"value\":{\"type\":\"Compares\",\"value\":{\"comparison\":{\"type\":\"AtLeast\"},\"measured\":{\"type\":\"Count\",\"value\":{\"aggregation\":{\"type\":\"Members\"},\"filter\":{\"type\":\"And\",\"value\":[]},\"scope\":{\"type\":\"InZone\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"zone\":{\"type\":\"Graveyard\"}}}}},\"threshold\":{\"type\":\"Literal\",\"value\":7}}}} "
  -- CR 602.5b's counted rider, which CR 702.177a's exhaust rewrites into:
  -- Greenbelt Guardian's "Activate only once".
  Spec.it s "OnlyOnce" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ActivationRestriction.OnlyOnce
      " {\"type\":\"OnlyOnce\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ActivationRestriction.codec
