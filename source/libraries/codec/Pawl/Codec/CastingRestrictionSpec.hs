module Pawl.Codec.CastingRestrictionSpec where

import qualified Pawl.Codec.CastingRestriction as CastingRestriction
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Phase as Phase

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastingRestriction" $ do
  -- Rally the Troops' "only during the declare attackers step" (CR 500.1).
  Spec.it s "DuringPhase, Rally the Troops' declare attackers step" $
    Common.assertJsonCodec
      s
      CastingRestriction.toJson
      CastingRestriction.fromJson
      (CastingRestriction.DuringPhase (Phase.Combat CombatStep.DeclareAttackers))
      "{\"type\":\"DuringPhase\",\"value\":{\"type\":\"Combat\",\"value\":{\"type\":\"DeclareAttackers\"}}}"
  -- A different phase, so the phase is pinned as part of the encoding rather
  -- than defaulted past.
  Spec.it s "DuringPhase, upkeep" $
    Common.assertJsonCodec
      s
      CastingRestriction.toJson
      CastingRestriction.fromJson
      (CastingRestriction.DuringPhase (Phase.Beginning BeginningStep.Upkeep))
      "{\"type\":\"DuringPhase\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}}}"
  Spec.it s "AttackedThisStep" $
    Common.assertJsonCodec
      s
      CastingRestriction.toJson
      CastingRestriction.fromJson
      CastingRestriction.AttackedThisStep
      "{\"type\":\"AttackedThisStep\"}"
