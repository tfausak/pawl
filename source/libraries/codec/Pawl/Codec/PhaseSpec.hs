{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PhaseSpec where

import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Phase as Phase

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Phase" $ do
  Spec.it s "Beginning" $
    Common.assertJsonCodec
      s
      Phase.toJson
      Phase.fromJson
      (Phase.Beginning BeginningStep.Upkeep)
      """ {"type":"Beginning","value":{"type":"Upkeep"}} """
  Spec.it s "PrecombatMain" $
    Common.assertJsonCodec
      s
      Phase.toJson
      Phase.fromJson
      Phase.PrecombatMain
      """ {"type":"PrecombatMain"} """
  Spec.it s "Combat" $
    Common.assertJsonCodec
      s
      Phase.toJson
      Phase.fromJson
      (Phase.Combat CombatStep.DeclareBlockers)
      """ {"type":"Combat","value":{"type":"DeclareBlockers"}} """
  Spec.it s "PostcombatMain" $
    Common.assertJsonCodec
      s
      Phase.toJson
      Phase.fromJson
      Phase.PostcombatMain
      """ {"type":"PostcombatMain"} """
  Spec.it s "Ending" $
    Common.assertJsonCodec
      s
      Phase.toJson
      Phase.fromJson
      (Phase.Ending EndingStep.EndStep)
      """ {"type":"Ending","value":{"type":"EndStep"}} """
