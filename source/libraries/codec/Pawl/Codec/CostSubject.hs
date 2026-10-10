module Pawl.Codec.CostSubject where

import qualified Pawl.Codec.ActivationCriteria as ActivationCriteria
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CostSubject as CostSubject

codec :: Codec.Codec CostSubject.CostSubject
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Spells" CostSubject.Spells,
      Arm.payload "Activations" ActivationCriteria.codec CostSubject.Activations (\x -> case x of CostSubject.Activations y -> Just y; _ -> Nothing)
    ]

tagOf :: CostSubject.CostSubject -> String
tagOf x = case x of
  CostSubject.Spells {} -> "Spells"
  CostSubject.Activations {} -> "Activations"
