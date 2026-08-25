module Pawl.Codec.TurnUpProcedureSpec where

import qualified Pawl.Codec.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnUpProcedure" $ do
  -- The one procedure a row is ever conditional on today: CR 702.37b's megamorph
  -- counter, whose cost only CR 702.37e's procedure pays.
  Spec.it s "Morph (CR 702.37e)" $
    Common.assertCodec
      s
      TurnUpProcedure.codec
      TurnUpProcedure.Morph
      " {\"type\":\"Morph\"} "
  -- Exhaustive where the literal above is representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s TurnUpProcedure.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s TurnUpProcedure.codec
