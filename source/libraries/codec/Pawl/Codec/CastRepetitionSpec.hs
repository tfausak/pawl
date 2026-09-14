module Pawl.Codec.CastRepetitionSpec where

import qualified Pawl.Codec.CastRepetition as CastRepetition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastRepetition as CastRepetition

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastRepetition" $ do
  Spec.it s "Once" $
    Common.assertCodec
      s
      CastRepetition.codec
      CastRepetition.Once
      " {\"type\":\"Once\"} "
  Spec.it s "AnyNumber" $
    Common.assertCodec
      s
      CastRepetition.codec
      CastRepetition.AnyNumber
      " {\"type\":\"AnyNumber\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CastRepetition.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastRepetition.codec
