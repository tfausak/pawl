module Pawl.Codec.SupertypeSpec where

import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Supertype" $ do
  Spec.it s "Basic" $
    Common.assertCodec
      s
      Supertype.codec
      Supertype.Basic
      " {\"type\":\"Basic\"} "

  Spec.it s "Legendary" $
    Common.assertCodec
      s
      Supertype.codec
      Supertype.Legendary
      " {\"type\":\"Legendary\"} "

  Spec.it s "Ongoing" $
    Common.assertCodec
      s
      Supertype.codec
      Supertype.Ongoing
      " {\"type\":\"Ongoing\"} "

  Spec.it s "Snow" $
    Common.assertCodec
      s
      Supertype.codec
      Supertype.Snow
      " {\"type\":\"Snow\"} "

  Spec.it s "World" $
    Common.assertCodec
      s
      Supertype.codec
      Supertype.World
      " {\"type\":\"World\"} "

  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Supertype.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Supertype.codec
