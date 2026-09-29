module Pawl.Codec.SacrificeToEnterSpec where

import qualified Pawl.Codec.SacrificeToEnter as SacrificeToEnter
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.SacrificeToEnter as SacrificeToEnter
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SacrificeToEnter" $ do
  Spec.it s "MkSacrificeToEnter" $
    Common.assertCodec
      s
      SacrificeToEnter.codec
      ( SacrificeToEnter.MkSacrificeToEnter
          { SacrificeToEnter.count = 1,
            SacrificeToEnter.filter = Filter.HasSubtype Subtype.Forest
          }
      )
      " {\"count\":1,\"filter\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Forest\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SacrificeToEnter.codec
