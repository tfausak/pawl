module Pawl.Codec.AttachAllSpec where

import qualified Pawl.Codec.AttachAll as AttachAll
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttachAll as AttachAll
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttachAll" $ do
  -- CR 701.3a: the movers are an ObjectRef and the one destination a Filter, so
  -- the two keys carry different kinds and both are required.
  Spec.it s "MkAttachAll, both keys" $
    Common.assertCodec
      s
      AttachAll.codec
      ( AttachAll.MkAttachAll
          { AttachAll.subjects = ObjectRef.EachMatching (Filter.HasCardType CardType.Artifact),
            AttachAll.destination = Filter.IsSource
          }
      )
      " {\"subjects\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}},\"destination\":{\"type\":\"IsSource\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttachAll.codec
