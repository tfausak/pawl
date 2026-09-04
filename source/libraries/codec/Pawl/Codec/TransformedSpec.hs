module Pawl.Codec.TransformedSpec where

import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Codec.Transformed as Transformed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Transformed as Transformed

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Transformed" $ do
  -- CR 701.27a, with CR 701.27e's characteristics: the whole projection of the
  -- permanent as it stood the instant the turn finished, Pawl.Codec.Moved's
  -- shape over the same sample.
  Spec.it s "MkTransformed carries the sampled characteristics" $
    Common.assertCodec
      s
      Transformed.codec
      ( Transformed.MkTransformed
          { Transformed.object = ObjectId.MkObjectId 1,
            Transformed.characteristics = ProjectedCharacteristicsSpec.testCharacteristics
          }
      )
      ( "{\"object\":1,\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "}"
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s Transformed.codec
