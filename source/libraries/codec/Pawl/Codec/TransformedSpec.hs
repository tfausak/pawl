module Pawl.Codec.TransformedSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Codec.Transformed as Transformed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Transformed as Transformed

-- The shared sample's rendering with its names array emptied, for the CR 708.2a
-- case below. A substitution rather than a second literal: the two halves of an
-- assertCodec have to describe the same record, and rebuilding the whole
-- rendering by hand is how they come apart.
noNamesJson :: String
noNamesJson =
  Text.unpack
    ( Text.replace
        (Text.pack "\"names\":[\"Test Creature\"]")
        (Text.pack "\"names\":[]")
        (Text.pack ProjectedCharacteristicsSpec.testCharacteristicsJson)
    )

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
  -- CR 708.2a's none, which is a real answer rather than an absence: a permanent
  -- with no name turned over triggers no "transforms into" ability at all, and
  -- the empty array has to survive the round trip for the matcher to see it.
  Spec.it s "MkTransformed, a permanent with no name" $
    Common.assertCodec
      s
      Transformed.codec
      ( Transformed.MkTransformed
          { Transformed.object = ObjectId.MkObjectId 2,
            Transformed.characteristics = ProjectedCharacteristicsSpec.testCharacteristics {PC.names = Set.empty}
          }
      )
      ("{\"object\":2,\"characteristics\":" <> noNamesJson <> "}")
  Spec.it s "has a schema" $ Common.assertHasSchema s Transformed.codec
