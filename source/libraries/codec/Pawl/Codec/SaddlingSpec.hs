module Pawl.Codec.SaddlingSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Saddling as Saddling
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Saddling as Saddling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Saddling" $ do
  -- CR 702.171c. TWO saddlers, and neither of them the Mount: only an
  -- asymmetric case catches a codec that crossed the two sides of the relation.
  Spec.it s "MkSaddling, both keys" $
    Common.assertCodec
      s
      Saddling.codec
      ( Saddling.MkSaddling
          { Saddling.mount = ObjectId.MkObjectId 1,
            Saddling.saddledBy = Set.fromList [ObjectId.MkObjectId 2, ObjectId.MkObjectId 3]
          }
      )
      " {\"mount\":1,\"saddledBy\":[2,3]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Saddling.codec
