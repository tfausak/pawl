module Pawl.Codec.BecameCrewedSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.BecameCrewed as BecameCrewed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BecameCrewed as BecameCrewed
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BecameCrewed" $ do
  -- CR 702.122b/c. TWO crewers, and neither of them the Vehicle: only an
  -- asymmetric case catches a codec that crossed the two sides of the relation.
  Spec.it s "MkBecameCrewed, both keys" $
    Common.assertCodec
      s
      BecameCrewed.codec
      ( BecameCrewed.MkBecameCrewed
          { BecameCrewed.vehicle = ObjectId.MkObjectId 1,
            BecameCrewed.crewedBy = Set.fromList [ObjectId.MkObjectId 2, ObjectId.MkObjectId 3]
          }
      )
      " {\"crewedBy\":[2,3],\"vehicle\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecameCrewed.codec
