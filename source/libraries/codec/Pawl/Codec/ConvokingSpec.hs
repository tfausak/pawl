module Pawl.Codec.ConvokingSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Convoking as Convoking
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Convoking as Convoking
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Convoking" $ do
  -- CR 702.51c. TWO convokers, and neither of them the spell: only an
  -- asymmetric case catches a codec that crossed the two sides of the relation.
  Spec.it s "MkConvoking, both keys" $
    Common.assertCodec
      s
      Convoking.codec
      ( Convoking.MkConvoking
          { Convoking.spell = ObjectId.MkObjectId 1,
            Convoking.convokedBy = Set.fromList [ObjectId.MkObjectId 2, ObjectId.MkObjectId 3]
          }
      )
      " {\"convokedBy\":[2,3],\"spell\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Convoking.codec
