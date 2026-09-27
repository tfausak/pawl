module Pawl.Codec.EnteringTogetherSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.EnteringTogether as EnteringTogether
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EnteringTogether as EnteringTogether
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EnteringTogether" $ do
  -- CR 613.7m: a moved card and a token, in arrival order; only the token waits
  -- for its CR 111.2 entry event.
  Spec.it s "a batch mid-action" $
    Common.assertCodec
      s
      EnteringTogether.codec
      EnteringTogether.MkEnteringTogether
        { EnteringTogether.arrivals = Seq.fromList [ObjectId.MkObjectId 7, ObjectId.MkObjectId 4],
          EnteringTogether.minted = Seq.singleton (ObjectId.MkObjectId 4)
        }
      " {\"arrivals\":[7,4],\"minted\":[4]} "
  Spec.it s "an empty batch" $
    Common.assertCodec s EnteringTogether.codec EnteringTogether.empty " {} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s EnteringTogether.codec
