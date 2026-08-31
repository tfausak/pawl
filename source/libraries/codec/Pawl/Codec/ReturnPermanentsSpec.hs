module Pawl.Codec.ReturnPermanentsSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ReturnPermanents as ReturnPermanents
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (ReturnPermanents.ReturnPermanents Keyword.Keyword)
codec = ReturnPermanents.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReturnPermanents" $ do
  -- Meloku the Clouded Mirror's one land. The count is HOW MANY, matched
  -- exactly, Pawl.Codec.TapPermanents' key names over a different action.
  Spec.it s "MkReturnPermanents" $
    Common.assertCodec
      s
      codec
      ( ReturnPermanents.MkReturnPermanents
          { ReturnPermanents.count = 1,
            ReturnPermanents.whichPermanents = Filter.HasCardType CardType.Land
          }
      )
      " {\"count\":1,\"whichPermanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
