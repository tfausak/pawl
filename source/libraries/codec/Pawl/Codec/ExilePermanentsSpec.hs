module Pawl.Codec.ExilePermanentsSpec where

import qualified Pawl.Codec.ExilePermanents as ExilePermanents
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ExilePermanents as ExilePermanents
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (ExilePermanents.ExilePermanents Keyword.Keyword)
codec = ExilePermanents.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExilePermanents" $ do
  -- Food Chain's one creature. The count is HOW MANY, matched
  -- exactly, Pawl.Codec.TapPermanents' key names over a different action.
  Spec.it s "MkExilePermanents" $
    Common.assertCodec
      s
      codec
      ( ExilePermanents.MkExilePermanents
          { ExilePermanents.count = 1,
            ExilePermanents.whichPermanents = Filter.HasCardType CardType.Creature
          }
      )
      " {\"count\":1,\"whichPermanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
