{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SacrificeSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Sacrifice as Sacrifice
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Subtype as Subtype

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Sacrifice.Sacrifice Keyword.Keyword)
codec = Sacrifice.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Sacrifice" $ do
  -- CR 701.21a: Fireblast's two Mountains. The count is HOW MANY, matched exactly.
  Spec.it s "MkSacrifice" $
    Common.assertCodec
      s
      codec
      ( Sacrifice.MkSacrifice
          { Sacrifice.count = 2,
            Sacrifice.whichPermanents = Filter.HasSubtype Subtype.Mountain
          }
      )
      """ {"count":2,"whichPermanents":{"type":"HasSubtype","value":{"type":"Mountain"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
