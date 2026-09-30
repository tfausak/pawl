module Pawl.Codec.BeholdSpec where

import qualified Pawl.Codec.Behold as Behold
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Behold as Behold
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Subtype as Subtype

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Behold.Behold Keyword.Keyword)
codec = Behold.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Behold" $ do
  -- Kindle the Inner Flame's three Elementals.
  Spec.it s "MkBehold" $
    Common.assertCodec
      s
      codec
      (Behold.MkBehold {Behold.count = 3, Behold.whichObjects = Filter.HasSubtype Subtype.Elemental})
      " {\"count\":3,\"whichObjects\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Elemental\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
