module Pawl.Codec.CardsPutIntoZoneSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.CardsPutIntoZone as CardsPutIntoZone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardsPutIntoZone as CardsPutIntoZone
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CardsPutIntoZone" $ do
  -- Dutiful Knowledge Seeker's payload: "cards" into a library from anywhere.
  Spec.it s "MkCardsPutIntoZone, from anywhere" $
    Common.assertCodec
      s
      CardsPutIntoZone.codec
      ( CardsPutIntoZone.MkCardsPutIntoZone
          { CardsPutIntoZone.filter = Filter.Not Filter.IsToken,
            CardsPutIntoZone.from = Set.empty,
            CardsPutIntoZone.to = Zone.Library
          }
      )
      " {\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"IsToken\"}},\"to\":{\"type\":\"Library\"}} "
  Spec.it s "MkCardsPutIntoZone, from named zones" $
    Common.assertCodec
      s
      CardsPutIntoZone.codec
      ( CardsPutIntoZone.MkCardsPutIntoZone
          { CardsPutIntoZone.filter = Filter.And [],
            CardsPutIntoZone.from = Set.fromList [Zone.Graveyard],
            CardsPutIntoZone.to = Zone.Hand
          }
      )
      " {\"filter\":{\"type\":\"And\",\"value\":[]},\"from\":[{\"type\":\"Graveyard\"}],\"to\":{\"type\":\"Hand\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CardsPutIntoZone.codec
