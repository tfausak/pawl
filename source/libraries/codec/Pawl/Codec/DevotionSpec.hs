module Pawl.Codec.DevotionSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Devotion as Devotion
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Devotion as Devotion
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Devotion" $ do
  -- CR 700.5: whose devotion and to which colours -- a LEAF, so nothing here is
  -- a Quantity.
  Spec.it s "MkDevotion" $
    Common.assertCodec
      s
      Devotion.codec
      ( Devotion.MkDevotion
          { Devotion.player = PlayerRef.Relative PlayerRelation.You,
            Devotion.colors = Set.fromList [Color.White, Color.Black]
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"colors\":[{\"type\":\"White\"},{\"type\":\"Black\"}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Devotion.codec
