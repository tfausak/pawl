module Pawl.Codec.DrawSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Draw as Draw
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Draw" $ do
  -- Every draw in the pool but Shahrazad and Sindbad's looks back at nothing, so
  -- the slot key is absent here and present below.
  Spec.it s "MkDraw, no slot: the key is omitted" $
    Common.assertCodec
      s
      Draw.codec
      (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2) Nothing)
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "MkDraw, CR 121.1's drawn card remembered: the key is written" $
    Common.assertCodec
      s
      Draw.codec
      ( Draw.MkDraw
          (PlayerRef.Relative PlayerRelation.You)
          (Quantity.Literal 1)
          (Just (SlotName.MkSlotName (Text.pack "drawn")))
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"slot\":\"drawn\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Draw.codec
