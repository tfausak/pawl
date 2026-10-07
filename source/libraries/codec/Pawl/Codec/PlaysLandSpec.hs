module Pawl.Codec.PlaysLandSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlaysLand as PlaysLand
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlaysLand as PlaysLand
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlaysLand" $ do
  -- Urianger Augurelt's "whenever you play a land from exile".
  Spec.it s "MkPlaysLand, both keys" $
    Common.assertCodec
      s
      PlaysLand.codec
      ( PlaysLand.MkPlaysLand
          { PlaysLand.player = PlayerRelation.You,
            PlaysLand.from = Zone.Exile,
            PlaysLand.filter = Filter.IsBound (SlotName.MkSlotName (Text.pack "exiled"))
          }
      )
      " {\"player\":{\"type\":\"You\"},\"from\":{\"type\":\"Exile\"},\"filter\":{\"type\":\"IsBound\",\"value\":\"exiled\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlaysLand.codec
