module Pawl.Codec.ExileHauntingSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ExileHaunting as ExileHaunting
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExileHaunting" $ do
  -- CR 702.55's haunt. BOTH fields are a SlotName, so the fixture names them
  -- differently on purpose: only an asymmetric case catches a codec that
  -- exiled the wrong object and haunted the wrong one.
  Spec.it s "MkExileHaunting, both keys" $
    Common.assertCodec
      s
      ExileHaunting.codec
      ( ExileHaunting.MkExileHaunting
          { ExileHaunting.card = SlotName.MkSlotName (Text.pack "haunter"),
            ExileHaunting.host = SlotName.MkSlotName (Text.pack "haunted")
          }
      )
      " {\"card\":\"haunter\",\"host\":\"haunted\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ExileHaunting.codec
