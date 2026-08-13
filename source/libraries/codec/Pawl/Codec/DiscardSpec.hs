{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DiscardSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Discard as Discard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Discard" $ do
  -- CR 701.8: the players the slot names each discard this many.
  Spec.it s "MkDiscard, both keys" $
    Common.assertCodec
      s
      Discard.codec
      ( Discard.MkDiscard
          { Discard.slot = SlotName.MkSlotName (Text.pack "player"),
            Discard.quantity = Quantity.Literal 1
          }
      )
      """ {"slot":"player","quantity":{"type":"Literal","value":1}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Discard.codec
