{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DesignateSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Designate as Designate
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Designate" $ do
  -- CR 701.60 / CR 702.112: give the slot's object this designation.
  Spec.it s "MkDesignate, both keys" $
    Common.assertCodec
      s
      Designate.codec
      ( Designate.MkDesignate
          { Designate.designation = Designation.Suspected,
            Designate.slot = SlotName.MkSlotName (Text.pack "self")
          }
      )
      """ {"designation":{"type":"Suspected"},"slot":"self"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Designate.codec
