{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.LookAtSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.LookAt as LookAt
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LookAt" $ do
  -- Into the Wilds' "look at the top card of your library".
  Spec.it s "MkLookAt" $
    Common.assertCodec
      s
      LookAt.codec
      ( LookAt.MkLookAt
          (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) 1))
          (SlotName.MkSlotName (Text.pack "looked"))
      )
      """ {"ref":{"type":"TopOfLibrary","value":{"count":1,"player":{"type":"Relative","value":{"type":"You"}}}},"slot":"looked"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s LookAt.codec
