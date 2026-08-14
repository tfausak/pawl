{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TopOfLibrarySpec where

import qualified Pawl.Codec.TopOfLibrary as TopOfLibrary
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TopOfLibrary" $ do
  -- CR 401.1.
  Spec.it s "MkTopOfLibrary, both keys" $
    Common.assertCodec
      s
      TopOfLibrary.codec
      ( TopOfLibrary.MkTopOfLibrary
          { TopOfLibrary.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibrary.count = 3
          }
      )
      """ {"player":{"type":"Relative","value":{"type":"You"}},"count":3} """
  Spec.it s "has a schema" $ Common.assertHasSchema s TopOfLibrary.codec
