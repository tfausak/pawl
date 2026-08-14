{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TokenRSpec where

import qualified Pawl.Codec.TokenR as TokenR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TokenR" $ do
  -- CR 614.13: Doubling Season doubles the tokens you create.
  Spec.it s "MkTokenR" $
    Common.assertCodec
      s
      TokenR.codec
      ( TokenR.MkTokenR
          { TokenR.matching = TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours},
            TokenR.scaling = Scaling.Multiply 2
          }
      )
      """ {"matching":{"whose":{"type":"Yours"}},"scaling":{"type":"Multiply","value":2}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s TokenR.codec
