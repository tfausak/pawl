{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MillCountR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.MillCountRewrite as MillCountRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MillCountR as MillCountR

codec :: Codec.Codec MillCountR.MillCountR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec MillCountR.whose
  rewrite <- Fields.required "rewrite" MillCountRewrite.codec MillCountR.rewrite
  pure
    MillCountR.MkMillCountR
      { MillCountR.whose = whose,
        MillCountR.rewrite = rewrite
      }
