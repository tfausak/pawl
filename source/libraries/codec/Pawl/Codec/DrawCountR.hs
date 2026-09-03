{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DrawCountR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.DrawCountRewrite as DrawCountRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DrawCountR as DrawCountR

codec :: Codec.Codec DrawCountR.DrawCountR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec DrawCountR.whose
  atLeast <- Fields.required "atLeast" Common.natural DrawCountR.atLeast
  rewrite <- Fields.required "rewrite" DrawCountRewrite.codec DrawCountR.rewrite
  pure
    DrawCountR.MkDrawCountR
      { DrawCountR.whose = whose,
        DrawCountR.atLeast = atLeast,
        DrawCountR.rewrite = rewrite
      }
