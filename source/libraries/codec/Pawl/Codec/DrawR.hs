{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DrawR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.DrawRewrite as DrawRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DrawR as DrawR

codec :: Codec.Codec DrawR.DrawR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec DrawR.whose
  rewrite <- Fields.required "rewrite" DrawRewrite.codec DrawR.rewrite
  pure
    DrawR.MkDrawR
      { DrawR.whose = whose,
        DrawR.rewrite = rewrite
      }
