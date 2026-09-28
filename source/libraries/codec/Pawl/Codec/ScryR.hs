{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ScryR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.ScryRewrite as ScryRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ScryR as ScryR

codec :: Codec.Codec ScryR.ScryR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec ScryR.whose
  rewrite <- Fields.required "rewrite" ScryRewrite.codec ScryR.rewrite
  pure
    ScryR.MkScryR
      { ScryR.whose = whose,
        ScryR.rewrite = rewrite
      }
