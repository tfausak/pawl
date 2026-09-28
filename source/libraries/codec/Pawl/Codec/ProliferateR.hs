{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ProliferateR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.ProliferateRewrite as ProliferateRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ProliferateR as ProliferateR

codec :: Codec.Codec ProliferateR.ProliferateR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec ProliferateR.whose
  rewrite <- Fields.required "rewrite" ProliferateRewrite.codec ProliferateR.rewrite
  pure
    ProliferateR.MkProliferateR
      { ProliferateR.whose = whose,
        ProliferateR.rewrite = rewrite
      }
