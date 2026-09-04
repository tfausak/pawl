{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LifeGainR where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.LifeGainRewrite as LifeGainRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LifeGainR as LifeGainR

codec :: Codec.Codec LifeGainR.LifeGainR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec LifeGainR.whose
  rewrite <- Fields.required "rewrite" LifeGainRewrite.codec LifeGainR.rewrite
  pure
    LifeGainR.MkLifeGainR
      { LifeGainR.whose = whose,
        LifeGainR.rewrite = rewrite
      }
