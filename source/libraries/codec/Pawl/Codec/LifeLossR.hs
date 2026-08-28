{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LifeLossR where

import qualified Pawl.Codec.LifeLossPattern as LifeLossPattern
import qualified Pawl.Codec.LifeLossRewrite as LifeLossRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LifeLossR as LifeLossR

codec :: Codec.Codec LifeLossR.LifeLossR
codec = Fields.object $ do
  matching <- Fields.required "matching" LifeLossPattern.codec LifeLossR.matching
  rewrite <- Fields.required "rewrite" LifeLossRewrite.codec LifeLossR.rewrite
  pure
    LifeLossR.MkLifeLossR
      { LifeLossR.matching = matching,
        LifeLossR.rewrite = rewrite
      }
