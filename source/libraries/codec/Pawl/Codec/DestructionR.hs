{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DestructionR where

import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DestructionR as DestructionR

codec :: Codec.Codec DestructionR.DestructionR
codec = Fields.object $ do
  matching <- Fields.defaulted "matching" Nothing (Common.maybe (Filter.codec Keyword.codec)) DestructionR.matching
  rewrite <- Fields.required "rewrite" DestructionRewrite.codec DestructionR.rewrite
  pure
    DestructionR.MkDestructionR
      { DestructionR.matching = matching,
        DestructionR.rewrite = rewrite
      }
