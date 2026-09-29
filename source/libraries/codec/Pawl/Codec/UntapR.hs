{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.UntapR where

import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.UntapRewrite as UntapRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.UntapR as UntapR

codec :: Codec.Codec UntapR.UntapR
codec = Fields.object $ do
  during <- Fields.defaulted "during" Nothing (Common.maybe Phase.codec) UntapR.during
  rewrite <- Fields.required "rewrite" UntapRewrite.codec UntapR.rewrite
  pure
    UntapR.MkUntapR
      { UntapR.during = during,
        UntapR.rewrite = rewrite
      }
