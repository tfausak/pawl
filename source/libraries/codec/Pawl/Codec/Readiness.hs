module Pawl.Codec.Readiness where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Readiness as Readiness

-- | Ready as true, for a document people write by hand: @"ready": true@.
codec :: Codec.Codec Readiness.Readiness
codec =
  Common.boolean
    { Codec.encode = \x -> Codec.encode Common.boolean (x == Readiness.Ready),
      Codec.decode = \value -> do
        b <- Codec.decode Common.boolean value
        pure (if b then Readiness.Ready else Readiness.Sick)
    }
