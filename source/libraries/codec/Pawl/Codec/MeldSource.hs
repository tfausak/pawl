{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MeldSource where

import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MeldSource as MeldSource

-- | Common.nonEmpty rejects an empty array, so the wire cannot describe a
-- melded permanent that no card represents.
codec :: Codec.Codec MeldSource.MeldSource
codec = Fields.object $ do
  result <- Fields.required "result" PrintingId.codec MeldSource.result
  components <- Fields.required "components" (Common.nonEmpty PrintingId.codec) MeldSource.components
  pure
    MeldSource.MkMeldSource
      { MeldSource.result = result,
        MeldSource.components = components
      }
