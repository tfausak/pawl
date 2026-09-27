{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EnteringTogether where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EnteringTogether as EnteringTogether

-- | Both fields defaulted to empty, so a batch nothing has entered yet is `{}`.
codec :: Codec.Codec EnteringTogether.EnteringTogether
codec = Fields.object $ do
  arrivals <- Fields.defaulted "arrivals" Seq.empty (Common.seq ObjectId.codec) EnteringTogether.arrivals
  minted <- Fields.defaulted "minted" Seq.empty (Common.seq ObjectId.codec) EnteringTogether.minted
  pure
    EnteringTogether.MkEnteringTogether
      { EnteringTogether.arrivals = arrivals,
        EnteringTogether.minted = minted
      }
