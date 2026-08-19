{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CounterR where

import qualified Pawl.Codec.CounterPattern as CounterPattern
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterR as CounterR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec CounterR.CounterR
codec = Fields.object $ do
  matching <- Fields.required "matching" CounterPattern.codec CounterR.matching
  scaling <- Fields.required "scaling" Scaling.codec CounterR.scaling
  pure
    CounterR.MkCounterR
      { CounterR.matching = matching,
        CounterR.scaling = scaling
      }
