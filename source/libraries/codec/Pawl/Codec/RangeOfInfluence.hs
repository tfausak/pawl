module Pawl.Codec.RangeOfInfluence where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence

-- | Keyed by a PlayerId, so 'Common.naturalMap', as Pawl.Codec.Teams is.
codec :: Codec.Codec RangeOfInfluence.RangeOfInfluence
codec = Common.wrapper (Common.naturalMap PlayerId.codec Common.natural) RangeOfInfluence.MkRangeOfInfluence RangeOfInfluence.unwrap
