module Pawl.Codec.ClauseIndex where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ClauseIndex as ClauseIndex

codec :: Codec.Codec ClauseIndex.ClauseIndex
codec = Common.wrapper Common.natural ClauseIndex.MkClauseIndex ClauseIndex.unwrap
