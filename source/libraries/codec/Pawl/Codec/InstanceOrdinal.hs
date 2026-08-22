module Pawl.Codec.InstanceOrdinal where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.InstanceOrdinal as InstanceOrdinal

codec :: Codec.Codec InstanceOrdinal.InstanceOrdinal
codec = Common.wrapper Common.natural InstanceOrdinal.MkInstanceOrdinal InstanceOrdinal.unwrap
