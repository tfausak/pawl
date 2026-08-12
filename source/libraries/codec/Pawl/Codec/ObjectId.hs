module Pawl.Codec.ObjectId where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ObjectId as ObjectId

codec :: Codec.Codec ObjectId.ObjectId
codec = Common.wrapper Common.natural ObjectId.MkObjectId ObjectId.unwrap
