-- | The @ObjectId ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.ObjectId where

import Data.Text (Text)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ObjectId as ObjectId

objectIdToJson :: ObjectId.ObjectId -> Value
objectIdToJson (ObjectId.MkObjectId n) = Json.natTo n

jsonToObjectId :: Value -> Either Text ObjectId.ObjectId
jsonToObjectId value = ObjectId.MkObjectId <$> Json.natFrom value
