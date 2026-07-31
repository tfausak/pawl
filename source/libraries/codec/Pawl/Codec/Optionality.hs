-- | The @Optionality ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Optionality where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value (Null))
import qualified Pawl.Types.Optionality as Optionality

optionalityToJson :: Optionality.Optionality -> Value
optionalityToJson o = Json.nullary . Text.pack $ case o of
  Optionality.Mandatory -> "Mandatory"
  Optionality.Optional -> "Optional"

jsonToOptionality :: Value -> Either Text Optionality.Optionality
jsonToOptionality =
  Json.decodeNullary
    (Text.pack "Optionality")
    [ (Text.pack "Mandatory", Optionality.Mandatory),
      (Text.pack "Optional", Optionality.Optional)
    ]

-- An omitted optionality decodes to Mandatory, the counterability posture (and
-- for the same reason): almost every mode in the corpus prints no "may", and a
-- required key would have meant editing every card file to say nothing.
jsonToOptionalityDefault :: Value -> Either Text Optionality.Optionality
jsonToOptionalityDefault value = case value of
  Null _ -> Right Optionality.Mandatory
  _ -> jsonToOptionality value
