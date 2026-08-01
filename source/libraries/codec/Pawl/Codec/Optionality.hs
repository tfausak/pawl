module Pawl.Codec.Optionality where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Optionality as Optionality

toJson :: Optionality.Optionality -> Value.Value
toJson o = Common.nullary $ case o of
  Optionality.Mandatory -> "Mandatory"
  Optionality.Optional -> "Optional"

fromJson :: Value.Value -> Either Text.Text Optionality.Optionality
fromJson =
  Common.decodeNullary
    "Optionality"
    [ ("Mandatory", Optionality.Mandatory),
      ("Optional", Optionality.Optional)
    ]

-- | An omitted optionality decodes to Mandatory, the counterability posture (and
-- for the same reason): almost every mode in the corpus prints no "may", and a
-- required key would have meant editing every card file to say nothing.
fromJsonDefault :: Value.Value -> Either Text.Text Optionality.Optionality
fromJsonDefault value = case value of
  Value.Null _ -> Right Optionality.Mandatory
  _ -> fromJson value
