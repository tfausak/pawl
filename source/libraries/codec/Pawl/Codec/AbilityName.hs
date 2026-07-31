-- | The @AbilityName ⇆ Json@ codec (#481).
module Pawl.Codec.AbilityName where

import Data.Text (Text)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.AbilityName as AbilityName

abilityNameToJson :: AbilityName.AbilityName -> Value
abilityNameToJson (AbilityName.MkAbilityName t) = Json.jText t

jsonToAbilityName :: Value -> Either Text AbilityName.AbilityName
jsonToAbilityName value = AbilityName.MkAbilityName <$> Json.asText value
