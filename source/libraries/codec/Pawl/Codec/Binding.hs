-- | The @Binding ⇆ Json@ codec (#481).
module Pawl.Codec.Binding where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ModeIndex as ModeIndex
import Pawl.Codec.ProjectedCharacteristics (jsonToProjectedCharacteristics, projectedCharacteristicsToJson)
import Pawl.Codec.Recipient (jsonToRecipient, recipientToJson)
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Subtype as Subtype
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.SlotName as SlotName

-- Runtime-only, never in card JSON -- covered for the same reason SetController's
-- PlayerId is: the codec must stay total over the transitive closure of what the
-- game state carries.
bindingToJson :: Binding.Binding -> Value
bindingToJson b =
  Json.jObject
    [ (Text.pack "target", Json.maybeTo recipientToJson (Binding.target b)),
      (Text.pack "subtypes", Json.maybeTo (\(f, t) -> Array (MkArray [Subtype.toJson f, Subtype.toJson t])) (Binding.subtypes b)),
      (Text.pack "amount", Json.maybeTo Json.natTo (Binding.amount b)),
      (Text.pack "modes", Json.maybeTo (Json.setTo ModeIndex.toJson) (Binding.modes b)),
      (Text.pack "copy", Json.maybeTo projectedCharacteristicsToJson (Binding.copy b))
    ]

jsonToBinding :: Value -> Either Text Binding.Binding
jsonToBinding value = do
  ps <- Json.asObject value
  t <- Json.maybeFrom jsonToRecipient (Json.getOpt (Text.pack "target") ps)
  s <- Json.maybeFrom Subtype.fromJsonPair (Json.getOpt (Text.pack "subtypes") ps)
  a <- Json.maybeFrom Json.natFrom (Json.getOpt (Text.pack "amount") ps)
  m <- Json.maybeFrom (Json.setFrom ModeIndex.fromJson) (Json.getOpt (Text.pack "modes") ps)
  c <- Json.maybeFrom jsonToProjectedCharacteristics (Json.getOpt (Text.pack "copy") ps)
  pure
    Binding.MkBinding
      { Binding.target = t,
        Binding.subtypes = s,
        Binding.amount = a,
        Binding.modes = m,
        Binding.copy = c
      }

bindingsToJson :: Map.Map SlotName.SlotName Binding.Binding -> Value
bindingsToJson m =
  Json.listTo
    (\(k, v) -> Json.jObject [(Text.pack "slot", SlotName.toJson k), (Text.pack "binding", bindingToJson v)])
    (Map.toAscList m)

jsonToBindings :: Value -> Either Text (Map.Map SlotName.SlotName Binding.Binding)
jsonToBindings value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "slot") ps >>= SlotName.fromJson
        b <- Json.field (Text.pack "binding") ps >>= jsonToBinding
        pure (k, b)
   in Map.fromList <$> Json.listFrom decodeEntry value
