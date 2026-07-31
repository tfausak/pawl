-- | The @TypeLine ⇆ Json@ codec (#481).
module Pawl.Codec.TypeLine where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.CardType (cardTypeToJson, jsonToCardType)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Subtype (jsonToSubtype, subtypeToJson)
import Pawl.Codec.Supertype (jsonToSupertype, supertypeToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.TypeLine as TypeLine

typeLineToJson :: TypeLine.TypeLine -> Value
typeLineToJson tl =
  Json.jObject
    [ (Text.pack "supertypes", Json.setTo supertypeToJson (TypeLine.supertypes tl)),
      (Text.pack "types", Json.setTo cardTypeToJson (TypeLine.types tl)),
      (Text.pack "subtypes", Json.setTo subtypeToJson (TypeLine.subtypes tl))
    ]

jsonToTypeLine :: Value -> Either Text TypeLine.TypeLine
jsonToTypeLine value = do
  ps <- Json.asObject value
  sup <- Json.field (Text.pack "supertypes") ps >>= Json.setFrom jsonToSupertype
  tys <- Json.field (Text.pack "types") ps >>= Json.setFrom jsonToCardType
  sub <- Json.field (Text.pack "subtypes") ps >>= Json.setFrom jsonToSubtype
  pure (TypeLine.MkTypeLine sup tys sub)
