module Pawl.Codec.TypeLine where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TypeLine as TypeLine

toJson :: TypeLine.TypeLine -> Value.Value
toJson tl =
  Common.object
    [ Common.pair "supertypes" . Common.encodeSet Supertype.toJson $ TypeLine.supertypes tl,
      Common.pair "types" . Common.encodeSet CardType.toJson $ TypeLine.types tl,
      Common.pair "subtypes" . Common.encodeSet Subtype.toJson $ TypeLine.subtypes tl
    ]

fromJson :: Value.Value -> Either Text.Text TypeLine.TypeLine
fromJson value = do
  ps <- Common.asObject value
  sup <- Common.field "supertypes" ps >>= Common.decodeSet Supertype.fromJson
  tys <- Common.field "types" ps >>= Common.decodeSet CardType.fromJson
  sub <- Common.field "subtypes" ps >>= Common.decodeSet Subtype.fromJson
  pure (TypeLine.MkTypeLine sup tys sub)
