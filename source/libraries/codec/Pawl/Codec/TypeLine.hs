module Pawl.Codec.TypeLine where

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TypeLine as TypeLine

toJson :: TypeLine.TypeLine -> Value.Value
toJson tl =
  Common.object . concat $
    [ Common.optionalPair "supertypes" Set.empty (Common.encodeSet Supertype.toJson) (TypeLine.supertypes tl),
      Common.requiredPair "types" (Common.encodeSet CardType.toJson) (TypeLine.types tl),
      Common.optionalPair "subtypes" Set.empty (Common.encodeSet Subtype.toJson) (TypeLine.subtypes tl)
    ]

fromJson :: Value.Value -> Either Text.Text TypeLine.TypeLine
fromJson value = do
  ps <- Common.asObject value
  sup <- Common.defaultedField "supertypes" Set.empty (Common.decodeSet Supertype.fromJson) ps
  tys <- Common.field "types" ps >>= Common.decodeSet CardType.fromJson
  -- CR 205.1 gives a card at least one card type unconditionally, unlike its
  -- subtypes and supertypes, so an empty set is a malformed file rather than a
  -- card with no types.
  Monad.when (Set.null tys) . Left $ Text.pack "typeLine has no types"
  sub <- Common.defaultedField "subtypes" Set.empty (Common.decodeSet Subtype.fromJson) ps
  pure (TypeLine.MkTypeLine sup tys sub)
