{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TypeLine where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TypeLine as TypeLine

-- | CR 205.1 gives a card at least one card type unconditionally, unlike its
-- subtypes and supertypes, so an empty set is a malformed file rather than a
-- card with no types.
hasTypes :: TypeLine.TypeLine -> Either Text.Text TypeLine.TypeLine
hasTypes tl =
  if Set.null (TypeLine.types tl)
    then Left (Text.pack "typeLine has no types")
    else Right tl

codec :: Codec.Codec TypeLine.TypeLine
codec = Fields.objectWith hasTypes $ do
  supertypes <- Fields.defaulted "supertypes" Set.empty (Common.set Supertype.codec) TypeLine.supertypes
  types <- Fields.required "types" (Common.set CardType.codec) TypeLine.types
  subtypes <- Fields.defaulted "subtypes" Set.empty (Common.set Subtype.codec) TypeLine.subtypes
  pure
    TypeLine.MkTypeLine
      { TypeLine.supertypes = supertypes,
        TypeLine.types = types,
        TypeLine.subtypes = subtypes
      }
