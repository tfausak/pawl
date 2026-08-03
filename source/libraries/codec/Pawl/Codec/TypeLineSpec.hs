{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TypeLineSpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TypeLine as TypeLine
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TypeLine as TypeLine

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TypeLine" $ do
  Spec.it s "MkTypeLine" $
    Common.assertJsonCodec
      s
      TypeLine.toJson
      TypeLine.fromJson
      (TypeLine.MkTypeLine (Set.singleton Supertype.Basic) (Set.singleton CardType.Land) (Set.singleton Subtype.Mountain))
      """ {"supertypes":[{"type":"Basic"}],"types":[{"type":"Land"}],"subtypes":[{"type":"Mountain"}]} """
  -- CR 308.1/308.2: the kindred shape -- two card types, and a CREATURE
  -- subtype on a card that is not a creature. Bitterblossom's type line,
  -- and the only one in the pool where the subtype's family and the
  -- card types disagree.
  Spec.it s "MkTypeLine (kindred)" $
    Common.assertJsonCodec
      s
      TypeLine.toJson
      TypeLine.fromJson
      (TypeLine.MkTypeLine Set.empty (Set.fromList [CardType.Kindred, CardType.Enchantment]) (Set.singleton Subtype.Faerie))
      """ {"types":[{"type":"Enchantment"},{"type":"Kindred"}],"subtypes":[{"type":"Faerie"}]} """
  -- CR 306.3 / 205.3j: Jace Beleren's, and the first type line whose
  -- subtype is a planeswalker type.
  Spec.it s "MkTypeLine (planeswalker)" $
    Common.assertJsonCodec
      s
      TypeLine.toJson
      TypeLine.fromJson
      (TypeLine.MkTypeLine (Set.singleton Supertype.Legendary) (Set.singleton CardType.Planeswalker) (Set.singleton Subtype.Jace))
      """ {"supertypes":[{"type":"Legendary"}],"types":[{"type":"Planeswalker"}],"subtypes":[{"type":"Jace"}]} """
  -- R6: the typeLine requirement only guards a truncated file if it reaches the
  -- content. An empty types set is the shape a half-written card file takes.
  Spec.it s "rejects an empty types set" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"types":[]} """) >>= TypeLine.fromJson))
      "expected a decode failure"
  Spec.it s "rejects an absent types key" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {} """) >>= TypeLine.fromJson))
      "expected a decode failure"
  Spec.it s "omits empty supertypes and subtypes" $
    Common.assertJsonCodec
      s
      TypeLine.toJson
      TypeLine.fromJson
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Sorcery) Set.empty)
      """ {"types":[{"type":"Sorcery"}]} """
