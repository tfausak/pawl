{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.KeywordFamilySpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.KeywordFamily as KeywordFamily
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.KeywordFamily as KeywordFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.KeywordFamily" $ do
  Spec.it s "Hexproof" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Hexproof
      """ {"type":"Hexproof"} """

  Spec.it s "Landwalk" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Landwalk
      """ {"type":"Landwalk"} """

  Spec.it s "Cycling" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Cycling
      """ {"type":"Cycling"} """

  Spec.it s "Flashback" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Flashback
      """ {"type":"Flashback"} """

  Spec.it s "Morph" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Morph
      """ {"type":"Morph"} """

  Spec.it s "Entwine" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Entwine
      """ {"type":"Entwine"} """

  Spec.it s "Bushido" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Bushido
      """ {"type":"Bushido"} """

  Spec.it s "Poisonous" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Poisonous
      """ {"type":"Poisonous"} """

  Spec.it s "Renown" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Renown
      """ {"type":"Renown"} """

  Spec.it s "Annihilator" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Annihilator
      """ {"type":"Annihilator"} """

  Spec.it s "Outlast" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Outlast
      """ {"type":"Outlast"} """

  Spec.it s "Crew" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Crew
      """ {"type":"Crew"} """

  Spec.it s "Rampage" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Rampage
      """ {"type":"Rampage"} """

  Spec.it s "Afflict" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Afflict
      """ {"type":"Afflict"} """

  Spec.it s "Toxic" $
    Common.assertJsonCodec
      s
      KeywordFamily.toJson
      KeywordFamily.fromJson
      KeywordFamily.Toxic
      """ {"type":"Toxic"} """

  -- The family tag and the written instance's tag share a name and must stay
  -- distinguishable on the wire: `{"type":"Toxic"}` is CR 702.164's ability and
  -- `{"type":"Toxic","value":2}` is what a card prints. Pinned here rather than
  -- left to the Filter arms, since this is a property of the two keyword codecs
  -- and not of the atoms that carry them.
  Spec.it s "the family tag is not the instance's tag" $
    Spec.assertBool
      s
      (KeywordFamily.toJson KeywordFamily.Toxic /= Common.tagged "Toxic" (Just (Common.integer 2)))
      "the toxic family is not toxic 2"
