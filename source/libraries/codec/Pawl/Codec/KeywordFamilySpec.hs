{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.KeywordFamilySpec where

import qualified Pawl.Codec.KeywordFamily as KeywordFamily
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.KeywordFamily as KeywordFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.KeywordFamily" $ do
  Spec.it s "Hexproof" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Hexproof
      """ {"type":"Hexproof"} """

  Spec.it s "Landwalk" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Landwalk
      """ {"type":"Landwalk"} """

  Spec.it s "Cycling" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Cycling
      """ {"type":"Cycling"} """

  Spec.it s "Flashback" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Flashback
      """ {"type":"Flashback"} """

  Spec.it s "Morph" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Morph
      """ {"type":"Morph"} """

  Spec.it s "Kicker" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Kicker
      """ {"type":"Kicker"} """

  Spec.it s "Entwine" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Entwine
      """ {"type":"Entwine"} """

  Spec.it s "Bushido" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Bushido
      """ {"type":"Bushido"} """

  Spec.it s "Modular" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Modular
      """ {"type":"Modular"} """

  Spec.it s "Vanishing" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Vanishing
      """ {"type":"Vanishing"} """

  Spec.it s "Poisonous" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Poisonous
      """ {"type":"Poisonous"} """

  Spec.it s "Renown" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Renown
      """ {"type":"Renown"} """

  Spec.it s "Afterlife" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Afterlife
      """ {"type":"Afterlife"} """

  Spec.it s "Soulshift" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Soulshift
      """ {"type":"Soulshift"} """

  Spec.it s "Reinforce" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Reinforce
      """ {"type":"Reinforce"} """

  Spec.it s "Annihilator" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Annihilator
      """ {"type":"Annihilator"} """

  Spec.it s "Outlast" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Outlast
      """ {"type":"Outlast"} """

  Spec.it s "Crew" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Crew
      """ {"type":"Crew"} """

  Spec.it s "Fabricate" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Fabricate
      """ {"type":"Fabricate"} """

  Spec.it s "Rampage" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Rampage
      """ {"type":"Rampage"} """

  Spec.it s "Afflict" $
    Common.assertCodec
      s
      KeywordFamily.codec
      KeywordFamily.Afflict
      """ {"type":"Afflict"} """

  Spec.it s "Toxic" $
    Common.assertCodec
      s
      KeywordFamily.codec
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
      (Codec.encode KeywordFamily.codec KeywordFamily.Toxic /= Common.tagged "Toxic" (Just (Value.integer 2)))
      "the toxic family is not toxic 2"

  Spec.it s "has a schema" $
    Common.assertHasSchema s KeywordFamily.codec
