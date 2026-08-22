module Pawl.Codec.FaceDownCharacteristicsSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TypeLine as TypeLine

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FaceDownCharacteristics" $ do
  -- CR 708.2a's list, which is what every key defaults to, so the object is
  -- empty on the wire.
  Spec.it s "the CR 708.2a defaults write no keys" $
    Common.assertCodec
      s
      FaceDownCharacteristics.codec
      FaceDownCharacteristics.defaultValue
      " {} "
  -- Cyber Conversion's "it's a 2/2 Cyberman artifact creature": the type line is
  -- listed and the 2/2 is CR 708.2a's own, so only the one key is written.
  Spec.it s "a listed type line writes that key alone" $
    Common.assertCodec
      s
      FaceDownCharacteristics.codec
      FaceDownCharacteristics.defaultValue
        { FaceDownCharacteristics.typeLine =
            TypeLine.MkTypeLine
              { TypeLine.supertypes = Set.empty,
                TypeLine.types = Set.fromList [CardType.Artifact, CardType.Creature],
                TypeLine.subtypes = Set.singleton Subtype.Cyberman
              }
        }
      " {\"typeLine\":{\"subtypes\":[{\"type\":\"Cyberman\"}],\"types\":[{\"type\":\"Artifact\"},{\"type\":\"Creature\"}]}} "
  -- CR 702.168b's and CR 701.58a's ward {2}, the one listing in the rules that
  -- names a keyword -- minted by
  -- Pawl.Types.FaceDownCharacteristics.disguisedValue rather than by a card, and
  -- on the wire so the record round-trips whole.
  Spec.it s "a listed keyword writes that key alone" $
    Common.assertCodec
      s
      FaceDownCharacteristics.codec
      FaceDownCharacteristics.disguisedValue
      " {\"keywords\":[{\"type\":\"Ward\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s FaceDownCharacteristics.codec
