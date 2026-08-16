{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TurnFaceDownSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.TurnFaceDown as TurnFaceDown
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TypeLine as TypeLine

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnFaceDown" $ do
  -- Backslide: CR 708.2a supplies the list, so the key is omitted.
  Spec.it s "MkTurnFaceDown, no listed characteristics: the key is omitted" $
    Common.assertCodec
      s
      TurnFaceDown.codec
      (TurnFaceDown.MkTurnFaceDown (SlotName.MkSlotName (Text.pack "target")) FaceDownCharacteristics.defaultValue)
      """ {"slot":"target"} """
  -- Cyber Conversion: CR 708.2's listed set, written.
  Spec.it s "MkTurnFaceDown, a listed set: the key is written" $
    Common.assertCodec
      s
      TurnFaceDown.codec
      ( TurnFaceDown.MkTurnFaceDown
          (SlotName.MkSlotName (Text.pack "target"))
          FaceDownCharacteristics.defaultValue
            { FaceDownCharacteristics.typeLine =
                TypeLine.MkTypeLine
                  { TypeLine.supertypes = Set.empty,
                    TypeLine.types = Set.fromList [CardType.Artifact, CardType.Creature],
                    TypeLine.subtypes = Set.singleton Subtype.Cyberman
                  }
            }
      )
      """ {"characteristics":{"typeLine":{"subtypes":[{"type":"Cyberman"}],"types":[{"type":"Artifact"},{"type":"Creature"}]}},"slot":"target"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s TurnFaceDown.codec
