module Pawl.Codec.ControllerRelationSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControllerRelation" $ do
  Spec.it s "Yours" $
    Common.assertCodec
      s
      ControllerRelation.codec
      ControllerRelation.Yours
      " {\"type\":\"Yours\"} "
  Spec.it s "Anyones" $
    Common.assertCodec
      s
      ControllerRelation.codec
      ControllerRelation.Anyones
      " {\"type\":\"Anyones\"} "
  Spec.it s "Opponents" $
    Common.assertCodec
      s
      ControllerRelation.codec
      ControllerRelation.Opponents
      " {\"type\":\"Opponents\"} "
  Spec.it s "EnchantedPlayers" $
    Common.assertCodec
      s
      ControllerRelation.codec
      ControllerRelation.EnchantedPlayers
      " {\"type\":\"EnchantedPlayers\"} "
  -- CR 601.2c: Plagiarize's "target player".
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.InSlot (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"InSlot\",\"value\":\"target\"} "
  Spec.it s "Among" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Among (Set.fromList [PlayerId.MkPlayerId 1, PlayerId.MkPlayerId 2]))
      " {\"type\":\"Among\",\"value\":[1,2]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ControllerRelation.codec
