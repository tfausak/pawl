module Pawl.Codec.ControllerRelationSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControllerRelation" $ do
  Spec.it s "You" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Related PlayerRelation.You)
      " {\"type\":\"You\"} "
  Spec.it s "AnyPlayer" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Related PlayerRelation.AnyPlayer)
      " {\"type\":\"AnyPlayer\"} "
  Spec.it s "Opponent" $
    Common.assertCodec
      s
      ControllerRelation.codec
      (ControllerRelation.Related PlayerRelation.Opponent)
      " {\"type\":\"Opponent\"} "
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
  -- Exhaustive over the relation, whose arms the codec derives; see
  -- Pawl.Codec.PlayerScopeSpec's twin.
  Spec.it s "speaks every relation under the relation's own tag" $
    mapM_
      (\relation -> Common.assertCodec s ControllerRelation.codec (ControllerRelation.Related relation) (" {\"type\":\"" <> show relation <> "\"} "))
      [minBound .. maxBound :: PlayerRelation.PlayerRelation]
  Spec.it s "has a schema" $
    Common.assertHasSchema s ControllerRelation.codec
