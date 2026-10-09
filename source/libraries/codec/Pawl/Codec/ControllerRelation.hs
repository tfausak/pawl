module Pawl.Codec.ControllerRelation where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ControllerRelation as ControllerRelation

codec :: Codec.Codec ControllerRelation.ControllerRelation
codec =
  Arm.tagged
    tagOf
    ( PlayerRelation.arms ControllerRelation.Related
        <> [ Arm.nullary "EnchantedPlayers" ControllerRelation.EnchantedPlayers,
             Arm.payload "InSlot" SlotName.codec ControllerRelation.InSlot (\x -> case x of ControllerRelation.InSlot y -> Just y; _ -> Nothing),
             Arm.payload "Among" (Common.set PlayerId.codec) ControllerRelation.Among (\x -> case x of ControllerRelation.Among y -> Just y; _ -> Nothing)
           ]
    )

tagOf :: ControllerRelation.ControllerRelation -> String
tagOf x = case x of
  ControllerRelation.Related relation -> PlayerRelation.tag relation
  ControllerRelation.EnchantedPlayers {} -> "EnchantedPlayers"
  ControllerRelation.InSlot {} -> "InSlot"
  ControllerRelation.Among {} -> "Among"
