module Pawl.Codec.PlayerScope where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayerScope as PlayerScope

codec :: Codec.Codec PlayerScope.PlayerScope
codec =
  Arm.tagged
    tagOf
    ( PlayerRelation.arms PlayerScope.Related
        <> [Arm.nullary "ControllingMostPermanents" PlayerScope.ControllingMostPermanents]
    )

tagOf :: PlayerScope.PlayerScope -> String
tagOf x = case x of
  PlayerScope.Related relation -> PlayerRelation.tag relation
  PlayerScope.ControllingMostPermanents {} -> "ControllingMostPermanents"
