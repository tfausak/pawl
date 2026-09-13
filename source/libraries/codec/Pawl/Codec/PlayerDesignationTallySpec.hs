module Pawl.Codec.PlayerDesignationTallySpec where

import qualified Pawl.Codec.PlayerDesignationTally as PlayerDesignationTally
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerDesignation as PlayerDesignation
import qualified Pawl.Types.PlayerDesignationTally as PlayerDesignationTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerDesignationTally" $ do
  -- CR 702.131c / 702.195b: whether a player has one of the two rest-of-game
  -- marks -- a LEAF, so nothing here is a Quantity.
  Spec.it s "MkPlayerDesignationTally" $
    Common.assertCodec
      s
      PlayerDesignationTally.codec
      ( PlayerDesignationTally.MkPlayerDesignationTally
          { PlayerDesignationTally.player = PlayerRef.Relative PlayerRelation.You,
            PlayerDesignationTally.designation = PlayerDesignation.CitysBlessing
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"designation\":{\"type\":\"CitysBlessing\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerDesignationTally.codec
