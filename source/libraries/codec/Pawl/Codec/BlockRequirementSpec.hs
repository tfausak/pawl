module Pawl.Codec.BlockRequirementSpec where

import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlockRequirement" $ do
  -- Lure's shape (CR 303.4m): the enchanted creature IS the attacker every
  -- creature able to block must block, so the subject axis is absent.
  Spec.it s "MkBlockRequirement" $
    Common.assertCodec
      s
      BlockRequirement.codec
      (BlockRequirement.MkBlockRequirement Nothing (Just Affected.Attached) Nothing)
      " {\"attacker\":{\"type\":\"Attached\"}} "
  -- Razorgrass Screen's shape: the requirement names its own source and no
  -- attacker, so the object axis is the absent one.
  Spec.it s "MkBlockRequirement with no attacker" $
    Common.assertCodec
      s
      BlockRequirement.codec
      (BlockRequirement.MkBlockRequirement (Just Affected.Attached) Nothing Nothing)
      " {\"subject\":{\"type\":\"Attached\"}} "
  -- Seton's Desire's shape: CR 509.1c's second reading, "or that it must block
  -- if some condition is met", written as CR 604.2's "as long as" clause over
  -- the cards in the controller's graveyard -- the same threshold clause
  -- Pawl.Codec.AttackRequirementSpec spells for Otarian Juggernaut.
  Spec.it s "a gated requirement" $
    Common.assertCodec
      s
      BlockRequirement.codec
      ( BlockRequirement.MkBlockRequirement
          Nothing
          (Just Affected.Attached)
          ( Just
              ( Condition.Compares
                  ( Compares.MkCompares
                      ( Quantity.Count
                          Count.MkCount
                            { Count.scope = Scope.InZone (InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.You)),
                              Count.filter = Filter.And [],
                              Count.aggregation = Aggregation.Members
                            }
                      )
                      Comparison.AtLeast
                      (Quantity.Literal 7)
                  )
              )
          )
      )
      " {\"attacker\":{\"type\":\"Attached\"},\"while\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Count\",\"value\":{\"aggregation\":{\"type\":\"Members\"},\"filter\":{\"type\":\"And\",\"value\":[]},\"scope\":{\"type\":\"InZone\",\"value\":{\"zone\":{\"type\":\"Graveyard\"},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}}}}},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":7}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockRequirement.codec
