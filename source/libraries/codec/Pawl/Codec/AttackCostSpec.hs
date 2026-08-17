-- Covers Pawl.Codec.AttackCost and the two codecs only it dispatches to,
-- Pawl.Codec.AttackCostScope and Pawl.Codec.PerAttacker. They share a spec
-- rather than taking one each because neither is reachable on the wire except
-- through the object below, so the pair of round trips here is the whole of
-- what a card can say.
module Pawl.Codec.AttackCostSpec where

import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.Codec.AttackCostScope as AttackCostScope
import qualified Pawl.Codec.PerAttacker as PerAttacker
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackCostScope as AttackCostScope
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.PerAttacker as PerAttacker
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackCost" $ do
  -- The subject is every creature, since only a creature can be declared as an
  -- attacker, and the cost is ONE attacker's share (CR 508.1h totals them).
  Spec.it s "MkAttackCost" $
    Common.assertCodec
      s
      AttackCost.codec
      ( AttackCost.MkAttackCost
          (Affected.Matching (Filter.HasCardType CardType.Creature))
          (PerAttacker.Fixed (ManaCost.MkManaCost [ManaSymbol.Generic 2]))
          AttackCostScope.Controller
      )
      " {\"perAttacker\":{\"type\":\"Fixed\",\"value\":[{\"type\":\"Generic\",\"value\":2}]},\"scope\":{\"type\":\"Controller\"},\"subject\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- Sphere of Safety's half of the type: the wide scope and a counted share, the
  -- two arms Ghostly Prison's literal above does not reach.
  Spec.it s "MkAttackCost with a counted share and the wider scope" $
    Common.assertCodec
      s
      AttackCost.codec
      ( AttackCost.MkAttackCost
          (Affected.Matching (Filter.HasCardType CardType.Creature))
          ( PerAttacker.Counted
              ( Quantity.Count
                  ( Count.MkCount
                      (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
                      (Filter.And [Filter.HasCardType CardType.Enchantment, Filter.ControlledBy PlayerRelation.You])
                      Aggregation.Members
                  )
              )
          )
          AttackCostScope.ControllerAndPlaneswalkers
      )
      " {\"perAttacker\":{\"type\":\"Counted\",\"value\":{\"type\":\"Count\",\"value\":{\"aggregation\":{\"type\":\"Members\"},\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Enchantment\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}]},\"scope\":{\"type\":\"InZone\",\"value\":{\"player\":{\"type\":\"EachPlayer\"},\"zone\":{\"type\":\"Battlefield\"}}}}}},\"scope\":{\"type\":\"ControllerAndPlaneswalkers\"},\"subject\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- Exhaustive where the two literals above are representative: Arm.enum derives
  -- the scope's arm list from the type, so this is what would catch an arm the
  -- derivation missed or two that encode alike.
  Spec.it s "the scope round trips every constructor" $ Common.assertEnumCodec s AttackCostScope.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackCost.codec
  Spec.it s "the scope has a schema" $ Common.assertHasSchema s AttackCostScope.codec
  Spec.it s "the share has a schema" $ Common.assertHasSchema s PerAttacker.codec
