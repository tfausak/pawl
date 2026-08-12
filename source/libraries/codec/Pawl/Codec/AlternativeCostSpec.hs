{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AlternativeCostSpec where

import qualified Pawl.Codec.AlternativeCost as AlternativeCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AlternativeCost" $ do
  -- CR 118.9's ordinary case, Fireblast's: no condition, so the key is omitted.
  Spec.it s "an unconditioned alternative omits the condition key" $
    Common.assertJsonCodec
      s
      AlternativeCost.toJson
      AlternativeCost.fromJson
      AlternativeCost.MkAlternativeCost
        { AlternativeCost.condition = Nothing,
          AlternativeCost.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost []), Cost.components = []}
        }
      """ {"cost":{"mana":[]}} """
  -- CR 604.2's "as long as" clause on one,
  -- Asmoranomardicadaistinaculdacar's: the condition is the payload a recursive
  -- decoder could lose, so it is round-tripped alongside a mana part that is not
  -- {0}.
  Spec.it s "a conditioned alternative round-trips its condition" $
    Common.assertJsonCodec
      s
      AlternativeCost.toJson
      AlternativeCost.fromJson
      AlternativeCost.MkAlternativeCost
        { AlternativeCost.condition =
            Just
              ( Condition.Compares
                  (Quantity.CardsDiscardedThisTurn (PlayerRef.Relative PlayerRelation.You))
                  Comparison.AtLeast
                  (Quantity.Literal 1)
              ),
          AlternativeCost.cost =
            Cost.MkCost
              { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Hybrid (ManaType.Colored Color.Black) (ManaType.Colored Color.Red)]),
                Cost.components = []
              }
        }
      """ {"condition":{"measured":{"type":"CardsDiscardedThisTurn","value":{"type":"Relative","value":{"type":"You"}}},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":1}},"cost":{"mana":[{"type":"Hybrid","value":[{"type":"Colored","value":{"type":"Black"}},{"type":"Colored","value":{"type":"Red"}}]}]}} """
