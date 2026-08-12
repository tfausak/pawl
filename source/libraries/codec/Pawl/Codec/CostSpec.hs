{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CostSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation
-- anywhere in the pool.
toJson :: Cost.Cost Keyword.Keyword -> Value.Value
toJson = Cost.toJson Keyword.toJson

fromJson :: Value.Value -> Either Text.Text (Cost.Cost Keyword.Keyword)
fromJson = Cost.fromJson Keyword.fromJson

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Cost" $ do
  Spec.it s "MkCost, with a mana part and components" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Cost.MkCost
        { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
          Cost.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
        }
      """ {"mana":[{"type":"Generic","value":4}],"components":[{"type":"TapThis"},{"type":"SacrificeThis"}]} """
  -- CR 118.5a: {0} is a real, payable cost, and ManaCost's empty list IS {0}.
  Spec.it s "MkCost, {0} and no components" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost []), Cost.components = []}
      """ {"mana":[]} """
  -- CR 118.6: Nothing (unpayable) and Just (MkManaCost []) ({0}) are both real,
  -- distinct values, so 'mana' is REQUIRED rather than defaulted -- an omitted
  -- key has no single value it could mean. A card file that lost its mana field
  -- must fail to load rather than silently become unpayable.
  Spec.it s "an omitted mana field is a decode error" $
    Spec.assertBool
      s
      (Either.isLeft (fromJson (Value.object [Value.pair "components" (Value.array [])])))
      "expected a decode failure"
  -- Every field at once: an unpayable cost (CR 118.6) with no components.
  -- 'mana' is required, so Nothing still writes as an explicit null; only the
  -- 'components' key is omitted.
  Spec.it s "an all-default value still writes the required mana key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Cost.MkCost {Cost.mana = Nothing, Cost.components = []}
      """ {"mana":null} """
