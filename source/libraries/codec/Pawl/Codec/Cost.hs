module Pawl.Codec.Cost where

import qualified Data.Text as Text
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Cost as Cost

-- | The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header. The 'Eq'
-- constraint is only for 'Common.optionalPair' on 'components'.
toJson :: (Eq keyword) => (keyword -> Value.Value) -> Cost.Cost keyword -> Value.Value
toJson encode c =
  Value.object
    ( Common.requiredPair "mana" (Common.encodeMaybe ManaCost.toJson) (Cost.mana c)
        <> Common.optionalPair "components" [] (Common.encodeList (CostComponent.toJson encode)) (Cost.components c)
    )

-- | CR 118.6: 'mana' is REQUIRED, not defaulted, despite being a 'Maybe' --
-- Nothing and Just (MkManaCost []) are both real, distinct values, so there is
-- no single default an absent key could mean. A card file that forgets 'mana'
-- is malformed rather than unpayable-by-default.
fromJson :: (Value.Value -> Either Text.Text keyword) -> Value.Value -> Either Text.Text (Cost.Cost keyword)
fromJson decode value = do
  ps <- Common.asObject value
  m <- Common.field "mana" ps >>= Common.decodeMaybe ManaCost.fromJson
  cs <- Common.defaultedField "components" [] (Common.decodeList (CostComponent.fromJson decode)) ps
  pure Cost.MkCost {Cost.mana = m, Cost.components = cs}
