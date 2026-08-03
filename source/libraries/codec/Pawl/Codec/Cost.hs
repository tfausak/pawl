module Pawl.Codec.Cost where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Cost as Cost

-- | The keyword codec is a PARAMETER, mirroring CostComponent's own -- see
-- Pawl.Codec.Filter's header: the CostComponents this carries need one, and
-- every caller passes Pawl.Codec.Keyword.toJson. The 'Eq' constraint is only
-- for 'Common.optionalPair' on 'components'; every instantiation in the pool is
-- at 'Pawl.Types.Keyword.Keyword', which has one.
toJson :: (Eq keyword) => (keyword -> Value.Value) -> Cost.Cost keyword -> Value.Value
toJson encode c =
  Common.object
    ( Common.optionalPair "mana" Nothing (Common.encodeMaybe ManaCost.toJson) (Cost.mana c)
        <> Common.optionalPair "components" [] (Common.encodeList (CostComponent.toJson encode)) (Cost.components c)
    )

-- | CR 118.6: an ABSENT mana field decodes to Nothing -- an unpayable cost -- and
-- never to {0}. Every ability-bearing card file states its mana part explicitly
-- (`[]` for {0}), so the absent case is only ever reached by a malformed file.
fromJson :: (Value.Value -> Either Text.Text keyword) -> Value.Value -> Either Text.Text (Cost.Cost keyword)
fromJson decode value = do
  ps <- Common.asObject value
  m <- Common.defaultedField "mana" Nothing (Common.decodeMaybe ManaCost.fromJson) ps
  cs <- Common.defaultedField "components" [] (Common.decodeList (CostComponent.fromJson decode)) ps
  pure Cost.MkCost {Cost.mana = m, Cost.components = cs}
