module Pawl.Codec.Cost where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Cost as Cost

toJson :: Cost.Cost -> Value.Value
toJson c =
  Common.object
    [ Common.pair "mana" (Common.encodeMaybe ManaCost.toJson (Cost.mana c)),
      Common.pair "components" (Common.encodeList CostComponent.toJson (Cost.components c))
    ]

-- | CR 118.6: an ABSENT mana field decodes to Nothing -- an unpayable cost -- and
-- never to {0}. Every ability-bearing card file states its mana part explicitly
-- (@[]@ for {0}), so the absent case is only ever reached by a malformed file.
fromJson :: Value.Value -> Either Text.Text Cost.Cost
fromJson value = do
  ps <- Common.asObject value
  m <- Common.decodeMaybe ManaCost.fromJson (Common.nullableField "mana" ps)
  cs <- Common.decodeListDefault CostComponent.fromJson (Common.nullableField "components" ps)
  pure Cost.MkCost {Cost.mana = m, Cost.components = cs}
