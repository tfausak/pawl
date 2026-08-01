-- | The @ActivatedAbility ⇆ Json@ codec (#481).
module Pawl.Codec.ActivatedAbility where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivationTiming as ActivationTiming
import Pawl.Codec.Cost (costToJson, jsonToCost)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Modal (jsonToModal, modalToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming

activatedAbilityToJson :: (card -> Value) -> ActivatedAbility.ActivatedAbility card -> Value
activatedAbilityToJson codec aa =
  Json.jObject $
    [ (Text.pack "cost", costToJson (ActivatedAbility.cost aa)),
      (Text.pack "modal", modalToJson codec (ActivatedAbility.modal aa))
    ]
      -- CR 307.5: emitted only for a restricted ability, so the absence of the
      -- key means "no timing rider" -- the same optional-field shape Card.enchant
      -- takes, and it leaves every card without one byte-identical.
      <> ( case ActivatedAbility.timing aa of
             ActivationTiming.AnyTime -> []
             _ -> [(Text.pack "timing", ActivationTiming.toJson (ActivatedAbility.timing aa))]
         )

jsonToActivatedAbility :: (Value -> Either Text card) -> Value -> Either Text (ActivatedAbility.ActivatedAbility card)
jsonToActivatedAbility decode value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "cost") ps >>= jsonToCost
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal decode
  t <- case Json.optField (Text.pack "timing") ps of
    Nothing -> pure ActivationTiming.AnyTime
    Just v -> ActivationTiming.fromJson v
  pure (ActivatedAbility.MkActivatedAbility c m t)
