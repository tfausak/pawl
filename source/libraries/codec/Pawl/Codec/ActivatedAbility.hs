module Pawl.Codec.ActivatedAbility where

import qualified Data.Text as Text
import qualified Pawl.Codec.ActivationTiming as ActivationTiming
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming

toJson :: (Eq card) => (card -> Value.Value) -> ActivatedAbility.ActivatedAbility card -> Value.Value
toJson codec aa =
  Common.object $
    [ Common.pair "cost" (Cost.toJson Keyword.toJson (ActivatedAbility.cost aa)),
      Common.pair "modal" (Modal.toJson codec (ActivatedAbility.modal aa))
    ]
      -- CR 307.5: emitted only for a restricted ability, so the absence of the
      -- key means "no timing rider" -- the same optional-field shape Card.enchant
      -- takes, and it leaves every card without one byte-identical.
      <> ( case ActivatedAbility.timing aa of
             ActivationTiming.AnyTime -> []
             _ -> [Common.pair "timing" (ActivationTiming.toJson (ActivatedAbility.timing aa))]
         )

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (ActivatedAbility.ActivatedAbility card)
fromJson decode value = do
  ps <- Common.asObject value
  c <- Common.field "cost" ps >>= Cost.fromJson Keyword.fromJson
  m <- Common.field "modal" ps >>= Modal.fromJson decode
  t <- case Common.optionalField "timing" ps of
    Nothing -> pure ActivationTiming.AnyTime
    Just v -> ActivationTiming.fromJson v
  pure (ActivatedAbility.MkActivatedAbility c m t)
