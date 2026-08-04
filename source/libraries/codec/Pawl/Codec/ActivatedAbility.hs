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

-- | CR 117.1b's unrestricted case, which is what every ability without a timing
-- rider means; CR 307.5 is the narrower carve-out.
defaultTiming :: ActivationTiming.ActivationTiming
defaultTiming = ActivationTiming.AnyTime

toJson :: (Eq card) => (card -> Value.Value) -> ActivatedAbility.ActivatedAbility card -> Value.Value
toJson codec aa =
  Common.object
    ( Common.requiredPair "cost" (Cost.toJson Keyword.toJson) (ActivatedAbility.cost aa)
        <> Common.requiredPair "modal" (Modal.toJson codec) (ActivatedAbility.modal aa)
        -- CR 307.5: emitted only for a restricted ability, so the absence of
        -- the key means "no timing rider".
        <> Common.optionalPair "timing" defaultTiming ActivationTiming.toJson (ActivatedAbility.timing aa)
    )

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (ActivatedAbility.ActivatedAbility card)
fromJson decode value = do
  ps <- Common.asObject value
  c <- Common.field "cost" ps >>= Cost.fromJson Keyword.fromJson
  m <- Common.field "modal" ps >>= Modal.fromJson decode
  t <- Common.defaultedField "timing" defaultTiming ActivationTiming.fromJson ps
  pure (ActivatedAbility.MkActivatedAbility c m t)
