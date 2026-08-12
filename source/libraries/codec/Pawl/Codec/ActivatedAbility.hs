module Pawl.Codec.ActivatedAbility where

import qualified Data.Text as Text
import qualified Pawl.Codec.ActivationRestriction as ActivationRestriction
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility

toJson :: (Eq card) => (card -> Value.Value) -> ActivatedAbility.ActivatedAbility card -> Value.Value
toJson codec aa =
  Value.object
    ( Common.requiredPair "cost" (Codec.encode (Cost.codec Keyword.codec)) (ActivatedAbility.cost aa)
        <> Common.requiredPair "modal" (Modal.toJson codec) (ActivatedAbility.modal aa)
        -- CR 602.5: emitted only for a restricted ability, so the absence of the
        -- key is CR 602.2's default -- no "activate only ..." rider at all.
        <> Common.optionalPair "restrictions" [] (Common.encodeList ActivationRestriction.toJson) (ActivatedAbility.restrictions aa)
        -- CR 702.178a: emitted only for a GRANTED ability, so the absence of the
        -- key means the object simply has this ability.
        <> Common.optionalPair "condition" Nothing (Common.encodeMaybe Condition.toJson) (ActivatedAbility.condition aa)
    )

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (ActivatedAbility.ActivatedAbility card)
fromJson decode value = do
  ps <- Common.asObject value
  c <- Common.field "cost" ps >>= Codec.decode (Cost.codec Keyword.codec)
  m <- Common.field "modal" ps >>= Modal.fromJson decode
  t <- Common.defaultedField "restrictions" [] (Common.decodeList ActivationRestriction.fromJson) ps
  g <- Common.defaultedField "condition" Nothing (Common.decodeMaybe Condition.fromJson) ps
  pure (ActivatedAbility.MkActivatedAbility c m t g)
