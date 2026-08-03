module Pawl.Codec.DamagePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.SourceRelation as SourceRelation
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.DamagePattern as DamagePattern

-- | `whichRecipient` is runtime-only -- CR 615.7's shielded permanent or player
-- is baked by Resolve's PreventNextDamage arm, never authored on a card -- but
-- this codec is structural over the record and so accepts one from card JSON.
-- The same treatment, and the same reason, as PhasePattern's `whosePhase`;
-- Pawl.CardSpec's "no card authors a recipient-scoped damage pattern" is what
-- keeps the pool honest.
toJson :: DamagePattern.DamagePattern -> Value.Value
toJson p =
  Common.object
    [ Common.pair "whichKind" . Common.encodeMaybe DamageKind.toJson $ DamagePattern.whichKind p,
      Common.pair "whichSource" . SourceRelation.toJson $ DamagePattern.whichSource p,
      Common.pair "whichRecipient" . Common.encodeMaybe Recipient.toJson $ DamagePattern.whichRecipient p
    ]

fromJson :: Value.Value -> Either Text.Text DamagePattern.DamagePattern
fromJson value = do
  ps <- Common.asObject value
  k <- Common.field "whichKind" ps >>= Common.decodeMaybe DamageKind.fromJson
  src <- Common.field "whichSource" ps >>= SourceRelation.fromJson
  r <- Common.field "whichRecipient" ps >>= Common.decodeMaybe Recipient.fromJson
  pure
    DamagePattern.MkDamagePattern
      { DamagePattern.whichKind = k,
        DamagePattern.whichSource = src,
        DamagePattern.whichRecipient = r
      }
