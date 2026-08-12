module Pawl.Codec.DamagePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.SourceRelation as SourceRelation
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.SourceRelation as SourceRelation

-- | CR 615.10's "if A SOURCE would deal damage" names no source in particular,
-- which is what a pattern that says nothing about the source means.
defaultWhichSource :: SourceRelation.SourceRelation
defaultWhichSource = SourceRelation.AnySource

-- | `whichRecipient` is runtime-only -- the permanent or player a shield covers
-- (CR 615.7's, and CR 615.3's unbounded one) is baked by Resolve's prevention
-- arms, never authored on a card -- but
-- this codec is structural over the record and so accepts one from card JSON.
-- A corpus lint keeps the pool honest instead, as for PhasePattern's
-- `whosePhase`.
toJson :: DamagePattern.DamagePattern -> Value.Value
toJson p =
  Value.object
    ( Common.optionalPair "whichKind" Nothing (Common.encodeMaybe (Codec.encode DamageKind.codec)) (DamagePattern.whichKind p)
        <> Common.optionalPair "whichSource" defaultWhichSource (Codec.encode SourceRelation.codec) (DamagePattern.whichSource p)
        <> Common.optionalPair "whichRecipient" Nothing (Common.encodeMaybe Recipient.toJson) (DamagePattern.whichRecipient p)
    )

fromJson :: Value.Value -> Either Text.Text DamagePattern.DamagePattern
fromJson value = do
  ps <- Common.asObject value
  k <- Common.defaultedField "whichKind" Nothing (Common.decodeMaybe (Codec.decode DamageKind.codec)) ps
  src <- Common.defaultedField "whichSource" defaultWhichSource (Codec.decode SourceRelation.codec) ps
  r <- Common.defaultedField "whichRecipient" Nothing (Common.decodeMaybe Recipient.fromJson) ps
  pure
    DamagePattern.MkDamagePattern
      { DamagePattern.whichKind = k,
        DamagePattern.whichSource = src,
        DamagePattern.whichRecipient = r
      }
