{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamagePattern where

import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 615.10's "if A SOURCE would deal damage" names no source in particular,
-- which is what a pattern that says nothing about the source means -- the
-- trivial predicate, as for ZoneChangePattern.whatObject.
defaultWhatSource :: Filter.Filter Keyword.Keyword
defaultWhatSource = Filter.And []

-- | `whichRecipient` is runtime-only -- the permanent or player a shield covers
-- (CR 615.7's, and CR 615.3's unbounded one) is baked by Resolve's prevention
-- arms, never authored on a card -- but
-- this codec is structural over the record and so accepts one from card JSON.
-- A corpus lint keeps the pool honest instead, as for PhasePattern's
-- `whosePhase`.
codec :: Codec.Codec DamagePattern.DamagePattern
codec = Fields.object $ do
  whichKind <- Fields.defaulted "whichKind" Nothing (Common.maybe DamageKind.codec) DamagePattern.whichKind
  whatSource <- Fields.defaulted "whatSource" defaultWhatSource (Filter.codec Keyword.codec) DamagePattern.whatSource
  whatRecipient <- Fields.defaulted "whatRecipient" Nothing (Common.maybe (Filter.codec Keyword.codec)) DamagePattern.whatRecipient
  whichRecipient <- Fields.defaulted "whichRecipient" Nothing (Common.maybe Recipient.codec) DamagePattern.whichRecipient
  pure
    DamagePattern.MkDamagePattern
      { DamagePattern.whichKind = whichKind,
        DamagePattern.whatSource = whatSource,
        DamagePattern.whatRecipient = whatRecipient,
        DamagePattern.whichRecipient = whichRecipient
      }
