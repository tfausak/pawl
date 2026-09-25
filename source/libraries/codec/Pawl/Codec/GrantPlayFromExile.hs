{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.GrantPlayFromExile where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ManaSpending as ManaSpending
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PermissionVerb as PermissionVerb
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.PermissionVerb as PermissionVerb.Type
import qualified Pawl.Types.PlayerRef as PlayerRef.Type
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, with each rider elided when
-- it is the default -- Pawl.Codec.OfferCast's posture with its CastOffer, and
-- for the same reason: a permission that says nothing about mana is the ordinary
-- one, so a key belongs only on the card that prints CR 118.14's clause or CR
-- 118.9's. `player` defaults for that reason too: CR 109.5's "you" is what a
-- permission naming no seat means, and Pawl.Codec.OfferCast's `caster` takes the
-- same default.
codec :: Codec.Codec GrantPlayFromExile.GrantPlayFromExile
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec GrantPlayFromExile.duration
  player <- Fields.defaulted "player" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec GrantPlayFromExile.player
  ref <- Fields.required "ref" ObjectRef.codec GrantPlayFromExile.ref
  spending <- Fields.defaulted "spending" ManaSpending.AsProduced ManaSpending.codec GrantPlayFromExile.spending
  withoutPayingManaCost <- Fields.defaulted "withoutPayingManaCost" False Common.boolean GrantPlayFromExile.withoutPayingManaCost
  verb <- Fields.defaulted "verb" PermissionVerb.Type.Play PermissionVerb.codec GrantPlayFromExile.verb
  pure
    GrantPlayFromExile.MkGrantPlayFromExile
      { GrantPlayFromExile.duration = duration,
        GrantPlayFromExile.player = player,
        GrantPlayFromExile.ref = ref,
        GrantPlayFromExile.spending = spending,
        GrantPlayFromExile.withoutPayingManaCost = withoutPayingManaCost,
        GrantPlayFromExile.verb = verb
      }
