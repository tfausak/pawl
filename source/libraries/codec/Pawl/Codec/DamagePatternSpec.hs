{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DamagePatternSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SourceRelation as SourceRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamagePattern" $ do
  -- A named kind, no source restriction, no recipient restriction.
  Spec.it s "a named kind" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern (Just DamageKind.Combat) SourceRelation.AnySource Nothing)
      """ {"whichKind":{"type":"Combat"}} """
  -- No kind, no source, no recipient -- every field elided at its default, so
  -- the pattern matches any damage instance whatsoever.
  Spec.it s "no kind (matches any)" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern Nothing SourceRelation.AnySource Nothing)
      """ {} """
  -- CR 614.15's keying: names the damage its own resolution is dealing, and
  -- says nothing about the kind.
  Spec.it s "the effect's own source (CR 614.15)" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern Nothing SourceRelation.TheSource Nothing)
      """ {"whichSource":{"type":"TheSource"}} """
  -- The permanent a shield covers (CR 615.7's, and CR 615.3's unbounded one),
  -- baked by Resolve's prevention arms and never authored on a card.
  Spec.it s "a shielded recipient (CR 615.7)" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern Nothing SourceRelation.AnySource (Just (Recipient.ToCreature (ObjectId.MkObjectId 7))))
      """ {"whichRecipient":{"type":"ToCreature","value":7}} """
