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
  Spec.it s "a named kind" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern (Just DamageKind.Combat) SourceRelation.AnySource Nothing)
      """ {"whichKind":{"type":"Combat"},"whichSource":{"type":"AnySource"},"whichRecipient":null} """
  Spec.it s "no kind (matches any)" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern Nothing SourceRelation.AnySource Nothing)
      """ {"whichKind":null,"whichSource":{"type":"AnySource"},"whichRecipient":null} """
  -- CR 614.15's keying: Galvanic Blast's metalcraft clause names the damage its
  -- own resolution is dealing, and says nothing about the kind.
  Spec.it s "the effect's own source (CR 614.15)" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern Nothing SourceRelation.TheSource Nothing)
      """ {"whichKind":null,"whichSource":{"type":"TheSource"},"whichRecipient":null} """
  -- CR 615.7's shielded permanent, baked by Resolve's PreventNextDamage arm from
  -- the resolution's chosen target and never authored on a card (see
  -- Pawl.Codec.DamagePattern).
  Spec.it s "a shielded recipient (CR 615.7)" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern Nothing SourceRelation.AnySource (Just (Recipient.ToCreature (ObjectId.MkObjectId 7))))
      """ {"whichKind":null,"whichSource":{"type":"AnySource"},"whichRecipient":{"type":"ToCreature","value":7}} """
