{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DamageRewriteSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageRewrite as DamageRewrite
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageRewrite" $ do
  Spec.it s "PreventAll" $
    Common.assertJsonCodec
      s
      DamageRewrite.toJson
      DamageRewrite.fromJson
      DamageRewrite.PreventAll
      """ {"type":"PreventAll"} """
  -- CR 122.1c's prevention half. Minted from a permanent's shield counters and
  -- never authored on a card, so this codec is the only place its wire form is
  -- pinned.
  Spec.it s "PreventRemovingShieldCounter" $
    Common.assertJsonCodec
      s
      DamageRewrite.toJson
      DamageRewrite.fromJson
      DamageRewrite.PreventRemovingShieldCounter
      """ {"type":"PreventRemovingShieldCounter"} """
  -- CR 615.7's shield, whose Natural is what REMAINS of it. Baked by Resolve's
  -- PreventNextDamage arm and never authored on a card, so this codec is the
  -- only place the wire form is pinned.
  Spec.it s "PreventNext" $
    Common.assertJsonCodec
      s
      DamageRewrite.toJson
      DamageRewrite.fromJson
      (DamageRewrite.PreventNext 4)
      """ {"type":"PreventNext","value":4} """
  -- CR 614.1a: a flat instead-amount.
  Spec.it s "SetAmount" $
    Common.assertJsonCodec
      s
      DamageRewrite.toJson
      DamageRewrite.fromJson
      (DamageRewrite.SetAmount 4)
      """ {"type":"SetAmount","value":4} """
  -- A doubling, which is Scaling's Multiply 2.
  Spec.it s "Scale" $
    Common.assertJsonCodec
      s
      DamageRewrite.toJson
      DamageRewrite.fromJson
      (DamageRewrite.Scale (Scaling.Multiply 2))
      """ {"type":"Scale","value":{"type":"Multiply","value":2}} """
  -- CR 614.9's redirection. Baked by Resolve's RedirectDamage arm and never
  -- authored on a card -- card data cannot name an ObjectId -- so this codec is
  -- the replay path's, not a card's.
  Spec.it s "Redirect" $
    Common.assertJsonCodec
      s
      DamageRewrite.toJson
      DamageRewrite.fromJson
      (DamageRewrite.Redirect (Recipient.ToCreature (ObjectId.MkObjectId 7)))
      """ {"type":"Redirect","value":{"type":"ToCreature","value":7}} """
