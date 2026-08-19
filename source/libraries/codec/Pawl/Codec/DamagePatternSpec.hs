module Pawl.Codec.DamagePatternSpec where

import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamagePattern" $ do
  -- A named kind, no source restriction, no recipient restriction.
  Spec.it s "a named kind" $
    Common.assertCodec
      s
      DamagePattern.codec
      (DamagePattern.MkDamagePattern (Just DamageKind.Combat) (Filter.And []) Nothing Nothing Nothing)
      " {\"whichKind\":{\"type\":\"Combat\"}} "
  -- No kind, no source, no recipient -- every field elided at its default, so
  -- the pattern matches any damage instance whatsoever.
  Spec.it s "no kind (matches any)" $
    Common.assertCodec
      s
      DamagePattern.codec
      (DamagePattern.MkDamagePattern Nothing (Filter.And []) Nothing Nothing Nothing)
      " {} "
  -- CR 614.15's keying: names the damage its own resolution is dealing, and
  -- says nothing about the kind.
  Spec.it s "the effect's own source (CR 614.15)" $
    Common.assertCodec
      s
      DamagePattern.codec
      (DamagePattern.MkDamagePattern Nothing Filter.IsSource Nothing Nothing Nothing)
      " {\"whatSource\":{\"type\":\"IsSource\"}} "
  -- The permanent a shield covers (CR 615.7's, and CR 615.3's unbounded one),
  -- baked by Resolve's prevention arms and never authored on a card.
  Spec.it s "a shielded recipient (CR 615.7)" $
    Common.assertCodec
      s
      DamagePattern.codec
      (DamagePattern.MkDamagePattern Nothing (Filter.And []) Nothing (Just (Recipient.ToCreature (ObjectId.MkObjectId 7))) Nothing)
      " {\"whichRecipient\":{\"type\":\"ToCreature\",\"value\":7}} "
  -- The recipient a CARD describes rather than one the engine baked -- Stormwild
  -- Capridor's "to this creature".
  Spec.it s "a described recipient (CR 615.1)" $
    Common.assertCodec
      s
      DamagePattern.codec
      (DamagePattern.MkDamagePattern (Just DamageKind.Noncombat) (Filter.And []) (Just Filter.IsSource) Nothing Nothing)
      " {\"whichKind\":{\"type\":\"Noncombat\"},\"whatRecipient\":{\"type\":\"IsSource\"}} "
  -- CR 609.7a's chosen source, baked as an id by Resolve's prevention arms and
  -- never authored either -- Healing Grace's "a source of your choice", whose
  -- printed properties are none, so the trivial filter rides beside the id.
  Spec.it s "a chosen source (CR 609.7a)" $
    Common.assertCodec
      s
      DamagePattern.codec
      (DamagePattern.MkDamagePattern Nothing (Filter.And []) Nothing Nothing (Just (ObjectId.MkObjectId 9)))
      " {\"whichSource\":9} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DamagePattern.codec
