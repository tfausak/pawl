{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.RedirectDamageSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.RedirectDamage as RedirectDamage
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RedirectDamage" $ do
  -- CR 614.9, Turn the Tables. 'from' and 'to' are both an ObjectRef, so the
  -- fixture names them differently on purpose: only an asymmetric case catches
  -- a codec that sent the damage back the way it came.
  Spec.it s "MkRedirectDamage, kind written" $
    Common.assertCodec
      s
      RedirectDamage.codec
      ( RedirectDamage.MkRedirectDamage
          { RedirectDamage.duration = Duration.UntilEndOfTurn,
            RedirectDamage.kind = Just DamageKind.Combat,
            RedirectDamage.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you")),
            RedirectDamage.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "attacker"))
          }
      )
      """ {"duration":{"type":"UntilEndOfTurn"},"kind":{"type":"Combat"},"from":{"type":"InSlot","value":"you"},"to":{"type":"InSlot","value":"attacker"}} """
  -- A redirect naming no kind, which is the elided case.
  Spec.it s "MkRedirectDamage, kind elided" $
    Common.assertCodec
      s
      RedirectDamage.codec
      ( RedirectDamage.MkRedirectDamage
          { RedirectDamage.duration = Duration.UntilEndOfTurn,
            RedirectDamage.kind = Nothing,
            RedirectDamage.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you")),
            RedirectDamage.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "attacker"))
          }
      )
      """ {"duration":{"type":"UntilEndOfTurn"},"from":{"type":"InSlot","value":"you"},"to":{"type":"InSlot","value":"attacker"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s RedirectDamage.codec
