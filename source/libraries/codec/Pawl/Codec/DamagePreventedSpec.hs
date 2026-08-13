{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DamagePreventedSpec where

import qualified Pawl.Codec.DamagePrevented as DamagePrevented
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamagePrevented" $ do
  -- CR 615.1: how much a shield stopped, and who it was headed for.
  Spec.it s "MkDamagePrevented, both keys" $
    Common.assertCodec
      s
      DamagePrevented.codec
      ( DamagePrevented.MkDamagePrevented
          { DamagePrevented.recipient = Recipient.ToPlayer (PlayerId.MkPlayerId 0),
            DamagePrevented.amount = 3
          }
      )
      """ {"recipient":{"type":"ToPlayer","value":0},"amount":3} """
  Spec.it s "has a schema" $ Common.assertHasSchema s DamagePrevented.codec
