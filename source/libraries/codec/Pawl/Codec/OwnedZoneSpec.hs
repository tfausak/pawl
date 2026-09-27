module Pawl.Codec.OwnedZoneSpec where

import qualified Pawl.Codec.OwnedZone as OwnedZone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.OwnedZone as OwnedZone
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OwnedZone" $ do
  -- God-Eternal Oketra's "exile", the owner defaulted.
  Spec.it s "MkOwnedZone, any owner" $
    Common.assertCodec
      s
      OwnedZone.codec
      (OwnedZone.MkOwnedZone {OwnedZone.zone = Zone.Exile, OwnedZone.owner = PlayerRelation.AnyPlayer})
      " {\"zone\":{\"type\":\"Exile\"}} "
  -- Enigma Sphinx's "your graveyard".
  Spec.it s "MkOwnedZone, yours" $
    Common.assertCodec
      s
      OwnedZone.codec
      (OwnedZone.MkOwnedZone {OwnedZone.zone = Zone.Graveyard, OwnedZone.owner = PlayerRelation.You})
      " {\"owner\":{\"type\":\"You\"},\"zone\":{\"type\":\"Graveyard\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s OwnedZone.codec
