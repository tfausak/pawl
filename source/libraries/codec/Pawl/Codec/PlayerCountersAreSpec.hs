module Pawl.Codec.PlayerCountersAreSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerCountersAre as PlayerCountersAre
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind.Type
import qualified Pawl.Types.PlayerCountersAre as PlayerCountersAre.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerCountersAre" $ do
  Spec.it s "a player, a kind and a count" $
    Common.assertCodec s PlayerCountersAre.codec (PlayerCountersAre.Type.MkPlayerCountersAre (Label.Type.MkLabel (Text.pack "bob")) PlayerCounterKind.Type.Poison 3) " {\"player\":\"bob\",\"kind\":{\"type\":\"Poison\"},\"count\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayerCountersAre.codec
