module Pawl.Codec.EmperorsSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.Emperors as Emperors
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Emperors as Emperors
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TeamId as TeamId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Emperors" $ do
  Spec.it s "no emperors" $
    Common.assertCodec
      s
      Emperors.codec
      Emperors.none
      " {} "

  -- CR 809.2: one emperor per team.
  Spec.it s "two emperors" $
    Common.assertCodec
      s
      Emperors.codec
      (Emperors.MkEmperors (Map.fromList [(TeamId.MkTeamId 0, PlayerId.MkPlayerId 1), (TeamId.MkTeamId 1, PlayerId.MkPlayerId 4)]))
      " {\"0\":1,\"1\":4} "

  Spec.it s "has a schema" $
    Common.assertHasSchema s Emperors.codec
