module Pawl.Codec.MonarchWatchSpec where

import qualified Pawl.Codec.MonarchWatch as MonarchWatch
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MonarchWatch" $ do
  -- CR 725: the watch is armed and nothing has happened yet.
  Spec.it s "a watch nothing has satisfied" $
    Common.assertCodec
      s
      MonarchWatch.codec
      MonarchWatch.MkMonarchWatch
        { MonarchWatch.controller = PlayerId.MkPlayerId 1,
          MonarchWatch.due = False
        }
      " {\"controller\":1,\"due\":false} "
  -- The recorded EVENT, which is what the field is: a crown that went to an
  -- opponent and back inside one resolution still frees the prisoner (#208), so
  -- a state written between the crowning and the settle has to carry the true.
  Spec.it s "a watch an opponent's crowning has satisfied" $
    Common.assertCodec
      s
      MonarchWatch.codec
      MonarchWatch.MkMonarchWatch
        { MonarchWatch.controller = PlayerId.MkPlayerId 2,
          MonarchWatch.due = True
        }
      " {\"controller\":2,\"due\":true} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s MonarchWatch.codec
