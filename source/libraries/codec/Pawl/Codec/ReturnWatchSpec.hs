module Pawl.Codec.ReturnWatchSpec where

import qualified Pawl.Codec.ReturnWatch as ReturnWatch
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ReturnWatch as ReturnWatch
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReturnWatch" $ do
  -- CR 610.3 as Glorious Protector prints it: a creature exiled from the
  -- battlefield, watching the permanent whose ability exiled it.
  Spec.it s "a watch on a permanent, returning to the battlefield" $
    Common.assertCodec
      s
      ReturnWatch.codec
      ReturnWatch.MkReturnWatch
        { ReturnWatch.source = ObjectId.MkObjectId 3,
          ReturnWatch.zone = Zone.Battlefield
        }
      " {\"source\":3,\"zone\":{\"type\":\"Battlefield\"}} "
  -- The zone is recorded rather than assumed, so a move out of another zone
  -- round trips as the zone it came from (CR 610.3's "previous zone").
  Spec.it s "a watch on a card that came from a graveyard" $
    Common.assertCodec
      s
      ReturnWatch.codec
      ReturnWatch.MkReturnWatch
        { ReturnWatch.source = ObjectId.MkObjectId 7,
          ReturnWatch.zone = Zone.Graveyard
        }
      " {\"source\":7,\"zone\":{\"type\":\"Graveyard\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ReturnWatch.codec
