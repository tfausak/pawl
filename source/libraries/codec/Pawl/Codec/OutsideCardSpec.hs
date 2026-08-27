module Pawl.Codec.OutsideCardSpec where

import qualified Pawl.Codec.OutsideCard as OutsideCard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.PrintingId as PrintingId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OutsideCard" $ do
  -- CR 103.2a.
  Spec.it s "InPool" $
    Common.assertCodec
      s
      OutsideCard.codec
      (OutsideCard.InPool (PrintingId.MkPrintingId 2))
      " {\"type\":\"InPool\",\"value\":2} "
  -- CR 729.4.
  Spec.it s "InAnotherGame" $
    Common.assertCodec
      s
      OutsideCard.codec
      (OutsideCard.InAnotherGame (ObjectId.MkObjectId 3))
      " {\"type\":\"InAnotherGame\",\"value\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s OutsideCard.codec
