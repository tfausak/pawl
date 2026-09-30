module Pawl.Codec.TurnedFaceUpSpec where

import qualified Pawl.Codec.TurnedFaceUp as TurnedFaceUp
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.TurnedFaceUp as TurnedFaceUp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnedFaceUp" $ do
  -- CR 107.3d: the X chosen for the cost, written out.
  Spec.it s "MkTurnedFaceUp with a chosen X" $
    Common.assertCodec
      s
      TurnedFaceUp.codec
      (TurnedFaceUp.MkTurnedFaceUp {TurnedFaceUp.object = ObjectId.MkObjectId 5, TurnedFaceUp.announcedX = Just 2})
      "{\"object\":5,\"announcedX\":2}"
  -- A zero X is a choice and survives the round trip, apart from the absent key.
  Spec.it s "MkTurnedFaceUp with X chosen as zero" $
    Common.assertCodec
      s
      TurnedFaceUp.codec
      (TurnedFaceUp.MkTurnedFaceUp {TurnedFaceUp.object = ObjectId.MkObjectId 5, TurnedFaceUp.announcedX = Just 0})
      "{\"object\":5,\"announcedX\":0}"
  -- A road up with no X in its cost leaves the key out.
  Spec.it s "MkTurnedFaceUp with no X" $
    Common.assertCodec
      s
      TurnedFaceUp.codec
      (TurnedFaceUp.MkTurnedFaceUp {TurnedFaceUp.object = ObjectId.MkObjectId 5, TurnedFaceUp.announcedX = Nothing})
      "{\"object\":5}"
  Spec.it s "has a schema" $ Common.assertHasSchema s TurnedFaceUp.codec
