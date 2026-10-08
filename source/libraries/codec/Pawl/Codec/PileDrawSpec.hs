module Pawl.Codec.PileDrawSpec where

import qualified Pawl.Codec.PileDraw as PileDraw
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Pile as Pile
import qualified Pawl.Types.PileDraw as PileDraw
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PileDraw" $ do
  -- CR 406.4.
  Spec.it s "round-trips" $
    Common.assertCodec
      s
      PileDraw.codec
      (PileDraw.MkPileDraw {PileDraw.pile = Pile.OfFaceDown (Timestamp.MkTimestamp 4), PileDraw.ordinal = 2})
      " {\"ordinal\":2,\"pile\":{\"type\":\"OfFaceDown\",\"value\":4}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PileDraw.codec
