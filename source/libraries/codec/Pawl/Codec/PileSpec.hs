module Pawl.Codec.PileSpec where

import qualified Pawl.Codec.Pile as Pile
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Pile as Pile
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Pile" $ do
  -- CR 702.143e.
  Spec.it s "OfForetold" $
    Common.assertCodec
      s
      Pile.codec
      (Pile.OfForetold (Timestamp.MkTimestamp 7))
      " {\"type\":\"OfForetold\",\"value\":7} "
  -- CR 406.4.
  Spec.it s "OfFaceDown" $
    Common.assertCodec
      s
      Pile.codec
      (Pile.OfFaceDown (PlayerId.MkPlayerId 1))
      " {\"type\":\"OfFaceDown\",\"value\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Pile.codec
