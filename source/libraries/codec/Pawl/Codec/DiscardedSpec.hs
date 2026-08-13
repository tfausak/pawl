{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DiscardedSpec where

import qualified Pawl.Codec.Discarded as Discarded
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Discarded" $ do
  -- CR 702.29a's cycle, which is the case the cause field exists for: one act
  -- that must answer both "was a card discarded?" and "was it cycled?".
  Spec.it s "MkDiscarded, a cycle" $
    Common.assertCodec
      s
      Discarded.codec
      ( Discarded.MkDiscarded
          { Discarded.player = PlayerId.MkPlayerId 0,
            Discarded.card = ObjectId.MkObjectId 7,
            Discarded.cause = DiscardCause.ToPayCyclingCost
          }
      )
      """ {"player":0,"card":7,"cause":{"type":"ToPayCyclingCost"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Discarded.codec
