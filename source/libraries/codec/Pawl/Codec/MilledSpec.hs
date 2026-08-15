{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.MilledSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.Milled as Milled
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Milled" $ do
  -- Two cards in one event, which is CR 701.17a's "all at once" and the reason
  -- the payload holds a sequence rather than one card.
  Spec.it s "MkMilled" $
    Common.assertCodec
      s
      Milled.codec
      ( Milled.MkMilled
          { Milled.player = PlayerId.MkPlayerId 0,
            Milled.cards = Seq.fromList [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8]
          }
      )
      """ {"player":0,"cards":[7,8]} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Milled.codec
