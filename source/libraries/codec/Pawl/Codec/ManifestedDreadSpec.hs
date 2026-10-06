module Pawl.Codec.ManifestedDreadSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.ManifestedDread as ManifestedDread
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManifestedDread as ManifestedDread
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManifestedDread" $ do
  Spec.it s "MkManifestedDread" $
    Common.assertCodec
      s
      ManifestedDread.codec
      ( ManifestedDread.MkManifestedDread
          { ManifestedDread.player = PlayerId.MkPlayerId 0,
            ManifestedDread.cards = Seq.fromList [ObjectId.MkObjectId 7]
          }
      )
      " {\"player\":0,\"cards\":[7]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManifestedDread.codec
