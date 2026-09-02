module Pawl.Codec.AimedAtSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.AimedAt as AimedAt
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AimedAt as AimedAt
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AimedAt" $ do
  Spec.it s "MkAimedAt, both keys" $
    Common.assertCodec
      s
      AimedAt.codec
      ( AimedAt.MkAimedAt
          { AimedAt.defenders = PlayerScope.You,
            AimedAt.kinds = Set.fromList [AttackTargetKind.OfPlayer, AttackTargetKind.OfPlaneswalker]
          }
      )
      " {\"defenders\":{\"type\":\"You\"},\"kinds\":[{\"type\":\"OfPlayer\"},{\"type\":\"OfPlaneswalker\"}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AimedAt.codec
