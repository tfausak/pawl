module Pawl.Codec.RevealedSpec where

import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Codec.Revealed as Revealed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Revealed" $ do
  -- CR 701.20a through CR 702.94a's miracle reveal.
  Spec.it s "MkRevealed, every key" $
    Common.assertCodec
      s
      Revealed.codec
      ( Revealed.MkRevealed
          { Revealed.player = PlayerId.MkPlayerId 0,
            Revealed.card = ObjectId.MkObjectId 7,
            Revealed.cause = RevealCause.ForMiracle,
            Revealed.characteristics = ProjectedCharacteristicsSpec.testCharacteristics
          }
      )
      ( "{\"player\":0,\"card\":7,\"cause\":{\"type\":\"ForMiracle\"},\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "}"
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s Revealed.codec
