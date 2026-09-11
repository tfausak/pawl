module Pawl.Codec.CopySnapshotSpec where

import qualified Pawl.Codec.CopySnapshot as CopySnapshot
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CopySnapshot as CopySnapshot

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CopySnapshot" $ do
  Spec.it s "round-trips both normal and flipped readings" $
    Common.assertCodec
      s
      CopySnapshot.codec
      CopySnapshot.MkCopySnapshot
        { CopySnapshot.normal = ProjectedCharacteristicsSpec.testCharacteristics,
          CopySnapshot.flipped = Just ProjectedCharacteristicsSpec.testCharacteristics
        }
      ( "{\"normal\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"flipped\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "}"
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s CopySnapshot.codec
