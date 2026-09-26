module Pawl.Codec.BackupSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Backup as Backup
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Backup as Backup
import qualified Pawl.Types.Keyword as Keyword

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Backup.Backup Keyword.Keyword)
codec = Backup.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Backup" $ do
  -- CR 702.165a: backup printed first, so nothing sits above it.
  Spec.it s "MkBackup with nothing above" $
    Common.assertCodec
      s
      codec
      (Backup.MkBackup {Backup.count = 2, Backup.printedAbove = Set.empty})
      " {\"count\":2} "
  Spec.it s "MkBackup with flash above" $
    Common.assertCodec
      s
      codec
      (Backup.MkBackup {Backup.count = 1, Backup.printedAbove = Set.singleton Keyword.Flash})
      " {\"count\":1,\"printedAbove\":[{\"type\":\"Flash\"}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
