module Pawl.Types.ModifyTarget where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's ModifyTarget arm (#1305): apply this
-- modification to the objects the ObjectRef names, for this duration.
data ModifyTarget = MkModifyTarget
  { duration :: Duration.Duration,
    modification :: Modification.Modification,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
