module Pawl.Types.AttachAll where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 701.3a: attach every object `subjects` names to ONE permanent
-- `destination` admits, chosen as the effect resolves (Glamer Spinners, Balan).
-- A Filter.CanHostSubject in `destination` asks whether the candidate can host
-- EVERY subject, Glamer Spinners' 2008-05-01 ruling.
data AttachAll = MkAttachAll
  { subjects :: ObjectRef.ObjectRef,
    destination :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
