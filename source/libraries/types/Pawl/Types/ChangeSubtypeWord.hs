module Pawl.Types.ChangeSubtypeWord where

import qualified Pawl.Types.Subtype as Subtype

-- | The payload of Pawl.Types.Modification's ChangeSubtypeWord arm (#1305): CR
-- 612's text change, Artificial Evolution's "change one creature type to
-- another".
data ChangeSubtypeWord = MkChangeSubtypeWord
  { from :: Subtype.Subtype,
    to :: Subtype.Subtype
  }
  deriving (Eq, Ord, Show)
