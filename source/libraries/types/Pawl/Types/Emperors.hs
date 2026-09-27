module Pawl.Types.Emperors where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TeamId as TeamId

-- | CR 809.2: each team's emperor. Empty is every game that is not the Emperor
-- variant.
--
-- Keyed by TEAM, because CR 809.2 gives each team exactly one emperor; a map
-- from team cannot name two.
newtype Emperors = MkEmperors
  { unwrap :: Map.Map TeamId.TeamId PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)

-- | A game that is not the Emperor variant.
none :: Emperors
none = MkEmperors Map.empty

-- | Is this player their team's emperor?
isEmperor :: Emperors -> PlayerId.PlayerId -> Bool
isEmperor emperors pid = pid `elem` Map.elems (unwrap emperors)
