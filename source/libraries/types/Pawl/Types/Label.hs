module Pawl.Types.Label where

import qualified Data.Text as Text

-- | A name a scenario's board gives one seat or one object it places. Seats and
-- objects share the one namespace, so a single reference form names either.
newtype Label = MkLabel
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
