module Pawl.Types.Taking where

import qualified Data.Text as Text
import qualified Pawl.Types.Choices as Choices

-- | CR 116.2 / 605.3a: taking one other action at priority, named as the
-- Offered view renders it, and the choices it asks.
data Taking = MkTaking
  { action :: Text.Text,
    choices :: Choices.Choices
  }
  deriving (Eq, Ord, Show)
