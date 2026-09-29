module Pawl.Types.Casting where

import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.Reference as Reference

-- | CR 601.2: casting one spell, and the choices its casting asks.
data Casting = MkCasting
  { object :: Reference.Reference,
    choices :: Choices.Choices
  }
  deriving (Eq, Ord, Show)
