module Pawl.Types.Activation where

import Numeric.Natural (Natural)
import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.Reference as Reference

-- | CR 602.2: activating one ability of one object, and the choices it asks.
data Activation = MkActivation
  { object :: Reference.Reference,
    -- | Which of the object's activated abilities, by its index among them;
    -- Nothing accepts any, and two offers are then ambiguous.
    ability :: Maybe Natural,
    choices :: Choices.Choices
  }
  deriving (Eq, Ord, Show)
