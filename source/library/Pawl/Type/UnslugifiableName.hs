module Pawl.Type.UnslugifiableName where

import qualified Control.Exception as Exception
import Data.Text (Text)

-- A registry was asked for a card by a name with no slug ("!!!"), so there is no
-- file name to look for. Distinct from UnknownCard: nothing was searched for and
-- no path was built, because the question itself has no answer.
newtype UnslugifiableName = MkUnslugifiableName Text
  deriving (Eq, Show)

instance Exception.Exception UnslugifiableName
