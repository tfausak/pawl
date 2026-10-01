module Pawl.Types.ViewIs where

import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Reference as Reference
import qualified Pawl.Types.Reply as Reply
import qualified Pawl.Types.View as View
import qualified Pawl.Types.Zone as Zone

-- | A view of the state, rendered as JSON, equal to the expected value.
data ViewIs = MkViewIs
  { view :: View.View,
    player :: Maybe Label.Label,
    object :: Maybe Reference.Reference,
    zone :: Maybe Zone.Zone,
    is :: Reply.Reply
  }
  deriving (Eq, Ord, Show)
