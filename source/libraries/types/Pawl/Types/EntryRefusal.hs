module Pawl.Types.EntryRefusal where

-- | Why a permanent that was materialized for its entry loop did not enter
-- after all; each caller of Pawl.Engine.Event.runEntry unmakes the entry and
-- then puts the object where the refusing rule says.
data EntryRefusal
  = -- | CR 614.1a: an EntryRewrite.SacrificeToEnter went unpaid, so "put it into its owner's graveyard instead".
    Unpaid
  | -- | CR 303.4g: it became an Aura as it entered and has nothing legal to enchant.
    Unhosted
  deriving (Eq, Ord, Show)
