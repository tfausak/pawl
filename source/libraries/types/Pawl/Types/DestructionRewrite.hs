module Pawl.Types.DestructionRewrite where

-- | CR 614.8 / 701.19a: how a replacement rewrites a would-be-destroyed event.
-- Under Regenerate the destruction itself does not happen, so nothing downstream
-- of it (a put-into-graveyard, and therefore Rest in Peace) ever runs.
data DestructionRewrite
  = Regenerate
  deriving (Eq, Ord, Show)
