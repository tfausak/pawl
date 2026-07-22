module Pawl.Type.DestructionRewrite where

-- CR 614.8 / 701.19a: how a replacement rewrites a would-be-destroyed event.
-- Regenerate is "instead, tap it, remove all damage from it, and remove it from
-- combat" -- the destruction itself does not happen, so nothing downstream of it
-- (a put-into-graveyard, and therefore Rest in Peace) ever runs.
data DestructionRewrite = Regenerate
  deriving (Eq, Ord, Show)
