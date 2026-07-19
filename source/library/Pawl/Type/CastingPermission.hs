{-# LANGUAGE DeriveLift #-}

module Pawl.Type.CastingPermission where

import Language.Haskell.TH.Syntax (Lift)

-- CR 113.6 / 601.3: a static permission to cast a card from a zone or under a
-- condition it normally could not. Classified by the permission pattern (the M3f
-- TriggerCondition shape). CastFromLibraryWhileSearching = Panglacial Wurm: "while
-- you're searching your library, you may cast this from your library." A general
-- "cast from the top of your library" (Garruk's Horde) is a future permission.
-- Only Pawl.Cast reads it (a membership test, permitsCastWhileSearching).
data CastingPermission = CastFromLibraryWhileSearching
  deriving (Eq, Lift, Ord, Show)
