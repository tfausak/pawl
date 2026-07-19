{-# LANGUAGE DeriveLift #-}

module Pawl.Type.ReplacementEffect where

import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.Zone (Zone)

-- CR 614.1a: a replacement effect. Classified by the event pattern it intercepts:
-- a zone change whose destination is `whenDestination` heads for `toDestination`
-- instead. Rest in Peace = RedirectZoneChange Graveyard Exile (any object, from
-- any source zone). Its own leaf family, distinct from Effect (one-shot) and
-- Modification (continuous). Only Pawl.Event may case on it.
data ReplacementEffect = RedirectZoneChange
  { whenDestination :: Zone,
    toDestination :: Zone
  }
  deriving (Eq, Lift, Ord, Show)
