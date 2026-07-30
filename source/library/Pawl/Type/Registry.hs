-- The card pool: a root directory of one-card-per-file JSON, plus the cards
-- read from it so far. Not a record of every card -- nothing is read until it is
-- asked for, so a root holding the whole ~34k-card pool costs one MVar.
module Pawl.Type.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map
import qualified Pawl.Slug as Slug
import qualified Pawl.Type.Card as Card

data Registry = MkRegistry
  { root :: FilePath,
    -- Keyed by slug (Pawl.Slug.slugify of the card's name). An MVar rather
    -- than an IORef because the test suite is built -threaded and tasty runs
    -- cases concurrently: holding it across the read-and-parse is what makes
    -- "each file is parsed at most once" exact rather than merely likely.
    -- Not a TVar either, for that same reason: the lock has to span a
    -- readFile, which no transaction can, so the STM equivalent is a TMVar --
    -- this, plus a hand-written copy of base's modifyMVar (#265).
    cache :: MVar.MVar (Map.Map Slug.Slug Card.Card)
  }
  -- No Show: MVar has no Show instance. Eq is MVar identity, so two registries
  -- over one root are equal only if they share a cache.
  deriving (Eq)
