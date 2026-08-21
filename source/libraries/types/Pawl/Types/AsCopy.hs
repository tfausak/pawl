module Pawl.Types.AsCopy where

import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 707.5 / 614.1c: the payload of Pawl.Types.EntryRewrite's @AsCopy@ arm --
-- "you may have this permanent enter as a copy of [any enchantment on the
-- battlefield]".
--
-- The Filter is the printed noun phrase after "a copy of", read as a quality of
-- the candidate: Clone's "any creature", Copy Enchantment's "any enchantment".
-- It is NOT the rewrite's @matching@ field, which says which ENTERING permanent
-- the replacement modifies (CR 614.12's subject); these are two different
-- objects and Clone writes @IsSource@ for the one and @HasCardType Creature@ for
-- the other.
--
-- "On the battlefield" is not in the Filter and cannot be: the zone is the
-- offer's domain rather than a quality of a candidate, and
-- Pawl.Engine.Replacement.legalCopyTargets walks the battlefield to supply it.
--
-- The exceptions are CR 707.9's "except ..." clause, empty for a plain Clone.
-- They ride the rewrite rather than being a rewrite of their own, because CR
-- 707.9 makes them modifications OF the copying process: they happen only when a
-- copy is actually made, so declining the "may" leaves the object its printed
-- self and no exception applies.
data AsCopy = MkAsCopy
  { eligible :: Filter.Filter Keyword.Keyword,
    exceptions :: [CopyException.CopyException],
    -- | CR 614.1d inside CR 614.1c's sentence: Vesuva's "you may have this land
    -- enter TAPPED as a copy of any land on the battlefield". One replacement
    -- doing two things, so the status rides the rewrite rather than sitting in a
    -- second EntryRewrite.Tapped beside it: a second replacement would tap a
    -- Vesuva that DECLINED the copy, which the printed sentence does not.
    --
    -- Not a CopyException: CR 707.2 excludes status from the copiable values, so
    -- this may not be written into the snapshot. It goes onto the object through
    -- Pawl.Engine.Event.enterTapped, the same write EntryRewrite.Tapped makes.
    tapped :: Bool
  }
  deriving (Eq, Ord, Show)
