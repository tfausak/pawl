module Pawl.Types.AsCopy where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone

-- | CR 707.5 / 614.1c: the payload of Pawl.Types.EntryRewrite's @AsCopy@ arm --
-- "you may have this permanent enter as a copy of [any enchantment on the
-- battlefield]".
--
-- The Filter is the printed noun phrase after "a copy of", read as a quality of
-- the candidate: Clone's "any creature", Copy Enchantment's "any enchantment",
-- Clever Impersonator's negated "any nonland permanent".
-- It is NOT the rewrite's @matching@ field, which says which ENTERING permanent
-- the replacement modifies (CR 614.12's subject); these are two different
-- objects and Clone writes @IsSource@ for the one and @HasCardType Creature@ for
-- the other.
--
-- "On the battlefield" is not in the Filter and cannot be: the zone is the
-- offer's domain rather than a quality of a candidate. It is `zone` instead,
-- which Pawl.Engine.Replacement.legalCopyTargets walks.
--
-- The exceptions are CR 707.9's "except ..." clause, empty for a plain Clone.
-- They ride the rewrite rather than being a rewrite of their own, because CR
-- 707.9 makes them modifications OF the copying process: they happen only when a
-- copy is actually made, so declining the "may" leaves the object its printed
-- self and no exception applies.
data AsCopy ability effect = MkAsCopy
  { eligible :: Filter.Filter Keyword.Keyword,
    exceptions :: [CopyException.CopyException ability],
    -- | CR 614.1d inside CR 614.1c's sentence: Vesuva's "you may have this land
    -- enter TAPPED as a copy of any land on the battlefield". One replacement
    -- doing two things, so the status rides the rewrite rather than sitting in a
    -- second EntryRewrite.Tapped beside it: a second replacement would tap a
    -- Vesuva that DECLINED the copy, which the printed sentence does not.
    --
    -- Not a CopyException: CR 707.2 excludes status from the copiable values, so
    -- this may not be written into the snapshot. It goes onto the object through
    -- Pawl.Engine.Event.enterTapped, the same write EntryRewrite.Tapped makes.
    tapped :: Bool,
    -- | CR 707.9e: the exception that is an ADDITIONAL EFFECT rather than a
    -- modification of a characteristic -- Altered Ego's "except it enters with X
    -- additional +1\/+1 counters on it". Nothing for a clause stating none.
    --
    -- Not a CopyException, for `tapped`'s reason one rule over: every arm of that
    -- type writes a characteristic into the copiable snapshot (CR 707.9a, CR
    -- 707.9b), and CR 122.1 makes a counter a marker placed ON an object rather
    -- than one of its characteristics, so there is nothing to write. The same
    -- sentence over CR 707.1's token mint is Littjara Mirrorlake's, and it is
    -- CreateCopy's `riders` rather than one of its `exceptions` for exactly this
    -- reason.
    --
    -- The WithCounters payload EntryRewrite's own CR 614.1c row carries, rather
    -- than an EntryRiders record: the other riders are already said elsewhere or
    -- cannot be said here -- `tapped` above is CR 614.1d's own field, and no
    -- printed copy exception puts a permanent onto the battlefield attacking,
    -- blocking, transformed or face down. So the narrow payload is the whole
    -- sayable clause, and Pawl.Engine.Event's AsCopy arm places it through
    -- addEnteringCounters (by way of addCopyExceptionCounters), the same funnel
    -- the CR 614.1c row uses, so CR 614.16 sees it (Doubling Season).
    --
    -- Placed only where the copy is actually MADE, `exceptions`' reason: CR 707.9
    -- makes the clause a modification of the copying process, so declining
    -- Clone's "may" leaves the permanent its printed self and no counters.
    --
    -- CR 707.9e's second sentence takes them back when a later copy effect is
    -- applied to the same entry: the scenario "CR 707.9e a copy effect applied
    -- after Altered Ego's takes back its additional counters" proves it.
    counters :: Maybe WithCounters.WithCounters,
    -- | CR 707.9g / 603.11: the triggered ability linked to this replacement,
    -- "When you do, ..." in the same paragraph (Wall of Stolen Identity). Empty
    -- for none. Its effects read the copied object under
    -- Pawl.Engine.Binding.copiedObject.
    --
    -- Effects rather than a printed triggered ability, because the copy
    -- overwrites the permanent's abilities (CR 707.2) and the ability is not
    -- one it has afterwards: it rides the replacement, and Pawl.Engine.Event's
    -- AsCopy arm arms it as a reflexive entry only where a copy was made.
    whenYouDo :: Seq.Seq effect,
    -- | CR 707.5's domain: Battlefield for "any creature on the battlefield"
    -- (Clone), Graveyard for "any creature card in a graveyard" (Superior
    -- Spider-Man), every player's graveyard alike.
    zone :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
