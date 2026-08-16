module Pawl.Types.SpellCast where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

-- | CR 603.2 over CR 601's cast: which spells fire the ability, whose turns
-- count, which zone the cast came out of, and which occurrence within the turn
-- -- Monastery Swiftspear's prowess against Kess's once-per-turn window against
-- Harness the Storm's "from your hand" against Clarion Spirit's "your second
-- spell each turn".
data SpellCast = MkSpellCast
  { filter :: Filter.Filter Keyword.Keyword,
    scope :: TurnScope.TurnScope,
    -- | CR 601.2a's "moves it from where it is to the stack": which zone that
    -- was. Nothing for the overwhelming majority of cast triggers, which watch
    -- every cast whatever zone it came from.
    --
    -- NOT a conjunct of the Filter above, and for the reason the TurnScope beside
    -- it is not either: CR 400.7 mints the spell as a new object with no memory
    -- of the zone it left, so no characteristic of the candidate can answer the
    -- question. The event carries it instead (Pawl.Types.SpellWasCast.zone).
    --
    -- ONE zone, not a set and not a negation: every printing in the pool names a
    -- single zone. Vega, the Watcher's "from anywhere other than your hand" is
    -- the shape that would want more, and no card here prints it.
    zone :: Maybe Zone.Zone,
    -- | Clarion Spirit's "your SECOND spell each turn": the cast fires the
    -- ability only when it is the nth one of the turn matching everything
    -- above. Nothing for every trigger that watches each matching cast alike,
    -- which is almost all of them.
    --
    -- There is no comprehensive rule for the phrase, exactly as
    -- Pawl.Types.TriggerFrequency's haddock says of "for the first time each
    -- turn": it is plain card text narrowing a CR 601.2i trigger event, and
    -- this field says so rather than manufacturing a citation.
    --
    -- Counted over the casts the rest of this record ALREADY admits rather than
    -- over every cast in the log -- the printed sentence puts the ordinal
    -- inside the description ("your second spell", "your first spell during
    -- each opponent's turn"), so the set counted is the set matched.
    --
    -- Which is why the number is not stamped on GameEvent.SpellCast the way CR
    -- 121.1's is stamped on GameEvent.Drew: a draw's ordinal is per player and
    -- can be counted as the event is filed, and a cast's is per CONDITION, since
    -- two abilities watching the same cast can be counting different sets. The
    -- count is taken where the Filter is, at Pawl.Engine.Event.castOrdinal.
    --
    -- Pawl.Types.PlayerDrawsNthCard is the same question over CR 121's draw, and
    -- answers it the same way: EQUALITY, not "at least", so a turn with four
    -- casts fires a second-spell trigger once.
    --
    -- INCLUSIVE of the cast being matched, which is what makes the second cast
    -- the one that answers 2: CR 601.2i has already filed the event by the time
    -- CR 603.2 checks the condition against it. The opposite reading of the same
    -- ordering is Pawl.Types.CostReduction.perEach's, and it comes out the other
    -- way for the honest reason -- CR 601.2f runs BEFORE rule 601.2i files
    -- anything.
    --
    -- Zero has no printing and needs no rejection: an inclusive count starts at
    -- one, so an ordinal of zero matches no cast.
    ordinal :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)
