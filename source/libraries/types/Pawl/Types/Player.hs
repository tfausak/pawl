module Pawl.Types.Player where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Status as Status

data Player = MkPlayer
  { life :: Integer,
    status :: Status.Status,
    -- | CR 122.1: player counters, counted per kind. Unlike object counters (CR
    -- 122.2, which cease to exist on a zone change), these persist for the whole
    -- game -- a player never changes zones. Absent kind means zero
    -- (Map.findWithDefault 0), the convention Object.counters uses.
    counters :: Map.Map PlayerCounterKind.PlayerCounterKind Natural.Natural,
    -- | CR 701.54c: how many times the Ring has tempted this player. Zero until
    -- it does, and only ever climbs -- nothing in rule 701.54 takes a temptation
    -- back.
    --
    -- Counted rather than derived, because CR 701.54d makes the count larger than
    -- anything the board remembers: "the Ring tempts a player whenever they
    -- complete the actions in 701.54a, EVEN IF SOME OR ALL OF THOSE ACTIONS WERE
    -- IMPOSSIBLE". A player with no creatures is tempted and designates nothing,
    -- so neither the Ring-bearer nor the emblem's existence can stand in for this.
    --
    -- NOT a PlayerCounterKind, though the field beside it would hold a Natural
    -- per player for free: CR 122.1 makes a counter a MARKER an effect can add or
    -- remove and a card can count, and rule 701.54 never calls this one. Proliferate
    -- (CR 701.34a) would find it if it were.
    --
    -- Read by nothing yet. CR 701.54c makes the emblem's ability set a function of
    -- this number, and none of its four tiers is built: the base one (#707), and
    -- the two-, three- and four-temptation ones, which are triggered abilities an
    -- emblem cannot fire from the command zone (#709).
    ringTemptations :: Natural.Natural,
    -- | CR 702.179b: this player's speed, or Nothing for a player who has none.
    -- "Players do not have speed until a rule or effect sets their speed to a
    -- specific value", so the absence is a THIRD state and not a zero: CR 704.5z
    -- starts a player's engines only when they have NO speed, and would fire
    -- forever against a player whose speed had been driven to 0 if the two were
    -- spelled alike.
    --
    -- Nothing still READS as zero wherever a card asks (CR 702.179f, "if that
    -- player has no speed, their speed is 0 for the purpose of an effect that
    -- refers to speed"), which Pawl.Engine.Quantity's Speed arm applies. The
    -- distinction the Maybe carries is therefore invisible to card text and
    -- visible only to the two rules that create speed, CR 702.179a and CR
    -- 702.179c.
    --
    -- NOT a PlayerCounterKind, for Player.ringTemptations' reason: CR 122.1
    -- makes a counter a MARKER an effect can add or remove, and rule 702.179
    -- never calls speed one -- proliferate (CR 701.34a) would find it if it were.
    --
    -- Bounded above at 4 by the only rule that raises it: CR 702.179d increases
    -- speed only "if your speed is less than 4", and CR 702.179e reads 4 as max
    -- speed. Nothing in rule 702.179 lowers it.
    speed :: Maybe Natural.Natural,
    -- | CR 903.3: the card this player designated as their commander, or Nothing
    -- outside a Commander game. The designation is made before the game begins
    -- and never changes.
    --
    -- A PRINTING and not an ObjectId, because CR 400.7 mints a fresh id on every
    -- zone change and a commander crosses zones constantly -- command zone to
    -- stack to battlefield to command zone again. An id would name a dead object
    -- after the first cast. A printing survives, and CR 903.5's singleton deck
    -- construction is what makes it unambiguous: a Commander deck holds at most
    -- one card with a given name, so "the object whose printing is this one" picks
    -- out exactly the commander. Pawl does not ENFORCE rule 903.5 (#940), so a
    -- malformed deck with two copies would make both answer to it.
    commander :: Maybe Printing.Printing,
    -- | CR 903.8: how many times this player has cast their commander from the
    -- command zone this game. The commander tax is {2} for each of them.
    --
    -- Counted here rather than on the object for `commander`'s reason -- no object
    -- survives the round trip. Counts CASTS and not returns, which is the detail
    -- rule 903.8 turns on: a commander that dies and goes back to the command zone
    -- without being recast has not made its own next cast any dearer.
    commanderCasts :: Natural.Natural
  }
  deriving (Eq, Show)
