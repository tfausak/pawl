module Pawl.Types.Player where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId
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
    -- Read by Pawl.Engine.Ring, which is where CR 701.54c's "as long as the Ring
    -- has tempted that player N or more times" turns this number into the
    -- emblem's ability set: the base tier and the four-temptation one are built.
    --
    -- Not implemented: the two- and three-temptation tiers (#706).
    ringTemptations :: Natural.Natural,
    -- | CR 702.179b: this player's speed, or Nothing for a player who has none.
    -- "Players do not have speed until a rule or effect sets their speed to a
    -- specific value", so the absence is a THIRD state and not a zero: CR 704.5aa
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
    -- stack to battlefield to command zone again. An object id would name a dead
    -- object after the first cast. A printing survives, and CR 903.5b's singleton
    -- deck construction is what makes it unambiguous: a Commander deck holds at
    -- most one card with a given English name, so "the object whose printing is
    -- this one" picks out exactly the commander. Pawl does not ENFORCE rule
    -- 903.5b (#940), so a malformed deck with two copies would make both answer
    -- to it.
    --
    -- The printing is named by its id (#1592), which is stable for the whole
    -- game: Pawl.Engine.Setup.internDeck interns each of a deck's distinct
    -- printings exactly once, so this id is the same one the commander's own
    -- objects carry and Pawl.Engine.Commander.isCommander can compare the two.
    commander :: Maybe PrintingId.PrintingId,
    -- | CR 903.8: how many times this player has cast their commander from the
    -- command zone this game. The commander tax is {2} for each of them.
    --
    -- Counted here rather than on the object for `commander`'s reason -- no object
    -- survives the round trip. Counts CASTS and not returns, which is the detail
    -- rule 903.8 turns on: a commander that dies and goes back to the command zone
    -- without being recast has not made its own next cast any dearer.
    commanderCasts :: Natural.Natural,
    -- | CR 903.10a: how much COMBAT damage this player has been dealt by each
    -- commander, over the course of the game. Absent key means zero, the
    -- convention `counters` above uses. Only ever climbs -- rule 903.10a counts
    -- the whole game, so nothing takes damage back out.
    --
    -- Keyed by the commander's OWNER, not by an object and not by a printing.
    -- CR 400.7 mints a fresh id on every zone change, so an id could not
    -- survive the commander's first cast; and `commander` above is at most one
    -- printing per player (CR 903.3 designates one card), so the owner names
    -- exactly one commander today. Partner and background decks, which give a
    -- player two, would need a finer key (#939).
    commanderDamage :: Map.Map PlayerId.PlayerId Natural.Natural,
    -- | CR 309.2 \/ 309.2a: the dungeon cards this player owns from outside the
    -- game, empty for a player who brought none. Deck.dungeons is where they come
    -- from and Pawl.Engine.Setup.createDeck copies them here.
    --
    -- PRINTINGS and not ObjectIds, for `commander`'s reason turned inside out:
    -- there is no object at all until CR 701.49a brings a card into the command
    -- zone, and CR 309.5b removes it from the game and lets the SAME card be
    -- brought back in as a new object. What persists across both is the printing,
    -- named by its id (#1592) -- interned by Pawl.Engine.Setup.internDeck along
    -- with the rest of the deck, so it is minted before any of this is read.
    --
    -- SEVERAL, which is what makes CR 309.2a's "a dungeon card they own" a choice:
    -- Pawl.Engine.Dungeon.enter raises Prompt.ChooseDungeon over this set. CR
    -- 309.3's one-at-a-time limit is about the COMMAND ZONE, which
    -- Pawl.Engine.Dungeon.inDungeon holds, and says nothing about how many a player
    -- may own outside the game.
    --
    -- Read only by Pawl.Engine.Dungeon. Nothing is taken out of it when a dungeon
    -- enters the game: CR 309.5b has the player put a dungeon they own back into the
    -- command zone after finishing one, so this is a supply rather than a stock.
    dungeons :: Set.Set PrintingId.PrintingId,
    -- | CR 309.7: how many dungeons this player has completed. "A player
    -- completes a dungeon as that dungeon card is removed from the game", so
    -- Pawl.Engine.Dungeon.remove is the sole writer -- the one function both CR
    -- 701.49c's venture out of the bottommost room and CR 704.5t's state-based
    -- action go through. Zero until one is, and only ever climbs: nothing in the
    -- rules uncompletes a dungeon.
    --
    -- Keyed on the dungeon card's OWNER, which is CR 309.6's own word ("the
    -- dungeon card's owner removes it from the game"). In the venture case the
    -- owner and the resolving controller coincide; in the state-based action the
    -- owner is the only player available.
    --
    -- A COUNT and not the set of dungeons completed, though the printing is in
    -- hand at the removal site. CR 309.7 states no more than the fact of
    -- completion, and no card in data/cards/ asks which dungeon: Gloom Stalker's
    -- "as long as you've completed a dungeon" is this compared to 1. Acererak the
    -- Archlich's "if you haven't completed Tomb of Annihilation" is the card that
    -- would need the names. Not implemented: the named read (#2259).
    --
    -- NOT a PlayerCounterKind, for Player.ringTemptations' reason: CR 122.1 makes
    -- a counter a MARKER an effect can add or remove, and rule 309 never calls
    -- this one -- proliferate (CR 701.34a) would find it if it were.
    --
    -- Read by Pawl.Engine.Quantity's Quantity.DungeonsCompleted arm.
    completedDungeons :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
