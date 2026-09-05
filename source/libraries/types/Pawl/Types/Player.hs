module Pawl.Types.Player where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
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
    -- emblem's ability set -- every tier of it.
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
    -- game: Pawl.Engine.Setup.createDeck interns each of a deck's distinct
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
    -- named by its id (#1592) -- interned by Pawl.Engine.Setup.createDeck along
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
    -- | CR 400.11 \/ 400.11a: the cards this player owns that are outside the game,
    -- counted per printing. Empty for a player who brought no sideboard.
    --
    -- PER PLAYER and not on the GameState, because CR 108.3b leaves no card
    -- outside the game ownerless -- a sideboard card is owned by "the player who
    -- started the game with it in their sideboard", and every other one by its
    -- legal owner. Every rule that reaches in asks for the acting player's OWN
    -- cards (CR 309.2a, CR 701.23j, CR 701.48a, CR 702.139a), so ownership is the
    -- key rather than a predicate a reader could forget.
    --
    -- PrintingIds and not Printings, and no object minted: Player.dungeons' reason
    -- exactly. Outside the game is not a zone (CR 400.11), so there is nowhere for
    -- an object to sit until something brings the card in; CR 400.11c is why
    -- nothing else can reach these in the meantime.
    --
    -- COUNTED and not a set, which is where this parts from `dungeons` above: this
    -- is a STOCK. Pawl.Engine.Event.bringIn spends an entry, because a card
    -- brought into the game is in the game (CR 400.11b) and the next Burning Wish
    -- cannot find that same copy again. A dungeon card is never spent (CR 309.5b),
    -- so its field forgets its counts.
    --
    -- NOT merged with `dungeons`, though CR 309.2 puts dungeon cards outside the
    -- game too: CR 309.2 keeps them out of deck and sideboard both, CR 701.49a
    -- chooses among them by a rule of its own rather than by a card's filter, and
    -- CR 309.2d forbids anything else from bringing one in. One map would have
    -- Pawl.Engine.Event.eligible offering dungeon cards to Burning Wish.
    --
    -- Where the rest of what is outside the game will land: CR 727.2's restart
    -- cards (#135) and CR 707.13's copy created outside the game (#888). CR
    -- 729.4's other half -- the main game's cards, which a subgame sees as
    -- outside it -- deliberately did NOT land here: those are objects in a game
    -- that is on hold rather than a count of printings a player set aside, so
    -- they ride Pawl.Types.GameState's outsideObjects and only for as long as
    -- the subgame runs. Sticker sheets (#872) are
    -- outside the game too but are not cards and have no characteristics (CR
    -- 123.2), so they need a field of their own rather than this one.
    outsideTheGame :: Map.Map PrintingId.PrintingId Natural.Natural,
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
    -- A COUNT, beside the names below rather than folded from them: CR 309.5b
    -- lets the same dungeon be brought back in and completed again, so a player
    -- who completed Undercity twice has completed two dungeons and one name.
    -- Gloom Stalker's "as long as you've completed a dungeon" is this compared
    -- to 1.
    --
    -- NOT a PlayerCounterKind, for Player.ringTemptations' reason: CR 122.1 makes
    -- a counter a MARKER an effect can add or remove, and rule 309 never calls
    -- this one -- proliferate (CR 701.34a) would find it if it were.
    --
    -- Read by Pawl.Engine.Quantity's Quantity.DungeonsCompleted arm.
    completedDungeons :: Natural.Natural,
    -- | CR 309.7: WHICH dungeons this player has completed, by name -- Acererak
    -- the Archlich's "if you haven't completed Tomb of Annihilation". Written by
    -- Pawl.Engine.Dungeon.remove beside the tally above, and read by
    -- Pawl.Engine.Quantity's Quantity.CompletedDungeon arm.
    --
    -- NAMES and not PrintingIds, because a name is what Acererak's text names and
    -- CR 201.2a makes sameness of name the question: two printings of one dungeon
    -- are the same dungeon to it, and a printing id would tell them apart.
    completedDungeonNames :: Set.Set CardName.CardName,
    -- | CR 103.2a \/ 702.139b: this player's STARTING DECK, counted per printing --
    -- their deck once the sideboard has been set aside, and, in a Commander game,
    -- before the commander is set aside. Empty for a game built without decks.
    --
    -- A SNAPSHOT and not a live read of the library, which is the whole reason the
    -- field exists: CR 702.139b fixes the set at one moment before the game begins,
    -- and by the time a companion's condition could be asked again the opening hands
    -- have been drawn and the library no longer holds the deck. Pawl.Engine.Setup.createDeck
    -- is the sole writer, and nothing rewrites it -- CR 103.2a names a moment, not a
    -- state that tracks.
    --
    -- The COMMANDER is counted in, which is CR 702.139b's second sentence: "in a
    -- Commander game, this is also before you've set aside your commander". So this
    -- is Deck.cards plus Deck.commander, and not what Pawl.Engine.Setup.createDeck
    -- dealt into the library.
    --
    -- The SIDEBOARD is not, which is CR 103.2a's first sentence and the reason
    -- Player.outsideTheGame is a different field: what is set aside is outside the
    -- game, and what is left is this.
    --
    -- The VANGUARD and the DUNGEONS are not either, and for a sharper reason than
    -- the sideboard's: CR 902.3 and CR 309.2 keep both out of the deck to begin
    -- with, so neither was ever among the cards a sideboard could be set aside from.
    --
    -- PrintingIds and not Printings, and COUNTED, for Player.outsideTheGame's two
    -- reasons: the ids are interned by Pawl.Engine.Setup.createDeck before any of
    -- this is read, and CR 100.2a lets a deck hold four copies of a card, which a
    -- set would collapse.
    --
    -- Read by Pawl.Engine.Companion, which is where CR 702.139a's condition is
    -- evaluated against it.
    startingDeck :: Map.Map PrintingId.PrintingId Natural.Natural,
    -- | CR 103.2b \/ 702.139a: the card this player revealed from outside the game
    -- as their companion, or Nothing for the players who revealed none -- which is
    -- every player in a game whose decks brought no companion.
    --
    -- A PRINTING and not an ObjectId, for Player.commander's reason turned inside
    -- out: CR 103.2b leaves the revealed card OUTSIDE the game, where CR 400.11
    -- gives it no object at all until CR 116.2g brings it in.
    --
    -- ONE, which is CR 103.2b's own cap: "a player may reveal no more than one card
    -- this way".
    --
    -- NOT taken out of Player.outsideTheGame when it is chosen: CR 103.2b's last
    -- sentence keeps the revealed card out there, and CR 116.2g is what spends it.
    companion :: Maybe PrintingId.PrintingId,
    -- | CR 116.2g: has this player already taken the companion special action this
    -- game? "Only if they haven't done so yet this game" is this flag, and
    -- Pawl.Engine.Companion.take is its sole writer.
    --
    -- A FLAG and not a count, because rule 116.2g permits the action exactly once
    -- and nothing in rule 702.139 gives it back; and separate from `companion`
    -- above rather than clearing it, because CR 702.139c keeps the card in the game
    -- afterwards and a card that reads "your chosen companion" would still find it.
    companionTaken :: Bool
  }
  deriving (Eq, Ord, Show)
