module Pawl.Types.ObjectRef where

import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.RandomCardInHand as RandomCardInHand
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil

-- | WHICH OBJECTS an object-affecting effect names -- the object-side counterpart
-- of Pawl.Types.PlayerRef.
--
-- InSlot stands apart from the other arms: its objects were named BEFORE the
-- effect runs -- a slot, filled at cast or as the ability was placed -- where
-- every other arm's are found AS it runs. That is the distinction CR 115.10a
-- draws: only InSlot can name a target; no other arm ever does.
data ObjectRef
  = -- | CR 601.2c / 608.2b: the objects bound in a slot -- the one Recipient a
    -- target or a reserved binding put there, or every member where the slot
    -- holds a group, which is a definition rather than a target (CR 115.10a).
    InSlot SlotName.SlotName
  | -- | CR 109.2 / Day of Judgment: every permanent on the battlefield matching
    -- the Filter, swept when the effect executes (CR 608.2c).
    EachMatching (Filter.Filter Keyword.Keyword)
  | -- | CR 109.2a / Rise of the Dark Realms: every card matching the Filter in the
    -- graveyards the payload's ZoneScope names.
    EachCardInGraveyard EachCardInGraveyard.EachCardInGraveyard
  | -- | CR 109.2a / Ignorant Bliss: every card in the resolving controller's hand,
    -- nullary because CR 400.2's hidden zone and CR 109.5's "you" leave neither a
    -- player nor a filter to state.
    EachCardInYourHand
  | -- | CR 109.2a / Amnesia: every card the optional Filter matches in the hands
    -- the payload's ZoneScope names.
    EachCardInHand EachCardInHand.EachCardInHand
  | -- | CR 400.12 / 109.2a / Leveler, Caldera Breaker: every card in the resolving
    -- controller's library that the optional Filter matches. Not CR 701.23a's
    -- search, so no search trigger fires and CR 701.24 shuffles nothing -- proved
    -- by Pawl.MassEffectSpec's "CR 701.23a a sweep is not a search, and CR 701.24
    -- shuffles nothing".
    EachCardInYourLibrary (Maybe (Filter.Filter Keyword.Keyword))
  | -- | CR 607.2a / Hoarding Dragon, Karn Liberated: every card in exile that an
    -- instruction in an ability of this effect's source put there, narrowed by the
    -- optional Filter.
    EachCardExiledWithSource (Maybe (Filter.Filter Keyword.Keyword))
  | -- | CR 109.2b / Swift Silence: every spell on the stack matching the Filter,
    -- abilities excluded (CR 113.9).
    --
    -- Not implemented: a set holding the abilities alone, which no card has needed
    -- (gap #2032).
    EachSpell (Filter.Filter Keyword.Keyword)
  | -- | CR 405.1 / Glen Elendra's Answer: every object on the stack matching the
    -- Filter, spells and abilities alike.
    EachOnStack (Filter.Filter Keyword.Keyword)
  | -- | CR 120.3a / Molten Disaster: every player in the game, an ObjectRef because
    -- a damage clause's ref is one. Pawl.ReplacementSpec's Molten Disaster case
    -- proves that a clause naming this arm beside an EachMatching is one CR 608.2f
    -- batch.
    EachPlayer
  | -- | CR 102.1 / 120.3a / Soul Immolation: every opponent of the resolving
    -- controller, the arm above narrowed by CR 109.5's perspective.
    EachOpponent
  | -- | CR 614.12a / 120.3a / Stuffy Doll: the player this effect's source chose as
    -- it entered the battlefield, read off Object.chosenPlayer there.
    ChosenPlayer
  | -- | CR 401.2 / 121.1 / Count on Luck, Act on Impulse: the cards on top of the
    -- libraries the payload's PlayerRef names, as deep as its Quantity, deepest
    -- named last.
    TopOfLibrary TopOfLibrary.TopOfLibrary
  | -- | CR 401.2 / Treasure Hunt, Open the Way: the cards on top of a library down
    -- to and INCLUDING the one whose match completes the payload's count.
    TopOfLibraryUntil TopOfLibraryUntil.TopOfLibraryUntil
  | -- | CR 404.1 / Soldevi Digger: the top card of each graveyard the PlayerRef
    -- names -- the NEWEST arrival, which is the LAST member of the pile and the
    -- opposite end from the library arms above (Pawl.Engine.Game.insertIntoZone
    -- appends a graveyard arrival). No depth, no Filter and no prompt: CR 404.2
    -- leaves the order out of the players' hands, so "the top card" names exactly
    -- one and there is nothing to ask -- and Scryfall
    -- @o:/top card of .*graveyard/@, 2026-09-06, returns no printing that reads
    -- this position more than one card deep. A card naming "the top two cards of
    -- your graveyard" would want the depth the library arm above carries.
    --
    -- Not implemented: a card that TESTS the top card rather than acting on it
    -- unconditionally -- Guiding Spirit's "if the top card of target player's
    -- graveyard is a creature card" -- which wants a Condition that can name this
    -- position, not another field here (gap #3318).
    TopOfGraveyard PlayerRef.PlayerRef
  | -- | CR 608.2d / Port of Karfell: a card in a graveyard matching the Filter,
    -- chosen as the effect runs rather than targeted (CR 115.1).
    --
    -- Not implemented: a count above one -- Fall of the Thran's "each player
    -- returns TWO land cards from their graveyard to the battlefield" -- and with
    -- it the exclusion "another" states (#1437).
    --
    -- A QUESTION rather than a read, so only an opcode whose gather reaches the
    -- Game monad carries it out; Pawl.CardSpec's inertChoosers rejects the other
    -- pairings, and it is the only thing that states that ragged matrix.
    ChosenCardInGraveyard ChosenCardInGraveyard.ChosenCardInGraveyard
  | -- | CR 608.2d / 402.3 / Karn Liberated: a card in a hand matching the Filter,
    -- chosen as the effect runs by the seat whose hand it is -- which is why one
    -- PlayerRef does the chooser's and the zone's duty at once.
    --
    -- Never a target, because pawl has no target pool over a hidden zone (#559).
    ChosenCardInHand ChosenCardInHand.ChosenCardInHand
  | -- | CR 608.2d / Commune with the Gods: a card chosen out of the group a slot
    -- holds -- the printed "from among them" -- read through
    -- Pawl.Engine.Resolve.Effect.fromAmongMembers.
    ChosenCardFromAmong ChosenCardFromAmong.ChosenCardFromAmong
  | -- | CR 608.2c / Mulch: every card in the group a slot holds that the Filter
    -- matches -- the printed "all land cards revealed this way", the arm above's
    -- plural and a read rather than a question.
    EachCardFromAmong EachCardFromAmong.EachCardFromAmong
  | -- | CR 701.20a / 701.9b / Merfolk Spy, Fall: the cards randomness names out of
    -- the hands the PlayerRef names, asked of the interpreter through
    -- Prompt.RandomObject and filtered back against the candidates.
    --
    -- Not implemented: the MoveToZone gather, which elides this arm so that only
    -- Reveal carries it out (#1733).
    RandomCardInHand RandomCardInHand.RandomCardInHand
  | -- | CR 608.2d / Tovolar, Dire Overlord: any number of the permanents on the
    -- battlefield matching the Filter, offered rather than swept, the empty answer
    -- legal.
    AnyNumberMatching (Filter.Filter Keyword.Keyword)
  | -- | CR 608.2d / 701.42a / Hanweir Battlements: exactly one of the permanents on
    -- the battlefield matching the Filter, chosen as the effect runs and not asked
    -- at a single candidate, where CR 608.2d leaves one legal announcement.
    ChosenPermanent (Filter.Filter Keyword.Keyword)
  | -- | CR 608.2f / 701.42a / Hanweir Battlements: the effect's source together
    -- with exactly one permanent the Filter admits, named as one instruction so
    -- that the pair moves in one event -- proved by Pawl.MeldSpec's "CR 608.2f the
    -- pair leaves the battlefield in one event".
    SourceAndChosenPermanent (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
