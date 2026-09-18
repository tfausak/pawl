-- | CR 701.58a, cloak: "turn it face down. It becomes a 2\/2 face-down creature
-- card with ward {2}, no name, no subtypes, and no mana cost. Put that card onto
-- the battlefield face down", and the whole of the keyword action.
--
-- Pawl.Engine.Populate's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so the procedure
-- lives in the engine rather than in card data. The closed\/open invariant
-- forbids the rules core casing on an EFFECT's identity, and nothing here does
-- -- Pawl.Engine.Resolve.Effect's Effect.Cloak arm calls in without saying which
-- effect it is.
--
-- The LISTING is the rule's and never a card's, which is the whole reason this
-- is an opcode rather than an Effect.MoveToZone carrying the rider a manifest
-- card writes: CR 701.58a fixes the 2\/2 and the ward {2}, so a printing has
-- nothing to say about them and no card should be able to say it wrong.
--
-- Rule 701.58 has no "whenever a player cloaks", so there is no GameEvent here
-- and no trigger condition to hang one on. Scryfall @o:"cloaks"@, 2026-09-18,
-- returns no card; a printing worded that way is what would need one.
module Pawl.Engine.Cloak where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import Pawl.Types.Game (Game)
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- | CR 701.58a's second and third sentences as one rider: the 2\/2 with ward {2}
-- CR 702.168b lists in the same words (FaceDownCharacteristics.disguisedValue),
-- under a reason of this rule's own so that CR 701.58b's procedure -- and not
-- disguise's -- is the one the permanent is turnable by.
--
-- A settled count rather than a Quantity, Pawl.Engine.Foretell.riders' reason:
-- the rule states no counters, so there is no CR 608.2h moment to evaluate one
-- at.
riders :: EntryRiders.EntryRiders Natural
riders =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = TapState.Untapped,
      EntryRiders.attacking = Nothing,
      EntryRiders.blocking = Nothing,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False,
      EntryRiders.exiledFaceDown = False,
      EntryRiders.attachedTo = Nothing,
      EntryRiders.faceDown =
        Just
          FaceDownState.MkFaceDownState
            { FaceDownState.reason = FaceDownReason.Cloaked,
              FaceDownState.listed = FaceDownCharacteristics.disguisedValue
            }
    }

-- | CR 701.58a performed on the top card of this player's library, which is
-- what the printings that reach it say ("cloak the top card of your library",
-- "its controller cloaks the top card of their library").
--
-- NO PROMPT, because the rule leaves nothing to ask: which card is cloaked is
-- fixed by the library's order, and the listing is the rule's.
--
-- Through Event.changeZoneEntering, the door the faceDown rider is read from, so
-- CR 708.3 turns the card over BEFORE the move and no reader ever sees it face
-- up on the battlefield. That door is also CR 701.58f: a rule or effect that
-- prohibits the face-down object from entering leaves the move cancelled, and
-- the card is then still the library card it was.
--
-- The permanent enters under the player who cloaked, which is CR 110.2a: that
-- player is the one the effect instructed, and here it is their own library the
-- card came off. Not implemented: a printing that has one player cloak from
-- ANOTHER's library, whose permanent rule 110.2a puts under the instructed
-- player instead (#3855).
--
-- An EMPTY LIBRARY cloaks nothing. CR 701.58a names a card, and CR 609.3 leaves
-- the rest of the effect to do as much as it can.
cloak :: PlayerId -> Game ()
cloak pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> pure ()
    top : _ -> Monad.void (Event.changeZoneEntering top Zone.Battlefield LibraryPosition.defaultValue riders (Just pid))
