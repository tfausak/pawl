-- | Who may LOOK at a card in exile, and which exiled cards a player may
-- therefore be offered.
--
-- CR 406.3 makes exile a public zone with one exception -- a card "exiled face
-- down" -- and CR 406.4 turns that exception into a rule about CHOOSING: "the
-- player may choose a specific face-down card only if the player is allowed to
-- look at that card". Both questions are per-player, so neither can be answered
-- by a flag on the object alone; this module is where the two are asked.
--
-- THE INVARIANT: this is the closed half. CR 406.3's default and CR 702.143a's
-- grant are both rulebook, so reading Object.foretold here is the same act as
-- reading a Phase. Object.exileLookers is a set of grants an effect wrote, read
-- without asking which effect. Nothing here asks which CARD is in exile.
module Pawl.Engine.Exile where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as View
import qualified Pawl.Types.ExileLooker as ExileLooker
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Pile as Pile
import Pawl.Types.PlayerId (PlayerId)

-- | CR 406.3: may this player look at this exiled card?
--
-- The rule's default is YES for every player -- "exiled cards are, by default,
-- kept face up and may be examined by any player at any time" -- so the whole
-- question is Object.exiledFaceDown, which is the rider that overrides it.
--
-- CR 702.143a is one such grant: "that player may look at that card
-- as long as it remains in exile", said of the player who took the foretell
-- special action, who is the card's OWNER (CR 400.1 makes a hand a per-player
-- zone and the action exiles from the actor's own hand). CR 702.143d says the
-- same of a card an effect makes foretold, and names the owner outright. So the
-- Object.foretold stamp Pawl.Engine.Foretell writes IS the permission, and the
-- owner is the one player it names.
--
-- The foretold arm stays DERIVED rather than stored, and CR 406.3's tail is what
-- makes that exact: the permission runs until the card leaves the exile zone,
-- and the stamp has the same lifetime -- it is per-incarnation state, so CR
-- 400.7's fresh incarnation on the way out clears it at the same moment the rule
-- ends the permission. That rule's OTHER ending -- the card becoming part of a
-- pile of cards that are shuffled -- has nothing to end: pileOf below builds the
-- piles, and no card in `data/cards/` shuffles one.
--
-- Object.exileLookers is rule 406.3's OTHER grant, the one an instruction makes
-- -- "once a player is allowed to look at a card exiled face down, that player
-- may continue to look at that card" -- and ExileLooker.ThePlayer is stored
-- because the rule keeps it alive past the instruction that gave it, which
-- leaves nothing to derive it from. It names a player who need not be the OWNER:
-- Extract Power looks at each player's top card and exiles them, so the caster
-- may look at a card somebody else owns. Pawl.ExileSpec's "CR 406.3 the player
-- the exiling instruction let look names both cards, and the owner who was shown
-- nothing gets their pile" is what proves the two seats come apart.
--
-- ExileLooker.TheExiler is CR 702.75a's, and it is the one grant that is NOT
-- stored as a seat: hideaway's granted ability names "the player who controls
-- the permanent that exiled this card", so control is read afresh here. Which
-- permanent that is comes off CR 607.2's link (GameState.exiledWith), written by
-- the same instruction's exile. Pawl.ExileSpec's "CR 702.75a the look follows
-- control of the land that exiled the card, and CR 406.3's does not" is what
-- proves the read is live. Live and STICKY are not in tension: `accrueLookers`
-- below turns each seat that read into an ExileLooker.ThePlayer of its own, which
-- is CR 406.3's continuing permission rather than a second reading of rule
-- 702.75a.
mayLookAt :: PlayerId -> ObjectId -> GameState.GameState -> Bool
mayLookAt pid oid gs = Maybe.fromMaybe False $ do
  obj <- Game.lookupObject oid gs
  pure
    ( not (Object.exiledFaceDown obj)
        || (Maybe.isJust (Object.foretold obj) && Object.owner obj == pid)
        || any (looksAt pid oid gs) (Object.exileLookers obj)
    )

-- One grant of CR 406.3's look permission, asked about one player.
looksAt :: PlayerId -> ObjectId -> GameState.GameState -> ExileLooker.ExileLooker -> Bool
looksAt pid oid gs looker = case looker of
  ExileLooker.ThePlayer p -> p == pid
  ExileLooker.TheExiler -> exilerController oid gs == Just pid

-- CR 702.75a's "the player who controls the permanent that exiled this card",
-- read off CR 607.2's link. Both `looksAt` above and `accrueLookers` below ask
-- through this, so the live read and the sample it feeds cannot disagree about
-- who that is.
--
-- The battlefield test is a REGRESSION FENCE rather than proven behaviour: the
-- rule names a PERMANENT (CR 110.1) and CR 108.4 gives a controller to nothing
-- else, but dropping the test leaves the suite green. Nothing observes it, since
-- a hideaway permanent that leaves the battlefield lands in a zone where CR
-- 108.4 answers its OWNER -- who, hideaway having instructed that same player to
-- look at the card before exiling it face down, is already an
-- ExileLooker.ThePlayer of it.
exilerController :: ObjectId -> GameState.GameState -> Maybe PlayerId
exilerController oid gs = do
  exiler <- Map.lookup oid (GameState.exiledWith gs)
  Monad.guard (Set.member exiler (GameState.battlefield gs))
  View.controllerOf exiler gs

-- | CR 406.3's second sentence, as a SAMPLE: "once a player is allowed to look at
-- a card exiled face down, that player may continue to look at that card until
-- it leaves the exile zone ... even if the instruction allowing the player to do
-- so no longer applies". ExileLooker.TheExiler is the one grant here that can
-- stop applying, control of the exiling permanent being read live, so this
-- stamps whoever holds that control as an ExileLooker.ThePlayer of their own and
-- the permission then outlives the control.
--
-- STAMP-ONLY, which is the rule: nothing here ever removes a looker, and the
-- grant it writes is the same one Pawl.Engine.Resolve.Effect's
-- Effect.GrantLookAtExiled writes for the instruction's own controller.
--
-- SAMPLED rather than hooked for Pawl.Engine.Engine.checkControlContinuity's
-- reason -- control is DERIVED (CR 613.1b), so no resolution announces the
-- change a grant could hang on -- and CR 117.5's settle is where
-- Pawl.Engine.Engine takes it, CR 704.3's "whenever a player would get priority"
-- being the coarsest moment a player could act on the permission.
--
-- Not gated on the card being face down, Effect.GrantLookAtExiled's reason: a
-- stamp on a face-up exiled card is inert, `mayLookAt` reading Object.exileLookers
-- only where CR 406.3's face-up default has not already answered yes.
--
-- Pawl.ExileSpec's "CR 406.3 the look bob had while he controlled the land
-- survives losing it, and a land that exiled nothing gives him none" is what
-- proves the stamp outlives the control.
accrueLookers :: GameState.GameState -> GameState.GameState
accrueLookers gs =
  let accrue oid objs = case Map.lookup oid objs of
        Nothing -> objs
        Just obj
          | not (Set.member ExileLooker.TheExiler (Object.exileLookers obj)) -> objs
          | otherwise -> case exilerController oid gs of
              Nothing -> objs
              Just pid ->
                Map.insert
                  oid
                  obj {Object.exileLookers = Set.insert (ExileLooker.ThePlayer pid) (Object.exileLookers obj)}
                  objs
   in gs {GameState.objects = foldr accrue (GameState.objects gs) (Set.toList (GameState.exile gs))}

-- | CR 406.4's first half: may this player choose this exiled card SPECIFICALLY?
--
-- A face-up card is choosable by anybody, and a face-down one only by a player
-- allowed to look at it. The perspective is CR 109.5's "you" -- the player CR
-- 601.2c has choosing targets -- and Nothing is a genuinely absent one, which
-- takes the vacuous posture every player-referencing question in
-- Pawl.Engine.Target takes: no player is allowed to look, so no face-down card
-- is choosable.
--
-- A card this answers False for is not thereby unreachable: CR 406.4's second
-- half offers the chooser the PILE it sits in instead, which
-- Pawl.Engine.Target.piledOffer substitutes and Pawl.Engine.Target.drawFromPiles
-- draws out of.
mayChoose :: Maybe PlayerId -> ObjectId -> GameState.GameState -> Bool
mayChoose perspective oid gs = case perspective of
  Just pid -> mayLookAt pid oid gs
  Nothing -> not (maybe False Object.exiledFaceDown (Game.lookupObject oid gs))

-- | CR 406.4: which pile this card is in, or Nothing for one that is in none --
-- every card in exile face up, and every object outside exile.
--
-- CR 406.4's criteria are when and how the card was exiled. A FORETOLD card is
-- its own pile under CR 702.143e, which requires a player's foretold cards to
-- stay differentiable from each other and from the rest of their face-down
-- exiled cards, and Object.timestamp is CR 613.7d's record of when this
-- incarnation was exiled -- unique, so the pile is a singleton and the rule's
-- random draw over it names the one card in it.
--
-- Every OTHER face-down exiled card is sorted by the stamp
-- Pawl.Engine.Resolve.Effect.recordExilePile gave the instruction that exiled it, which
-- is the rule's two criteria together: one execution of one instruction is one
-- moment, and its being that instruction is the how. Two effects that each exile
-- a hand face down therefore make two piles, and a chooser who may not look picks
-- which of them to draw out of.
--
-- The FORETOLD arm wins where both would answer, since CR 702.143e is the
-- stronger separation: a card an effect exiled face down carries a pile stamp
-- already, and rule 702.143d can then make that card foretold where it sits, at
-- which point rule 702.143e wants it differentiable from the pile it was in.
--
-- A card in exile face down with NO stamp falls back to a pile of its own, and no
-- such card exists: every face-down exile pawl can reach carries
-- EntryRiders.exiledFaceDown, which only an effect sets (Pawl.Engine.Foretell is
-- the other writer, and its cards take the arm above), and every effect runs
-- inside recordExilePile's window. That fallback is unreachable rather than a
-- second reading of the rule, and it is the direction to be careful about rather
-- than the safe one -- a pile of one is a card the chooser has effectively named
-- -- which is why the writer's window is the thing to keep total.
pileOf :: ObjectId -> GameState.GameState -> Maybe Pile.Pile
pileOf oid gs = do
  obj <- Game.lookupObject oid gs
  Monad.guard (Set.member oid (GameState.exile gs) && Object.exiledFaceDown obj)
  pure $ case Object.foretold obj of
    Just _ -> Pile.OfForetold (Object.timestamp obj)
    Nothing -> Pile.OfFaceDown (Map.findWithDefault (Object.timestamp obj) oid (GameState.exilePiles gs))
