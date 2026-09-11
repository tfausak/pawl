-- Rule 722 in the one voice the rest of the engine cannot supply for itself: the
-- two gates CR 722.3a puts on gaining the prepared designation, and CR 722.3c's
-- mint -- "as a permanent with a prepare spell gains the prepared designation or
-- phases in prepared, its controller creates a copy of that object in exile,
-- except that copy has only the characteristics of that permanent's prepare
-- spell".
--
-- The rule's other halves live where the questions they answer already live. CR
-- 722.3c's "for as long as the copy remains in exile, the prepared permanent's
-- controller may cast the copy" is a casting permission and is read by
-- Pawl.Engine.Cast.permitsCastFromExile; its "this copy remains in exile for as
-- long as the prepared permanent remains on the battlefield and has the prepared
-- designation" is that rule's stated exception to CR 704.5e and is swept by
-- Pawl.Engine.Sba; and its last sentence -- the permanent loses the designation
-- at the time the spell becomes cast (CR 601.2i) -- is written by
-- Pawl.Engine.Cast at that step. All three read Object.preparedCopyOf, which
-- `mint` below is the only writer of.
--
-- THE INVARIANT: rule 722 is part of the rulebook, so reading Layout.Preparation
-- and Designation.Prepared here is the same closed-half act as reading a Phase.
-- This module never asks which CARD is being prepared, and what the minted copy
-- DOES is the prepare face's own text -- card data, loaded like any other.
module Pawl.Engine.Prepare where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 722.2a: does this object have a PREPARE SPELL -- "an object for which these
-- alternative characteristics exist, even if the object currently doesn't use
-- them"?
--
-- A read of the object's COPIABLE VALUES through Pawl.Engine.Game.prepareSpellOf,
-- never of the printing behind Object.source, and CR 722.2b is why: "the existence
-- and values of these alternative characteristics are part of the object's
-- copiable values". So a Clone that entered as a copy of a preparation card has a
-- prepare spell and a preparation card that became a copy of something else has
-- none. That function's own comment says why CR 722.3c's "ignoring other
-- exceptions to the copying process" does not say otherwise.
--
-- Every zone, which is CR 722.2a's own scope -- "a card, spell, or permanent". A
-- token, an ability and an emblem have no card and no snapshot, and so no prepare
-- spell.
hasPrepareSpell :: ObjectId -> GameState -> Bool
hasPrepareSpell oid gs = case Game.prepareSpellOf oid gs of
  Nothing -> False
  Just _ -> True

-- CR 722.3a's FIRST gate: "a permanent can't gain this designation unless it has a
-- prepare spell". The rule's second gate -- it may not gain one it already has --
-- is the transition guard Pawl.Engine.Resolve.Effect's Designate arm already
-- applies to every designation, so it is not restated here.
--
-- A case on WHICH designation, which is the same closed-half act as a case on a
-- keyword (docs/design.md section 1): every other mark in Pawl.Types.Designation
-- is set by a rule that states no such precondition, so they answer True and the
-- rule that does state one answers for itself.
mayGain :: Designation.Designation -> ObjectId -> GameState -> Bool
mayGain designation oid gs = case designation of
  Designation.Renowned -> True
  Designation.Monstrous -> True
  Designation.Suspected -> True
  Designation.Solved -> True
  Designation.Prepared -> hasPrepareSpell oid gs

-- CR 722.3c, run immediately after the designation is written: "its controller
-- creates a copy of that object in exile, except that copy has only the
-- characteristics of that permanent's prepare spell, ignoring other exceptions to
-- the copying process that apply to that permanent. Those characteristics become
-- the copy's normal characteristics."
--
-- A no-op for every other designation, and for a permanent with no prepare spell
-- (CR 722.3a already refused that one at `mayGain` above) -- both by construction
-- rather than by a guard, since `mint` answers with the state unchanged.
mintOnDesignated :: Designation.Designation -> ObjectId -> Game ()
mintOnDesignated designation oid =
  Monad.when (designation == Designation.Prepared) (State.modify' (mint oid))

-- CR 722.3c's second trigger: "or phases in prepared". A permanent that phased
-- out still carrying the designation comes back prepared -- CR 702.26d makes the
-- phasing event not a zone change, so CR 400.7 mints no new incarnation for the
-- mark to be lost with -- and the rule mints a copy for that arrival exactly as
-- it does for a gain, so this is `mint` again and not a second minting road.
--
-- Nothing has to end the OLD copy first: `copyStands` below reads battlefield
-- membership, so the copy went with the permanent when it phased out and CR
-- 704.5e's sweep removed it. That is what makes rule 722.3c's two branches
-- disjoint rather than cumulative.
--
-- Called from Pawl.Engine.Phasing's phasing event once every returning permanent
-- is back in the battlefield set, which is the rule's own order -- the copy is
-- minted for a permanent that HAS phased in. That module's comment says why
-- nothing in the pool observes the order.
--
-- A no-op for a permanent that phases in UNPREPARED, which is every other
-- permanent that ever phases in.
mintOnPhasedIn :: ObjectId -> GameState -> GameState
mintOnPhasedIn oid gs =
  let prepared = maybe False (Set.member Designation.Prepared . Object.designations) (Game.lookupObject oid gs)
   in if prepared then mint oid gs else gs

-- CR 722.3c's copy itself, shared by the rule's two triggers above -- one
-- function so a permanent that gains the designation and one that phases in
-- prepared cannot come to mint different things.
--
-- The copy's card is the prepare face alone under Layout.Normal, which is that
-- last sentence exactly: a one-faced card whose characteristics are its normal
-- ones, so nothing downstream has to know it came off a preparation card. The
-- printing is interned like a token's (CR 111.3's road, Pawl.Engine.Event's
-- createTokens), and Source.OfCardCopy is what keeps it from being a card.
--
-- The face comes from the permanent's COPIABLE VALUES (hasPrepareSpell's read,
-- Pawl.Engine.Game.prepareSpellOf), which is CR 722.2b. "IGNORING OTHER
-- EXCEPTIONS TO THE COPYING PROCESS THAT APPLY TO THAT PERMANENT" narrows what is
-- minted rather than where the face is read: CR 707.9's riders -- a copy effect
-- that says "except it's a 1/1" -- do not reach this copy, which is exactly what
-- taking the prepare face WHOLE, rather than folding the permanent's projection
-- into it, already does.
--
-- Its OWNER is the permanent's controller, whom the rule names as the creator; CR
-- 108.4 gives an object created outside a card's ownership no other candidate. Who
-- may CAST it is asked live off the permanent instead (Pawl.Engine.Cast), because
-- CR 109.4 lets control change while the copy sits there.
--
-- No zone-change event is recorded: the copy is CREATED in exile rather than moved
-- there, so nothing was exiled and no "whenever a card is exiled" trigger has an
-- event to watch. CR 111.5's token-creation rollback has no counterpart here --
-- rule 722.3c states no replaceable creation event.
--
-- Answers with the state unchanged for a permanent with no prepare spell (CR
-- 722.3a already refused that one at `mayGain` above), by construction rather
-- than by a guard, since Pawl.Engine.Game.prepareSpellOf answers Nothing.
mint :: ObjectId -> GameState -> GameState
mint oid gs =
  case (Game.prepareSpellOf oid gs, Projection.controllerOf oid gs) of
    (Just face, Just controller) ->
      let copyCard = Card.Type.MkCard {Card.Type.layout = Layout.Normal, Card.Type.faces = pure face}
          (printingId, gs1) = Game.intern (Printing.MkPrinting copyCard) gs
          (copyId, gs2) = Game.freshObjectId gs1
          (ts, gs3) = Game.freshTimestamp gs2
          copy =
            Object.MkObject
              { Object.owner = controller,
                Object.enteredUnder = Nothing,
                Object.source = Source.OfCardCopy printingId,
                Object.zone = Zone.Exile,
                Object.tapped = TapState.Untapped,
                Object.facing = Facing.FaceUp,
                Object.flipped = False,
                -- CR 406.3's "exiled face down" is written by the effect that
                -- exiles; rule 722.3c states no such rider, so the copy is
                -- ordinary face-up exile that everybody may look at.
                Object.exiledFaceDown = False,
                Object.exileLookers = Set.empty,
                Object.damage = 0,
                Object.sickness = Sickness.Sick,
                Object.bindings = Map.empty,
                Object.counters = Map.empty,
                Object.counterTimestamps = Map.empty,
                Object.attachedTo = Nothing,
                Object.chosenColor = Nothing,
                Object.chosenSubtype = Nothing,
                Object.chosenNames = Set.empty,
                Object.chosenPlayer = Nothing,
                Object.timestamp = ts,
                -- One face, so there is no half to single out (CR 709.3b).
                Object.face = Nothing,
                Object.turnedOverAt = Nothing,
                Object.worldSince = Nothing,
                -- CR 722.3c's permission names the prepared permanent's
                -- controller LIVE, so it is asked of `preparedCopyOf` below
                -- rather than frozen into an ExilePlayPermission here.
                Object.playableFromExile = Nothing,
                Object.plotted = Nothing,
                Object.foretold = Nothing,
                Object.preparedCopyOf = Just oid,
                Object.ringBearerFor = Nothing,
                Object.protector = Nothing,
                Object.ventureRoom = Nothing,
                Object.classLevel = Nothing,
                Object.unlockedHalves = Set.empty,
                Object.designations = Set.empty,
                Object.designationValues = Map.empty,
                Object.kicked = Map.empty,
                Object.bestowed = False,
                Object.mutating = False,
                Object.prototyped = False,
                Object.boughtBack = False,
                Object.phyrexianLifePaid = 0,
                Object.manaSpent = Mana.MkMana [],
                Object.announcedX = Nothing,
                Object.castFrom = Nothing,
                Object.castUsing = Nothing,
                Object.detainedUntil = Set.empty,
                Object.goadedBy = Set.empty,
                Object.doesNotUntapNext = False,
                Object.exertedBy = Set.empty,
                Object.activatedOnce = Set.empty
              }
       in Game.insertIntoZone
            Zone.Exile
            LibraryPosition.Top
            controller
            copyId
            gs3 {GameState.objects = Map.insert copyId copy (GameState.objects gs3)}
    _ -> gs

-- CR 722.3c's residency condition, read by Pawl.Engine.Sba on every state-based
-- check: "this copy remains in exile for as long as the prepared permanent remains
-- on the battlefield and has the prepared designation. This is an exception to
-- rule 704.5e."
--
-- BOTH conjuncts, and neither is redundant: the permanent can lose the designation
-- while staying on the battlefield (CR 722.3b, and CR 601.2i when the copy itself
-- is cast), and it can leave the battlefield still prepared, in which case CR
-- 400.7's new incarnation has no designations and the id names an object in
-- another zone. So the read is LIVE off the named permanent rather than a flag
-- anything would have to unset.
--
-- Battlefield MEMBERSHIP and not Object.zone, which is
-- Pawl.Engine.Damage.onBattlefield's distinction and CR 702.26b's: a phased-out
-- permanent "is treated as though it doesn't exist" while its zone still reads
-- Zone.Battlefield (CR 702.26d), and Pawl.Engine.Phasing's design is that every
-- battlefield reader gets rule 702.26b for free by walking the SET. So a prepared
-- permanent that phases out stops keeping its copy, which is what makes CR
-- 722.3c's "or phases in prepared" branch a fresh mint rather than a second copy
-- beside the first. Pawl.PreparationSpec's "CR 702.26b Reality Ripple phases the
-- Aviator out and the copy ceases to exist" is what proves it, and its "CR 722.3c
-- the Aviator phases in prepared and mints a fresh Jump copy" the pair.
--
-- False for an object that is not a prepare copy at all, which is every object but
-- one per prepared permanent -- so the caller may ask it of anything.
copyStands :: Object.Object -> GameState -> Bool
copyStands copy gs = case Object.preparedCopyOf copy of
  Nothing -> False
  Just permanentId -> case Game.lookupObject permanentId gs of
    Nothing -> False
    Just permanent ->
      Set.member permanentId (GameState.battlefield gs)
        && Set.member Designation.Prepared (Object.designations permanent)

-- CR 601.2i by way of CR 722.3c's last sentence: "that permanent loses the
-- prepared designation at the time the spell becomes cast". Called by
-- Pawl.Engine.Cast with the id the cast copy carried in `preparedCopyOf` before CR
-- 601.2a's move, which is the only place that id is still readable -- CR 400.7's
-- new incarnation on the stack has none.
--
-- Removing the designation is the whole of it: the copy has already left exile, so
-- `copyStands` above has nothing left to sweep, and CR 722.3a's "can't gain this
-- designation if the permanent already has it" is what makes a second preparation
-- mint a second copy rather than a duplicate.
unprepare :: ObjectId -> Game ()
unprepare oid =
  State.modify'
    ( \gs ->
        gs
          { GameState.objects =
              Map.adjust
                (\o -> o {Object.designations = Set.delete Designation.Prepared (Object.designations o)})
                oid
                (GameState.objects gs)
          }
    )
