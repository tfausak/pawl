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
-- `mintOnDesignated` below is the only writer of.
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
import qualified Pawl.Engine.Card as Card
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
-- A read of the CARD through Pawl.Engine.Card.prepareFace, so it answers the same
-- for a preparation card wherever it lies, which is what CR 722.2a's "card, spell,
-- or permanent" asks for. A token, an ability and an emblem have no card and so no
-- prepare spell.
hasPrepareSpell :: ObjectId -> GameState -> Bool
hasPrepareSpell oid gs = case Game.cardOf oid gs >>= Card.prepareFace of
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
-- The copy's card is the prepare face alone under Layout.Normal, which is that
-- last sentence exactly: a one-faced card whose characteristics are its normal
-- ones, so nothing downstream has to know it came off a preparation card. The
-- printing is interned like a token's (CR 111.3's road, Pawl.Engine.Event's
-- createTokens), and Source.OfCardCopy is what keeps it from being a card.
--
-- "IGNORING OTHER EXCEPTIONS TO THE COPYING PROCESS" is why the face is taken off
-- the CARD rather than off Projection.copiableCharacteristics: a Clone of a
-- prepared permanent, or a copy effect with an exception, changes what the
-- permanent's copiable values are, and this rule says to disregard all of it and
-- read the prepare spell. CR 722.2b keeps the alternative characteristics
-- copiable, which is a claim about the PERMANENT's copiable values rather than
-- about this copy.
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
-- CR 722.3c's other trigger, "or phases in prepared", is not implemented: nothing
-- calls this from Pawl.Engine.Phasing, so a permanent that phases out prepared and
-- back in gets no second copy (#868).
--
-- A no-op for every other designation, and for a permanent with no prepare spell
-- (CR 722.3a already refused that one at `mayGain` above) -- both by construction
-- rather than by a guard, since `prepareFace` answers Nothing.
mintOnDesignated :: Designation.Designation -> ObjectId -> Game ()
mintOnDesignated designation oid = Monad.when (designation == Designation.Prepared) $ do
  gs <- State.get
  case (Game.cardOf oid gs >>= Card.prepareFace, Projection.controllerOf oid gs) of
    (Just face, Just controller) -> do
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
                Object.detainedUntil = Set.empty,
                Object.goadedBy = Set.empty,
                Object.doesNotUntapNext = False,
                Object.exertedBy = Set.empty,
                Object.activatedOnce = Set.empty
              }
      State.put
        ( Game.insertIntoZone
            Zone.Exile
            LibraryPosition.Top
            controller
            copyId
            gs3 {GameState.objects = Map.insert copyId copy (GameState.objects gs3)}
        )
    _ -> pure ()

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
-- False for an object that is not a prepare copy at all, which is every object but
-- one per prepared permanent -- so the caller may ask it of anything.
copyStands :: Object.Object -> GameState -> Bool
copyStands copy gs = case Object.preparedCopyOf copy of
  Nothing -> False
  Just permanentId -> case Game.lookupObject permanentId gs of
    Nothing -> False
    Just permanent ->
      Object.zone permanent == Zone.Battlefield
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
