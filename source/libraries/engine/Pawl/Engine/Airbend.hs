-- | CR 701.65, "airbend": the whole of the keyword action, as the two
-- instructions rule 701.65a spells it out as.
--
-- Pawl.Engine.Earthbend's sibling one rule over, standing on the same ground:
-- rule 701 is a keyword-action rule exactly as rule 702 is a keyword rule, so the
-- procedure lives in the engine rather than in card data. The closed\/open
-- invariant forbids the rules core casing on an EFFECT's identity, and nothing
-- here does -- Pawl.Engine.Resolve.Effect's Effect.Airbend arm calls in without
-- saying which effect it is.
--
-- The FIRST instruction, the exile, is already in the effect vocabulary, so it is
-- written as an Effect and the same executor runs it: the CR 400.7 funnel is what
-- applies replacement effects, raises the zone-change events and mints the
-- incarnation in exile.
--
-- The SECOND is rule 701.65a's standing permission, and that is the one thing
-- this module cannot write as card data. Effect.GrantPlayFromExile grants to CR
-- 109.5's "you" and states no amount, where rule 701.65a names the card's OWNER
-- and fixes the {2} itself; no card states either, so no opcode field carries
-- them and 'permission' mints them here.
--
-- Not implemented: CR 701.65b's "whenever a player airbends", which has no arm
-- anywhere and no GameEvent to hang one on. Avatar Aang is the printing that
-- wants it (#3918); rule 701.66b's earthbend and rule 701.67c's waterbend are
-- the siblings that have theirs.
module Pawl.Engine.Airbend where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Binding as Binding
import Pawl.Types.Card (Card)
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- | CR 701.65a's first sentence: "that player exiles those objects". Handed to
-- Pawl.Engine.Resolve.Effect's executor, so the exile runs exactly as the same
-- instruction printed on a card would -- ONE batch, which is CR 608.2f.
--
-- Every rider is Pawl.Codec.EntryRiders.defaultValue's, written out because that
-- value lives in the codec library: rule 701.65a states nothing about how the
-- objects arrive, and exile has no tapped, attacking or attached reading anyway.
--
-- The arrivals are bound under Binding.airbentObjects, which is how the
-- permission below finds them: rule 701.65a's "for each card exiled this way" is
-- a question about what the funnel actually minted, not about what the reference
-- named, so a member a replacement effect diverted or CR 608.2b dropped is not
-- among them.
exile :: ObjectRef.ObjectRef -> Effect Card (GrantedAbility.GrantedAbility Card)
exile ref =
  Effect.MoveToZone
    MoveToZone.MkMoveToZone
      { MoveToZone.ref = ref,
        MoveToZone.zone = Zone.Exile,
        MoveToZone.riders =
          EntryRiders.MkEntryRiders
            { EntryRiders.tapped = TapState.Untapped,
              EntryRiders.attacking = Nothing,
              EntryRiders.blocking = Nothing,
              EntryRiders.transformed = False,
              EntryRiders.counters = Map.empty,
              EntryRiders.underOwner = False,
              EntryRiders.exiledFaceDown = False,
              EntryRiders.attachedTo = Nothing,
              EntryRiders.faceDown = Nothing
            },
        MoveToZone.slot = Just Binding.airbentObjects,
        MoveToZone.origin = Nothing,
        MoveToZone.placement = LibraryPlacement.defaultValue,
        MoveToZone.duration = Nothing
      }

-- | CR 701.65a's second sentence: "for as long as it remains exiled, its owner
-- may cast it by paying {2} rather than paying its mana cost".
--
-- `player` is the card's OWNER and not CR 109.5's "you", which is rule 701.65a's
-- own word and the only permission in the tree that reads that way.
--
-- Expiry.Never is CR 611.2a's default for a permission stating no duration, and
-- "for as long as it remains exiled" is not one: CR 400.7 ends it for free, since
-- leaving exile mints an incarnation that carries no permission. CR 715.3d's
-- Adventure permission is written the same way for the same reason.
--
-- Origin.Granted, not Adventure: rule 715.3d's closing clause excludes its OWN
-- permission from casting an Adventure half, and this is not it.
--
-- No CR 118.14 rider: rule 701.65a says nothing about what mana may be spent, so
-- the {2} is paid with mana as produced.
permission :: ObjectId -> PlayerId -> ExilePlayPermission.ExilePlayPermission
permission source owner =
  ExilePlayPermission.MkExilePlayPermission
    { ExilePlayPermission.player = owner,
      ExilePlayPermission.source = source,
      ExilePlayPermission.expiry = Expiry.Never,
      ExilePlayPermission.spending = ManaSpending.AsProduced,
      ExilePlayPermission.alternativeManaCost = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
      ExilePlayPermission.origin = PlayPermissionOrigin.Granted
    }

-- | CR 701.65a's "for each CARD exiled this way": stamp 'permission' onto one
-- object the exile minted.
--
-- GATED on the object being a card, which rule 701.65a's own wording is: the
-- instruction exiles "permanents and\/or spells", and a token (CR 111.7) or a
-- copy of a spell (CR 707.10) that went to exile is neither a card nor castable,
-- so the sentence passes over it. Source.OfCard is that question -- a copy of a
-- card (Source.OfCardCopy) is not one either.
--
-- GATED on the object being in exile besides, which is rule 701.65a's "for as
-- long as it REMAINS exiled" read where the permission is written: a replacement
-- effect that redirected the move leaves nothing for the permission to be about.
grant :: ObjectId -> ObjectId -> GameState.GameState -> GameState.GameState
grant source oid gs =
  let stamp obj =
        if Object.zone obj == Zone.Exile && isCard (Object.source obj)
          then obj {Object.playableFromExile = Just (permission source (Object.owner obj))}
          else obj
      isCard s = case s of
        Source.OfCard _ -> True
        _ -> False
   in gs {GameState.objects = Map.adjust stamp oid (GameState.objects gs)}
