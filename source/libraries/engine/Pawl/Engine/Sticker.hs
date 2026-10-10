-- | CR 123.3: the stickers a player has access to, and putting one on an
-- object. Availability is computed, never stored: a move to a hidden zone frees
-- a sticker with no bookkeeping (CR 123.5).
module Pawl.Engine.Sticker where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AbilitySticker as AbilitySticker
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker
import qualified Pawl.Types.StickerKind as StickerKind
import qualified Pawl.Types.StickerPlacement as StickerPlacement
import qualified Pawl.Types.StickerPut as StickerPut
import qualified Pawl.Types.StickerRef as StickerRef
import qualified Pawl.Types.StickerSheet as StickerSheet

-- | CR 123.3a: every sticker on one sheet, by position.
refsOn :: PlayerId -> Natural -> StickerSheet.StickerSheet -> [StickerRef.StickerRef]
refsOn pid slot sheet =
  let refs kind n = [StickerRef.MkStickerRef {StickerRef.owner = pid, StickerRef.sheet = slot, StickerRef.kind = kind, StickerRef.index = i} | i <- List.genericTake n [0 :: Natural ..]]
   in refs StickerKind.Name (Natural.length (StickerSheet.names sheet))
        <> refs StickerKind.Ability (Natural.length (StickerSheet.abilities sheet))
        <> refs StickerKind.PowerToughness (Natural.length (StickerSheet.powerToughness sheet))
        <> refs StickerKind.Art (StickerSheet.art sheet)

-- | CR 123.3 / 123.2c: the stickers of these kinds on the player's chosen
-- sheets that are on no object they own, in any zone.
--
-- Not implemented: a ruling on a stickered card that changed owners; its
-- stickers count only against its current owner (#4888).
available :: PlayerId -> Set.Set StickerKind.StickerKind -> GameState -> [StickerRef.StickerRef]
available pid kinds gs = case Map.lookup pid (GameState.players gs) of
  Nothing -> []
  Just player ->
    let used = Set.fromList [StickerPlacement.sticker p | obj <- Map.elems (GameState.objects gs), Object.owner obj == pid, p <- Foldable.toList (Object.stickers obj)]
        printed =
          [ ref
          | (slot, sheet) <- zip [0 :: Natural ..] (Foldable.toList (Player.stickerSheets player)),
            Set.member slot (Player.chosenStickerSheets player),
            ref <- refsOn pid slot sheet,
            Set.member (StickerRef.kind ref) kinds
          ]
     in filter (\ref -> Set.notMember ref used) printed

-- | CR 123.3c / 107.17a: a sticker's ticket cost, printed on its sheet; a name
-- or art sticker has none.
ticketCost :: StickerRef.StickerRef -> GameState -> Natural
ticketCost ref gs = case StickerRef.kind ref of
  StickerKind.Ability -> maybe 0 AbilitySticker.tickets (Game.abilityStickerOf ref gs)
  StickerKind.PowerToughness -> maybe 0 PowerToughnessSticker.tickets (Game.powerToughnessStickerOf ref gs)
  StickerKind.Name -> 0
  StickerKind.Art -> 0

-- | CR 123.3 / 123.3c: the stickers `pid` may put on `oid`: `available`, less
-- each whose ticket cost is above `cap` and, unless the placement is free,
-- above the ticket counters of `oid`'s owner.
offered :: PlayerId -> ObjectId -> Set.Set StickerKind.StickerKind -> Maybe Natural -> Bool -> GameState -> [StickerRef.StickerRef]
offered pid oid kinds cap free gs =
  let owner = fmap Object.owner (Game.lookupObject oid gs)
      tickets = maybe 0 (Map.findWithDefault 0 PlayerCounterKind.Ticket . Player.counters) (owner >>= \o -> Map.lookup o (GameState.players gs))
      fits ref =
        let cost = ticketCost ref gs
         in maybe True (cost <=) cap && (free || cost <= tickets)
   in filter fits (available pid kinds gs)

-- | CR 123.3c / 107.17a: the owner of `oid` removes the sticker's ticket cost,
-- RemovePlayerCounters' road (Game.counterSharers).
payTickets :: ObjectId -> StickerRef.StickerRef -> GameState -> GameState
payTickets oid ref gs = case fmap Object.owner (Game.lookupObject oid gs) of
  Nothing -> gs
  Just owner ->
    let cost = ticketCost ref gs
        lose p = p {Player.counters = Map.adjust (\had -> had - min had cost) PlayerCounterKind.Ticket (Player.counters p)}
     in gs {GameState.players = List.foldl' (flip (Map.adjust lose)) (GameState.players gs) (Game.counterSharers PlayerCounterKind.Ticket owner gs)}

-- | CR 123.3 / 613.7k: put the sticker on the object, stamped now, and record
-- the placement for "whenever you place a sticker". CR 123.6b: @position@ is a
-- name sticker's.
put :: PlayerId -> ObjectId -> StickerRef.StickerRef -> Maybe Natural -> GameState -> GameState
put placer oid ref position gs =
  let (ts, stamped) = Game.freshTimestamp gs
      placement = StickerPlacement.MkStickerPlacement {StickerPlacement.sticker = ref, StickerPlacement.timestamp = ts, StickerPlacement.position = position}
      placed = stamped {GameState.objects = Map.adjust (\o -> o {Object.stickers = Object.stickers o Seq.|> placement}) oid (GameState.objects stamped)}
   in Event.recordEvent (GameEvent.StickerPut StickerPut.MkStickerPut {StickerPut.placer = placer, StickerPut.object = oid, StickerPut.kind = StickerRef.kind ref}) placed
