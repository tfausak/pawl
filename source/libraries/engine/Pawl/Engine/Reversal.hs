-- | CR 733.1's partition, built as a state rather than asked as a question:
-- the state a player ends in who reverses an illegal play but KEEPS the legal
-- mana abilities they activated while making it.
--
-- Three states go in -- where the action began, where the CR 605.3a mana window
-- opened, and where it closed -- and one comes out, holding the window's writes
-- and none of the announcement's. It is one-directional: the announcement's own
-- `before -> entry` diff is undone on top of `closed`, rather than the window
-- being replayed onto `before`. Replaying is not exact, and Crypt of Agadeem is
-- why -- a mana ability that counts the graveyard counts it with the
-- announcement's spell already gone from it, and CR 733.1 keeps the ability as
-- it HAPPENED.
--
-- Pawl.Engine.Cost.reverseIllegal is the caller.
module Pawl.Engine.Reversal where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState

-- CR 733.1: `closed` with the announcement undone -- "the entire action is
-- reversed and any payments already made are canceled", while the mana
-- abilities the payer activated stand. The arguments are where the action
-- began, where the payment's mana window opened, and where it closed.
--
-- Nothing where some leaf was written by BOTH sides to two different values,
-- which is a state this function declines to invent. That is CONSERVATIVE
-- rather than a claim of impossibility: Pawl.Engine.Cost then reverses the whole
-- action unasked, which is what every caller did before this function existed,
-- so the worst it can cost is the question. Nothing in the suite reaches it --
-- CostSpec's "Reversal" groups and Pawl.FaceDownSpec's would lose their prompt
-- if it did.
--
-- A TOTAL record construction, one bind per field, never a record update: a new
-- GameState field is then a `-Wmissing-fields` error here rather than a leaf
-- that silently keeps the announcement's value.
--
-- The `objects` and `combat` fields are whole-value leaves, so a permanent or a
-- declaration both sides wrote answers Nothing. That is the combat toll's case
-- and not this one -- CR 508.1f taps a creature the window may tap again -- and
-- the per-field descent those two need is #3867.
withoutAnnouncement :: GameState -> GameState -> GameState -> Maybe GameState
withoutAnnouncement before entry closed = do
  settings <- one GameState.settings
  objects <- mapOf GameState.objects
  library <- libraries GameState.library
  hand <- zoneOf GameState.hand
  graveyard <- zoneOf GameState.graveyard
  battlefield <- setOf GameState.battlefield
  phasedOut <- mapOf GameState.phasedOut
  exile <- setOf GameState.exile
  command <- setOf GameState.command
  stack <- listOf GameState.stack
  players <- mapOf GameState.players
  outsideObjects <- mapOf GameState.outsideObjects
  broughtIn <- seqOf GameState.broughtIn
  manaPool <- mapOf GameState.manaPool
  combat <- one GameState.combat
  events <- eventsOf GameState.events
  nextEventGroup <- newest GameState.nextEventGroup
  eventGroupDepth <- one GameState.eventGroupDepth
  lastKnown <- mapOf GameState.lastKnown
  stackArchive <- mapOf GameState.stackArchive
  scannedThrough <- one GameState.scannedThrough
  battlefieldWhenTriggered <- mapOfMaps GameState.battlefieldWhenTriggered
  controlSample <- mapOf GameState.controlSample
  damageScannedThrough <- one GameState.damageScannedThrough
  delayedTriggers <- seqOf GameState.delayedTriggers
  continuousEffects <- listOf GameState.continuousEffects
  copyEffects <- listOf GameState.copyEffects
  replacements <- listOf GameState.replacements
  pendingPreventionRiders <- seqOf GameState.pendingPreventionRiders
  pendingDamageEffects <- seqOf GameState.pendingDamageEffects
  ambientAmounts <- mapOf GameState.ambientAmounts
  detachedBindings <- mapOfMaps GameState.detachedBindings
  pendingEntryEffects <- seqOf GameState.pendingEntryEffects
  enteringBeside <- setOf GameState.enteringBeside
  enteringSubjects <- setOf GameState.enteringSubjects
  enteringCounters <- mapOfMaps GameState.enteringCounters
  playerEffects <- listOf GameState.playerEffects
  blockRequirements <- listOf GameState.blockRequirements
  attackRequirements <- listOf GameState.attackRequirements
  unregeneratables <- listOf GameState.unregeneratables
  blockProhibitions <- listOf GameState.blockProhibitions
  attackProhibitions <- listOf GameState.attackProhibitions
  activationProhibitions <- listOf GameState.activationProhibitions
  ignoredAbilities <- listOf GameState.ignoredAbilities
  turnOrder <- one GameState.turnOrder
  activePlayer <- one GameState.activePlayer
  phase <- one GameState.phase
  remaining <- one GameState.remaining
  priority <- one GameState.priority
  passes <- one GameState.passes
  turnNumber <- one GameState.turnNumber
  result <- one GameState.result
  restartSignal <- one GameState.restartSignal
  endTurnSignal <- one GameState.endTurnSignal
  nextObjectId <- newest GameState.nextObjectId
  printings <- newest GameState.printings
  printingIds <- newest GameState.printingIds
  nextPrintingId <- newest GameState.nextPrintingId
  nextTimestamp <- newest GameState.nextTimestamp
  lastChoice <- newest GameState.lastChoice
  drewFromEmpty <- setOf GameState.drewFromEmpty
  landsPlayed <- mapOf GameState.landsPlayed
  drawsThisTurn <- mapOf GameState.drawsThisTurn
  activatedThisTurn <- mapOfSets GameState.activatedThisTurn
  triggeredThisGame <- setOf GameState.triggeredThisGame
  pendingControl <- mapOf GameState.pendingControl
  control <- mapOf GameState.control
  monarch <- one GameState.monarch
  initiative <- one GameState.initiative
  daytime <- one GameState.daytime
  spellsCastLastTurn <- one GameState.spellsCastLastTurn
  castsLastTurn <- mapOf GameState.castsLastTurn
  exiledUntilMonarch <- mapOf GameState.exiledUntilMonarch
  movedUntilSourceLeaves <- mapOf GameState.movedUntilSourceLeaves
  haunting <- mapOf GameState.haunting
  exiledWith <- mapOf GameState.exiledWith
  exilePiles <- mapOf GameState.exilePiles
  extraTurns <- listOf GameState.extraTurns
  turnAnchor <- one GameState.turnAnchor
  pure
    GameState.MkGameState
      { GameState.settings = settings,
        GameState.objects = objects,
        GameState.library = library,
        GameState.hand = hand,
        GameState.graveyard = graveyard,
        GameState.battlefield = battlefield,
        GameState.phasedOut = phasedOut,
        GameState.exile = exile,
        GameState.command = command,
        GameState.stack = stack,
        GameState.players = players,
        GameState.outsideObjects = outsideObjects,
        GameState.broughtIn = broughtIn,
        GameState.manaPool = manaPool,
        GameState.combat = combat,
        GameState.events = events,
        GameState.nextEventGroup = nextEventGroup,
        GameState.eventGroupDepth = eventGroupDepth,
        GameState.lastKnown = lastKnown,
        GameState.stackArchive = stackArchive,
        GameState.scannedThrough = scannedThrough,
        GameState.battlefieldWhenTriggered = battlefieldWhenTriggered,
        GameState.controlSample = controlSample,
        GameState.damageScannedThrough = damageScannedThrough,
        GameState.delayedTriggers = delayedTriggers,
        GameState.continuousEffects = continuousEffects,
        GameState.copyEffects = copyEffects,
        GameState.replacements = replacements,
        GameState.pendingPreventionRiders = pendingPreventionRiders,
        GameState.pendingDamageEffects = pendingDamageEffects,
        GameState.ambientAmounts = ambientAmounts,
        GameState.detachedBindings = detachedBindings,
        GameState.pendingEntryEffects = pendingEntryEffects,
        GameState.enteringBeside = enteringBeside,
        GameState.enteringSubjects = enteringSubjects,
        GameState.enteringCounters = enteringCounters,
        GameState.playerEffects = playerEffects,
        GameState.blockRequirements = blockRequirements,
        GameState.attackRequirements = attackRequirements,
        GameState.unregeneratables = unregeneratables,
        GameState.blockProhibitions = blockProhibitions,
        GameState.attackProhibitions = attackProhibitions,
        GameState.activationProhibitions = activationProhibitions,
        GameState.ignoredAbilities = ignoredAbilities,
        GameState.turnOrder = turnOrder,
        GameState.activePlayer = activePlayer,
        GameState.phase = phase,
        GameState.remaining = remaining,
        GameState.priority = priority,
        GameState.passes = passes,
        GameState.turnNumber = turnNumber,
        GameState.result = result,
        GameState.restartSignal = restartSignal,
        GameState.endTurnSignal = endTurnSignal,
        GameState.nextObjectId = nextObjectId,
        GameState.printings = printings,
        GameState.printingIds = printingIds,
        GameState.nextPrintingId = nextPrintingId,
        GameState.nextTimestamp = nextTimestamp,
        GameState.lastChoice = lastChoice,
        GameState.drewFromEmpty = drewFromEmpty,
        GameState.landsPlayed = landsPlayed,
        GameState.drawsThisTurn = drawsThisTurn,
        GameState.activatedThisTurn = activatedThisTurn,
        GameState.triggeredThisGame = triggeredThisGame,
        GameState.pendingControl = pendingControl,
        GameState.control = control,
        GameState.monarch = monarch,
        GameState.initiative = initiative,
        GameState.daytime = daytime,
        GameState.spellsCastLastTurn = spellsCastLastTurn,
        GameState.castsLastTurn = castsLastTurn,
        GameState.exiledUntilMonarch = exiledUntilMonarch,
        GameState.movedUntilSourceLeaves = movedUntilSourceLeaves,
        GameState.haunting = haunting,
        GameState.exiledWith = exiledWith,
        GameState.exilePiles = exilePiles,
        GameState.extraTurns = extraTurns,
        GameState.turnAnchor = turnAnchor
      }
  where
    -- A whole value, compared as one leaf.
    one :: (Eq a) => (GameState -> a) -> Maybe a
    one field = leaf (field before) (field entry) (field closed)
    -- The window's value, always: the monotone counters, and the two intern
    -- tables `nextPrintingId` indexes. Giving back a counter the announcement
    -- advanced would hand an id or a timestamp out twice, which is a state no
    -- rule describes; CR 733.1 reverses a game action, not the bookkeeping.
    newest :: (GameState -> a) -> Maybe a
    newest field = Just (field closed)
    -- Per key, with an absent key its own value: a key only the announcement
    -- added goes, and one only the window added stays.
    mapOf :: (Ord k, Eq v) => (GameState -> Map.Map k v) -> Maybe (Map.Map k v)
    mapOf field = mapWith leaf (field before) (field entry) (field closed)
    mapOfMaps :: (Ord k, Ord j, Eq v) => (GameState -> Map.Map k (Map.Map j v)) -> Maybe (Map.Map k (Map.Map j v))
    mapOfMaps field = mapWith (mapWith leaf) (field before) (field entry) (field closed)
    mapOfSets :: (Ord k, Ord v) => (GameState -> Map.Map k (Set.Set v)) -> Maybe (Map.Map k (Set.Set v))
    mapOfSets field = mapWith members (field before) (field entry) (field closed)
    setOf :: (Ord a) => (GameState -> Set.Set a) -> Maybe (Set.Set a)
    setOf field = members (field before) (field entry) (field closed)
    listOf :: (Eq a) => (GameState -> [a]) -> Maybe [a]
    listOf field = Just (elements (field before) (field entry) (field closed))
    seqOf :: (Eq a) => (GameState -> Seq.Seq a) -> Maybe (Seq.Seq a)
    seqOf field = Just (Seq.fromList (elements (Foldable.toList (field before)) (Foldable.toList (field entry)) (Foldable.toList (field closed))))
    zoneOf :: (Ord k, Eq a) => (GameState -> Map.Map k (Seq.Seq a)) -> Maybe (Map.Map k (Seq.Seq a))
    zoneOf field = mapWith (\b e c -> Just (Seq.fromList (elements (Foldable.toList b) (Foldable.toList e) (Foldable.toList c)))) (field before) (field entry) (field closed)
    -- CR 733.1's last sentence: a shuffle is never reversed. Where neither side
    -- moved a library card the membership is unchanged and the window's ORDER
    -- stands -- which is every board this is reached on today, the
    -- announcement's whole diff being empty for every caller that gets here
    -- (Pawl.Engine.Cost.reverseIllegal).
    --
    -- Not implemented: where a card DID move, the element rule below runs and a
    -- shuffle performed beside that move goes back with it -- as does the move
    -- itself, which rule 733.1's last sentence forbids reversing as well. Both
    -- sides can make one. The announcement can cast from a library (Panglacial
    -- Wurm), and the window can too: CR 605.1a disqualifies an activated ability
    -- whose cost or effect MOVES a library card, which a shuffle does not
    -- (synthetic-shuffling-tomb), and CR 605.1b states no library clause at all,
    -- so a triggered mana ability resolved inline may even mill (#3119).
    libraries :: (Ord k, Ord a) => (GameState -> Map.Map k (Seq.Seq a)) -> Maybe (Map.Map k (Seq.Seq a))
    libraries field = mapWith ordering (field before) (field entry) (field closed)
      where
        ordering b e c
          | Set.fromList (Foldable.toList b) == Set.fromList (Foldable.toList c) = Just c
          | otherwise = Just (Seq.fromList (elements (Foldable.toList b) (Foldable.toList e) (Foldable.toList c)))
    -- CR 733.1: "no abilities trigger and no effects apply as a result of an
    -- undone action", so the announcement's own segment of the log goes and the
    -- window's stands. The log only grows, so that segment is the one between
    -- the two lengths.
    --
    -- GameState.scannedThrough and GameState.damageScannedThrough are the two
    -- fields that index this log by position, and both sit at or before
    -- `before`'s length: the CR 117.5 scan moves them at a priority boundary and
    -- during a resolution, and Pawl.Engine.Cost.applyManaTriggers deliberately
    -- leaves the watermark where it found it. So the dropped segment is past
    -- both, and `one` answers them unchanged.
    eventsOf :: (GameState -> Seq.Seq a) -> Maybe (Seq.Seq a)
    eventsOf field = Just (Seq.take (Seq.length (field before)) (field closed) <> Seq.drop (Seq.length (field entry)) (field closed))

-- The rule every leaf takes. `before` is where the action began, `entry` where
-- the mana window opened, `closed` where it shut.
--
-- Whichever side did not write it loses: the announcement's write is undone,
-- and the window's stands. Where both wrote it to one value the answer is that
-- value either way, and where they disagree there is nothing to answer.
leaf :: (Eq a) => a -> a -> a -> Maybe a
leaf before entry closed
  | before == entry = Just closed
  | entry == closed = Just before
  | before == closed = Just before
  | otherwise = Nothing

-- `leaf` reached through a map's keys, with a missing key read as a value of its
-- own so that an inserted key and a deleted one take the same rule as a changed
-- one. `combine` is the rule for a key all three hold.
mapWith :: (Ord k, Eq v) => (v -> v -> v -> Maybe v) -> Map.Map k v -> Map.Map k v -> Map.Map k v -> Maybe (Map.Map k v)
mapWith combine before entry closed =
  let at key = case (Map.lookup key before, Map.lookup key entry, Map.lookup key closed) of
        (Just b, Just e, Just c) -> fmap (\v -> (key, Just v)) (combine b e c)
        (b, e, c) -> fmap (\v -> (key, v)) (leaf b e c)
      keys = Set.unions [Map.keysSet before, Map.keysSet entry, Map.keysSet closed]
      held (key, value) = fmap ((,) key) value
   in fmap (Map.fromList . Maybe.mapMaybe held) (traverse at (Set.toList keys))

-- `leaf` reached through a set's members, each one's membership its own leaf.
members :: (Ord a) => Set.Set a -> Set.Set a -> Set.Set a -> Maybe (Set.Set a)
members before entry closed =
  let at x = fmap ((,) x) (leaf (Set.member x before) (Set.member x entry) (Set.member x closed))
   in fmap (Set.fromList . fmap fst . filter snd) (traverse at (Set.toList (Set.unions [before, entry, closed])))

-- `leaf` reached through a sequence's elements, which ORDER makes total: the
-- entry state's elements are what the announcement added, so what stays is
-- everything `before` held that the window did not take away, then everything
-- the window added. A conflict cannot arise -- an element is present or it is
-- not -- so this answers a sequence rather than a Maybe.
elements :: (Eq a) => [a] -> [a] -> [a] -> [a]
elements before entry closed =
  filter (\x -> elem x closed || notElem x entry) before
    <> filter (\x -> notElem x before && notElem x entry) closed
