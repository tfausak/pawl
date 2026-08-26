{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Object where

import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.ClassLevel as ClassLevel
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Codec.Facing as Facing
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Mana as Mana
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.Codec.Sickness as Sickness
import qualified Pawl.Codec.Source as Source
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterKind as CounterKind.Type
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Timestamp as Timestamp.Type

-- | One counter kind and CR 613.7c's timestamp for it. A pair per kind through
-- 'Common.keyedList' rather than 'Common.multiset', which pairs a key with a
-- COUNT: the value here is a timestamp, so the shape Pawl.Codec.EntryRiders
-- writes is the one that fits.
counterTimestamp :: Codec.Codec (CounterKind.Type.CounterKind Keyword.Type.Keyword, Timestamp.Type.Timestamp)
counterTimestamp = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) fst
  timestamp <- Fields.required "timestamp" Timestamp.codec snd
  pure (kind, timestamp)

-- | Every field 'Fields.required', the posture Pawl.Codec.Combat takes for the
-- rest of the game state: a Maybe is written as an explicit null rather than
-- elided, and an empty collection as an empty array. Several of the absences are
-- states in their own right rather than a zero -- CR 109.4's object with no
-- controller at all on `enteredUnder`, CR 702.170a's un-plotted card against a card
-- plotted on turn 0 -- and the type's own haddock argues each; writing them all
-- alike is what keeps the reader from having to know which.
--
-- `counters` is 'Common.multiset', whose entries are key/count objects, so a
-- kind sitting at ZERO survives the round trip: Pawl.Engine.Damage takes CR
-- 120.3c's loyalty and CR 120.3h's defense counters off with Map.insert and a
-- saturating subtraction, leaving the entry at 0 rather than pruning it --
-- which is the state CR 704.5i and CR 704.5v then read. `bindings` goes
-- through Pawl.Codec.Binding's own 'Binding.codecMap', which is the slot-name
-- keyed object.
codec :: Codec.Codec Object.Object
codec = Fields.object $ do
  owner <- Fields.required "owner" PlayerId.codec Object.owner
  enteredUnder <- Fields.required "enteredUnder" (Common.maybe PlayerId.codec) Object.enteredUnder
  source <- Fields.required "source" Source.codec Object.source
  zone <- Fields.required "zone" Zone.codec Object.zone
  tapped <- Fields.required "tapped" TapState.codec Object.tapped
  facing <- Fields.required "facing" Facing.codec Object.facing
  exiledFaceDown <- Fields.required "exiledFaceDown" Common.boolean Object.exiledFaceDown
  damage <- Fields.required "damage" Common.natural Object.damage
  sickness <- Fields.required "sickness" Sickness.codec Object.sickness
  bindings <- Fields.required "bindings" Binding.codecMap Object.bindings
  counters <- Fields.required "counters" (Common.multiset (CounterKind.codec Keyword.codec)) Object.counters
  counterTimestamps <- Fields.required "counterTimestamps" (Common.keyedList counterTimestamp) Object.counterTimestamps
  attachedTo <- Fields.required "attachedTo" (Common.maybe Recipient.codec) Object.attachedTo
  chosenColor <- Fields.required "chosenColor" (Common.maybe Color.codec) Object.chosenColor
  chosenSubtype <- Fields.required "chosenSubtype" (Common.maybe Subtype.codec) Object.chosenSubtype
  chosenNames <- Fields.required "chosenNames" (Common.set CardName.codec) Object.chosenNames
  chosenPlayer <- Fields.required "chosenPlayer" (Common.maybe PlayerId.codec) Object.chosenPlayer
  timestamp <- Fields.required "timestamp" Timestamp.codec Object.timestamp
  face <- Fields.required "face" (Common.maybe CardName.codec) Object.face
  turnedOverAt <- Fields.required "turnedOverAt" (Common.maybe Timestamp.codec) Object.turnedOverAt
  worldSince <- Fields.required "worldSince" (Common.maybe Timestamp.codec) Object.worldSince
  playableFromExile <- Fields.required "playableFromExile" (Common.maybe ExilePlayPermission.codec) Object.playableFromExile
  plotted <- Fields.required "plotted" (Common.maybe Common.natural) Object.plotted
  foretold <- Fields.required "foretold" (Common.maybe Common.natural) Object.foretold
  ringBearerFor <- Fields.required "ringBearerFor" (Common.maybe PlayerId.codec) Object.ringBearerFor
  protector <- Fields.required "protector" (Common.maybe PlayerId.codec) Object.protector
  ventureRoom <- Fields.required "ventureRoom" (Common.maybe RoomIndex.codec) Object.ventureRoom
  classLevel <- Fields.required "classLevel" (Common.maybe ClassLevel.codec) Object.classLevel
  unlockedHalves <- Fields.required "unlockedHalves" (Common.set CardName.codec) Object.unlockedHalves
  designations <- Fields.required "designations" (Common.set Designation.codec) Object.designations
  kicked <- Fields.required "kicked" Common.boolean Object.kicked
  bestowed <- Fields.required "bestowed" (Common.maybe Timestamp.codec) Object.bestowed
  phyrexianLifePaid <- Fields.required "phyrexianLifePaid" Common.natural Object.phyrexianLifePaid
  manaSpent <- Fields.required "manaSpent" Mana.codec Object.manaSpent
  announcedX <- Fields.required "announcedX" (Common.maybe Common.natural) Object.announcedX
  detainedUntil <- Fields.required "detainedUntil" (Common.set PlayerId.codec) Object.detainedUntil
  goadedBy <- Fields.required "goadedBy" (Common.set PlayerId.codec) Object.goadedBy
  doesNotUntapNext <- Fields.required "doesNotUntapNext" Common.boolean Object.doesNotUntapNext
  exertedBy <- Fields.required "exertedBy" (Common.set PlayerId.codec) Object.exertedBy
  pure
    Object.MkObject
      { Object.owner = owner,
        Object.enteredUnder = enteredUnder,
        Object.source = source,
        Object.zone = zone,
        Object.tapped = tapped,
        Object.facing = facing,
        Object.exiledFaceDown = exiledFaceDown,
        Object.damage = damage,
        Object.sickness = sickness,
        Object.bindings = bindings,
        Object.counters = counters,
        Object.counterTimestamps = counterTimestamps,
        Object.attachedTo = attachedTo,
        Object.chosenColor = chosenColor,
        Object.chosenSubtype = chosenSubtype,
        Object.chosenNames = chosenNames,
        Object.chosenPlayer = chosenPlayer,
        Object.timestamp = timestamp,
        Object.face = face,
        Object.turnedOverAt = turnedOverAt,
        Object.worldSince = worldSince,
        Object.playableFromExile = playableFromExile,
        Object.plotted = plotted,
        Object.foretold = foretold,
        Object.ringBearerFor = ringBearerFor,
        Object.protector = protector,
        Object.ventureRoom = ventureRoom,
        Object.classLevel = classLevel,
        Object.unlockedHalves = unlockedHalves,
        Object.designations = designations,
        Object.kicked = kicked,
        Object.bestowed = bestowed,
        Object.phyrexianLifePaid = phyrexianLifePaid,
        Object.manaSpent = manaSpent,
        Object.announcedX = announcedX,
        Object.detainedUntil = detainedUntil,
        Object.goadedBy = goadedBy,
        Object.doesNotUntapNext = doesNotUntapNext,
        Object.exertedBy = exertedBy
      }
