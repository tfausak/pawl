{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Object where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.ClassLevel as ClassLevel
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Cost as Cost
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
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind.Type
import qualified Pawl.Types.Facing as Facing.Type
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.TapState as TapState.Type
import qualified Pawl.Types.Timestamp as Timestamp.Type

-- | CR 702.33d: one of a spell's kicker costs and how many times its controller
-- declared it (CR 702.33c). A pair per cost through 'Common.keyedList' for
-- 'counterTimestamp' below's reason -- the value is a count the wire states
-- rather than a repeat it could spell -- and possibly EMPTY, since that is every
-- object no kicker was announced for.
kickerPayment :: Codec.Codec (Cost.Type.Cost Keyword.Type.Keyword, Natural.Natural)
kickerPayment = Fields.object $ do
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) fst
  times <- Fields.required "times" Common.natural snd
  pure (cost, times)

-- | One counter kind and CR 613.7c's timestamp for it. A pair per kind through
-- 'Common.keyedList' rather than 'Common.multiset', which pairs a key with a
-- COUNT: the value here is a timestamp, so the shape Pawl.Codec.EntryRiders
-- writes is the one that fits.
counterTimestamp :: Codec.Codec (CounterKind.Type.CounterKind Keyword.Type.Keyword, Timestamp.Type.Timestamp)
counterTimestamp = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) fst
  timestamp <- Fields.required "timestamp" Timestamp.codec snd
  pure (kind, timestamp)

-- | `owner`, `source`, `zone`, `timestamp` and `sickness` are 'Fields.required'
-- -- none of them has a value that means "unset" -- and every other field is
-- 'Fields.defaulted', so an object that has done nothing writes those keys and
-- nothing else. An absence and the default are the same value in both
-- directions, so the states the type's own haddock distinguishes survive: CR
-- 109.4's object with no controller at all is `enteredUnder` absent, and CR
-- 702.170a's un-plotted card is `plotted` absent against a `"plotted":0` for one
-- plotted on turn 0.
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
  enteredUnder <- Fields.defaulted "enteredUnder" Nothing (Common.maybe PlayerId.codec) Object.enteredUnder
  source <- Fields.required "source" Source.codec Object.source
  zone <- Fields.required "zone" Zone.codec Object.zone
  tapped <- Fields.defaulted "tapped" TapState.Type.Untapped TapState.codec Object.tapped
  facing <- Fields.defaulted "facing" Facing.Type.FaceUp Facing.codec Object.facing
  exiledFaceDown <- Fields.defaulted "exiledFaceDown" False Common.boolean Object.exiledFaceDown
  damage <- Fields.defaulted "damage" 0 Common.natural Object.damage
  sickness <- Fields.required "sickness" Sickness.codec Object.sickness
  bindings <- Fields.defaulted "bindings" Map.empty Binding.codecMap Object.bindings
  counters <- Fields.defaulted "counters" Map.empty (Common.multiset (CounterKind.codec Keyword.codec)) Object.counters
  counterTimestamps <- Fields.defaulted "counterTimestamps" Map.empty (Common.keyedList counterTimestamp) Object.counterTimestamps
  attachedTo <- Fields.defaulted "attachedTo" Nothing (Common.maybe Recipient.codec) Object.attachedTo
  chosenColor <- Fields.defaulted "chosenColor" Nothing (Common.maybe Color.codec) Object.chosenColor
  chosenSubtype <- Fields.defaulted "chosenSubtype" Nothing (Common.maybe Subtype.codec) Object.chosenSubtype
  chosenNames <- Fields.defaulted "chosenNames" Set.empty (Common.set CardName.codec) Object.chosenNames
  chosenPlayer <- Fields.defaulted "chosenPlayer" Nothing (Common.maybe PlayerId.codec) Object.chosenPlayer
  timestamp <- Fields.required "timestamp" Timestamp.codec Object.timestamp
  face <- Fields.defaulted "face" Nothing (Common.maybe CardName.codec) Object.face
  turnedOverAt <- Fields.defaulted "turnedOverAt" Nothing (Common.maybe Timestamp.codec) Object.turnedOverAt
  worldSince <- Fields.defaulted "worldSince" Nothing (Common.maybe Timestamp.codec) Object.worldSince
  playableFromExile <- Fields.defaulted "playableFromExile" Nothing (Common.maybe ExilePlayPermission.codec) Object.playableFromExile
  plotted <- Fields.defaulted "plotted" Nothing (Common.maybe Common.natural) Object.plotted
  foretold <- Fields.defaulted "foretold" Nothing (Common.maybe Common.natural) Object.foretold
  ringBearerFor <- Fields.defaulted "ringBearerFor" Nothing (Common.maybe PlayerId.codec) Object.ringBearerFor
  protector <- Fields.defaulted "protector" Nothing (Common.maybe PlayerId.codec) Object.protector
  ventureRoom <- Fields.defaulted "ventureRoom" Nothing (Common.maybe RoomIndex.codec) Object.ventureRoom
  classLevel <- Fields.defaulted "classLevel" Nothing (Common.maybe ClassLevel.codec) Object.classLevel
  unlockedHalves <- Fields.defaulted "unlockedHalves" Set.empty (Common.set CardName.codec) Object.unlockedHalves
  designations <- Fields.defaulted "designations" Set.empty (Common.set Designation.codec) Object.designations
  kicked <- Fields.defaulted "kicked" Map.empty (Common.keyedList kickerPayment) Object.kicked
  bestowed <- Fields.defaulted "bestowed" False Common.boolean Object.bestowed
  phyrexianLifePaid <- Fields.defaulted "phyrexianLifePaid" 0 Common.natural Object.phyrexianLifePaid
  manaSpent <- Fields.defaulted "manaSpent" (Mana.Type.MkMana []) Mana.codec Object.manaSpent
  announcedX <- Fields.defaulted "announcedX" Nothing (Common.maybe Common.natural) Object.announcedX
  detainedUntil <- Fields.defaulted "detainedUntil" Set.empty (Common.set PlayerId.codec) Object.detainedUntil
  goadedBy <- Fields.defaulted "goadedBy" Set.empty (Common.set PlayerId.codec) Object.goadedBy
  doesNotUntapNext <- Fields.defaulted "doesNotUntapNext" False Common.boolean Object.doesNotUntapNext
  exertedBy <- Fields.defaulted "exertedBy" Set.empty (Common.set PlayerId.codec) Object.exertedBy
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
