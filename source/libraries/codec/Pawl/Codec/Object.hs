{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Object where

import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.CardIdentity as CardIdentity
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.ClassLevel as ClassLevel
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.ControlClock as ControlClock
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.ExileLooker as ExileLooker
import qualified Pawl.Codec.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Codec.Facing as Facing
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Mana as Mana
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Pairing as Pairing
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.RoomHalf as RoomHalf
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.Codec.Sickness as Sickness
import qualified Pawl.Codec.Source as Source
import qualified Pawl.Codec.StickerPlacement as StickerPlacement
import qualified Pawl.Codec.StoredResult as StoredResult
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterKind as CounterKind.Type
import qualified Pawl.Types.Designation as Designation.Type
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.PlayerId as PlayerId.Type
import qualified Pawl.Types.PrintingId as PrintingId.Type
import qualified Pawl.Types.Source as Source.Type
import qualified Pawl.Types.Timestamp as Timestamp.Type
import qualified Pawl.Types.Zone as Zone.Type

-- | CR 701.37c's X for one designation. A pair per mark through
-- 'Common.keyedList', because the key is structured, so it cannot be an object
-- key.
designationValue :: Codec.Codec (Designation.Type.Designation, Natural.Natural)
designationValue = Fields.object $ do
  designation <- Fields.required "designation" Designation.codec fst
  value <- Fields.required "value" Common.natural snd
  pure (designation, value)

-- | One counter kind and CR 613.7c's timestamp for it. A pair per kind through
-- 'Common.keyedList' rather than 'Common.multiset', which pairs a key with a
-- COUNT: the value here is a timestamp, so the shape Pawl.Codec.EntryRiders
-- writes is the one that fits.
counterTimestamp :: Codec.Codec (CounterKind.Type.CounterKind Keyword.Type.Keyword, Timestamp.Type.Timestamp)
counterTimestamp = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) fst
  timestamp <- Fields.required "timestamp" Timestamp.codec snd
  pure (kind, timestamp)

-- | Object.new, whose four arguments are 'Fields.required' keys and so are never
-- read off it: the one place every defaulted key's default comes from.
defaults :: Object.Object
defaults = Object.new (PlayerId.Type.MkPlayerId 0) (Source.Type.OfCard (PrintingId.Type.MkPrintingId 0)) Zone.Type.Hand (Timestamp.Type.MkTimestamp 0)

-- | `owner`, `source`, `zone`, `timestamp` and `sickness` are 'Fields.required'
-- -- none of them has a value that means "unset" -- and every other field is
-- 'Fields.defaulted' to its value in Object.new (read off `defaults`), so an
-- object that has done nothing writes those keys and nothing else. An absence
-- and the default are the same value in both directions, so the states the type's own haddock distinguishes survive: CR
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
  identity <- Fields.defaulted "identity" (Object.identity defaults) (Common.maybe CardIdentity.codec) Object.identity
  enteredUnder <- Fields.defaulted "enteredUnder" (Object.enteredUnder defaults) (Common.maybe PlayerId.codec) Object.enteredUnder
  source <- Fields.required "source" Source.codec Object.source
  zone <- Fields.required "zone" Zone.codec Object.zone
  tapped <- Fields.defaulted "tapped" (Object.tapped defaults) TapState.codec Object.tapped
  facing <- Fields.defaulted "facing" (Object.facing defaults) Facing.codec Object.facing
  flipped <- Fields.defaulted "flipped" (Object.flipped defaults) Common.boolean Object.flipped
  exiledFaceDown <- Fields.defaulted "exiledFaceDown" (Object.exiledFaceDown defaults) Common.boolean Object.exiledFaceDown
  damage <- Fields.defaulted "damage" (Object.damage defaults) Common.natural Object.damage
  sickness <- Fields.required "sickness" Sickness.codec Object.sickness
  controlClock <- Fields.defaulted "controlClock" (Object.controlClock defaults) (Common.keyedList ControlClock.entry) Object.controlClock
  bindings <- Fields.defaulted "bindings" (Object.bindings defaults) Binding.codecMap Object.bindings
  counters <- Fields.defaulted "counters" (Object.counters defaults) (Common.multiset (CounterKind.codec Keyword.codec)) Object.counters
  counterTimestamps <- Fields.defaulted "counterTimestamps" (Object.counterTimestamps defaults) (Common.keyedList counterTimestamp) Object.counterTimestamps
  attachedTo <- Fields.defaulted "attachedTo" (Object.attachedTo defaults) (Common.maybe Recipient.codec) Object.attachedTo
  chosenColors <- Fields.defaulted "chosenColors" (Object.chosenColors defaults) (Common.set Color.codec) Object.chosenColors
  chosenSubtype <- Fields.defaulted "chosenSubtype" (Object.chosenSubtype defaults) (Common.maybe Subtype.codec) Object.chosenSubtype
  chosenNames <- Fields.defaulted "chosenNames" (Object.chosenNames defaults) (Common.set CardName.codec) Object.chosenNames
  chosenPlayer <- Fields.defaulted "chosenPlayer" (Object.chosenPlayer defaults) (Common.maybe PlayerId.codec) Object.chosenPlayer
  timestamp <- Fields.required "timestamp" Timestamp.codec Object.timestamp
  face <- Fields.defaulted "face" (Object.face defaults) (Common.maybe CardName.codec) Object.face
  turnedOverAt <- Fields.defaulted "turnedOverAt" (Object.turnedOverAt defaults) (Common.maybe Timestamp.codec) Object.turnedOverAt
  worldSince <- Fields.defaulted "worldSince" (Object.worldSince defaults) (Common.maybe Timestamp.codec) Object.worldSince
  playableFromExile <- Fields.defaulted "playableFromExile" (Object.playableFromExile defaults) (Common.maybe ExilePlayPermission.codec) Object.playableFromExile
  plotted <- Fields.defaulted "plotted" (Object.plotted defaults) (Common.maybe Common.natural) Object.plotted
  foretold <- Fields.defaulted "foretold" (Object.foretold defaults) (Common.maybe Common.natural) Object.foretold
  foretellCostReduction <- Fields.defaulted "foretellCostReduction" (Object.foretellCostReduction defaults) (Common.maybe ManaCost.codec) Object.foretellCostReduction
  warped <- Fields.defaulted "warped" (Object.warped defaults) (Common.maybe Common.natural) Object.warped
  preparedCopyOf <- Fields.defaulted "preparedCopyOf" (Object.preparedCopyOf defaults) (Common.maybe ObjectId.codec) Object.preparedCopyOf
  ringBearerFor <- Fields.defaulted "ringBearerFor" (Object.ringBearerFor defaults) (Common.maybe PlayerId.codec) Object.ringBearerFor
  paired <- Fields.defaulted "paired" (Object.paired defaults) (Common.maybe Pairing.codec) Object.paired
  duplicate <- Fields.defaulted "duplicate" (Object.duplicate defaults) (Common.maybe ProjectedCharacteristics.codec) Object.duplicate
  stickers <- Fields.defaulted "stickers" (Object.stickers defaults) (Common.seq StickerPlacement.codec) Object.stickers
  protector <- Fields.defaulted "protector" (Object.protector defaults) (Common.maybe PlayerId.codec) Object.protector
  ventureRoom <- Fields.defaulted "ventureRoom" (Object.ventureRoom defaults) (Common.maybe RoomIndex.codec) Object.ventureRoom
  classLevel <- Fields.defaulted "classLevel" (Object.classLevel defaults) (Common.maybe ClassLevel.codec) Object.classLevel
  unlockedHalves <- Fields.defaulted "unlockedHalves" (Object.unlockedHalves defaults) (Common.set RoomHalf.codec) Object.unlockedHalves
  designations <- Fields.defaulted "designations" (Object.designations defaults) (Common.set Designation.codec) Object.designations
  designationValues <- Fields.defaulted "designationValues" (Object.designationValues defaults) (Common.keyedList designationValue) Object.designationValues
  storedResults <- Fields.defaulted "storedResults" (Object.storedResults defaults) (Common.multiset StoredResult.codec) Object.storedResults
  paidCosts <- Fields.defaulted "paidCosts" (Object.paidCosts defaults) (Common.multiset Keyword.codec) Object.paidCosts
  tributePaid <- Fields.defaulted "tributePaid" (Object.tributePaid defaults) Common.boolean Object.tributePaid
  bestowed <- Fields.defaulted "bestowed" (Object.bestowed defaults) Common.boolean Object.bestowed
  mutating <- Fields.defaulted "mutating" (Object.mutating defaults) Common.boolean Object.mutating
  prototyped <- Fields.defaulted "prototyped" (Object.prototyped defaults) Common.boolean Object.prototyped
  boughtBack <- Fields.defaulted "boughtBack" (Object.boughtBack defaults) Common.boolean Object.boughtBack
  unannounced <- Fields.defaulted "unannounced" (Object.unannounced defaults) Common.boolean Object.unannounced
  spliced <- Fields.defaulted "spliced" (Object.spliced defaults) (Common.seq PrintingId.codec) Object.spliced
  phyrexianLifePaid <- Fields.defaulted "phyrexianLifePaid" (Object.phyrexianLifePaid defaults) Common.natural Object.phyrexianLifePaid
  manaSpent <- Fields.defaulted "manaSpent" (Object.manaSpent defaults) Mana.codec Object.manaSpent
  announcedX <- Fields.defaulted "announcedX" (Object.announcedX defaults) (Common.maybe Common.natural) Object.announcedX
  castFrom <- Fields.defaulted "castFrom" (Object.castFrom defaults) (Common.maybe Zone.codec) Object.castFrom
  castUsing <- Fields.defaulted "castUsing" (Object.castUsing defaults) (Common.maybe Keyword.codec) Object.castUsing
  castGrant <- Fields.defaulted "castGrant" (Object.castGrant defaults) (Common.maybe Keyword.codec) Object.castGrant
  exileLookers <- Fields.defaulted "exileLookers" (Object.exileLookers defaults) (Common.set ExileLooker.codec) Object.exileLookers
  detainedUntil <- Fields.defaulted "detainedUntil" (Object.detainedUntil defaults) (Common.set PlayerId.codec) Object.detainedUntil
  goadedBy <- Fields.defaulted "goadedBy" (Object.goadedBy defaults) (Common.set PlayerId.codec) Object.goadedBy
  doesNotUntapFor <- Fields.defaulted "doesNotUntapFor" (Object.doesNotUntapFor defaults) Common.natural Object.doesNotUntapFor
  exertedBy <- Fields.defaulted "exertedBy" (Object.exertedBy defaults) (Common.set PlayerId.codec) Object.exertedBy
  -- Common.repeats rather than multiset: a spend count never falls to zero,
  -- and one entry per spend is the array this field was before it counted.
  activatedOnce <- Fields.defaulted "activatedOnce" (Object.activatedOnce defaults) (Common.repeats (ActivatedAbility.codec Card.codec (GrantedAbility.codec Card.codec))) Object.activatedOnce
  pure
    Object.MkObject
      { Object.owner = owner,
        Object.identity = identity,
        Object.enteredUnder = enteredUnder,
        Object.source = source,
        Object.zone = zone,
        Object.tapped = tapped,
        Object.facing = facing,
        Object.flipped = flipped,
        Object.exiledFaceDown = exiledFaceDown,
        Object.damage = damage,
        Object.sickness = sickness,
        Object.controlClock = controlClock,
        Object.bindings = bindings,
        Object.counters = counters,
        Object.counterTimestamps = counterTimestamps,
        Object.attachedTo = attachedTo,
        Object.chosenColors = chosenColors,
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
        Object.foretellCostReduction = foretellCostReduction,
        Object.warped = warped,
        Object.preparedCopyOf = preparedCopyOf,
        Object.ringBearerFor = ringBearerFor,
        Object.duplicate = duplicate,
        Object.stickers = stickers,
        Object.paired = paired,
        Object.protector = protector,
        Object.ventureRoom = ventureRoom,
        Object.classLevel = classLevel,
        Object.unlockedHalves = unlockedHalves,
        Object.designations = designations,
        Object.designationValues = designationValues,
        Object.storedResults = storedResults,
        Object.paidCosts = paidCosts,
        Object.tributePaid = tributePaid,
        Object.bestowed = bestowed,
        Object.mutating = mutating,
        Object.prototyped = prototyped,
        Object.boughtBack = boughtBack,
        Object.unannounced = unannounced,
        Object.spliced = spliced,
        Object.phyrexianLifePaid = phyrexianLifePaid,
        Object.manaSpent = manaSpent,
        Object.announcedX = announcedX,
        Object.castFrom = castFrom,
        Object.castUsing = castUsing,
        Object.castGrant = castGrant,
        Object.exileLookers = exileLookers,
        Object.detainedUntil = detainedUntil,
        Object.goadedBy = goadedBy,
        Object.doesNotUntapFor = doesNotUntapFor,
        Object.exertedBy = exertedBy,
        Object.activatedOnce = activatedOnce
      }
