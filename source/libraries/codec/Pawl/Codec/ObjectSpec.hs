module Pawl.Codec.ObjectSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Object as Object
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Object" $ do
  -- CR 402.1: a card in a hand, which is most of what a game state holds. Every
  -- optional axis is at its absent value, so this case is where a codec that
  -- elided a null or wrote an empty array as an absent key is caught -- and
  -- where `plotted` is Nothing against the Just 0 below, which CR 702.170a makes
  -- two different cards rather than one.
  Spec.it s "a card in a hand" $
    Common.assertCodec
      s
      Object.codec
      Object.MkObject
        { Object.owner = PlayerId.MkPlayerId 1,
          Object.enteredUnder = Nothing,
          Object.source = Source.OfCard (PrintingId.MkPrintingId 2),
          Object.zone = Zone.Hand,
          Object.tapped = TapState.Untapped,
          Object.facing = Facing.FaceUp,
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
          Object.timestamp = Timestamp.MkTimestamp 3,
          Object.face = Nothing,
          Object.turnedOverAt = Nothing,
          Object.worldSince = Nothing,
          Object.playableFromExile = Nothing,
          Object.plotted = Nothing,
          Object.foretold = Nothing,
          Object.ringBearerFor = Nothing,
          Object.protector = Nothing,
          Object.ventureRoom = Nothing,
          Object.classLevel = Nothing,
          Object.unlockedHalves = Set.empty,
          Object.designations = Set.empty,
          Object.kicked = False,
          Object.bestowed = False,
          Object.phyrexianLifePaid = 0,
          Object.manaSpent = Mana.MkMana [],
          Object.announcedX = Nothing,
          Object.detainedUntil = Set.empty,
          Object.goadedBy = Set.empty,
          Object.doesNotUntapNext = False,
          Object.exertedBy = Set.empty
        }
      ( " {\"owner\":1,\"enteredUnder\":null,\"source\":{\"type\":\"OfCard\",\"value\":2}"
          <> ",\"zone\":{\"type\":\"Hand\"},\"tapped\":{\"type\":\"Untapped\"}"
          <> ",\"facing\":{\"type\":\"FaceUp\"},\"exiledFaceDown\":false,\"damage\":0"
          <> ",\"sickness\":{\"type\":\"Sick\"},\"bindings\":{},\"counters\":[]"
          <> ",\"counterTimestamps\":[],\"attachedTo\":null,\"chosenColor\":null"
          <> ",\"chosenSubtype\":null,\"chosenNames\":[],\"chosenPlayer\":null"
          <> ",\"timestamp\":3,\"face\":null,\"turnedOverAt\":null,\"worldSince\":null"
          <> ",\"playableFromExile\":null,\"plotted\":null,\"foretold\":null"
          <> ",\"ringBearerFor\":null,\"protector\":null,\"ventureRoom\":null"
          <> ",\"classLevel\":null,\"unlockedHalves\":[],\"designations\":[]"
          <> ",\"kicked\":false,\"bestowed\":false,\"phyrexianLifePaid\":0"
          <> ",\"manaSpent\":[]"
          <> ",\"announcedX\":null,\"detainedUntil\":[],\"goadedBy\":[]"
          <> ",\"doesNotUntapNext\":false,\"exertedBy\":[]} "
      )
  -- Every axis away from the case above, so no two same-typed fields hold the
  -- same value and a codec swapping a pair of them is caught: the six PlayerId
  -- axes are six seats, the four Timestamp axes four numbers, and the three
  -- Set PlayerId axes three different seats.
  --
  -- `counters` carries a kind at ZERO beside one at three. That is a state the
  -- engine really produces: Pawl.Engine.Damage takes CR 120.3c's loyalty
  -- counters off with Map.insert and a saturating subtraction, so a planeswalker
  -- dealt lethal damage keeps a Loyalty entry at 0. An encoder that dropped a
  -- zero would lose it, which the field's own absent-means-zero convention
  -- (Pawl.Types.Object) would make look harmless.
  Spec.it s "a permanent carrying every axis at once" $
    Common.assertCodec
      s
      Object.codec
      Object.MkObject
        { Object.owner = PlayerId.MkPlayerId 1,
          Object.enteredUnder = Just (PlayerId.MkPlayerId 2),
          Object.source = Source.OfToken (PrintingId.MkPrintingId 3),
          Object.zone = Zone.Battlefield,
          Object.tapped = TapState.Tapped,
          Object.facing =
            Facing.FaceDown
              FaceDownState.MkFaceDownState
                { FaceDownState.reason = FaceDownReason.Morphed,
                  FaceDownState.listed = FaceDownCharacteristics.defaultValue
                },
          Object.exiledFaceDown = True,
          Object.damage = 4,
          Object.sickness = Sickness.Settled (PlayerId.MkPlayerId 5),
          Object.bindings =
            Map.singleton
              (SlotName.MkSlotName (Text.pack "target"))
              Binding.MkBinding
                { Binding.targets = Just (Set.singleton (Recipient.ToCreature (ObjectId.MkObjectId 6))),
                  Binding.amount = Nothing,
                  Binding.modes = Nothing,
                  Binding.copy = Nothing,
                  Binding.objects = Nothing
                },
          Object.counters =
            Map.fromList
              [ (CounterKind.PlusOnePlusOne, 3),
                (CounterKind.Loyalty, 0)
              ],
          Object.counterTimestamps = Map.singleton CounterKind.PlusOnePlusOne (Timestamp.MkTimestamp 7),
          Object.attachedTo = Just (Recipient.ToCreature (ObjectId.MkObjectId 8)),
          Object.chosenColor = Just Color.Red,
          Object.chosenSubtype = Just Subtype.Goblin,
          Object.chosenNames = Set.singleton (CardName.MkCardName (Text.pack "Mountain")),
          Object.chosenPlayer = Just (PlayerId.MkPlayerId 9),
          Object.timestamp = Timestamp.MkTimestamp 10,
          Object.face = Just (CardName.MkCardName (Text.pack "Delver of Secrets")),
          Object.turnedOverAt = Just (Timestamp.MkTimestamp 11),
          Object.worldSince = Just (Timestamp.MkTimestamp 12),
          Object.playableFromExile =
            Just
              ExilePlayPermission.MkExilePlayPermission
                { ExilePlayPermission.player = PlayerId.MkPlayerId 13,
                  ExilePlayPermission.source = ObjectId.MkObjectId 14,
                  ExilePlayPermission.expiry = Expiry.AtCleanup,
                  ExilePlayPermission.spending = ManaSpending.AnyType,
                  ExilePlayPermission.origin = PlayPermissionOrigin.Granted
                },
          Object.plotted = Just 0,
          Object.foretold = Just 15,
          Object.ringBearerFor = Just (PlayerId.MkPlayerId 16),
          Object.protector = Just (PlayerId.MkPlayerId 17),
          Object.ventureRoom = Just (RoomIndex.MkRoomIndex 18),
          Object.classLevel = Just (ClassLevel.MkClassLevel 2),
          Object.unlockedHalves = Set.singleton (CardName.MkCardName (Text.pack "Fire")),
          Object.designations = Set.singleton Designation.Renowned,
          Object.kicked = True,
          Object.bestowed = True,
          Object.phyrexianLifePaid = 19,
          Object.manaSpent =
            Mana.MkMana
              [ ManaUnit.MkManaUnit
                  { ManaUnit.manaType = ManaType.Colored Color.Green,
                    ManaUnit.tags = Set.empty,
                    ManaUnit.retention = ManaRetention.Ordinary,
                    ManaUnit.restriction = Nothing,
                    ManaUnit.rider = Nothing
                  }
              ],
          Object.announcedX = Just 20,
          Object.detainedUntil = Set.singleton (PlayerId.MkPlayerId 21),
          Object.goadedBy = Set.singleton (PlayerId.MkPlayerId 22),
          Object.doesNotUntapNext = True,
          Object.exertedBy = Set.singleton (PlayerId.MkPlayerId 23)
        }
      ( " {\"owner\":1,\"enteredUnder\":2,\"source\":{\"type\":\"OfToken\",\"value\":3}"
          <> ",\"zone\":{\"type\":\"Battlefield\"},\"tapped\":{\"type\":\"Tapped\"}"
          <> ",\"facing\":{\"type\":\"FaceDown\",\"value\":{\"reason\":{\"type\":\"Morphed\"},\"listed\":{}}}"
          <> ",\"exiledFaceDown\":true,\"damage\":4"
          <> ",\"sickness\":{\"type\":\"Settled\",\"value\":5}"
          <> ",\"bindings\":{\"target\":{\"targets\":[{\"type\":\"ToCreature\",\"value\":6}]}}"
          <> ",\"counters\":[{\"key\":{\"type\":\"PlusOnePlusOne\"},\"value\":3}"
          <> ",{\"key\":{\"type\":\"Loyalty\"},\"value\":0}]"
          <> ",\"counterTimestamps\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"timestamp\":7}]"
          <> ",\"attachedTo\":{\"type\":\"ToCreature\",\"value\":8}"
          <> ",\"chosenColor\":{\"type\":\"Red\"},\"chosenSubtype\":{\"type\":\"Goblin\"}"
          <> ",\"chosenNames\":[\"Mountain\"],\"chosenPlayer\":9"
          <> ",\"timestamp\":10,\"face\":\"Delver of Secrets\",\"turnedOverAt\":11"
          <> ",\"worldSince\":12"
          <> ",\"playableFromExile\":{\"player\":13,\"source\":14,\"expiry\":{\"type\":\"AtCleanup\"}"
          <> ",\"spending\":{\"type\":\"AnyType\"},\"origin\":{\"type\":\"Granted\"}}"
          <> ",\"plotted\":0,\"foretold\":15,\"ringBearerFor\":16,\"protector\":17"
          <> ",\"ventureRoom\":18,\"classLevel\":2,\"unlockedHalves\":[\"Fire\"]"
          <> ",\"designations\":[{\"type\":\"Renowned\"}],\"kicked\":true"
          <> ",\"bestowed\":true"
          <> ",\"phyrexianLifePaid\":19"
          <> ",\"manaSpent\":[{\"manaType\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}"
          <> ",\"tags\":[],\"retention\":{\"type\":\"Ordinary\"},\"restriction\":null,\"rider\":null}]"
          <> ",\"announcedX\":20,\"detainedUntil\":[21],\"goadedBy\":[22]"
          <> ",\"doesNotUntapNext\":true,\"exertedBy\":[23]} "
      )
  Spec.it s "has a schema" $
    Common.assertHasSchema s Object.codec
