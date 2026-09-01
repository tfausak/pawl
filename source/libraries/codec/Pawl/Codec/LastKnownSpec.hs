module Pawl.Codec.LastKnownSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.LastKnown as LastKnown
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Source as Source

-- | The bare projection an all-default value writes, reused as the `copiable`
-- half below.
minimalJson :: String
minimalJson = "{\"names\":[\"Mountain\"],\"cardTypes\":[{\"type\":\"Land\"}]}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LastKnown" $ do
  -- CR 608.2h, all ten axes. `characteristics` and `copiable` are the same type
  -- and hold DIFFERENT values here, because CR 707.2's layer-1-only reading is
  -- exactly what the whole fold loses -- an encoder writing one where the other
  -- belongs would round trip against equal values.
  Spec.it s "what an object was, with an attachment" $
    Common.assertCodec
      s
      LastKnown.codec
      LastKnown.MkLastKnown
        { LastKnown.characteristics = ProjectedCharacteristicsSpec.testCharacteristics,
          LastKnown.controller = PlayerId.MkPlayerId 1,
          LastKnown.owner = PlayerId.MkPlayerId 6,
          LastKnown.source = Source.OfCard (PrintingId.MkPrintingId 2),
          LastKnown.counters = Map.singleton CounterKind.PlusOnePlusOne 3,
          LastKnown.copiable = ProjectedCharacteristicsSpec.minimalCharacteristics,
          LastKnown.attachedTo = Just (Recipient.ToCreature (ObjectId.MkObjectId 8)),
          LastKnown.chosenNames = Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")),
          LastKnown.blocking = True,
          LastKnown.protector = Just (PlayerId.MkPlayerId 7)
        }
      ( " {\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"controller\":1,\"owner\":6,\"source\":{\"type\":\"OfCard\",\"value\":2}"
          <> ",\"counters\":[{\"key\":{\"type\":\"PlusOnePlusOne\"},\"value\":3}]"
          <> ",\"copiable\":"
          <> minimalJson
          <> ",\"attachedTo\":{\"type\":\"ToCreature\",\"value\":8}"
          <> ",\"chosenNames\":[\"Goblin Piker\"],\"blocking\":true,\"protector\":7} "
      )
  -- CR 109.3: neither an attachment nor a chosen name is a characteristic, and
  -- most objects have neither, so the absent case is written out rather than left
  -- to the case above.
  Spec.it s "an object that was attached to nothing, carried no counters and chose no name" $
    Common.assertCodec
      s
      LastKnown.codec
      LastKnown.MkLastKnown
        { LastKnown.characteristics = ProjectedCharacteristicsSpec.minimalCharacteristics,
          LastKnown.controller = PlayerId.MkPlayerId 4,
          LastKnown.owner = PlayerId.MkPlayerId 4,
          LastKnown.source = Source.OfToken (PrintingId.MkPrintingId 5),
          LastKnown.counters = Map.empty,
          LastKnown.copiable = ProjectedCharacteristicsSpec.minimalCharacteristics,
          LastKnown.attachedTo = Nothing,
          LastKnown.chosenNames = Set.empty,
          LastKnown.blocking = False,
          LastKnown.protector = Nothing
        }
      ( " {\"characteristics\":"
          <> minimalJson
          <> ",\"controller\":4,\"owner\":4,\"source\":{\"type\":\"OfToken\",\"value\":5}"
          <> ",\"counters\":[]"
          <> ",\"copiable\":"
          <> minimalJson
          <> ",\"attachedTo\":null"
          <> ",\"chosenNames\":[],\"blocking\":false,\"protector\":null} "
      )
  Spec.it s "has a schema" $
    Common.assertHasSchema s LastKnown.codec
