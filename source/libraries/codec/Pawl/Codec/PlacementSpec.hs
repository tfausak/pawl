module Pawl.Codec.PlacementSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Placement as Placement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.CounterKind as CounterKind.Type
import qualified Pawl.Types.FaceDownReason as FaceDownReason.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Placement as Placement.Type
import qualified Pawl.Types.Readiness as Readiness.Type
import qualified Pawl.Types.TapState as TapState.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Placement" $ do
  Spec.it s "only a card is required" $
    Common.assertCodec s Placement.codec (plain "Goblin Piker") " {\"card\":\"Goblin Piker\"} "
  Spec.it s "every field" $
    Common.assertCodec
      s
      Placement.codec
      (plain "Goblin Piker")
        { Placement.Type.label = Just (Label.Type.MkLabel (Text.pack "piker")),
          Placement.Type.tapped = TapState.Type.Tapped,
          Placement.Type.readiness = Readiness.Type.Ready,
          Placement.Type.damage = 1,
          Placement.Type.counters = Map.singleton CounterKind.Type.PlusOnePlusOne 2,
          Placement.Type.token = True,
          Placement.Type.controller = Just (Label.Type.MkLabel (Text.pack "bob")),
          Placement.Type.attached = Just (Label.Type.MkLabel (Text.pack "bear")),
          Placement.Type.commander = True,
          Placement.Type.face = Just (CardName.Type.MkCardName (Text.pack "Nightfall Predator")),
          Placement.Type.faceDown = Just FaceDownReason.Type.Manifested,
          Placement.Type.protector = Just (Label.Type.MkLabel (Text.pack "carol"))
        }
      " {\"card\":\"Goblin Piker\",\"label\":\"piker\",\"tapped\":true,\"ready\":true,\"damage\":1,\"counters\":[{\"key\":{\"type\":\"PlusOnePlusOne\"},\"value\":2}],\"token\":true,\"controller\":\"bob\",\"attached\":\"bear\",\"commander\":true,\"face\":\"Nightfall Predator\",\"faceDown\":{\"type\":\"Manifested\"},\"protector\":\"carol\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Placement.codec

plain :: String -> Placement.Type.Placement
plain name =
  Placement.Type.MkPlacement
    { Placement.Type.card = CardName.Type.MkCardName (Text.pack name),
      Placement.Type.label = Nothing,
      Placement.Type.tapped = TapState.Type.Untapped,
      Placement.Type.readiness = Readiness.Type.Sick,
      Placement.Type.damage = 0,
      Placement.Type.counters = Map.empty,
      Placement.Type.token = False,
      Placement.Type.controller = Nothing,
      Placement.Type.attached = Nothing,
      Placement.Type.commander = False,
      Placement.Type.face = Nothing,
      Placement.Type.faceDown = Nothing,
      Placement.Type.protector = Nothing
    }
