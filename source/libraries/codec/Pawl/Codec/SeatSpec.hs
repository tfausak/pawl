module Pawl.Codec.SeatSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Seat as Seat
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Placement as Placement.Type
import qualified Pawl.Types.Readiness as Readiness.Type
import qualified Pawl.Types.Seat as Seat.Type
import qualified Pawl.Types.TapState as TapState.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Seat" $ do
  Spec.it s "life defaults to twenty and zones to empty" $
    Common.assertCodec s Seat.codec (empty "bob") " {\"name\":\"bob\"} "
  Spec.it s "every field" $
    Common.assertCodec
      s
      Seat.codec
      (empty "alice")
        { Seat.Type.life = 7,
          Seat.Type.battlefield = Seq.singleton (card "Mountain"),
          Seat.Type.hand = Seq.singleton (card "Lightning Bolt"),
          Seat.Type.graveyard = Seq.singleton (card "Goblin Piker"),
          Seat.Type.library = Seq.singleton (card "Island"),
          Seat.Type.exile = Seq.singleton (card "Bad Moon")
        }
      " {\"name\":\"alice\",\"life\":7,\"battlefield\":[{\"card\":\"Mountain\"}],\"hand\":[{\"card\":\"Lightning Bolt\"}],\"graveyard\":[{\"card\":\"Goblin Piker\"}],\"library\":[{\"card\":\"Island\"}],\"exile\":[{\"card\":\"Bad Moon\"}]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Seat.codec

empty :: String -> Seat.Type.Seat
empty name =
  Seat.Type.MkSeat
    { Seat.Type.name = Label.Type.MkLabel (Text.pack name),
      Seat.Type.life = 20,
      Seat.Type.battlefield = Seq.empty,
      Seat.Type.hand = Seq.empty,
      Seat.Type.graveyard = Seq.empty,
      Seat.Type.library = Seq.empty,
      Seat.Type.exile = Seq.empty
    }

card :: String -> Placement.Type.Placement
card name =
  Placement.Type.MkPlacement
    { Placement.Type.card = CardName.Type.MkCardName (Text.pack name),
      Placement.Type.label = Nothing,
      Placement.Type.tapped = TapState.Type.Untapped,
      Placement.Type.readiness = Readiness.Type.Sick,
      Placement.Type.damage = 0,
      Placement.Type.counters = Map.empty,
      Placement.Type.controller = Nothing,
      Placement.Type.attached = Nothing
    }
