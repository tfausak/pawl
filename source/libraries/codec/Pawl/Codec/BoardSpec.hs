module Pawl.Codec.BoardSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Board as Board
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackOption as AttackOption.Type
import qualified Pawl.Types.Board as Board.Type
import qualified Pawl.Types.CombatStep as CombatStep.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Phase as Phase.Type
import qualified Pawl.Types.Seat as Seat.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Board" $ do
  Spec.it s "the step is its flat name" $
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.phase = Phase.Type.Combat CombatStep.Type.DeclareAttackers,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"DeclareAttackers\"} "
  Spec.it s "a monarch" $
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Just (Label.Type.MkLabel (Text.pack "bob")),
          Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\",\"monarch\":\"bob\"} "
  Spec.it s "an attack option, and none" $ do
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Just AttackOption.Type.Leftward
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\",\"attackOption\":\"Leftward\"} "
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Nothing
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\",\"attackOption\":null} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Board.codec

seat :: String -> Seat.Type.Seat
seat name =
  Seat.Type.MkSeat
    { Seat.Type.name = Label.Type.MkLabel (Text.pack name),
      Seat.Type.life = 20,
      Seat.Type.battlefield = Seq.empty,
      Seat.Type.hand = Seq.empty,
      Seat.Type.graveyard = Seq.empty,
      Seat.Type.library = Seq.empty,
      Seat.Type.exile = Seq.empty
    }
