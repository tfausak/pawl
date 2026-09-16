module Pawl.Codec.FromOutsideTheGameSpec where

import qualified Pawl.Codec.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.OutsideDestination as OutsideDestination

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FromOutsideTheGame" $ do
  -- CR 400.11c / 701.20a. Burning Wish's shape: a quality, and the reveal it
  -- prints.
  Spec.it s "MkFromOutsideTheGame, revealing" $
    Common.assertCodec
      s
      FromOutsideTheGame.codec
      ( FromOutsideTheGame.MkFromOutsideTheGame
          { FromOutsideTheGame.destination = OutsideDestination.Hand,
            FromOutsideTheGame.filter = Filter.HasCardType CardType.Sorcery,
            FromOutsideTheGame.reveal = True
          }
      )
      " {\"destination\":{\"type\":\"Hand\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Sorcery\"}},\"reveal\":true} "
  -- The Raven's Warning's shape, and the pair is the point: all three fields
  -- differ from the case above, so a dropped reveal, a dropped filter and a
  -- dropped destination alike fail to round-trip. The empty And is CR 400.11c's
  -- "a card you own from outside the game" -- a quality that card does not state
  -- either, which admits everything.
  Spec.it s "MkFromOutsideTheGame, no reveal, no stated quality, and a library destination" $
    Common.assertCodec
      s
      FromOutsideTheGame.codec
      ( FromOutsideTheGame.MkFromOutsideTheGame
          { FromOutsideTheGame.destination = OutsideDestination.LibraryTop,
            FromOutsideTheGame.filter = Filter.And [],
            FromOutsideTheGame.reveal = False
          }
      )
      " {\"destination\":{\"type\":\"LibraryTop\"},\"filter\":{\"type\":\"And\",\"value\":[]},\"reveal\":false} "
