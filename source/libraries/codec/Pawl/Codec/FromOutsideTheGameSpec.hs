module Pawl.Codec.FromOutsideTheGameSpec where

import qualified Pawl.Codec.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FromOutsideTheGame" $ do
  -- CR 400.11c / 701.20a. Burning Wish's shape: a quality, and the reveal it
  -- prints.
  Spec.it s "MkFromOutsideTheGame, revealing" $
    Common.assertCodec
      s
      FromOutsideTheGame.codec
      ( FromOutsideTheGame.MkFromOutsideTheGame
          { FromOutsideTheGame.filter = Filter.HasCardType CardType.Sorcery,
            FromOutsideTheGame.reveal = True
          }
      )
      " {\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Sorcery\"}},\"reveal\":true} "
  -- Death Wish's shape, and the pair is the point: both fields differ from the
  -- case above, so neither a dropped reveal nor a dropped filter round-trips.
  -- The empty And is CR 400.11c's "a card you own from outside the game" -- a
  -- quality the card does not state, which admits everything.
  Spec.it s "MkFromOutsideTheGame, no reveal and no stated quality" $
    Common.assertCodec
      s
      FromOutsideTheGame.codec
      ( FromOutsideTheGame.MkFromOutsideTheGame
          { FromOutsideTheGame.filter = Filter.And [],
            FromOutsideTheGame.reveal = False
          }
      )
      " {\"filter\":{\"type\":\"And\",\"value\":[]},\"reveal\":false} "
