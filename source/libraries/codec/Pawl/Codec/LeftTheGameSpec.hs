module Pawl.Codec.LeftTheGameSpec where

import qualified Pawl.Codec.LeftTheGame as LeftTheGame
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LeftTheGame as LeftTheGame
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LeftTheGame" $ do
  -- CR 729.4a: a card a subgame took out of a main-game graveyard.
  Spec.it s "MkLeftTheGame, both keys" $
    Common.assertCodec
      s
      LeftTheGame.codec
      ( LeftTheGame.MkLeftTheGame
          { LeftTheGame.object = ObjectId.MkObjectId 7,
            LeftTheGame.from = Zone.Graveyard
          }
      )
      " {\"object\":7,\"from\":{\"type\":\"Graveyard\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LeftTheGame.codec
