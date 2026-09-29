module Pawl.Codec.CountIsSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CountIs as CountIs
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.CountIs as CountIs.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Zone as Zone.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CountIs" $ do
  Spec.it s "the zone is a bare name" $
    Common.assertCodec
      s
      CountIs.codec
      (CountIs.Type.MkCountIs (Label.Type.MkLabel (Text.pack "bob")) Zone.Type.Graveyard (CardName.Type.MkCardName (Text.pack "Goblin Piker")) 2)
      " {\"player\":\"bob\",\"zone\":\"Graveyard\",\"card\":\"Goblin Piker\",\"count\":2} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CountIs.codec
