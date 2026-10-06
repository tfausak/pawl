module Pawl.Codec.WhenSpentSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.WhenSpent as WhenSpent
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.WhenSpent as WhenSpent

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.WhenSpent" $ do
  Spec.it s "a filter and the name of the ability it arms" $
    Common.assertCodec
      s
      WhenSpent.codec
      WhenSpent.MkWhenSpent
        { WhenSpent.casts = Filter.HasCardType CardType.Instant,
          WhenSpent.ability = AbilityName.MkAbilityName (Text.pack "copy")
        }
      " {\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Instant\"}},\"ability\":\"copy\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s WhenSpent.codec
