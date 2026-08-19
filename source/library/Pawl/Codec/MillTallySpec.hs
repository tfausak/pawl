module Pawl.Codec.MillTallySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MillTally as MillTally
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MillTally" $ do
  -- CR 728.1's own tally: "for each NONLAND card milled this way".
  Spec.it s "MkMillTally, a slot and a filter" $
    Common.assertCodec
      s
      MillTally.codec
      MillTally.MkMillTally
        { MillTally.slot = SlotName.MkSlotName (Text.pack "milled"),
          MillTally.filter = Filter.Not (Filter.HasCardType CardType.Land)
        }
      " {\"slot\":\"milled\",\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MillTally.codec
