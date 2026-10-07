module Pawl.Codec.AgainstLastCardExiledWithSpec where

import qualified Pawl.Codec.AgainstLastCardExiledWith as AgainstLastCardExiledWith
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AgainstLastCardExiledWith as AgainstLastCardExiledWith
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity

-- | Instantiated at 'Quantity.Quantity', the only concrete instantiation
-- anywhere: 'Pawl.Codec.Quantity' passes its own recursive codec in.
codec :: Codec.Codec (AgainstLastCardExiledWith.AgainstLastCardExiledWith Quantity.Quantity)
codec = AgainstLastCardExiledWith.codec Quantity.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AgainstLastCardExiledWith" $ do
  -- Which linked card to aim at, then what to read off it.
  Spec.it s "MkAgainstLastCardExiledWith" $
    Common.assertCodec
      s
      codec
      ( AgainstLastCardExiledWith.MkAgainstLastCardExiledWith
          { AgainstLastCardExiledWith.filter = Filter.HasCardType CardType.Creature,
            AgainstLastCardExiledWith.quantity = Quantity.Power
          }
      )
      " {\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"quantity\":{\"type\":\"Power\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
