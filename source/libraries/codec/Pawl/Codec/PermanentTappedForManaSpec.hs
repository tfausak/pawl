module Pawl.Codec.PermanentTappedForManaSpec where

import qualified Pawl.Codec.PermanentTappedForMana as PermanentTappedForMana
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PermanentTappedForMana as PermanentTappedForMana
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentTappedForMana" $ do
  -- Autumn Willow, Harmony's payload: whose tap fires it, and the quality the
  -- tapped permanent has to have.
  Spec.it s "MkPermanentTappedForMana, both keys" $
    Common.assertCodec
      s
      PermanentTappedForMana.codec
      ( PermanentTappedForMana.MkPermanentTappedForMana
          { PermanentTappedForMana.player = PlayerRelation.You,
            PermanentTappedForMana.filter = Filter.HasCardType CardType.Land
          }
      )
      " {\"player\":{\"type\":\"You\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentTappedForMana.codec
