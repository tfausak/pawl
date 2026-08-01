module Pawl.Codec.PoolSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Pool as Pool
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Pool as Pool

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Pool" $ do
  Spec.it s "Creatures" $
    Common.assertJsonCodec s Pool.toJson Pool.fromJson Pool.Creatures "{\"type\":\"Creatures\"}"
  Spec.it s "Players" $
    Common.assertJsonCodec s Pool.toJson Pool.fromJson Pool.Players "{\"type\":\"Players\"}"
  Spec.it s "AnyTarget" $
    Common.assertJsonCodec s Pool.toJson Pool.fromJson Pool.AnyTarget "{\"type\":\"AnyTarget\"}"
  Spec.it s "Permanents" $
    Common.assertJsonCodec s Pool.toJson Pool.fromJson Pool.Permanents "{\"type\":\"Permanents\"}"
  Spec.it s "Spells" $
    Common.assertJsonCodec s Pool.toJson Pool.fromJson Pool.Spells "{\"type\":\"Spells\"}"
  Spec.it s "SpellsAndPermanents" $
    Common.assertJsonCodec s Pool.toJson Pool.fromJson Pool.SpellsAndPermanents "{\"type\":\"SpellsAndPermanents\"}"
