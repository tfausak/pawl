{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerCounterTallySpec where

import qualified Pawl.Codec.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerRef as PlayerRef

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerCounterTally" $ do
  -- CR 122: how many counters of a kind a player already has -- a LEAF, so
  -- nothing here is a Quantity.
  Spec.it s "MkPlayerCounterTally" $
    Common.assertCodec
      s
      PlayerCounterTally.codec
      ( PlayerCounterTally.MkPlayerCounterTally
          { PlayerCounterTally.player = PlayerRef.EachPlayer,
            PlayerCounterTally.kind = PlayerCounterKind.Energy
          }
      )
      """ {"player":{"type":"EachPlayer"},"kind":{"type":"Energy"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerCounterTally.codec
