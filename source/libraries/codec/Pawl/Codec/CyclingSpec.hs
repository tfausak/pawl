{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CyclingSpec where

import qualified Pawl.Codec.Cycling as Cycling
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Cycling.Cycling Keyword.Keyword)
codec = Cycling.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Cycling" $ do
  -- CR 702.29a: plain cycling, so searchFor is Nothing.
  Spec.it s "MkCycling" $
    Common.assertCodec
      s
      codec
      ( Cycling.MkCycling
          { Cycling.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]), Cost.components = []},
            Cycling.searchFor = Nothing
          }
      )
      """ {"cost":{"mana":[{"type":"Generic","value":2}]},"searchFor":null} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
