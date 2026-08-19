module Pawl.Codec.PerAttacker where

import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PerAttacker as PerAttacker

-- | Tagged rather than a bare mana cost with a fallback, so a card says which
-- kind of share it prints: {2} and {X} are not two spellings of one thing.
codec :: Codec.Codec PerAttacker.PerAttacker
codec =
  Arm.tagged
    [ Arm.payload "Fixed" ManaCost.codec PerAttacker.Fixed (\x -> case x of PerAttacker.Fixed y -> Just y; _ -> Nothing),
      Arm.payload "Counted" Quantity.codec PerAttacker.Counted (\x -> case x of PerAttacker.Counted y -> Just y; _ -> Nothing)
    ]
