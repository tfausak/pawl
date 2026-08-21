module Pawl.Codec.PerCreature where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PerCreature as PerCreature

-- | Tagged rather than a bare cost with a fallback, so a card says which kind of
-- share it prints: {2} and {X} are not two spellings of one thing.
codec :: Codec.Codec PerCreature.PerCreature
codec =
  Arm.tagged
    [ Arm.payload "Fixed" (Cost.codec Keyword.codec) PerCreature.Fixed (\x -> case x of PerCreature.Fixed y -> Just y; _ -> Nothing),
      Arm.payload "Counted" Quantity.codec PerCreature.Counted (\x -> case x of PerCreature.Counted y -> Just y; _ -> Nothing)
    ]
