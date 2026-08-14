module Pawl.Codec.Duration where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Duration as Duration

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Duration.Duration
codec =
  Arm.tagged
    [ Arm.nullary "UntilEndOfTurn" Duration.UntilEndOfTurn,
      Arm.nullary "Indefinite" Duration.Indefinite,
      Arm.nullary "UntilYourNextTurn" Duration.UntilYourNextTurn,
      Arm.nullary "UntilEndOfYourNextTurn" Duration.UntilEndOfYourNextTurn,
      Arm.payload "ForAsLongAs" Condition.codec Duration.ForAsLongAs (\x -> case x of Duration.ForAsLongAs y -> Just y; _ -> Nothing),
      Arm.nullary "UntilEndOfCombat" Duration.UntilEndOfCombat
    ]
