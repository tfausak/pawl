module Pawl.Codec.Duration where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Duration as Duration

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Duration.Duration
codec =
  Arm.tagged
    encode
    [ Arm.nullary "UntilEndOfTurn" Duration.UntilEndOfTurn,
      Arm.nullary "Indefinite" Duration.Indefinite,
      Arm.nullary "UntilYourNextTurn" Duration.UntilYourNextTurn,
      Arm.payload "ForAsLongAs" Condition.codec Duration.ForAsLongAs,
      Arm.nullary "UntilEndOfCombat" Duration.UntilEndOfCombat
    ]
  where
    encode d = case d of
      Duration.UntilEndOfTurn -> Common.nullary "UntilEndOfTurn"
      Duration.Indefinite -> Common.nullary "Indefinite"
      Duration.UntilYourNextTurn -> Common.nullary "UntilYourNextTurn"
      Duration.ForAsLongAs c -> Common.tagged "ForAsLongAs" . Just $ Codec.encode Condition.codec c
      Duration.UntilEndOfCombat -> Common.nullary "UntilEndOfCombat"
