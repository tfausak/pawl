module Pawl.Codec.Duration where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Duration as Duration

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Duration.Duration
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "UntilEndOfTurn" Duration.UntilEndOfTurn,
      Arm.nullary "Indefinite" Duration.Indefinite,
      Arm.nullary "Perpetual" Duration.Perpetual,
      Arm.nullary "UntilYourNextTurn" Duration.UntilYourNextTurn,
      Arm.nullary "UntilEndOfYourNextTurn" Duration.UntilEndOfYourNextTurn,
      Arm.payload "UntilEndOfNextTurnOf" PlayerRef.codec Duration.UntilEndOfNextTurnOf (\x -> case x of Duration.UntilEndOfNextTurnOf y -> Just y; _ -> Nothing),
      Arm.payload "DuringNextTurnOf" PlayerRef.codec Duration.DuringNextTurnOf (\x -> case x of Duration.DuringNextTurnOf y -> Just y; _ -> Nothing),
      Arm.payload "ForAsLongAs" Condition.codec Duration.ForAsLongAs (\x -> case x of Duration.ForAsLongAs y -> Just y; _ -> Nothing),
      Arm.nullary "UntilEndOfCombat" Duration.UntilEndOfCombat,
      Arm.payload "UntilPaid" (Cost.codec Keyword.codec) Duration.UntilPaid (\x -> case x of Duration.UntilPaid y -> Just y; _ -> Nothing),
      Arm.nullary "UntilUsed" Duration.UntilUsed
    ]

tagOf :: Duration.Duration -> String
tagOf x = case x of
  Duration.UntilEndOfTurn {} -> "UntilEndOfTurn"
  Duration.Indefinite {} -> "Indefinite"
  Duration.Perpetual {} -> "Perpetual"
  Duration.UntilYourNextTurn {} -> "UntilYourNextTurn"
  Duration.UntilEndOfYourNextTurn {} -> "UntilEndOfYourNextTurn"
  Duration.UntilEndOfNextTurnOf {} -> "UntilEndOfNextTurnOf"
  Duration.DuringNextTurnOf {} -> "DuringNextTurnOf"
  Duration.ForAsLongAs {} -> "ForAsLongAs"
  Duration.UntilEndOfCombat {} -> "UntilEndOfCombat"
  Duration.UntilPaid {} -> "UntilPaid"
  Duration.UntilUsed {} -> "UntilUsed"
