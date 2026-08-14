module Pawl.Codec.ModeSelection where

import qualified Pawl.Codec.ChooseBetween as ChooseBetween
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ModeSelection as ModeSelection

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema. 'ChooseBetween' already wrote a named object, so giving it a
-- record left every card file alone.
codec :: Codec.Codec ModeSelection.ModeSelection
codec =
  Arm.tagged
    [ Arm.payload "ChooseExactly" Common.natural ModeSelection.ChooseExactly (\x -> case x of ModeSelection.ChooseExactly y -> Just y; _ -> Nothing),
      -- CR 700.2d: "You may choose the same mode more than once." A separate tag
      -- rather than a field on the one above, so a card printing the ordinary
      -- instruction encodes exactly as it always did.
      Arm.payload "ChooseExactlyWithRepeats" Common.natural ModeSelection.ChooseExactlyWithRepeats (\x -> case x of ModeSelection.ChooseExactlyWithRepeats y -> Just y; _ -> Nothing),
      Arm.payload "ChooseBetween" ChooseBetween.codec ModeSelection.ChooseBetween (\x -> case x of ModeSelection.ChooseBetween y -> Just y; _ -> Nothing)
    ]
