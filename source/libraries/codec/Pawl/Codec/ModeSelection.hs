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
    encode
    [ Arm.payload "ChooseExactly" Common.natural ModeSelection.ChooseExactly,
      -- CR 700.2d: "You may choose the same mode more than once." A separate tag
      -- rather than a field on the one above, so a card printing the ordinary
      -- instruction encodes exactly as it always did.
      Arm.payload "ChooseExactlyWithRepeats" Common.natural ModeSelection.ChooseExactlyWithRepeats,
      Arm.payload "ChooseBetween" ChooseBetween.codec ModeSelection.ChooseBetween
    ]
  where
    encode m = case m of
      ModeSelection.ChooseExactly n -> Common.tagged "ChooseExactly" . Just $ Codec.encode Common.natural n
      ModeSelection.ChooseExactlyWithRepeats n -> Common.tagged "ChooseExactlyWithRepeats" . Just $ Codec.encode Common.natural n
      ModeSelection.ChooseBetween cb -> Common.tagged "ChooseBetween" . Just $ Codec.encode ChooseBetween.codec cb
