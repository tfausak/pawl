{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModeSelectionSpec where

import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ModeSelection as ModeSelection

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ModeSelection" $ do
  Spec.it s "ChooseExactly" $
    Common.assertJsonCodec
      s
      ModeSelection.toJson
      ModeSelection.fromJson
      (ModeSelection.ChooseExactly 1)
      """ {"type":"ChooseExactly","value":1} """

  -- CR 700.2d's exception. The encoding of the default above is byte-identical to
  -- what it was before this constructor existed, which is what let every card
  -- file stay as written.
  Spec.it s "ChooseExactlyWithRepeats" $
    Common.assertJsonCodec
      s
      ModeSelection.toJson
      ModeSelection.fromJson
      (ModeSelection.ChooseExactlyWithRepeats 3)
      """ {"type":"ChooseExactlyWithRepeats","value":3} """
