{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModeSelectionSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ChooseBetween as ChooseBetween
import qualified Pawl.Types.ModeSelection as ModeSelection

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ModeSelection" $ do
  Spec.it s "ChooseExactly" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseExactly 1)
      """ {"type":"ChooseExactly","value":1} """

  -- CR 700.2d's exception. The encoding of the default above is byte-identical to
  -- what it was before this constructor existed, which is what let every card
  -- file stay as written.
  Spec.it s "ChooseExactlyWithRepeats" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseExactlyWithRepeats 3)
      """ {"type":"ChooseExactlyWithRepeats","value":3} """

  -- CR 700.2's "Choose one or both --" (data/cards/vandalize.json). Named fields
  -- rather than a two-element list, so the bound a card means is written down.
  Spec.it s "ChooseBetween" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseBetween (ChooseBetween.MkChooseBetween 1 2))
      """ {"type":"ChooseBetween","value":{"least":1,"most":2}} """

  Spec.it s "has a schema" $ Common.assertHasSchema s ModeSelection.codec

  -- The one invariant Pawl.Types.ModeSelection states, enforced by
  -- Pawl.Codec.ChooseBetween and reached through this tag.
  Spec.it s "rejects a minimum above the maximum" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"ChooseBetween","value":{"least":2,"most":1}} """) >>= Codec.decode ModeSelection.codec))
      "expected a decode failure"
