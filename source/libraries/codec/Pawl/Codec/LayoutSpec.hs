module Pawl.Codec.LayoutSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Layout as Layout
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Layout as Layout

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Layout" $ do
  Spec.it s "Normal" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Normal
      " {\"type\":\"Normal\"} "
  -- CR 709.1.
  Spec.it s "Split" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Split
      " {\"type\":\"Split\"} "
  -- CR 709.5.
  Spec.it s "Room" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Room
      " {\"type\":\"Room\"} "
  -- CR 710.1.
  Spec.it s "Flip" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Flip
      " {\"type\":\"Flip\"} "
  -- CR 715.1.
  Spec.it s "Adventure" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Adventure
      " {\"type\":\"Adventure\"} "
  -- CR 720.1.
  Spec.it s "Omen" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Omen
      " {\"type\":\"Omen\"} "
  -- CR 722.1.
  Spec.it s "Preparation" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Preparation
      " {\"type\":\"Preparation\"} "
  -- CR 712.2.
  Spec.it s "Transforming" $
    Common.assertCodec
      s
      Layout.codec
      Layout.Transforming
      " {\"type\":\"Transforming\"} "
  -- CR 712.3.
  Spec.it s "ModalDoubleFaced" $
    Common.assertCodec
      s
      Layout.codec
      Layout.ModalDoubleFaced
      " {\"type\":\"ModalDoubleFaced\"} "
  -- CR 712.4.
  Spec.it s "Meld" $
    Common.assertCodec
      s
      Layout.codec
      (Layout.Meld (CardName.MkCardName (Text.pack "Graf Rats")))
      " {\"type\":\"Meld\",\"value\":\"Graf Rats\"} "
  -- CR 701.42b: the counterpart is what makes two meld cards a pair, so a meld
  -- card naming none must not load.
  Spec.it s "a Meld layout without its counterpart is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"Meld\"} ") >>= Codec.decode Layout.codec))
      "expected a bare Meld tag to fail to decode"
  -- Card layouts name more frames than have landed. A file naming one must
  -- fail loudly rather than fall back to Normal. A plane card needs no layout:
  -- Deck.planes is what puts it in the planar deck (CR 901.3).
  Spec.it s "a layout that has not landed is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"Planar\"} ") >>= Codec.decode Layout.codec))
      "expected an unknown layout tag to fail to decode"
  Spec.it s "has a schema" $
    Common.assertHasSchema s Layout.codec
