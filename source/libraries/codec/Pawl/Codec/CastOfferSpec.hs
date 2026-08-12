{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CastOfferSpec where

import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastOffer as CastOffer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastOffer" $ do
  -- CR 310.11b's own offer: both riders written out.
  Spec.it s "MkCastOffer, transformed and without paying its mana cost" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = True, CastOffer.withoutPayingManaCost = True}
      """ {"transformed":true,"withoutPayingManaCost":true} """
  Spec.it s "MkCastOffer, an ordinary cast omits both keys" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.defaultValue
      """ {} """
  -- The two riders are independent (CR 712.11a is about a face, CR 118.9 about a
  -- cost), so each must survive on its own -- which is also what makes the
  -- elision above unambiguous.
  Spec.it s "MkCastOffer, transformed alone" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = True, CastOffer.withoutPayingManaCost = False}
      """ {"transformed":true} """
  Spec.it s "MkCastOffer, free alone" $
    Common.assertCodec
      s
      CastOffer.codec
      CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = True}
      """ {"withoutPayingManaCost":true} """
  Spec.describe s "defaultValue" $ do
    Spec.it s "carries neither rider" $
      Spec.assertEq s CastOffer.defaultValue CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = False}
    Spec.it s "a missing transformed key decodes as False" $
      Common.assertFromJson s (Codec.decode CastOffer.codec) "{\"withoutPayingManaCost\":false}" CastOffer.defaultValue
    Spec.it s "a missing withoutPayingManaCost key decodes as False" $
      Common.assertFromJson s (Codec.decode CastOffer.codec) "{\"transformed\":false}" CastOffer.defaultValue
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastOffer.codec
