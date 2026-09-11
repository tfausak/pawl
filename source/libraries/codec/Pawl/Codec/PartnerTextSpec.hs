module Pawl.Codec.PartnerTextSpec where

import qualified Pawl.Codec.PartnerText as PartnerText
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PartnerText as PartnerText

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PartnerText" $ do
  Spec.it s "FriendsForever" $
    Common.assertCodec
      s
      PartnerText.codec
      PartnerText.FriendsForever
      " {\"type\":\"FriendsForever\"} "

  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PartnerText.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s PartnerText.codec
