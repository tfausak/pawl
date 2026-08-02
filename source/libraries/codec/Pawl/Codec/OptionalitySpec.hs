module Pawl.Codec.OptionalitySpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Optionality as Optionality

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Optionality" $ do
  Spec.it s "Mandatory" $
    Common.assertJsonCodec s Optionality.toJson Optionality.fromJson Optionality.Mandatory "{\"type\":\"Mandatory\"}"
  Spec.it s "Optional" $
    Common.assertJsonCodec s Optionality.toJson Optionality.fromJson Optionality.Optional "{\"type\":\"Optional\"}"
  Spec.describe s "fromJsonDefault" $ do
    -- CR 603.5's "may" is printed text, so an absent key means Mandatory.
    Spec.it s "absent key decodes as Mandatory" $
      Common.assertFromJson s Optionality.fromJsonDefault "null" Optionality.Mandatory
    Spec.it s "Optional still decodes explicitly" $
      Common.assertFromJson s Optionality.fromJsonDefault "{\"type\":\"Optional\"}" Optionality.Optional
