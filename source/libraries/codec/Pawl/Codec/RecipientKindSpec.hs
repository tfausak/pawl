module Pawl.Codec.RecipientKindSpec where

import qualified Pawl.Codec.RecipientKind as RecipientKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RecipientKind as RecipientKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RecipientKind" $ do
  -- CR 115.1a's tag, the one a card in data/cards writes today.
  Spec.it s "Creature" $
    Common.assertCodec
      s
      RecipientKind.codec
      RecipientKind.Creature
      " {\"type\":\"Creature\"} "
  -- Exhaustive where the literal above is representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RecipientKind.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RecipientKind.codec
