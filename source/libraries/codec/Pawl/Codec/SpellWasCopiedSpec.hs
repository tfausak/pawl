module Pawl.Codec.SpellWasCopiedSpec where

import qualified Pawl.Codec.SpellWasCopied as SpellWasCopied
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SpellWasCopied as SpellWasCopied

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SpellWasCopied" $ do
  Spec.it s "MkSpellWasCopied, both keys" $
    Common.assertCodec
      s
      SpellWasCopied.codec
      ( SpellWasCopied.MkSpellWasCopied
          { SpellWasCopied.player = PlayerId.MkPlayerId 1,
            SpellWasCopied.copy = ObjectId.MkObjectId 2
          }
      )
      " {\"player\":1,\"copy\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SpellWasCopied.codec
