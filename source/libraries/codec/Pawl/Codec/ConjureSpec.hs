module Pawl.Codec.ConjureSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Conjure as Conjure
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureDestination as ConjureDestination

-- | The @card@ parameter is instantiated at 'Text.Text': this codec reaches it
-- only through the supplied codec, so any type proves the shape.
codec :: Codec.Codec (Conjure.Conjure Text.Text)
codec = Conjure.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Conjure" $ do
  -- Neither key is elided, so there is one form to prove.
  Spec.it s "MkConjure" $
    Common.assertCodec
      s
      codec
      ( Conjure.MkConjure
          { Conjure.card = Text.pack "Ornithopter",
            Conjure.destination = ConjureDestination.Hand
          }
      )
      " {\"card\":\"Ornithopter\",\"destination\":{\"type\":\"Hand\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
