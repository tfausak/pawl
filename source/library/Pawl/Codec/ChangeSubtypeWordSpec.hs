module Pawl.Codec.ChangeSubtypeWordSpec where

import qualified Pawl.Codec.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChangeSubtypeWord" $ do
  -- CR 612.1, as Artificial Evolution changes one creature type to another. Asymmetric on purpose: a swapped codec would round-trip an equal pair.
  Spec.it s "MkChangeSubtypeWord" $
    Common.assertCodec
      s
      ChangeSubtypeWord.codec
      ( ChangeSubtypeWord.MkChangeSubtypeWord
          { ChangeSubtypeWord.from = Subtype.Wall,
            ChangeSubtypeWord.to = Subtype.Bird
          }
      )
      " {\"from\":{\"type\":\"Wall\"},\"to\":{\"type\":\"Bird\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChangeSubtypeWord.codec
