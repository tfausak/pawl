module Pawl.Codec.TransformedSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Transformed as Transformed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Transformed as Transformed

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Transformed" $ do
  -- CR 701.27a, with CR 701.27e's characteristic: the face the permanent turned
  -- to, by name.
  Spec.it s "MkTransformed, a permanent that turned to its back face" $
    Common.assertCodec
      s
      Transformed.codec
      ( Transformed.MkTransformed
          { Transformed.object = ObjectId.MkObjectId 1,
            Transformed.names = Set.singleton (CardName.MkCardName (Text.pack "Blightsower Thallid"))
          }
      )
      " {\"object\":1,\"names\":[\"Blightsower Thallid\"]} "
  -- CR 708.2a's none, which is a real answer rather than an absence: a permanent
  -- with no name turned over triggers no "transforms into" ability at all.
  Spec.it s "MkTransformed, a permanent with no name" $
    Common.assertCodec
      s
      Transformed.codec
      ( Transformed.MkTransformed
          { Transformed.object = ObjectId.MkObjectId 2,
            Transformed.names = Set.empty
          }
      )
      " {\"object\":2,\"names\":[]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Transformed.codec
