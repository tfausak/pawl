module Pawl.Codec.CantSearchLibrariesSpec where

import qualified Pawl.Codec.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CantSearchLibraries" $ do
  -- Leonin Arbiter's unqualified "players can't search libraries": no library
  -- narrowed and no cause narrowed.
  Spec.it s "MkCantSearchLibraries unqualified" $
    Common.assertCodec
      s
      CantSearchLibraries.codec
      CantSearchLibraries.MkCantSearchLibraries
        { CantSearchLibraries.library = PlayerScope.EachPlayer,
          CantSearchLibraries.cause = PlayerScope.EachPlayer
        }
      " {\"library\":{\"type\":\"EachPlayer\"},\"cause\":{\"type\":\"EachPlayer\"}} "
  -- Ashiok, Dream Render: both axes narrowed to the prohibited player.
  Spec.it s "MkCantSearchLibraries narrowed on both axes" $
    Common.assertCodec
      s
      CantSearchLibraries.codec
      CantSearchLibraries.MkCantSearchLibraries
        { CantSearchLibraries.library = PlayerScope.You,
          CantSearchLibraries.cause = PlayerScope.You
        }
      " {\"library\":{\"type\":\"You\"},\"cause\":{\"type\":\"You\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CantSearchLibraries.codec
