module Pawl.Codec.CantSearchLibrariesSpec where

import qualified Pawl.Codec.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.Types.PlayerRelation as PlayerRelation
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
        { CantSearchLibraries.library = PlayerScope.Related PlayerRelation.AnyPlayer,
          CantSearchLibraries.cause = PlayerScope.Related PlayerRelation.AnyPlayer
        }
      " {\"library\":{\"type\":\"AnyPlayer\"},\"cause\":{\"type\":\"AnyPlayer\"}} "
  -- Ashiok, Dream Render: both axes narrowed to the prohibited player.
  Spec.it s "MkCantSearchLibraries narrowed on both axes" $
    Common.assertCodec
      s
      CantSearchLibraries.codec
      CantSearchLibraries.MkCantSearchLibraries
        { CantSearchLibraries.library = PlayerScope.Related PlayerRelation.You,
          CantSearchLibraries.cause = PlayerScope.Related PlayerRelation.You
        }
      " {\"library\":{\"type\":\"You\"},\"cause\":{\"type\":\"You\"}} "
  -- The two fields hold the same type and the cases above give them the same
  -- value, so neither tells a swapped pair apart. This one does: no printing
  -- narrows the axes differently, and the case exists to pin which key is which.
  Spec.it s "MkCantSearchLibraries narrowed differently on each axis" $
    Common.assertCodec
      s
      CantSearchLibraries.codec
      CantSearchLibraries.MkCantSearchLibraries
        { CantSearchLibraries.library = PlayerScope.Related PlayerRelation.You,
          CantSearchLibraries.cause = PlayerScope.Related PlayerRelation.Opponent
        }
      " {\"library\":{\"type\":\"You\"},\"cause\":{\"type\":\"Opponent\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CantSearchLibraries.codec
