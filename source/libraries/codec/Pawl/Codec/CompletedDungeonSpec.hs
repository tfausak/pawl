module Pawl.Codec.CompletedDungeonSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CompletedDungeon as CompletedDungeon
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CompletedDungeon as CompletedDungeon
import qualified Pawl.Types.PlayerRef as PlayerRef

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CompletedDungeon" $ do
  -- CR 309.7 asked of one dungeon by name -- a LEAF, so nothing here is a
  -- Quantity.
  Spec.it s "MkCompletedDungeon" $
    Common.assertCodec
      s
      CompletedDungeon.codec
      ( CompletedDungeon.MkCompletedDungeon
          { CompletedDungeon.player = PlayerRef.EachPlayer,
            CompletedDungeon.dungeon = CardName.MkCardName (Text.pack "Tomb of Annihilation")
          }
      )
      " {\"player\":{\"type\":\"EachPlayer\"},\"dungeon\":\"Tomb of Annihilation\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CompletedDungeon.codec
