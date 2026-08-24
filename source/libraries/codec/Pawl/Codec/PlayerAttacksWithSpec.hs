module Pawl.Codec.PlayerAttacksWithSpec where

import qualified Pawl.Codec.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerAttacksWith" $ do
  -- Hermes, Overseer of Elpis' payload: whose declaration fires it, and the
  -- quality one of the declared creatures has to have.
  Spec.it s "MkPlayerAttacksWith, both keys" $
    Common.assertCodec
      s
      PlayerAttacksWith.codec
      ( PlayerAttacksWith.MkPlayerAttacksWith
          { PlayerAttacksWith.player = PlayerRelation.You,
            PlayerAttacksWith.filter = Filter.HasSubtype Subtype.Bird
          }
      )
      " {\"player\":{\"type\":\"You\"},\"filter\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Bird\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerAttacksWith.codec
