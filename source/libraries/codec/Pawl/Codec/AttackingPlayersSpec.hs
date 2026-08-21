module Pawl.Codec.AttackingPlayersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AttackingPlayers as AttackingPlayers
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackingPlayers" $ do
  Spec.it s "MkAttackingPlayers, every key" $
    Common.assertCodec
      s
      AttackingPlayers.codec
      ( AttackingPlayers.MkAttackingPlayers
          { AttackingPlayers.relation = PlayerRelation.Opponent,
            AttackingPlayers.attacked = SlotName.MkSlotName (Text.pack "attackedPlayer")
          }
      )
      " {\"relation\":{\"type\":\"Opponent\"},\"attacked\":\"attackedPlayer\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackingPlayers.codec
