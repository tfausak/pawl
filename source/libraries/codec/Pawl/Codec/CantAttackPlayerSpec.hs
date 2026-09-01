module Pawl.Codec.CantAttackPlayerSpec where

import qualified Pawl.Codec.CantAttackPlayer as CantAttackPlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CantAttackPlayer as CantAttackPlayer
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CantAttackPlayer" $ do
  -- CR 508.1c's attacking pairwise restriction, Blazing Archon's payload. The
  -- scope is You rather than EachPlayer, which is the value that would read the
  -- same as a blanket "can't attack" on any board.
  Spec.it s "MkCantAttackPlayer, unless elided" $
    Common.assertCodec
      s
      CantAttackPlayer.codec
      ( CantAttackPlayer.MkCantAttackPlayer
          { CantAttackPlayer.affected = Affected.Matching (Filter.HasCardType CardType.Creature),
            CantAttackPlayer.defenders = PlayerScope.You,
            CantAttackPlayer.unless = Nothing
          }
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"defenders\":{\"type\":\"You\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CantAttackPlayer.codec
