module Pawl.Codec.CreatureBecomesBlockedByAtLeastSpec where

import qualified Pawl.Codec.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CreatureBecomesBlockedByAtLeast" $ do
  -- Seifer, Balamb Rival's payload: whom the attacker is attacking, and the
  -- floor on how many creatures block it.
  Spec.it s "MkCreatureBecomesBlockedByAtLeast, both keys" $
    Common.assertCodec
      s
      CreatureBecomesBlockedByAtLeast.codec
      ( CreatureBecomesBlockedByAtLeast.MkCreatureBecomesBlockedByAtLeast
          { CreatureBecomesBlockedByAtLeast.attacked = PlayerRelation.Opponent,
            CreatureBecomesBlockedByAtLeast.blockers = 2
          }
      )
      " {\"attacked\":{\"type\":\"Opponent\"},\"blockers\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CreatureBecomesBlockedByAtLeast.codec
