module Pawl.Codec.AttackCostSpec where

import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  -- Ghostly Prison's shape: the subject is every creature (Affected.Matching over
  -- a bare card type, since only a creature can be declared as an attacker), and
  -- the cost is ONE attacker's share -- Ghostly Prison's "for each" repeats it per
  -- taxed attacker, and CR 508.1h totals what that comes to.
  Spec.describe s "Pawl.Codec.AttackCost" . Spec.it s "MkAttackCost" $
    Common.assertJsonCodec
      s
      AttackCost.toJson
      AttackCost.fromJson
      ( AttackCost.MkAttackCost
          (Affected.Matching (Filter.HasCardType CardType.Creature))
          (ManaCost.MkManaCost [ManaSymbol.Generic 2])
      )
      "{\"perAttacker\":[{\"type\":\"Generic\",\"value\":2}],\"subject\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}}"
