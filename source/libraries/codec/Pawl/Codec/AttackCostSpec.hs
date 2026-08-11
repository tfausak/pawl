{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AttackCostSpec where

import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  -- The subject is every creature, since only a creature can be declared as an
  -- attacker, and the cost is ONE attacker's share (CR 508.1h totals them).
  Spec.describe s "Pawl.Codec.AttackCost" . Spec.it s "MkAttackCost" $
    Common.assertJsonCodec
      s
      AttackCost.toJson
      AttackCost.fromJson
      ( AttackCost.MkAttackCost
          (Affected.Matching (Filter.HasCardType CardType.Creature))
          (ManaCost.MkManaCost [ManaSymbol.Generic 2])
      )
      """ {"perAttacker":[{"type":"Generic","value":2}],"subject":{"type":"Matching","value":{"type":"HasCardType","value":{"type":"Creature"}}}} """
