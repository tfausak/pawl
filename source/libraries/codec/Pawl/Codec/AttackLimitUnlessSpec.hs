module Pawl.Codec.AttackLimitUnlessSpec where

import qualified Pawl.Codec.AttackLimitUnless as AttackLimitUnless
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackLimitUnless as AttackLimitUnless
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackLimitUnless" $ do
  -- "limit" and not "affected": a bound names no creature, and the key set is
  -- what tells a reader of the card file which payload shape this is.
  Spec.it s "MkAttackLimitUnless, defenders and unless elided" $
    Common.assertCodec
      s
      AttackLimitUnless.codec
      (AttackLimitUnless.MkAttackLimitUnless {AttackLimitUnless.limit = 1, AttackLimitUnless.defenders = Nothing, AttackLimitUnless.unless = Nothing})
      " {\"limit\":1} "
  -- CR 802.3a: the scoped bound, which is what Crawlspace writes.
  Spec.it s "MkAttackLimitUnless, defenders written" $
    Common.assertCodec
      s
      AttackLimitUnless.codec
      (AttackLimitUnless.MkAttackLimitUnless {AttackLimitUnless.limit = 2, AttackLimitUnless.defenders = Just PlayerScope.You, AttackLimitUnless.unless = Nothing})
      " {\"limit\":2,\"defenders\":{\"type\":\"You\"}} "
  Spec.it s "MkAttackLimitUnless, unless written" $
    Common.assertCodec
      s
      AttackLimitUnless.codec
      ( AttackLimitUnless.MkAttackLimitUnless
          { AttackLimitUnless.limit = 3,
            AttackLimitUnless.defenders = Nothing,
            AttackLimitUnless.unless =
              Just $
                Condition.Compares
                  (Compares.MkCompares (Quantity.Literal 1) Comparison.AtLeast (Quantity.Literal 1))
          }
      )
      " {\"limit\":3,\"unless\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":1},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":1}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackLimitUnless.codec
