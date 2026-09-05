module Pawl.Codec.AffectedUnlessSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AffectedUnless as AffectedUnless
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AffectedUnless" $ do
  -- CR 508.1c's gate absent, which is most printed restrictions: the key is
  -- omitted rather than written null.
  Spec.it s "MkAffectedUnless, unless elided" $
    Common.assertCodec
      s
      AffectedUnless.codec
      ( AffectedUnless.MkAffectedUnless
          { AffectedUnless.affected = Affected.Matching (Filter.HasCardType CardType.Creature),
            AffectedUnless.unless = Nothing,
            AffectedUnless.name = Nothing
          }
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  Spec.it s "MkAffectedUnless, unless written" $
    Common.assertCodec
      s
      AffectedUnless.codec
      ( AffectedUnless.MkAffectedUnless
          { AffectedUnless.affected = Affected.Attached,
            AffectedUnless.unless =
              Just $
                Condition.Compares
                  (Compares.MkCompares (Quantity.Literal 1) Comparison.AtLeast (Quantity.Literal 1)),
            AffectedUnless.name = Nothing
          }
      )
      " {\"affected\":{\"type\":\"Attached\"},\"unless\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":1},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":1}}}} "
  -- CR 116.2d: the name a face gives the ability stating the restriction, which
  -- only a face granting the ignore writes (Volrath's Curse).
  Spec.it s "MkAffectedUnless, name written" $
    Common.assertCodec
      s
      AffectedUnless.codec
      ( AffectedUnless.MkAffectedUnless
          { AffectedUnless.affected = Affected.Attached,
            AffectedUnless.unless = Nothing,
            AffectedUnless.name = Just (AbilityName.MkAbilityName (Text.pack "this effect"))
          }
      )
      " {\"affected\":{\"type\":\"Attached\"},\"name\":\"this effect\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AffectedUnless.codec
