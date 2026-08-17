module Pawl.Codec.DamageRSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.DamageR as DamageR
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageR" $ do
  -- CR 615.1: Fog prevents all combat damage.
  Spec.it s "MkDamageR" $
    Common.assertCodec
      s
      (DamageR.codec (Effect.codec Card.codec))
      ( DamageR.MkDamageR
          { DamageR.matching =
              DamagePattern.MkDamagePattern
                { DamagePattern.whichKind = Just DamageKind.Combat,
                  DamagePattern.whatSource = Filter.And [],
                  DamagePattern.whatRecipient = Nothing,
                  DamagePattern.whichRecipient = Nothing
                },
            DamageR.rewrite = DamageRewrite.PreventAll,
            DamageR.riders = Seq.empty
          }
      )
      " {\"matching\":{\"whichKind\":{\"type\":\"Combat\"}},\"rewrite\":{\"type\":\"PreventAll\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (DamageR.codec (Effect.codec Card.codec))
