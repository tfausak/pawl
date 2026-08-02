module Pawl.Codec.DamageRewriteSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageRewrite as DamageRewrite
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageRewrite" $ do
  Spec.it s "PreventAll" $
    Common.assertJsonCodec s DamageRewrite.toJson DamageRewrite.fromJson DamageRewrite.PreventAll "{\"type\":\"PreventAll\"}"
  -- CR 614.1a: Galvanic Blast's "deals 4 damage instead".
  Spec.it s "SetAmount" $
    Common.assertJsonCodec s DamageRewrite.toJson DamageRewrite.fromJson (DamageRewrite.SetAmount 4) "{\"type\":\"SetAmount\",\"value\":4}"
  -- Furnace of Rath's "double that damage", which is Scaling's Multiply 2 -- the
  -- same value Corpsejack Menace doubles counters with.
  Spec.it s "Scale" $
    Common.assertJsonCodec s DamageRewrite.toJson DamageRewrite.fromJson (DamageRewrite.Scale (Scaling.Multiply 2)) "{\"type\":\"Scale\",\"value\":{\"type\":\"Multiply\",\"value\":2}}"
