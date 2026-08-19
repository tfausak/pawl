module Pawl.Codec.ArmDelayedTriggerSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Onset as Onset

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ArmDelayedTrigger" $ do
  -- CR 603.7a/603.7b: both halves of the envelope at their default, which is
  -- Tidal Wave's shape and by far the common one.
  Spec.it s "MkArmDelayedTrigger, both defaults elided" $
    Common.assertCodec
      s
      ArmDelayedTrigger.codec
      ( ArmDelayedTrigger.MkArmDelayedTrigger
          { ArmDelayedTrigger.name = AbilityName.MkAbilityName (Text.pack "sacrifice it"),
            ArmDelayedTrigger.onset = Onset.Immediately,
            ArmDelayedTrigger.duration = Nothing
          }
      )
      " {\"name\":\"sacrifice it\"} "
  -- CR 603.7b's stated duration alone -- Full Throttle's "this turn".
  Spec.it s "MkArmDelayedTrigger, duration alone" $
    Common.assertCodec
      s
      ArmDelayedTrigger.codec
      ( ArmDelayedTrigger.MkArmDelayedTrigger
          { ArmDelayedTrigger.name = AbilityName.MkAbilityName (Text.pack "each combat"),
            ArmDelayedTrigger.onset = Onset.Immediately,
            ArmDelayedTrigger.duration = Just Duration.UntilEndOfTurn
          }
      )
      " {\"name\":\"each combat\",\"duration\":{\"type\":\"UntilEndOfTurn\"}} "
  -- Meandering Towershell's "on your next turn": the onset alone. Under the
  -- positional payload this was the three-element form, and only its LENGTH
  -- separated it from the case above -- an onset and a duration are both
  -- tagged objects.
  Spec.it s "MkArmDelayedTrigger, onset alone" $
    Common.assertCodec
      s
      ArmDelayedTrigger.codec
      ( ArmDelayedTrigger.MkArmDelayedTrigger
          { ArmDelayedTrigger.name = AbilityName.MkAbilityName (Text.pack "return it"),
            ArmDelayedTrigger.onset = Onset.FromYourNextTurn,
            ArmDelayedTrigger.duration = Nothing
          }
      )
      " {\"name\":\"return it\",\"onset\":{\"type\":\"FromYourNextTurn\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ArmDelayedTrigger.codec
