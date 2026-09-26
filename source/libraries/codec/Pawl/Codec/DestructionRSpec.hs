module Pawl.Codec.DestructionRSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.DestructionR as DestructionR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DestructionR as DestructionR
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DestructionR" $ do
  -- CR 701.19a, "regenerate this creature": the rule names the subject.
  Spec.it s "MkDestructionR (no subject)" $
    Common.assertCodec
      s
      DestructionR.codec
      (DestructionR.MkDestructionR {DestructionR.matching = Nothing, DestructionR.rewrite = DestructionRewrite.Regenerate})
      " {\"rewrite\":{\"type\":\"Regenerate\"}} "
  -- CR 614.1a, Pyramids' "the next time target land would be destroyed".
  Spec.it s "MkDestructionR (a printed subject)" $
    Common.assertCodec
      s
      DestructionR.codec
      (DestructionR.MkDestructionR {DestructionR.matching = Just (Filter.IsBound (SlotName.MkSlotName (Text.pack "land"))), DestructionR.rewrite = DestructionRewrite.Heal})
      " {\"matching\":{\"type\":\"IsBound\",\"value\":\"land\"},\"rewrite\":{\"type\":\"Heal\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DestructionR.codec
