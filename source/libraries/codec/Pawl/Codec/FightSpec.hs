module Pawl.Codec.FightSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Fight as Fight
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Fight" $ do
  -- Prey Upon's two slots, in the order the object keys sort rather than the
  -- order the card's sentence names them.
  Spec.it s "MkFight, both slots" $
    Common.assertCodec
      s
      Fight.codec
      (Fight.MkFight (SlotName.MkSlotName (Text.pack "mine")) (SlotName.MkSlotName (Text.pack "theirs")))
      " {\"first\":\"mine\",\"second\":\"theirs\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Fight.codec
