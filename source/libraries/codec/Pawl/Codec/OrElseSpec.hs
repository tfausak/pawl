module Pawl.Codec.OrElseSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.OrElse as OrElse
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.OrElse as OrElse
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

toJson :: OrElse.OrElse -> Value.Value
toJson = Codec.encode OrElse.codec

fromJson :: Value.Value -> Either Text.Text OrElse.OrElse
fromJson = Codec.decode OrElse.codec

-- One constructor, so three cases: the unmarked chooser, which is elided, a card
-- that names somebody else, and CR 701.55a's villainous flag, elided the same
-- way.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OrElse" $ do
  Spec.it s "a branch announced by the resolving controller writes only its sibling" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (ClauseIndex.MkClauseIndex 1) (PlayerRef.Relative PlayerRelation.You) False)
      " {\"sibling\":1} "
  Spec.it s "and one the whole table announces writes the chooser" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (ClauseIndex.MkClauseIndex 0) PlayerRef.EachPlayer False)
      " {\"sibling\":0,\"chooser\":{\"type\":\"EachPlayer\"}} "
  Spec.it s "CR 701.55a a villainous choice writes the flag" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (ClauseIndex.MkClauseIndex 1) (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) True)
      " {\"sibling\":1,\"chooser\":{\"type\":\"InSlot\",\"value\":\"target\"},\"villainous\":true} "
