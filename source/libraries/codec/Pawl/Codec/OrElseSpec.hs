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

toJson :: OrElse.OrElse -> Value.Value
toJson = Codec.encode OrElse.codec

fromJson :: Value.Value -> Either Text.Text OrElse.OrElse
fromJson = Codec.decode OrElse.codec

-- One constructor, so two cases: the unmarked chooser, which is elided, and a
-- card that names somebody else.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OrElse" $ do
  Spec.it s "a branch announced by the resolving controller writes only its sibling" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (ClauseIndex.MkClauseIndex 1) (PlayerRef.Relative PlayerRelation.You))
      " {\"sibling\":1} "
  Spec.it s "and one the whole table announces writes the chooser" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (OrElse.MkOrElse (ClauseIndex.MkClauseIndex 0) PlayerRef.EachPlayer)
      " {\"sibling\":0,\"chooser\":{\"type\":\"EachPlayer\"}} "
