module Pawl.Codec.HandActionSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.HandAction as HandAction
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.HandAction as HandAction
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

-- | The `card` parameter is instantiated at 'Text.Text' throughout, for
-- 'Pawl.Codec.ClauseSpec''s reason: the codec reaches it only through the
-- supplied Effect codec, so any type proves the shape.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

codec :: Codec.Codec (HandAction.HandAction Text.Text)
codec = HandAction.codec cardCodec

toJson :: HandAction.HandAction Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (HandAction.HandAction Text.Text)
fromJson = Codec.decode codec

-- One constructor, so three cases: an ungated action (every card in the corpus
-- but one), CR 103.6's gate when present, and both fields defaulted away.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.HandAction" $ do
  Spec.it s "MkHandAction with one effect and no gate" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (HandAction.MkHandAction Nothing [Effect.ExileHandThenDraw])
      " {\"effects\":[{\"type\":\"ExileHandThenDraw\"}]} "
  -- CR 103.1: Gemstone Caverns' "you're not the starting player", the whole
  -- reason this type is a record rather than a list of effects.
  Spec.it s "a gated action keeps its condition" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( HandAction.MkHandAction
          (Just (Condition.Compares (Compares.MkCompares (Quantity.IsStartingPlayer (PlayerRef.Relative PlayerRelation.You)) Comparison.Exactly (Quantity.Literal 0))))
          []
      )
      " {\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"IsStartingPlayer\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":0}}}} "
  Spec.it s "both keys default away" $ do
    v <- Common.assertJson s " {} "
    Spec.assertEq s (fromJson v) (Right (HandAction.MkHandAction Nothing []))
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
