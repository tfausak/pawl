{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ClauseSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Clause as Clause
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.SlotName as SlotName

-- | The `card` parameter is instantiated at 'Text.Text' throughout, for
-- 'Pawl.Codec.ModeSpec''s reason: the codec reaches it only through the supplied
-- Effect codec, so any type proves the shape.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Clause.Clause Text.Text -> Value.Value
toJson = Clause.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Clause.Clause Text.Text)
fromJson = Clause.fromJson cardFromJson

-- One constructor and one field, so two cases: a populated clause, and the
-- empty one whose only key is omitted.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Clause" $ do
  Spec.it s "MkClause with one effect" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target")))))
      """ {"effects":[{"type":"Attach","value":"target"}]} """
  Spec.it s "an empty clause omits its only key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Seq.empty)
      """ {} """
