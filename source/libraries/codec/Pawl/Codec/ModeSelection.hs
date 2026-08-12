module Pawl.Codec.ModeSelection where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ModeSelection as ModeSelection

toJson :: ModeSelection.ModeSelection -> Value.Value
toJson m = case m of
  ModeSelection.ChooseExactly n -> Common.tagged "ChooseExactly" . Just $ Common.encodeNatural n
  ModeSelection.ChooseExactlyWithRepeats n -> Common.tagged "ChooseExactlyWithRepeats" . Just $ Common.encodeNatural n
  ModeSelection.ChooseBetween least most ->
    Common.tagged "ChooseBetween" . Just $
      Value.object
        [ Value.pair "least" (Common.encodeNatural least),
          Value.pair "most" (Common.encodeNatural most)
        ]

fromJson :: Value.Value -> Either Text.Text ModeSelection.ModeSelection
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("ChooseExactly", Just n) -> ModeSelection.ChooseExactly <$> Common.decodeNatural n
    -- CR 700.2d: "You may choose the same mode more than once." A separate tag
    -- rather than a field on the one above, so a card printing the ordinary
    -- instruction encodes exactly as it always did.
    ("ChooseExactlyWithRepeats", Just n) -> ModeSelection.ChooseExactlyWithRepeats <$> Common.decodeNatural n
    -- CR 700.2's "Choose one or both --": a range, named field by field for
    -- Pawl.Codec.TargetCount's reason -- two bare numbers in a list would let a
    -- card swap the bound it meant. The one invariant Pawl.Types.ModeSelection
    -- states is enforced here, this being where a range enters the engine.
    ("ChooseBetween", Just v) -> do
      ps <- Common.asObject v
      least <- Common.field "least" ps >>= Common.decodeNatural
      most <- Common.field "most" ps >>= Common.decodeNatural
      if least > most
        then Left (Text.pack "ModeSelection: least must not exceed most")
        else pure (ModeSelection.ChooseBetween least most)
    _ -> Left . Text.pack $ "unknown ModeSelection: " <> t
