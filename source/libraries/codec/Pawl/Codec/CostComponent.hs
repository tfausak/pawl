module Pawl.Codec.CostComponent where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CostComponent as CostComponent

-- | Tagged rather than bare-nullary from the start: this family grows
-- payload-carrying constructors (PayLife, Sacrifice), so the decoder is written
-- against Common.asTagged and only gains arms.
--
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
toJson :: (keyword -> Value.Value) -> CostComponent.CostComponent keyword -> Value.Value
toJson encode c = case c of
  CostComponent.TapThis -> Common.nullary "TapThis"
  CostComponent.UntapThis -> Common.nullary "UntapThis"
  CostComponent.SacrificeThis -> Common.nullary "SacrificeThis"
  CostComponent.PayLife n -> Common.tagged "PayLife" . Just $ Common.encodeNatural n
  CostComponent.Sacrifice n c_ -> Common.tagged "Sacrifice" . Just . Common.array $ [Common.encodeNatural n, Filter.toJson encode c_]
  CostComponent.DiscardCards n -> Common.tagged "DiscardCards" . Just $ Common.encodeNatural n
  CostComponent.DiscardThis -> Common.nullary "DiscardThis"
  CostComponent.PayEnergy n -> Common.tagged "PayEnergy" . Just $ Common.encodeNatural n
  CostComponent.AddLoyaltyToThis n -> Common.tagged "AddLoyaltyToThis" . Just $ Common.encodeNatural n
  CostComponent.RemoveLoyaltyFromThis n -> Common.tagged "RemoveLoyaltyFromThis" . Just $ Common.encodeNatural n

fromJson :: (Value.Value -> Either Text.Text keyword) -> Value.Value -> Either Text.Text (CostComponent.CostComponent keyword)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("TapThis", _) -> Right CostComponent.TapThis
    ("UntapThis", _) -> Right CostComponent.UntapThis
    ("SacrificeThis", _) -> Right CostComponent.SacrificeThis
    ("PayLife", Just v) -> CostComponent.PayLife <$> Common.decodeNatural v
    ("Sacrifice", Just (Value.Array (Array.MkArray [n, c_]))) -> do
      count <- Common.decodeNatural n
      filter_ <- Filter.fromJson decode c_
      pure $ CostComponent.Sacrifice count filter_
    ("DiscardCards", Just v) -> CostComponent.DiscardCards <$> Common.decodeNatural v
    ("DiscardThis", _) -> Right CostComponent.DiscardThis
    ("PayEnergy", Just v) -> CostComponent.PayEnergy <$> Common.decodeNatural v
    ("AddLoyaltyToThis", Just v) -> CostComponent.AddLoyaltyToThis <$> Common.decodeNatural v
    ("RemoveLoyaltyFromThis", Just v) -> CostComponent.RemoveLoyaltyFromThis <$> Common.decodeNatural v
    _ -> Left . Text.pack $ "unknown CostComponent: " <> t
