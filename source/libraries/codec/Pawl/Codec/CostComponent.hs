-- | The @CostComponent ⇆ Json@ codec (#481).
module Pawl.Codec.CostComponent where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.CostComponent as CostComponent

-- Tagged rather than bare-nullary from the start: this family grows
-- payload-carrying constructors (PayLife, Sacrifice), so the decoder is written
-- against Json.tag and only gains arms.
costComponentToJson :: CostComponent.CostComponent -> Value
costComponentToJson c = case c of
  CostComponent.TapThis -> Json.nullary (Text.pack "TapThis")
  CostComponent.UntapThis -> Json.nullary (Text.pack "UntapThis")
  CostComponent.SacrificeThis -> Json.nullary (Text.pack "SacrificeThis")
  CostComponent.PayLife n -> Json.tagged (Text.pack "PayLife") (Just (Json.natTo n))
  CostComponent.Sacrifice n c_ -> Json.tagged (Text.pack "Sacrifice") (Just (Array (MkArray [Json.natTo n, Filter.toJson c_])))
  CostComponent.DiscardCards n -> Json.tagged (Text.pack "DiscardCards") (Just (Json.natTo n))
  CostComponent.DiscardThis -> Json.nullary (Text.pack "DiscardThis")
  CostComponent.PayEnergy n -> Json.tagged (Text.pack "PayEnergy") (Just (Json.natTo n))
  CostComponent.AddLoyaltyToThis n -> Json.tagged (Text.pack "AddLoyaltyToThis") (Just (Json.natTo n))
  CostComponent.RemoveLoyaltyFromThis n -> Json.tagged (Text.pack "RemoveLoyaltyFromThis") (Just (Json.natTo n))

jsonToCostComponent :: Value -> Either Text CostComponent.CostComponent
jsonToCostComponent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("TapThis", _) -> Right CostComponent.TapThis
    ("UntapThis", _) -> Right CostComponent.UntapThis
    ("SacrificeThis", _) -> Right CostComponent.SacrificeThis
    ("PayLife", Just v) -> fmap CostComponent.PayLife (Json.natFrom v)
    ("Sacrifice", Just (Array (MkArray [n, c_]))) -> do
      count <- Json.natFrom n
      filter_ <- Filter.fromJson c_
      pure (CostComponent.Sacrifice count filter_)
    ("DiscardCards", Just v) -> fmap CostComponent.DiscardCards (Json.natFrom v)
    ("DiscardThis", _) -> Right CostComponent.DiscardThis
    ("PayEnergy", Just v) -> fmap CostComponent.PayEnergy (Json.natFrom v)
    ("AddLoyaltyToThis", Just v) -> fmap CostComponent.AddLoyaltyToThis (Json.natFrom v)
    ("RemoveLoyaltyFromThis", Just v) -> fmap CostComponent.RemoveLoyaltyFromThis (Json.natFrom v)
    _ -> Left (Text.pack "unknown CostComponent: " <> t)
