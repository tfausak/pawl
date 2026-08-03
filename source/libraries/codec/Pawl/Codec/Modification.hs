module Pawl.Codec.Modification where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Modification as Modification

toJson :: Modification.Modification -> Value.Value
toJson m = case m of
  Modification.GainKeyword k -> Common.tagged "GainKeyword" . Just $ Keyword.toJson k
  Modification.LoseAllAbilities -> Common.nullary "LoseAllAbilities"
  Modification.SetBasePowerToughness p t -> Common.tagged "SetBasePowerToughness" . Just . Common.array $ [Quantity.toJson p, Quantity.toJson t]
  Modification.ModifyPowerToughness p t -> Common.tagged "ModifyPowerToughness" . Just . Common.array $ [Quantity.toJson p, Quantity.toJson t]
  Modification.SetLandSubtype s -> Common.tagged "SetLandSubtype" . Just $ Subtype.toJson s
  Modification.AddLandSubtype s -> Common.tagged "AddLandSubtype" . Just $ Subtype.toJson s
  Modification.SetCreatureSubtype s -> Common.tagged "SetCreatureSubtype" . Just $ Subtype.toJson s
  Modification.AddCardType c -> Common.tagged "AddCardType" . Just $ CardType.toJson c
  Modification.ChangeSubtypeWord a b -> Common.tagged "ChangeSubtypeWord" . Just . Common.array $ [Subtype.toJson a, Subtype.toJson b]
  Modification.SetController p -> Common.tagged "SetController" . Just $ PlayerId.toJson p
  Modification.SetControllerToSource -> Common.nullary "SetControllerToSource"
  Modification.SetColor cs -> Common.tagged "SetColor" . Just $ Common.encodeSet Color.toJson cs
  Modification.AddColor cs -> Common.tagged "AddColor" . Just $ Common.encodeSet Color.toJson cs
  Modification.SwitchPowerToughness -> Common.nullary "SwitchPowerToughness"

fromJson :: Value.Value -> Either Text.Text Modification.Modification
fromJson value = do
  (t, mv) <- Common.asTagged value
  let pair v = case v of
        Just (Value.Array (Array.MkArray [x, y])) -> Right (x, y)
        _ -> Left $ Text.pack "expected a two-element array"
  case t of
    "GainKeyword" -> Common.withValue mv (fmap Modification.GainKeyword . Keyword.fromJson)
    "LoseAllAbilities" -> Right Modification.LoseAllAbilities
    "SetBasePowerToughness" -> pair mv >>= \(x, y) -> Modification.SetBasePowerToughness <$> Quantity.fromJson x <*> Quantity.fromJson y
    "ModifyPowerToughness" -> pair mv >>= \(x, y) -> Modification.ModifyPowerToughness <$> Quantity.fromJson x <*> Quantity.fromJson y
    "SetLandSubtype" -> Common.withValue mv (fmap Modification.SetLandSubtype . Subtype.fromJson)
    "AddLandSubtype" -> Common.withValue mv (fmap Modification.AddLandSubtype . Subtype.fromJson)
    "SetCreatureSubtype" -> Common.withValue mv (fmap Modification.SetCreatureSubtype . Subtype.fromJson)
    "AddCardType" -> Common.withValue mv (fmap Modification.AddCardType . CardType.fromJson)
    "ChangeSubtypeWord" -> pair mv >>= \(x, y) -> Modification.ChangeSubtypeWord <$> Subtype.fromJson x <*> Subtype.fromJson y
    "SetController" -> Common.withValue mv (fmap Modification.SetController . PlayerId.fromJson)
    "SetControllerToSource" -> Right Modification.SetControllerToSource
    "SetColor" -> Common.withValue mv (fmap Modification.SetColor . Common.decodeSet Color.fromJson)
    "AddColor" -> Common.withValue mv (fmap Modification.AddColor . Common.decodeSet Color.fromJson)
    "SwitchPowerToughness" -> Right Modification.SwitchPowerToughness
    _ -> Left . Text.pack $ "unknown Modification: " <> t
