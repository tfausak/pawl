-- | The @Modification ⇆ Json@ codec (#481).
module Pawl.Codec.Modification where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.Subtype as Subtype
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Modification as Modification

modificationToJson :: Modification.Modification -> Value
modificationToJson m = case m of
  Modification.GainKeyword k -> Json.tagged (Text.pack "GainKeyword") (Just (Keyword.toJson k))
  Modification.LoseAllAbilities -> Json.nullary (Text.pack "LoseAllAbilities")
  Modification.SetBasePowerToughness p t -> Json.tagged (Text.pack "SetBasePowerToughness") (Just (Array (MkArray [Quantity.toJson p, Quantity.toJson t])))
  Modification.ModifyPowerToughness p t -> Json.tagged (Text.pack "ModifyPowerToughness") (Just (Array (MkArray [Quantity.toJson p, Quantity.toJson t])))
  Modification.SetLandSubtype s -> Json.tagged (Text.pack "SetLandSubtype") (Just (Subtype.toJson s))
  Modification.AddLandSubtype s -> Json.tagged (Text.pack "AddLandSubtype") (Just (Subtype.toJson s))
  Modification.AddCardType c -> Json.tagged (Text.pack "AddCardType") (Just (CardType.toJson c))
  Modification.ChangeSubtypeWord a b -> Json.tagged (Text.pack "ChangeSubtypeWord") (Just (Array (MkArray [Subtype.toJson a, Subtype.toJson b])))
  Modification.SetController p -> Json.tagged (Text.pack "SetController") (Just (PlayerId.toJson p))
  Modification.SetControllerToSource -> Json.nullary (Text.pack "SetControllerToSource")
  Modification.SetColor cs -> Json.tagged (Text.pack "SetColor") (Just (Json.setTo Color.toJson cs))
  Modification.SwitchPowerToughness -> Json.nullary (Text.pack "SwitchPowerToughness")

jsonToModification :: Value -> Either Text Modification.Modification
jsonToModification value = do
  (t, mv) <- Json.tag value
  let pair v = case v of
        Just (Array (MkArray [x, y])) -> Right (x, y)
        _ -> Left (Text.pack "expected a two-element array")
  case Text.unpack t of
    "GainKeyword" -> Json.withValue mv (fmap Modification.GainKeyword . Keyword.fromJson)
    "LoseAllAbilities" -> Right Modification.LoseAllAbilities
    "SetBasePowerToughness" -> pair mv >>= \(x, y) -> Modification.SetBasePowerToughness <$> Quantity.fromJson x <*> Quantity.fromJson y
    "ModifyPowerToughness" -> pair mv >>= \(x, y) -> Modification.ModifyPowerToughness <$> Quantity.fromJson x <*> Quantity.fromJson y
    "SetLandSubtype" -> Json.withValue mv (fmap Modification.SetLandSubtype . Subtype.fromJson)
    "AddLandSubtype" -> Json.withValue mv (fmap Modification.AddLandSubtype . Subtype.fromJson)
    "AddCardType" -> Json.withValue mv (fmap Modification.AddCardType . CardType.fromJson)
    "ChangeSubtypeWord" -> pair mv >>= \(x, y) -> Modification.ChangeSubtypeWord <$> Subtype.fromJson x <*> Subtype.fromJson y
    "SetController" -> Json.withValue mv (fmap Modification.SetController . PlayerId.fromJson)
    "SetControllerToSource" -> Right Modification.SetControllerToSource
    "SetColor" -> Json.withValue mv (fmap Modification.SetColor . Json.setFrom Color.fromJson)
    "SwitchPowerToughness" -> Right Modification.SwitchPowerToughness
    _ -> Left (Text.pack "unknown Modification: " <> t)
