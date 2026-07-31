-- | The @Modification ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Modification where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.CardType (cardTypeToJson, jsonToCardType)
import Pawl.Codec.Color (colorToJson, jsonToColor)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Codec.Quantity (jsonToQuantity, quantityToJson)
import Pawl.Codec.Subtype (jsonToSubtype, subtypeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Modification as Modification

modificationToJson :: Modification.Modification -> Value
modificationToJson m = case m of
  Modification.GainKeyword k -> Json.tagged (Text.pack "GainKeyword") (Just (keywordToJson k))
  Modification.LoseAllAbilities -> Json.nullary (Text.pack "LoseAllAbilities")
  Modification.SetBasePowerToughness p t -> Json.tagged (Text.pack "SetBasePowerToughness") (Just (Array (MkArray [quantityToJson p, quantityToJson t])))
  Modification.ModifyPowerToughness p t -> Json.tagged (Text.pack "ModifyPowerToughness") (Just (Array (MkArray [quantityToJson p, quantityToJson t])))
  Modification.SetLandSubtype s -> Json.tagged (Text.pack "SetLandSubtype") (Just (subtypeToJson s))
  Modification.AddLandSubtype s -> Json.tagged (Text.pack "AddLandSubtype") (Just (subtypeToJson s))
  Modification.AddCardType c -> Json.tagged (Text.pack "AddCardType") (Just (cardTypeToJson c))
  Modification.ChangeSubtypeWord a b -> Json.tagged (Text.pack "ChangeSubtypeWord") (Just (Array (MkArray [subtypeToJson a, subtypeToJson b])))
  Modification.SetController p -> Json.tagged (Text.pack "SetController") (Just (playerIdToJson p))
  Modification.SetControllerToSource -> Json.nullary (Text.pack "SetControllerToSource")
  Modification.SetColor cs -> Json.tagged (Text.pack "SetColor") (Just (Json.setTo colorToJson cs))
  Modification.SwitchPowerToughness -> Json.nullary (Text.pack "SwitchPowerToughness")

jsonToModification :: Value -> Either Text Modification.Modification
jsonToModification value = do
  (t, mv) <- Json.tag value
  let pair v = case v of
        Just (Array (MkArray [x, y])) -> Right (x, y)
        _ -> Left (Text.pack "expected a two-element array")
  case Text.unpack t of
    "GainKeyword" -> Json.withValue mv (fmap Modification.GainKeyword . jsonToKeyword)
    "LoseAllAbilities" -> Right Modification.LoseAllAbilities
    "SetBasePowerToughness" -> pair mv >>= \(x, y) -> Modification.SetBasePowerToughness <$> jsonToQuantity x <*> jsonToQuantity y
    "ModifyPowerToughness" -> pair mv >>= \(x, y) -> Modification.ModifyPowerToughness <$> jsonToQuantity x <*> jsonToQuantity y
    "SetLandSubtype" -> Json.withValue mv (fmap Modification.SetLandSubtype . jsonToSubtype)
    "AddLandSubtype" -> Json.withValue mv (fmap Modification.AddLandSubtype . jsonToSubtype)
    "AddCardType" -> Json.withValue mv (fmap Modification.AddCardType . jsonToCardType)
    "ChangeSubtypeWord" -> pair mv >>= \(x, y) -> Modification.ChangeSubtypeWord <$> jsonToSubtype x <*> jsonToSubtype y
    "SetController" -> Json.withValue mv (fmap Modification.SetController . jsonToPlayerId)
    "SetControllerToSource" -> Right Modification.SetControllerToSource
    "SetColor" -> Json.withValue mv (fmap Modification.SetColor . Json.setFrom jsonToColor)
    "SwitchPowerToughness" -> Right Modification.SwitchPowerToughness
    _ -> Left (Text.pack "unknown Modification: " <> t)
