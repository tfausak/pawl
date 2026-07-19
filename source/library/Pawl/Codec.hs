-- | The sole authority for @Card ⇆ Json@ (§2 of the M3.5 spec), mirroring
-- 'Pawl.Resolve' (the sole @case@-on-@Effect@ home). Free @xToJson@\/@jsonToX@
-- functions -- no type classes -- over the transitive closure of @Card@'s
-- fields. Every @Pawl.Type.*@ module stays JSON-free; casing on an effect's
-- identity here is open-half machinery, not the rules core.
module Pawl.Codec where

import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Json as Json
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AdditionalCost as AdditionalCost
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.CardCriterion as CardCriterion
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CastingPermission as CastingPermission
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import Pawl.Type.Json (Value (Array, Null, Object))
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone

-- Helpers --------------------------------------------------------------------

nullary :: Text -> Value
nullary t = Json.tagged t Nothing

decodeNullary :: Text -> [(Text, a)] -> Value -> Either Text a
decodeNullary tyName table value = do
  (t, _) <- Json.tag value
  case lookup t table of
    Just x -> Right x
    Nothing -> Left (Text.pack "unknown " <> tyName <> Text.pack ": " <> t)

listTo :: (a -> Value) -> [a] -> Value
listTo f = Array . map f

listFrom :: (Value -> Either Text a) -> Value -> Either Text [a]
listFrom f value = Json.asArray value >>= mapM f

setTo :: (a -> Value) -> Set a -> Value
setTo f = listTo f . Set.toAscList

setFrom :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Set a)
setFrom f value = Set.fromList <$> listFrom f value

maybeTo :: (a -> Value) -> Maybe a -> Value
maybeTo = maybe Null

maybeFrom :: (Value -> Either Text a) -> Value -> Either Text (Maybe a)
maybeFrom f value = case value of
  Null -> Right Nothing
  _ -> Just <$> f value

natTo :: Natural -> Value
natTo = Json.jInt . toInteger

natFrom :: Value -> Either Text Natural
natFrom value = do
  n <- Json.asInteger value
  if n >= 0 then Right (fromInteger n) else Left (Text.pack "expected natural")

-- Leaf enums -----------------------------------------------------------------

colorToJson :: Color.Color -> Value
colorToJson c = nullary . Text.pack $ case c of
  Color.White -> "White"
  Color.Blue -> "Blue"
  Color.Black -> "Black"
  Color.Red -> "Red"
  Color.Green -> "Green"

jsonToColor :: Value -> Either Text Color.Color
jsonToColor =
  decodeNullary
    (Text.pack "Color")
    [ (Text.pack "White", Color.White),
      (Text.pack "Blue", Color.Blue),
      (Text.pack "Black", Color.Black),
      (Text.pack "Red", Color.Red),
      (Text.pack "Green", Color.Green)
    ]

cardTypeToJson :: CardType.CardType -> Value
cardTypeToJson c = nullary . Text.pack $ case c of
  CardType.Land -> "Land"
  CardType.Creature -> "Creature"
  CardType.Instant -> "Instant"
  CardType.Enchantment -> "Enchantment"
  CardType.Artifact -> "Artifact"
  CardType.Sorcery -> "Sorcery"

jsonToCardType :: Value -> Either Text CardType.CardType
jsonToCardType =
  decodeNullary
    (Text.pack "CardType")
    [ (Text.pack "Land", CardType.Land),
      (Text.pack "Creature", CardType.Creature),
      (Text.pack "Instant", CardType.Instant),
      (Text.pack "Enchantment", CardType.Enchantment),
      (Text.pack "Artifact", CardType.Artifact),
      (Text.pack "Sorcery", CardType.Sorcery)
    ]

subtypeToJson :: Subtype.Subtype -> Value
subtypeToJson s = nullary . Text.pack $ case s of
  Subtype.Mountain -> "Mountain"
  Subtype.Swamp -> "Swamp"
  Subtype.Forest -> "Forest"
  Subtype.Island -> "Island"
  Subtype.Plains -> "Plains"
  Subtype.Goblin -> "Goblin"
  Subtype.Warrior -> "Warrior"
  Subtype.Human -> "Human"
  Subtype.Bird -> "Bird"
  Subtype.Ogre -> "Ogre"
  Subtype.Centaur -> "Centaur"
  Subtype.Cat -> "Cat"
  Subtype.Dinosaur -> "Dinosaur"
  Subtype.Beast -> "Beast"
  Subtype.Rat -> "Rat"
  Subtype.Elephant -> "Elephant"
  Subtype.Myr -> "Myr"

jsonToSubtype :: Value -> Either Text Subtype.Subtype
jsonToSubtype =
  decodeNullary
    (Text.pack "Subtype")
    [ (Text.pack "Mountain", Subtype.Mountain),
      (Text.pack "Swamp", Subtype.Swamp),
      (Text.pack "Forest", Subtype.Forest),
      (Text.pack "Island", Subtype.Island),
      (Text.pack "Plains", Subtype.Plains),
      (Text.pack "Goblin", Subtype.Goblin),
      (Text.pack "Warrior", Subtype.Warrior),
      (Text.pack "Human", Subtype.Human),
      (Text.pack "Bird", Subtype.Bird),
      (Text.pack "Ogre", Subtype.Ogre),
      (Text.pack "Centaur", Subtype.Centaur),
      (Text.pack "Cat", Subtype.Cat),
      (Text.pack "Dinosaur", Subtype.Dinosaur),
      (Text.pack "Beast", Subtype.Beast),
      (Text.pack "Rat", Subtype.Rat),
      (Text.pack "Elephant", Subtype.Elephant),
      (Text.pack "Myr", Subtype.Myr)
    ]

supertypeToJson :: Supertype.Supertype -> Value
supertypeToJson s = nullary . Text.pack $ case s of
  Supertype.Basic -> "Basic"
  Supertype.Legendary -> "Legendary"

jsonToSupertype :: Value -> Either Text Supertype.Supertype
jsonToSupertype =
  decodeNullary
    (Text.pack "Supertype")
    [ (Text.pack "Basic", Supertype.Basic),
      (Text.pack "Legendary", Supertype.Legendary)
    ]

keywordToJson :: Keyword.Keyword -> Value
keywordToJson k = nullary . Text.pack $ case k of
  Keyword.Deathtouch -> "Deathtouch"
  Keyword.Defender -> "Defender"
  Keyword.DoubleStrike -> "DoubleStrike"
  Keyword.FirstStrike -> "FirstStrike"
  Keyword.Flying -> "Flying"
  Keyword.Haste -> "Haste"
  Keyword.Indestructible -> "Indestructible"
  Keyword.Reach -> "Reach"
  Keyword.Trample -> "Trample"
  Keyword.Vigilance -> "Vigilance"

jsonToKeyword :: Value -> Either Text Keyword.Keyword
jsonToKeyword =
  decodeNullary
    (Text.pack "Keyword")
    [ (Text.pack "Deathtouch", Keyword.Deathtouch),
      (Text.pack "Defender", Keyword.Defender),
      (Text.pack "DoubleStrike", Keyword.DoubleStrike),
      (Text.pack "FirstStrike", Keyword.FirstStrike),
      (Text.pack "Flying", Keyword.Flying),
      (Text.pack "Haste", Keyword.Haste),
      (Text.pack "Indestructible", Keyword.Indestructible),
      (Text.pack "Reach", Keyword.Reach),
      (Text.pack "Trample", Keyword.Trample),
      (Text.pack "Vigilance", Keyword.Vigilance)
    ]

zoneToJson :: Zone.Zone -> Value
zoneToJson z = nullary . Text.pack $ case z of
  Zone.Library -> "Library"
  Zone.Hand -> "Hand"
  Zone.Graveyard -> "Graveyard"
  Zone.Battlefield -> "Battlefield"
  Zone.Stack -> "Stack"
  Zone.Exile -> "Exile"

jsonToZone :: Value -> Either Text Zone.Zone
jsonToZone =
  decodeNullary
    (Text.pack "Zone")
    [ (Text.pack "Library", Zone.Library),
      (Text.pack "Hand", Zone.Hand),
      (Text.pack "Graveyard", Zone.Graveyard),
      (Text.pack "Battlefield", Zone.Battlefield),
      (Text.pack "Stack", Zone.Stack),
      (Text.pack "Exile", Zone.Exile)
    ]

durationToJson :: Duration.Duration -> Value
durationToJson d = nullary . Text.pack $ case d of
  Duration.UntilEndOfTurn -> "UntilEndOfTurn"
  Duration.Indefinite -> "Indefinite"

jsonToDuration :: Value -> Either Text Duration.Duration
jsonToDuration =
  decodeNullary
    (Text.pack "Duration")
    [ (Text.pack "UntilEndOfTurn", Duration.UntilEndOfTurn),
      (Text.pack "Indefinite", Duration.Indefinite)
    ]

additionalCostToJson :: AdditionalCost.AdditionalCost -> Value
additionalCostToJson a = nullary . Text.pack $ case a of
  AdditionalCost.TapSelf -> "TapSelf"
  AdditionalCost.SacrificeSelf -> "SacrificeSelf"

jsonToAdditionalCost :: Value -> Either Text AdditionalCost.AdditionalCost
jsonToAdditionalCost =
  decodeNullary
    (Text.pack "AdditionalCost")
    [ (Text.pack "TapSelf", AdditionalCost.TapSelf),
      (Text.pack "SacrificeSelf", AdditionalCost.SacrificeSelf)
    ]

targetSpecToJson :: TargetSpec.TargetSpec -> Value
targetSpecToJson t = nullary . Text.pack $ case t of
  TargetSpec.AnyTarget -> "AnyTarget"
  TargetSpec.CreatureTarget -> "CreatureTarget"
  TargetSpec.SpellOrPermanentTarget -> "SpellOrPermanentTarget"
  TargetSpec.LandTarget -> "LandTarget"
  TargetSpec.PlayerTarget -> "PlayerTarget"
  TargetSpec.CreatureOrEnchantmentTarget -> "CreatureOrEnchantmentTarget"

jsonToTargetSpec :: Value -> Either Text TargetSpec.TargetSpec
jsonToTargetSpec =
  decodeNullary
    (Text.pack "TargetSpec")
    [ (Text.pack "AnyTarget", TargetSpec.AnyTarget),
      (Text.pack "CreatureTarget", TargetSpec.CreatureTarget),
      (Text.pack "SpellOrPermanentTarget", TargetSpec.SpellOrPermanentTarget),
      (Text.pack "LandTarget", TargetSpec.LandTarget),
      (Text.pack "PlayerTarget", TargetSpec.PlayerTarget),
      (Text.pack "CreatureOrEnchantmentTarget", TargetSpec.CreatureOrEnchantmentTarget)
    ]

triggerConditionToJson :: TriggerCondition.TriggerCondition -> Value
triggerConditionToJson t = nullary . Text.pack $ case t of
  TriggerCondition.SelfEnters -> "SelfEnters"

jsonToTriggerCondition :: Value -> Either Text TriggerCondition.TriggerCondition
jsonToTriggerCondition =
  decodeNullary
    (Text.pack "TriggerCondition")
    [(Text.pack "SelfEnters", TriggerCondition.SelfEnters)]

castingPermissionToJson :: CastingPermission.CastingPermission -> Value
castingPermissionToJson c = nullary . Text.pack $ case c of
  CastingPermission.CastFromLibraryWhileSearching -> "CastFromLibraryWhileSearching"

jsonToCastingPermission :: Value -> Either Text CastingPermission.CastingPermission
jsonToCastingPermission =
  decodeNullary
    (Text.pack "CastingPermission")
    [(Text.pack "CastFromLibraryWhileSearching", CastingPermission.CastFromLibraryWhileSearching)]

cardCriterionToJson :: CardCriterion.CardCriterion -> Value
cardCriterionToJson c = nullary . Text.pack $ case c of
  CardCriterion.BasicLandCard -> "BasicLandCard"

jsonToCardCriterion :: Value -> Either Text CardCriterion.CardCriterion
jsonToCardCriterion =
  decodeNullary
    (Text.pack "CardCriterion")
    [(Text.pack "BasicLandCard", CardCriterion.BasicLandCard)]

-- Newtypes -------------------------------------------------------------------

slotNameToJson :: SlotName.SlotName -> Value
slotNameToJson (SlotName.MkSlotName t) = Json.jText t

jsonToSlotName :: Value -> Either Text SlotName.SlotName
jsonToSlotName value = SlotName.MkSlotName <$> Json.asText value

objectIdToJson :: ObjectId.ObjectId -> Value
objectIdToJson (ObjectId.MkObjectId n) = natTo n

jsonToObjectId :: Value -> Either Text ObjectId.ObjectId
jsonToObjectId value = ObjectId.MkObjectId <$> natFrom value

-- Mana, quantity, power/toughness --------------------------------------------

manaTypeToJson :: ManaType.ManaType -> Value
manaTypeToJson mt = case mt of
  ManaType.Colored c -> Json.tagged (Text.pack "Colored") (Just (colorToJson c))
  ManaType.Colorless -> nullary (Text.pack "Colorless")

jsonToManaType :: Value -> Either Text ManaType.ManaType
jsonToManaType value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Colored", Just v) -> ManaType.Colored <$> jsonToColor v
    ("Colorless", _) -> Right ManaType.Colorless
    _ -> Left (Text.pack "unknown ManaType: " <> t)

manaSymbolToJson :: ManaSymbol.ManaSymbol -> Value
manaSymbolToJson ms = case ms of
  ManaSymbol.Generic n -> Json.tagged (Text.pack "Generic") (Just (natTo n))
  ManaSymbol.OfType mt -> Json.tagged (Text.pack "OfType") (Just (manaTypeToJson mt))
  ManaSymbol.Variable -> nullary (Text.pack "Variable")

jsonToManaSymbol :: Value -> Either Text ManaSymbol.ManaSymbol
jsonToManaSymbol value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Generic", Just v) -> ManaSymbol.Generic <$> natFrom v
    ("OfType", Just v) -> ManaSymbol.OfType <$> jsonToManaType v
    ("Variable", _) -> Right ManaSymbol.Variable
    _ -> Left (Text.pack "unknown ManaSymbol: " <> t)

manaCostToJson :: ManaCost.ManaCost -> Value
manaCostToJson (ManaCost.MkManaCost xs) = listTo manaSymbolToJson xs

jsonToManaCost :: Value -> Either Text ManaCost.ManaCost
jsonToManaCost value = ManaCost.MkManaCost <$> listFrom jsonToManaSymbol value

quantityToJson :: Quantity.Quantity -> Value
quantityToJson q = case q of
  Quantity.Literal n -> Json.tagged (Text.pack "Literal") (Just (Json.jInt n))
  Quantity.ManaValue -> nullary (Text.pack "ManaValue")
  Quantity.X -> nullary (Text.pack "X")

jsonToQuantity :: Value -> Either Text Quantity.Quantity
jsonToQuantity value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Json.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("X", _) -> Right Quantity.X
    _ -> Left (Text.pack "unknown Quantity: " <> t)

powerToJson :: Power.Power -> Value
powerToJson (Power.MkPower q) = quantityToJson q

jsonToPower :: Value -> Either Text Power.Power
jsonToPower value = Power.MkPower <$> jsonToQuantity value

toughnessToJson :: Toughness.Toughness -> Value
toughnessToJson (Toughness.MkToughness q) = quantityToJson q

jsonToToughness :: Value -> Either Text Toughness.Toughness
jsonToToughness value = Toughness.MkToughness <$> jsonToQuantity value

-- Modification, affected -----------------------------------------------------

modificationToJson :: Modification.Modification -> Value
modificationToJson m = case m of
  Modification.GainKeyword k -> Json.tagged (Text.pack "GainKeyword") (Just (keywordToJson k))
  Modification.LoseAllAbilities -> nullary (Text.pack "LoseAllAbilities")
  Modification.SetBasePowerToughness p t -> Json.tagged (Text.pack "SetBasePowerToughness") (Just (Array [quantityToJson p, quantityToJson t]))
  Modification.ModifyPowerToughness p t -> Json.tagged (Text.pack "ModifyPowerToughness") (Just (Array [quantityToJson p, quantityToJson t]))
  Modification.SetLandSubtype s -> Json.tagged (Text.pack "SetLandSubtype") (Just (subtypeToJson s))
  Modification.AddLandSubtype s -> Json.tagged (Text.pack "AddLandSubtype") (Just (subtypeToJson s))
  Modification.AddCardType c -> Json.tagged (Text.pack "AddCardType") (Just (cardTypeToJson c))
  Modification.ChangeSubtypeWord a b -> Json.tagged (Text.pack "ChangeSubtypeWord") (Just (Array [subtypeToJson a, subtypeToJson b]))

jsonToModification :: Value -> Either Text Modification.Modification
jsonToModification value = do
  (t, mv) <- Json.tag value
  let pair v = case v of
        Just (Array [x, y]) -> Right (x, y)
        _ -> Left (Text.pack "expected a two-element array")
  case Text.unpack t of
    "GainKeyword" -> withValue mv (fmap Modification.GainKeyword . jsonToKeyword)
    "LoseAllAbilities" -> Right Modification.LoseAllAbilities
    "SetBasePowerToughness" -> pair mv >>= \(x, y) -> Modification.SetBasePowerToughness <$> jsonToQuantity x <*> jsonToQuantity y
    "ModifyPowerToughness" -> pair mv >>= \(x, y) -> Modification.ModifyPowerToughness <$> jsonToQuantity x <*> jsonToQuantity y
    "SetLandSubtype" -> withValue mv (fmap Modification.SetLandSubtype . jsonToSubtype)
    "AddLandSubtype" -> withValue mv (fmap Modification.AddLandSubtype . jsonToSubtype)
    "AddCardType" -> withValue mv (fmap Modification.AddCardType . jsonToCardType)
    "ChangeSubtypeWord" -> pair mv >>= \(x, y) -> Modification.ChangeSubtypeWord <$> jsonToSubtype x <*> jsonToSubtype y
    _ -> Left (Text.pack "unknown Modification: " <> t)

withValue :: Maybe Value -> (Value -> Either Text a) -> Either Text a
withValue mv f = case mv of
  Just v -> f v
  Nothing -> Left (Text.pack "missing tagged value")

affectedToJson :: Affected.Affected -> Value
affectedToJson a = case a of
  Affected.TheseObjects ids -> Json.tagged (Text.pack "TheseObjects") (Just (setTo objectIdToJson ids))
  Affected.AllCreatures -> nullary (Text.pack "AllCreatures")
  Affected.AllLands -> nullary (Text.pack "AllLands")
  Affected.AllNonbasicLands -> nullary (Text.pack "AllNonbasicLands")
  Affected.OtherNonAuraEnchantments -> nullary (Text.pack "OtherNonAuraEnchantments")

jsonToAffected :: Value -> Either Text Affected.Affected
jsonToAffected value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "TheseObjects" -> withValue mv (fmap Affected.TheseObjects . setFrom jsonToObjectId)
    "AllCreatures" -> Right Affected.AllCreatures
    "AllLands" -> Right Affected.AllLands
    "AllNonbasicLands" -> Right Affected.AllNonbasicLands
    "OtherNonAuraEnchantments" -> Right Affected.OtherNonAuraEnchantments
    _ -> Left (Text.pack "unknown Affected: " <> t)

-- Effect ---------------------------------------------------------------------

effectToJson :: Effect.Effect -> Value
effectToJson e = case e of
  Effect.DealDamage s q -> Json.tagged (Text.pack "DealDamage") (Just (Array [slotNameToJson s, quantityToJson q]))
  Effect.ModifyTarget d m s -> Json.tagged (Text.pack "ModifyTarget") (Just (Array [durationToJson d, modificationToJson m, slotNameToJson s]))
  Effect.ChangeText s -> Json.tagged (Text.pack "ChangeText") (Just (slotNameToJson s))
  Effect.AddMana mt -> Json.tagged (Text.pack "AddMana") (Just (manaTypeToJson mt))
  Effect.Search c -> Json.tagged (Text.pack "Search") (Just (cardCriterionToJson c))
  Effect.ExileAllGraveyards -> nullary (Text.pack "ExileAllGraveyards")
  Effect.ControlPlayerNextTurn s -> Json.tagged (Text.pack "ControlPlayerNextTurn") (Just (slotNameToJson s))
  Effect.Destroy s -> Json.tagged (Text.pack "Destroy") (Just (slotNameToJson s))
  Effect.MoveToZone s z -> Json.tagged (Text.pack "MoveToZone") (Just (Array [slotNameToJson s, zoneToJson z]))

jsonToEffect :: Value -> Either Text Effect.Effect
jsonToEffect value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "DealDamage" -> case mv of
      Just (Array [s, q]) -> Effect.DealDamage <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "DealDamage expects [slot, quantity]")
    "ModifyTarget" -> case mv of
      Just (Array [d, m, s]) -> Effect.ModifyTarget <$> jsonToDuration d <*> jsonToModification m <*> jsonToSlotName s
      _ -> Left (Text.pack "ModifyTarget expects [duration, modification, slot]")
    "ChangeText" -> withValue mv (fmap Effect.ChangeText . jsonToSlotName)
    "AddMana" -> withValue mv (fmap Effect.AddMana . jsonToManaType)
    "Search" -> withValue mv (fmap Effect.Search . jsonToCardCriterion)
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "ControlPlayerNextTurn" -> withValue mv (fmap Effect.ControlPlayerNextTurn . jsonToSlotName)
    "Destroy" -> withValue mv (fmap Effect.Destroy . jsonToSlotName)
    "MoveToZone" -> case mv of
      Just (Array [s, z]) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z
      _ -> Left (Text.pack "MoveToZone expects [slot, zone]")
    _ -> Left (Text.pack "unknown Effect: " <> t)

-- Records & abilities --------------------------------------------------------

targetSpecsToJson :: Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Value
targetSpecsToJson m =
  listTo (\(k, v) -> Object [(Text.pack "slot", slotNameToJson k), (Text.pack "spec", targetSpecToJson v)]) (Map.toAscList m)

jsonToTargetSpecs :: Value -> Either Text (Map.Map SlotName.SlotName TargetSpec.TargetSpec)
jsonToTargetSpecs value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "slot") ps >>= jsonToSlotName
        s <- Json.field (Text.pack "spec") ps >>= jsonToTargetSpec
        pure (k, s)
   in Map.fromList <$> listFrom decodeEntry value

typeLineToJson :: TypeLine.TypeLine -> Value
typeLineToJson tl =
  Object
    [ (Text.pack "supertypes", setTo supertypeToJson (TypeLine.supertypes tl)),
      (Text.pack "types", setTo cardTypeToJson (TypeLine.types tl)),
      (Text.pack "subtypes", setTo subtypeToJson (TypeLine.subtypes tl))
    ]

jsonToTypeLine :: Value -> Either Text TypeLine.TypeLine
jsonToTypeLine value = do
  ps <- Json.asObject value
  sup <- Json.field (Text.pack "supertypes") ps >>= setFrom jsonToSupertype
  tys <- Json.field (Text.pack "types") ps >>= setFrom jsonToCardType
  sub <- Json.field (Text.pack "subtypes") ps >>= setFrom jsonToSubtype
  pure (TypeLine.MkTypeLine sup tys sub)

staticAbilityToJson :: StaticAbility.StaticAbility -> Value
staticAbilityToJson sa =
  Object
    [ (Text.pack "affected", affectedToJson (StaticAbility.affected sa)),
      (Text.pack "modification", modificationToJson (StaticAbility.modification sa))
    ]

jsonToStaticAbility :: Value -> Either Text StaticAbility.StaticAbility
jsonToStaticAbility value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "affected") ps >>= jsonToAffected
  m <- Json.field (Text.pack "modification") ps >>= jsonToModification
  pure (StaticAbility.MkStaticAbility a m)

abilityCostToJson :: AbilityCost.AbilityCost -> Value
abilityCostToJson ac =
  Object
    [ (Text.pack "mana", maybeTo manaCostToJson (AbilityCost.mana ac)),
      (Text.pack "additional", listTo additionalCostToJson (AbilityCost.additional ac))
    ]

jsonToAbilityCost :: Value -> Either Text AbilityCost.AbilityCost
jsonToAbilityCost value = do
  ps <- Json.asObject value
  manaCost <- maybeFrom jsonToManaCost (Maybe.fromMaybe Null (Json.optField (Text.pack "mana") ps))
  add <- Json.field (Text.pack "additional") ps >>= listFrom jsonToAdditionalCost
  pure (AbilityCost.MkAbilityCost manaCost add)

activatedAbilityToJson :: ActivatedAbility.ActivatedAbility -> Value
activatedAbilityToJson aa =
  Object
    [ (Text.pack "cost", abilityCostToJson (ActivatedAbility.cost aa)),
      (Text.pack "effects", listTo effectToJson (ActivatedAbility.effects aa)),
      (Text.pack "targetSpecs", targetSpecsToJson (ActivatedAbility.targetSpecs aa))
    ]

jsonToActivatedAbility :: Value -> Either Text ActivatedAbility.ActivatedAbility
jsonToActivatedAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "cost") ps >>= jsonToAbilityCost
  es <- Json.field (Text.pack "effects") ps >>= listFrom jsonToEffect
  ts <- Json.field (Text.pack "targetSpecs") ps >>= jsonToTargetSpecs
  pure (ActivatedAbility.MkActivatedAbility c es ts)

replacementEffectToJson :: ReplacementEffect.ReplacementEffect -> Value
replacementEffectToJson (ReplacementEffect.RedirectZoneChange w t) =
  Json.tagged (Text.pack "RedirectZoneChange") (Just (Array [zoneToJson w, zoneToJson t]))

jsonToReplacementEffect :: Value -> Either Text ReplacementEffect.ReplacementEffect
jsonToReplacementEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("RedirectZoneChange", Just (Array [w, d])) -> ReplacementEffect.RedirectZoneChange <$> jsonToZone w <*> jsonToZone d
    _ -> Left (Text.pack "unknown ReplacementEffect: " <> t)

triggeredAbilityToJson :: TriggeredAbility.TriggeredAbility -> Value
triggeredAbilityToJson ta =
  Object
    [ (Text.pack "condition", triggerConditionToJson (TriggeredAbility.condition ta)),
      (Text.pack "effects", listTo effectToJson (TriggeredAbility.effects ta)),
      (Text.pack "targetSpecs", targetSpecsToJson (TriggeredAbility.targetSpecs ta))
    ]

jsonToTriggeredAbility :: Value -> Either Text TriggeredAbility.TriggeredAbility
jsonToTriggeredAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "condition") ps >>= jsonToTriggerCondition
  es <- Json.field (Text.pack "effects") ps >>= listFrom jsonToEffect
  ts <- Json.field (Text.pack "targetSpecs") ps >>= jsonToTargetSpecs
  pure (TriggeredAbility.MkTriggeredAbility c es ts)

-- Card & Printing ------------------------------------------------------------

cardToJson :: CardT.Card -> Value
cardToJson c =
  Object
    [ (Text.pack "name", Json.jText (CardT.name c)),
      (Text.pack "manaCost", maybeTo manaCostToJson (CardT.manaCost c)),
      (Text.pack "typeLine", typeLineToJson (CardT.typeLine c)),
      (Text.pack "power", maybeTo powerToJson (CardT.power c)),
      (Text.pack "toughness", maybeTo toughnessToJson (CardT.toughness c)),
      (Text.pack "keywords", setTo keywordToJson (CardT.keywords c)),
      (Text.pack "staticAbilities", listTo staticAbilityToJson (CardT.staticAbilities c)),
      (Text.pack "effects", listTo effectToJson (CardT.effects c)),
      (Text.pack "activatedAbilities", listTo activatedAbilityToJson (CardT.activatedAbilities c)),
      (Text.pack "replacementEffects", listTo replacementEffectToJson (CardT.replacementEffects c)),
      (Text.pack "triggeredAbilities", listTo triggeredAbilityToJson (CardT.triggeredAbilities c)),
      (Text.pack "castingPermissions", listTo castingPermissionToJson (CardT.castingPermissions c)),
      (Text.pack "targetSpecs", targetSpecsToJson (CardT.targetSpecs c))
    ]

getOpt :: Text -> [(Text, Value)] -> Value
getOpt k ps = Maybe.fromMaybe Null (Json.optField k ps)

jsonToCard :: Value -> Either Text CardT.Card
jsonToCard value = do
  ps <- Json.asObject value
  name <- Json.field (Text.pack "name") ps >>= Json.asText
  manaCost <- maybeFrom jsonToManaCost (getOpt (Text.pack "manaCost") ps)
  typeLine <- Json.field (Text.pack "typeLine") ps >>= jsonToTypeLine
  power <- maybeFrom jsonToPower (getOpt (Text.pack "power") ps)
  toughness <- maybeFrom jsonToToughness (getOpt (Text.pack "toughness") ps)
  keywords <- Json.field (Text.pack "keywords") ps >>= setFrom jsonToKeyword
  statics <- Json.field (Text.pack "staticAbilities") ps >>= listFrom jsonToStaticAbility
  effects <- Json.field (Text.pack "effects") ps >>= listFrom jsonToEffect
  activated <- Json.field (Text.pack "activatedAbilities") ps >>= listFrom jsonToActivatedAbility
  replacements <- Json.field (Text.pack "replacementEffects") ps >>= listFrom jsonToReplacementEffect
  triggered <- Json.field (Text.pack "triggeredAbilities") ps >>= listFrom jsonToTriggeredAbility
  permissions <- Json.field (Text.pack "castingPermissions") ps >>= listFrom jsonToCastingPermission
  targets <- Json.field (Text.pack "targetSpecs") ps >>= jsonToTargetSpecs
  pure
    CardT.MkCard
      { CardT.name = name,
        CardT.manaCost = manaCost,
        CardT.typeLine = typeLine,
        CardT.power = power,
        CardT.toughness = toughness,
        CardT.keywords = keywords,
        CardT.staticAbilities = statics,
        CardT.effects = effects,
        CardT.activatedAbilities = activated,
        CardT.replacementEffects = replacements,
        CardT.triggeredAbilities = triggered,
        CardT.castingPermissions = permissions,
        CardT.targetSpecs = targets
      }

printingToJson :: Printing.Printing -> Value
printingToJson (Printing.MkPrinting c) = cardToJson c

jsonToPrinting :: Value -> Either Text Printing.Printing
jsonToPrinting value = Printing.MkPrinting <$> jsonToCard value

-- Slug -----------------------------------------------------------------------

-- The file name for a card: its name lowercased, every non-alphanumeric run
-- (spaces, punctuation, "//") collapsed to a single "-". "Urborg, Tomb of
-- Yawgmoth" -> "urborg-tomb-of-yawgmoth".
slugify :: Text -> Text
slugify t =
  let keep c = if Char.isAlphaNum c then c else ' '
   in Text.intercalate (Text.pack "-") (Text.words (Text.map keep (Text.toLower t)))
