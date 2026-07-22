-- | The sole authority for @Card ⇆ Json@ (§2 of the M3.5 spec), mirroring
-- 'Pawl.Resolve' (the sole @case@-on-@Effect@ home). Free @xToJson@\/@jsonToX@
-- functions -- no type classes -- over the transitive closure of @Card@'s
-- fields. Every @Pawl.Type.*@ module stays JSON-free; casing on an effect's
-- identity here is open-half machinery, not the rules core.
module Pawl.Codec where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Json as Json
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.AbilityName as AbilityName
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AdditionalCost as AdditionalCost
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Binding as Binding
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.CardCriterion as CardCriterion
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CastingPermission as CastingPermission
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.CountSpec as CountSpec
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.CounterPattern as CounterPattern
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.DamagePattern as DamagePattern
import qualified Pawl.Type.DamageRewrite as DamageRewrite
import qualified Pawl.Type.DelayedTrigger as DelayedTrigger
import qualified Pawl.Type.DestructionRewrite as DestructionRewrite
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.EntryOption as EntryOption
import qualified Pawl.Type.EntryRewrite as EntryRewrite
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.Json (Value (Array, Boolean, Null, Object))
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PermanentCriterion as PermanentCriterion
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Scaling as Scaling
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.StateCondition as StateCondition
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.TokenPattern as TokenPattern
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern

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

seqTo :: (a -> Value) -> Seq.Seq a -> Value
seqTo f = Array . map f . Foldable.toList

seqFrom :: (Value -> Either Text a) -> Value -> Either Text (Seq.Seq a)
seqFrom f value = Seq.fromList <$> listFrom f value

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

counterKindToJson :: CounterKind.CounterKind -> Value
counterKindToJson k = nullary . Text.pack $ case k of
  CounterKind.PlusOnePlusOne -> "PlusOnePlusOne"
  CounterKind.MinusOneMinusOne -> "MinusOneMinusOne"

jsonToCounterKind :: Value -> Either Text CounterKind.CounterKind
jsonToCounterKind =
  decodeNullary
    (Text.pack "CounterKind")
    [ (Text.pack "PlusOnePlusOne", CounterKind.PlusOnePlusOne),
      (Text.pack "MinusOneMinusOne", CounterKind.MinusOneMinusOne)
    ]

countSpecToJson :: CountSpec.CountSpec -> Value
countSpecToJson s = nullary . Text.pack $ case s of
  CountSpec.CardTypesInAllGraveyards -> "CardTypesInAllGraveyards"
  CountSpec.CardsInYourHand -> "CardsInYourHand"
  CountSpec.CreaturesDiedThisTurn -> "CreaturesDiedThisTurn"

jsonToCountSpec :: Value -> Either Text CountSpec.CountSpec
jsonToCountSpec =
  decodeNullary
    (Text.pack "CountSpec")
    [ (Text.pack "CardTypesInAllGraveyards", CountSpec.CardTypesInAllGraveyards),
      (Text.pack "CardsInYourHand", CountSpec.CardsInYourHand),
      (Text.pack "CreaturesDiedThisTurn", CountSpec.CreaturesDiedThisTurn)
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
  Subtype.Skeleton -> "Skeleton"
  Subtype.Wall -> "Wall"
  Subtype.Wizard -> "Wizard"
  Subtype.Shapeshifter -> "Shapeshifter"
  Subtype.Lhurgoyf -> "Lhurgoyf"
  Subtype.Arcane -> "Arcane"
  Subtype.Barbarian -> "Barbarian"
  Subtype.Zombie -> "Zombie"

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
      (Text.pack "Myr", Subtype.Myr),
      (Text.pack "Skeleton", Subtype.Skeleton),
      (Text.pack "Wall", Subtype.Wall),
      (Text.pack "Wizard", Subtype.Wizard),
      (Text.pack "Shapeshifter", Subtype.Shapeshifter),
      (Text.pack "Lhurgoyf", Subtype.Lhurgoyf),
      (Text.pack "Arcane", Subtype.Arcane),
      (Text.pack "Barbarian", Subtype.Barbarian),
      (Text.pack "Zombie", Subtype.Zombie)
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
  Keyword.Fear -> "Fear"
  Keyword.Devoid -> "Devoid"

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
      (Text.pack "Vigilance", Keyword.Vigilance),
      (Text.pack "Fear", Keyword.Fear),
      (Text.pack "Devoid", Keyword.Devoid)
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

beginningStepToJson :: BeginningStep.BeginningStep -> Value
beginningStepToJson s = nullary . Text.pack $ case s of
  BeginningStep.Untap -> "Untap"
  BeginningStep.Upkeep -> "Upkeep"
  BeginningStep.DrawStep -> "DrawStep"

jsonToBeginningStep :: Value -> Either Text BeginningStep.BeginningStep
jsonToBeginningStep =
  decodeNullary
    (Text.pack "BeginningStep")
    [ (Text.pack "Untap", BeginningStep.Untap),
      (Text.pack "Upkeep", BeginningStep.Upkeep),
      (Text.pack "DrawStep", BeginningStep.DrawStep)
    ]

combatStepToJson :: CombatStep.CombatStep -> Value
combatStepToJson s = nullary . Text.pack $ case s of
  CombatStep.BeginningOfCombat -> "BeginningOfCombat"
  CombatStep.DeclareAttackers -> "DeclareAttackers"
  CombatStep.DeclareBlockers -> "DeclareBlockers"
  CombatStep.CombatDamage -> "CombatDamage"
  CombatStep.EndOfCombat -> "EndOfCombat"

jsonToCombatStep :: Value -> Either Text CombatStep.CombatStep
jsonToCombatStep =
  decodeNullary
    (Text.pack "CombatStep")
    [ (Text.pack "BeginningOfCombat", CombatStep.BeginningOfCombat),
      (Text.pack "DeclareAttackers", CombatStep.DeclareAttackers),
      (Text.pack "DeclareBlockers", CombatStep.DeclareBlockers),
      (Text.pack "CombatDamage", CombatStep.CombatDamage),
      (Text.pack "EndOfCombat", CombatStep.EndOfCombat)
    ]

endingStepToJson :: EndingStep.EndingStep -> Value
endingStepToJson s = nullary . Text.pack $ case s of
  EndingStep.EndStep -> "EndStep"
  EndingStep.Cleanup -> "Cleanup"

jsonToEndingStep :: Value -> Either Text EndingStep.EndingStep
jsonToEndingStep =
  decodeNullary
    (Text.pack "EndingStep")
    [ (Text.pack "EndStep", EndingStep.EndStep),
      (Text.pack "Cleanup", EndingStep.Cleanup)
    ]

phaseToJson :: Phase.Phase -> Value
phaseToJson p = case p of
  Phase.Beginning s -> Json.tagged (Text.pack "Beginning") (Just (beginningStepToJson s))
  Phase.PrecombatMain -> nullary (Text.pack "PrecombatMain")
  Phase.Combat s -> Json.tagged (Text.pack "Combat") (Just (combatStepToJson s))
  Phase.PostcombatMain -> nullary (Text.pack "PostcombatMain")
  Phase.Ending s -> Json.tagged (Text.pack "Ending") (Just (endingStepToJson s))

jsonToPhase :: Value -> Either Text Phase.Phase
jsonToPhase value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Beginning", Just v) -> Phase.Beginning <$> jsonToBeginningStep v
    ("PrecombatMain", _) -> Right Phase.PrecombatMain
    ("Combat", Just v) -> Phase.Combat <$> jsonToCombatStep v
    ("PostcombatMain", _) -> Right Phase.PostcombatMain
    ("Ending", Just v) -> Phase.Ending <$> jsonToEndingStep v
    _ -> Left (Text.pack "unknown Phase: " <> t)

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

usesToJson :: Uses.Uses -> Value
usesToJson u = nullary . Text.pack $ case u of
  Uses.Unlimited -> "Unlimited"
  Uses.Once -> "Once"

jsonToUses :: Value -> Either Text Uses.Uses
jsonToUses =
  decodeNullary
    (Text.pack "Uses")
    [ (Text.pack "Unlimited", Uses.Unlimited),
      (Text.pack "Once", Uses.Once)
    ]

controllerRelationToJson :: ControllerRelation.ControllerRelation -> Value
controllerRelationToJson r = nullary . Text.pack $ case r of
  ControllerRelation.Yours -> "Yours"
  ControllerRelation.Anyones -> "Anyones"

jsonToControllerRelation :: Value -> Either Text ControllerRelation.ControllerRelation
jsonToControllerRelation =
  decodeNullary
    (Text.pack "ControllerRelation")
    [ (Text.pack "Yours", ControllerRelation.Yours),
      (Text.pack "Anyones", ControllerRelation.Anyones)
    ]

permanentCriterionToJson :: PermanentCriterion.PermanentCriterion -> Value
permanentCriterionToJson c = nullary . Text.pack $ case c of
  PermanentCriterion.AnyPermanent -> "AnyPermanent"
  PermanentCriterion.CreaturePermanent -> "CreaturePermanent"

jsonToPermanentCriterion :: Value -> Either Text PermanentCriterion.PermanentCriterion
jsonToPermanentCriterion =
  decodeNullary
    (Text.pack "PermanentCriterion")
    [ (Text.pack "AnyPermanent", PermanentCriterion.AnyPermanent),
      (Text.pack "CreaturePermanent", PermanentCriterion.CreaturePermanent)
    ]

damageRewriteToJson :: DamageRewrite.DamageRewrite -> Value
damageRewriteToJson r = nullary . Text.pack $ case r of
  DamageRewrite.PreventAll -> "PreventAll"

jsonToDamageRewrite :: Value -> Either Text DamageRewrite.DamageRewrite
jsonToDamageRewrite =
  decodeNullary (Text.pack "DamageRewrite") [(Text.pack "PreventAll", DamageRewrite.PreventAll)]

destructionRewriteToJson :: DestructionRewrite.DestructionRewrite -> Value
destructionRewriteToJson r = nullary . Text.pack $ case r of
  DestructionRewrite.Regenerate -> "Regenerate"

jsonToDestructionRewrite :: Value -> Either Text DestructionRewrite.DestructionRewrite
jsonToDestructionRewrite =
  decodeNullary (Text.pack "DestructionRewrite") [(Text.pack "Regenerate", DestructionRewrite.Regenerate)]

scalingToJson :: Scaling.Scaling -> Value
scalingToJson s = case s of
  Scaling.Multiply n -> Json.tagged (Text.pack "Multiply") (Just (natTo n))
  Scaling.AddMore n -> Json.tagged (Text.pack "AddMore") (Just (natTo n))

jsonToScaling :: Value -> Either Text Scaling.Scaling
jsonToScaling value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Multiply", Just v) -> fmap Scaling.Multiply (natFrom v)
    ("AddMore", Just v) -> fmap Scaling.AddMore (natFrom v)
    _ -> Left (Text.pack "unknown Scaling: " <> t)

entryOptionToJson :: EntryOption.EntryOption -> Value
entryOptionToJson o =
  Object
    [ (Text.pack "power", Json.jInt (EntryOption.power o)),
      (Text.pack "toughness", Json.jInt (EntryOption.toughness o)),
      (Text.pack "keywords", setTo keywordToJson (EntryOption.keywords o))
    ]

jsonToEntryOption :: Value -> Either Text EntryOption.EntryOption
jsonToEntryOption value = do
  ps <- Json.asObject value
  p <- Json.field (Text.pack "power") ps >>= Json.asInteger
  t <- Json.field (Text.pack "toughness") ps >>= Json.asInteger
  ks <- Json.field (Text.pack "keywords") ps >>= setFrom jsonToKeyword
  pure
    EntryOption.MkEntryOption
      { EntryOption.power = p,
        EntryOption.toughness = t,
        EntryOption.keywords = ks
      }

entryRewriteToJson :: EntryRewrite.EntryRewrite -> Value
entryRewriteToJson r = case r of
  EntryRewrite.AsCopy -> nullary (Text.pack "AsCopy")
  EntryRewrite.ChoiceOf options -> Json.tagged (Text.pack "ChoiceOf") (Just (listTo entryOptionToJson options))

jsonToEntryRewrite :: Value -> Either Text EntryRewrite.EntryRewrite
jsonToEntryRewrite value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AsCopy", _) -> Right EntryRewrite.AsCopy
    ("ChoiceOf", Just v) -> fmap EntryRewrite.ChoiceOf (listFrom jsonToEntryOption v)
    _ -> Left (Text.pack "unknown EntryRewrite: " <> t)

zoneChangePatternToJson :: ZoneChangePattern.ZoneChangePattern -> Value
zoneChangePatternToJson p =
  Object
    [ (Text.pack "whenDestination", zoneToJson (ZoneChangePattern.whenDestination p)),
      (Text.pack "whoseObject", controllerRelationToJson (ZoneChangePattern.whoseObject p))
    ]

jsonToZoneChangePattern :: Value -> Either Text ZoneChangePattern.ZoneChangePattern
jsonToZoneChangePattern value = do
  ps <- Json.asObject value
  d <- Json.field (Text.pack "whenDestination") ps >>= jsonToZone
  w <- Json.field (Text.pack "whoseObject") ps >>= jsonToControllerRelation
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = d,
        ZoneChangePattern.whoseObject = w
      }

counterPatternToJson :: CounterPattern.CounterPattern -> Value
counterPatternToJson p =
  Object
    [ (Text.pack "whichKind", maybeTo counterKindToJson (CounterPattern.whichKind p)),
      (Text.pack "whose", controllerRelationToJson (CounterPattern.whose p)),
      (Text.pack "onWhat", permanentCriterionToJson (CounterPattern.onWhat p))
    ]

jsonToCounterPattern :: Value -> Either Text CounterPattern.CounterPattern
jsonToCounterPattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= maybeFrom jsonToCounterKind
  w <- Json.field (Text.pack "whose") ps >>= jsonToControllerRelation
  o <- Json.field (Text.pack "onWhat") ps >>= jsonToPermanentCriterion
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = k,
        CounterPattern.whose = w,
        CounterPattern.onWhat = o
      }

tokenPatternToJson :: TokenPattern.TokenPattern -> Value
tokenPatternToJson p =
  Object [(Text.pack "whose", controllerRelationToJson (TokenPattern.whose p))]

jsonToTokenPattern :: Value -> Either Text TokenPattern.TokenPattern
jsonToTokenPattern value = do
  ps <- Json.asObject value
  w <- Json.field (Text.pack "whose") ps >>= jsonToControllerRelation
  pure TokenPattern.MkTokenPattern {TokenPattern.whose = w}

damagePatternToJson :: DamagePattern.DamagePattern -> Value
damagePatternToJson p =
  Object [(Text.pack "whichKind", maybeTo damageKindToJson (DamagePattern.whichKind p))]

jsonToDamagePattern :: Value -> Either Text DamagePattern.DamagePattern
jsonToDamagePattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= maybeFrom jsonToDamageKind
  pure DamagePattern.MkDamagePattern {DamagePattern.whichKind = k}

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
  TargetSpec.SpellTarget -> "SpellTarget"
  TargetSpec.WallTarget -> "WallTarget"
  TargetSpec.NonlandPermanentTarget -> "NonlandPermanentTarget"
  TargetSpec.NonblackCreatureTarget -> "NonblackCreatureTarget"

jsonToTargetSpec :: Value -> Either Text TargetSpec.TargetSpec
jsonToTargetSpec =
  decodeNullary
    (Text.pack "TargetSpec")
    [ (Text.pack "AnyTarget", TargetSpec.AnyTarget),
      (Text.pack "CreatureTarget", TargetSpec.CreatureTarget),
      (Text.pack "SpellOrPermanentTarget", TargetSpec.SpellOrPermanentTarget),
      (Text.pack "LandTarget", TargetSpec.LandTarget),
      (Text.pack "PlayerTarget", TargetSpec.PlayerTarget),
      (Text.pack "CreatureOrEnchantmentTarget", TargetSpec.CreatureOrEnchantmentTarget),
      (Text.pack "SpellTarget", TargetSpec.SpellTarget),
      (Text.pack "WallTarget", TargetSpec.WallTarget),
      (Text.pack "NonlandPermanentTarget", TargetSpec.NonlandPermanentTarget),
      (Text.pack "NonblackCreatureTarget", TargetSpec.NonblackCreatureTarget)
    ]

turnScopeToJson :: TurnScope.TurnScope -> Value
turnScopeToJson s = nullary . Text.pack $ case s of
  TurnScope.EachTurn -> "EachTurn"
  TurnScope.ControllersTurn -> "ControllersTurn"

jsonToTurnScope :: Value -> Either Text TurnScope.TurnScope
jsonToTurnScope =
  decodeNullary
    (Text.pack "TurnScope")
    [ (Text.pack "EachTurn", TurnScope.EachTurn),
      (Text.pack "ControllersTurn", TurnScope.ControllersTurn)
    ]

triggerConditionToJson :: TriggerCondition.TriggerCondition -> Value
triggerConditionToJson c = case c of
  TriggerCondition.SelfEnters -> nullary (Text.pack "SelfEnters")
  TriggerCondition.StepBegins p s -> Json.tagged (Text.pack "StepBegins") (Just (Array [phaseToJson p, turnScopeToJson s]))
  TriggerCondition.StateIs c2 -> Json.tagged (Text.pack "StateIs") (Just (stateConditionToJson c2))

jsonToTriggerCondition :: Value -> Either Text TriggerCondition.TriggerCondition
jsonToTriggerCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("StepBegins", Just (Array [p, s])) -> TriggerCondition.StepBegins <$> jsonToPhase p <*> jsonToTurnScope s
    ("StateIs", Just v) -> TriggerCondition.StateIs <$> jsonToStateCondition v
    _ -> Left (Text.pack "unknown TriggerCondition: " <> t)

stateConditionToJson :: StateCondition.StateCondition -> Value
stateConditionToJson c = case c of
  StateCondition.YouControlNo s -> Json.tagged (Text.pack "YouControlNo") (Just (subtypeToJson s))
  StateCondition.NoPermanentsOfSubtype s -> Json.tagged (Text.pack "NoPermanentsOfSubtype") (Just (subtypeToJson s))

jsonToStateCondition :: Value -> Either Text StateCondition.StateCondition
jsonToStateCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("YouControlNo", Just v) -> StateCondition.YouControlNo <$> jsonToSubtype v
    ("NoPermanentsOfSubtype", Just v) -> StateCondition.NoPermanentsOfSubtype <$> jsonToSubtype v
    _ -> Left (Text.pack "unknown StateCondition: " <> t)

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

-- SetController's PlayerId is runtime-only (never in card JSON, since a
-- SetController effect is baked at GainControl resolution, never authored on a
-- card), but the codec must stay total. Mirrors ObjectId's Natural encoding
-- (natTo/natFrom), not a bare Integer: PlayerId wraps a Natural (no partial
-- fromInteger on a negative wire value).
playerIdToJson :: PlayerId.PlayerId -> Value
playerIdToJson (PlayerId.MkPlayerId n) = natTo n

jsonToPlayerId :: Value -> Either Text PlayerId.PlayerId
jsonToPlayerId value = PlayerId.MkPlayerId <$> natFrom value

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
  Quantity.Star -> nullary (Text.pack "Star")
  Quantity.Plus a b -> Json.tagged (Text.pack "Plus") (Just (Array [quantityToJson a, quantityToJson b]))
  Quantity.Count s -> Json.tagged (Text.pack "Count") (Just (countSpecToJson s))

jsonToQuantity :: Value -> Either Text Quantity.Quantity
jsonToQuantity value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Json.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("X", _) -> Right Quantity.X
    ("Star", _) -> Right Quantity.Star
    ("Plus", Just (Array [x, y])) -> Quantity.Plus <$> jsonToQuantity x <*> jsonToQuantity y
    ("Count", Just v) -> Quantity.Count <$> jsonToCountSpec v
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
  Modification.SetController p -> Json.tagged (Text.pack "SetController") (Just (playerIdToJson p))
  Modification.SetColor cs -> Json.tagged (Text.pack "SetColor") (Just (setTo colorToJson cs))
  Modification.SwitchPowerToughness -> nullary (Text.pack "SwitchPowerToughness")

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
    "SetController" -> withValue mv (fmap Modification.SetController . jsonToPlayerId)
    "SetColor" -> withValue mv (fmap Modification.SetColor . setFrom jsonToColor)
    "SwitchPowerToughness" -> Right Modification.SwitchPowerToughness
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
  Affected.CreaturesOfColor c -> Json.tagged (Text.pack "CreaturesOfColor") (Just (colorToJson c))

jsonToAffected :: Value -> Either Text Affected.Affected
jsonToAffected value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "TheseObjects" -> withValue mv (fmap Affected.TheseObjects . setFrom jsonToObjectId)
    "AllCreatures" -> Right Affected.AllCreatures
    "AllLands" -> Right Affected.AllLands
    "AllNonbasicLands" -> Right Affected.AllNonbasicLands
    "OtherNonAuraEnchantments" -> Right Affected.OtherNonAuraEnchantments
    "CreaturesOfColor" -> withValue mv (fmap Affected.CreaturesOfColor . jsonToColor)
    _ -> Left (Text.pack "unknown Affected: " <> t)

recipientToJson :: Recipient.Recipient -> Value
recipientToJson r = case r of
  Recipient.ToCreature oid -> Json.tagged (Text.pack "ToCreature") (Just (objectIdToJson oid))
  Recipient.ToPlayer pid -> Json.tagged (Text.pack "ToPlayer") (Just (playerIdToJson pid))
  Recipient.ToObject oid -> Json.tagged (Text.pack "ToObject") (Just (objectIdToJson oid))

jsonToRecipient :: Value -> Either Text Recipient.Recipient
jsonToRecipient value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ToCreature", Just v) -> Recipient.ToCreature <$> jsonToObjectId v
    ("ToPlayer", Just v) -> Recipient.ToPlayer <$> jsonToPlayerId v
    ("ToObject", Just v) -> Recipient.ToObject <$> jsonToObjectId v
    _ -> Left (Text.pack "unknown Recipient: " <> t)

damageKindToJson :: DamageKind.DamageKind -> Value
damageKindToJson k = nullary . Text.pack $ case k of
  DamageKind.Combat -> "Combat"
  DamageKind.Noncombat -> "Noncombat"

jsonToDamageKind :: Value -> Either Text DamageKind.DamageKind
jsonToDamageKind =
  decodeNullary
    (Text.pack "DamageKind")
    [ (Text.pack "Combat", DamageKind.Combat),
      (Text.pack "Noncombat", DamageKind.Noncombat)
    ]

damageEventToJson :: DamageEvent.DamageEvent -> Value
damageEventToJson ev =
  Object
    [ (Text.pack "source", objectIdToJson (DamageEvent.source ev)),
      (Text.pack "target", recipientToJson (DamageEvent.target ev)),
      (Text.pack "amount", natTo (DamageEvent.amount ev)),
      (Text.pack "dealtByDeathtouch", Json.jBool (DamageEvent.dealtByDeathtouch ev)),
      (Text.pack "kind", damageKindToJson (DamageEvent.kind ev))
    ]

jsonToDamageEvent :: Value -> Either Text DamageEvent.DamageEvent
jsonToDamageEvent value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  t <- Json.field (Text.pack "target") ps >>= jsonToRecipient
  a <- Json.field (Text.pack "amount") ps >>= natFrom
  d <- Json.field (Text.pack "dealtByDeathtouch") ps >>= jsonToBoolDefault False
  k <- Json.field (Text.pack "kind") ps >>= jsonToDamageKind
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = s,
        DamageEvent.target = t,
        DamageEvent.amount = a,
        DamageEvent.dealtByDeathtouch = d,
        DamageEvent.kind = k
      }

zoneChangeToJson :: ZoneChange.ZoneChange -> Value
zoneChangeToJson zc =
  Object
    [ (Text.pack "object", objectIdToJson (ZoneChange.object zc)),
      (Text.pack "from", zoneToJson (ZoneChange.from zc)),
      (Text.pack "to", zoneToJson (ZoneChange.to zc))
    ]

jsonToZoneChange :: Value -> Either Text ZoneChange.ZoneChange
jsonToZoneChange value = do
  ps <- Json.asObject value
  o <- Json.field (Text.pack "object") ps >>= jsonToObjectId
  f <- Json.field (Text.pack "from") ps >>= jsonToZone
  t <- Json.field (Text.pack "to") ps >>= jsonToZone
  pure (ZoneChange.MkZoneChange o f t)

projectedCharacteristicsToJson :: PC.ProjectedCharacteristics -> Value
projectedCharacteristicsToJson pc =
  Object
    [ (Text.pack "keywords", setTo keywordToJson (PC.keywords pc)),
      (Text.pack "colors", setTo colorToJson (PC.colors pc)),
      (Text.pack "power", maybeTo Json.jInt (PC.power pc)),
      (Text.pack "toughness", maybeTo Json.jInt (PC.toughness pc)),
      (Text.pack "characteristicPT", maybeTo (\(p, t) -> Array [quantityToJson p, quantityToJson t]) (PC.characteristicPT pc)),
      (Text.pack "cardTypes", setTo cardTypeToJson (PC.cardTypes pc)),
      (Text.pack "subtypes", setTo subtypeToJson (PC.subtypes pc)),
      (Text.pack "rulesTextActive", Json.jBool (PC.rulesTextActive pc)),
      (Text.pack "activatedAbilities", listTo activatedAbilityToJson (PC.activatedAbilities pc)),
      (Text.pack "replacementEffects", listTo replacementEffectToJson (PC.replacementEffects pc)),
      (Text.pack "triggeredAbilities", listTo triggeredAbilityToJson (PC.triggeredAbilities pc))
    ]

jsonToProjectedCharacteristics :: Value -> Either Text PC.ProjectedCharacteristics
jsonToProjectedCharacteristics value = do
  ps <- Json.asObject value
  kws <- Json.field (Text.pack "keywords") ps >>= setFrom jsonToKeyword
  cols <- Json.field (Text.pack "colors") ps >>= setFrom jsonToColor
  -- power/toughness/characteristicPT are encoded as required keys (maybeTo
  -- writes JSON null for Nothing, never omits the key), so decoding them is
  -- Json.field (required) >>= maybeFrom (Null -> Nothing), exactly like every
  -- other field here -- not the optional getOpt a truly-omittable key would need.
  pow <- Json.field (Text.pack "power") ps >>= maybeFrom Json.asInteger
  tou <- Json.field (Text.pack "toughness") ps >>= maybeFrom Json.asInteger
  cda <- Json.field (Text.pack "characteristicPT") ps >>= maybeFrom jsonToQuantityPair
  cts <- Json.field (Text.pack "cardTypes") ps >>= setFrom jsonToCardType
  subs <- Json.field (Text.pack "subtypes") ps >>= setFrom jsonToSubtype
  live <- Json.field (Text.pack "rulesTextActive") ps >>= jsonToBoolDefault True
  acts <- Json.field (Text.pack "activatedAbilities") ps >>= listFrom jsonToActivatedAbility
  reps <- Json.field (Text.pack "replacementEffects") ps >>= listFrom jsonToReplacementEffect
  trigs <- Json.field (Text.pack "triggeredAbilities") ps >>= listFrom jsonToTriggeredAbility
  pure
    PC.MkProjectedCharacteristics
      { PC.keywords = kws,
        PC.colors = cols,
        PC.power = pow,
        PC.toughness = tou,
        PC.characteristicPT = cda,
        PC.cardTypes = cts,
        PC.subtypes = subs,
        PC.rulesTextActive = live,
        PC.activatedAbilities = acts,
        PC.replacementEffects = reps,
        PC.triggeredAbilities = trigs
      }

jsonToQuantityPair :: Value -> Either Text (Quantity.Quantity, Quantity.Quantity)
jsonToQuantityPair value = case value of
  Array [p, t] -> do
    p_ <- jsonToQuantity p
    t_ <- jsonToQuantity t
    pure (p_, t_)
  _ -> Left (Text.pack "expected a [power, toughness] quantity pair")

gameEventToJson :: GameEvent.GameEvent -> Value
gameEventToJson e = case e of
  GameEvent.Moved zc pc -> Json.tagged (Text.pack "Moved") (Just (Array [zoneChangeToJson zc, projectedCharacteristicsToJson pc]))
  GameEvent.DamageDealt ev -> Json.tagged (Text.pack "DamageDealt") (Just (damageEventToJson ev))
  GameEvent.StepBegan p pid -> Json.tagged (Text.pack "StepBegan") (Just (Array [phaseToJson p, playerIdToJson pid]))

jsonToGameEvent :: Value -> Either Text GameEvent.GameEvent
jsonToGameEvent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Moved", Just (Array [zc, pc])) -> GameEvent.Moved <$> jsonToZoneChange zc <*> jsonToProjectedCharacteristics pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> jsonToDamageEvent v
    ("StepBegan", Just (Array [p, pid])) -> GameEvent.StepBegan <$> jsonToPhase p <*> jsonToPlayerId pid
    _ -> Left (Text.pack "unknown GameEvent: " <> t)

-- Effect ---------------------------------------------------------------------

effectToJson :: Effect.Effect CardT.Card -> Value
effectToJson e = case e of
  Effect.DealDamage s q -> Json.tagged (Text.pack "DealDamage") (Just (Array [slotNameToJson s, quantityToJson q]))
  Effect.ModifyTarget d m s -> Json.tagged (Text.pack "ModifyTarget") (Just (Array [durationToJson d, modificationToJson m, slotNameToJson s]))
  Effect.ChangeText s -> Json.tagged (Text.pack "ChangeText") (Just (slotNameToJson s))
  Effect.AddMana mt -> Json.tagged (Text.pack "AddMana") (Just (manaTypeToJson mt))
  Effect.Search c -> Json.tagged (Text.pack "Search") (Just (cardCriterionToJson c))
  Effect.ExileAllGraveyards -> nullary (Text.pack "ExileAllGraveyards")
  Effect.ControlPlayerNextTurn s -> Json.tagged (Text.pack "ControlPlayerNextTurn") (Just (slotNameToJson s))
  Effect.Destroy s -> Json.tagged (Text.pack "Destroy") (Just (slotNameToJson s))
  Effect.Sacrifice s -> Json.tagged (Text.pack "Sacrifice") (Just (slotNameToJson s))
  Effect.Counter s -> Json.tagged (Text.pack "Counter") (Just (slotNameToJson s))
  Effect.MoveToZone s z -> Json.tagged (Text.pack "MoveToZone") (Just (Array [slotNameToJson s, zoneToJson z]))
  Effect.Draw q -> Json.tagged (Text.pack "Draw") (Just (quantityToJson q))
  Effect.Mill s q -> Json.tagged (Text.pack "Mill") (Just (Array [slotNameToJson s, quantityToJson q]))
  Effect.Discard s q -> Json.tagged (Text.pack "Discard") (Just (Array [slotNameToJson s, quantityToJson q]))
  Effect.Create q c Nothing -> Json.tagged (Text.pack "Create") (Just (Array [quantityToJson q, cardToJson c]))
  Effect.Create q c (Just s) -> Json.tagged (Text.pack "Create") (Just (Array [quantityToJson q, cardToJson c, slotNameToJson s]))
  Effect.Replace d u re -> Json.tagged (Text.pack "Replace") (Just (Array [durationToJson d, usesToJson u, replacementEffectToJson re]))
  Effect.RegenerateSelf -> nullary (Text.pack "RegenerateSelf")
  Effect.PutCounters k q s -> Json.tagged (Text.pack "PutCounters") (Just (Array [counterKindToJson k, quantityToJson q, slotNameToJson s]))
  Effect.Untap s -> Json.tagged (Text.pack "Untap") (Just (slotNameToJson s))
  Effect.GainControl d s -> Json.tagged (Text.pack "GainControl") (Just (Array [durationToJson d, slotNameToJson s]))
  Effect.ArmDelayedTrigger n -> Json.tagged (Text.pack "ArmDelayedTrigger") (Just (abilityNameToJson n))

jsonToEffect :: Value -> Either Text (Effect.Effect CardT.Card)
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
    "Sacrifice" -> withValue mv (fmap Effect.Sacrifice . jsonToSlotName)
    "Counter" -> withValue mv (fmap Effect.Counter . jsonToSlotName)
    "MoveToZone" -> case mv of
      Just (Array [s, z]) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z
      _ -> Left (Text.pack "MoveToZone expects [slot, zone]")
    "Draw" -> withValue mv (fmap Effect.Draw . jsonToQuantity)
    "Mill" -> case mv of
      Just (Array [s, q]) -> Effect.Mill <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Mill expects [slot, quantity]")
    "Discard" -> case mv of
      Just (Array [s, q]) -> Effect.Discard <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Discard expects [slot, quantity]")
    "Create" -> case mv of
      Just (Array [q, c]) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> pure Nothing
      Just (Array [q, c, s]) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> (Just <$> jsonToSlotName s)
      _ -> Left (Text.pack "Create expects [Quantity, Card] or [Quantity, Card, slot]")
    "ArmDelayedTrigger" -> withValue mv (fmap Effect.ArmDelayedTrigger . jsonToAbilityName)
    "Replace" -> case mv of
      Just (Array [d, u, re]) -> do
        duration <- jsonToDuration d
        uses <- jsonToUses u
        effect <- jsonToReplacementEffect re
        pure (Effect.Replace duration uses effect)
      _ -> Left (Text.pack "Replace expects [Duration, Uses, ReplacementEffect]")
    "RegenerateSelf" -> Right Effect.RegenerateSelf
    "PutCounters" -> case mv of
      Just (Array [k, q, s]) -> Effect.PutCounters <$> jsonToCounterKind k <*> jsonToQuantity q <*> jsonToSlotName s
      _ -> Left (Text.pack "PutCounters expects [counterKind, quantity, slot]")
    "Untap" -> withValue mv (fmap Effect.Untap . jsonToSlotName)
    "GainControl" -> case mv of
      Just (Array [d, s]) -> Effect.GainControl <$> jsonToDuration d <*> jsonToSlotName s
      _ -> Left (Text.pack "GainControl expects [duration, slot]")
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

activatedAbilityToJson :: ActivatedAbility.ActivatedAbility CardT.Card -> Value
activatedAbilityToJson aa =
  Object
    [ (Text.pack "cost", abilityCostToJson (ActivatedAbility.cost aa)),
      (Text.pack "modal", modalToJson (ActivatedAbility.modal aa))
    ]

jsonToActivatedAbility :: Value -> Either Text (ActivatedAbility.ActivatedAbility CardT.Card)
jsonToActivatedAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "cost") ps >>= jsonToAbilityCost
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal
  pure (ActivatedAbility.MkActivatedAbility c m)

replacementEffectToJson :: ReplacementEffect.ReplacementEffect -> Value
replacementEffectToJson re = case re of
  ReplacementEffect.ZoneChangeR p z ->
    Json.tagged (Text.pack "ZoneChangeR") (Just (Array [zoneChangePatternToJson p, zoneToJson z]))
  ReplacementEffect.EntryR r ->
    Json.tagged (Text.pack "EntryR") (Just (entryRewriteToJson r))
  ReplacementEffect.DamageR p r ->
    Json.tagged (Text.pack "DamageR") (Just (Array [damagePatternToJson p, damageRewriteToJson r]))
  ReplacementEffect.DestructionR r ->
    Json.tagged (Text.pack "DestructionR") (Just (destructionRewriteToJson r))
  ReplacementEffect.CounterR p s ->
    Json.tagged (Text.pack "CounterR") (Just (Array [counterPatternToJson p, scalingToJson s]))
  ReplacementEffect.TokenR p s ->
    Json.tagged (Text.pack "TokenR") (Just (Array [tokenPatternToJson p, scalingToJson s]))

jsonToReplacementEffect :: Value -> Either Text ReplacementEffect.ReplacementEffect
jsonToReplacementEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ZoneChangeR", Just (Array [p, z])) -> do
      pattern_ <- jsonToZoneChangePattern p
      dest <- jsonToZone z
      pure (ReplacementEffect.ZoneChangeR pattern_ dest)
    ("EntryR", Just v) -> fmap ReplacementEffect.EntryR (jsonToEntryRewrite v)
    ("DamageR", Just (Array [p, r])) -> do
      pattern_ <- jsonToDamagePattern p
      rewrite <- jsonToDamageRewrite r
      pure (ReplacementEffect.DamageR pattern_ rewrite)
    ("DestructionR", Just v) -> fmap ReplacementEffect.DestructionR (jsonToDestructionRewrite v)
    ("CounterR", Just (Array [p, s])) -> do
      pattern_ <- jsonToCounterPattern p
      scaling <- jsonToScaling s
      pure (ReplacementEffect.CounterR pattern_ scaling)
    ("TokenR", Just (Array [p, s])) -> do
      pattern_ <- jsonToTokenPattern p
      scaling <- jsonToScaling s
      pure (ReplacementEffect.TokenR pattern_ scaling)
    _ -> Left (Text.pack "unknown ReplacementEffect: " <> t)

triggeredAbilityToJson :: TriggeredAbility.TriggeredAbility CardT.Card -> Value
triggeredAbilityToJson ta =
  Object
    ( [ (Text.pack "condition", triggerConditionToJson (TriggeredAbility.condition ta)),
        (Text.pack "modal", modalToJson (TriggeredAbility.modal ta))
      ]
        ++ ( case TriggeredAbility.intervening ta of
               Nothing -> []
               Just c -> [(Text.pack "intervening", stateConditionToJson c)]
           )
    )

jsonToTriggeredAbility :: Value -> Either Text (TriggeredAbility.TriggeredAbility CardT.Card)
jsonToTriggeredAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "condition") ps >>= jsonToTriggerCondition
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal
  i <- maybeFrom jsonToStateCondition (getOpt (Text.pack "intervening") ps)
  pure
    TriggeredAbility.MkTriggeredAbility
      { TriggeredAbility.condition = c,
        TriggeredAbility.modal = m,
        TriggeredAbility.intervening = i
      }

abilityNameToJson :: AbilityName.AbilityName -> Value
abilityNameToJson (AbilityName.MkAbilityName t) = Json.jText t

jsonToAbilityName :: Value -> Either Text AbilityName.AbilityName
jsonToAbilityName value = AbilityName.MkAbilityName <$> Json.asText value

-- The targetSpecsToJson shape: a name-keyed map as a sorted array of entries, so
-- the render is deterministic and the file byte-stable.
delayedAbilitiesToJson :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility CardT.Card) -> Value
delayedAbilitiesToJson m =
  listTo
    (\(k, v) -> Object [(Text.pack "name", abilityNameToJson k), (Text.pack "ability", triggeredAbilityToJson v)])
    (Map.toAscList m)

jsonToDelayedAbilities :: Value -> Either Text (Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility CardT.Card))
jsonToDelayedAbilities value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "name") ps >>= jsonToAbilityName
        a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility
        pure (k, a)
   in Map.fromList <$> listFrom decodeEntry value

-- An omitted delayedAbilities field decodes to empty, so every card file that
-- predates P4 stays byte-identical (the copyOnEnter precedent).
mapFromDefault :: (Value -> Either Text (Map.Map k v)) -> Value -> Either Text (Map.Map k v)
mapFromDefault f value = case value of
  Null -> Right Map.empty
  _ -> f value

-- Runtime-only, never in card JSON -- covered for the same reason SetController's
-- PlayerId is: the codec must stay total over the transitive closure of what the
-- game state carries.
bindingToJson :: Binding.Binding -> Value
bindingToJson b =
  Object
    [ (Text.pack "target", maybeTo recipientToJson (Binding.target b)),
      (Text.pack "subtypes", maybeTo (\(f, t) -> Array [subtypeToJson f, subtypeToJson t]) (Binding.subtypes b)),
      (Text.pack "amount", maybeTo natTo (Binding.amount b)),
      (Text.pack "modes", maybeTo (setTo modeIndexToJson) (Binding.modes b)),
      (Text.pack "copy", maybeTo projectedCharacteristicsToJson (Binding.copy b))
    ]

jsonToBinding :: Value -> Either Text Binding.Binding
jsonToBinding value = do
  ps <- Json.asObject value
  t <- maybeFrom jsonToRecipient (getOpt (Text.pack "target") ps)
  s <- maybeFrom jsonToSubtypePair (getOpt (Text.pack "subtypes") ps)
  a <- maybeFrom natFrom (getOpt (Text.pack "amount") ps)
  m <- maybeFrom (setFrom jsonToModeIndex) (getOpt (Text.pack "modes") ps)
  c <- maybeFrom jsonToProjectedCharacteristics (getOpt (Text.pack "copy") ps)
  pure
    Binding.MkBinding
      { Binding.target = t,
        Binding.subtypes = s,
        Binding.amount = a,
        Binding.modes = m,
        Binding.copy = c
      }

jsonToSubtypePair :: Value -> Either Text (Subtype.Subtype, Subtype.Subtype)
jsonToSubtypePair value = case value of
  Array [f, t] -> do
    f_ <- jsonToSubtype f
    t_ <- jsonToSubtype t
    pure (f_, t_)
  _ -> Left (Text.pack "expected a [from, to] subtype pair")

bindingsToJson :: Map.Map SlotName.SlotName Binding.Binding -> Value
bindingsToJson m =
  listTo
    (\(k, v) -> Object [(Text.pack "slot", slotNameToJson k), (Text.pack "binding", bindingToJson v)])
    (Map.toAscList m)

jsonToBindings :: Value -> Either Text (Map.Map SlotName.SlotName Binding.Binding)
jsonToBindings value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "slot") ps >>= jsonToSlotName
        b <- Json.field (Text.pack "binding") ps >>= jsonToBinding
        pure (k, b)
   in Map.fromList <$> listFrom decodeEntry value

delayedTriggerToJson :: DelayedTrigger.DelayedTrigger -> Value
delayedTriggerToJson d =
  Object
    [ (Text.pack "ability", triggeredAbilityToJson (DelayedTrigger.ability d)),
      (Text.pack "source", objectIdToJson (DelayedTrigger.source d)),
      (Text.pack "controller", playerIdToJson (DelayedTrigger.controller d)),
      (Text.pack "bindings", bindingsToJson (DelayedTrigger.bindings d))
    ]

jsonToDelayedTrigger :: Value -> Either Text DelayedTrigger.DelayedTrigger
jsonToDelayedTrigger value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  c <- Json.field (Text.pack "controller") ps >>= jsonToPlayerId
  b <- Json.field (Text.pack "bindings") ps >>= jsonToBindings
  pure
    DelayedTrigger.MkDelayedTrigger
      { DelayedTrigger.ability = a,
        DelayedTrigger.source = s,
        DelayedTrigger.controller = c,
        DelayedTrigger.bindings = b
      }

-- Modal -----------------------------------------------------------------------

modeIndexToJson :: ModeIndex.ModeIndex -> Value
modeIndexToJson (ModeIndex.MkModeIndex n) = natTo n

jsonToModeIndex :: Value -> Either Text ModeIndex.ModeIndex
jsonToModeIndex value = ModeIndex.MkModeIndex <$> natFrom value

modeSelectionToJson :: ModeSelection.ModeSelection -> Value
modeSelectionToJson (ModeSelection.ChooseExactly n) =
  Json.tagged (Text.pack "ChooseExactly") (Just (natTo n))

jsonToModeSelection :: Value -> Either Text ModeSelection.ModeSelection
jsonToModeSelection value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ChooseExactly", Just n) -> ModeSelection.ChooseExactly <$> natFrom n
    _ -> Left (Text.pack "unknown ModeSelection: " <> t)

modeToJson :: Mode.Mode CardT.Card -> Value
modeToJson m =
  Object
    [ (Text.pack "effects", seqTo effectToJson (Mode.effects m)),
      (Text.pack "targetSpecs", targetSpecsToJson (Mode.targetSpecs m))
    ]

jsonToMode :: Value -> Either Text (Mode.Mode CardT.Card)
jsonToMode value = do
  ps <- Json.asObject value
  es <- Json.field (Text.pack "effects") ps >>= seqFrom jsonToEffect
  ts <- Json.field (Text.pack "targetSpecs") ps >>= jsonToTargetSpecs
  pure (Mode.MkMode es ts)

modalToJson :: Modal.Modal CardT.Card -> Value
modalToJson m =
  Object
    [ (Text.pack "modes", seqTo modeToJson (Modal.modes m)),
      (Text.pack "selection", modeSelectionToJson (Modal.selection m))
    ]

jsonToModal :: Value -> Either Text (Modal.Modal CardT.Card)
jsonToModal value = do
  ps <- Json.asObject value
  ms <- Json.field (Text.pack "modes") ps >>= seqFrom jsonToMode
  if Seq.null ms
    then Left (Text.pack "modal has no modes")
    else do
      sel <- Json.field (Text.pack "selection") ps >>= jsonToModeSelection
      pure (Modal.MkModal ms sel)

-- Card & Printing ------------------------------------------------------------

cardToJson :: CardT.Card -> Value
cardToJson c =
  Object
    ( [ (Text.pack "name", Json.jText (CardT.name c)),
        (Text.pack "manaCost", maybeTo manaCostToJson (CardT.manaCost c)),
        (Text.pack "typeLine", typeLineToJson (CardT.typeLine c)),
        (Text.pack "power", maybeTo powerToJson (CardT.power c)),
        (Text.pack "toughness", maybeTo toughnessToJson (CardT.toughness c)),
        (Text.pack "keywords", setTo keywordToJson (CardT.keywords c)),
        (Text.pack "staticAbilities", listTo staticAbilityToJson (CardT.staticAbilities c)),
        (Text.pack "spell", modalToJson (CardT.spell c)),
        (Text.pack "activatedAbilities", listTo activatedAbilityToJson (CardT.activatedAbilities c)),
        (Text.pack "replacementEffects", listTo replacementEffectToJson (CardT.replacementEffects c)),
        (Text.pack "triggeredAbilities", listTo triggeredAbilityToJson (CardT.triggeredAbilities c)),
        (Text.pack "castingPermissions", listTo castingPermissionToJson (CardT.castingPermissions c))
      ]
        ++ (if CardT.copyOnEnter c then [(Text.pack "copyOnEnter", Json.jBool True)] else [])
        ++ ( if Set.null (CardT.colorIndicator c)
               then []
               else [(Text.pack "colorIndicator", setTo colorToJson (CardT.colorIndicator c))]
           )
        ++ ( case CardT.characteristicPT c of
               Nothing -> []
               Just q -> [(Text.pack "characteristicPT", quantityToJson q)]
           )
        ++ ( if Map.null (CardT.delayedAbilities c)
               then []
               else [(Text.pack "delayedAbilities", delayedAbilitiesToJson (CardT.delayedAbilities c))]
           )
    )

getOpt :: Text -> [(Text, Value)] -> Value
getOpt k ps = Maybe.fromMaybe Null (Json.optField k ps)

jsonToBoolDefault :: Bool -> Value -> Either Text Bool
jsonToBoolDefault d value = case value of
  Null -> Right d
  Boolean b -> Right b
  _ -> Left (Text.pack "expected a boolean")

-- An omitted set field decodes to empty. Lets an all-default field stay OUT of
-- the committed JSON, so existing files remain byte-identical (the copyOnEnter
-- precedent, P2).
setFromDefault :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Set a)
setFromDefault f value = case value of
  Null -> Right Set.empty
  _ -> setFrom f value

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
  spell <- Json.field (Text.pack "spell") ps >>= jsonToModal
  activated <- Json.field (Text.pack "activatedAbilities") ps >>= listFrom jsonToActivatedAbility
  replacements <- Json.field (Text.pack "replacementEffects") ps >>= listFrom jsonToReplacementEffect
  triggered <- Json.field (Text.pack "triggeredAbilities") ps >>= listFrom jsonToTriggeredAbility
  permissions <- Json.field (Text.pack "castingPermissions") ps >>= listFrom jsonToCastingPermission
  copyOnEnter <- jsonToBoolDefault False (getOpt (Text.pack "copyOnEnter") ps)
  colorIndicator <- setFromDefault jsonToColor (getOpt (Text.pack "colorIndicator") ps)
  characteristicPT <- maybeFrom jsonToQuantity (getOpt (Text.pack "characteristicPT") ps)
  delayed <- mapFromDefault jsonToDelayedAbilities (getOpt (Text.pack "delayedAbilities") ps)
  pure
    CardT.MkCard
      { CardT.name = name,
        CardT.manaCost = manaCost,
        CardT.typeLine = typeLine,
        CardT.power = power,
        CardT.toughness = toughness,
        CardT.keywords = keywords,
        CardT.staticAbilities = statics,
        CardT.spell = spell,
        CardT.activatedAbilities = activated,
        CardT.replacementEffects = replacements,
        CardT.triggeredAbilities = triggered,
        CardT.castingPermissions = permissions,
        CardT.copyOnEnter = copyOnEnter,
        CardT.colorIndicator = colorIndicator,
        CardT.characteristicPT = characteristicPT,
        CardT.delayedAbilities = delayed
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
