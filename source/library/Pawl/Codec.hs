-- | The sole authority for @Card ⇆ Json@ (§2 of the M3.5 spec), mirroring
-- 'Pawl.Resolve' (the sole @case@-on-@Effect@ home). Free @xToJson@\/@jsonToX@
-- functions -- no type classes -- over the transitive closure of @Card@'s
-- fields. Every @Pawl.Types.*@ module stays JSON-free; casing on an effect's
-- identity here is open-half machinery, not the rules core.
module Pawl.Codec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Json as Json
import Pawl.Json.Array (Array (MkArray))
import qualified Pawl.Json.Boolean as Boolean
import Pawl.Json.Value (Value (Array, Boolean, Null, Object))
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.TokenEntry as TokenEntry
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

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
listTo f = Array . MkArray . fmap f

listFrom :: (Value -> Either Text a) -> Value -> Either Text [a]
listFrom f value = Json.asArray value >>= mapM f

-- CR 613.6's card-data invariant: a static ability has at least one part. An
-- empty array is a decode failure, not an ability that does nothing.
nonEmptyTo :: (a -> Value) -> NonEmpty.NonEmpty a -> Value
nonEmptyTo f = listTo f . NonEmpty.toList

nonEmptyFrom :: (Value -> Either Text a) -> Value -> Either Text (NonEmpty.NonEmpty a)
nonEmptyFrom f value = do
  xs <- listFrom f value
  case NonEmpty.nonEmpty xs of
    Nothing -> Left (Text.pack "expected a non-empty array")
    Just ne -> pure ne

seqTo :: (a -> Value) -> Seq.Seq a -> Value
seqTo f = Array . MkArray . fmap f . Foldable.toList

seqFrom :: (Value -> Either Text a) -> Value -> Either Text (Seq.Seq a)
seqFrom f value = Seq.fromList <$> listFrom f value

setTo :: (a -> Value) -> Set a -> Value
setTo f = listTo f . Set.toAscList

setFrom :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Set a)
setFrom f value = Set.fromList <$> listFrom f value

-- A count-per-key multiset, on the wire as a plain array WITH REPEATS rather
-- than as key/count pairs: it is what the thing being encoded is a list of, and
-- the encoding stays legible beside setTo's. Ascending by key, so it is
-- canonical. multisetFrom recounts, so a hand-written file may repeat a key in
-- any order and a zero count is simply unsayable.
multisetTo :: (a -> Value) -> Map.Map a Natural -> Value
multisetTo f = listTo f . concatMap (\(k, n) -> List.genericReplicate n k) . Map.toAscList

multisetFrom :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Map.Map a Natural)
multisetFrom f value = Map.fromListWith (+) . fmap (\k -> (k, 1)) <$> listFrom f value

maybeTo :: (a -> Value) -> Maybe a -> Value
maybeTo = maybe Json.jNull

maybeFrom :: (Value -> Either Text a) -> Value -> Either Text (Maybe a)
maybeFrom f value = case value of
  Null _ -> Right Nothing
  _ -> Just <$> f value

natTo :: Natural -> Value
natTo = Json.jInt . toInteger

natFrom :: Value -> Either Text Natural
natFrom value = do
  n <- Json.asInteger value
  case Integer.toNatural n of
    Just x -> Right x
    Nothing -> Left (Text.pack "expected natural")

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

-- No longer uniformly nullary: CR 122.1b's keyword counter carries the keyword it
-- grants, so this tags like every other payload-bearing sum here rather than
-- delegating the whole type to the nullary helper.
counterKindToJson :: CounterKind.CounterKind -> Value
counterKindToJson k = case k of
  CounterKind.PlusOnePlusOne -> nullary (Text.pack "PlusOnePlusOne")
  CounterKind.MinusOneMinusOne -> nullary (Text.pack "MinusOneMinusOne")
  CounterKind.Keyword kw -> Json.tagged (Text.pack "Keyword") (Just (keywordToJson kw))

jsonToCounterKind :: Value -> Either Text CounterKind.CounterKind
jsonToCounterKind value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("PlusOnePlusOne", _) -> Right CounterKind.PlusOnePlusOne
    ("MinusOneMinusOne", _) -> Right CounterKind.MinusOneMinusOne
    ("Keyword", Just v) -> CounterKind.Keyword <$> jsonToKeyword v
    _ -> Left (Text.pack "unknown CounterKind: " <> t)

playerCounterKindToJson :: PlayerCounterKind.PlayerCounterKind -> Value
playerCounterKindToJson k = nullary . Text.pack $ case k of
  PlayerCounterKind.Energy -> "Energy"
  PlayerCounterKind.Poison -> "Poison"

jsonToPlayerCounterKind :: Value -> Either Text PlayerCounterKind.PlayerCounterKind
jsonToPlayerCounterKind =
  decodeNullary
    (Text.pack "PlayerCounterKind")
    [ (Text.pack "Energy", PlayerCounterKind.Energy),
      (Text.pack "Poison", PlayerCounterKind.Poison)
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
  Subtype.Fungus -> "Fungus"
  Subtype.Elemental -> "Elemental"
  Subtype.Rogue -> "Rogue"
  Subtype.Hag -> "Hag"
  Subtype.Warlock -> "Warlock"
  Subtype.Soldier -> "Soldier"
  Subtype.Phyrexian -> "Phyrexian"
  Subtype.Elf -> "Elf"
  Subtype.Nightmare -> "Nightmare"
  Subtype.Horse -> "Horse"
  Subtype.Aura -> "Aura"
  Subtype.Equipment -> "Equipment"
  Subtype.Scout -> "Scout"
  Subtype.Artificer -> "Artificer"
  Subtype.Troll -> "Troll"
  Subtype.Nomad -> "Nomad"
  Subtype.Shaman -> "Shaman"
  Subtype.Demon -> "Demon"
  Subtype.Cleric -> "Cleric"
  Subtype.Illusion -> "Illusion"
  Subtype.Spirit -> "Spirit"
  Subtype.Angel -> "Angel"
  Subtype.Insect -> "Insect"
  Subtype.Berserker -> "Berserker"
  Subtype.Thopter -> "Thopter"
  Subtype.Dragon -> "Dragon"
  Subtype.Unicorn -> "Unicorn"

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
      (Text.pack "Zombie", Subtype.Zombie),
      (Text.pack "Fungus", Subtype.Fungus),
      (Text.pack "Elemental", Subtype.Elemental),
      (Text.pack "Rogue", Subtype.Rogue),
      (Text.pack "Hag", Subtype.Hag),
      (Text.pack "Warlock", Subtype.Warlock),
      (Text.pack "Soldier", Subtype.Soldier),
      (Text.pack "Phyrexian", Subtype.Phyrexian),
      (Text.pack "Elf", Subtype.Elf),
      (Text.pack "Nightmare", Subtype.Nightmare),
      (Text.pack "Horse", Subtype.Horse),
      (Text.pack "Aura", Subtype.Aura),
      (Text.pack "Equipment", Subtype.Equipment),
      (Text.pack "Scout", Subtype.Scout),
      (Text.pack "Artificer", Subtype.Artificer),
      (Text.pack "Troll", Subtype.Troll),
      (Text.pack "Nomad", Subtype.Nomad),
      (Text.pack "Shaman", Subtype.Shaman),
      (Text.pack "Demon", Subtype.Demon),
      (Text.pack "Cleric", Subtype.Cleric),
      (Text.pack "Illusion", Subtype.Illusion),
      (Text.pack "Spirit", Subtype.Spirit),
      (Text.pack "Angel", Subtype.Angel),
      (Text.pack "Insect", Subtype.Insect),
      (Text.pack "Berserker", Subtype.Berserker),
      (Text.pack "Thopter", Subtype.Thopter),
      (Text.pack "Dragon", Subtype.Dragon),
      (Text.pack "Unicorn", Subtype.Unicorn)
    ]

-- CR 702.29e's typecycling filter, absent for plain cycling: null rather than an
-- omitted key, because this rides inside a positional pair.
optionalFilter :: Value -> Either Text (Maybe Filter.Filter)
optionalFilter value = case value of
  Null _ -> Right Nothing
  _ -> fmap Just (jsonToFilter value)

searchDestinationToJson :: SearchDestination.SearchDestination -> Value
searchDestinationToJson d = nullary . Text.pack $ case d of
  SearchDestination.BattlefieldTapped -> "BattlefieldTapped"
  SearchDestination.RevealThenHand -> "RevealThenHand"

jsonToSearchDestination :: Value -> Either Text SearchDestination.SearchDestination
jsonToSearchDestination =
  decodeNullary
    (Text.pack "SearchDestination")
    [ (Text.pack "BattlefieldTapped", SearchDestination.BattlefieldTapped),
      (Text.pack "RevealThenHand", SearchDestination.RevealThenHand)
    ]

supertypeToJson :: Supertype.Supertype -> Value
supertypeToJson s = nullary . Text.pack $ case s of
  Supertype.Basic -> "Basic"
  Supertype.Legendary -> "Legendary"
  Supertype.World -> "World"

jsonToSupertype :: Value -> Either Text Supertype.Supertype
jsonToSupertype =
  decodeNullary
    (Text.pack "Supertype")
    [ (Text.pack "Basic", Supertype.Basic),
      (Text.pack "Legendary", Supertype.Legendary),
      (Text.pack "World", Supertype.World)
    ]

-- Not decodeNullary's table shape any more: CR 702.164a's toxic and CR 702.70a's
-- poisonous each carry an N, so this is the tagged-with-an-optional-payload case
-- jsonToQuantity uses.
keywordToJson :: Keyword.Keyword -> Value
keywordToJson k = case k of
  Keyword.Deathtouch -> nullary (Text.pack "Deathtouch")
  Keyword.Defender -> nullary (Text.pack "Defender")
  Keyword.DoubleStrike -> nullary (Text.pack "DoubleStrike")
  Keyword.FirstStrike -> nullary (Text.pack "FirstStrike")
  Keyword.Flying -> nullary (Text.pack "Flying")
  Keyword.Haste -> nullary (Text.pack "Haste")
  Keyword.Indestructible -> nullary (Text.pack "Indestructible")
  Keyword.Reach -> nullary (Text.pack "Reach")
  Keyword.Trample -> nullary (Text.pack "Trample")
  Keyword.Vigilance -> nullary (Text.pack "Vigilance")
  Keyword.Cycling cost searchFor -> Json.tagged (Text.pack "Cycling") (Just (Array (MkArray [costToJson cost, maybe Json.jNull filterToJson searchFor])))
  Keyword.Flashback cost -> Json.tagged (Text.pack "Flashback") (Just (costToJson cost))
  Keyword.Fear -> nullary (Text.pack "Fear")
  Keyword.Poisonous n -> Json.tagged (Text.pack "Poisonous") (Just (natTo n))
  Keyword.Infect -> nullary (Text.pack "Infect")
  Keyword.Devoid -> nullary (Text.pack "Devoid")
  Keyword.Toxic n -> Json.tagged (Text.pack "Toxic") (Just (natTo n))

jsonToKeyword :: Value -> Either Text Keyword.Keyword
jsonToKeyword value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Deathtouch", _) -> Right Keyword.Deathtouch
    ("Defender", _) -> Right Keyword.Defender
    ("DoubleStrike", _) -> Right Keyword.DoubleStrike
    ("FirstStrike", _) -> Right Keyword.FirstStrike
    ("Flying", _) -> Right Keyword.Flying
    ("Haste", _) -> Right Keyword.Haste
    ("Indestructible", _) -> Right Keyword.Indestructible
    ("Reach", _) -> Right Keyword.Reach
    ("Trample", _) -> Right Keyword.Trample
    ("Vigilance", _) -> Right Keyword.Vigilance
    ("Cycling", Just (Array (MkArray [c, f]))) -> Keyword.Cycling <$> jsonToCost c <*> optionalFilter f
    ("Flashback", Just v) -> Keyword.Flashback <$> jsonToCost v
    ("Fear", _) -> Right Keyword.Fear
    ("Poisonous", Just v) -> Keyword.Poisonous <$> natFrom v
    ("Infect", _) -> Right Keyword.Infect
    ("Devoid", _) -> Right Keyword.Devoid
    ("Toxic", Just v) -> Keyword.Toxic <$> natFrom v
    _ -> Left (Text.pack "unknown Keyword: " <> t)

zoneToJson :: Zone.Zone -> Value
zoneToJson z = nullary . Text.pack $ case z of
  Zone.Library -> "Library"
  Zone.Hand -> "Hand"
  Zone.Graveyard -> "Graveyard"
  Zone.Battlefield -> "Battlefield"
  Zone.Stack -> "Stack"
  Zone.Exile -> "Exile"
  Zone.Command -> "Command"

jsonToZone :: Value -> Either Text Zone.Zone
jsonToZone =
  decodeNullary
    (Text.pack "Zone")
    [ (Text.pack "Library", Zone.Library),
      (Text.pack "Hand", Zone.Hand),
      (Text.pack "Graveyard", Zone.Graveyard),
      (Text.pack "Battlefield", Zone.Battlefield),
      (Text.pack "Stack", Zone.Stack),
      (Text.pack "Exile", Zone.Exile),
      (Text.pack "Command", Zone.Command)
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
durationToJson d = case d of
  Duration.UntilEndOfTurn -> nullary (Text.pack "UntilEndOfTurn")
  Duration.Indefinite -> nullary (Text.pack "Indefinite")
  Duration.UntilYourNextTurn -> nullary (Text.pack "UntilYourNextTurn")
  Duration.ForAsLongAs c -> Json.tagged (Text.pack "ForAsLongAs") (Just (conditionToJson c))

jsonToDuration :: Value -> Either Text Duration.Duration
jsonToDuration value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("UntilEndOfTurn", _) -> Right Duration.UntilEndOfTurn
    ("Indefinite", _) -> Right Duration.Indefinite
    ("UntilYourNextTurn", _) -> Right Duration.UntilYourNextTurn
    ("ForAsLongAs", Just v) -> Duration.ForAsLongAs <$> jsonToCondition v
    _ -> Left (Text.pack "unknown Duration: " <> t)

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
  ControllerRelation.Opponents -> "Opponents"

jsonToControllerRelation :: Value -> Either Text ControllerRelation.ControllerRelation
jsonToControllerRelation =
  decodeNullary
    (Text.pack "ControllerRelation")
    [ (Text.pack "Yours", ControllerRelation.Yours),
      (Text.pack "Anyones", ControllerRelation.Anyones),
      (Text.pack "Opponents", ControllerRelation.Opponents)
    ]

playerRelationToJson :: PlayerRelation.PlayerRelation -> Value
playerRelationToJson r = nullary . Text.pack $ case r of
  PlayerRelation.You -> "You"
  PlayerRelation.Opponent -> "Opponent"

jsonToPlayerRelation :: Value -> Either Text PlayerRelation.PlayerRelation
jsonToPlayerRelation =
  decodeNullary
    (Text.pack "PlayerRelation")
    [ (Text.pack "You", PlayerRelation.You),
      (Text.pack "Opponent", PlayerRelation.Opponent)
    ]

-- Recursive, mirroring quantityToJson/jsonToQuantity: And/Or carry their
-- operands as a JSON Array, Not as a single nested object, and each atom
-- delegates to the leaf-enum codec for the characteristic it cases on.
filterToJson :: Filter.Filter -> Value
filterToJson filter_ = case filter_ of
  Filter.HasCardType t -> Json.tagged (Text.pack "HasCardType") (Just (cardTypeToJson t))
  Filter.HasSupertype s -> Json.tagged (Text.pack "HasSupertype") (Just (supertypeToJson s))
  Filter.HasColor c -> Json.tagged (Text.pack "HasColor") (Just (colorToJson c))
  Filter.HasSubtype s -> Json.tagged (Text.pack "HasSubtype") (Just (subtypeToJson s))
  Filter.PowerAtLeast n -> Json.tagged (Text.pack "PowerAtLeast") (Just (Json.jInt n))
  Filter.ControlledBy r -> Json.tagged (Text.pack "ControlledBy") (Just (playerRelationToJson r))
  Filter.IsPlayer r -> Json.tagged (Text.pack "IsPlayer") (Just (playerRelationToJson r))
  Filter.IsSource -> nullary (Text.pack "IsSource")
  Filter.IsAttacking -> nullary (Text.pack "IsAttacking")
  Filter.IsBlocking -> nullary (Text.pack "IsBlocking")
  Filter.AttackedThisTurn -> nullary (Text.pack "AttackedThisTurn")
  Filter.IsAttachedToCreature -> nullary (Text.pack "IsAttachedToCreature")
  Filter.IsToken -> nullary (Text.pack "IsToken")
  Filter.And fs -> Json.tagged (Text.pack "And") (Just (Array (MkArray (fmap filterToJson fs))))
  Filter.Or fs -> Json.tagged (Text.pack "Or") (Just (Array (MkArray (fmap filterToJson fs))))
  Filter.Not f -> Json.tagged (Text.pack "Not") (Just (filterToJson f))

jsonToFilter :: Value -> Either Text Filter.Filter
jsonToFilter value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("HasCardType", Just v) -> Filter.HasCardType <$> jsonToCardType v
    ("HasSupertype", Just v) -> Filter.HasSupertype <$> jsonToSupertype v
    ("HasColor", Just v) -> Filter.HasColor <$> jsonToColor v
    ("HasSubtype", Just v) -> Filter.HasSubtype <$> jsonToSubtype v
    ("PowerAtLeast", Just v) -> Filter.PowerAtLeast <$> Json.asInteger v
    ("ControlledBy", Just v) -> Filter.ControlledBy <$> jsonToPlayerRelation v
    ("IsPlayer", Just v) -> Filter.IsPlayer <$> jsonToPlayerRelation v
    ("IsSource", _) -> Right Filter.IsSource
    ("IsAttacking", _) -> Right Filter.IsAttacking
    ("IsBlocking", _) -> Right Filter.IsBlocking
    ("AttackedThisTurn", _) -> Right Filter.AttackedThisTurn
    ("IsAttachedToCreature", _) -> Right Filter.IsAttachedToCreature
    ("IsToken", _) -> Right Filter.IsToken
    ("And", Just (Array (MkArray vs))) -> Filter.And <$> traverse jsonToFilter vs
    ("Or", Just (Array (MkArray vs))) -> Filter.Or <$> traverse jsonToFilter vs
    ("Not", Just v) -> Filter.Not <$> jsonToFilter v
    _ -> Left (Text.pack "unknown Filter: " <> t)

playerRefToJson :: PlayerRef.PlayerRef -> Value
playerRefToJson r = case r of
  PlayerRef.EachPlayer -> nullary (Text.pack "EachPlayer")
  PlayerRef.Relative rel -> Json.tagged (Text.pack "Relative") (Just (playerRelationToJson rel))
  PlayerRef.InSlot n -> Json.tagged (Text.pack "InSlot") (Just (slotNameToJson n))

jsonToPlayerRef :: Value -> Either Text PlayerRef.PlayerRef
jsonToPlayerRef value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("EachPlayer", _) -> Right PlayerRef.EachPlayer
    ("Relative", Just v) -> PlayerRef.Relative <$> jsonToPlayerRelation v
    ("InSlot", Just v) -> PlayerRef.InSlot <$> jsonToSlotName v
    _ -> Left (Text.pack "unknown PlayerRef: " <> t)

-- An ObjectRef is told apart by JSON TYPE rather than by a tag, the shape
-- Effect.Create's optional TokenEntry already uses: a slot name is a string
-- (slotNameToJson) and a Filter is an object, so the two can never be confused.
-- Untagged on purpose -- `"target"` is what an object-affecting effect has always
-- written, and it goes on meaning the one slot it always meant.
objectRefToJson :: ObjectRef.ObjectRef -> Value
objectRefToJson r = case r of
  ObjectRef.InSlot n -> slotNameToJson n
  ObjectRef.EachMatching f -> filterToJson f

jsonToObjectRef :: Value -> Either Text ObjectRef.ObjectRef
jsonToObjectRef value = case value of
  Object _ -> ObjectRef.EachMatching <$> jsonToFilter value
  _ -> ObjectRef.InSlot <$> jsonToSlotName value

eventShapeToJson :: EventShape.EventShape -> Value
eventShapeToJson s = case s of
  EventShape.MovedBetween from to -> Json.tagged (Text.pack "MovedBetween") (Just (Array (MkArray [zoneToJson from, zoneToJson to])))

jsonToEventShape :: Value -> Either Text EventShape.EventShape
jsonToEventShape value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("MovedBetween", Just (Array (MkArray [f, u]))) -> EventShape.MovedBetween <$> jsonToZone f <*> jsonToZone u
    _ -> Left (Text.pack "unknown EventShape: " <> t)

scopeToJson :: Scope.Scope -> Value
scopeToJson s = case s of
  Scope.InZone z r -> Json.tagged (Text.pack "InZone") (Just (Array (MkArray [zoneToJson z, playerRefToJson r])))
  Scope.InHistory e -> Json.tagged (Text.pack "InHistory") (Just (eventShapeToJson e))

jsonToScope :: Value -> Either Text Scope.Scope
jsonToScope value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("InZone", Just (Array (MkArray [z, r]))) -> Scope.InZone <$> jsonToZone z <*> jsonToPlayerRef r
    ("InHistory", Just v) -> Scope.InHistory <$> jsonToEventShape v
    _ -> Left (Text.pack "unknown Scope: " <> t)

-- No longer wholly nullary, and so no longer decodeNullary's shape: Greatest
-- carries the per-member Quantity it reads.
aggregationToJson :: Aggregation.Aggregation Quantity.Quantity -> Value
aggregationToJson a = case a of
  Aggregation.Objects -> nullary (Text.pack "Objects")
  Aggregation.DistinctCardTypes -> nullary (Text.pack "DistinctCardTypes")
  Aggregation.Greatest q -> Json.tagged (Text.pack "Greatest") (Just (quantityToJson q))

jsonToAggregation :: Value -> Either Text (Aggregation.Aggregation Quantity.Quantity)
jsonToAggregation value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Objects", Nothing) -> Right Aggregation.Objects
    ("DistinctCardTypes", Nothing) -> Right Aggregation.DistinctCardTypes
    ("Greatest", Just v) -> Aggregation.Greatest <$> jsonToQuantity v
    _ -> Left (Text.pack "unknown Aggregation: " <> t)

comparisonToJson :: Comparison.Comparison -> Value
comparisonToJson c = nullary . Text.pack $ case c of
  Comparison.Exactly -> "Exactly"
  Comparison.AtLeast -> "AtLeast"
  Comparison.AtMost -> "AtMost"

jsonToComparison :: Value -> Either Text Comparison.Comparison
jsonToComparison =
  decodeNullary
    (Text.pack "Comparison")
    [ (Text.pack "Exactly", Comparison.Exactly),
      (Text.pack "AtLeast", Comparison.AtLeast),
      (Text.pack "AtMost", Comparison.AtMost)
    ]

countToJson :: Count.Type.Count Quantity.Quantity -> Value
countToJson (Count.Type.MkCount s f a) =
  Json.tagged (Text.pack "Count") (Just (Array (MkArray [scopeToJson s, filterToJson f, aggregationToJson a])))

jsonToCount :: Value -> Either Text (Count.Type.Count Quantity.Quantity)
jsonToCount value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Count", Just (Array (MkArray [s, f, a]))) -> Count.Type.MkCount <$> jsonToScope s <*> jsonToFilter f <*> jsonToAggregation a
    _ -> Left (Text.pack "unknown Count: " <> t)

-- Both sides go through quantityToJson, and that is BACKWARD COMPATIBLE with
-- the Count-on-the-left shape rather than merely similar to it: quantityToJson's
-- Count arm delegates to countToJson and emits no wrapper of its own, so a
-- `Quantity.Count c` is byte-for-byte the JSON `countToJson c` used to produce.
-- Every committed card file that carries a condition therefore round-trips
-- untouched.
conditionToJson :: Condition.Type.Condition -> Value
conditionToJson (Condition.Type.MkCondition m cmp q) =
  Json.tagged (Text.pack "Condition") (Just (Array (MkArray [quantityToJson m, comparisonToJson cmp, quantityToJson q])))

jsonToCondition :: Value -> Either Text Condition.Type.Condition
jsonToCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Condition", Just (Array (MkArray [m, cmp, q]))) -> Condition.Type.MkCondition <$> jsonToQuantity m <*> jsonToComparison cmp <*> jsonToQuantity q
    _ -> Left (Text.pack "unknown Condition: " <> t)

playerScopeToJson :: PlayerScope.PlayerScope -> Value
playerScopeToJson s = nullary . Text.pack $ case s of
  PlayerScope.You -> "You"
  PlayerScope.Opponents -> "Opponents"
  PlayerScope.EachPlayer -> "EachPlayer"

jsonToPlayerScope :: Value -> Either Text PlayerScope.PlayerScope
jsonToPlayerScope =
  decodeNullary
    (Text.pack "PlayerScope")
    [ (Text.pack "You", PlayerScope.You),
      (Text.pack "Opponents", PlayerScope.Opponents),
      (Text.pack "EachPlayer", PlayerScope.EachPlayer)
    ]

playerEffectToJson :: PlayerEffect.PlayerEffect -> Value
playerEffectToJson e = case e of
  PlayerEffect.CantCastSpells -> nullary (Text.pack "CantCastSpells")
  PlayerEffect.CantCastMoreThan n -> Json.tagged (Text.pack "CantCastMoreThan") (Just (natTo n))
  PlayerEffect.IncreaseSpellCost c n -> Json.tagged (Text.pack "IncreaseSpellCost") (Just (Array (MkArray [filterToJson c, natTo n])))
  PlayerEffect.ReduceSpellCost c m -> Json.tagged (Text.pack "ReduceSpellCost") (Just (Array (MkArray [filterToJson c, manaCostToJson m])))
  PlayerEffect.NoMaximumHandSize -> nullary (Text.pack "NoMaximumHandSize")
  PlayerEffect.DontLoseUnspentMana -> nullary (Text.pack "DontLoseUnspentMana")

jsonToPlayerEffect :: Value -> Either Text PlayerEffect.PlayerEffect
jsonToPlayerEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("CantCastSpells", _) -> Right PlayerEffect.CantCastSpells
    ("CantCastMoreThan", Just v) -> PlayerEffect.CantCastMoreThan <$> natFrom v
    ("IncreaseSpellCost", Just (Array (MkArray [c, n]))) -> PlayerEffect.IncreaseSpellCost <$> jsonToFilter c <*> natFrom n
    ("ReduceSpellCost", Just (Array (MkArray [c, m]))) -> PlayerEffect.ReduceSpellCost <$> jsonToFilter c <*> jsonToManaCost m
    ("NoMaximumHandSize", _) -> Right PlayerEffect.NoMaximumHandSize
    ("DontLoseUnspentMana", _) -> Right PlayerEffect.DontLoseUnspentMana
    _ -> Left (Text.pack "unknown PlayerEffect: " <> t)

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
  Json.jObject
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
  Json.jObject
    [ (Text.pack "whenDestination", zoneToJson (ZoneChangePattern.whenDestination p)),
      (Text.pack "whichObject", zoneChangeSubjectToJson (ZoneChangePattern.whichObject p)),
      (Text.pack "whoseObject", controllerRelationToJson (ZoneChangePattern.whoseObject p))
    ]

jsonToZoneChangePattern :: Value -> Either Text ZoneChangePattern.ZoneChangePattern
jsonToZoneChangePattern value = do
  ps <- Json.asObject value
  d <- Json.field (Text.pack "whenDestination") ps >>= jsonToZone
  s <- Json.field (Text.pack "whichObject") ps >>= jsonToZoneChangeSubject
  w <- Json.field (Text.pack "whoseObject") ps >>= jsonToControllerRelation
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = d,
        ZoneChangePattern.whichObject = s,
        ZoneChangePattern.whoseObject = w
      }

zoneChangeSubjectToJson :: ZoneChangeSubject.ZoneChangeSubject -> Value
zoneChangeSubjectToJson s = nullary . Text.pack $ case s of
  ZoneChangeSubject.AnyObject -> "AnyObject"
  ZoneChangeSubject.TheSource -> "TheSource"

jsonToZoneChangeSubject :: Value -> Either Text ZoneChangeSubject.ZoneChangeSubject
jsonToZoneChangeSubject =
  decodeNullary
    (Text.pack "ZoneChangeSubject")
    [ (Text.pack "AnyObject", ZoneChangeSubject.AnyObject),
      (Text.pack "TheSource", ZoneChangeSubject.TheSource)
    ]

counterPatternToJson :: CounterPattern.CounterPattern -> Value
counterPatternToJson p =
  Json.jObject
    [ (Text.pack "whichKind", maybeTo counterKindToJson (CounterPattern.whichKind p)),
      (Text.pack "whose", controllerRelationToJson (CounterPattern.whose p)),
      (Text.pack "onWhat", filterToJson (CounterPattern.onWhat p))
    ]

jsonToCounterPattern :: Value -> Either Text CounterPattern.CounterPattern
jsonToCounterPattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= maybeFrom jsonToCounterKind
  w <- Json.field (Text.pack "whose") ps >>= jsonToControllerRelation
  o <- Json.field (Text.pack "onWhat") ps >>= jsonToFilter
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = k,
        CounterPattern.whose = w,
        CounterPattern.onWhat = o
      }

tokenPatternToJson :: TokenPattern.TokenPattern -> Value
tokenPatternToJson p =
  Json.jObject [(Text.pack "whose", controllerRelationToJson (TokenPattern.whose p))]

jsonToTokenPattern :: Value -> Either Text TokenPattern.TokenPattern
jsonToTokenPattern value = do
  ps <- Json.asObject value
  w <- Json.field (Text.pack "whose") ps >>= jsonToControllerRelation
  pure TokenPattern.MkTokenPattern {TokenPattern.whose = w}

-- `whosePhase` is meant to be runtime-only -- a player-scoped skip is baked by
-- Resolve's SkipNextPhase arm, not authored on a card -- but the codec must stay
-- total, so this accepts one from card JSON and a lint owes the pool the check
-- (#437). Same treatment, and same reason, as SetController's PlayerId above.
phasePatternToJson :: PhasePattern.PhasePattern -> Value
phasePatternToJson p =
  Json.jObject
    [ (Text.pack "whichPhase", phaseToJson (PhasePattern.whichPhase p)),
      (Text.pack "whosePhase", maybeTo playerIdToJson (PhasePattern.whosePhase p))
    ]

jsonToPhasePattern :: Value -> Either Text PhasePattern.PhasePattern
jsonToPhasePattern value = do
  ps <- Json.asObject value
  p <- Json.field (Text.pack "whichPhase") ps >>= jsonToPhase
  w <- Json.field (Text.pack "whosePhase") ps >>= maybeFrom jsonToPlayerId
  pure PhasePattern.MkPhasePattern {PhasePattern.whichPhase = p, PhasePattern.whosePhase = w}

damagePatternToJson :: DamagePattern.DamagePattern -> Value
damagePatternToJson p =
  Json.jObject [(Text.pack "whichKind", maybeTo damageKindToJson (DamagePattern.whichKind p))]

jsonToDamagePattern :: Value -> Either Text DamagePattern.DamagePattern
jsonToDamagePattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= maybeFrom jsonToDamageKind
  pure DamagePattern.MkDamagePattern {DamagePattern.whichKind = k}

-- Tagged rather than bare-nullary from the start: this family grows
-- payload-carrying constructors (PayLife, Sacrifice), so the decoder is written
-- against Json.tag and only gains arms.
costComponentToJson :: CostComponent.CostComponent -> Value
costComponentToJson c = case c of
  CostComponent.TapThis -> nullary (Text.pack "TapThis")
  CostComponent.UntapThis -> nullary (Text.pack "UntapThis")
  CostComponent.SacrificeThis -> nullary (Text.pack "SacrificeThis")
  CostComponent.PayLife n -> Json.tagged (Text.pack "PayLife") (Just (natTo n))
  CostComponent.Sacrifice n c_ -> Json.tagged (Text.pack "Sacrifice") (Just (Array (MkArray [natTo n, filterToJson c_])))
  CostComponent.DiscardCards n -> Json.tagged (Text.pack "DiscardCards") (Just (natTo n))
  CostComponent.DiscardThis -> nullary (Text.pack "DiscardThis")
  CostComponent.PayEnergy n -> Json.tagged (Text.pack "PayEnergy") (Just (natTo n))

jsonToCostComponent :: Value -> Either Text CostComponent.CostComponent
jsonToCostComponent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("TapThis", _) -> Right CostComponent.TapThis
    ("UntapThis", _) -> Right CostComponent.UntapThis
    ("SacrificeThis", _) -> Right CostComponent.SacrificeThis
    ("PayLife", Just v) -> fmap CostComponent.PayLife (natFrom v)
    ("Sacrifice", Just (Array (MkArray [n, c_]))) -> do
      count <- natFrom n
      filter_ <- jsonToFilter c_
      pure (CostComponent.Sacrifice count filter_)
    ("DiscardCards", Just v) -> fmap CostComponent.DiscardCards (natFrom v)
    ("DiscardThis", _) -> Right CostComponent.DiscardThis
    ("PayEnergy", Just v) -> fmap CostComponent.PayEnergy (natFrom v)
    _ -> Left (Text.pack "unknown CostComponent: " <> t)

poolToJson :: Pool.Pool -> Value
poolToJson p = nullary . Text.pack $ case p of
  Pool.Creatures -> "Creatures"
  Pool.Players -> "Players"
  Pool.AnyTarget -> "AnyTarget"
  Pool.Permanents -> "Permanents"
  Pool.Spells -> "Spells"
  Pool.SpellsAndPermanents -> "SpellsAndPermanents"

jsonToPool :: Value -> Either Text Pool.Pool
jsonToPool =
  decodeNullary
    (Text.pack "Pool")
    [ (Text.pack "Creatures", Pool.Creatures),
      (Text.pack "Players", Pool.Players),
      (Text.pack "AnyTarget", Pool.AnyTarget),
      (Text.pack "Permanents", Pool.Permanents),
      (Text.pack "Spells", Pool.Spells),
      (Text.pack "SpellsAndPermanents", Pool.SpellsAndPermanents)
    ]

-- The product shape: {"pool": <pool>, "filter": <filter | omitted>}. The filter
-- key is omitted when Nothing (a bare "target creature" narrows nothing),
-- mirroring how optional fields are encoded elsewhere. CR 601.2c's "another" is
-- a Not IsSource conjunct inside that filter, not a key of its own (#163).
targetSpecToJson :: TargetSpec.TargetSpec -> Value
targetSpecToJson (TargetSpec.MkTargetSpec pool restriction) =
  let base = [(Text.pack "pool", poolToJson pool)]
      withFilter = case restriction of
        Nothing -> base
        Just f -> base <> [(Text.pack "filter", filterToJson f)]
   in Json.jObject withFilter

jsonToTargetSpec :: Value -> Either Text TargetSpec.TargetSpec
jsonToTargetSpec value = do
  ps <- Json.asObject value
  pool <- Json.field (Text.pack "pool") ps >>= jsonToPool
  restriction <- case Json.optField (Text.pack "filter") ps of
    Nothing -> Right Nothing
    Just v -> Just <$> jsonToFilter v
  pure (TargetSpec.MkTargetSpec pool restriction)

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
  TriggerCondition.PermanentEnters f -> Json.tagged (Text.pack "PermanentEnters") (Just (filterToJson f))
  TriggerCondition.StepBegins p s -> Json.tagged (Text.pack "StepBegins") (Just (Array (MkArray [phaseToJson p, turnScopeToJson s])))
  TriggerCondition.StateIs c2 -> Json.tagged (Text.pack "StateIs") (Just (conditionToJson c2))
  TriggerCondition.SelfDealsCombatDamageToPlayer -> nullary (Text.pack "SelfDealsCombatDamageToPlayer")
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> nullary (Text.pack "CreatureDealtCombatDamageToMonarch")
  TriggerCondition.SelfAttacks f -> Json.tagged (Text.pack "SelfAttacks") (Just (triggerFrequencyToJson f))
  TriggerCondition.SelfCycled -> nullary (Text.pack "SelfCycled")
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> nullary (Text.pack "SelfPutIntoGraveyardFromLibrary")
  TriggerCondition.SelfDies -> nullary (Text.pack "SelfDies")

jsonToTriggerCondition :: Value -> Either Text TriggerCondition.TriggerCondition
jsonToTriggerCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("PermanentEnters", Just v) -> TriggerCondition.PermanentEnters <$> jsonToFilter v
    ("StepBegins", Just (Array (MkArray [p, s]))) -> TriggerCondition.StepBegins <$> jsonToPhase p <*> jsonToTurnScope s
    ("StateIs", Just v) -> TriggerCondition.StateIs <$> jsonToCondition v
    ("SelfDealsCombatDamageToPlayer", _) -> Right TriggerCondition.SelfDealsCombatDamageToPlayer
    ("CreatureDealtCombatDamageToMonarch", _) -> Right TriggerCondition.CreatureDealtCombatDamageToMonarch
    ("SelfAttacks", Just v) -> TriggerCondition.SelfAttacks <$> jsonToTriggerFrequency v
    ("SelfCycled", _) -> Right TriggerCondition.SelfCycled
    ("SelfPutIntoGraveyardFromLibrary", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromLibrary
    ("SelfDies", _) -> Right TriggerCondition.SelfDies
    _ -> Left (Text.pack "unknown TriggerCondition: " <> t)

castingPermissionToJson :: CastingPermission.CastingPermission -> Value
castingPermissionToJson c = nullary . Text.pack $ case c of
  CastingPermission.CastFromLibraryWhileSearching -> "CastFromLibraryWhileSearching"
  CastingPermission.CastFromGraveyard -> "CastFromGraveyard"

jsonToCastingPermission :: Value -> Either Text CastingPermission.CastingPermission
jsonToCastingPermission =
  decodeNullary
    (Text.pack "CastingPermission")
    [ (Text.pack "CastFromLibraryWhileSearching", CastingPermission.CastFromLibraryWhileSearching),
      (Text.pack "CastFromGraveyard", CastingPermission.CastFromGraveyard)
    ]

-- Newtypes -------------------------------------------------------------------

slotNameToJson :: SlotName.SlotName -> Value
slotNameToJson (SlotName.MkSlotName t) = Json.jText t

jsonToSlotName :: Value -> Either Text SlotName.SlotName
jsonToSlotName value = SlotName.MkSlotName <$> Json.asText value

objectIdToJson :: ObjectId.ObjectId -> Value
objectIdToJson (ObjectId.MkObjectId n) = natTo n

jsonToObjectId :: Value -> Either Text ObjectId.ObjectId
jsonToObjectId value = ObjectId.MkObjectId <$> natFrom value

-- SetController's PlayerId is meant to be runtime-only (a SetController effect
-- is baked at GainControl resolution, not authored on a card), but the codec
-- must stay total, so this arm ACCEPTS one from card JSON and Pawl.CardSpec
-- lints the pool against it instead (#199). Mirrors ObjectId's Natural encoding
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

regenerabilityToJson :: Regenerability.Regenerability -> Value
regenerabilityToJson r = nullary . Text.pack $ case r of
  Regenerability.Regenerable -> "Regenerable"
  Regenerability.CantBeRegenerated -> "CantBeRegenerated"

triggerFrequencyToJson :: TriggerFrequency.TriggerFrequency -> Value
triggerFrequencyToJson f = nullary . Text.pack $ case f of
  TriggerFrequency.EveryTime -> "EveryTime"
  TriggerFrequency.FirstTimeEachTurn -> "FirstTimeEachTurn"

extraPhaseToJson :: ExtraPhase.ExtraPhase -> Value
extraPhaseToJson e = nullary . Text.pack $ case e of
  ExtraPhase.ExtraCombat -> "ExtraCombat"
  ExtraPhase.ExtraMain -> "ExtraMain"

counterabilityToJson :: Counterability.Counterability -> Value
counterabilityToJson c = nullary . Text.pack $ case c of
  Counterability.Counterable -> "Counterable"
  Counterability.CantBeCountered -> "CantBeCountered"

-- Absent means Counterable (CR 113.6g is printed text: a card either says it or
-- does not), the shape jsonToBoolDefault gives the other defaulted keys.
jsonToCounterabilityDefault :: Value -> Either Text Counterability.Counterability
jsonToCounterabilityDefault value = case value of
  Null _ -> Right Counterability.Counterable
  _ -> jsonToCounterability value

jsonToCounterability :: Value -> Either Text Counterability.Counterability
jsonToCounterability =
  decodeNullary
    (Text.pack "Counterability")
    [ (Text.pack "Counterable", Counterability.Counterable),
      (Text.pack "CantBeCountered", Counterability.CantBeCountered)
    ]

jsonToRegenerability :: Value -> Either Text Regenerability.Regenerability
jsonToRegenerability =
  decodeNullary
    (Text.pack "Regenerability")
    [ (Text.pack "Regenerable", Regenerability.Regenerable),
      (Text.pack "CantBeRegenerated", Regenerability.CantBeRegenerated)
    ]

jsonToTriggerFrequency :: Value -> Either Text TriggerFrequency.TriggerFrequency
jsonToTriggerFrequency =
  decodeNullary
    (Text.pack "TriggerFrequency")
    [ (Text.pack "EveryTime", TriggerFrequency.EveryTime),
      (Text.pack "FirstTimeEachTurn", TriggerFrequency.FirstTimeEachTurn)
    ]

jsonToExtraPhase :: Value -> Either Text ExtraPhase.ExtraPhase
jsonToExtraPhase =
  decodeNullary
    (Text.pack "ExtraPhase")
    [ (Text.pack "ExtraCombat", ExtraPhase.ExtraCombat),
      (Text.pack "ExtraMain", ExtraPhase.ExtraMain)
    ]

manaProductionToJson :: ManaProduction.ManaProduction -> Value
manaProductionToJson mp = case mp of
  ManaProduction.OfType mt -> Json.tagged (Text.pack "OfType") (Just (manaTypeToJson mt))
  ManaProduction.AnyColor -> nullary (Text.pack "AnyColor")

jsonToManaProduction :: Value -> Either Text ManaProduction.ManaProduction
jsonToManaProduction value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("OfType", Just v) -> ManaProduction.OfType <$> jsonToManaType v
    ("AnyColor", _) -> Right ManaProduction.AnyColor
    _ -> Left (Text.pack "unknown ManaProduction: " <> t)

manaSymbolToJson :: ManaSymbol.ManaSymbol -> Value
manaSymbolToJson ms = case ms of
  ManaSymbol.Generic n -> Json.tagged (Text.pack "Generic") (Just (natTo n))
  ManaSymbol.OfType mt -> Json.tagged (Text.pack "OfType") (Just (manaTypeToJson mt))
  ManaSymbol.Hybrid a b -> Json.tagged (Text.pack "Hybrid") (Just (Array (MkArray [manaTypeToJson a, manaTypeToJson b])))
  ManaSymbol.MonocoloredHybrid mt -> Json.tagged (Text.pack "MonocoloredHybrid") (Just (manaTypeToJson mt))
  -- A Color, not a ManaType: CR 107.4f's five Phyrexian symbols are all coloured.
  ManaSymbol.Phyrexian c -> Json.tagged (Text.pack "Phyrexian") (Just (colorToJson c))
  ManaSymbol.Variable -> nullary (Text.pack "Variable")

jsonToManaSymbol :: Value -> Either Text ManaSymbol.ManaSymbol
jsonToManaSymbol value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Generic", Just v) -> ManaSymbol.Generic <$> natFrom v
    ("OfType", Just v) -> ManaSymbol.OfType <$> jsonToManaType v
    ("Hybrid", Just (Array (MkArray [av, bv]))) -> ManaSymbol.Hybrid <$> jsonToManaType av <*> jsonToManaType bv
    ("MonocoloredHybrid", Just v) -> ManaSymbol.MonocoloredHybrid <$> jsonToManaType v
    ("Phyrexian", Just v) -> ManaSymbol.Phyrexian <$> jsonToColor v
    ("Variable", _) -> Right ManaSymbol.Variable
    _ -> Left (Text.pack "unknown ManaSymbol: " <> t)

manaCostToJson :: ManaCost.ManaCost -> Value
manaCostToJson (ManaCost.MkManaCost xs) = listTo manaSymbolToJson xs

jsonToManaCost :: Value -> Either Text ManaCost.ManaCost
jsonToManaCost value = ManaCost.MkManaCost <$> listFrom jsonToManaSymbol value

-- Quantity.Count's arm is `countToJson c` directly, NOT re-wrapped in another
-- "Count" tag: countToJson already tags its own output "Count" (it is shared
-- with Condition's embedding of a Count), and the two types happen to use the
-- SAME tag name at two different levels. Re-wrapping would double-tag
-- ({"type":"Count","value":{"type":"Count","value":[...]}}) -- guarded by the
-- CodecSpec round-trip test.
quantityToJson :: Quantity.Quantity -> Value
quantityToJson q = case q of
  Quantity.Literal n -> Json.tagged (Text.pack "Literal") (Just (Json.jInt n))
  Quantity.ManaValue -> nullary (Text.pack "ManaValue")
  Quantity.Power -> nullary (Text.pack "Power")
  Quantity.X -> nullary (Text.pack "X")
  Quantity.Star -> nullary (Text.pack "Star")
  Quantity.Plus a b -> Json.tagged (Text.pack "Plus") (Just (Array (MkArray [quantityToJson a, quantityToJson b])))
  Quantity.Count c -> countToJson c

jsonToQuantity :: Value -> Either Text Quantity.Quantity
jsonToQuantity value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Json.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("Power", _) -> Right Quantity.Power
    ("X", _) -> Right Quantity.X
    ("Star", _) -> Right Quantity.Star
    ("Plus", Just (Array (MkArray [x, y]))) -> Quantity.Plus <$> jsonToQuantity x <*> jsonToQuantity y
    -- jsonToCount re-derives the tag from the WHOLE value (see the comment on
    -- quantityToJson) rather than from `mv`, which has already had it
    -- stripped.
    ("Count", _) -> Quantity.Count <$> jsonToCount value
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
  Modification.SetBasePowerToughness p t -> Json.tagged (Text.pack "SetBasePowerToughness") (Just (Array (MkArray [quantityToJson p, quantityToJson t])))
  Modification.ModifyPowerToughness p t -> Json.tagged (Text.pack "ModifyPowerToughness") (Just (Array (MkArray [quantityToJson p, quantityToJson t])))
  Modification.SetLandSubtype s -> Json.tagged (Text.pack "SetLandSubtype") (Just (subtypeToJson s))
  Modification.AddLandSubtype s -> Json.tagged (Text.pack "AddLandSubtype") (Just (subtypeToJson s))
  Modification.AddCardType c -> Json.tagged (Text.pack "AddCardType") (Just (cardTypeToJson c))
  Modification.ChangeSubtypeWord a b -> Json.tagged (Text.pack "ChangeSubtypeWord") (Just (Array (MkArray [subtypeToJson a, subtypeToJson b])))
  Modification.SetController p -> Json.tagged (Text.pack "SetController") (Just (playerIdToJson p))
  Modification.SetControllerToSource -> nullary (Text.pack "SetControllerToSource")
  Modification.SetColor cs -> Json.tagged (Text.pack "SetColor") (Just (setTo colorToJson cs))
  Modification.SwitchPowerToughness -> nullary (Text.pack "SwitchPowerToughness")

jsonToModification :: Value -> Either Text Modification.Modification
jsonToModification value = do
  (t, mv) <- Json.tag value
  let pair v = case v of
        Just (Array (MkArray [x, y])) -> Right (x, y)
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
    "SetControllerToSource" -> Right Modification.SetControllerToSource
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
  Affected.Matching f -> Json.tagged (Text.pack "Matching") (Just (filterToJson f))
  Affected.Attached -> Json.tagged (Text.pack "Attached") Nothing

jsonToAffected :: Value -> Either Text Affected.Affected
jsonToAffected value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "TheseObjects" -> withValue mv (fmap Affected.TheseObjects . setFrom jsonToObjectId)
    "Matching" -> withValue mv (fmap Affected.Matching . jsonToFilter)
    "Attached" -> pure Affected.Attached
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
  Json.jObject
    [ (Text.pack "source", objectIdToJson (DamageEvent.source ev)),
      (Text.pack "target", recipientToJson (DamageEvent.target ev)),
      (Text.pack "amount", natTo (DamageEvent.amount ev)),
      (Text.pack "dealtByDeathtouch", Json.jBool (DamageEvent.dealtByDeathtouch ev)),
      (Text.pack "dealtByInfect", Json.jBool (DamageEvent.dealtByInfect ev)),
      (Text.pack "dealtByToxic", natTo (DamageEvent.dealtByToxic ev)),
      (Text.pack "kind", damageKindToJson (DamageEvent.kind ev))
    ]

jsonToDamageEvent :: Value -> Either Text DamageEvent.DamageEvent
jsonToDamageEvent value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  t <- Json.field (Text.pack "target") ps >>= jsonToRecipient
  a <- Json.field (Text.pack "amount") ps >>= natFrom
  d <- Json.field (Text.pack "dealtByDeathtouch") ps >>= jsonToBoolDefault False
  i <- Json.field (Text.pack "dealtByInfect") ps >>= jsonToBoolDefault False
  x <- Json.field (Text.pack "dealtByToxic") ps >>= natFrom
  k <- Json.field (Text.pack "kind") ps >>= jsonToDamageKind
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = s,
        DamageEvent.target = t,
        DamageEvent.amount = a,
        DamageEvent.dealtByDeathtouch = d,
        DamageEvent.dealtByInfect = i,
        DamageEvent.dealtByToxic = x,
        DamageEvent.kind = k
      }

zoneChangeToJson :: ZoneChange.ZoneChange -> Value
zoneChangeToJson zc =
  Json.jObject
    [ (Text.pack "departed", objectIdToJson (ZoneChange.departed zc)),
      (Text.pack "object", objectIdToJson (ZoneChange.object zc)),
      (Text.pack "from", zoneToJson (ZoneChange.from zc)),
      (Text.pack "to", zoneToJson (ZoneChange.to zc))
    ]

jsonToZoneChange :: Value -> Either Text ZoneChange.ZoneChange
jsonToZoneChange value = do
  ps <- Json.asObject value
  d <- Json.field (Text.pack "departed") ps >>= jsonToObjectId
  o <- Json.field (Text.pack "object") ps >>= jsonToObjectId
  f <- Json.field (Text.pack "from") ps >>= jsonToZone
  t <- Json.field (Text.pack "to") ps >>= jsonToZone
  pure (ZoneChange.MkZoneChange d o f t)

projectedCharacteristicsToJson :: PC.ProjectedCharacteristics -> Value
projectedCharacteristicsToJson pc =
  Json.jObject
    [ (Text.pack "name", Json.jText (PC.name pc)),
      (Text.pack "supertypes", setTo supertypeToJson (PC.supertypes pc)),
      (Text.pack "keywords", multisetTo keywordToJson (PC.keywords pc)),
      (Text.pack "colors", setTo colorToJson (PC.colors pc)),
      (Text.pack "power", maybeTo Json.jInt (PC.power pc)),
      (Text.pack "toughness", maybeTo Json.jInt (PC.toughness pc)),
      (Text.pack "characteristicPT", maybeTo (\(p, t) -> Array (MkArray [quantityToJson p, quantityToJson t])) (PC.characteristicPT pc)),
      (Text.pack "cardTypes", setTo cardTypeToJson (PC.cardTypes pc)),
      (Text.pack "subtypes", setTo subtypeToJson (PC.subtypes pc)),
      (Text.pack "activatedAbilities", listTo activatedAbilityToJson (PC.activatedAbilities pc)),
      (Text.pack "replacementEffects", listTo replacementEffectToJson (PC.replacementEffects pc)),
      (Text.pack "triggeredAbilities", listTo triggeredAbilityToJson (PC.triggeredAbilities pc))
    ]

jsonToProjectedCharacteristics :: Value -> Either Text PC.ProjectedCharacteristics
jsonToProjectedCharacteristics value = do
  ps <- Json.asObject value
  nm <- Json.field (Text.pack "name") ps >>= Json.asText
  sups <- Json.field (Text.pack "supertypes") ps >>= setFrom jsonToSupertype
  kws <- Json.field (Text.pack "keywords") ps >>= multisetFrom jsonToKeyword
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
  acts <- Json.field (Text.pack "activatedAbilities") ps >>= listFrom jsonToActivatedAbility
  reps <- Json.field (Text.pack "replacementEffects") ps >>= listFrom jsonToReplacementEffect
  trigs <- Json.field (Text.pack "triggeredAbilities") ps >>= listFrom jsonToTriggeredAbility
  pure
    PC.MkProjectedCharacteristics
      { PC.name = nm,
        PC.supertypes = sups,
        PC.keywords = kws,
        PC.colors = cols,
        PC.power = pow,
        PC.toughness = tou,
        PC.characteristicPT = cda,
        PC.cardTypes = cts,
        PC.subtypes = subs,
        PC.activatedAbilities = acts,
        PC.replacementEffects = reps,
        PC.triggeredAbilities = trigs
      }

jsonToQuantityPair :: Value -> Either Text (Quantity.Quantity, Quantity.Quantity)
jsonToQuantityPair value = case value of
  Array (MkArray [p, t]) -> do
    p_ <- jsonToQuantity p
    t_ <- jsonToQuantity t
    pure (p_, t_)
  _ -> Left (Text.pack "expected a [power, toughness] quantity pair")

gameEventToJson :: GameEvent.GameEvent -> Value
gameEventToJson e = case e of
  GameEvent.Moved zc pc -> Json.tagged (Text.pack "Moved") (Just (Array (MkArray [zoneChangeToJson zc, projectedCharacteristicsToJson pc])))
  GameEvent.DamageDealt ev -> Json.tagged (Text.pack "DamageDealt") (Just (damageEventToJson ev))
  GameEvent.StepBegan p pid -> Json.tagged (Text.pack "StepBegan") (Just (Array (MkArray [phaseToJson p, playerIdToJson pid])))
  GameEvent.SpellCast pid -> Json.tagged (Text.pack "SpellCast") (Just (playerIdToJson pid))
  GameEvent.BecameMonarch pid -> Json.tagged (Text.pack "BecameMonarch") (Just (playerIdToJson pid))
  GameEvent.Cycled oid -> Json.tagged (Text.pack "Cycled") (Just (objectIdToJson oid))
  GameEvent.Revealed pid pc -> Json.tagged (Text.pack "Revealed") (Just (Array (MkArray [playerIdToJson pid, projectedCharacteristicsToJson pc])))
  GameEvent.AttackerDeclared oid -> Json.tagged (Text.pack "AttackerDeclared") (Just (objectIdToJson oid))

jsonToGameEvent :: Value -> Either Text GameEvent.GameEvent
jsonToGameEvent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Moved", Just (Array (MkArray [zc, pc]))) -> GameEvent.Moved <$> jsonToZoneChange zc <*> jsonToProjectedCharacteristics pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> jsonToDamageEvent v
    ("StepBegan", Just (Array (MkArray [p, pid]))) -> GameEvent.StepBegan <$> jsonToPhase p <*> jsonToPlayerId pid
    ("SpellCast", Just v) -> GameEvent.SpellCast <$> jsonToPlayerId v
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> jsonToPlayerId v
    ("Cycled", Just v) -> GameEvent.Cycled <$> jsonToObjectId v
    ("Revealed", Just (Array (MkArray [pid, pc]))) -> GameEvent.Revealed <$> jsonToPlayerId pid <*> jsonToProjectedCharacteristics pc
    ("AttackerDeclared", Just v) -> GameEvent.AttackerDeclared <$> jsonToObjectId v
    _ -> Left (Text.pack "unknown GameEvent: " <> t)

-- MonarchTarget ----------------------------------------------------------------

monarchTargetToJson :: MonarchTarget.MonarchTarget -> Value
monarchTargetToJson t = nullary . Text.pack $ case t of
  MonarchTarget.TheController -> "TheController"
  MonarchTarget.ControllerOfSource -> "ControllerOfSource"

jsonToMonarchTarget :: Value -> Either Text MonarchTarget.MonarchTarget
jsonToMonarchTarget =
  decodeNullary
    (Text.pack "MonarchTarget")
    [ (Text.pack "TheController", MonarchTarget.TheController),
      (Text.pack "ControllerOfSource", MonarchTarget.ControllerOfSource)
    ]

-- TokenEntry -----------------------------------------------------------------

tapStateToJson :: TapState.TapState -> Value
tapStateToJson t = nullary . Text.pack $ case t of
  TapState.Untapped -> "Untapped"
  TapState.Tapped -> "Tapped"

jsonToTapState :: Value -> Either Text TapState.TapState
jsonToTapState =
  decodeNullary
    (Text.pack "TapState")
    [ (Text.pack "Untapped", TapState.Untapped),
      (Text.pack "Tapped", TapState.Tapped)
    ]

tokenEntryToJson :: TokenEntry.TokenEntry -> Value
tokenEntryToJson e =
  Json.jObject
    [ (Text.pack "tapped", tapStateToJson (TokenEntry.tapped e)),
      (Text.pack "attacking", Json.jBool (TokenEntry.attacking e))
    ]

jsonToTokenEntry :: Value -> Either Text TokenEntry.TokenEntry
jsonToTokenEntry value = do
  ps <- Json.asObject value
  t <- getOpt (Text.pack "tapped") ps `orDefault` (TapState.Untapped, jsonToTapState)
  a <- jsonToBoolDefault False (getOpt (Text.pack "attacking") ps)
  pure
    TokenEntry.MkTokenEntry
      { TokenEntry.tapped = t,
        TokenEntry.attacking = a
      }
  where
    orDefault v (d, f) = case v of
      Null _ -> Right d
      _ -> f v

-- CR 110.5b: "permanents enter the battlefield untapped ... unless a spell or
-- ability says otherwise", and a creature is attacking only if something says it
-- is (CR 506.3). So this is what a Create that says nothing extra means, and the
-- value the encoding ELIDES: a card file carries a TokenEntry only when the
-- effect really does say otherwise, which is what keeps every token-making file
-- written before this one byte-identical.
defaultTokenEntry :: TokenEntry.TokenEntry
defaultTokenEntry =
  TokenEntry.MkTokenEntry
    { TokenEntry.tapped = TapState.Untapped,
      TokenEntry.attacking = False
    }

-- Effect ---------------------------------------------------------------------

effectToJson :: Effect.Effect CardT.Card -> Value
effectToJson e = case e of
  Effect.DealDamage s q -> Json.tagged (Text.pack "DealDamage") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.ModifyTarget d m s -> Json.tagged (Text.pack "ModifyTarget") (Just (Array (MkArray [durationToJson d, modificationToJson m, slotNameToJson s])))
  Effect.ChangeText s -> Json.tagged (Text.pack "ChangeText") (Just (slotNameToJson s))
  Effect.AddMana production -> Json.tagged (Text.pack "AddMana") (Just (manaProductionToJson production))
  Effect.Search f d -> Json.tagged (Text.pack "Search") (Just (Array (MkArray [filterToJson f, searchDestinationToJson d])))
  Effect.ExileAllGraveyards -> nullary (Text.pack "ExileAllGraveyards")
  Effect.Proliferate -> nullary (Text.pack "Proliferate")
  Effect.ExileHandThenDraw -> nullary (Text.pack "ExileHandThenDraw")
  Effect.PlayerSacrifices slot f q -> Json.tagged (Text.pack "PlayerSacrifices") (Just (Array (MkArray [slotNameToJson slot, filterToJson f, quantityToJson q])))
  Effect.RestartGame -> nullary (Text.pack "RestartGame")
  Effect.ControlPlayerNextTurn s -> Json.tagged (Text.pack "ControlPlayerNextTurn") (Just (slotNameToJson s))
  Effect.Destroy s r -> Json.tagged (Text.pack "Destroy") (Just (Array (MkArray [objectRefToJson s, regenerabilityToJson r])))
  Effect.Sacrifice s -> Json.tagged (Text.pack "Sacrifice") (Just (slotNameToJson s))
  Effect.RemoveFromCombat s -> Json.tagged (Text.pack "RemoveFromCombat") (Just (slotNameToJson s))
  Effect.Counter s -> Json.tagged (Text.pack "Counter") (Just (slotNameToJson s))
  Effect.MoveToZone s z -> Json.tagged (Text.pack "MoveToZone") (Just (Array (MkArray [slotNameToJson s, zoneToJson z])))
  Effect.Draw r q -> Json.tagged (Text.pack "Draw") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  Effect.Mill s q -> Json.tagged (Text.pack "Mill") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.Discard s q -> Json.tagged (Text.pack "Discard") (Just (Array (MkArray [slotNameToJson s, quantityToJson q])))
  Effect.LoseLife r q -> Json.tagged (Text.pack "LoseLife") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  Effect.GainLife r q -> Json.tagged (Text.pack "GainLife") (Just (Array (MkArray [playerRefToJson r, quantityToJson q])))
  -- Create's payload is positional, and the TokenEntry is ELIDED when it is the
  -- CR 110.5b default (defaultTokenEntry) -- the same posture `counterability`
  -- takes, so a card file that says nothing about how its tokens enter stays
  -- exactly as it was written. The three-element form is therefore two shapes,
  -- told apart on decode by JSON TYPE rather than by position: a slot name is a
  -- string (slotNameToJson) and a TokenEntry is an object, so the two can never
  -- be confused.
  Effect.Create q c te ms ->
    Json.tagged (Text.pack "Create") . Just . Array . MkArray $
      [quantityToJson q, cardToJson c]
        <> (if te == defaultTokenEntry then [] else [tokenEntryToJson te])
        <> fmap slotNameToJson (Maybe.maybeToList ms)
  Effect.Replace d u re -> Json.tagged (Text.pack "Replace") (Just (Array (MkArray [durationToJson d, usesToJson u, replacementEffectToJson re])))
  Effect.SkipNextPhase r ph -> Json.tagged (Text.pack "SkipNextPhase") (Just (Array (MkArray [playerRefToJson r, phaseToJson ph])))
  Effect.PutCounters k q s -> Json.tagged (Text.pack "PutCounters") (Just (Array (MkArray [counterKindToJson k, quantityToJson q, slotNameToJson s])))
  Effect.GainPlayerCounters r k q -> Json.tagged (Text.pack "GainPlayerCounters") (Just (Array (MkArray [playerRefToJson r, playerCounterKindToJson k, quantityToJson q])))
  Effect.Untap r -> Json.tagged (Text.pack "Untap") (Just (objectRefToJson r))
  Effect.AddPhases ps -> Json.tagged (Text.pack "AddPhases") (Just (Array (MkArray (fmap extraPhaseToJson ps))))
  Effect.GainControl d s -> Json.tagged (Text.pack "GainControl") (Just (Array (MkArray [durationToJson d, slotNameToJson s])))
  -- The duration is ELIDED when absent, which is CR 603.7b's default -- so
  -- Tidal Wave's one-shot entry stays a bare ability name and only a card that
  -- states a duration writes the two-element form.
  Effect.ArmDelayedTrigger n md ->
    Json.tagged (Text.pack "ArmDelayedTrigger") . Just $ case md of
      Nothing -> abilityNameToJson n
      Just d -> Array (MkArray [abilityNameToJson n, durationToJson d])
  Effect.AffectPlayers d s pe -> Json.tagged (Text.pack "AffectPlayers") (Just (Array (MkArray [durationToJson d, playerScopeToJson s, playerEffectToJson pe])))
  Effect.CreateEmblem c -> Json.tagged (Text.pack "CreateEmblem") (Just (cardToJson c))
  Effect.BecomeMonarch t -> Json.tagged (Text.pack "BecomeMonarch") (Just (monarchTargetToJson t))
  Effect.ExileUntilMonarch s -> Json.tagged (Text.pack "ExileUntilMonarch") (Just (slotNameToJson s))
  Effect.Attach s -> Json.tagged (Text.pack "Attach") (Just (slotNameToJson s))
  Effect.AttachTarget s f -> Json.tagged (Text.pack "AttachTarget") (Just (Array (MkArray [slotNameToJson s, filterToJson f])))
  Effect.PlaySubgame s -> Json.tagged (Text.pack "PlaySubgame") (Just (slotNameToJson s))
  Effect.TakeExtraTurn r -> Json.tagged (Text.pack "TakeExtraTurn") (Just (playerRefToJson r))

jsonToEffect :: Value -> Either Text (Effect.Effect CardT.Card)
jsonToEffect value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "DealDamage" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.DealDamage <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "DealDamage expects [slot, quantity]")
    "ModifyTarget" -> case mv of
      Just (Array (MkArray [d, m, s])) -> Effect.ModifyTarget <$> jsonToDuration d <*> jsonToModification m <*> jsonToSlotName s
      _ -> Left (Text.pack "ModifyTarget expects [duration, modification, slot]")
    "ChangeText" -> withValue mv (fmap Effect.ChangeText . jsonToSlotName)
    "AddMana" -> withValue mv (fmap Effect.AddMana . jsonToManaProduction)
    "Search" -> case mv of
      Just (Array (MkArray [f, d])) -> Effect.Search <$> jsonToFilter f <*> jsonToSearchDestination d
      _ -> Left (Text.pack "Search expects [filter, destination]")
    "ExileAllGraveyards" -> Right Effect.ExileAllGraveyards
    "Proliferate" -> Right Effect.Proliferate
    "ExileHandThenDraw" -> Right Effect.ExileHandThenDraw
    "PlayerSacrifices" -> case mv of
      Just (Array (MkArray [sv, fv, qv])) -> Effect.PlayerSacrifices <$> jsonToSlotName sv <*> jsonToFilter fv <*> jsonToQuantity qv
      _ -> Left (Text.pack "PlayerSacrifices expects [slot, filter, quantity]")
    "RestartGame" -> Right Effect.RestartGame
    "ControlPlayerNextTurn" -> withValue mv (fmap Effect.ControlPlayerNextTurn . jsonToSlotName)
    "Destroy" -> case mv of
      Just (Array (MkArray [sv, rv])) -> Effect.Destroy <$> jsonToObjectRef sv <*> jsonToRegenerability rv
      _ -> Left (Text.pack "Destroy expects [objectRef, regenerability]")
    "Sacrifice" -> withValue mv (fmap Effect.Sacrifice . jsonToSlotName)
    "RemoveFromCombat" -> withValue mv (fmap Effect.RemoveFromCombat . jsonToSlotName)
    "Counter" -> withValue mv (fmap Effect.Counter . jsonToSlotName)
    "MoveToZone" -> case mv of
      Just (Array (MkArray [s, z])) -> Effect.MoveToZone <$> jsonToSlotName s <*> jsonToZone z
      _ -> Left (Text.pack "MoveToZone expects [slot, zone]")
    "Draw" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.Draw <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "Draw expects [playerRef, quantity]")
    "Mill" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.Mill <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Mill expects [slot, quantity]")
    "Discard" -> case mv of
      Just (Array (MkArray [s, q])) -> Effect.Discard <$> jsonToSlotName s <*> jsonToQuantity q
      _ -> Left (Text.pack "Discard expects [slot, quantity]")
    "LoseLife" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.LoseLife <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "LoseLife expects [playerRef, quantity]")
    "GainLife" -> case mv of
      Just (Array (MkArray [r, q])) -> Effect.GainLife <$> jsonToPlayerRef r <*> jsonToQuantity q
      _ -> Left (Text.pack "GainLife expects [playerRef, quantity]")
    -- The four shapes the encoder above can emit. The three-element one is read
    -- by JSON type: an Object is the TokenEntry, anything else is the slot name
    -- (a string), which is what lets the entry be elided when it is the default
    -- without a hole in the array.
    "Create" -> case mv of
      Just (Array (MkArray [q, c])) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> pure defaultTokenEntry <*> pure Nothing
      Just (Array (MkArray [q, c, e@(Object _)])) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> jsonToTokenEntry e <*> pure Nothing
      Just (Array (MkArray [q, c, s])) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> pure defaultTokenEntry <*> (Just <$> jsonToSlotName s)
      Just (Array (MkArray [q, c, e, s])) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> jsonToTokenEntry e <*> (Just <$> jsonToSlotName s)
      _ -> Left (Text.pack "Create expects [Quantity, Card], optionally with a TokenEntry and/or a slot")
    "ArmDelayedTrigger" -> case mv of
      Just (Array (MkArray [n, d])) -> Effect.ArmDelayedTrigger <$> jsonToAbilityName n <*> fmap Just (jsonToDuration d)
      _ -> withValue mv (fmap (`Effect.ArmDelayedTrigger` Nothing) . jsonToAbilityName)
    "Replace" -> case mv of
      Just (Array (MkArray [d, u, re])) -> do
        duration <- jsonToDuration d
        uses <- jsonToUses u
        effect <- jsonToReplacementEffect re
        pure (Effect.Replace duration uses effect)
      _ -> Left (Text.pack "Replace expects [Duration, Uses, ReplacementEffect]")
    "SkipNextPhase" -> case mv of
      Just (Array (MkArray [r, ph])) -> Effect.SkipNextPhase <$> jsonToPlayerRef r <*> jsonToPhase ph
      _ -> Left (Text.pack "SkipNextPhase expects [playerRef, phase]")
    "PutCounters" -> case mv of
      Just (Array (MkArray [k, q, s])) -> Effect.PutCounters <$> jsonToCounterKind k <*> jsonToQuantity q <*> jsonToSlotName s
      _ -> Left (Text.pack "PutCounters expects [counterKind, quantity, slot]")
    "GainPlayerCounters" -> case mv of
      Just (Array (MkArray [r, k, q])) -> Effect.GainPlayerCounters <$> jsonToPlayerRef r <*> jsonToPlayerCounterKind k <*> jsonToQuantity q
      _ -> Left (Text.pack "GainPlayerCounters expects [playerRef, playerCounterKind, quantity]")
    "Untap" -> withValue mv (fmap Effect.Untap . jsonToObjectRef)
    "AddPhases" -> case mv of
      Just (Array (MkArray ps)) -> Effect.AddPhases <$> traverse jsonToExtraPhase ps
      _ -> Left (Text.pack "AddPhases expects [ExtraPhase]")
    "GainControl" -> case mv of
      Just (Array (MkArray [d, s])) -> Effect.GainControl <$> jsonToDuration d <*> jsonToSlotName s
      _ -> Left (Text.pack "GainControl expects [duration, slot]")
    "AffectPlayers" -> case mv of
      Just (Array (MkArray [d, s, pe])) -> Effect.AffectPlayers <$> jsonToDuration d <*> jsonToPlayerScope s <*> jsonToPlayerEffect pe
      _ -> Left (Text.pack "AffectPlayers expects [Duration, PlayerScope, PlayerEffect]")
    "CreateEmblem" -> withValue mv (fmap Effect.CreateEmblem . jsonToCard)
    "BecomeMonarch" -> withValue mv (fmap Effect.BecomeMonarch . jsonToMonarchTarget)
    "ExileUntilMonarch" -> withValue mv (fmap Effect.ExileUntilMonarch . jsonToSlotName)
    "Attach" -> withValue mv (fmap Effect.Attach . jsonToSlotName)
    "AttachTarget" -> case mv of
      Just (Array (MkArray [s, f])) -> Effect.AttachTarget <$> jsonToSlotName s <*> jsonToFilter f
      _ -> Left (Text.pack "AttachTarget expects [slot, filter]")
    "PlaySubgame" -> withValue mv (fmap Effect.PlaySubgame . jsonToSlotName)
    "TakeExtraTurn" -> withValue mv (fmap Effect.TakeExtraTurn . jsonToPlayerRef)
    _ -> Left (Text.pack "unknown Effect: " <> t)

-- Records & abilities --------------------------------------------------------

targetSpecsToJson :: Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Value
targetSpecsToJson m =
  listTo (\(k, v) -> Json.jObject [(Text.pack "slot", slotNameToJson k), (Text.pack "spec", targetSpecToJson v)]) (Map.toAscList m)

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
  Json.jObject
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

-- CR 613.6: the parts of one ability's effect travel together, so the wire format
-- is one affected set and an ARRAY of modifications -- never one entry per layer.
staticAbilityToJson :: StaticAbility.StaticAbility -> Value
staticAbilityToJson sa =
  Json.jObject
    [ (Text.pack "affected", affectedToJson (StaticAbility.affected sa)),
      (Text.pack "modifications", nonEmptyTo modificationToJson (StaticAbility.modifications sa))
    ]

jsonToStaticAbility :: Value -> Either Text StaticAbility.StaticAbility
jsonToStaticAbility value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "affected") ps >>= jsonToAffected
  ms <- Json.field (Text.pack "modifications") ps >>= nonEmptyFrom jsonToModification
  pure (StaticAbility.MkStaticAbility a ms)

playerStaticAbilityToJson :: PlayerStaticAbility.PlayerStaticAbility -> Value
playerStaticAbilityToJson pa =
  Json.jObject
    [ (Text.pack "scope", playerScopeToJson (PlayerStaticAbility.scope pa)),
      (Text.pack "effect", playerEffectToJson (PlayerStaticAbility.effect pa))
    ]

jsonToPlayerStaticAbility :: Value -> Either Text PlayerStaticAbility.PlayerStaticAbility
jsonToPlayerStaticAbility value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "scope") ps >>= jsonToPlayerScope
  e <- Json.field (Text.pack "effect") ps >>= jsonToPlayerEffect
  pure (PlayerStaticAbility.MkPlayerStaticAbility s e)

blockRequirementToJson :: BlockRequirement.BlockRequirement -> Value
blockRequirementToJson br =
  Json.jObject [(Text.pack "attacker", affectedToJson (BlockRequirement.attacker br))]

jsonToBlockRequirement :: Value -> Either Text BlockRequirement.BlockRequirement
jsonToBlockRequirement value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "attacker") ps >>= jsonToAffected
  pure (BlockRequirement.MkBlockRequirement a)

costToJson :: Cost.Cost -> Value
costToJson c =
  Json.jObject
    [ (Text.pack "mana", maybeTo manaCostToJson (Cost.mana c)),
      (Text.pack "components", listTo costComponentToJson (Cost.components c))
    ]

-- CR 118.6: an ABSENT mana field decodes to Nothing -- an unpayable cost -- and
-- never to {0}. Every ability-bearing card file states its mana part explicitly
-- (`[]` for {0}), so the absent case is only ever reached by a malformed file.
jsonToCost :: Value -> Either Text Cost.Cost
jsonToCost value = do
  ps <- Json.asObject value
  m <- maybeFrom jsonToManaCost (getOpt (Text.pack "mana") ps)
  cs <- listFromDefault jsonToCostComponent (getOpt (Text.pack "components") ps)
  pure Cost.MkCost {Cost.mana = m, Cost.components = cs}

activatedAbilityToJson :: ActivatedAbility.ActivatedAbility CardT.Card -> Value
activatedAbilityToJson aa =
  Json.jObject $
    [ (Text.pack "cost", costToJson (ActivatedAbility.cost aa)),
      (Text.pack "modal", modalToJson (ActivatedAbility.modal aa))
    ]
      -- CR 307.5: emitted only for a restricted ability, so the absence of the
      -- key means "no timing rider" -- the same optional-field shape Card.enchant
      -- takes, and it leaves every card without one byte-identical.
      <> ( case ActivatedAbility.timing aa of
             ActivationTiming.AnyTime -> []
             ActivationTiming.SorcerySpeed -> [(Text.pack "timing", activationTimingToJson (ActivatedAbility.timing aa))]
         )

activationTimingToJson :: ActivationTiming.ActivationTiming -> Value
activationTimingToJson t = nullary . Text.pack $ case t of
  ActivationTiming.AnyTime -> "AnyTime"
  ActivationTiming.SorcerySpeed -> "SorcerySpeed"

jsonToActivationTiming :: Value -> Either Text ActivationTiming.ActivationTiming
jsonToActivationTiming =
  decodeNullary
    (Text.pack "ActivationTiming")
    [ (Text.pack "AnyTime", ActivationTiming.AnyTime),
      (Text.pack "SorcerySpeed", ActivationTiming.SorcerySpeed)
    ]

jsonToActivatedAbility :: Value -> Either Text (ActivatedAbility.ActivatedAbility CardT.Card)
jsonToActivatedAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "cost") ps >>= jsonToCost
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal
  t <- case Json.optField (Text.pack "timing") ps of
    Nothing -> pure ActivationTiming.AnyTime
    Just v -> jsonToActivationTiming v
  pure (ActivatedAbility.MkActivatedAbility c m t)

replacementEffectToJson :: ReplacementEffect.ReplacementEffect -> Value
replacementEffectToJson re = case re of
  ReplacementEffect.ZoneChangeR p z ->
    Json.tagged (Text.pack "ZoneChangeR") (Just (Array (MkArray [zoneChangePatternToJson p, zoneToJson z])))
  ReplacementEffect.EntryR r ->
    Json.tagged (Text.pack "EntryR") (Just (entryRewriteToJson r))
  ReplacementEffect.DamageR p r ->
    Json.tagged (Text.pack "DamageR") (Just (Array (MkArray [damagePatternToJson p, damageRewriteToJson r])))
  ReplacementEffect.DestructionR r ->
    Json.tagged (Text.pack "DestructionR") (Just (destructionRewriteToJson r))
  ReplacementEffect.CounterR p s ->
    Json.tagged (Text.pack "CounterR") (Just (Array (MkArray [counterPatternToJson p, scalingToJson s])))
  ReplacementEffect.TokenR p s ->
    Json.tagged (Text.pack "TokenR") (Just (Array (MkArray [tokenPatternToJson p, scalingToJson s])))
  ReplacementEffect.PhaseR p ->
    Json.tagged (Text.pack "PhaseR") (Just (phasePatternToJson p))

jsonToReplacementEffect :: Value -> Either Text ReplacementEffect.ReplacementEffect
jsonToReplacementEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ZoneChangeR", Just (Array (MkArray [p, z]))) -> do
      pattern_ <- jsonToZoneChangePattern p
      dest <- jsonToZone z
      pure (ReplacementEffect.ZoneChangeR pattern_ dest)
    ("EntryR", Just v) -> fmap ReplacementEffect.EntryR (jsonToEntryRewrite v)
    ("DamageR", Just (Array (MkArray [p, r]))) -> do
      pattern_ <- jsonToDamagePattern p
      rewrite <- jsonToDamageRewrite r
      pure (ReplacementEffect.DamageR pattern_ rewrite)
    ("DestructionR", Just v) -> fmap ReplacementEffect.DestructionR (jsonToDestructionRewrite v)
    ("CounterR", Just (Array (MkArray [p, s]))) -> do
      pattern_ <- jsonToCounterPattern p
      scaling <- jsonToScaling s
      pure (ReplacementEffect.CounterR pattern_ scaling)
    ("TokenR", Just (Array (MkArray [p, s]))) -> do
      pattern_ <- jsonToTokenPattern p
      scaling <- jsonToScaling s
      pure (ReplacementEffect.TokenR pattern_ scaling)
    ("PhaseR", Just v) -> fmap ReplacementEffect.PhaseR (jsonToPhasePattern v)
    _ -> Left (Text.pack "unknown ReplacementEffect: " <> t)

triggeredAbilityToJson :: TriggeredAbility.TriggeredAbility CardT.Card -> Value
triggeredAbilityToJson ta =
  Json.jObject
    ( [ (Text.pack "condition", triggerConditionToJson (TriggeredAbility.condition ta)),
        (Text.pack "modal", modalToJson (TriggeredAbility.modal ta))
      ]
        <> ( case TriggeredAbility.intervening ta of
               Nothing -> []
               Just c -> [(Text.pack "intervening", conditionToJson c)]
           )
    )

jsonToTriggeredAbility :: Value -> Either Text (TriggeredAbility.TriggeredAbility CardT.Card)
jsonToTriggeredAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "condition") ps >>= jsonToTriggerCondition
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal
  i <- maybeFrom jsonToCondition (getOpt (Text.pack "intervening") ps)
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
    (\(k, v) -> Json.jObject [(Text.pack "name", abilityNameToJson k), (Text.pack "ability", triggeredAbilityToJson v)])
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
-- predates P4 stays byte-identical (the same precedent characteristicPT and
-- colorIndicator follow).
mapFromDefault :: (Value -> Either Text (Map.Map k v)) -> Value -> Either Text (Map.Map k v)
mapFromDefault f value = case value of
  Null _ -> Right Map.empty
  _ -> f value

-- Runtime-only, never in card JSON -- covered for the same reason SetController's
-- PlayerId is: the codec must stay total over the transitive closure of what the
-- game state carries.
bindingToJson :: Binding.Binding -> Value
bindingToJson b =
  Json.jObject
    [ (Text.pack "target", maybeTo recipientToJson (Binding.target b)),
      (Text.pack "subtypes", maybeTo (\(f, t) -> Array (MkArray [subtypeToJson f, subtypeToJson t])) (Binding.subtypes b)),
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
  Array (MkArray [f, t]) -> do
    f_ <- jsonToSubtype f
    t_ <- jsonToSubtype t
    pure (f_, t_)
  _ -> Left (Text.pack "expected a [from, to] subtype pair")

bindingsToJson :: Map.Map SlotName.SlotName Binding.Binding -> Value
bindingsToJson m =
  listTo
    (\(k, v) -> Json.jObject [(Text.pack "slot", slotNameToJson k), (Text.pack "binding", bindingToJson v)])
    (Map.toAscList m)

jsonToBindings :: Value -> Either Text (Map.Map SlotName.SlotName Binding.Binding)
jsonToBindings value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "slot") ps >>= jsonToSlotName
        b <- Json.field (Text.pack "binding") ps >>= jsonToBinding
        pure (k, b)
   in Map.fromList <$> listFrom decodeEntry value

-- CR 611.2: the STORED duration, which unlike every other type in this module
-- never appears in card JSON -- a card carries a Duration and Pawl.Expiry.arm
-- turns it into this. The one thing that serialises an Expiry is a
-- DelayedTrigger, below, because CR 603.7b lets a delayed ability state one.
expiryToJson :: Expiry.Expiry -> Value
expiryToJson e = case e of
  Expiry.AtCleanup -> nullary (Text.pack "AtCleanup")
  Expiry.Never -> nullary (Text.pack "Never")
  Expiry.While p c -> Json.tagged (Text.pack "While") (Just (Array (MkArray [playerIdToJson p, conditionToJson c])))
  Expiry.AtTurnOf p -> Json.tagged (Text.pack "AtTurnOf") (Just (playerIdToJson p))

jsonToExpiry :: Value -> Either Text Expiry.Expiry
jsonToExpiry value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AtCleanup", _) -> Right Expiry.AtCleanup
    ("Never", _) -> Right Expiry.Never
    ("While", Just (Array (MkArray [p, c]))) -> Expiry.While <$> jsonToPlayerId p <*> jsonToCondition c
    ("AtTurnOf", Just v) -> Expiry.AtTurnOf <$> jsonToPlayerId v
    _ -> Left (Text.pack "unknown Expiry: " <> t)

delayedTriggerToJson :: DelayedTrigger.DelayedTrigger -> Value
delayedTriggerToJson d =
  Json.jObject
    [ (Text.pack "ability", triggeredAbilityToJson (DelayedTrigger.ability d)),
      (Text.pack "source", objectIdToJson (DelayedTrigger.source d)),
      (Text.pack "controller", playerIdToJson (DelayedTrigger.controller d)),
      (Text.pack "bindings", bindingsToJson (DelayedTrigger.bindings d)),
      -- CR 603.7b: absent for an ability with no stated duration, which is the
      -- rule's default and every entry in the pool but Full Throttle's.
      (Text.pack "expiry", maybeTo expiryToJson (DelayedTrigger.expiry d))
    ]

jsonToDelayedTrigger :: Value -> Either Text DelayedTrigger.DelayedTrigger
jsonToDelayedTrigger value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  c <- Json.field (Text.pack "controller") ps >>= jsonToPlayerId
  b <- Json.field (Text.pack "bindings") ps >>= jsonToBindings
  e <- maybeFrom jsonToExpiry (getOpt (Text.pack "expiry") ps)
  pure
    DelayedTrigger.MkDelayedTrigger
      { DelayedTrigger.ability = a,
        DelayedTrigger.source = s,
        DelayedTrigger.controller = c,
        DelayedTrigger.bindings = b,
        DelayedTrigger.expiry = e
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

optionalityToJson :: Optionality.Optionality -> Value
optionalityToJson o = nullary . Text.pack $ case o of
  Optionality.Mandatory -> "Mandatory"
  Optionality.Optional -> "Optional"

jsonToOptionality :: Value -> Either Text Optionality.Optionality
jsonToOptionality =
  decodeNullary
    (Text.pack "Optionality")
    [ (Text.pack "Mandatory", Optionality.Mandatory),
      (Text.pack "Optional", Optionality.Optional)
    ]

-- An omitted optionality decodes to Mandatory, the counterability posture (and
-- for the same reason): almost every mode in the corpus prints no "may", and a
-- required key would have meant editing every card file to say nothing.
jsonToOptionalityDefault :: Value -> Either Text Optionality.Optionality
jsonToOptionalityDefault value = case value of
  Null _ -> Right Optionality.Mandatory
  _ -> jsonToOptionality value

modeToJson :: Mode.Mode CardT.Card -> Value
modeToJson m =
  Json.jObject
    ( [ (Text.pack "effects", seqTo effectToJson (Mode.effects m)),
        (Text.pack "targetSpecs", targetSpecsToJson (Mode.targetSpecs m))
      ]
        -- Omitted when Mandatory; see jsonToOptionalityDefault.
        <> ( case Mode.optionality m of
               Optionality.Mandatory -> []
               Optionality.Optional -> [(Text.pack "optionality", optionalityToJson (Mode.optionality m))]
           )
    )

jsonToMode :: Value -> Either Text (Mode.Mode CardT.Card)
jsonToMode value = do
  ps <- Json.asObject value
  es <- Json.field (Text.pack "effects") ps >>= seqFrom jsonToEffect
  ts <- Json.field (Text.pack "targetSpecs") ps >>= jsonToTargetSpecs
  o <- jsonToOptionalityDefault (getOpt (Text.pack "optionality") ps)
  pure (Mode.MkMode es ts o)

modalToJson :: Modal.Modal CardT.Card -> Value
modalToJson m =
  Json.jObject
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
  Json.jObject
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
        <> ( if Set.null (CardT.colorIndicator c)
               then []
               else [(Text.pack "colorIndicator", setTo colorToJson (CardT.colorIndicator c))]
           )
        <> ( case CardT.characteristicPT c of
               Nothing -> []
               Just q -> [(Text.pack "characteristicPT", quantityToJson q)]
           )
        <> ( if Map.null (CardT.delayedAbilities c)
               then []
               else [(Text.pack "delayedAbilities", delayedAbilitiesToJson (CardT.delayedAbilities c))]
           )
        <> ( if null (CardT.playerAbilities c)
               then []
               else [(Text.pack "playerAbilities", listTo playerStaticAbilityToJson (CardT.playerAbilities c))]
           )
        <> ( if null (CardT.blockRequirements c)
               then []
               else [(Text.pack "blockRequirements", listTo blockRequirementToJson (CardT.blockRequirements c))]
           )
        <> ( if null (CardT.additionalCosts c)
               then []
               else [(Text.pack "additionalCosts", listTo costComponentToJson (CardT.additionalCosts c))]
           )
        <> ( if null (CardT.alternativeCosts c)
               then []
               else [(Text.pack "alternativeCosts", listTo costToJson (CardT.alternativeCosts c))]
           )
        -- Omitted when Counterable, the posture every other defaulted key here
        -- takes: one card in the pool prints "this spell can't be countered", and
        -- a required key would have meant editing every other card file to say
        -- nothing.
        <> ( case CardT.counterability c of
               Counterability.Counterable -> []
               Counterability.CantBeCountered -> [(Text.pack "counterability", counterabilityToJson (CardT.counterability c))]
           )
        <> ( if null (CardT.mulliganAction c)
               then []
               else [(Text.pack "mulliganAction", listTo effectToJson (CardT.mulliganAction c))]
           )
        <> ( if null (CardT.openingHandAction c)
               then []
               else [(Text.pack "openingHandAction", listTo effectToJson (CardT.openingHandAction c))]
           )
        <> ( case CardT.enchant c of
               Nothing -> []
               Just spec -> [(Text.pack "enchant", targetSpecToJson spec)]
           )
    )

getOpt :: Text -> [(Text, Value)] -> Value
getOpt k ps = Maybe.fromMaybe Json.jNull (Json.optField k ps)

jsonToBoolDefault :: Bool -> Value -> Either Text Bool
jsonToBoolDefault d value = case value of
  Null _ -> Right d
  Boolean b -> Right (Boolean.unwrap b)
  _ -> Left (Text.pack "expected a boolean")

-- An omitted set field decodes to empty. Lets an all-default field stay OUT of
-- the committed JSON, so existing files remain byte-identical (the same
-- precedent delayedAbilities and characteristicPT follow, P2/P4).
setFromDefault :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Set a)
setFromDefault f value = case value of
  Null _ -> Right Set.empty
  _ -> setFrom f value

-- An omitted list field decodes to empty, the list counterpart of
-- setFromDefault. Lets an all-default field stay OUT of the committed JSON, so
-- every existing card file remains byte-identical.
listFromDefault :: (Value -> Either Text a) -> Value -> Either Text [a]
listFromDefault f value = case value of
  Null _ -> Right []
  _ -> listFrom f value

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
  colorIndicator <- setFromDefault jsonToColor (getOpt (Text.pack "colorIndicator") ps)
  characteristicPT <- maybeFrom jsonToQuantity (getOpt (Text.pack "characteristicPT") ps)
  delayed <- mapFromDefault jsonToDelayedAbilities (getOpt (Text.pack "delayedAbilities") ps)
  playerAbilities <- listFromDefault jsonToPlayerStaticAbility (getOpt (Text.pack "playerAbilities") ps)
  blockRequirements <- listFromDefault jsonToBlockRequirement (getOpt (Text.pack "blockRequirements") ps)
  additionalCosts <- listFromDefault jsonToCostComponent (getOpt (Text.pack "additionalCosts") ps)
  alternativeCosts <- listFromDefault jsonToCost (getOpt (Text.pack "alternativeCosts") ps)
  mulliganAction <- listFromDefault jsonToEffect (getOpt (Text.pack "mulliganAction") ps)
  openingHandAction <- listFromDefault jsonToEffect (getOpt (Text.pack "openingHandAction") ps)
  enchant <- maybeFrom jsonToTargetSpec (getOpt (Text.pack "enchant") ps)
  counterability <- jsonToCounterabilityDefault (getOpt (Text.pack "counterability") ps)
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
        CardT.colorIndicator = colorIndicator,
        CardT.characteristicPT = characteristicPT,
        CardT.delayedAbilities = delayed,
        CardT.playerAbilities = playerAbilities,
        CardT.blockRequirements = blockRequirements,
        CardT.additionalCosts = additionalCosts,
        CardT.alternativeCosts = alternativeCosts,
        CardT.mulliganAction = mulliganAction,
        CardT.openingHandAction = openingHandAction,
        CardT.enchant = enchant,
        CardT.counterability = counterability
      }

printingToJson :: Printing.Printing -> Value
printingToJson (Printing.MkPrinting c) = cardToJson c

jsonToPrinting :: Value -> Either Text Printing.Printing
jsonToPrinting value = Printing.MkPrinting <$> jsonToCard value
