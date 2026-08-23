module Pawl.Engine.Projection where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Void as Void
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.HybridPhyrexian as HybridPhyrexian
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import Pawl.Types.Layer (Layer)
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSlot as TargetSlot
import Pawl.Types.Timestamp (Timestamp)
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- CR 613.1f's grant carries a whole quoted ability, and a card's abilities are
-- written against a whole Card (CR 707.8a).
type Modification = Modification.Modification (GrantedAbility.GrantedAbility Card.Type.Card)

-- CR 611.2: a modification a RESOLUTION created, widened. Exhaustive rather than
-- a catch-all, so a later arm carrying an ability has to be widened deliberately.
widenModification :: Modification.Modification Void.Void -> Modification
widenModification m = case m of
  Modification.GainAbility a -> Void.absurd a
  Modification.GainKeyword k -> Modification.GainKeyword k
  Modification.GainEnchant x -> Modification.GainEnchant x
  Modification.LoseAllAbilities -> Modification.LoseAllAbilities
  Modification.LoseNamedAbility n -> Modification.LoseNamedAbility n
  Modification.SetBasePowerToughness x -> Modification.SetBasePowerToughness x
  Modification.ModifyPowerToughness x -> Modification.ModifyPowerToughness x
  Modification.SetLandSubtype x -> Modification.SetLandSubtype x
  Modification.SetLandSubtypeToChosen -> Modification.SetLandSubtypeToChosen
  Modification.AddLandSubtype x -> Modification.AddLandSubtype x
  Modification.SetCreatureSubtype x -> Modification.SetCreatureSubtype x
  Modification.AddCreatureSubtype x -> Modification.AddCreatureSubtype x
  Modification.AddEveryCreatureSubtype -> Modification.AddEveryCreatureSubtype
  Modification.AddSubtype x -> Modification.AddSubtype x
  Modification.AddCardType x -> Modification.AddCardType x
  Modification.SetCardType x -> Modification.SetCardType x
  Modification.AddSupertype x -> Modification.AddSupertype x
  Modification.RemoveSupertype x -> Modification.RemoveSupertype x
  Modification.ChangeSubtypeWord x -> Modification.ChangeSubtypeWord x
  Modification.SetController x -> Modification.SetController x
  Modification.SetControllerToSource -> Modification.SetControllerToSource
  Modification.SetColor x -> Modification.SetColor x
  Modification.AddColor x -> Modification.AddColor x
  Modification.AddChosenColor -> Modification.AddChosenColor
  Modification.SwitchPowerToughness -> Modification.SwitchPowerToughness

-- CR 613.1: the layer a modification applies in. A classification, never the
-- modification's identity.
layer :: Modification -> Layer
layer m = case m of
  Modification.GainKeyword _ -> Layer.Ability
  -- CR 613.1f, not layer 4: CR 702.5a makes enchant a static ABILITY, so
  -- granting one is an ability-adding effect however much the clause it comes
  -- from ("becomes an Aura enchantment with enchant creature") also changes
  -- types.
  Modification.GainEnchant _ -> Layer.Ability
  Modification.GainAbility _ -> Layer.Ability
  Modification.LoseAllAbilities -> Layer.Ability
  -- CR 613.1f again, and the same layer as the wipe above: what differs is the
  -- SCOPE of the removal, never when it applies.
  Modification.LoseNamedAbility _ -> Layer.Ability
  Modification.SetBasePowerToughness {} -> Layer.SetPT
  Modification.ModifyPowerToughness {} -> Layer.ModifyPT
  Modification.SetLandSubtype _ -> Layer.Type
  Modification.SetLandSubtypeToChosen -> Layer.Type
  Modification.AddLandSubtype _ -> Layer.Type
  Modification.SetCreatureSubtype _ -> Layer.Type
  Modification.AddCreatureSubtype _ -> Layer.Type
  Modification.AddEveryCreatureSubtype -> Layer.Type
  -- CR 613.1d, and a regression fence rather than a proved behaviour: no board
  -- in the pool orders the one AddSubtype (Ygra, Eater of All's Food) against an
  -- effect in another layer, so answering any other layer here leaves the suite
  -- green.
  Modification.AddSubtype _ -> Layer.Type
  Modification.AddCardType _ -> Layer.Type
  Modification.SetCardType _ -> Layer.Type
  Modification.AddSupertype _ -> Layer.Type
  Modification.RemoveSupertype _ -> Layer.Type
  Modification.ChangeSubtypeWord {} -> Layer.Text
  Modification.SetController _ -> Layer.Control
  Modification.SetControllerToSource -> Layer.Control
  Modification.SetColor _ -> Layer.Color
  Modification.AddColor _ -> Layer.Color
  Modification.AddChosenColor -> Layer.Color
  Modification.SwitchPowerToughness -> Layer.SwitchPT

-- Apply one modification to characteristics-in-progress. P/T quantities are
-- evaluated against the CURRENT state (CR 604.2); Resolve freezes a resolution's
-- effects to literals (CR 608.2h / 611.2d), so this is the identity on them.
-- CR 109.5: a static ability's perspective is its SOURCE's controller, so `src`
-- supplies both that and the InSlot binding source. A CDA instead builds its
-- context from the object's own controller (applyCharacteristicPT, CR 604.3a(3)).
applyModification :: Count.ViewOf -> ObjectId -> GameState -> ObjectId -> Modification -> ProjectedCharacteristics -> ProjectedCharacteristics
applyModification viewOf src gs oid m pc =
  let context = Filter.contextFor (controllerOf src gs) (Just src)
   in case m of
        -- CR 613.1f layer 6: a grant adds an ability, so two grants of the same
        -- keyword count twice.
        Modification.GainKeyword k ->
          pc {PC.keywords = Map.insertWith (+) k 1 (PC.keywords pc)}
        -- CR 613.1f layer 6 / CR 702.5c: an APPEND, since "if an Aura has
        -- multiple instances of enchant, all of them apply" -- the printed
        -- instances are in the seed and this adds to them, so
        -- Pawl.Engine.Card.foldEnchant conjoins the two exactly as it conjoins two
        -- printed ones.
        Modification.GainEnchant slot ->
          pc {PC.enchant = PC.enchant pc <> [slot]}
        -- CR 613.1f layer 6: one whole quoted ability. Appended to the card's own
        -- printed abilities, which is what makes it the RECEIVER's (CR 113.7, CR
        -- 602.2, CR 603.3a, CR 303.4e) and lets two grants stack in CR 613.7
        -- timestamp order. The case is on CR 113.3's ability KIND, which decides
        -- only which of the two lists the ability joins -- nothing here reads what
        -- the ability does.
        Modification.GainAbility g -> case g of
          GrantedAbility.Activated a ->
            pc {PC.activatedAbilities = PC.activatedAbilities pc <> [a]}
          GrantedAbility.Triggered t ->
            pc {PC.triggeredAbilities = PC.triggeredAbilities pc <> [t]}
        -- CR 604.3: a CDA is a static ability, so this loses it too.
        Modification.LoseAllAbilities ->
          pc
            { PC.keywords = Map.empty,
              PC.characteristicPT = Nothing,
              PC.activatedAbilities = [],
              PC.replacementEffects = [],
              PC.triggeredAbilities = [],
              -- CR 702.5a makes enchant an ability, so CR 613.1f's removal takes
              -- it with the rest. Unproven: Humility reaches only creatures and
              -- nothing in the pool wipes a noncreature permanent's abilities, so
              -- dropping this line leaves the suite green.
              PC.enchant = []
            }
        -- CR 613.1f layer 6, the wipe above narrowed to one name: every ability
        -- the card gave this name goes, and every other ability stays. A Licid
        -- keeps "Enchanted creature has flying" while losing the ability that
        -- animated it, which is the whole difference between the two arms.
        --
        -- Reaches PC.activatedAbilities alone, that being the only list whose
        -- members carry a name (#2134). Nothing else is emptied -- not the
        -- keywords, not the CDA -- because the clause names one ability.
        Modification.LoseNamedAbility n ->
          pc {PC.activatedAbilities = filter ((/= Just n) . ActivatedAbility.name) (PC.activatedAbilities pc)}
        Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) ->
          pc
            { PC.power = setPT (PC.power pc) (Quantity.evaluate viewOf context gs oid p),
              PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate viewOf context gs oid t)
            }
        Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) ->
          pc
            { PC.power = addPT (PC.power pc) (Quantity.evaluate viewOf context gs oid p),
              PC.toughness = addPT (PC.toughness pc) (Quantity.evaluate viewOf context gs oid t)
            }
        Modification.AddLandSubtype s -> gainSubtype s pc
        -- CR 205.1a/205.1b: setting a subtype replaces only the creature types
        -- (CR 205.3m), strips no abilities and touches no card type. The strip
        -- runs whether or not CR 205.3d then lets the new type in, which no board
        -- can tell apart: a creature type is only ever present on an object one
        -- of whose card types correlates with it, so an object this arm refuses
        -- has none to strip.
        Modification.SetCreatureSubtype s ->
          gainSubtype s pc {PC.subtypes = Set.filter (not . Subtype.isCreatureType) (PC.subtypes pc)}
        -- CR 205.1b's add: every creature type already present is kept.
        Modification.AddCreatureSubtype s -> gainSubtype s pc
        -- The same add over CR 205.3m's whole list (CR 702.73a).
        -- applySubtypeDefining writes the same thing at the start of this layer
        -- (CR 613.3); this runs in timestamp order (CR 613.7).
        Modification.AddEveryCreatureSubtype ->
          pc {PC.subtypes = Set.union (gainableSubtypes pc Subtype.everyCreatureType) (PC.subtypes pc)}
        -- CR 205.1b's add again, over CR 205.3g's and CR 205.3h's families: the
        -- object keeps every subtype it had. Literally the two adds above, and
        -- deliberately so -- what differs is CR 612.2's gate, which lives on the
        -- constructor rather than here.
        Modification.AddSubtype s -> gainSubtype s pc
        Modification.AddCardType t ->
          pc {PC.cardTypes = Set.insert t (PC.cardTypes pc)}
        -- CR 205.1a's set, and the whole of it: the new card type replaces the
        -- existing ones; instant and sorcery survive; and a subtype whose family
        -- correlates with no card type the object now has goes with the type that
        -- carried it. A subtype Pawl.Engine.Subtype cannot classify answers with
        -- the empty set and is kept. No ability clause: CR 205.1a says nothing
        -- about abilities.
        Modification.SetCardType t ->
          let kept = Set.filter retainedThroughCardTypeSet (PC.cardTypes pc)
              types = Set.insert t kept
           in pc
                { PC.cardTypes = types,
                  PC.subtypes = Set.filter (correspondsTo types) (PC.subtypes pc)
                }
        -- CR 205.4b: a gain inserts into the supertype set. CR 205.4 gives an
        -- object a SET of supertypes, so a second grant does not stack.
        Modification.AddSupertype t ->
          pc {PC.supertypes = Set.insert t (PC.supertypes pc)}
        -- CR 205.4b's other direction, a delete; removing one the object never had
        -- is the identity rather than an error.
        Modification.RemoveSupertype t ->
          pc {PC.supertypes = Set.delete t (PC.supertypes pc)}
        -- CR 305.7's set, with the type written into card data.
        Modification.SetLandSubtype s -> setLandSubtypeTo s pc
        -- CR 305.7's set again, with the type read off the source's own entry
        -- choice (CR 614.1c). An unchosen source sets and strips nothing rather
        -- than guessing a type, the posture AddChosenColor takes toward an
        -- unchosen colour. That leaves this arm disagreeing with setsLandSubtype,
        -- which classifies by CONSTRUCTOR and so would still strip the land's
        -- abilities; unreachable, since the only producer is an entry replacement
        -- whose rewrite writes the field before the permanent is on the
        -- battlefield to be projected.
        Modification.SetLandSubtypeToChosen ->
          case Game.lookupObject src gs >>= Object.chosenSubtype of
            Nothing -> pc
            Just s -> setLandSubtypeTo s pc
        -- CR 612.1/612.2: a text-changing effect swaps a subtype word in the type
        -- line, the rules text, the keywords (CR 702.14a) and the CDA (CR 208.2a /
        -- 604.3). Layer 3, so it folds before layer 4, and it reaches nothing a
        -- later layer GRANTS (CR 613.1c/613.1d, CR 612.3).
        --
        -- Map.mapKeysWith (+) rather than Map.mapKeys: the swap can collide two
        -- keys, as with islandwalk and swampwalk hacked Island -> Swamp. Rule
        -- 702's minted abilities are built after this fold, so the pair is
        -- recorded in PC.subtypeWordChanges for the mint, in CR 613.1 order.
        --
        Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord from to) ->
          let pairs = [(from, to)]
              pc' =
                pc
                  { PC.keywords = Map.mapKeysWith (+) (Filter.rewriteKeyword pairs) (PC.keywords pc),
                    PC.activatedAbilities = fmap (rewriteActivatedAbility pairs) (PC.activatedAbilities pc),
                    PC.triggeredAbilities = fmap (rewriteTriggeredAbility pairs) (PC.triggeredAbilities pc),
                    PC.replacementEffects = fmap (rewritePrintedReplacement pairs) (PC.replacementEffects pc),
                    PC.characteristicPT = fmap (rewriteCharacteristicPT pairs) (PC.characteristicPT pc),
                    PC.subtypeWordChanges = PC.subtypeWordChanges pc <> [ChangeSubtypeWord.MkChangeSubtypeWord from to]
                  }
           in if Set.member from (PC.subtypes pc')
                then pc' {PC.subtypes = Set.insert to (Set.delete from (PC.subtypes pc'))}
                else pc'
        -- CR 613.1b layer 2: controllerOf reads GameState.continuousEffects
        -- directly. Identity here to keep gather/project's walk total.
        Modification.SetController _ -> pc
        Modification.SetControllerToSource -> pc
        Modification.SetColor cs ->
          -- CR 105.3: the new colours replace all previous ones.
          pc {PC.colors = cs}
        -- CR 105.3's parenthetical: an "in addition" colour adds.
        Modification.AddColor cs ->
          pc {PC.colors = Set.union cs (PC.colors pc)}
        -- CR 105.3's parenthetical, colour read off the source's entry choice.
        Modification.AddChosenColor ->
          case Game.lookupObject src gs >>= Object.chosenColor of
            Nothing -> pc
            Just c -> pc {PC.colors = Set.insert c (PC.colors pc)}
        -- CR 613.4d.
        Modification.SwitchPowerToughness ->
          pc {PC.power = PC.toughness pc, PC.toughness = PC.power pc}

-- CR 205.3d: an object can't gain a subtype that doesn't correspond to one of
-- its types. CR 205.1a's removal clause is the same question in the other
-- direction, so the SetCardType arm asks this too -- one predicate, so a subtype
-- a card-type set would strip is one a later grant cannot put back.
--
-- The card types are the ones the fold has produced SO FAR, which is what lets
-- an effect grant the card type and the subtype together: Song of the Dryads,
-- Ashaya, Grist and Life and Limb each list their card-type modifications ahead
-- of the subtypes those types carry, and applyModification folds a static
-- ability's modifications in that order.
--
-- Not implemented: CR 613.1 applies one effect's parts together, so the
-- correspondence should be judged over the whole effect rather than over the
-- part of it the fold has reached; a card naming the subtype ahead of the card
-- type would have the subtype refused (#2041).
correspondsTo :: Set CardType.CardType -> Subtype.Type.Subtype -> Bool
correspondsTo types subtype =
  let family = Subtype.correlatedCardTypes subtype
   in Set.null family || not (Set.disjoint family types)

-- One grant, dropped when CR 205.3d refuses it. The refusal is proved at the
-- land-type direction (Pawl.ProjectionSpec's Synthetic Marsh Song case) and is a
-- regression fence at the creature-type one, where every grant in data/cards
-- names creatures in its affected set (Turn to Frog, Slivdrazi Monstrosity) or
-- adds the Creature card type first (Life and Limb, Grist).
gainSubtype :: Subtype.Type.Subtype -> ProjectedCharacteristics -> ProjectedCharacteristics
gainSubtype s pc
  | correspondsTo (PC.cardTypes pc) s = pc {PC.subtypes = Set.insert s (PC.subtypes pc)}
  | otherwise = pc

-- The same refusal over a whole set, for the two grants that write CR 205.3m's
-- list at once.
--
-- A regression fence rather than a proved behaviour: neutering it leaves the
-- whole suite green, because neither caller can reach an object CR 205.3d
-- refuses. Maskwood Nexus names creatures in its affected set, and CR 702.73b
-- puts changeling on creature and Kindred cards, both of which correlate.
gainableSubtypes :: ProjectedCharacteristics -> Set Subtype.Type.Subtype -> Set Subtype.Type.Subtype
gainableSubtypes pc = Set.filter (correspondsTo (PC.cardTypes pc))

-- CR 205.1a's named exception to its own set: instant and sorcery are retained.
retainedThroughCardTypeSet :: CardType.CardType -> Bool
retainedThroughCardTypeSet t = t == CardType.Instant || t == CardType.Sorcery

-- CR 305.7's strip, shared by both modifications that set a land's subtype. It
-- does the subtype and ability clauses; the new basic type's mana ability rides
-- the subtype and is read at the mana call site (CR 305.6). An ability landing on
-- OTHER objects is gated instead: setSubtypeStripped for an ability whose effect
-- had not started applying by the end of layer 4, which is where an object that
-- became a land AT layer 4 is caught, and liveGiven for the rest.
--
-- CR 305.7's copiable-effects clause needs nothing beyond this one strip: the
-- fold is SEEDED from copiableCharacteristics (CR 613.2c), so an ability a layer-1
-- copy effect gave the land is in these lists by the time layer 4 runs, and goes
-- with the printed text rather than surviving it. Proved by Pawl.CopySpec's "CR
-- 305.7 Blood Moon strips the abilities Vesuva copied from another land".
--
-- CR 205.3d gates the whole of it, strip included: both clauses of CR 305.7 are
-- about what happens to a LAND, so an object with no Land card type is left
-- exactly as it was rather than losing its abilities to a subtype it cannot
-- gain.
setLandSubtypeTo :: Subtype.Type.Subtype -> ProjectedCharacteristics -> ProjectedCharacteristics
setLandSubtypeTo s pc
  | not (correspondsTo (PC.cardTypes pc) s) = pc
  | otherwise =
      pc
        { PC.subtypes = Set.insert s (Set.filter (not . Subtype.isLandType) (PC.subtypes pc)),
          PC.keywords = Map.empty,
          PC.characteristicPT = Nothing,
          PC.activatedAbilities = [],
          PC.replacementEffects = [],
          PC.triggeredAbilities = [],
          -- CR 305.7's strip reaches an enchant ability for CR 613.1f's reason
          -- above. Unproven for the same reason: no board in the pool sets the
          -- land subtype of a permanent that has one.
          PC.enchant = []
        }

-- CR 613.4b: layer 7b establishes base P/T, so an object with no printed P/T
-- gains it. Contrast addPT (7c), which only modifies.
setPT :: Maybe Integer -> Maybe Integer -> Maybe Integer
setPT base new = case (base, new) of
  (_, Just n) -> Just n
  (Just b, Nothing) -> Just b
  (Nothing, Nothing) -> Nothing

-- Layer 7c adds; an unevaluable delta leaves the value, a land stays without.
addPT :: Maybe Integer -> Maybe Integer -> Maybe Integer
addPT base delta = case (base, delta) of
  (Just b, Just d) -> Just (b + d)
  (Just b, Nothing) -> Just b
  (Nothing, _) -> Nothing

-- One layer's worth of a continuous effect. Projection-internal; not a domain
-- type.
data Gathered = MkGathered
  { -- Which effect this part belongs to: Just (source, the ability's index) for a
    -- static ability with parts in more than one layer, Nothing otherwise. CR
    -- 613.6's affected-set decision is keyed on this pair and reused (projectWith).
    gEffect :: !(Maybe (ObjectId, Natural)),
    gSource :: ObjectId,
    gAffected :: Affected.Affected,
    gLayer :: Layer,
    -- CR 613.6's decision point: the lowest layer reached by any part of this
    -- part's effect. A caller outside the fold carries it rather than re-deriving
    -- it at the wrong layer. Equal to gLayer for a one-part effect.
    gLowest :: Layer,
    gTimestamp :: Timestamp,
    gModification :: Modification
  }

-- CR 611.2c / 613: does the effect from `source` apply to `oid`, given the
-- PARTIAL projection built by the layers below this one? CR 109.5: an
-- affected-set filter's "you" is the SOURCE's controller. For callers OUTSIDE the
-- layer fold; a caller inside wants affectsGiven with its own layer's bound.
affects :: ObjectId -> ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affects source oid a partial gs = affectsGiven (fullView gs) source oid a partial gs

-- affects with the reader for the objects a filter reaches past the candidate --
-- an ATTACHED candidate's host and the permanents attached TO a candidate (CR
-- 701.3a, CR 303.4b), the CR 702.178a gate's board -- which
-- every caller has to pick at the same depth as `partial`. See
-- viewOfCharacteristics for why the depths must agree.
affectsGiven :: Count.ViewOf -> ObjectId -> ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affectsGiven peers source oid a partial gs = case a of
  Affected.TheseObjects s -> Set.member oid s
  -- CR 303.4m: read the SOURCE's attachment, not the candidate's. An unattached
  -- source, or one attached to a player, names no object (CR 702.5d's
  -- enchant-player Auras go through AttachedPlayerControls below).
  Affected.Attached -> hostOf source gs == Just oid
  Affected.Matching f ->
    let -- CR 109.5: "you" is the SOURCE's controller. Safe to force: controlGrants
        -- consults no liveness gate and so cannot re-enter this function.
        perspective = controllerOf source gs
     in Set.member oid (GameState.battlefield gs)
          && Filter.matches (Filter.contextFor perspective (Just source)) (viewOfCharacteristics peers oid partial (controllerOf oid gs) (countersOf oid gs) gs) f
  -- Matching's body without the battlefield conjunct.
  Affected.MatchingAnywhere f ->
    let perspective = controllerOf source gs
     in Filter.matches (Filter.contextFor perspective (Just source)) (viewOfCharacteristics peers oid partial (controllerOf oid gs) (countersOf oid gs) gs) f
  -- Matching's body with that conjunct NEGATED rather than dropped.
  Affected.MatchingOffBattlefield f ->
    let perspective = controllerOf source gs
     in not (Set.member oid (GameState.battlefield gs))
          && Filter.matches (Filter.contextFor perspective (Just source)) (viewOfCharacteristics peers oid partial (controllerOf oid gs) (countersOf oid gs) gs) f
  -- CR 303.4b / 303.4m: the source's attachment again, read for the PLAYER it
  -- names. The Filter's perspective stays the source's controller (CR 109.5), not
  -- the enchanted player's. The candidate's controller is bound once and used
  -- twice, since controllerOf rebuilds controlGrants on every call.
  Affected.AttachedPlayerControls f -> case Game.lookupObject source gs >>= Object.attachedTo of
    Just (Recipient.ToPlayer pid) ->
      let controller = controllerOf oid gs
       in Set.member oid (GameState.battlefield gs)
            && controller == Just pid
            && Filter.matches (Filter.contextFor (controllerOf source gs) (Just source)) (viewOfCharacteristics peers oid partial controller (countersOf oid gs) gs) f
    _ -> False

-- The characteristics view of an object: its CR 613 projection and its projected
-- controller (CR 613.1b; Nothing when the id is unknown). Rule 613.1 names no
-- zone, so a card in a hidden zone reads through this same view.
viewOfObject :: ObjectId -> GameState -> Filter.View
viewOfObject oid gs = viewOfObjectGiven Map.empty (controlGrants gs) oid gs

-- viewOfObject against a pre-projected board and a precomputed grant list. See
-- projectGiven for what the board is and when it is valid.
viewOfObjectGiven :: Map ObjectId ProjectedCharacteristics -> [ControlGrant] -> ObjectId -> GameState -> Filter.View
viewOfObjectGiven pcs grants oid gs =
  -- A host, and an attacher, are read the same way this object is. Recursive, and
  -- safe for the reason viewOfCharacteristics gives: both attachment views are
  -- lazy, so the recursion is driven by a filter's own AttachedTo / HasAttached
  -- nesting, which is finite.
  viewOfCharacteristics (\host -> Just (viewOfObjectGiven pcs grants host gs)) oid (projectGiven pcs oid gs) (controllerOfGiven grants Set.empty oid gs) (countersOf oid gs) gs

-- CR 112.2 / 601.2a: the view of a SPELL on the stack, whose controller is the
-- player who cast it. The caster is passed in rather than rediscovered, so a
-- caller reading GameEvent.SpellCast need not have seen the object land.
-- Characteristics come from the live projection, which is CR 601.2i's own order.
viewOfSpell :: PlayerId.PlayerId -> ObjectId -> GameState -> Filter.View
viewOfSpell caster oid gs = viewOfCharacteristics (fullView gs) oid (project oid gs) (Just caster) (countersOf oid gs) gs

-- The ViewOf for callers OUTSIDE the CR 613 layer fold; `viewUpTo` below is the
-- bounded counterpart for callers INSIDE it. Both are Count.ViewOf, so picking
-- the wrong one is a silent wrong answer, and a non-terminating one for a caller
-- inside the fold (see viewOfCharacteristics).
fullView :: GameState -> Count.ViewOf
fullView gs oid = Just (viewOfObject oid gs)

-- CR 113.7a / 608.2h: `fullView`, except that the one object named by `src` is
-- read from last known information once it no longer exists. Scoped to `src`
-- alone: the other object a resolving effect reads is its target, where CR 608.2b
-- wants the blank answer fullView gives. viewWithLastKnownAnywhere below is the
-- unscoped counterpart.
viewWithLastKnown :: ObjectId -> GameState -> Count.ViewOf
viewWithLastKnown src gs oid =
  if oid == src
    then viewWithLastKnownAnywhere gs oid
    else fullView gs oid

-- CR 608.2h for EVERY id the reader is aimed at rather than for one named object:
-- what an intervening "if" wants, since CR 603.4's clause may be about the object
-- the EVENT named. Rule 702.100a's evolve is the case. viewWithLastKnown above is
-- this scoped to one id and delegates here, since its callers hold a resolving
-- source, whose other object is a TARGET -- and CR 608.2b wants a blank answer.
--
-- Nothing when the object is gone and nothing was filed, which lands on the no-op
-- every caller gives an unevaluable quantity. The controller and the COUNTERS come
-- from the record: CR 122.2 made the counters cease to exist with the object.
viewWithLastKnownAnywhere :: GameState -> Count.ViewOf
viewWithLastKnownAnywhere gs oid =
  if Map.member oid (GameState.objects gs)
    then fullView gs oid
    else
      fmap
        (\lk -> viewOfCharacteristics (fullView gs) oid (LastKnown.characteristics lk) (Just (LastKnown.controller lk)) (LastKnown.counters lk) gs)
        (Map.lookup oid (GameState.lastKnown gs))

-- CR 608.2h: this object's last known information, and only when the id names
-- nothing, so a caller falls through to its live reader. Shared by the two
-- readers below so the rule cannot mean one thing for keywords and another for
-- control.
lastKnownOf :: ObjectId -> GameState -> Maybe LastKnown.LastKnown
lastKnownOf oid gs =
  if Map.member oid (GameState.objects gs)
    then Nothing
    else Map.lookup oid (GameState.lastKnown gs)

-- keywordsOf with CR 608.2h's fallback (CR 702.2e, CR 702.15c, CR 702.90d); toxic
-- (rule 702.164) has no such clause and rides this by uniformity.
keywordsWithLastKnown :: ObjectId -> GameState -> Map Keyword Natural
keywordsWithLastKnown oid gs = case lastKnownOf oid gs of
  Just lk -> PC.keywords (LastKnown.characteristics lk)
  Nothing -> keywordsOf oid gs

-- controllerOf with the same fallback (CR 702.15b for why a controller is wanted;
-- CR 608.2h for the authority). LastKnown.controller is a PlayerId, so this
-- answers Just wherever the live reader would answer Nothing for a gone source.
controllerWithLastKnown :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerWithLastKnown oid gs = case lastKnownOf oid gs of
  Just lk -> Just (LastKnown.controller lk)
  Nothing -> controllerOf oid gs

-- powerGiven with the same fallback, on CR 608.2b's own sentence about target
-- re-validation -- so a mentor (CR 702.134a) killed in response leaves its
-- trigger's target legal rather than fizzling it.
powerWithLastKnownGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Maybe Integer
powerWithLastKnownGiven pcs oid gs = case lastKnownOf oid gs of
  Just lk -> PC.power (LastKnown.characteristics lk)
  Nothing -> powerGiven pcs oid gs

-- The ViewOf a count gets when it is evaluated while `bound` is being applied:
-- candidates projected through the layers BEFORE that one. EVERY object in the
-- game, in whatever zone -- CR 613.1 names no zone. WHICH effects reach it is the
-- affected set's question, and four of the six arms reach off the battlefield:
-- Affected.TheseObjects (a fixed id set, CR 611.2c), MatchingAnywhere,
-- MatchingOffBattlefield, and Attached, which reads the SOURCE's host and so
-- carries no gate of its own. Only Matching and AttachedPlayerControls spell out
-- a battlefield conjunct.
--
-- Nothing for an id that names no object: CR 400.7 makes a departed object a new
-- one, and a caller handed a dead id wants the no-op an unevaluable quantity
-- already gives. fullView answers Just there, which is why
-- viewWithLastKnownAnywhere has to guard it. CR 701.3a: an attached candidate's
-- HOST is read at this same bound, and so are the permanents attached TO it,
-- which keeps a Filter.AttachedTo or Filter.HasAttached reached from inside the
-- fold out of a loop.
viewUpTo :: Layer -> [Gathered] -> GameState -> Count.ViewOf
viewUpTo bound cands gs oid =
  if Map.member oid (GameState.objects gs)
    then Just (viewOfCharacteristics (viewUpTo bound cands gs) oid (projectUpTo bound cands oid gs) (controllerOf oid gs) (countersOf oid gs) gs)
    else Nothing

-- The characteristics view of a printed card, from the FACE alone. The axes that
-- only an OBJECT can have are Nothing or empty, and each says so at its field.
--
-- Its readers are Pawl.ProjectionSpec's, which ask about a printed face with no
-- game around it. A reader that holds an OBJECT takes viewOfObject instead, in
-- whatever zone the object sits -- see #1911, which moved the last of them.
viewOfCard :: Face.Face Card.Type.Card -> Filter.View
viewOfCard face =
  let typeLine = Face.typeLine face
   in Filter.MkView
        { -- CR 201.1 off the printed FACE. A multi-faced card's combined view
          -- carries the halves joined for rendering (Engine.Card.merge2); CR
          -- 709.4a's set is viewOfCharacteristics', which has an id to ask
          -- Game.namesOf about.
          Filter.names = Set.singleton (Face.name face),
          Filter.cardTypes = TypeLine.types typeLine,
          Filter.supertypes = TypeLine.supertypes typeLine,
          -- CR 604.3 / 702.114a: a CDA functions in all zones, and this view
          -- enters no CR 613 fold, so devoid is applied here.
          Filter.colors =
            if definesColorless (Face.keywords face)
              then Set.empty
              else printedColorsOf face,
          -- CR 604.3 / 702.73a, the same one layer down: changeling "works
          -- everywhere".
          Filter.subtypes =
            if definesEveryCreatureType (Face.keywords face)
              then Set.union Subtype.everyCreatureType (TypeLine.subtypes typeLine)
              else TypeLine.subtypes typeLine,
          -- CR 702: read off the printed face, like the type line above.
          Filter.keywords = Face.keywords face,
          -- CR 208.1 read off the PRINTED power box -- see printedPower below.
          Filter.power = printedPower face,
          -- CR 208.1's other half off the printed toughness box.
          Filter.toughness = printedToughness face,
          -- CR 202.3: printed on the card, and rule 202.3 names no zone.
          Filter.manaValue = Just (Quantity.manaValueOf face),
          Filter.controller = Nothing,
          -- CR 108.3 gives an owner to a card IN THE GAME; this builder describes
          -- a printed FACE, so there is nothing to read Object.owner off.
          --
          -- Not implemented: the owner of a card matched OFF the battlefield,
          -- where viewUpTo falls back here with an object id in hand and CR 400.3
          -- would answer from Object.owner (#1068).
          Filter.owner = Nothing,
          -- Not an object, so no identity for IsSource to compare.
          Filter.identity = Nothing,
          Filter.playerIdentity = Nothing,
          -- CR 506.3 / 509.1a: a card off the battlefield never attacked or
          -- blocked; CR 303.4b: nor is it attached to anything.
          Filter.attacking = False,
          Filter.blocking = False,
          Filter.blocked = False,
          Filter.attackedThisTurn = False,
          -- CR 701.17a mills an OBJECT; this builder describes a printed FACE.
          -- viewOfCharacteristics is the view that holds an id and answers.
          Filter.milledThisTurn = False,
          -- CR 120.1a: damage is dealt to a battle, a creature or a
          -- planeswalker, and this builder describes a printed FACE rather than
          -- a permanent. viewOfCharacteristics is the view that holds an id and
          -- answers.
          Filter.dealtDamageThisTurn = False,
          Filter.attachedToView = Nothing,
          -- CR 303.4b's mirror, and Nothing for the same reason: a printed face
          -- is not an object, so no permanent's Object.attachedTo names it.
          Filter.attachedViews = [],
          Filter.attachedTo = Nothing,
          -- CR 701.3a: only Pawl.Engine.Resolve's AttachTarget arm fills this
          -- field, and its candidates are battlefield permanents.
          Filter.canHostSubject = False,
          -- CR 701.3a's other side: only Pawl.Engine.Resolve's Effect.Search arm
          -- fills this field, and it overlays it onto viewOfObject rather than
          -- reaching this builder, which holds a printed FACE and no board.
          Filter.canAttachToSubject = False,
          -- CR 111.6: "A token isn't a card." CR 704.5d already made a token in
          -- any zone this builder describes cease to exist.
          Filter.token = False,
          Filter.tapped = False,
          -- CR 701.27g asks about a permanent on the battlefield; this is a
          -- printed FACE with no object behind it, so the rule's own answer is
          -- False rather than an unknown.
          Filter.transformed = False,
          -- CR 122.1a-b: a counter can sit on a card off the battlefield, but this
          -- builder describes a printed FACE, so there is nothing to be on.
          Filter.counters = Map.empty,
          -- CR 701.54b: the designation rides an OBJECT, and CR 701.54a gives it
          -- only to a battlefield permanent.
          Filter.ringBearerFor = Nothing,
          -- The designations ride an OBJECT, and each of those rules gives its
          -- designation only to a permanent.
          Filter.designations = Set.empty,
          -- CR 716.2b gives a level to a PERMANENT, and this builder describes a
          -- printed face.
          Filter.classLevel = Nothing,
          Filter.kicked = False,
          -- CR 601.2h pays the cost of a SPELL, and this builder describes a
          -- printed face.
          Filter.manaSpentTags = Set.empty,
          -- CR 602.1 / 605.1a off the PRINTED face: the card's printed abilities
          -- plus rule 702's HAND ones (CR 702.29b, CR 702.77b), not the
          -- battlefield ones, which are minted from the post-layer keyword map.
          Filter.nonManaActivatedAbility =
            not
              ( all
                  ManaAbility.isManaAbility
                  (Face.activatedAbilities face <> Keyword.handAbilitiesOf (Face.keywords face))
              )
        }

-- CR 208.1's PRINTED power box, for a card off the battlefield. Nothing for a
-- face with no power box, since CR 208.1 gives power only to creature cards.
--
-- CR 208.2b's zero is the STAR's answer here, and only here. Deliberately NOT
-- Quantity.evaluate's Star arm, which stays Nothing: there a star that survived
-- baseCharacteristics is a hole rather than a zero. A face with a characteristicPT
-- answers Nothing here, since CR 208.2a's number is applyCharacteristicPT's, in
-- every zone (CR 604.3).
printedPower :: Face.Face Card.Type.Card -> Maybe Integer
printedPower face = case Face.characteristicPT face of
  Just _ -> Nothing
  Nothing -> case fmap Power.unwrap (Face.power face) of
    Just (Quantity.Type.Literal n) -> Just n
    Just Quantity.Type.Star -> Just 0
    -- Every other shape: no power box at all, or a box holding neither a number
    -- nor CR 208.2's bare star. The latter is unreachable -- a composite box like
    -- 1+* comes with a characteristicPT and left through the arm above.
    _ -> Nothing

-- printedPower's mirror, arm for arm, on the printed toughness box. CR 208.2b's
-- sentence names power and toughness together, so the star reads 0 here too.
printedToughness :: Face.Face Card.Type.Card -> Maybe Integer
printedToughness face = case Face.characteristicPT face of
  Just _ -> Nothing
  Nothing -> case fmap Toughness.unwrap (Face.toughness face) of
    Just (Quantity.Type.Literal n) -> Just n
    Just Quantity.Type.Star -> Just 0
    _ -> Nothing

-- CR 508.3a: does this event record THIS object being declared as an attacker?
-- Only Combat.declareAttackers appends one, so CR 508.4's creature put onto the
-- battlefield attacking stays out.
declaredIt :: ObjectId -> GameEvent.GameEvent -> Bool
declaredIt oid event = case event of
  GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared declared _ _) -> declared == oid
  _ -> False

-- CR 701.17a: does this event record THIS object as one of a mill's cards? Only
-- Resolve's Mill arm appends one, so a surveil's or an explore's bin stays out.
milledIt :: ObjectId -> GameEvent.GameEvent -> Bool
milledIt oid event = case event of
  GameEvent.Milled (Milled.MkMilled _ cards) -> Foldable.elem oid cards
  _ -> False

-- Shared assembly: fill a View from a projection's characteristics, a supplied
-- controller and supplied counters.
--
-- Counters and `peers` come in as arguments because CR 109.3's characteristic
-- list holds neither: only the caller knows whether it reads a live object or CR
-- 608.2h's record of one, and only the caller knows how deep the fold it stands
-- in has got. `peers` must be a bounded reader -- a full projection taken here
-- re-enters gather, whose CR 604.2 gate is object-independent, and loops.
viewOfCharacteristics :: Count.ViewOf -> ObjectId -> ProjectedCharacteristics -> Maybe PlayerId.PlayerId -> Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural -> GameState -> Filter.View
viewOfCharacteristics peers oid pc controller counters gs =
  Filter.MkView
    { -- CR 201.1 / 709.4a off the PROJECTION: names are copiable (CR 707.2), so a
      -- Clone answers to what it copied.
      Filter.names = PC.names pc,
      Filter.cardTypes = PC.cardTypes pc,
      -- CR 205.4 / 613.1d off the PROJECTION: layer 4 writes supertypes too.
      Filter.supertypes = PC.supertypes pc,
      Filter.colors = PC.colors pc,
      Filter.subtypes = PC.subtypes pc,
      -- CR 109.3 / 613.1f: abilities are characteristics and layer 6 writes them.
      -- keysSet because PC.keywords counts instances and HasKeyword asks membership.
      Filter.keywords = Map.keysSet (PC.keywords pc),
      Filter.power = PC.power pc,
      Filter.toughness = PC.toughness pc,
      -- CR 202.3 / 707.2 off the PROJECTION: mana cost is copiable, so layer 1
      -- replaces it. The printed cost is read in baseCharacteristics.
      Filter.manaValue = PC.manaValue pc,
      Filter.controller = controller,
      -- CR 108.3 / 110.2 / 111.2: read off the OBJECT rather than through the
      -- `controller` parameter, since layer 2 has already moved control and
      -- nothing moves ownership. Nothing for an id naming nothing (CR 608.2h,
      -- #1069).
      Filter.owner = fmap Object.owner (Game.lookupObject oid gs),
      Filter.identity = Just oid,
      Filter.playerIdentity = Nothing,
      -- CR 508.1k: a combat status, not a characteristic (CR 109.3).
      Filter.attacking = Map.member oid (Combat.attackers (GameState.combat gs)),
      -- CR 509.1g: likewise. Combat.blockers is keyed by ATTACKER, so blocking is
      -- membership in some attacker's set rather than a key lookup.
      Filter.blocking = any (Set.member oid) (Map.elems (Combat.blockers (GameState.combat gs))),
      -- CR 509.1h: the key lookup the line above is careful not to be. Stays True
      -- once every creature blocking it has left combat.
      Filter.blocked = Map.member oid (Combat.blockers (GameState.combat gs)),
      -- CR 608.2i: from the turn's event log, which CR 511.3 does not clear.
      Filter.attackedThisTurn = any (declaredIt oid . LoggedEvent.event) (GameState.events gs),
      -- CR 701.17a / 608.2i: the same log, read for the mills.
      Filter.milledThisTurn = any (milledIt oid . LoggedEvent.event) (GameState.events gs),
      -- CR 120.1 / 608.2i: the same log again, read for the damage. Never
      -- Object.damage -- CR 120.6 removes the marks on a regeneration and CR
      -- 120.3d/120.3e mark none at all for wither or infect, and either creature
      -- was still dealt damage this turn.
      Filter.dealtDamageThisTurn = any ((== Just oid) . Game.damagedObject . LoggedEvent.event) (GameState.events gs),
      -- CR 701.3a: not a characteristic, so the attachment comes off
      -- Object.attachedTo -- but the HOST's characteristics are projected, so it
      -- arrives as a view of its own read through `peers` (CR 613.1). CR 303.4 /
      -- 110.1: narrowed to a host on the battlefield, which is what makes
      -- `AttachedTo (And [])` mean "attached to a permanent". The view under the
      -- Just must stay lazy -- `peers` will project again.
      Filter.attachedToView =
        Game.lookupObject oid gs
          >>= Object.attachedTo
          >>= Recipient.objectOf
          >>= \host -> if Set.member host (GameState.battlefield gs) then peers host else Nothing,
      -- CR 303.4b / 301.5a with the arrow turned round: the permanents attached TO
      -- this candidate. pawl keeps the attachment on the ATTACHED permanent, so
      -- there is nothing to look up from this side and the index is built by
      -- sweeping the battlefield -- which also supplies CR 110.1's narrowing for
      -- free, `attachedToView` above having to state it. Each attacher's own
      -- characteristics come through `peers`, at this caller's depth, which is
      -- what keeps a HasAttached reached from inside the layer fold out of a loop.
      -- The list must stay lazy in its spine: nothing forces the sweep unless a
      -- Filter names the atom.
      Filter.attachedViews =
        Maybe.mapMaybe
          peers
          [ attacher
          | attacher <- Set.toList (GameState.battlefield gs),
            (Game.lookupObject attacher gs >>= Object.attachedTo >>= Recipient.objectOf) == Just oid
          ],
      -- CR 701.3a / 301.5a: the same attachment as the HOST'S ID -- IsAttachedToSource
      -- compares it against the match's source, which this builder does not know.
      -- Not narrowed to the battlefield the way `attachedToView` is.
      Filter.attachedTo = hostOf oid gs,
      -- CR 701.3a: filled only by Resolve's AttachTarget arm, the one place that
      -- knows what is being moved.
      Filter.canHostSubject = False,
      -- CR 701.3a's other side: filled only by Resolve's Effect.Search arm, the
      -- one place that knows which host the instruction fixed. Overlaid onto this
      -- builder's result rather than passed in, so a search pays for it only when
      -- its filter names the atom.
      Filter.canAttachToSubject = False,
      -- CR 111.6: fixed for the life of the object (CR 400.7).
      Filter.token = Game.isToken oid gs,
      Filter.tapped = Game.isTapped oid gs,
      -- CR 701.27g's two conjuncts, and the only site that fills the field. The
      -- face is read CURRENT -- Game.isFrontFaceUp reads Object.face, never the
      -- Object.turnedOverAt beside it -- which is the rule's first exclusion, a
      -- permanent front face up being untransformed however it got there. The
      -- second exclusion holds today because no object is represented by more
      -- than one card; see #369.
      --
      -- The battlefield conjunct is a REGRESSION FENCE rather than a proved
      -- behaviour: every Count that reaches the atom is already scoped to a
      -- zone, so dropping it leaves the suite green. It is CR 701.27g's own
      -- wording, and the object it excludes -- a double-faced spell on the stack
      -- with its back face up (CR 712.11a) -- is reachable, so the conjunct
      -- stays.
      Filter.transformed = Set.member oid (GameState.battlefield gs) && not (Game.isFrontFaceUp oid gs),
      Filter.counters = counters,
      -- CR 701.54b: a designation rather than a characteristic. Nothing for an id
      -- naming no object -- a designation dies with the permanent (CR 400.7).
      Filter.ringBearerFor = Game.lookupObject oid gs >>= Object.ringBearerFor,
      -- Designations rather than characteristics: ringBearerFor's posture above.
      Filter.designations = maybe Set.empty Object.designations (Game.lookupObject oid gs),
      -- CR 716.2b: a designation too, so `designations`' posture again -- and
      -- Nothing for an id naming no object leaves CR 716.2d to answer level 1 at
      -- the read, which is what a CR 608.2h asker gets for a permanent that is
      -- gone.
      Filter.classLevel = Game.lookupObject oid gs >>= Object.classLevel,
      -- CR 702.33d: read live off the object, so the CR 608.2h path answers False
      -- for a spell that has left the stack.
      Filter.kicked = maybe False Object.kicked (Game.lookupObject oid gs),
      -- CR 400.7d / CR 107.4h: read live off the object like `kicked`, and
      -- flattened to the tags here because that is the whole of what the
      -- vocabulary asks (see the field's own comment in Pawl.Engine.Filter).
      Filter.manaSpentTags = foldMap (foldMap ManaUnit.tags . Mana.unwrap . Object.manaSpent) (Game.lookupObject oid gs),
      -- CR 602.1 / 605.1a off the PROJECTION like `keywords`: abilities are
      -- characteristics (CR 109.3) written by layer 6. The whole list the object
      -- HAS, not the list it can activate here. LAZY -- see the field's own comment
      -- in Pawl.Engine.Filter.
      Filter.nonManaActivatedAbility = not (all ManaAbility.isManaAbility (abilitiesFromCharacteristics peers pc oid gs))
    }

-- CR 122.1: the counters on an object right now, and none for an id naming nothing.
countersOf :: ObjectId -> GameState -> Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
countersOf oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)

-- CR 707.2 / 613.1a: an object's layer-1 (copy) result -- its stamped copy
-- snapshot when it has one, the printed base otherwise. Base-or-snapshot only, so
-- counters, pumps, control and ability grants are never part of a copiable value.
-- Not a recursion: a copy of a copy stored resolved values when it was stamped.
copiableCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
copiableCharacteristics oid gs =
  case Game.lookupObject oid gs >>= (Binding.copyOf . Object.bindings) of
    Just snapshot -> snapshot
    Nothing -> baseCharacteristics oid gs

-- CR 208.2 / 604.3: the card's characteristic-defining P/T, with the printed star
-- resolved to what the CDA counts. Nothing unless the card declares a CDA *and*
-- has a printed power and toughness box (CR 208.1) for the star to sit in.
seedCharacteristicPT :: Face.Face Card.Type.Card -> Maybe CharacteristicPT.CharacteristicPT
seedCharacteristicPT face =
  case (Face.characteristicPT face, Face.power face, Face.toughness face) of
    (Just star, Just (Power.MkPower p), Just (Toughness.MkToughness t)) ->
      Just
        CharacteristicPT.MkCharacteristicPT
          { CharacteristicPT.power = Quantity.substituteStar star p,
            CharacteristicPT.toughness = Quantity.substituteStar star t
          }
    _ -> Nothing

-- Printed characteristics before any effect: CR 613.1's starting point.
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.faceOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { -- No card behind this object (an ability on the stack).
        PC.names = Set.empty,
        PC.supertypes = Set.empty,
        PC.keywords = Map.empty,
        PC.colors = Set.empty,
        -- No card, so no mana cost to read -- which is not CR 202.3a's 0 (#674).
        PC.manaValue = Nothing,
        PC.power = Nothing,
        PC.toughness = Nothing,
        PC.loyalty = Nothing,
        PC.defense = Nothing,
        PC.characteristicPT = Nothing,
        PC.cardTypes = Set.empty,
        PC.subtypes = Set.empty,
        PC.activatedAbilities = [],
        PC.replacementEffects = [],
        PC.triggeredAbilities = [],
        PC.enchant = [],
        PC.subtypeWordChanges = []
      }
  Just face ->
    -- The seed predates every layer, so it can describe no object: every view is
    -- Nothing. That silences the printed box's board-reading shapes only where
    -- they go through this view at all -- Pawl.Engine.Count.evaluate reads a
    -- Scope.InHistory snapshot and a Scope.OverPlayers player directly, so a Count
    -- over either would read LIVE state here. What keeps both out is CR 208.1 /
    -- 208.2: a printed box is a number or a star, and Pawl.CardSpec's "every
    -- printed power and toughness box is a number or a star" lint holds every
    -- card's own faces to it. A MINTED face may print a computed box (CR 111.3),
    -- which Resolve.bakeTokenCharacteristics stamps into a Literal as the token is
    -- created -- except when it could not determine one (gap #1908).
    let seedViewOf = const Nothing
        seedContext = Filter.contextFor (controllerOf oid gs) (Just oid)
     in PC.MkProjectedCharacteristics
          { -- CR 709.4a: the names the object shows, which `face` cannot carry --
            -- Game.namesOf decides which halves show.
            PC.names = Game.namesOf oid gs,
            PC.supertypes = TypeLine.supertypes (Face.typeLine face),
            -- CR 702: a printed keyword appears once; layer 6 adds multiplicity.
            PC.keywords = Map.fromSet (const 1) (Face.keywords face),
            PC.colors = printedColorsOf face,
            -- CR 202.3, derived here so the rest of the fold reads a number.
            -- Game.manaCostFaceOf rather than `face`: CR 712.8e reads a transformed
            -- permanent's mana value off its FRONT face's cost, and CR 708.2a's
            -- face-down face has no mana cost (so CR 202.3a's 0).
            PC.manaValue = fmap Quantity.manaValueOf (Game.manaCostFaceOf oid gs),
            -- Quantity.evaluate, not Quantity.determine: CR 208.2a's "use 0
            -- instead" belongs to a CDA, so a printed star with none behind it is
            -- Nothing. A star given its value by CR 208.2b reports Nothing off the
            -- battlefield; one with a CDA is filled at layer 7a.
            PC.power = case Face.power face of
              Nothing -> Nothing
              Just (Power.MkPower q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
            PC.toughness = case Face.toughness face of
              Nothing -> Nothing
              Just (Toughness.MkToughness q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
            -- CR 306.5a: a literal number, copied through rather than evaluated.
            PC.loyalty = Face.loyalty face,
            -- CR 310.4a: a literal number, likewise.
            PC.defense = Face.defense face,
            PC.characteristicPT = seedCharacteristicPT face,
            PC.cardTypes = TypeLine.types (Face.typeLine face),
            PC.subtypes = TypeLine.subtypes (Face.typeLine face),
            PC.activatedAbilities = Face.activatedAbilities face,
            PC.replacementEffects = Face.replacementEffects face,
            PC.triggeredAbilities = Face.triggeredAbilities face,
            -- CR 702.5a's printed instances. In the SEED rather than folded in
            -- later, so they ride copiableCharacteristics: CR 707.2 names rules
            -- text among the copiable values, and a granted instance is not
            -- copiable precisely because applyModification writes it after the
            -- seed. Read off `face`, so CR 708.2a's face-down substitution leaves
            -- a face-down permanent with none.
            PC.enchant = Face.enchant face,
            -- The seed is CR 613.1's starting point, before layer 3 has run.
            PC.subtypeWordChanges = []
          }

-- CR 202.2 / 204.2 / 202.2b: an object's printed colours, from its mana cost's
-- coloured symbols and its colour indicator. No devoid here: CR 702.114a makes it
-- a CDA, which CR 613.3 puts at the start of layer 5 (applyColorDefining).
printedColorsOf :: Face.Face Card.Type.Card -> Set Color.Color
printedColorsOf face =
  Set.union
    (Face.colorIndicator face)
    (manaCostColors (Face.manaCost face))

-- CR 702.114a. The one place that decides what devoid means.
definesColorless :: Set Keyword -> Bool
definesColorless = Set.member Keyword.Type.Devoid

-- CR 702.73a, definesColorless' twin: the one place changeling is decided.
definesEveryCreatureType :: Set Keyword -> Bool
definesEveryCreatureType = Set.member Keyword.Type.Changeling

-- CR 613.3 / 613.1e: the object's own colour-defining ability, applied at the
-- start of layer 5. Folded in place rather than gathered: a CDA affects only its
-- own object (CR 604.3a(3)) and has no timestamp to sort on. Read off the partial
-- projection's keywords, which at layer 5 hold exactly CR 604.3a(2)'s sources. A
-- GRANTED devoid never reaches here -- CR 604.3a denies it CDA status, so
-- grantedDefiningParts routes it into layer 5 as a timestamped colour effect.
applyColorDefining :: ProjectedCharacteristics -> ProjectedCharacteristics
applyColorDefining pc =
  if definesColorless (Map.keysSet (PC.keywords pc))
    then pc {PC.colors = Set.empty}
    else pc

-- CR 613.3 / 613.1d: the object's own subtype-defining ability, CR 702.73a's
-- changeling, at the start of layer 4 -- applyColorDefining one layer down, and
-- every reason in its header carries over. A UNION, since CR 205.1b keeps the
-- subtypes of the object's other families. Being at the START of layer 4 is what
-- makes Turn to Frog's SetCreatureSubtype win over it. CR 205.3d gates it like
-- any other grant, which on a printed changeling object refuses nothing: CR
-- 702.73b puts the ability on creature and Kindred cards, and both correlate
-- with CR 205.3m's list.
applySubtypeDefining :: ProjectedCharacteristics -> ProjectedCharacteristics
applySubtypeDefining pc =
  if definesEveryCreatureType (Map.keysSet (PC.keywords pc))
    then pc {PC.subtypes = Set.union (gainableSubtypes pc Subtype.everyCreatureType) (PC.subtypes pc)}
    else pc

-- CR 202.1b: a land has no mana cost at all, so it contributes no colours.
manaCostColors :: Maybe ManaCost.ManaCost -> Set Color.Color
manaCostColors mc = case mc of
  Nothing -> Set.empty
  Just (ManaCost.MkManaCost symbols) -> Set.fromList (concatMap symbolColors symbols)

-- CR 202.2b: only a coloured mana symbol carries a colour; colourless is not a
-- colour (CR 105.2c). A list, since a hybrid is all of its colours (CR 107.4e).
symbolColors :: ManaSymbol.ManaSymbol -> [Color.Color]
symbolColors symbol = case symbol of
  ManaSymbol.OfType (ManaType.Colored c) -> [c]
  ManaSymbol.OfType ManaType.Colorless -> []
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) -> Maybe.mapMaybe colorOfManaType [a, b]
  -- CR 107.4b/107.4e: a monocolored hybrid's other half is generic, so the named
  -- half is the whole contribution.
  ManaSymbol.MonocoloredHybrid t -> Maybe.maybeToList (colorOfManaType t)
  -- CR 107.4f / 202.2d: Phyrexian symbols are coloured mana symbols. Total `[c]`
  -- since Phyrexian carries a Color -- there is no colourless Phyrexian symbol.
  ManaSymbol.Phyrexian c -> [c]
  -- CR 107.4f: "a hybrid Phyrexian mana symbol is BOTH of its component
  -- colors", which CR 202.2d makes the object. Tamiyo, Compleated Sage is green
  -- and blue whichever of her {G/U/P}'s three ways paid for her.
  ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian l r) -> [l, r]
  -- CR 107.4h: snow is neither a colour nor a type of mana.
  ManaSymbol.Snow -> []
  ManaSymbol.Generic _ -> []
  ManaSymbol.Variable -> []

-- CR 105.2c: colourless is not a colour, so a colourless hybrid half adds none.
colorOfManaType :: ManaType.ManaType -> Maybe Color.Color
colorOfManaType manaType = case manaType of
  ManaType.Colored c -> Just c
  ManaType.Colorless -> Nothing

-- affects evaluated against an object's BASE characteristics (used by
-- source-liveness, which must not recurse into the projection it feeds).
affectsBase :: ObjectId -> ObjectId -> Affected.Affected -> GameState -> Bool
affectsBase source oid a gs = affectsGiven (baseView gs) source oid a (baseCharacteristics oid gs) gs

-- The ViewOf that reads every object at its BASE characteristics, for a caller
-- feeding the projection rather than reading it. fullView and viewUpTo are the
-- counterparts. Nothing for an id naming no object. Swapping this for fullView
-- leaves the whole suite green: no printing reaches it (gap #1757).
baseView :: GameState -> Count.ViewOf
baseView gs oid =
  if Map.member oid (GameState.objects gs)
    then Just (viewOfCharacteristics (baseView gs) oid (baseCharacteristics oid gs) (controllerOf oid gs) (countersOf oid gs) gs)
    else Nothing

-- CR 608.2h / 611.2d: evaluate a modification's quantities once and rewrite them
-- to literals, when Resolve stores a continuous effect.
--
-- TWO objects, neither of them the affected one. `source` is CR 113.7a's source;
-- `announcedOn` is the object CR 601.2b stamped the chosen X on -- for an
-- ACTIVATED ability the ability object, not the permanent.
--
-- Not applied to a static ability's effect (CR 611.2 scopes 611.2a-d to a
-- resolution, and CR 604.2's effect is regenerated per projection), nor to what
-- frozenStaticParts hands over as a permanent leaves. Nothing when a quantity
-- cannot be evaluated at store time: it cannot be determined later either.
--
-- The CONTEXT comes from the caller rather than being built here, because a
-- Filter.contextFor built here would carry no slot bindings: Quantity.AgainstSlot
-- reads Filter.slotObjects, so against an empty map it answers Nothing, the
-- freeze answers Nothing, and Resolve stores no continuous effect at all. Rush of
-- Blood's "+X/+0 ... where X is its power" is the producer, and
-- Pawl.Engine.Resolve.effectContext is the one engine caller's spelling.
freezeQuantities :: GameState -> ObjectId -> ObjectId -> Filter.Context -> Modification.Modification ability -> Maybe (Modification.Modification ability)
freezeQuantities gs announcedOn source context m =
  let viewOf = fullView gs
      freeze q = fmap Quantity.Type.Literal (Quantity.evaluateFor viewOf context gs announcedOn source q)
   in case m of
        Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) -> fmap Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness <$> freeze p <*> freeze t)
        Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) -> fmap Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness <$> freeze p <*> freeze t)
        -- No quantity to freeze; named explicitly per the exhaustiveness discipline.
        Modification.GainKeyword _ -> Just m
        Modification.GainEnchant _ -> Just m
        -- The granted ability's own quantities are NOT frozen: CR 611.2d fixes a
        -- variable in this effect, not in a quoted ability's own future one.
        Modification.GainAbility _ -> Just m
        Modification.LoseAllAbilities -> Just m
        Modification.LoseNamedAbility _ -> Just m
        Modification.SetLandSubtype _ -> Just m
        Modification.SetLandSubtypeToChosen -> Just m
        Modification.AddLandSubtype _ -> Just m
        Modification.SetCreatureSubtype _ -> Just m
        Modification.AddCreatureSubtype _ -> Just m
        Modification.AddEveryCreatureSubtype -> Just m
        Modification.AddSubtype _ -> Just m
        Modification.AddCardType _ -> Just m
        Modification.SetCardType _ -> Just m
        Modification.AddSupertype _ -> Just m
        Modification.RemoveSupertype _ -> Just m
        Modification.ChangeSubtypeWord {} -> Just m
        Modification.SetController _ -> Just m
        Modification.SetControllerToSource -> Just m
        Modification.SetColor _ -> Just m
        Modification.AddColor _ -> Just m
        Modification.AddChosenColor -> Just m
        Modification.SwitchPowerToughness -> Just m

-- Every Quantity a modification carries, in order. A new Quantity field goes here
-- as well as in freezeQuantities -- the compiler forces the arm, not its content.
quantitiesOf :: Modification.Modification ability -> [Quantity.Type.Quantity]
quantitiesOf m = case m of
  Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) -> [p, t]
  Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) -> [p, t]
  Modification.GainKeyword _ -> []
  -- A target slot's Filter can nest a Count, but a Filter's quantities are read
  -- where the Filter is matched rather than frozen here -- the answer GainKeyword
  -- gives above, whose keyword can nest one too.
  Modification.GainEnchant _ -> []
  -- The layer fold evaluates nothing inside a quoted ability.
  Modification.GainAbility _ -> []
  Modification.LoseAllAbilities -> []
  Modification.LoseNamedAbility _ -> []
  Modification.SetLandSubtype _ -> []
  Modification.SetLandSubtypeToChosen -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCreatureSubtype _ -> []
  Modification.AddEveryCreatureSubtype -> []
  Modification.AddSubtype _ -> []
  Modification.AddCardType _ -> []
  Modification.SetCardType _ -> []
  Modification.AddSupertype _ -> []
  Modification.RemoveSupertype _ -> []
  Modification.ChangeSubtypeWord {} -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.AddColor _ -> []
  Modification.AddChosenColor -> []
  Modification.SwitchPowerToughness -> []

-- CR 305.7: does this modification SET a land's subtype, and so strip the land's
-- rules text? Total, like removesAbilities: a new subtype-setting Modification
-- must break this build rather than silently answer False.
setsLandSubtype :: Modification.Modification ability -> Bool
setsLandSubtype m = case m of
  Modification.SetLandSubtype _ -> True
  -- CR 305.7 does not care where the type came from: a type chosen as the source
  -- entered (CR 614.1c) strips rules text as a printed one does.
  Modification.SetLandSubtypeToChosen -> True
  -- The OTHER direction of CR 305.7's last sentence: a land that GAINS a type in
  -- addition to its own keeps its rules text.
  Modification.AddLandSubtype _ -> False
  -- An ability grant is layer 6 and sets no subtype at all.
  Modification.GainAbility _ -> False
  Modification.GainKeyword _ -> False
  Modification.GainEnchant _ -> False
  -- A control op, not a type change.
  Modification.SetController _ -> False
  Modification.SetControllerToSource -> False
  -- The OTHER subtype set: CR 305.7's strip is about a LAND whose subtype is set,
  -- and CR 205.1a/205.1b's creature-type set carries no such clause.
  Modification.SetCreatureSubtype _ -> False
  Modification.AddCreatureSubtype _ -> False
  Modification.AddEveryCreatureSubtype -> False
  -- Not a SET, so CR 305.7 does not fire whatever family the subtype belongs to
  -- -- the same answer AddLandSubtype gives above, and Pawl.CardSpec keeps CR
  -- 205.3i's land types off this arm anyway.
  Modification.AddSubtype _ -> False
  -- The CARD-TYPE set: CR 305.7 fires on setting a land's SUBTYPE, so making an
  -- object a land does not strip its rules text.
  Modification.SetCardType _ -> False
  Modification.AddCardType _ -> False
  -- CR 205.4b changes a supertype and says nothing about subtypes.
  Modification.AddSupertype _ -> False
  Modification.RemoveSupertype _ -> False
  -- CR 612 swaps one word for another inside rules text; it sets no subtype.
  Modification.ChangeSubtypeWord {} -> False
  Modification.LoseAllAbilities -> False
  Modification.LoseNamedAbility _ -> False
  Modification.SetBasePowerToughness {} -> False
  Modification.ModifyPowerToughness {} -> False
  Modification.SwitchPowerToughness -> False
  Modification.SetColor _ -> False
  Modification.AddColor _ -> False
  Modification.AddChosenColor -> False

-- Every SetLandSubtype and SetLandSubtypeToChosen effect in the game, each with
-- its source and affected set, for a reader OUTSIDE the layer fold. A legitimate
-- case-on-Modification -- Projection is its sole home. CR 604.2's "as long as"
-- gate is answered here against the same seed list gather feeds its own gates, so
-- a static ability whose clause is false strips nothing under CR 305.7 either.
-- The seed costs a whole extra walk, so it is spent only on a board that has a
-- conditional static ability at all.
setLandSubtypeEffects :: GameState -> [(ObjectId, Affected.Affected)]
setLandSubtypeEffects gs =
  let functioning =
        if anyConditional gs
          then conditionHolds (gatherGiven (const False) alwaysFunctioning Nothing gs) gs
          else alwaysFunctioning
   in setLandSubtypeEffectsGiven functioning gs

-- setLandSubtypeEffects with the CR 604.2 gate left open, for a caller INSIDE the
-- layer fold. The gated reader above is never called from anywhere the projection
-- can reach.
setLandSubtypeEffectsGiven :: (ObjectId -> Layer -> Condition.Type.Condition -> Bool) -> GameState -> [(ObjectId, Affected.Affected)]
setLandSubtypeEffectsGiven functioning gs =
  let isSet = setsLandSubtype
      fromStored eff =
        if isSet (ContinuousEffect.modification eff)
          then [(ContinuousEffect.source eff, ContinuousEffect.affected eff)]
          else []
      -- The affected set is REWRITTEN here, the same CR 612 word swap gatherStatic
      -- applies to the same ability's set. The two must agree, or the halves
      -- of one rule disagree about which permanents an ability names. Hoisted out
      -- of gather's walk so the fold runs once per battlefield permanent rather
      -- than once per permanent per projection. The MODIFICATIONS stay unrewritten,
      -- which is sound because a word swap cannot change whether an arm is a set
      -- arm.
      fromPerm permId = case Game.faceOf permId gs of
        Nothing -> []
        Just face ->
          let changes = textChangesAffecting permId gs
              -- CR 604.2's clause, asked exactly as gatherStatic asks it. Free for
              -- an unconditional ability, since staticLives answers first.
              lives sa = staticLives (functioning permId) changes (minimum (fmap layer (staticParts changes sa))) sa
           in fmap (\sa -> (permId, rewriteAffected changes (StaticAbility.affected sa))) $
                filter (\sa -> any isSet (StaticAbility.modifications sa) && functionsFromZone Zone.Battlefield sa && lives sa) (Face.staticAbilities face)
   in concatMap fromStored (GameState.continuousEffects gs)
        <> concatMap fromPerm (abilitySources gs)

-- CR 305.7: a land whose subtype is set to a basic type loses its rules-text
-- abilities. The GATE half, for readers whose ability lands on objects other than
-- the bearer; every other rules-text ability is stripped inside the fold by
-- setLandSubtypeTo. The effect list is hoisted so a caller computes it once rather
-- than once per permanent. Which effects apply is CR 613.8's question, answered by
-- appliedSetEffects.
--
-- The INSIDE-THE-FOLD gate, and gather (via permanentParts) is its one caller.
-- "Applies to" reads BASE characteristics so nothing recurses into the projection
-- being built -- which is also CR 613.8's own answer for an ability deciding AT
-- layer 4, base being the state as that layer begins: a setter that already
-- reaches the permanent there is one the ability's effect depends on, so the
-- setter applies first and CR 613.6 rescues nothing. An ability deciding LATER is
-- setSubtypeStripped's, judged against the projection through layer 4. Readers
-- outside the fold use liveAfterLayers. The layer-2 control fold asks NEITHER
-- gate -- see controlGrants.
--
-- Not implemented: a layer-4 ability whose OWN effect would take its source out
-- of the setter's affected set makes the setter depend on it (CR 613.8a), so it
-- applies first and keeps its text; base cannot see that, and it is stripped
-- (#1489).
liveGiven :: [(ObjectId, Affected.Affected)] -> ObjectId -> GameState -> Bool
liveGiven setEffs oid gs =
  not
    ( hasLandType (baseCharacteristics oid gs)
        && any (\(src, aff) -> affectsBase src oid aff gs) (appliedSetEffects setEffs gs)
    )

-- CR 305.7's subject: only a LAND loses its rules text to a subtype set. CR 205.3d
-- is what makes that a precondition rather than a description of every board a
-- setter can reach -- a setter reaching an object with no Land card type sets no
-- subtype there (setLandSubtypeTo), so it takes no abilities either. The three
-- gates ask it of the characteristics each is judged against: base for liveGiven,
-- the finished projection for liveAfterLayers, through layer 4 for
-- setSubtypeStripped.
hasLandType :: ProjectedCharacteristics -> Bool
hasLandType = Set.member CardType.Land . PC.cardTypes

-- The same CR 305.7 gate for a reader OUTSIDE the layer fold. CR 613.10 and CR
-- 613.11 run such readers after the projection is finished, so this may read the
-- projection where liveGiven must read base. Not a fixpoint: liveGiven bottoms out
-- at baseCharacteristics, which folds nothing. WHICH effects apply is still
-- answered against base by appliedSetEffects; only the final membership test moves
-- to the finished projection, which keeps CR 613.8's ordering out of here.
liveAfterLayers :: [(ObjectId, Affected.Affected)] -> ObjectId -> GameState -> Bool
liveAfterLayers setEffs oid gs =
  let view = project oid gs
   in not
        ( hasLandType view
            && any (\(src, aff) -> affects src oid aff view gs) (appliedSetEffects setEffs gs)
        )

-- CR 305.7's strip, asked of ONE ability rather than of the whole permanent: does
-- a subtype-setting effect reach `oid` by the time layer 4 has finished? The
-- caller pairs it with CR 613.6 -- see permanentParts' `removed` -- so only an
-- ability whose effect had not yet started applying is stripped by this.
--
-- The membership test reads the projection THROUGH layer 4 (inclusive), which is
-- the whole point: a permanent another effect animated into a land is one CR
-- 305.7 reaches, and base characteristics cannot see that. Bounded there rather
-- than at the finished projection because CR 613.1d is where a setter applies;
-- WHICH setters apply is still appliedSetEffects's question, judged against base.
--
-- Not a fixpoint: the candidate list is gather's SEED pass, built with every
-- ability gate wired open, so the projection behind this gate never re-enters it.
-- The seed can only over-project (gather says why), and here that can only widen
-- the set a setter reaches.
--
-- Not implemented: CR 613.8a's dependency is decided at `oid` alone rather than
-- over the setter's whole affected set (#236).
setSubtypeStripped :: [Gathered] -> [(ObjectId, Affected.Affected)] -> GameState -> ObjectId -> Bool
setSubtypeStripped cands setEffs gs = case appliedSetEffects setEffs gs of
  -- Almost every board sets no land's subtype, and then no projection is spent on
  -- the question.
  [] -> const False
  applied ->
    let -- The peers a setter's own filter reaches, at the setter's decision point.
        peers = viewUpTo Layer.Type cands gs
        -- Bound before `oid`, so the candidate-only work is shared across the walk.
        throughType = projectWith (<= Layer.Type) cands
     in \oid ->
          let partial = throughType oid gs
           in -- CR 305.7's subject, asked of the projection through layer 4 --
              -- where the setter applies, and the state affectsGiven judges
              -- membership against.
              hasLandType partial
                && any (\(src, aff) -> affectsGiven peers src oid aff partial gs) applied

-- CR 613.8: which of the CR 305.7 subtype-setting effects actually apply, in the
-- order the rule applies them. An effect that strips a land's rules-text abilities
-- can switch OFF another such effect, which CR 613.8a makes a dependency; CR
-- 613.8b's last sentence falls back to timestamp order inside a loop, and CR
-- 613.8c is why this is a loop rather than a sort.
--
-- An effect whose source has already been stripped is dropped rather than applied;
-- one that strips its OWN source still applies. Timestamps are the SOURCE
-- PERMANENT's (CR 613.7d), and a source that has left has none, sorting last.
-- Indices carry the identity, since two permanents can generate equal pairs.
appliedSetEffects :: [(ObjectId, Affected.Affected)] -> GameState -> [(ObjectId, Affected.Affected)]
appliedSetEffects setEffs gs =
  let indexed = zip [0 :: Int ..] setEffs
      stampOf (_, (src, _)) = fmap Object.timestamp (Game.lookupObject src gs)
      -- CR 613.8a, for these effects: does `other` strip `e`'s source?
      dependsOn (_, (src, _)) (_, (osrc, oaff)) = affectsBase osrc src oaff gs
      earliest :: [(Int, (ObjectId, Affected.Affected))] -> (Int, (ObjectId, Affected.Affected))
      earliest = List.minimumBy (Ord.comparing (\e -> (stampOf e, fst e)))
      go remaining applied = case remaining of
        [] -> reverse applied
        _ ->
          let waiting e = any (dependsOn e) (filter (\o -> fst o /= fst e) remaining)
              ready = filter (not . waiting) remaining
              -- CR 613.8b: nothing ready means every remaining effect is in a loop.
              next = earliest (if null ready then remaining else ready)
              (nsrc, _) = snd next
              stripped = any (\(src, aff) -> affectsBase src nsrc aff gs) applied
           in go (filter (\o -> fst o /= fst next) remaining) (if stripped then applied else snd next : applied)
   in go indexed []

-- Every subtype-word pair a ChangeSubtypeWord continuous effect imposes on `oid`
-- (CR 612). CR 612.2's family gate is applied where a pair meets a word, not here.
-- Stored resolution effects only, read against BASE characteristics, so nothing
-- recurses.
textChangesAffecting :: ObjectId -> GameState -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)]
textChangesAffecting oid gs =
  let pairOf eff = case ContinuousEffect.modification eff of
        Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord from to) ->
          if affectsGiven (baseView gs) (ContinuousEffect.source eff) oid (ContinuousEffect.affected eff) (baseCharacteristics oid gs) gs
            then Just (from, to)
            else Nothing
        _ -> Nothing
   in Maybe.mapMaybe pairOf (GameState.continuousEffects gs)

-- Apply text-changes to a modification's subtype words (CR 612.1/612.2).
--
-- CR 612.2 gates each arm: the arm's family is fixed by its constructor, and the
-- PAIR's family is read off the word being replaced, which is why no family tag
-- rides on the stored ChangeSubtypeWord. Exhaustive rather than a catch-all, which
-- is what had let GainKeyword go unrewritten while carrying a land-type word.
-- Polymorphic in the granted ability, and takes ITS rewriter, since a card's grant
-- descends into the quoted ability while ModifyTarget's Void one cannot occur.
rewriteModification :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modification -> Modification
rewriteModification = rewriteModificationWith rewriteGrantedAbility

rewriteModificationWith :: ([(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ability -> ability) -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modification.Modification ability -> Modification.Modification ability
rewriteModificationWith rewriteAbility pairs m =
  let -- `inFamily from` is CR 612.2's gate.
      swap inFamily from to s = if s == from && inFamily from then to else s
      apply1 acc (from, to) = case acc of
        Modification.SetLandSubtype s -> Modification.SetLandSubtype (swap Subtype.isLandType from to s)
        Modification.AddLandSubtype s -> Modification.AddLandSubtype (swap Subtype.isLandType from to s)
        -- CR 612.2's other named example: a Turn to Frog's Frog on the stack.
        Modification.SetCreatureSubtype s -> Modification.SetCreatureSubtype (swap Subtype.isCreatureType from to s)
        Modification.AddCreatureSubtype s -> Modification.AddCreatureSubtype (swap Subtype.isCreatureType from to s)
        -- Holds no word to swap: it names CR 205.3m's list, not a member of it.
        Modification.AddEveryCreatureSubtype -> acc
        -- Deliberately unrewritten. CR 612.2 changes only a word used in the
        -- correct way, and this arm carries no family to check the word against;
        -- the two family-tagged adds above are where a land-type or creature-type
        -- word rides, and Pawl.CardSpec keeps those words out of this one.
        Modification.AddSubtype _ -> acc
        -- CR 702.14a: "[type]walk" holds a land-type word, so a hacked Lord of
        -- Atlantis grants swampwalk. The GRANTER's text is what this reads, which
        -- is CR 612.3. Filter.rewriteKeyword since the word is inside a Filter; no
        -- family gate is restated there -- the word's use is its family.
        Modification.GainKeyword k -> Modification.GainKeyword (Filter.rewriteKeyword [(from, to)] k)
        -- CR 612.1 through the granted enchant's own Filter, which is text
        -- printed on the GRANTER (CR 612.3) exactly as the keyword above is.
        -- rewriteTargetSlot is the same descent a mode's target slots take.
        Modification.GainEnchant slot -> Modification.GainEnchant (rewriteTargetSlot [(from, to)] slot)
        -- CR 612.1 over the whole quoted ability: the words are printed on the
        -- GRANTER, so a text change affecting it rewrites them before the grant.
        Modification.GainAbility a -> Modification.GainAbility (rewriteAbility [(from, to)] a)
        -- Carries no word: the type is read off the source at projection time.
        Modification.SetLandSubtypeToChosen -> acc
        -- A control op carries no subtype word either.
        Modification.SetController _ -> acc
        Modification.SetControllerToSource -> acc
        -- An ability wipe names nothing at all, and neither does a P/T switch.
        Modification.LoseAllAbilities -> acc
        -- Names an ABILITY of the same card, which is no subtype word.
        Modification.LoseNamedAbility _ -> acc
        Modification.SwitchPowerToughness -> acc
        -- CR 612.1 through both boxes: a Quantity.Count carries a Filter, so a
        -- hacked Aspect of Wolf counts the new type. rewriteQuantity is the same
        -- descent rewriteCondition and the CDA path take.
        Modification.SetBasePowerToughness pt ->
          Modification.SetBasePowerToughness
            pt
              { SetBasePowerToughness.power = rewriteQuantity [(from, to)] (SetBasePowerToughness.power pt),
                SetBasePowerToughness.toughness = rewriteQuantity [(from, to)] (SetBasePowerToughness.toughness pt)
              }
        Modification.ModifyPowerToughness pt ->
          Modification.ModifyPowerToughness
            pt
              { ModifyPowerToughness.power = rewriteQuantity [(from, to)] (ModifyPowerToughness.power pt),
                ModifyPowerToughness.toughness = rewriteQuantity [(from, to)] (ModifyPowerToughness.toughness pt)
              }
        -- CR 205.2a's card types are a different list from CR 205.3's subtypes, and
        -- CR 205.4a's supertypes a third, so these hold no word a pair could name.
        Modification.AddCardType _ -> acc
        Modification.SetCardType _ -> acc
        Modification.AddSupertype _ -> acc
        Modification.RemoveSupertype _ -> acc
        -- The two words of a STORED text change are its own resolution's choice
        -- (CR 608.2d). A text changer's PRINTED clause is reached by rewriteEffect.
        Modification.ChangeSubtypeWord {} -> acc
        -- CR 612.2 names colour words as a swappable family, but pawl's only text
        -- changer swaps subtypes, so no pair reaching here holds a colour word.
        Modification.SetColor _ -> acc
        Modification.AddColor _ -> acc
        Modification.AddChosenColor -> acc
   in List.foldl' apply1 m pairs

-- rewriteModification's sibling for the other half of a static ability. Under CR
-- 612.1 an ability's affected clause is rules text like any other, so a hacked
-- Kormus Bell animates Islands. Exhaustive over Affected: a new arm carrying a
-- Filter must break this build rather than silently keep the old word.
rewriteAffected :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Affected.Affected -> Affected.Affected
rewriteAffected pairs a = case a of
  Affected.Matching f -> Affected.Matching (Filter.rewrite pairs f)
  Affected.MatchingAnywhere f -> Affected.MatchingAnywhere (Filter.rewrite pairs f)
  Affected.MatchingOffBattlefield f -> Affected.MatchingOffBattlefield (Filter.rewrite pairs f)
  Affected.AttachedPlayerControls f -> Affected.AttachedPlayerControls (Filter.rewrite pairs f)
  -- A frozen id set names no word (CR 611.2c), and an attachment names none.
  Affected.TheseObjects _ -> a
  Affected.Attached -> a

-- CR 612's subtype word swap over an effect's AST. Cases on an effect's
-- STRUCTURE -- does this arm carry a word a swap could reach -- never on which
-- effect it is.
rewriteEffect :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Effect.Effect Card.Type.Card -> Effect.Effect Card.Type.Card
rewriteEffect pairs effect = case effect of
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    Effect.ModifyTarget (ModifyTarget.MkModifyTarget (rewriteDuration pairs duration) (rewriteModificationWith (const Void.absurd) pairs modification) (rewriteObjectRef pairs ref))
  Effect.DealDamage (DealDamage.MkDealDamage ref quantity dealer excess) -> Effect.DealDamage (DealDamage.MkDealDamage (rewriteObjectRef pairs ref) quantity dealer excess)
  -- Two SlotNames and nothing else: no word a swap could reach.
  Effect.Fight _ -> effect
  -- CR 612.1: a text-changer's own restriction clause is text like any other.
  Effect.ChangeText (ChangeText.MkChangeText family forbidden slot) ->
    Effect.ChangeText (ChangeText.MkChangeText family (Set.map (swapWordIn family pairs) forbidden) slot)
  Effect.AddMana _ -> effect
  Effect.Search (Search.MkSearch searcher owner zones quantity filter_ upTo destination) -> Effect.Search (Search.MkSearch searcher owner zones quantity (Filter.rewrite pairs filter_) upTo destination)
  Effect.ExileAllGraveyards -> effect
  Effect.Proliferate -> effect
  Effect.Bolster _ -> effect
  -- CR 612.1 / 612.2a: amass's subtype is a printed word of CR 205.3m's family,
  -- and the token's own name follows it.
  Effect.Amass (Amass.MkAmass quantity subtype) ->
    Effect.Amass (Amass.MkAmass quantity (List.foldl' (\s (from, to) -> if s == from && Subtype.isCreatureType from then to else s) subtype pairs))
  Effect.Blight _ -> effect
  Effect.TemptWithTheRing -> effect
  Effect.Venture -> effect
  Effect.ExileHandThenDraw -> effect
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot filter_ quantity) -> Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot (Filter.rewrite pairs filter_) quantity)
  Effect.RestartGame exempt -> Effect.RestartGame (fmap (rewriteObjectRef pairs) exempt)
  Effect.ControlPlayerNextTurn _ -> effect
  Effect.Destroy (Destroy.MkDestroy ref regenerability mSlot mBuried mPermanents) -> Effect.Destroy (Destroy.MkDestroy (rewriteObjectRef pairs ref) regenerability mSlot mBuried mPermanents)
  Effect.Sacrifice _ -> effect
  Effect.TurnFaceDown _ -> effect
  Effect.TurnFaceUp _ -> effect
  Effect.RemoveFromCombat _ -> effect
  Effect.BecomesBlocked _ -> effect
  -- Not implemented: a CR 122.1b keyword counter named in the riders keeps its
  -- printed keyword through the swap (#1190).
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone riders mSlot mOrigin position) -> Effect.MoveToZone (MoveToZone.MkMoveToZone (rewriteObjectRef pairs ref) zone riders mSlot mOrigin position)
  Effect.Draw {} -> effect
  Effect.Mill (Mill.MkMill ref quantity mTally mSlot) ->
    Effect.Mill (Mill.MkMill ref quantity (fmap (\t -> t {MillTally.filter = Filter.rewrite pairs (MillTally.filter t)}) mTally) mSlot)
  Effect.Reveal (Reveal.MkReveal ref slot) -> Effect.Reveal (Reveal.MkReveal (rewriteObjectRef pairs ref) slot)
  Effect.LookAt (LookAt.MkLookAt ref slot) -> Effect.LookAt (LookAt.MkLookAt (rewriteObjectRef pairs ref) slot)
  Effect.Scry {} -> effect
  Effect.Surveil {} -> effect
  Effect.Fateseal {} -> effect
  Effect.Explore ref -> Effect.Explore (rewriteObjectRef pairs ref)
  -- The These arm's ref carries a Filter, so rule 612's text change reaches it
  -- exactly as Reveal's does; the Counted arm holds two slot NAMES and a count,
  -- and a slot name is not a word rule 612 can swap.
  Effect.Discard subject -> case subject of
    Discard.Counted _ -> effect
    Discard.These ref -> Effect.Discard (Discard.These (rewriteObjectRef pairs ref))
  Effect.LoseLife {} -> effect
  Effect.GainLife {} -> effect
  Effect.ExchangeLifeTotals _ -> effect
  Effect.SetLifeTotal {} -> effect
  Effect.RedistributeLifeTotals -> effect
  Effect.IncreaseSpeed {} -> effect
  Effect.DecreaseSpeed {} -> effect
  -- CR 612.2a: the token's creature types and its name are the same words, and
  -- they live in the defining card.
  --
  -- Not implemented: a CR 122.1b keyword counter named in the riders keeps its
  -- printed keyword (#1190).
  Effect.Create (Create.MkCreate quantity card riders slot creator) -> Effect.Create (Create.MkCreate quantity (rewriteCard pairs card) riders slot creator)
  -- CR 707.2 excludes text-changing effects from copiable values, so what the
  -- token becomes is not rewritten -- only the ref is.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref) -> Effect.CreateCopy (CreateCopy.MkCreateCopy quantity (rewriteObjectRef pairs ref))
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject) ->
    Effect.BecomeCopy (BecomeCopy.MkBecomeCopy (rewriteObjectRef pairs original) (rewriteObjectRef pairs subject))
  Effect.Replace {} -> effect
  Effect.SkipNextPhase {} -> effect
  -- CR 612.1: a rider's text is as changeable as any other.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration kind ref chosenSource quantity rider) ->
    Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage (rewriteDuration pairs duration) kind ref chosenSource quantity (fmap (rewriteEffect pairs) rider))
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration kind ref direction chosenSource rider) ->
    Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage (rewriteDuration pairs duration) kind ref direction chosenSource (fmap (rewriteEffect pairs) rider))
  Effect.RedirectDamage {} -> effect
  Effect.Counter (Counter.MkCounter ref mSlot) -> Effect.Counter (Counter.MkCounter (rewriteObjectRef pairs ref) mSlot)
  -- Not implemented: a CR 122.1b keyword counter named in the kind keeps its
  -- printed keyword through the swap, where Filter's HasCounters arm rewrites
  -- the same kind (#1840).
  Effect.PutCounters (PutCounters.MkPutCounters kind quantity ref) ->
    Effect.PutCounters (PutCounters.MkPutCounters kind (rewriteQuantity pairs quantity) (rewriteObjectRef pairs ref))
  Effect.RemoveCounters {} -> effect
  Effect.GainPlayerCounters {} -> effect
  Effect.RemovePlayerCounters {} -> effect
  Effect.PayAnyEnergy _ -> effect
  Effect.Tap ref -> Effect.Tap (rewriteObjectRef pairs ref)
  Effect.Untap ref -> Effect.Untap (rewriteObjectRef pairs ref)
  Effect.Detain ref -> Effect.Detain (rewriteObjectRef pairs ref)
  Effect.Goad ref -> Effect.Goad (rewriteObjectRef pairs ref)
  Effect.MakePlotted ref -> Effect.MakePlotted (rewriteObjectRef pairs ref)
  Effect.DoesNotUntapNext ref -> Effect.DoesNotUntapNext (rewriteObjectRef pairs ref)
  Effect.Transform ref -> Effect.Transform (rewriteObjectRef pairs ref)
  Effect.PhaseOut ref -> Effect.PhaseOut (rewriteObjectRef pairs ref)
  Effect.AddPhases _ -> effect
  Effect.EndTurn -> effect
  Effect.EndCombatPhase -> effect
  Effect.GainControl (DurationRef.MkDurationRef duration ref) -> Effect.GainControl (DurationRef.MkDurationRef (rewriteDuration pairs duration) (rewriteObjectRef pairs ref))
  Effect.ArmDelayedTrigger {} -> effect
  -- Not implemented: the Filter inside the PlayerEffect keeps its printed word
  -- while the spell is on the stack (#1370).
  Effect.AffectPlayers {} -> effect
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration blocker attacker) ->
    Effect.RequireBlock (RequireBlock.MkRequireBlock (rewriteDuration pairs duration) (rewriteObjectRef pairs blocker) (rewriteObjectRef pairs attacker))
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration ref) ->
    Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated (rewriteDuration pairs duration) (rewriteObjectRef pairs ref))
  -- CR 612.1 reaches the OBJECT axis's ref and not its player: a word swap
  -- changes card text, and the defender clause of Alluring Siren's sentence is
  -- "you" rather than any word a Filter could name.
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration attacker defender) ->
    Effect.RequireAttack (RequireAttack.MkRequireAttack (rewriteDuration pairs duration) (rewriteObjectRef pairs attacker) defender)
  -- CR 114.3 leaves an emblem no type line and no name, so its ABILITIES are the
  -- whole of what CR 612.1 can reach.
  Effect.CreateEmblem card -> Effect.CreateEmblem (rewriteCard pairs card)
  Effect.BecomeMonarch {} -> effect
  Effect.Designate (Designate.MkDesignate _ _) -> effect
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> effect
  Effect.Unsuspect ref -> Effect.Unsuspect (rewriteObjectRef pairs ref)
  Effect.Evolve _ -> effect
  Effect.Mentor _ -> effect
  Effect.Train _ -> effect
  Effect.ItBecomes _ -> effect
  Effect.ExileUntilMonarch _ -> effect
  Effect.ExileHaunting {} -> effect
  Effect.Attach _ -> effect
  Effect.AttachTarget (AttachTarget.MkAttachTarget slot filter_) -> Effect.AttachTarget (AttachTarget.MkAttachTarget slot (Filter.rewrite pairs filter_))
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot filter_) -> Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot (Filter.rewrite pairs filter_))
  Effect.PlaySubgame _ -> effect
  Effect.ChooseOpponent _ -> effect
  Effect.ChooseOpponentAtRandom _ -> effect
  Effect.RollDie {} -> effect
  Effect.TakeExtraTurn {} -> effect
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named ref) -> Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named (rewriteObjectRef pairs ref))
  Effect.OfferCast {} -> effect
  Effect.GrantPlayFromExile grant ->
    Effect.GrantPlayFromExile
      grant
        { GrantPlayFromExile.duration = rewriteDuration pairs (GrantPlayFromExile.duration grant),
          GrantPlayFromExile.ref = rewriteObjectRef pairs (GrantPlayFromExile.ref grant)
        }
  Effect.ForEach (ForEach.MkForEach ref slot body) ->
    Effect.ForEach (ForEach.MkForEach (rewriteObjectRef pairs ref) slot (fmap (rewriteEffect pairs) body))

-- CR 612.2 over one word whose family a card's text names rather than a
-- constructor -- a ChangeText's forbidden-word set.
swapWordIn :: SubtypeFamily.SubtypeFamily -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Subtype.Type.Subtype -> Subtype.Type.Subtype
swapWordIn family pairs word = List.foldl' step word pairs
  where
    step s (from, to) = if s == from && Subtype.inFamily family from then to else s

-- CR 612.1 through an ObjectRef. An InSlot names an object chosen at cast time,
-- and the player-naming arms hold no subtype word; only the Filters and the two
-- library walks' Quantities can carry one -- the Filter that says what ends a
-- walk of a library included.
rewriteObjectRef :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ObjectRef.ObjectRef -> ObjectRef.ObjectRef
rewriteObjectRef pairs ref = case ref of
  ObjectRef.InSlot _ -> ref
  ObjectRef.EachMatching f -> ObjectRef.EachMatching (Filter.rewrite pairs f)
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard s f) -> ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard s (Filter.rewrite pairs f))
  ObjectRef.EachCardInYourHand -> ref
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand s f) -> ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand s (fmap (Filter.rewrite pairs) f))
  ObjectRef.EachCardExiledWithSource f -> ObjectRef.EachCardExiledWithSource (fmap (Filter.rewrite pairs) f)
  ObjectRef.EachSpell f -> ObjectRef.EachSpell (Filter.rewrite pairs f)
  ObjectRef.EachOnStack f -> ObjectRef.EachOnStack (Filter.rewrite pairs f)
  ObjectRef.EachPlayer -> ref
  ObjectRef.EachOpponent -> ref
  ObjectRef.ChosenPlayer -> ref
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary p c) -> ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary p (rewriteQuantity pairs c))
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil p f c) -> ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil p (Filter.rewrite pairs f) (rewriteQuantity pairs c))
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard c s f) -> ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard c s (Filter.rewrite pairs f))
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand p f) -> ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand p (Filter.rewrite pairs f))
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong n f) -> ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong n (Filter.rewrite pairs f))
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong n f) -> ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong n (Filter.rewrite pairs f))
  ObjectRef.RandomCardInHand _ -> ref

-- CR 612.1/612.2a through the CARD an Effect.Create or an Effect.CreateEmblem
-- defines its token or emblem with: the type line, the name, and the rules text.
-- The NAME's change is gated on the type line, since CR 612.2a licenses it only
-- where the word is used as a creature type. The RULES TEXT is walked
-- unconditionally, and an emblem is why: CR 114.3 leaves it no type line at all.
-- Recursive, and terminating because a Card is a finite first-order value and
-- every step descends into a strict subterm. Every FACE, since a card's printed
-- subtypes, name and text are per-face (CR 712.8).
rewriteCard :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Card.Type.Card -> Card.Type.Card
rewriteCard pairs card = card {Card.Type.faces = fmap (rewriteFace pairs) (Card.Type.faces card)}

rewriteFace :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Face.Face Card.Type.Card -> Face.Face Card.Type.Card
rewriteFace pairs face = List.foldl' apply1 face pairs
  where
    apply1 f (from, to) =
      let pair = [(from, to)]
          typeLine = Face.typeLine f
          subtypes = TypeLine.subtypes typeLine
          renamed =
            if Set.notMember from subtypes
              then f
              else
                f
                  { Face.typeLine = typeLine {TypeLine.subtypes = Set.insert to (Set.delete from subtypes)},
                    Face.name = rewriteTokenName from to (Face.name f)
                  }
       in renamed
            { -- CR 702.14a's land-type word inside a landwalk. Set.map rather
              -- than Map.mapKeysWith (+), since a face's keywords are a Set.
              Face.keywords = Set.map (Filter.rewriteKeyword pair) (Face.keywords renamed),
              -- CR 208.2a's star, unevaluated as at layer 3.
              Face.characteristicPT = fmap (rewriteQuantity pair) (Face.characteristicPT renamed),
              -- CR 101.1's ceiling on X, whose Quantity can Count a criterion
              -- naming a land type word. A regression fence: neither printing
              -- pairs a bounded X with one -- both say "the greatest toughness
              -- among creatures you control" -- so no test can falsify it.
              Face.maximumX = fmap (rewriteQuantity pair) (Face.maximumX renamed),
              Face.spell = rewriteModal pair (Face.spell renamed),
              Face.activatedAbilities = fmap (rewriteActivatedAbility pair) (Face.activatedAbilities renamed),
              -- CR 604.2's static ability, on the card a token or emblem is
              -- defined with. A regression fence rather than a proved behaviour,
              -- as Face.maximumX above is: a walk of every defined face in
              -- data/cards for a replacementEffects key (2026-08-21) found none
              -- at all, so no board can tell this line from its absence.
              -- Pawl.ReplacementSpec's Dragonstorm Globe case is what proves
              -- rewritePrintedReplacement itself.
              Face.replacementEffects = fmap (rewritePrintedReplacement pair) (Face.replacementEffects renamed),
              Face.triggeredAbilities = fmap (rewriteTriggeredAbility pair) (Face.triggeredAbilities renamed),
              Face.delayedAbilities = fmap (rewriteTriggeredAbility pair) (Face.delayedAbilities renamed),
              Face.staticAbilities = fmap (rewriteStaticAbility pair) (Face.staticAbilities renamed)
            }

-- A whole static ability under CR 612.1, for the defined-card walk above. A
-- permanent's own statics are reached piecemeal instead, being read per layer.
rewriteStaticAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> StaticAbility.StaticAbility Card.Type.Card -> StaticAbility.StaticAbility Card.Type.Card
rewriteStaticAbility pairs sa =
  sa
    { StaticAbility.affected = rewriteAffected pairs (StaticAbility.affected sa),
      StaticAbility.condition = fmap (rewriteCondition pairs) (StaticAbility.condition sa),
      StaticAbility.modifications = fmap (rewriteModification pairs) (StaticAbility.modifications sa)
    }

-- CR 612.2a's name half, gated on both words being creature types -- CR 612.2
-- prohibits every other family. Text.replace, since CR 111.4's derived name
-- holds one word per subtype; it matches a substring, so a name holding the word
-- inside a longer one is over-reached (#644).
rewriteTokenName :: Subtype.Type.Subtype -> Subtype.Type.Subtype -> CardName.CardName -> CardName.CardName
rewriteTokenName from to name = case (Subtype.creatureTypeWord from, Subtype.creatureTypeWord to) of
  (Just f, Just t) -> CardName.MkCardName (Text.replace f t (CardName.unwrap name))
  _ -> name

-- CR 612.1 over an ACTIVATED ability printed on a permanent: the payload, CR
-- 702.178a's "as long as" gate, and the ACTIVATION COST (CR 118.1, CR 602.1a),
-- so a Magical Hack naming Forest moves which land Dark Heart of the Wood's cost
-- demands.
rewriteActivatedAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ActivatedAbility.ActivatedAbility Card.Type.Card -> ActivatedAbility.ActivatedAbility Card.Type.Card
rewriteActivatedAbility pairs ability =
  ability
    { ActivatedAbility.modal = rewriteModal pairs (ActivatedAbility.modal ability),
      ActivatedAbility.condition = fmap (rewriteCondition pairs) (ActivatedAbility.condition ability),
      ActivatedAbility.cost = Filter.rewriteCost pairs (ActivatedAbility.cost ability)
    }

-- CR 612.1 over a GRANTED ability (CR 613.1f), whichever of CR 113.3's two kinds
-- it is. The words are printed on the GRANTER, so a text change affecting that
-- permanent rewrites them before layer 6 hands them over.
rewriteGrantedAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> GrantedAbility.GrantedAbility Card.Type.Card -> GrantedAbility.GrantedAbility Card.Type.Card
rewriteGrantedAbility pairs granted = case granted of
  GrantedAbility.Activated a -> GrantedAbility.Activated (rewriteActivatedAbility pairs a)
  GrantedAbility.Triggered t -> GrantedAbility.Triggered (rewriteTriggeredAbility pairs t)

-- CR 612.1 over a TRIGGERED ability printed on a permanent. Three parts, not
-- just the payload: the CR 603.8 condition is where the word usually is, and CR
-- 603.4's intervening "if" shares rewriteCondition.
rewriteTriggeredAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TriggeredAbility Card.Type.Card -> TriggeredAbility Card.Type.Card
rewriteTriggeredAbility pairs ability =
  ability
    { TriggeredAbility.condition = rewriteTriggerCondition pairs (TriggeredAbility.condition ability),
      TriggeredAbility.intervening = fmap (rewriteCondition pairs) (TriggeredAbility.intervening ability),
      TriggeredAbility.modal = rewriteModal pairs (TriggeredAbility.modal ability)
    }

-- CR 612.1 over a REPLACEMENT effect printed on a permanent, which CR 604.2
-- makes a static ability's continuous effect and so text in the same text box as
-- a triggered ability's. Two carriers: the ability's own "as long as" clause, and
-- the effect itself.
rewritePrintedReplacement :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> PrintedReplacement.PrintedReplacement (Effect.Effect Card.Type.Card) -> PrintedReplacement.PrintedReplacement (Effect.Effect Card.Type.Card)
rewritePrintedReplacement pairs printed =
  printed
    { PrintedReplacement.condition = fmap (rewriteCondition pairs) (PrintedReplacement.condition printed),
      PrintedReplacement.effect = rewriteReplacementEffect pairs (PrintedReplacement.effect printed)
    }

-- CR 612.1 through the replacement effect itself: the EVENT PATTERN saying which
-- objects it watches, and the REWRITE it applies to one. Exhaustive rather than a
-- wildcard, in rewriteTriggerCondition's posture -- a later arm carrying a word
-- fails to compile here instead of silently keeping the printed one.
--
-- Classification, not identity: every arm is a CR 614.1 event class, and the
-- descent is by the field shapes those classes carry.
rewriteReplacementEffect :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ReplacementEffect (Effect.Effect Card.Type.Card) -> ReplacementEffect (Effect.Effect Card.Type.Card)
rewriteReplacementEffect pairs effect = case effect of
  -- CR 400.3's owner and the destination Zone name no word; the moving object's
  -- Filter does.
  ReplacementEffect.ZoneChangeR r ->
    ReplacementEffect.ZoneChangeR
      r
        { ZoneChangeR.matching =
            (ZoneChangeR.matching r)
              { ZoneChangePattern.whatObject = Filter.rewrite pairs (ZoneChangePattern.whatObject (ZoneChangeR.matching r))
              }
        }
  ReplacementEffect.EntryR r ->
    ReplacementEffect.EntryR
      r
        { EntryR.matching = Filter.rewrite pairs (EntryR.matching r),
          EntryR.rewrite = rewriteEntryRewrite pairs (EntryR.rewrite r)
        }
  -- The pattern's two Filters, and CR 615.5's riders. Its DamageKind, its
  -- Recipient and its ObjectId are a rules category and two baked identities, so
  -- none of the three holds a printed word; the DamageRewrite holds numbers, a
  -- Scaling and a Recipient.
  ReplacementEffect.DamageR r ->
    ReplacementEffect.DamageR
      r
        { DamageR.matching =
            (DamageR.matching r)
              { DamagePattern.whatSource = Filter.rewrite pairs (DamagePattern.whatSource (DamageR.matching r)),
                DamagePattern.whatRecipient = fmap (Filter.rewrite pairs) (DamagePattern.whatRecipient (DamageR.matching r))
              },
          DamageR.riders = fmap (rewriteEffect pairs) (DamageR.riders r)
        }
  -- CR 701.19a's regeneration and CR 122.1c's shield: two nullary rewrites with
  -- no pattern beside them, so there is nothing to swap.
  ReplacementEffect.DestructionR _ -> effect
  ReplacementEffect.CounterR r ->
    ReplacementEffect.CounterR
      r
        { CounterR.matching =
            (CounterR.matching r)
              { CounterPattern.whichKind = fmap (Filter.rewriteCounterKind pairs) (CounterPattern.whichKind (CounterR.matching r)),
                CounterPattern.onWhat = Filter.rewrite pairs (CounterPattern.onWhat (CounterR.matching r))
              }
        }
  -- A TokenPattern is one ControllerRelation and a Scaling is a number: CR 111.1 /
  -- 614.1's token-creation replacement asks WHOSE tokens, never which ones.
  ReplacementEffect.TokenR _ -> effect
  ReplacementEffect.TurnUpR r ->
    ReplacementEffect.TurnUpR
      r
        { TurnUpR.matching = Filter.rewrite pairs (TurnUpR.matching r),
          TurnUpR.rewrite = case TurnUpR.rewrite r of
            TurnUpRewrite.WithCounters w -> TurnUpRewrite.WithCounters (rewriteWithCounters pairs w)
            TurnUpRewrite.MayAttachTo f -> TurnUpRewrite.MayAttachTo (Filter.rewrite pairs f)
        }
  -- A PhasePattern is a PhaseSelector and a baked seat (CR 614.1b / 500.11), both
  -- rules categories rather than printed words.
  ReplacementEffect.PhaseR _ -> effect

-- CR 612.1 through what a CR 614.1c/614.1d entry replacement does. Exhaustive for
-- rewriteReplacementEffect's reason.
rewriteEntryRewrite :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> EntryRewrite.EntryRewrite (Effect.Effect Card.Type.Card) -> EntryRewrite.EntryRewrite (Effect.Effect Card.Type.Card)
rewriteEntryRewrite pairs rewrite = case rewrite of
  -- CR 707.9's exceptions set power and toughness; only the "which permanents"
  -- clause names a word.
  EntryRewrite.AsCopy c -> EntryRewrite.AsCopy c {AsCopy.eligible = Filter.rewrite pairs (AsCopy.eligible c)}
  -- CR 702.14a's word again, this time inside a keyword an option grants.
  EntryRewrite.ChoiceOf os -> EntryRewrite.ChoiceOf (fmap (\o -> o {EntryOption.keywords = Set.map (Filter.rewriteKeyword pairs) (EntryOption.keywords o)}) os)
  -- CR 105.1's five colours, CR 305.6's five basic land types and CR 102.1's
  -- seats are the offers themselves, so none of the three prints a word the card
  -- chose.
  EntryRewrite.ChooseColor -> rewrite
  EntryRewrite.ChooseBasicLandType -> rewrite
  EntryRewrite.ChoosePlayer -> rewrite
  -- CR 201.4a's restriction on which names may be named.
  EntryRewrite.ChooseCardNames f -> EntryRewrite.ChooseCardNames (Filter.rewrite pairs f)
  EntryRewrite.WithCounters w -> EntryRewrite.WithCounters (rewriteWithCounters pairs w)
  EntryRewrite.UnderSourceControl -> rewrite
  EntryRewrite.SacrificeAnyNumber s ->
    EntryRewrite.SacrificeAnyNumber
      s
        { SacrificeAnyNumber.filter = Filter.rewrite pairs (SacrificeAnyNumber.filter s),
          SacrificeAnyNumber.kind = fmap (Filter.rewriteCounterKind pairs) (SacrificeAnyNumber.kind s)
        }
  -- Rules 702.136a, 702.98a and 702.54a state these three whole, bloodthirst's
  -- number included, so the card prints no word for CR 612.1 to reach.
  EntryRewrite.Riot -> rewrite
  EntryRewrite.Unleash -> rewrite
  EntryRewrite.Bloodthirst _ -> rewrite
  -- CR 614.1d's bare "enters tapped", and the life total CR 614.1c's alternative
  -- to it asks for: a tap status and a number.
  EntryRewrite.Tapped -> rewrite
  EntryRewrite.PayLifeOrTapped _ -> rewrite
  -- CR 614.1c's "reveal a [matching] card": Rustic Clachan's says Kithkin, a
  -- creature type word CR 612.2 licenses. Latent all the same: that EntryR
  -- matches Filter.IsSource, so a text change would have to be on the permanent
  -- before it entered, and CR 400.7 is what forbids carrying one there.
  EntryRewrite.RevealOrTapped f -> EntryRewrite.RevealOrTapped (Filter.rewrite pairs f)
  EntryRewrite.EntersTransformed -> rewrite
  -- CR 614.1c's "as this enters, [do something]", the payload shared with an
  -- ability's clauses.
  EntryRewrite.RunEffects es -> EntryRewrite.RunEffects (fmap (rewriteEffect pairs) es)

-- CR 122.1b's keyword counter is the one counter kind holding a word; the amount
-- is a number.
rewriteWithCounters :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> WithCounters.WithCounters -> WithCounters.WithCounters
rewriteWithCounters pairs w = w {WithCounters.kind = Filter.rewriteCounterKind pairs (WithCounters.kind w)}

-- The modal payload both abilities carry. Both halves of a mode: its clauses'
-- effects, and its TARGET SLOTS, whose Filter is the candidate set CR 601.2c
-- (imported by CR 602.2b) reads. The Pool is not an omission: it names a rules
-- category (CR 115) rather than a word printed on the card.
rewriteModal :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modal.Modal Card.Type.Card -> Modal.Modal Card.Type.Card
rewriteModal pairs modal =
  let rewriteClause c =
        c
          { Clause.effects = fmap (rewriteEffect pairs) (Clause.effects c),
            -- Not implemented: the clause's CR 118.12 pay gate keeps its printed
            -- word, so the resolution-time offer asks the pre-hack question
            -- (#635).
            Clause.condition = fmap (rewriteCondition pairs) (Clause.condition c)
          }
      rewriteMode m =
        m
          { Mode.clauses = fmap rewriteClause (Mode.clauses m),
            Mode.targetSlots = fmap (rewriteTargetSlot pairs) (Mode.targetSlots m)
          }
   in modal {Modal.modes = fmap rewriteMode (Modal.modes modal)}

-- A single target slot under CR 612.1. Top-level because Pawl.Engine.Resolve
-- needs the same rewrite over a resolving spell's slots (CR 608.2b).
rewriteTargetSlot :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TargetSlot.TargetSlot -> TargetSlot.TargetSlot
rewriteTargetSlot pairs slot = slot {TargetSlot.filter = fmap (Filter.rewrite pairs) (TargetSlot.filter slot)}

-- CR 612.1 through a trigger's own condition. Exhaustive rather than a wildcard,
-- so a later condition carrying a Filter fails to compile here instead of
-- silently keeping the printed word.
rewriteTriggerCondition :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TriggerCondition.TriggerCondition -> TriggerCondition.TriggerCondition
rewriteTriggerCondition pairs condition = case condition of
  TriggerCondition.StateIs c -> TriggerCondition.StateIs (rewriteCondition pairs c)
  TriggerCondition.PermanentEnters f -> TriggerCondition.PermanentEnters (Filter.rewrite pairs f)
  TriggerCondition.PermanentDies f -> TriggerCondition.PermanentDies (Filter.rewrite pairs f)
  TriggerCondition.PermanentLeavesTheBattlefield f -> TriggerCondition.PermanentLeavesTheBattlefield (Filter.rewrite pairs f)
  -- The TurnScope is carried through untouched, not dropped: a rebuild that
  -- forgot the field would reset a text-changed trigger to firing every turn.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast f scope fromZone ordinal) -> TriggerCondition.SpellCast (SpellCast.MkSpellCast (Filter.rewrite pairs f) scope fromZone ordinal)
  TriggerCondition.SelfEnters -> condition
  TriggerCondition.StepBegins {} -> condition
  TriggerCondition.SelfDealsCombatDamageToPlayer -> condition
  TriggerCondition.SelfIsDealtDamage -> condition
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.rewrite pairs f)
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> condition
  TriggerCondition.OpponentLostLifeDuringYourTurn -> condition
  TriggerCondition.SelfCycled -> condition
  TriggerCondition.SelfRevealedForMiracle -> condition
  TriggerCondition.SelfDiscarded -> condition
  TriggerCondition.SelfCast -> condition
  TriggerCondition.SelfBecomesTargeted _ -> condition
  TriggerCondition.ControllerBecomesTarget {} -> condition
  TriggerCondition.PlayerDiscards _ -> condition
  TriggerCondition.PlayerCycles _ -> condition
  TriggerCondition.PlayerDrawsNthCard {} -> condition
  TriggerCondition.PlayerBecomesMonarch _ -> condition
  TriggerCondition.SelfAttacks _ -> condition
  TriggerCondition.SelfAttacksWithAnother f -> TriggerCondition.SelfAttacksWithAnother (Filter.rewrite pairs f)
  TriggerCondition.CreatureAttacksAlone f -> TriggerCondition.CreatureAttacksAlone (Filter.rewrite pairs f)
  TriggerCondition.CreatureAttacksYou -> condition
  TriggerCondition.AttachedPlayerIsAttacked -> condition
  TriggerCondition.YouAttack -> condition
  TriggerCondition.SelfAttacksPlayerWithMostLife -> condition
  TriggerCondition.SelfBlocks -> condition
  TriggerCondition.SelfBlocksCreature f -> TriggerCondition.SelfBlocksCreature (Filter.rewrite pairs f)
  TriggerCondition.SelfBlocksAtLeast _ -> condition
  TriggerCondition.SelfBlocksOneOrMore f -> TriggerCondition.SelfBlocksOneOrMore (Filter.rewrite pairs f)
  TriggerCondition.SelfBecomesBlocked -> condition
  TriggerCondition.SelfBecomesBlockedBy f -> TriggerCondition.SelfBecomesBlockedBy (Filter.rewrite pairs f)
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> TriggerCondition.SelfBecomesBlockedByOneOrMore (Filter.rewrite pairs f)
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> condition
  TriggerCondition.SelfAttacksUnblocked -> condition
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> condition
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> condition
  TriggerCondition.SelfDies -> condition
  TriggerCondition.SelfLeavesTheBattlefield -> condition
  TriggerCondition.HauntedCreatureDies -> condition
  TriggerCondition.SpellOrAbilityCounters _ -> condition
  TriggerCondition.DamageToPlayerPrevented _ -> condition
  TriggerCondition.PlayerGainsLife _ -> condition
  TriggerCondition.PlayerLosesLife _ -> condition
  TriggerCondition.SelfCountersReached {} -> condition
  TriggerCondition.SelfLastCounterRemoved _ -> condition
  TriggerCondition.SelfCountersRemoved _ -> condition
  TriggerCondition.SelfHalfUnlocked _ -> condition
  TriggerCondition.RoomFullyUnlocked _ -> condition
  TriggerCondition.AnyOf conditions -> TriggerCondition.AnyOf (fmap (rewriteTriggerCondition pairs) conditions)
  TriggerCondition.SelfTurnedFaceUp -> condition
  -- CR 612.1's text change swaps SUBTYPES here (Pawl.Types.ChangeText's pairs);
  -- CR 701.27e's payload is a face's NAME, which is not one, so nothing in this
  -- condition is rewritten. SelfHalfUnlocked's answer for its own door.
  TriggerCondition.SelfTransformedInto _ -> condition
  TriggerCondition.PermanentTurnedFaceUp f -> TriggerCondition.PermanentTurnedFaceUp (Filter.rewrite pairs f)
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated d f) -> TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated d (Filter.rewrite pairs f))
  TriggerCondition.SelfEvolves -> condition
  TriggerCondition.AttachedCreatureMentors -> condition
  TriggerCondition.AttachedCreatureDies -> condition
  TriggerCondition.SelfTrains -> condition
  TriggerCondition.PermanentSacrificed -> condition
  TriggerCondition.SagaFinalChapterTriggers _ -> condition
  -- CR 603.7's slot name is card data but not card TEXT, so no CR 612.1 swap
  -- reaches it; what the slot holds is read off the projection instead.
  TriggerCondition.LoseControlOfBound _ -> condition
  TriggerCondition.RoomEntered _ -> condition
  TriggerCondition.PlayerScries _ -> condition
  TriggerCondition.PlayerSurveils _ -> condition
  TriggerCondition.SelfBecomesPlotted -> condition
  TriggerCondition.PermanentExplores f -> TriggerCondition.PermanentExplores (Filter.rewrite pairs f)
  TriggerCondition.SelfExerted -> condition
  TriggerCondition.SelfBecomesAttachedBy f -> TriggerCondition.SelfBecomesAttachedBy (Filter.rewrite pairs f)
  -- CR 603.12's reflexive carries nothing at all, so there is no subtype to swap.
  TriggerCondition.Reflexive -> condition

-- CR 612.1 through Condition's predicate vocabulary. A Condition reached through
-- a CR 611.2b duration comes here by way of rewriteDuration below.
rewriteCondition :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Condition.Type.Condition -> Condition.Type.Condition
rewriteCondition pairs condition = case condition of
  Condition.Type.Compares c ->
    Condition.Type.Compares
      c
        { Compares.measured = rewriteQuantity pairs (Compares.measured c),
          Compares.threshold = rewriteQuantity pairs (Compares.threshold c)
        }
  Condition.Type.Any conditions -> Condition.Type.Any (fmap (rewriteCondition pairs) conditions)
  Condition.Type.All conditions -> Condition.Type.All (fmap (rewriteCondition pairs) conditions)

-- CR 612.1 through a Duration, which Pawl.Types.Duration holds as the card
-- prints it: a CR 611.2b "for as long as ..." clause is rules text like any
-- other, so the words inside its Condition are reachable. Every other arm is a
-- turn-structure window and names none.
--
-- Exhaustive rather than a wildcard, for rewriteTriggerCondition's reason: a new
-- arm carrying a Condition or a Filter must break this build.
rewriteDuration :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Duration.Duration -> Duration.Duration
rewriteDuration pairs duration = case duration of
  Duration.ForAsLongAs condition -> Duration.ForAsLongAs (rewriteCondition pairs condition)
  Duration.UntilEndOfTurn -> duration
  Duration.Indefinite -> duration
  Duration.UntilYourNextTurn -> duration
  Duration.UntilEndOfYourNextTurn -> duration
  Duration.UntilEndOfCombat -> duration
  -- CR 116.2c's price, which is a Cost and not a Condition. An activated
  -- ability's own cost is left alone by this descent for the same reason: no
  -- Cost arm carries a subtype word outside a Filter, and the Filters a cost's
  -- non-mana components hold are matched where the cost is paid.
  Duration.UntilPaid _ -> duration

-- CR 612.1 through a Quantity: a Count's Filter is where the subtype word hides,
-- and its Aggregation may name a further Quantity. Every remaining arm is a leaf.
rewriteQuantity :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Quantity.Type.Quantity -> Quantity.Type.Quantity
rewriteQuantity pairs quantity = case quantity of
  Quantity.Type.Count c ->
    Quantity.Type.Count
      c
        { Count.Type.filter = Filter.rewrite pairs (Count.Type.filter c),
          Count.Type.aggregation = rewriteAggregation pairs (Count.Type.aggregation c)
        }
  Quantity.Type.Plus (Plus.MkPlus x y) -> Quantity.Type.Plus (Plus.MkPlus (rewriteQuantity pairs x) (rewriteQuantity pairs y))
  Quantity.Type.Halved (Halved.MkHalved rounding inner) -> Quantity.Type.Halved (Halved.MkHalved rounding (rewriteQuantity pairs inner))
  Quantity.Type.Negate x -> Quantity.Type.Negate (rewriteQuantity pairs x)
  Quantity.Type.Literal _ -> quantity
  Quantity.Type.ManaValue -> quantity
  Quantity.Type.Power -> quantity
  Quantity.Type.Toughness -> quantity
  Quantity.Type.InSlot _ -> quantity
  Quantity.Type.Star -> quantity
  Quantity.Type.ManaCount _ -> quantity
  Quantity.Type.LifeTotal _ -> quantity
  Quantity.Type.Speed _ -> quantity
  Quantity.Type.IsMonarch _ -> quantity
  Quantity.Type.IsStartingPlayer _ -> quantity
  Quantity.Type.IsActivePlayer _ -> quantity
  Quantity.Type.HasDesignation _ -> quantity
  Quantity.Type.ClassLevel -> quantity
  Quantity.Type.WasKicked -> quantity
  Quantity.Type.SnowWasSpent -> quantity
  Quantity.Type.PlayerCounters {} -> quantity
  Quantity.Type.ObjectCounters _ -> quantity
  Quantity.Type.OpponentsAttacked _ -> quantity
  Quantity.Type.CardsDiscardedThisTurn _ -> quantity
  Quantity.Type.PlayersDealtDamageThisTurn _ -> quantity
  Quantity.Type.EnteredThisTurn -> quantity
  Quantity.Type.BlockersBeyondFirst -> quantity
  -- Not a leaf: the payload is a whole Quantity and may hide a Count.
  Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot slot (rewriteQuantity pairs inner))

-- Greatest is the only Aggregation carrying a Quantity.
rewriteAggregation :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Aggregation.Aggregation Quantity.Type.Quantity -> Aggregation.Aggregation Quantity.Type.Quantity
rewriteAggregation pairs aggregation = case aggregation of
  Aggregation.Greatest q -> Aggregation.Greatest (rewriteQuantity pairs q)
  Aggregation.Members -> aggregation
  Aggregation.DistinctCardTypes -> aggregation

-- CR 612.1 through CR 208.2a's characteristic-defining power and toughness. Both
-- boxes are rewritten rather than only the one a card fills, since seedCharacteristicPT
-- already keeps them as a pair.
rewriteCharacteristicPT :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> CharacteristicPT.CharacteristicPT -> CharacteristicPT.CharacteristicPT
rewriteCharacteristicPT pairs cda =
  CharacteristicPT.MkCharacteristicPT
    { CharacteristicPT.power = rewriteQuantity pairs (CharacteristicPT.power cda),
      CharacteristicPT.toughness = rewriteQuantity pairs (CharacteristicPT.toughness cda)
    }

-- Every continuous effect in the game: stored resolution effects, plus every
-- battlefield permanent's static abilities (CR 613.7a, with the permanent's own
-- timestamp), dropping a permanent whose static abilities are not live (CR
-- 305.7). Not filtered by object here -- project filters per layer against the
-- partial.
--
-- CR 613.6: the affected set belongs to the EFFECT rather than each of its parts,
-- so the parts of one static ability all carry that ability's key. A stored
-- effect or counter is a single part and carries none.
--
-- Three ability losses: CR 305.7's land-subtype strip (liveGiven) drops the
-- permanent outright, CR 613.1f's layer-6 removal (abilitiesRemoved) drops only
-- an ability whose every part lands after layer 6, and CR 604.2's "as long as"
-- gate drops one whose clause is currently false. Neither of the first two
-- touches a stored effect or a counter (CR 611.2a; CR 122.1a/613.4c). The last
-- needs a projection, so it is answered against the SEED list below, which is
-- built with every gate open -- nothing here re-enters gather.
gather :: GameState -> [Gathered]
gather gs =
  let ungated = gatherGiven (const False) alwaysFunctioning Nothing gs
   in -- Almost every board has no ability-removing effect, no conditional static
      -- ability and nothing setting a land's subtype, and then the gathered list
      -- IS the ungated one.
      if any (removesAbilities . gModification) ungated || anyConditional gs || any (setsLandSubtype . gModification) ungated
        then gatherGiven (abilitiesRemoved ungated gs) (conditionHolds ungated gs) (Just ungated) gs
        else ungated

-- The open CR 604.2 gate: every "as long as" clause answered true without being
-- looked at. What the seed pass gets, so the list the real gate reads is the
-- widest one -- an ability wrongly kept there can only over-project the state a
-- condition is judged against, never leave gather to re-enter itself.
--
-- Not implemented: controlGrants reads the printed list without the gate at all
-- (#1529).
alwaysFunctioning :: ObjectId -> Layer -> Condition.Type.Condition -> Bool
alwaysFunctioning _ _ _ = True

-- Does any static ability in play carry a CR 604.2 "as long as" clause at all?
-- gather's cheap structural precondition, a pure read of the printed faces with
-- no projection behind it. Emblems, the stack, graveyards, hands and libraries
-- are all walked, because gatherGiven gathers abilities from each (CR 114.4 /
-- 113.6 / 113.6b / 113.6f) and skipping one would leave its clause wired open by
-- alwaysFunctioning.
anyConditional :: GameState -> Bool
anyConditional gs =
  let conditional oid = case Game.faceOf oid gs of
        Nothing -> False
        Just face -> any (Maybe.isJust . StaticAbility.condition) (Face.staticAbilities face)
      conditionalStating zone oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj | not (mayStateZone gs zone obj) -> False
        Just obj -> case Game.faceOfObject gs obj of
          Nothing -> False
          Just face -> any (\sa -> Maybe.isJust (StaticAbility.condition sa) && statesZone zone sa) (Face.staticAbilities face)
   in any conditional (Set.toList (GameState.battlefield gs))
        || any conditional (Set.toList (GameState.command gs))
        || any conditional (GameState.stack gs)
        -- Every card in the graveyard rather than only the ones whose ability
        -- gatherGiven will keep: a superset costs a second walk, where a subset
        -- ungates a clause.
        || any conditional (graveyardCards gs)
        -- The two hidden zones narrow instead, and cost nothing for it: CR
        -- 113.6b's stated set is a printed field, so asking it here is the same
        -- read this function was already doing, and an ability that does not
        -- state the zone is one gatherGiven's hidden walks cannot keep however
        -- the clause answers. Without the narrowing an ordinary Kird Ape in hand
        -- would buy a second whole-board walk on every projection.
        || anyZoneCard GameState.hand (conditionalStating Zone.Hand) gs
        || anyZoneCard GameState.library (conditionalStating Zone.Library) gs

-- CR 604.2: is this static ability's "as long as" clause true right now?
--
-- The VIEW is bounded at the ability's own lowest layer -- CR 613.6's decision
-- point, and where abilitiesRemoved judges a remover's affected set. So Kird
-- Ape's layer-7c clause reads a Forest through layers 1-6. Bounded rather than
-- full because a condition read against a projection including its OWN layer
-- would be circular, and the descending bound is what makes the nesting
-- terminate.
--
-- CR 109.5: "you" is the SOURCE's controller, and the condition is evaluated
-- against the source. CR 303.4b: one of the four sites supplying the source's
-- host, read live off Object.attachedTo, so an Aura moved by CR 701.3a names its
-- new host on the next pass.
conditionHolds :: [Gathered] -> GameState -> ObjectId -> Layer -> Condition.Type.Condition -> Bool
conditionHolds cands gs src lowest =
  Condition.holds (viewUpTo lowest cands gs) ((Filter.contextFor (controllerOf src gs) (Just src)) {Filter.sourceAttachedTo = hostOf src gs}) gs src

-- CR 113.6 / 614.12: the battlefield permanents whose static abilities FUNCTION
-- right now. Everything on the battlefield, minus the permanents entering beside
-- the one whose entry loop is running (GameState.enteringBeside), which this
-- engine has already materialized but the rules have not let in yet: CR 614.12
-- admits only the entering permanent's own static abilities and "continuous
-- effects that already exist", and a sibling's are neither.
--
-- Empty of exclusions at every priority window, so outside an entry loop this is
-- the battlefield. Three walks read it -- this module's static-ability gather,
-- its CR 305.7 set-subtype scan and its layer-2 control grants -- which together
-- are every place a permanent's own printed static ability becomes a continuous
-- effect. Only the FIRST has an observer: Pawl.ReplacementSpec's "a Wood Elemental
-- reanimated beside Ashaya sacrifices nothing" goes red when it is widened back to
-- the whole battlefield, and neither of the other two moves a case, because
-- nothing in `data/cards/` puts a control-changer or a Blood Moon-shaped subtype
-- setter into a batch. Those two are regression fences, kept because a projection
-- that suppressed a sibling's ability in one walk and not the next would disagree
-- with itself.
--
-- anyConditional deliberately does NOT narrow by it: a superset costs a second
-- walk where a subset would leave a CR 604.2 clause wired open. Nor does
-- Pawl.Engine.Replacement.replacementsAffecting, whose siblings Event.loop
-- already drops by SOURCE (channel 2 of applyReplacementsIn's note).
abilitySources :: GameState -> [ObjectId]
abilitySources gs = Set.toList (Set.difference (GameState.battlefield gs) (GameState.enteringBeside gs))

-- gather's body with both ability gates left open. Called twice by gather --
-- once wired shut to build the list the gates read, once with the real answers.
gatherGiven :: (ObjectId -> Bool) -> (ObjectId -> Layer -> Condition.Type.Condition -> Bool) -> Maybe [Gathered] -> GameState -> [Gathered]
gatherGiven stripped functioning seed gs =
  let setEffs = setLandSubtypeEffectsGiven functioning gs
      -- CR 305.7's post-layer-4 half, wired open in the seed pass for the reason
      -- `stripped` is: the list this gate projects against is the seed itself.
      setStripped = case seed of
        Nothing -> const False
        Just cands -> setSubtypeStripped cands setEffs gs
      -- A stored effect carries exactly one modification, so CR 613.6 has
      -- nothing to hold together, and its set is CR 611.2c's TheseObjects,
      -- locked when the effect began.
      fromStored eff =
        MkGathered
          { gEffect = Nothing,
            gSource = ContinuousEffect.source eff,
            gAffected = ContinuousEffect.affected eff,
            gLayer = layer (ContinuousEffect.modification eff),
            gLowest = layer (ContinuousEffect.modification eff),
            gTimestamp = ContinuousEffect.timestamp eff,
            gModification = ContinuousEffect.modification eff
          }
      stored = fmap fromStored (GameState.continuousEffects gs)
      static = concatMap (fmap snd . permanentParts stripped functioning setEffs setStripped gs) (abilitySources gs)
      fromEmblem emblemId = case Game.lookupObject emblemId gs of
        Nothing -> []
        Just emblemObj -> case Game.faceOfObject gs emblemObj of
          Nothing -> []
          Just face ->
            -- CR 114.4 / 113.6: an emblem's abilities function in the command
            -- zone, sharing the emblem's timestamp (CR 613.7a). Never stripped:
            -- the pool's CR 613.1f removers reach creatures (CR 114.5).
            concat [gatherStatic (functioning emblemId) emblemId (Object.timestamp emblemObj) [] (const False) n sa | (n, sa) <- zip [0 :: Natural ..] (Face.staticAbilities face), functionsFromZone Zone.Command sa]
      emblems = concatMap fromEmblem (Set.toList (GameState.command gs))
      fromSpell spellId = case Game.lookupObject spellId gs of
        Nothing -> []
        Just spellObj -> case Game.faceOfObject gs spellObj of
          Nothing -> []
          Just face ->
            -- CR 604.2's second limb and CR 113.6: an instant's or sorcery's
            -- abilities function while it is on the stack. Read off the CARD
            -- TYPES, a classification rather than an identity. That read is the
            -- DEFAULT, which CR 113.6b's stated set overrides in both
            -- directions: a permanent spell's statics are not gathered here
            -- unless the ability names the stack -- Grist, the Hunger Tide's CR
            -- 113.6c clause names every zone but the battlefield, so a Grist
            -- SPELL is a creature spell -- and an instant's are dropped where it
            -- names some other zone (Viral Spawning). The other exceptions that
            -- reach the stack (CR 113.6d/e/g) are asked elsewhere, off the
            -- printed face. CR 613.7a: the effect shares the stack object's
            -- timestamp. Never stripped, for the emblem branch's reason.
            let isSpellStatic = not (Set.null (Set.intersection spellStaticTypes (TypeLine.types (Face.typeLine face))))
                keeps sa =
                  if Set.null (StaticAbility.functionsFrom sa)
                    then isSpellStatic
                    else statesZone Zone.Stack sa
             in concat [gatherStatic (functioning spellId) spellId (Object.timestamp spellObj) [] (const False) n sa | (n, sa) <- zip [0 :: Natural ..] (Face.staticAbilities face), keeps sa]
      spells = concatMap fromSpell (GameState.stack gs)
      fromGraveyardCard cardId = case Game.lookupObject cardId gs of
        Nothing -> []
        Just cardObj -> case Game.faceOfObject gs cardObj of
          Nothing -> []
          Just face ->
            -- CR 113.6f: an ability that restricts or modifies which zones its
            -- object can be cast from functions everywhere -- Viral Spawning's
            -- granted flashback (rule 702.34a) is one. WHICH abilities qualify
            -- is asked of rule 702 through Keyword.permissionsFor, a
            -- classification rather than an identity. The PRINTED type line
            -- answers rule 702.34a's instant-or-sorcery clause, since reading it
            -- off the projection this walk is building would be circular. CR
            -- 613.7a: the effect shares the card's own timestamp. Never
            -- stripped, for the emblem and spell branches' reason.
            --
            -- CR 113.6b takes precedence where an ability states its zones,
            -- CR 113.6f's classification decides where it does not, and the two
            -- meet on Viral Spawning: its Corrupted ability grants flashback AND
            -- names the graveyard, so it is kept here and dropped by `fromSpell`
            -- above rather than gathered twice.
            --
            -- CR 113.6c's negative form arrives here as a stated set holding
            -- the graveyard, so Grist, the Hunger Tide is kept by the second
            -- limb rather than by CR 113.6f's classification.
            let qualifies sa = any (grantsKeywordWhere (castZoneKeyword face)) (StaticAbility.modifications sa)
                keeps sa =
                  if Set.null (StaticAbility.functionsFrom sa)
                    then qualifies sa
                    else statesZone Zone.Graveyard sa
                indexed = zip [0 :: Natural ..] (Face.staticAbilities face)
             in concat [gatherStatic (functioning cardId) cardId (Object.timestamp cardObj) [] (const False) n sa | (n, sa) <- indexed, keeps sa]
      graveyards = concatMap fromGraveyardCard (graveyardCards gs)
      -- CR 113.6b/c: the two HIDDEN zones (CR 400.2), which no default in CR
      -- 113.6 ever reaches -- a card in a hand or a library has its abilities
      -- function only where CR 113.6b's stated set says so. So this arm asks
      -- `statesZone` rather than `functionsFromZone`: an ability that states no
      -- zone must NOT be gathered here, or every creature card in a library
      -- would start pumping the board from inside it. CR 613.7a: the effect
      -- shares the card's own timestamp. Never stripped, for the emblem
      -- branch's reason.
      --
      -- Walked on EVERY projection, once per card in every hand and every
      -- library, so what the walk costs per card is the whole of what it costs:
      -- mayStateZone below settles the common card without building a face, and
      -- Game.faceOfObject takes one lookup where the chain through Game.faceOf
      -- took three. Not implemented: nothing asserts what that walk costs per
      -- card -- a per-library-card ceiling held both until measuring bytes was
      -- judged too compiler-specific to keep (gap #578). See #1935, which
      -- measured the walk at 26% of the suite before them.
      --
      -- Not implemented: exile gets no arm of its own, so a stated set naming it
      -- is ignored -- Grist's does (gap #1933).
      fromHiddenCard zone cardId = case Game.lookupObject cardId gs of
        Nothing -> []
        Just cardObj | not (mayStateZone gs zone cardObj) -> []
        Just cardObj -> case Game.faceOfObject gs cardObj of
          Nothing -> []
          Just face ->
            concat [gatherStatic (functioning cardId) cardId (Object.timestamp cardObj) [] (const False) n sa | (n, sa) <- zip [0 :: Natural ..] (Face.staticAbilities face), statesZone zone sa]
      hands = foldZoneCards GameState.hand (fromHiddenCard Zone.Hand) gs
      libraries = foldZoneCards GameState.library (fromHiddenCard Zone.Library) gs
      counters = counterGathered gs
      designations = designationGathered gs
   in stored <> static <> emblems <> spells <> graveyards <> hands <> libraries <> counters <> designations

-- CR 113.6b: does this static ability function from `zone`? THE zone
-- classification -- one question, asked by each of gatherGiven's static-ability
-- walks that HAS a CR 113.6 default to fall back on and by the two battlefield
-- walks beside them
-- (setLandSubtypeEffectsGiven and controlGrants), so a permanent's printed
-- ability cannot become a continuous effect through one of those three and not
-- the others.
--
-- An EMPTY set is an ability that states no zone, and then CR 113.6's own
-- defaults stand -- which is what makes the caller's zone argument the whole
-- answer for nearly every ability in the pool. A stated set is CR 113.6b's
-- "only", so it replaces those defaults rather than adding to them;
-- `fromGraveyardCard` above is the one caller that has to tell the two cases
-- apart, because the default it would otherwise override is CR 113.6f's
-- classification rather than a bare zone. The hand and library walks ask
-- `statesZone` below instead, having no default at all to fall back on.
--
-- Structural, and deliberately not a Condition: CR 604.2's clause is asked of an
-- ability some walk has already kept, so it could narrow a gather but never
-- widen one.
functionsFromZone :: Zone.Zone -> StaticAbility.StaticAbility card -> Bool
functionsFromZone zone sa =
  let zones = StaticAbility.functionsFrom sa
   in Set.null zones || Set.member zone zones

-- CR 113.6b's stated set, without the empty-set default that
-- functionsFromZone folds in: does this ability SAY it functions from `zone`?
-- The question the two hidden zones take, where "states no zone" has to mean
-- "not here" rather than "wherever the caller is looking".
statesZone :: Zone.Zone -> StaticAbility.StaticAbility card -> Bool
statesZone zone = Set.member zone . StaticAbility.functionsFrom

-- statesZone asked of an object's CARD rather than of the face it is showing:
-- could any face this object might be showing state `zone`?
--
-- A SUPERSET, and that is the whole of its correctness argument. Every face an
-- object shows is one of its card's faces (CR 709.3b, CR 712.8a, CR 715.4) or a
-- merge of several (CR 709.4's combined view, CR 709.5's subtracted one), and
-- Card.merge2 builds a merge out of the halves' own fields -- so no shown face
-- carries a static ability that no printed face carries. A True answer decides
-- nothing; a False answer means the exact test below cannot keep anything.
--
-- Here because it is CHEAP where the exact test is not: a field read and a fold
-- over the printed faces, against BUILDING the face the object shows --
-- Game.resolveFaceFor's layout case, a NonEmpty, and Card.foldSplit's merge.
-- gatherGiven's hidden walks read it once per card in every hand and every
-- library on every projection, so that difference is the walk; see #1935.
--
-- A FACE-DOWN object is the one case it cannot narrow, and it does not try: CR
-- 708.2's substituted face comes from the ability that turned the object down
-- rather than from its card, so this answers True and leaves the work to the
-- exact test in gatherGiven.
mayStateZone :: GameState -> Zone.Zone -> Object.Object -> Bool
mayStateZone gs zone obj = case Object.facing obj of
  Facing.FaceDown _ -> True
  Facing.FaceUp -> case Game.cardOfSource gs (Just (Object.source obj)) of
    Nothing -> False
    Just card -> any (any (statesZone zone) . Face.staticAbilities) (Card.Type.faces card)

-- Fold over every card in one per-player zone, across every player, WITHOUT
-- materializing the ids: gatherGiven walks the two hidden zones on every
-- projection, and a list of every card in every library allocated and thrown
-- away each time is the other half of what #1935 measured. anyZoneCard below is
-- the `any` companion, which short-circuits and so cannot go through a Monoid.
foldZoneCards :: (Monoid m) => (GameState -> Map PlayerId.PlayerId (Seq.Seq ObjectId)) -> (ObjectId -> m) -> GameState -> m
foldZoneCards field f = foldMap (foldMap f) . field

anyZoneCard :: (GameState -> Map PlayerId.PlayerId (Seq.Seq ObjectId)) -> (ObjectId -> Bool) -> GameState -> Bool
anyZoneCard field p = any (any p) . field

-- Every card in every graveyard.
graveyardCards :: GameState -> [ObjectId]
graveyardCards = foldMap (foldr (:) []) . Map.elems . GameState.graveyard

-- CR 113.6f's classification, one keyword at a time: does rule 702 turn this
-- keyword into a permission to cast the object it is on from somewhere? The face
-- supplies rule 702.34a's card types, which is what makes flashback on a
-- creature card grant nothing.
castZoneKeyword :: Face.Face card -> Keyword -> Bool
castZoneKeyword face keyword = not (null (Keyword.permissionsFor (TypeLine.types (Face.typeLine face)) keyword))

-- CR 113.6's first sentence: the card types whose object has its abilities
-- function on the stack rather than on the battlefield. anyConditional
-- deliberately does NOT narrow by this -- a superset costs a second walk, where
-- a subset would leave a CR 604.2 clause wired open.
spellStaticTypes :: Set CardType.CardType
spellStaticTypes = Set.fromList [CardType.Instant, CardType.Sorcery]

-- ONE battlefield permanent's static-ability parts, each tagged with the index
-- of the ability it came from -- gatherStatic's `n`, the key half of CR 613.6's
-- decision memo. Hoisted out of gatherGiven's battlefield walk so that
-- frozenStaticParts gathers a permanent's parts by exactly the walk the fold
-- uses, gates and CR 612 rewrite included; a second copy of this body would
-- freeze a set the fold never applied.
permanentParts :: (ObjectId -> Bool) -> (ObjectId -> Layer -> Condition.Type.Condition -> Bool) -> [(ObjectId, Affected.Affected)] -> (ObjectId -> Bool) -> GameState -> ObjectId -> [(Natural, Gathered)]
permanentParts stripped functioning setEffs setStripped gs permId = case Game.lookupObject permId gs of
  Nothing -> []
  Just permObj -> case Game.faceOf permId gs of
    Nothing -> []
    Just face ->
      if null setEffs || liveGiven setEffs permId gs
        then
          -- CR 612: rewrite each static ability's subtype words by the text
          -- changes affecting THIS source, before its effect is folded on.
          let changes = textChangesAffecting permId gs
              -- CR 613.1f's layer-6 removal and CR 305.7's layer-4 strip, asked
              -- of ONE ability at CR 613.6's decision point rather than of the
              -- permanent as a whole. Each spares an ability whose effect had
              -- ALREADY started applying when the stripper did: rule 613.6 keeps
              -- such an effect applying even though the ability generating it is
              -- gone. An ability deciding AT layer 4 is spared here and left to
              -- the base-characteristics gate above, which is CR 613.8's order
              -- for it -- see liveGiven.
              removed lowest = (lowest > Layer.Ability && stripped permId) || (lowest > Layer.Type && setStripped permId)
              -- One thunk per permanent, shared by all its abilities. Bound
              -- here, OUTSIDE the zipWith, which is what shares it.
              partsOf = gatherStatic (functioning permId) permId (Object.timestamp permObj) changes removed
              -- CR 113.6b, applied WITHOUT disturbing the index: `n` is the key
              -- half of CR 613.6's decision memo and Pawl.Engine.Event's
              -- departure handover indexes Face.staticAbilities by it, so an
              -- ability this rule drops must leave a hole rather than shift its
              -- neighbours up.
              tagged n sa = if functionsFromZone Zone.Battlefield sa then fmap ((,) n) (partsOf n sa) else []
           in concat (zipWith tagged [0 ..] (Face.staticAbilities face))
        else []

-- CR 611.2c, applied to a static ability's effect: the parts `src`'s own static
-- abilities are generating RIGHT NOW, each with the index of the ability it
-- belongs to, the timestamp CR 613.7a gave it, and its affected set resolved to
-- the concrete objects it names at this instant.
--
-- The one reader is Pawl.Engine.Event's departure handover, where an effect that
-- outlives its own permanent (StaticAbility.lingers) becomes a STORED one and CR
-- 611.2c fixes its set. Never called for an ordinary ability, whose set goes on
-- being re-derived every projection (CR 613.5).
--
-- The set is asked ONCE PER ABILITY and copied onto each of its parts (CR
-- 613.6), which is what lets the caller store them as separate
-- single-modification effects without the split being observable.
frozenStaticParts :: ObjectId -> GameState -> [(Natural, Timestamp, Modification, Set ObjectId)]
frozenStaticParts src gs =
  let cands = gather gs
      -- gather's own seed list, and the same one it feeds its two gates.
      ungated = gatherGiven (const False) alwaysFunctioning Nothing gs
      -- gather's CR 604.2 gate, shared by the two readers that must agree on it.
      functioning = conditionHolds ungated gs
      setEffs = setLandSubtypeEffectsGiven functioning gs
      parts = permanentParts (abilitiesRemoved ungated gs) functioning setEffs (setSubtypeStripped ungated setEffs gs) gs src
      applies c oid =
        let lyr = gLowest c
            partial = projectUpTo lyr cands oid gs
            decided = decisionsUpTo lyr cands oid gs
         in case gEffect c >>= (`Map.lookup` decided) of
              Just answer -> answer
              Nothing -> affectsGiven (viewUpTo lyr cands gs) (gSource c) oid (gAffected c) partial gs
      -- One entry per ability rather than per part: the parts of an ability
      -- agree on both gAffected and gLowest, so whichever one Map.fromList keeps
      -- asks the same question. Lazy, so only the retained thunk is forced.
      byAbility = Map.fromList [(n, Set.filter (applies c) (candidatesFor (gAffected c) gs)) | (n, c) <- parts]
   in -- `parts` order, which is the card's PRINTED order and load-bearing once
      -- these become separate stored effects: the fold applies same-layer parts
      -- sharing one timestamp in list order.
      [(n, gTimestamp c, gModification c, Map.findWithDefault Set.empty n byAbility) | (n, c) <- parts]

-- The objects an affected set could possibly name, so freezing one need not
-- project every object in the game. Battlefield-scoped for every arm the
-- battlefield bounds, and the whole pool for MatchingAnywhere.
candidatesFor :: Affected.Affected -> GameState -> Set ObjectId
candidatesFor a gs = case a of
  Affected.Matching _ -> GameState.battlefield gs
  Affected.Attached -> GameState.battlefield gs
  Affected.AttachedPlayerControls _ -> GameState.battlefield gs
  Affected.MatchingAnywhere _ -> Map.keysSet (GameState.objects gs)
  -- Every object the battlefield does NOT hold, which is the arm's own gate.
  Affected.MatchingOffBattlefield _ -> Set.difference (Map.keysSet (GameState.objects gs)) (GameState.battlefield gs)
  -- Already fixed (CR 611.2c), so freezing it is the identity. Returned rather
  -- than special-cased away, so the caller has one shape to filter.
  Affected.TheseObjects s -> s

-- CR 613.1f, hoisted over the whole game: "were THIS object's abilities removed
-- by the time layer 6 finished?", as one predicate. For a caller OUTSIDE the
-- layer machine that must ask it once per battlefield permanent
-- (Pawl.Engine.PlayerEffect.applying, for CR 604.2's "and has the ability"), so
-- the candidate list is built once per read rather than once per permanent.
--
-- The list is gathered with the layer-6 gate OFF, which is what the gate itself
-- reads, and why this needs an extra pass rather than a fixpoint: deciding
-- whether a source's abilities were removed means projecting it no higher than
-- layer 6, and such a projection cannot see the layer-7 parts the gate drops.
--
-- Well-founded because nothing reachable from here reads a player effect back --
-- the module graph enforces it, since Projection does not import PlayerEffect.
--
-- CR 604.2's "as long as" gate IS asked, unlike inside gather: this reader is
-- outside the fold, so it answers the gate against the seed list the way
-- setLandSubtypeEffects does rather than wiring it open.
abilityRemoval :: GameState -> ObjectId -> Bool
abilityRemoval gs =
  let gated = gatedGather gs
   in -- Almost every board has no ability-removing effect, and then no projection
      -- is spent on the question.
      if any (removesAbilities . gModification) gated
        then abilitiesRemoved gated gs
        else const False

-- gather's candidate list with CR 604.2's gate asked and CR 613.1f's layer-6 gate
-- left open -- what the two outside-the-fold removal readers below share. The
-- layer-6 gate stays open for abilityRemoval's stated reason; CR 305.7's
-- post-layer-4 gate stays open beside it, this list being what such a gate would
-- have to project against. The CR 604.2 gate is answered against the seed list,
-- well-founded for gather's reason, since the seed is built with every gate open.
gatedGather :: GameState -> [Gathered]
gatedGather gs =
  let ungated = gatherGiven (const False) alwaysFunctioning Nothing gs
   in if anyConditional gs
        then gatherGiven (const False) (conditionHolds ungated gs) Nothing gs
        else ungated

-- abilityRemoval asked AT A TIMESTAMP: "were this object's abilities removed by a
-- removal applied AFTER `ts`?" -- CR 613.1f's question ordered by CR 613.7. A
-- layer-6 grant is wiped only by a removal with a LATER timestamp than its own;
-- one with an earlier timestamp has already applied by the time the grant lands,
-- and the grant survives it (CR 613.9's first Example). For the reader holding a
-- layer-6 grant OUTSIDE the layer fold: rule 701.60c's "this creature can't
-- block" in Pawl.Engine.CombatRestriction.
--
-- STRICTLY after. Two objects never share a timestamp, so CR 613.7m's APNAP
-- tie-break has nothing to decide here.
--
-- A REGRESSION FENCE rather than proved behaviour: no board in the pool arranges
-- a Designation.Suspected permanent holding a layer-6 grant with a later
-- timestamp than the removal on top of a conditional remover, so mutating this
-- gate away leaves the suite green -- the sibling above is where the same change
-- is proved.
abilityRemovalAfter :: GameState -> Timestamp -> ObjectId -> Bool
abilityRemovalAfter gs =
  let gated = gatedGather gs
   in if any (removesAbilities . gModification) gated
        then \ts -> abilitiesRemovedBy ((> ts) . gTimestamp) gated gs
        else \_ _ -> False

-- CR 613.1f: does this modification remove abilities? Total: a new
-- ability-removing Modification must break this build rather than silently answer
-- False.
removesAbilities :: Modification -> Bool
removesAbilities m = case m of
  Modification.LoseAllAbilities -> True
  -- CR 613.1f: a removal that names one ability is a removal all the same, so CR
  -- 613.9's dependency between a grant and a wipe sees it too.
  Modification.LoseNamedAbility _ -> True
  Modification.GainKeyword _ -> False
  -- A grant, the other direction of CR 613.1f, exactly as GainKeyword above and
  -- GainAbility below.
  Modification.GainEnchant _ -> False
  -- The other direction of CR 613.1f: a grant is not a removal, so timestamp
  -- order alone decides whether a granted ability survives Humility. Proven
  -- through the FOLD by Pawl.ActivateSpec's "Presence of Gond" pair; this arm's
  -- own answer is a regression fence -- flipping it to True leaves the suite
  -- green.
  Modification.GainAbility _ -> False
  -- CR 305.7 strips a land's rules text, but as a layer-4 type change performed
  -- by setLandSubtypeTo and the two gates beside it, never a layer-6 removal.
  -- setsLandSubtype is the classification; this one answers CR 613.1f.
  Modification.SetLandSubtype _ -> False
  Modification.SetLandSubtypeToChosen -> False
  Modification.SetCreatureSubtype _ -> False
  Modification.AddCreatureSubtype _ -> False
  Modification.AddEveryCreatureSubtype -> False
  Modification.AddSubtype _ -> False
  Modification.SetBasePowerToughness {} -> False
  Modification.ModifyPowerToughness {} -> False
  Modification.SwitchPowerToughness -> False
  Modification.AddLandSubtype _ -> False
  Modification.ChangeSubtypeWord {} -> False
  Modification.AddCardType _ -> False
  -- CR 205.1a's card-type set has no ability clause: Song of the Dryads strips
  -- rules text through the SetLandSubtype it carries beside this.
  Modification.SetCardType _ -> False
  -- CR 205.4b changes a supertype and says nothing about abilities.
  Modification.AddSupertype _ -> False
  Modification.RemoveSupertype _ -> False
  Modification.SetColor _ -> False
  Modification.AddColor _ -> False
  Modification.AddChosenColor -> False
  Modification.SetController _ -> False
  Modification.SetControllerToSource -> False

-- CR 613.1f / 613.1g: were `oid`'s abilities removed by the time layer 6
-- finished, by any remover on the board?
abilitiesRemoved :: [Gathered] -> GameState -> ObjectId -> Bool
abilitiesRemoved = abilitiesRemovedBy (const True)

-- CR 613.1f / 613.1g: abilitiesRemoved, counting only the removers `keep`
-- admits. `keep` narrows the REMOVERS alone, never `cands`: that list is also
-- what the object is projected THROUGH, so filtering it would answer the
-- question against a board the game does not have.
--
-- CR 613.6's rescue falls out of reading the removers off the same candidate
-- list: an ability-removing effect is itself a layer-6 part, so an ability
-- carrying one is never gated by this.
--
-- Each remover's affected set is judged at CR 613.6's decision point (gLowest),
-- not at layer 6. For a MULTI-PART remover it is not re-derived: decisionsUpTo
-- hands back projectWith's own memo, so this gate and the fold cannot drift.
-- Single-part removers are not memoized and keep the projectUpTo reading.
--
-- Not implemented: a single-part remover whose affected set another effect in
-- its own decision layer moves (#1008).
--
-- Not asked of the remover's own source: order WITHIN layer 6 is CR 613.7
-- timestamp, settled by the fold. CR 305.7's gate asks a related question one
-- level up and settles it by CR 613.8 -- see appliedSetEffects.
abilitiesRemovedBy :: (Gathered -> Bool) -> [Gathered] -> GameState -> ObjectId -> Bool
abilitiesRemovedBy keep cands gs oid =
  let byLowest = Map.fromListWith (<>) [(gLowest c, [c]) | c <- cands, removesAbilities (gModification c), keep c]
      removesAt (lyr, cs) =
        let partial = projectUpTo lyr cands oid gs
            decided = decisionsUpTo lyr cands oid gs
            removes c = case gEffect c >>= (`Map.lookup` decided) of
              Just answer -> answer
              Nothing -> affectsGiven (viewUpTo lyr cands gs) (gSource c) oid (gAffected c) partial gs
         in any removes cs
   in any removesAt (Map.toList byLowest)

-- The two keywords rule 702 states as a characteristic-defining ability,
-- expanded for the case where CR 604.3a denies them that status: an instance
-- another object's static ability GRANTS is an ordinary continuous effect,
-- applied in timestamp order rather than at the start of its layer (CR 613.3).
--
--   * CR 702.114a, devoid: SetColor with no colours (CR 105.3), layer 5.
--   * CR 702.73a, changeling: AddEveryCreatureSubtype, layer 4.
--
-- Emitted as a second PART of the granting ability rather than an effect of its
-- own, per CR 613.6 and CR 613.7a: one affected set, one timestamp. Neither part
-- is the last word -- a newer colour- or type-changing effect lands on top of it
-- (CR 613.7).
--
-- Only a static ability's grant is expanded. A devoid granted by a resolution is
-- not (#793), nor a changeling so granted (#1288).
grantedDefiningParts :: Modification -> NonEmpty.NonEmpty Modification
grantedDefiningParts m = case m of
  Modification.GainKeyword Keyword.Type.Devoid -> m NonEmpty.:| [Modification.SetColor Set.empty]
  Modification.GainKeyword Keyword.Type.Changeling -> m NonEmpty.:| [Modification.AddEveryCreatureSubtype]
  _ -> m NonEmpty.:| []

-- One static ability's parts, ready to fold: CR 613.6's unit. (src, n) is the
-- key every part of a MULTI-part ability carries, so projectWith can tell that a
-- layer-4 part and a layer-7b part are one effect sharing one affected set. A
-- one-part ability carries no key.
--
-- CR 612: text changes affecting the SOURCE rewrite each part's subtype words
-- first.
--
-- `stripped` is CR 613.1f's answer for the source. It costs an ability all of
-- its parts, and only when every one applies AFTER layer 6 -- an ability with a
-- part in layers 1-6 had already started to apply, which is CR 613.6's rescue.
--
-- `functioning` is CR 604.2's "as long as" gate, answered by conditionHolds at
-- the ability's lowest layer, and costs the ability all its parts
-- unconditionally: a clause that is false never let the effect start to apply.
gatherStatic :: (Layer -> Condition.Type.Condition -> Bool) -> ObjectId -> Timestamp -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> (Layer -> Bool) -> Natural -> StaticAbility.StaticAbility Card.Type.Card -> [Gathered]
gatherStatic functioning src ts changes removed n sa =
  let ms = staticParts changes sa
      key = case ms of
        _ NonEmpty.:| (_ : _) -> Just (src, n)
        _ -> Nothing
      -- CR 613.6's decision point, computed once for the whole ability and
      -- copied onto each of its parts.
      lowest = minimum (fmap layer ms)
      -- CR 612.1: rewritten because the affected clause is rules text too.
      -- Short-circuited, since Filter.rewrite walks the whole tree even for an
      -- empty pair list and the SBA sweep reruns gather at every priority.
      affected =
        if null changes
          then StaticAbility.affected sa
          else rewriteAffected changes (StaticAbility.affected sa)
      one m' =
        MkGathered
          { gEffect = key,
            gSource = src,
            gAffected = affected,
            gLayer = layer m',
            gLowest = lowest,
            gTimestamp = ts,
            gModification = m'
          }
      parts = fmap one (NonEmpty.toList ms)
      -- CR 604.2's clause, shared with setLandSubtypeEffects -- see staticLives.
      lives = staticLives functioning changes lowest sa
   in -- Cheap test first: `removed`'s projection is forced only if it matters.
      if not lives || removed lowest then [] else parts

-- The parts one printed static ability contributes. CR 612 rewrites each printed
-- modification first, since grantedDefiningParts then emits engine-minted parts
-- that are not card text for CR 612 to reach.
staticParts :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> StaticAbility.StaticAbility Card.Type.Card -> NonEmpty.NonEmpty Modification
staticParts changes sa = StaticAbility.modifications sa >>= grantedDefiningParts . rewriteModification changes

-- CR 604.2's "as long as" gate for ONE printed static ability, asked at CR
-- 613.6's decision point -- the minimum layer over its parts. Shared so
-- gatherStatic and setLandSubtypeEffects agree. CR 612.1: the clause is printed
-- text like the affected clause beside it, so the same word swap reaches it.
staticLives :: (Layer -> Condition.Type.Condition -> Bool) -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Layer -> StaticAbility.StaticAbility Card.Type.Card -> Bool
staticLives functioning changes lowest sa =
  maybe True (\c -> functioning lowest (if null changes then c else rewriteCondition changes c)) (StaticAbility.condition sa)

-- CR 122.1a / 613.4c: +1/+1 and -1/-1 counters modify P/T in layer 7c, as one
-- synthetic ModifyPowerToughness per KIND. CR 122.1b / 613.1f: a keyword counter
-- grants its keyword in layer 6, one grant per counter, since that layer counts
-- instances.
--
-- CR 613.7c: each is stamped from Object.counterTimestamps, the moment its kind
-- was put on, which is also why the two P\/T kinds emit separately rather than
-- as one net delta.
counterGathered :: GameState -> [Gathered]
counterGathered gs = concatMap fromObject (Set.toList (GameState.battlefield gs))
  where
    fromObject oid = case Game.lookupObject oid gs of
      Nothing -> []
      Just obj ->
        let cs = Object.counters obj
            at kind lyr m =
              MkGathered
                { gEffect = Nothing,
                  gSource = oid,
                  gAffected = Affected.TheseObjects (Set.singleton oid),
                  gLayer = lyr,
                  gLowest = lyr,
                  gTimestamp = Map.findWithDefault (Object.timestamp obj) kind (Object.counterTimestamps obj),
                  gModification = m
                }
            deltaOf kind sign =
              [ at kind Layer.ModifyPT (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal d) (Quantity.Type.Literal d)))
              | let d = sign * toInteger (Map.findWithDefault 0 kind cs),
                d /= 0
              ]
            pt = deltaOf CounterKind.PlusOnePlusOne 1 <> deltaOf CounterKind.MinusOneMinusOne (-1)
            grantOf (kind, n) = case kind of
              CounterKind.Keyword kw -> List.genericReplicate n (at kind Layer.Ability (Modification.GainKeyword kw))
              CounterKind.PlusOnePlusOne -> []
              CounterKind.MinusOneMinusOne -> []
              -- CR 122.1e: a loyalty counter grants nothing, and no CR 613 layer
              -- reads loyalty.
              CounterKind.Loyalty -> []
              -- CR 714.3: nor a lore counter.
              CounterKind.Lore -> []
              -- Nor a defense counter: no CR 613 layer reads defense either.
              CounterKind.Defense -> []
              -- Nor a time counter (CR 702.63a).
              CounterKind.Time -> []
              -- Nor a fade counter (CR 702.32a).
              CounterKind.Fade -> []
              -- CR 122.1c: a shield counter is not a keyword counter, so it must
              -- make no grant here. shieldOf mints its effects instead.
              CounterKind.Shield -> []
              -- Nor a level counter: CR 711.2a states a leveler's grants as
              -- static abilities of the card, which gatherStatic applies.
              CounterKind.Level -> []
              -- Nor a card-named counter: CR 122.1 letters no such kind, so no
              -- CR 613 layer reads one, and what reads the count is always the
              -- card's own condition (Quantity.ObjectCounters) rather than this
              -- fold. UNPROVEN by any board -- a grant that names nothing and no
              -- grant at all are indistinguishable, so there is no assertion to
              -- write here rather than one nobody wrote.
              CounterKind.Named _ -> []
         in pt <> concatMap grantOf (Map.toList cs)

-- CR 701.60c / 613.1f: a SUSPECTED permanent has menace, emitted as a layer-6
-- grant on the permanent itself. Read off Object.designations on every
-- projection rather than stamped when the designation is set, which IS "for as
-- long as it's suspected". The rule's other half, "can't block", is a combat
-- restriction and lives in Pawl.Engine.CombatRestriction, reading the SAME
-- timestamp through abilityRemovalAfter so one sentence gets one order.
designationGathered :: GameState -> [Gathered]
designationGathered gs = concatMap fromObject (Set.toList (GameState.battlefield gs))
  where
    fromObject oid = case Game.lookupObject oid gs of
      Nothing -> []
      Just obj
        | Set.member Designation.Suspected (Object.designations obj) ->
            [ MkGathered
                { gEffect = Nothing,
                  gSource = oid,
                  gAffected = Affected.TheseObjects (Set.singleton oid),
                  gLayer = Layer.Ability,
                  gLowest = Layer.Ability,
                  gTimestamp = Object.timestamp obj,
                  gModification = Modification.GainKeyword Keyword.Type.Menace
                }
            ]
        | otherwise -> []

-- A characteristic a projection holds, at the coarseness CR 613.8a's dependency
-- question needs: applying one effect can only change what another applies to if
-- it WRITES something that one READS.
--
-- NECESSARY and not sufficient, deliberately. `movesSet` screens on an aspect
-- overlap and then CONFIRMS by applying the other effect and re-asking the
-- affected set (`changesAt`), so an over-declared read costs that confirmation
-- and never a different order; `movableLayers` and `countingLayers` over-admit
-- for the same reason. Under-declaring is the defect this type can carry, which
-- is why `filterReads` and `modificationWrites` are exhaustive.
--
-- Names an aspect of ONE object's projection, with no way to say WHOSE, and that
-- is not a shortfall: `unitWrites` names no object either, and the per-object
-- question is exactly what `changesAt` answers. So a cross-object atom declares
-- the aspects it reads off ANOTHER object as if they were the candidate's, and
-- an aspect qualified by WHOSE would still have to match a plain write of the
-- same aspect -- an identical order for a larger type; see #357.
data Aspect
  = Types
  | Subtypes
  | -- CR 205.4 / 613.1d: layer 4 writes supertypes too, and CR 205.4b keeps them
    -- independent of card types.
    Supertypes
  | Colors
  | -- CR 109.3 / 613.1f: abilities are characteristics, so a keyword is an
    -- aspect exactly as a subtype is.
    Keywords
  | PowerA
  | Controller
  deriving (Eq, Ord)

-- Which aspects a Filter reads. Exhaustive on purpose: a new Filter arm reading
-- a projected characteristic must be classified here, or CR 613.8a would
-- silently stop seeing dependencies through it.
--
-- IsAttacking reads nothing, which CR 506.4 makes look otherwise: pawl stores
-- attacking as a combat record (CR 109.3), and every writer of that record runs
-- BETWEEN projections, so it is a fixed input to any single projection. What
-- that costs is timing, not dependency.
filterReads :: Filter.Type.Filter Keyword.Type.Keyword -> Set Aspect
filterReads f = case f of
  Filter.Type.HasCardType _ -> Set.singleton Types
  Filter.Type.HasSupertype _ -> Set.singleton Supertypes
  Filter.Type.HasColor _ -> Set.singleton Colors
  Filter.Type.HasSubtype _ -> Set.singleton Subtypes
  -- Reads no aspect: no Modification writes CR 201.1's names.
  Filter.Type.HasName _ -> Set.empty
  -- CR 613.1f: layer 6 adds and removes abilities.
  Filter.Type.HasKeyword _ -> Set.singleton Keywords
  -- The same aspect, read one step coarser.
  Filter.Type.HasKeywordFamily _ -> Set.singleton Keywords
  Filter.Type.PowerAtLeast _ -> Set.singleton PowerA
  Filter.Type.PowerAtMost _ -> Set.singleton PowerA
  -- The same aspect, covering BOTH powers the atom compares: Aspect names an
  -- aspect of one object's projection, with no way to say "the source's".
  Filter.Type.PowerLessThanSource -> Set.singleton PowerA
  Filter.Type.PowerGreaterThanSource -> Set.singleton PowerA
  Filter.Type.ControlledBy _ -> Set.singleton Controller
  -- The candidate's controller; who defends is a combat-record fact.
  Filter.Type.ControlledByDefendingPlayer -> Set.singleton Controller
  Filter.Type.ControlledByBound _ -> Set.singleton Controller
  Filter.Type.ControlledByPlayer _ -> Set.singleton Controller
  Filter.Type.ControlledByRecipient -> Set.singleton Controller
  -- Reads nothing, where its sibling above reads Controller: CR 108.3 lets no
  -- rule change an owner, so no Modification writes Object.owner.
  Filter.Type.OwnedBy _ -> Set.empty
  Filter.Type.IsSource -> Set.empty
  -- Reads an IDENTITY, which CR 109.3 does not count as a characteristic.
  Filter.Type.IsBound _ -> Set.empty
  -- Reads NAMES at both ends, which no Modification writes.
  Filter.Type.SameNameAsBound _ -> Set.empty
  Filter.Type.IsPlayer _ -> Set.empty
  -- Reads a CONTROLLER rather than a characteristic (CR 109.3 lists none).
  Filter.Type.IsControllerOfBound _ -> Set.empty
  -- Over-declared deliberately, as CanHostSubject is: the atom reads every
  -- permanent's controller plus whatever its nest reads of each.
  Filter.Type.ControlsMoreThanYou g -> Set.insert Controller (filterReads g)
  -- Reads a ZONE's size, which is no object's characteristic (CR 109.3) and no
  -- Modification writes; every zone change happens between projections.
  Filter.Type.CardsInGraveyardAtLeast _ -> Set.empty
  Filter.Type.IsAttacking -> Set.empty
  -- Reads nothing, for IsAttacking's reason and off the same record.
  Filter.Type.IsBlocking -> Set.empty
  Filter.Type.IsBlocked -> Set.empty
  -- Reads nothing: no Modification writes GameState.events.
  Filter.Type.AttackedThisTurn -> Set.empty
  Filter.Type.MilledThisTurn -> Set.empty
  Filter.Type.DealtDamageThisTurn -> Set.empty
  -- The nest's own reads, declared as if they were the CANDIDATE's even though
  -- they are the HOST's -- exactly right rather than merely safe, for the reason
  -- the note on Aspect above gives. The attachment itself reads nothing, for
  -- IsAttacking's reason: CR 109.3 keeps what an Aura enchants off the
  -- characteristics, so no Modification writes Object.attachedTo, and CR 303.4's
  -- attaching runs between projections as CR 110.1's zone change does.
  Filter.Type.AttachedTo g -> filterReads g
  -- The nest's own reads again, declared as if they were the CANDIDATE's even
  -- though they are an ATTACHER's -- exactly right rather than merely safe, for
  -- the atom above's reason. The attachment itself reads nothing, for that atom's
  -- reason too: no Modification writes Object.attachedTo.
  Filter.Type.HasAttached g -> filterReads g
  Filter.Type.IsAttachedToSource -> Set.empty
  Filter.Type.IsHostOfSource -> Set.empty
  -- Over-declared deliberately, per the note on Aspect above: the characteristics
  -- behind this atom are the candidate's (CR 301.5) and the subject's (CR
  -- 702.5a), and nothing distinguishes the two here.
  Filter.Type.CanHostSubject -> Set.fromList [Types, Subtypes, Colors, Keywords, PowerA, Controller]
  -- Over-declared for CanHostSubject's reason, one direction over: the
  -- characteristics behind this atom are the CANDIDATE's -- its subtypes (CR
  -- 303.4, CR 301.5) and its enchant ability (CR 702.5a) -- and the fixed host's,
  -- which that ability is read against, and nothing here distinguishes the two
  -- either.
  Filter.Type.CanAttachToSubject -> Set.fromList [Types, Subtypes, Colors, Keywords, PowerA, Controller]
  -- Reads nothing: no Modification writes Object.source.
  Filter.Type.IsToken -> Set.empty
  -- CR 110.5: tap status is not a characteristic, so no layer writes it.
  Filter.Type.IsTapped -> Set.empty
  -- Reads nothing: CR 712.8d/e make which face is up the thing characteristics
  -- are read OFF rather than one of them, so no Modification writes Object.face.
  Filter.Type.Transformed -> Set.empty
  -- CR 109.3 / 613.1f: the aspect LoseAllAbilities writes, Aspect having no
  -- finer grain than "the abilities".
  Filter.Type.HasNonManaActivatedAbility -> Set.singleton Keywords
  -- Reads nothing: CR 701.54b keeps the ring-bearer designation off the copiable
  -- values, so no Modification writes Object.ringBearerFor.
  Filter.Type.IsRingBearer -> Set.empty
  -- Reads nothing, for that atom's reason. What CR 701.60c hangs off `Suspected`
  -- IS in layer 6, but that is what the designation WRITES, not what this reads.
  Filter.Type.HasDesignation _ -> Set.empty
  -- Reads nothing: CR 109.3's characteristics do not include counters. The P/T a
  -- +1/+1 counter grants is CR 613.4c's, applied over the top of the set.
  Filter.Type.HasCounters _ -> Set.empty
  -- CR 202.3 reads the printed mana cost, which no Modification writes.
  Filter.Type.ManaValueAtMost _ -> Set.empty
  Filter.Type.ManaValueIsEven -> Set.empty
  Filter.Type.And fs -> foldMap filterReads fs
  Filter.Type.Or fs -> foldMap filterReads fs
  Filter.Type.Not g -> filterReads g

-- Which aspects a Modification writes -- the other half of the pair above.
--
-- The arms that write Keywords: GainKeyword, GainEnchant and LoseAllAbilities per
-- CR 613.1f, both subtype-setting arms per CR 305.7, and ChangeSubtypeWord per CR
-- 612.1, a text change reaching the land type inside a landwalk keyword.
--
-- ChangeSubtypeWord also rewrites PC.characteristicPT, deliberately not PowerA:
-- this asks what a modification writes IN ITS OWN LAYER, and the rewritten CDA
-- is still unevaluated there (CR 613.8a clause (c)).
modificationWrites :: Modification -> Set Aspect
modificationWrites m = case m of
  Modification.GainKeyword _ -> Set.singleton Keywords
  -- Writes ProjectedCharacteristics.enchant, which Aspect has no finer grain for
  -- than Keywords -- CR 702.5a's enchant is an ability, and
  -- Filter.CanHostSubject, the atom that reads it, declares Keywords. A
  -- REGRESSION FENCE rather than a proved behaviour: no board in the pool makes
  -- another effect's affected set depend on a granted enchant, so Set.empty here
  -- leaves the suite green.
  Modification.GainEnchant _ -> Set.singleton Keywords
  -- Writes ProjectedCharacteristics.activatedAbilities or .triggeredAbilities,
  -- neither of which any Filter atom reads. LoseAllAbilities declares Keywords
  -- because it also empties the map.
  Modification.GainAbility _ -> Set.empty
  Modification.LoseAllAbilities -> Set.singleton Keywords
  -- Writes ProjectedCharacteristics.activatedAbilities, which Aspect has no finer
  -- grain for than Keywords -- Filter.HasNonManaActivatedAbility, the atom that
  -- reads it, declares Keywords. A REGRESSION FENCE rather than a proved
  -- behaviour: no board in the pool makes another effect's affected set depend on
  -- a named removal, so Set.empty here leaves the suite green.
  Modification.LoseNamedAbility _ -> Set.singleton Keywords
  Modification.SetBasePowerToughness {} -> Set.singleton PowerA
  Modification.ModifyPowerToughness {} -> Set.singleton PowerA
  Modification.SwitchPowerToughness -> Set.singleton PowerA
  Modification.SetLandSubtype _ -> Set.fromList [Subtypes, Keywords]
  Modification.SetLandSubtypeToChosen -> Set.fromList [Subtypes, Keywords]
  Modification.AddLandSubtype _ -> Set.singleton Subtypes
  Modification.SetCreatureSubtype _ -> Set.singleton Subtypes
  Modification.AddCreatureSubtype _ -> Set.singleton Subtypes
  Modification.AddEveryCreatureSubtype -> Set.singleton Subtypes
  -- The honest answer -- the arm writes PC.subtypes and nothing else -- but a
  -- regression fence rather than a proved behaviour: no board in the pool makes
  -- another effect depend on the one AddSubtype, so Set.empty here leaves the
  -- suite green too.
  Modification.AddSubtype _ -> Set.singleton Subtypes
  Modification.ChangeSubtypeWord {} -> Set.fromList [Subtypes, Keywords]
  Modification.AddCardType _ -> Set.singleton Types
  -- CR 205.1a's set writes BOTH: the card types it replaces, and the subtypes it
  -- strips along with the types that carried them.
  Modification.SetCardType _ -> Set.fromList [Types, Subtypes]
  Modification.AddSupertype _ -> Set.singleton Supertypes
  Modification.RemoveSupertype _ -> Set.singleton Supertypes
  Modification.SetColor _ -> Set.singleton Colors
  Modification.AddColor _ -> Set.singleton Colors
  Modification.AddChosenColor -> Set.singleton Colors
  Modification.SetController _ -> Set.singleton Controller
  Modification.SetControllerToSource -> Set.singleton Controller

-- Which aspects a Modification's own QUANTITIES read -- CR 613.8a clause (b)'s
-- last limb: applying another effect can change "what it does to any of the
-- things it applies to" without touching what it applies to. Only the two
-- power/toughness arms carry a quantity, so a new arm carrying one has to be
-- classified here, or the dependency would silently stop being seen.
modificationReads :: Modification -> Set Aspect
modificationReads m = case m of
  Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) -> quantityReads p <> quantityReads t
  Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) -> quantityReads p <> quantityReads t
  Modification.GainKeyword _ -> Set.empty
  -- Carries no Quantity of its own; its Filter is read where the slot is matched.
  Modification.GainEnchant _ -> Set.empty
  -- A quoted ability's quantities are read at ITS resolution.
  Modification.GainAbility _ -> Set.empty
  Modification.LoseAllAbilities -> Set.empty
  -- Carries a name, which is not a Quantity.
  Modification.LoseNamedAbility _ -> Set.empty
  Modification.SwitchPowerToughness -> Set.empty
  Modification.SetLandSubtype _ -> Set.empty
  Modification.SetLandSubtypeToChosen -> Set.empty
  Modification.AddLandSubtype _ -> Set.empty
  Modification.SetCreatureSubtype _ -> Set.empty
  Modification.AddCreatureSubtype _ -> Set.empty
  Modification.AddEveryCreatureSubtype -> Set.empty
  Modification.AddSubtype _ -> Set.empty
  Modification.ChangeSubtypeWord {} -> Set.empty
  Modification.AddCardType _ -> Set.empty
  Modification.SetCardType _ -> Set.empty
  Modification.AddSupertype _ -> Set.empty
  Modification.RemoveSupertype _ -> Set.empty
  Modification.SetColor _ -> Set.empty
  Modification.AddColor _ -> Set.empty
  Modification.AddChosenColor -> Set.empty
  Modification.SetController _ -> Set.empty
  Modification.SetControllerToSource -> Set.empty

-- Which aspects evaluating a Quantity reads off a projection -- filterReads for
-- the OTHER half of a P/T modification, and exhaustive for the same reason.
-- Everything not listed reads nothing a Modification writes.
--
-- Over-declaring is the safe direction: this is only a SCREEN, and the
-- dependency itself is settled by re-evaluating the modification (see
-- projectDeciding's resolve).
quantityReads :: Quantity.Type.Quantity -> Set Aspect
quantityReads q = case q of
  Quantity.Type.Count c -> filterReads (Count.Type.filter c) <> aggregationReads (Count.Type.aggregation c)
  Quantity.Type.Power -> Set.singleton PowerA
  Quantity.Type.Toughness -> Set.singleton PowerA
  Quantity.Type.Plus (Plus.MkPlus a b) -> quantityReads a <> quantityReads b
  Quantity.Type.Halved (Halved.MkHalved _ a) -> quantityReads a
  Quantity.Type.Negate a -> quantityReads a
  Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot _ a) -> quantityReads a
  Quantity.Type.Literal _ -> Set.empty
  Quantity.Type.ManaValue -> Set.empty
  Quantity.Type.InSlot _ -> Set.empty
  Quantity.Type.Star -> Set.empty
  Quantity.Type.ManaCount _ -> Set.empty
  Quantity.Type.LifeTotal _ -> Set.empty
  Quantity.Type.Speed _ -> Set.empty
  Quantity.Type.IsMonarch _ -> Set.empty
  Quantity.Type.IsStartingPlayer _ -> Set.empty
  Quantity.Type.IsActivePlayer _ -> Set.empty
  Quantity.Type.PlayerCounters {} -> Set.empty
  Quantity.Type.ObjectCounters _ -> Set.empty
  Quantity.Type.HasDesignation _ -> Set.empty
  Quantity.Type.ClassLevel -> Set.empty
  Quantity.Type.WasKicked -> Set.empty
  Quantity.Type.SnowWasSpent -> Set.empty
  Quantity.Type.OpponentsAttacked _ -> Set.empty
  Quantity.Type.CardsDiscardedThisTurn _ -> Set.empty
  Quantity.Type.PlayersDealtDamageThisTurn _ -> Set.empty
  Quantity.Type.EnteredThisTurn -> Set.empty
  Quantity.Type.BlockersBeyondFirst -> Set.empty

-- What an Aggregation reads off each member the Filter kept. DistinctCardTypes
-- is CR 208.2a's fold over card types (layer 4); Greatest reads its Quantity's.
aggregationReads :: Aggregation.Aggregation Quantity.Type.Quantity -> Set Aspect
aggregationReads a = case a of
  Aggregation.Members -> Set.empty
  Aggregation.DistinctCardTypes -> Set.singleton Types
  Aggregation.Greatest q -> quantityReads q

-- What could move `c`'s affected set -- Nothing when nothing can. A TheseObjects
-- set names ids (CR 611.2c) and an Attached one reads its source's attachment
-- (CR 303.4m), neither of which a modification writes; and a filter reading no
-- projected aspect has nothing to change. CR 613.6's memo is a third, per object
-- and so kept in the fold.
movableAspects :: Gathered -> Maybe (Set Aspect)
movableAspects c =
  let readsOf f = let aspects = filterReads f in if Set.null aspects then Nothing else Just aspects
   in case gAffected c of
        Affected.TheseObjects _ -> Nothing
        Affected.Attached -> Nothing
        Affected.Matching f -> readsOf f
        Affected.MatchingAnywhere f -> readsOf f
        Affected.MatchingOffBattlefield f -> readsOf f
        -- Always movable, whatever the filter reads: the set is narrowed by WHO
        -- CONTROLS each candidate, and layer 2 writes Controller (CR 613.1b).
        Affected.AttachedPlayerControls f -> Just (Set.insert Controller (filterReads f))

-- Could another effect move this one's affected set at all? movableAspects above
-- with the aspects thrown away and an empty filter still counted movable.
staticallyMovable :: Gathered -> Bool
staticallyMovable c = case gAffected c of
  Affected.Matching _ -> True
  Affected.MatchingAnywhere _ -> True
  Affected.MatchingOffBattlefield _ -> True
  Affected.TheseObjects _ -> False
  Affected.Attached -> False
  -- Movable, unlike Attached: WHO CONTROLS a candidate is a layer-2 effect's
  -- business (CR 613.1b).
  Affected.AttachedPlayerControls _ -> True

-- The aspects one effect's parts at a layer write, and the aspects their
-- quantities read. Both halves of CR 613.8a's clause (b) are asked of the whole
-- unit, that being CR 613.8's own unit -- see effectUnits below.
unitWrites :: NonEmpty.NonEmpty Gathered -> Set Aspect
unitWrites = foldMap (modificationWrites . gModification)

unitReads :: NonEmpty.NonEmpty Gathered -> Set Aspect
unitReads = foldMap (modificationReads . gModification)

-- CR 613.8's unit is an EFFECT, not a modification, and CR 613.6 calls one
-- ability's modifications the parts of that effect. gatherStatic emits one
-- ability's parts contiguously and keys them alike, and filtering by layer
-- preserves that order, so adjacency finds a unit. A Nothing key is always a
-- unit of one.
effectUnits :: [Gathered] -> [NonEmpty.NonEmpty Gathered]
effectUnits =
  let sameEffect a b = case (gEffect a, gEffect b) of
        (Just x, Just y) -> x == y
        _ -> False
   in NonEmpty.groupBy sameEffect

-- CR 613: apply continuous effects layer by layer, ascending. Within a layer, CR
-- 613.8's dependency ordering falling back to CR 613.7 timestamp order. CR
-- 613.8's EXISTENCE dependency is the exception, handled by source-liveness
-- rather than the reorder. design.md section 2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs = projectFrom (gather gs) oid gs

-- CR 613.4a layer 7a: apply the object's own characteristic-defining P/T
-- ability, read from the PARTIAL projection (post-layer-6) so LoseAllAbilities
-- can strip it first, and evaluated against the current state.
--
-- Folded in place rather than emitted as a synthetic Gathered: gather runs
-- BEFORE the fold and has no partial to read; CR 604.3 / 208.2a make a CDA
-- function in all zones while gather walks the battlefield only; a CDA has no
-- source and no timestamp to sort on under CR 613.7; and this one is DYNAMIC,
-- CR 707.2 making a copy recompute from the printed text rather than freeze a
-- number into Binding.copy at entry.
--
-- Quantity.determine rather than setPT, because CR 208.2a makes a CDA always
-- produce a number: a creature whose CDA cannot be determined is a 0/0 that CR
-- 704.5f buries.
--
-- CR 604.3a(3): a CDA affects no other object, so the Filter.Context is the
-- object's OWN controller, not the source's (CR 109.5).
--
-- Not implemented: CR 613.8a's clause (c) lets two CDAs depend on each other, so
-- a CDA counting a characteristic another CDA defines sees the printed value
-- instead (#1332).
applyCharacteristicPT :: Count.ViewOf -> GameState -> ObjectId -> ProjectedCharacteristics -> ProjectedCharacteristics
applyCharacteristicPT viewOf gs oid pc = case PC.characteristicPT pc of
  Nothing -> pc
  Just cda ->
    let context = Filter.contextFor (controllerOf oid gs) (Just oid)
     in pc
          { PC.power = Just (Quantity.determine viewOf context gs oid (CharacteristicPT.power cda)),
            PC.toughness = Just (Quantity.determine viewOf context gs oid (CharacteristicPT.toughness cda))
          }

-- projectDeciding with the decision memo dropped. `admits` is bound before `oid`
-- so the candidate-only work inside the fold is shared across a board sweep.
projectWith :: (Layer -> Bool) -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectWith admits cands =
  let forObject = projectDeciding admits cands
   in \oid gs -> fst (forObject oid gs)

-- Project one object against a PRECOMBINED candidate list, applying only the
-- layers the predicate admits. CR 613.1 applies layers in order and Layer's
-- derived Ord IS that order, so `(< bound)` is exactly the layers before this
-- one.
--
-- The bound exists for counting: a Count evaluated while layer L is being
-- applied sees `< L`, so one encountered inside that fold sees `< K` for some
-- K < L. The bound strictly decreases and Layer is finite, so the nesting
-- terminates. Where a layer holds a writer of an aspect something there counts,
-- CR 613.8a's clause (b) makes the two dependent and the layer goes through
-- `resolve`, which reads the running board instead of re-projecting.
--
-- Returns CR 613.6's decision memo beside the characteristics: it is settled in
-- here and cannot be re-derived from a layer-bounded view without contradicting
-- either CR 613.6 or CR 613.7/613.8.
projectDeciding :: (Layer -> Bool) -> [Gathered] -> ObjectId -> GameState -> (ProjectedCharacteristics, Map (ObjectId, Natural) Bool)
-- Candidates-in, then a worker taking the object: everything derived from the
-- candidate list alone is bound before `oid`, so projectAll shares it.
projectDeciding admits cands = forObject
  where
    -- Layers 4, 5 and 7a are always visited, even with no gathered effect there:
    -- an object's own CDAs are not gathered candidates.
    layers = filter admits (Set.toAscList (Set.insert Layer.Type (Set.insert Layer.Color (Set.insert Layer.CharacteristicPT (Set.fromList (fmap gLayer cands))))))
    -- The layers CR 613.8 could reorder anything in: those holding an effect
    -- whose affected set another can move, and those holding an effect whose
    -- MAGNITUDE another can move. Deliberately coarser than the fold's own test,
    -- so it over-admits -- costing the general path, never a different answer.
    movableLayers = Set.union (Set.fromList (fmap gLayer (filter staticallyMovable cands))) countingLayers
    -- CR 613.8a clause (b)'s "what it does to" limb, screened statically: a
    -- layer holding an effect whose quantities read an aspect that same layer
    -- writes. Same-layer is the whole of it -- a count reading only EARLIER
    -- layers is answered exactly by the bounded view. Over-admits harmlessly.
    countingLayers = Set.fromList (fmap gLayer (filter countsItsOwnLayer cands))
    writesByLayer = Map.fromListWith Set.union (fmap (\c -> (gLayer c, modificationWrites (gModification c))) cands)
    countsItsOwnLayer c = not (Set.disjoint (modificationReads (gModification c)) (Map.findWithDefault Set.empty (gLayer c) writesByLayer))
    forObject oid gs =
      let applyLayer (partial, decided) lyr =
            let -- What a Count sees when nothing at this layer can move it: the
                -- layers strictly below (CR 613.1).
                bounded = viewUpTo lyr cands gs
                -- CR 613.3: characteristic-defining abilities first, within the
                -- layer they define -- subtype 4, colour 5, P/T 7a. Taken per
                -- OBJECT, as the dependency scan seeds every snapshot the same.
                seedFor o p = case lyr of
                  Layer.Type -> applySubtypeDefining p
                  Layer.Color -> applyColorDefining p
                  Layer.CharacteristicPT -> applyCharacteristicPT bounded gs o p
                  _ -> p
                seeded = seedFor oid partial
                -- CR 613.6: the affected set is asked ONCE per effect, at the
                -- lowest layer it reaches, and remembered for its other layers.
                -- Object-parameterised, like applyUnit and applyOne below: the
                -- dependency scan asks all three about every object.
                appliesTo o ds pc c = case gEffect c of
                  Just k | Just answer <- Map.lookup k ds -> answer
                  _ -> affectsGiven bounded (gSource c) o (gAffected c) pc gs
                -- Fold every part of ONE effect landing in this layer, in the
                -- order the card lists them (CR 613.6). The ViewOf is the SAME
                -- for every object the effect reaches, so a count inside it
                -- cannot see the effect's own work on a sibling.
                applyUnit viewOf o pc cs = List.foldl' (\p c -> applyModification viewOf (gSource c) gs o (gModification c) p) pc (NonEmpty.toList cs)
                -- Apply one effect, recording its decision the first time.
                -- Re-inserting an existing key rewrites the value just read.
                applyOne viewOf o (pc, ds) cs =
                  let c = NonEmpty.head cs
                      answer = appliesTo o ds pc c
                      ds' = case gEffect c of
                        Nothing -> ds
                        Just k -> Map.insert k answer ds
                   in (if answer then applyUnit viewOf o pc cs else pc, ds')
                -- CR 613.6's per-object half of movableAspects: an effect whose
                -- set this object already settled cannot be moved at it.
                decidedAt ds c = case gEffect c of
                  Just k -> Map.member k ds
                  Nothing -> False
                -- Every OTHER battlefield object's state as this layer begins,
                -- so all of them derive the same order. Lazy, and scanned after
                -- the projected object. Terminates for projectUpTo's reason: a
                -- snapshot admits strictly fewer layers.
                --
                -- Not implemented: a snapshot carries CR 208.3's noncreature P/T
                -- gate, which the projected object's mid-fold partial does not
                -- (#1111); nor are objects outside the battlefield scanned, so a
                -- MatchingAnywhere dependency in another zone is missed (#1112).
                snapshot o =
                  let (p, d) = projectDeciding (\l -> admits l && l < lyr) cands o gs
                   in (seedFor o p, d)
                -- Keyed rather than an association list, because resolve's ViewOf
                -- looks an object up once per candidate a Count folds over.
                -- WHNF-strict only, so the snapshots stay lazy.
                otherBoards = Map.fromSet snapshot (Set.delete oid (GameState.battlefield gs))
                -- CR 613.8b: an effect that depends on another waits for it, and
                -- CR 613.7 timestamp order picks the next among those waiting on
                -- nothing. Re-deriving `ready` each round IS CR 613.8c, and
                -- removing one effect per pass makes it terminate. An empty
                -- `ready` means a dependency loop, which CR 613.8b applies in
                -- timestamp order. `pending` holds EFFECTS, not modifications.
                resolve (pc, ds) others pending = case pending of
                  [] -> (pc, ds)
                  _ ->
                    let -- One applicability answer per effect per round per
                        -- object, shared by the dependency scan -- this is the
                        -- hot loop. CR 613.6 makes the head part answer for the
                        -- unit.
                        answersAt o p d = fmap (\(i, cs) -> (i, appliesTo o d p (NonEmpty.head cs))) pending
                        answerFor ans i = Maybe.fromMaybe False (List.lookup i ans)
                        -- The board as it stands: every battlefield object plus
                        -- the projected one, which need not be there.
                        running = Map.insert oid (pc, ds) others
                        -- What a Count sees: CR 613 puts no bound on the state an
                        -- effect's magnitude is computed from, so a count reads
                        -- the layers below AND this layer's effects that have
                        -- already applied. No recursion, so nothing here has to
                        -- terminate. An object with no entry falls back to the
                        -- bounded view, and noncreaturePT is CR 208.3, applied so
                        -- the two agree. Another OBJECT is read off the running
                        -- board too (CR 701.3a / CR 613.1).
                        --
                        -- Not implemented: an object off the running board is
                        -- projected but not scanned here, so a count reading a
                        -- same-layer effect on one still gets the bound (#1332).
                        -- A Tale for the Ages DOES put CR 303.4b's attachment
                        -- atom in a CR 613.8-movable layer, its affected set
                        -- being Matching -- but the reader is still unobserved:
                        -- mutating it to `fullView` leaves the suite green,
                        -- because no board in the pool makes a bounded view and
                        -- a full one disagree here, and none loops. The CR
                        -- 702.178a gate is not reached at all (gap #1757).
                        viewOfBoard board o = case Map.lookup o board of
                          Just (p, _) -> Just (viewOfCharacteristics (viewOfBoard board) o (noncreaturePT o gs p) (controllerOf o gs) (countersOf o gs) gs)
                          Nothing -> bounded o
                        view = viewOfBoard running
                        -- Every object CR 613.8a's question ranges over, the
                        -- projected one first.
                        boards = (oid, pc, ds, answersAt oid pc ds) : fmap (\(o, (p, d)) -> (o, p, d, answersAt o p d)) (Map.toList others)
                        -- CR 613.8a clause (b)'s "what it applies to", at ONE
                        -- object: applying `b` there changes whether `a` applies.
                        -- The tentative application is thrown away. `b` is
                        -- applied WHOLE -- half an effect is not a state CR 613
                        -- describes.
                        changesAt (j, bs) (i, as) (o, p, d, ans) =
                          let a = NonEmpty.head as
                           in not (decidedAt d a)
                                && answerFor ans j
                                && appliesTo o d (applyUnit view o p bs) a /= answerFor ans i
                        -- `a` depends on `b` when that holds ANYWHERE: CR 613.8a
                        -- asks about the whole affected SET, which is also how CR
                        -- 613.8b's loop becomes visible. Clause (c)'s CDA
                        -- exclusion needs no test -- a CDA is never a candidate;
                        -- clause (b)'s "existence" half is liveGiven's.
                        movesSet x@(_, as) y@(_, bs) =
                          case movableAspects (NonEmpty.head as) of
                            Nothing -> False
                            Just aspects ->
                              not (Set.disjoint aspects (unitWrites bs))
                                && any (changesAt y x) boards
                        -- CR 613.8a clause (b)'s LAST limb: applying `b` changes
                        -- what `a` does to the things it applies to. Only a
                        -- magnitude can move, so what is compared is the P/T `a`
                        -- writes from the SAME base under two views, judged only
                        -- where `a` applies. No decidedAt gate, unlike changesAt:
                        -- a settled set says nothing about magnitude.
                        changesMagnitude (i, as) (_, bs) =
                          let after = appliedEverywhere bs
                              afterView = viewOfBoard after
                              writtenPT p = (PC.power p, PC.toughness p)
                           in any
                                ( \(o, p, _, ans) ->
                                    answerFor ans i
                                      && writtenPT (applyUnit view o p as) /= writtenPT (applyUnit afterView o p as)
                                )
                                boards
                        -- `b` applied to every object whose set holds it, judged
                        -- against the board as it stands (CR 613.6).
                        appliedEverywhere bs =
                          Map.mapWithKey
                            (\o (p, d) -> (if appliesTo o d p (NonEmpty.head bs) then applyUnit view o p bs else p, d))
                            running
                        dependsOnOne x@(i, as) y@(j, bs) =
                          j /= i
                            && ( movesSet x y
                                   || (not (Set.disjoint (unitReads as) (unitWrites bs)) && changesMagnitude x y)
                               )
                        ready = filter (\a -> not (any (dependsOnOne a) pending)) pending
                        -- The dependency edges, built only when the whole round
                        -- is blocked.
                        edges = Map.fromList (fmap (\a@(i, _) -> (i, fmap fst (filter (dependsOnOne a) pending))) pending)
                        reach seen queue = case queue of
                          [] -> seen
                          x : xs ->
                            if Set.member x seen
                              then reach seen xs
                              else reach (Set.insert x seen) (Map.findWithDefault [] x edges <> xs)
                        -- On a cycle iff it can reach itself in one step or more.
                        onCycle (i, _) = Set.member i (reach Set.empty (Map.findWithDefault [] i edges))
                        batch = case ready of
                          _ : _ -> ready
                          -- `ready` empty means every remaining effect has an
                          -- outgoing edge, so `cyclic` is never empty and this
                          -- fallback is unreachable; it keeps `minimumBy` total.
                          [] -> case filter onCycle pending of
                            [] -> pending
                            cyclic -> cyclic
                        -- CR 613.7a gives every part of one ability the source
                        -- permanent's timestamp.
                        (chosen, next) = List.minimumBy (Ord.comparing (\(_, cs) -> gTimestamp (NonEmpty.head cs))) batch
                     in resolve (applyOne view oid (pc, ds) next) (Map.mapWithKey (\o st -> applyOne view o st next) others) (filter ((/= chosen) . fst) pending)
                -- Is there anything at this layer CR 613.8 could reorder?
                movableHere = Set.member lyr movableLayers
                -- CR 613.6's memo, populated against `seeded` -- sound only on
                -- the branch below where nothing is movable.
                remember ds c = case gEffect c of
                  Nothing -> ds
                  Just k
                    | gLayer c /= lyr || Map.member k ds -> ds
                    | otherwise -> Map.insert k (affectsGiven bounded (gSource c) oid (gAffected c) seeded gs) ds
             in if movableHere
                  then resolve (seeded, decided) otherBoards (zip [0 :: Int ..] (effectUnits (filter (\c -> gLayer c == lyr) cands)))
                  else
                    -- Nothing here can be moved, so no candidate depends on any
                    -- other: CR 613.8 says nothing, CR 613.7 timestamp order
                    -- stands, and judging against `seeded` gives the same answers
                    -- as judging one at a time. Also almost every layer of almost
                    -- every projection, which is why the fold is tighter here.
                    let decided' = List.foldl' remember decided cands
                        applies c = case gEffect c of
                          Nothing -> affectsGiven bounded (gSource c) oid (gAffected c) seeded gs
                          Just k -> Map.findWithDefault False k decided'
                        ordered = List.sortOn gTimestamp (filter (\c -> gLayer c == lyr && applies c) cands)
                        step pc c = applyModification bounded (gSource c) gs oid (gModification c) pc
                     in (List.foldl' step seeded ordered, decided')
          (folded, decisions) = List.foldl' applyLayer (copiableCharacteristics oid gs, Map.empty) layers
       in (noncreaturePT oid gs folded, decisions)

-- CR 208.3: a noncreature permanent has no power or toughness, and only on the
-- battlefield -- a Vehicle in a graveyard keeps its printed numbers. Applied to
-- the FINISHED fold so an uncrewed Consulate Dreadnought reports no power and a
-- crewed one 7 (CR 301.7b), and so a stored +1/+1 on an uncrewed Vehicle starts
-- counting the moment layer 4 makes it a creature (CR 208.3a).
--
-- Applied at the END of projectDeciding: moving it out eta-expands away the
-- candidate-only sharing projectAll rests on, and costs 2x on the aura benchmarks.
noncreaturePT :: ObjectId -> GameState -> ProjectedCharacteristics -> ProjectedCharacteristics
noncreaturePT oid gs pc
  | Set.member CardType.Creature (PC.cardTypes pc) = pc
  | not (Set.member oid (GameState.battlefield gs)) = pc
  | otherwise = pc {PC.power = Nothing, PC.toughness = Nothing}

-- CR 208.5: a creature with no value for its power has 0, likewise toughness --
-- reached by a card that strips a characteristic-defining ability (CR 305.7).
-- Guarded on CREATURE, and applied after noncreaturePT: CR 208.3 decides whether
-- this rule's premise is reached at all, so an uncrewed Consulate Dreadnought
-- keeps the Nothing it just got (CR 301.7a).
noValuePT :: ProjectedCharacteristics -> ProjectedCharacteristics
noValuePT pc
  | not (Set.member CardType.Creature (PC.cardTypes pc)) = pc
  | otherwise =
      pc
        { PC.power = Just (Maybe.fromMaybe 0 (PC.power pc)),
          PC.toughness = Just (Maybe.fromMaybe 0 (PC.toughness pc))
        }

-- Project one object against a PRECOMPUTED candidate list. gather is
-- oid-independent, so a whole-board sweep gathers once and folds each object
-- (projectAll) instead of re-gathering per object. Where CR 208.5 goes, and
-- deliberately NOT inside projectWith: "has no value" cannot be asked of a
-- layer-bounded mid-fold view.
--
-- Not implemented: CR 208.5's substitution is likewise absent from the view a
-- Count reads mid-layer (#1332).
projectFrom :: [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectFrom cands oid gs = noValuePT (projectWith (const True) cands oid gs)

-- CR 613.1: a projection bounded to the layers BEFORE `bound` -- what a Count
-- sees while layer `bound` is being applied.
projectUpTo :: Layer -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectUpTo bound = projectWith (< bound)

-- CR 613.6's answers, as the fold reached them, for every multi-part effect
-- whose lowest layer is at or below `bound`: keyed by gEffect, True when the
-- effect's affected set held `oid` at the layer it started to apply.
--
-- Bounded INCLUSIVELY, unlike projectUpTo: an effect deciding at `bound` decides
-- against its same-layer predecessors (CR 613.7, CR 613.8), so the layer must be
-- run -- which does not spoil the answer, recorded as the effect applied.
decisionsUpTo :: Layer -> [Gathered] -> ObjectId -> GameState -> Map (ObjectId, Natural) Bool
decisionsUpTo bound cands oid gs = snd (projectDeciding (<= bound) cands oid gs)

-- Project every battlefield object against ONE gather: O(gather + P*fold) rather
-- than O(P*(gather+fold)). The hot path for SBA sweeps and combat.
projectAll :: GameState -> Map ObjectId ProjectedCharacteristics
projectAll gs =
  let cands = gather gs
      -- Bound separately, NOT inlined: projectWith does its candidate-only work
      -- when applied to `cands`, so this sharing spans every object on the board.
      forObject = projectFrom cands
   in Map.fromSet (\oid -> forObject oid gs) (GameState.battlefield gs)

-- One object's characteristics out of a PRE-PROJECTED board, falling back to a
-- fresh single-object projection for an id the board does not hold. Every
-- `...Given` reader below is this plus one field read.
--
-- The board is a SNAPSHOT of one GameState, right only while that state is the
-- one being read. The battlefield test keeps the fallback cheap: the board is
-- value-strict, so touching it at all projects every permanent, and an
-- off-battlefield id could never have been a key.
projectGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> ProjectedCharacteristics
projectGiven pcs oid gs =
  let found =
        if Map.null pcs || not (Set.member oid (GameState.battlefield gs))
          then Nothing
          else Map.lookup oid pcs
   in case found of
        Just pc -> pc
        Nothing -> project oid gs

powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf = powerGiven Map.empty

powerGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Maybe Integer
powerGiven pcs oid gs = PC.power (projectGiven pcs oid gs)

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = PC.toughness (project oid gs)

-- CR 702: an object's keyword abilities after the layer fold, counted per
-- keyword. Most readers want hasKeyword or totalToxic.
keywordsOf :: ObjectId -> GameState -> Map Keyword Natural
keywordsOf = keywordsGiven Map.empty

keywordsGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Map Keyword Natural
keywordsGiven pcs oid gs = PC.keywords (projectGiven pcs oid gs)

-- CR 105.2 / 613.1e: an object's colours after the layer fold. The sole read
-- point -- the closed half never reads Face.manaCost for colour.
colorsOf :: ObjectId -> GameState -> Set Color.Color
colorsOf = colorsGiven Map.empty

colorsGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Color.Color
colorsGiven pcs oid gs = PC.colors (projectGiven pcs oid gs)

-- CR 602 / 613.1f: an object's activated abilities after the layer system. A
-- Humility'd creature has none.
--
-- CR 702.178a's "as long as your speed is 4" gate is applied HERE, after the
-- layer system, over the finished projection, and re-asked on every read rather
-- than sampled (CR 604.1). viewOfCharacteristics asks the same gate from INSIDE
-- the fold against a layer-bounded board; neither half of this pair is inside
-- it, so it takes the full view.
abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesOf = abilitiesGiven Map.empty

abilitiesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesGiven pcs oid gs = abilitiesFromCharacteristics (fullView gs) (projectGiven pcs oid gs) oid gs

-- abilitiesGiven with the projection already in hand -- the half
-- viewOfCharacteristics calls.
--
-- CR 702.29b and CR 702.77b are why handAbilitiesOf is in this list: a cycling
-- or reinforce ability exists in every zone, so the object HAS it here; it just
-- cannot be activated here (CR 113.6m).
--
-- CR 613.1: the gate's board comes in as a parameter. Taking fullView here would
-- not terminate for a caller inside the fold -- it re-enters `gather`, with no
-- memo and no descending bound.
abilitiesFromCharacteristics :: Count.ViewOf -> ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesFromCharacteristics peers pc oid gs =
  let granted ability = case ActivatedAbility.condition ability of
        Nothing -> True
        Just cond -> Condition.holds peers (Filter.contextFor (controllerOf oid gs) (Just oid)) gs oid cond
   in -- Rule 702's own activated abilities are appended here, minted from the
      -- POST-LAYER keyword map, so Humility takes crew away with the rest.
      filter
        granted
        ( PC.activatedAbilities pc
            <> Keyword.battlefieldAbilitiesOf (PC.keywords pc)
            <> Keyword.handAbilitiesOf (Map.keysSet (PC.keywords pc))
        )

-- CR 614 / 613 layer 6: an object's replacement effects after the layer system.
-- A Humility'd creature has none -- except the shield pair, which no layer can
-- reach; see shieldOf. CR 604.2's "as long as" clause is asked HERE, against a
-- finished projection, with the source's own controller for CR 109.5's "you".
-- Nothing is latched, so Jared Carthalion's shield goes away the moment the
-- monarchy does, with no trigger and no resolution in between.
replacementsOf :: ObjectId -> GameState -> [ReplacementEffect (Effect.Effect Card.Type.Card)]
replacementsOf oid gs =
  let pc = project oid gs
      lives pr = case PrintedReplacement.condition pr of
        Nothing -> True
        Just cond -> Condition.holds (fullView gs) (Filter.contextFor (controllerOf oid gs) (Just oid)) gs oid cond
   in fmap PrintedReplacement.effect (filter lives (PC.replacementEffects pc))
        <> intrinsicReplacementsOf (announcedXOf oid gs) (phyrexianLifePaidOf oid gs) pc
        <> shieldOf oid gs

-- CR 107.3m: the value of X for this object's enters-the-battlefield replacement
-- effects, and 0 for every object no such spell stands behind. Read off the
-- OBJECT, not the projection: an announcement is not a characteristic, so no CR
-- 613 layer can write it and CR 707.2 does not copy it.
announcedXOf :: ObjectId -> GameState -> Natural
announcedXOf oid gs = Maybe.fromMaybe 0 (Object.announcedX =<< Game.lookupObject oid gs)

-- CR 400.7d's other cost record, `announcedXOf` above's twin: how many of the
-- spell that became this permanent's Phyrexian mana symbols were announced to be
-- paid with life (CR 601.2b). Rule 702.150a's compleated is the one reader.
phyrexianLifePaidOf :: ObjectId -> GameState -> Natural
phyrexianLifePaidOf oid gs = maybe 0 Object.phyrexianLifePaid (Game.lookupObject oid gs)

-- CR 122.1c: the pair of effects one or more shield counters create.
--
-- ONE of each however many counters are on it, per the rule's own "a single
-- replacement effect and a single prevention effect": the count is how many
-- times the pair may still be applied, and two counters must not offer CR
-- 616.1's choice two interchangeable candidates.
--
-- Read off Object.counters rather than the projection: counters are not a
-- characteristic (CR 122.1) and the pair is a rule rather than an ability, so
-- the layer system has nothing to remove (compare CR 306.5b). The prevention
-- half's self-scope is likewise read off its SOURCE in Replacement.applies
-- rather than baked into DamagePattern.whichRecipient, which is compared to the
-- event's Recipient TAG (CR 510.1b) and not to which permanent was hit.
shieldOf :: ObjectId -> GameState -> [ReplacementEffect (Effect.Effect Card.Type.Card)]
shieldOf oid gs =
  if shieldCounters oid gs == 0
    then []
    else
      [ ReplacementEffect.DestructionR DestructionRewrite.RemoveShieldCounter,
        ReplacementEffect.DamageR
          ( DamageR.MkDamageR
              DamagePattern.MkDamagePattern
                { DamagePattern.whichKind = Nothing,
                  DamagePattern.whatSource = Filter.Type.And [],
                  -- Rule 122.1c's recipient is the permanent the pair was minted
                  -- onto, which the CR 616.1 loop already scopes by source.
                  DamagePattern.whatRecipient = Nothing,
                  DamagePattern.whichRecipient = Nothing,
                  -- No player chose this pair's source (CR 609.7a) and nothing
                  -- targeted it (CR 601.2c); rule 122.1c minted it off the
                  -- permanent's counters.
                  DamagePattern.whichSource = Nothing
                }
              DamageRewrite.PreventRemovingShieldCounter
              -- CR 615.5: the counter removal is part of the REWRITE, so this
              -- minted row carries no riders.
              Seq.empty
          )
      ]

-- CR 122.1c: how many shield counters this object has, which is how many more
-- events its pair may replace.
shieldCounters :: ObjectId -> GameState -> Natural
shieldCounters oid gs = case Game.lookupObject oid gs of
  Nothing -> 0
  Just obj -> Map.findWithDefault 0 CounterKind.Shield (Object.counters obj)

-- CR 306.5b / 614.1c: a planeswalker's intrinsic "enters with loyalty counters"
-- replacement, CR 310.4b's twin for a battle's defense, and rule 702.136a's riot
-- and CR 714.3a's Saga lore counter beside them. The keyword call at the end
-- also mints CR 702.37b's megamorph row, a CR 614.1e one.
--
-- CR 310.9a's protector is NOT here, though it too is chosen as a battle enters:
-- rule 310.9a names no ability and cites no rule 614. See
-- Pawl.Engine.Event.designateProtector.
--
-- Minted from the finished projection rather than stored on the card, so it is
-- keyed on the PROJECTED card type and reads the PROJECTED loyalty, a copiable
-- value under CR 707.2 (CR 707.5). Minting AFTER the layer fold puts loyalty out
-- of LoseAllAbilities' reach, CR 306.5b giving it as a rule, while riot, being a
-- keyword, is inside it. `announcedX` is CR 107.3m's, and the loyalty arm is its
-- one reader: a printed loyalty of X is settled at CR 601.2b before it arrives.
--
-- CR 702.150a's compleated lands INSIDE the loyalty arm rather than beside it as
-- a minted row of its own, and rule 702.150a's wording is why: it says the
-- permanent "instead enters the battlefield with THAT MANY loyalty counters minus
-- two", which changes the number this row places rather than placing any. Read
-- off the same finished projection, so a compleated ability the CR 613 fold
-- removed is gone -- which is what a keyword needs, where CR 306.5b's loyalty
-- itself is a rule and stays.
--
-- Not implemented: CR 616.1's choice of order between rule 702.150a and another
-- replacement modifying the same entry -- CR 614.16's counter multipliers are the
-- ones that would differ, and no such card is in `data/cards/` (#1996).
intrinsicReplacementsOf :: Natural -> Natural -> ProjectedCharacteristics -> [ReplacementEffect (Effect.Effect Card.Type.Card)]
intrinsicReplacementsOf announcedX phyrexianLifePaid pc =
  [ -- CR 614.1c: the entering object is the ability's own source, so the pattern
  -- is Filter.IsSource.
  ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Loyalty n)))
  | Set.member CardType.Planeswalker (PC.cardTypes pc),
    printed <- Maybe.maybeToList (PC.loyalty pc),
    let base = case printed of
          Loyalty.Literal m -> m
          Loyalty.Variable -> announcedX,
    -- CR 702.150a: "minus two for each of those mana symbols". Saturating,
    -- because a counter count is a Natural and rule 122.1 knows no negative
    -- number of them -- rule 702.150a's own "would enter with one or more" is
    -- what makes the floor unreachable on a printed card anyway.
    let n =
          if Map.member Keyword.Type.Compleated (PC.keywords pc)
            then Natural.minusSaturating base (2 * phyrexianLifePaid)
            else base
  ]
    -- CR 310.4b's intrinsic defense counters -- CR 306.5b's clause one rule
    -- number over, keyed on the projected card type.
    <> [ ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.Defense n)))
       | Set.member CardType.Battle (PC.cardTypes pc),
         Defense.MkDefense n <- Maybe.maybeToList (PC.defense pc)
       ]
    <> Keyword.mintedReplacementsOf (PC.keywords pc)
    -- CR 714.3a's intrinsic lore counter, minted off the same projection for CR
    -- 306.5b's reason: a subtype is not an ability.
    <> Saga.entryReplacementsOf pc

-- CR 614.1: every replacement effect active on the battlefield, PAIRED WITH ITS
-- SOURCE -- a ControllerRelation pattern (CR 109.5's "you") is unanswerable
-- without it. Short-circuits when no permanent's base card has one.
--
-- The short-circuit reads BASE cards while the result reads the PROJECTION,
-- sound only because every route to an unprinted replacement effect is covered:
-- `EntryR AsCopy` on a card that is itself a base card with one, CR 122.1c's
-- shield counters, or a minting keyword printed on or granted by a face.
--
-- Not implemented: a minting keyword reaching a permanent through a stored
-- continuous effect or a keyword counter is on no base face (#833).
replacementsAffecting :: GameState -> [(ObjectId, ReplacementEffect (Effect.Effect Card.Type.Card))]
replacementsAffecting gs =
  let onBattlefield = Set.toList (GameState.battlefield gs)
      -- CR 122.1c's pair is minted from COUNTERS, so it is on no base face --
      -- which is why it is asked before the face is looked up at all.
      baseHas oid | shieldCounters oid gs > 0 = True
      baseHas oid = case Game.faceOf oid gs of
        Nothing -> False
        -- The planeswalker disjunct keeps CR 306.5b's intrinsic replacement
        -- inside the short-circuit: it appears in no base face's list.
        Just face ->
          not (null (Face.replacementEffects face))
            || Set.member CardType.Planeswalker (TypeLine.types (Face.typeLine face))
            -- The Saga disjunct is the planeswalker one's twin, for CR 714.3a.
            || Set.member Subtype.Type.Saga (TypeLine.subtypes (Face.typeLine face))
            -- And the battle disjunct is the third, for CR 310.4b.
            || Set.member CardType.Battle (TypeLine.types (Face.typeLine face))
            || any Keyword.mintsReplacement (Face.keywords face)
            || any (any (grantsKeywordWhere Keyword.mintsReplacement) . StaticAbility.modifications) (Face.staticAbilities face)
      forOne oid = fmap (\re -> (oid, re)) (replacementsOf oid gs)
   in if not (any baseHas onBattlefield)
        then []
        else concatMap forOne onBattlefield

-- Does this modification hand its affected objects a keyword satisfying `p`?
-- Exhaustive rather than a catch-all: a modification added later that also hands
-- out abilities would otherwise answer False and take its grantee out of the
-- gathered set, a MISSING row rather than a build failure.
grantsKeywordWhere :: (Keyword -> Bool) -> Modification -> Bool
grantsKeywordWhere p m = case m of
  Modification.GainKeyword k -> p k
  -- Hands out CR 702.5a's enchant, which is not a Pawl.Types.Keyword at all, so
  -- there is nothing here for `p` to be asked about.
  Modification.GainEnchant _ -> False
  -- Hands out an ability but never a KEYWORD, which is all the callers ask about.
  Modification.GainAbility _ -> False
  Modification.LoseAllAbilities -> False
  Modification.LoseNamedAbility _ -> False
  Modification.SetBasePowerToughness {} -> False
  Modification.ModifyPowerToughness {} -> False
  Modification.SetLandSubtype _ -> False
  Modification.SetLandSubtypeToChosen -> False
  Modification.AddLandSubtype _ -> False
  Modification.SetCreatureSubtype _ -> False
  Modification.AddCreatureSubtype _ -> False
  Modification.AddEveryCreatureSubtype -> False
  Modification.AddSubtype _ -> False
  Modification.AddCardType _ -> False
  Modification.SetCardType _ -> False
  Modification.AddSupertype _ -> False
  Modification.RemoveSupertype _ -> False
  Modification.ChangeSubtypeWord {} -> False
  Modification.SetController _ -> False
  Modification.SetControllerToSource -> False
  Modification.SetColor _ -> False
  Modification.AddColor _ -> False
  Modification.AddChosenColor -> False
  Modification.SwitchPowerToughness -> False

-- CR 603 / 613 layer 6: an object's printed-and-granted triggered abilities
-- after the layer system. A Humility'd creature has none.
--
-- Not the whole list: rule 702's minted ones come from mintedTriggeredAbilitiesOf
-- below, and a reader wanting every triggered ability must add them.
triggeredAbilitiesOf :: ObjectId -> GameState -> [TriggeredAbility Card.Type.Card]
triggeredAbilitiesOf oid gs = PC.triggeredAbilities (project oid gs)

-- The other half of that list: the triggered abilities rule 702 MINTS from a
-- finished projection's keyword counts, with the object's CR 612 text changes
-- applied. The rewrite happens here rather than at layer 3 because a keyword's
-- rules text is the rule's, not the card's, so the words a text change reaches
-- do not exist until the mint runs (CR 612.1, CR 612.2a).
--
-- Over-reaches by CR 612.3 for a keyword GRANTED at layer 6, which arrives after
-- the layer-3 swap and should keep the printed word. No card grants afterlife,
-- fabricate or soulshift (gap #1600).
mintedTriggeredAbilitiesOf :: ProjectedCharacteristics -> [TriggeredAbility Card.Type.Card]
mintedTriggeredAbilitiesOf pc =
  let pairs = fmap (\c -> (ChangeSubtypeWord.from c, ChangeSubtypeWord.to c)) (PC.subtypeWordChanges pc)
   in fmap (rewriteTriggeredAbility pairs) (Keyword.triggeredAbilitiesOf (PC.keywords pc))

-- CR 702.5a / 613 layer 6: the object's enchant abilities after the fold --
-- printed and granted together, which is what Modification.GainEnchant exists to
-- reach. Folded into CR 702.5c's single slot by Pawl.Engine.Card.foldEnchant at
-- each reader.
--
-- No `enchantGiven` sibling beside it, where subtypesOf and cardTypesOf have one:
-- the caller that holds a pre-pass is Pawl.Engine.Sba, and it reads PC.enchant out
-- of that map directly rather than asking for a projection it already has.
enchantOf :: ObjectId -> GameState -> [TargetSlot.TargetSlot]
enchantOf oid gs = PC.enchant (project oid gs)

subtypesOf :: ObjectId -> GameState -> Set Subtype.Type.Subtype
subtypesOf = subtypesGiven Map.empty

subtypesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Subtype.Type.Subtype
subtypesGiven pcs oid gs = PC.subtypes (projectGiven pcs oid gs)

-- CR 201.1 / 707.2: the object's projected names -- a Clone's is the name it
-- copied. Plural for CR 709's several, and EMPTY for CR 708.2a's none.
namesOf :: ObjectId -> GameState -> Set CardName.CardName
namesOf oid = PC.names . project oid

-- CR 709.4a: an object has the chosen name if one of its names is it -- so a
-- Room with both doors open does not have its combined Face's rendering.
hasName :: CardName.CardName -> ObjectId -> GameState -> Bool
hasName name oid gs = Set.member name (namesOf oid gs)

-- CR 205.4: the object's projected supertypes, the sibling of subtypesOf.
supertypesOf :: ObjectId -> GameState -> Set Supertype.Supertype
supertypesOf = supertypesGiven Map.empty

-- The same supertypes against a pre-projected board.
supertypesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Supertype.Supertype
supertypesGiven pcs oid gs = PC.supertypes (projectGiven pcs oid gs)

cardTypesOf :: ObjectId -> GameState -> Set CardType.CardType
cardTypesOf = cardTypesGiven Map.empty

cardTypesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set CardType.CardType
cardTypesGiven pcs oid gs = PC.cardTypes (projectGiven pcs oid gs)

-- CR 613.1d: creature-ness is the projected card-type question. An
-- Opalescence'd enchantment is a creature.
isCreatureOf :: ObjectId -> GameState -> Bool
isCreatureOf = isCreatureGiven Map.empty

isCreatureGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isCreatureGiven pcs oid gs = Set.member CardType.Creature (cardTypesGiven pcs oid gs)

-- CR 613.1d again, for CR 115.4's "any target" pool and CR 120.3c's loyalty
-- removal.
isPlaneswalkerOf :: ObjectId -> GameState -> Bool
isPlaneswalkerOf = isPlaneswalkerGiven Map.empty

isPlaneswalkerGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isPlaneswalkerGiven pcs oid gs = Set.member CardType.Planeswalker (cardTypesGiven pcs oid gs)

-- CR 613.1d a third time, for CR 115.4's fourth kind of "any target" and CR
-- 120.3h's defense-counter removal.
isBattleOf :: ObjectId -> GameState -> Bool
isBattleOf = isBattleGiven Map.empty

isBattleGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isBattleGiven pcs oid gs = Set.member CardType.Battle (cardTypesGiven pcs oid gs)

-- The same question against a PRECOMPUTED candidate list rather than a
-- pre-projected board: cheaper for a caller asking about a handful of objects.
isCreatureFrom :: [Gathered] -> ObjectId -> GameState -> Bool
isCreatureFrom cands oid gs = Set.member CardType.Creature (PC.cardTypes (projectFrom cands oid gs))

-- Membership, which DISCARDS the count -- right for every keyword whose extra
-- instances the rules call redundant (CR 702.3c). totalToxic is the other shape.
hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword = hasKeywordGiven Map.empty

hasKeywordGiven :: Map ObjectId ProjectedCharacteristics -> Keyword -> ObjectId -> GameState -> Bool
hasKeywordGiven pcs keyword oid gs = Map.member keyword (keywordsGiven pcs oid gs)

-- Rule 702.164b: a creature's total toxic value sums the N of every toxic
-- ability it has, with no redundancy clause -- which is why keywords are counted.
totalToxic :: ObjectId -> GameState -> Natural
totalToxic oid gs = toxicIn (keywordsOf oid gs)

-- totalToxic's fold, over a keyword map the caller already has -- so a reader
-- that took the map through CR 608.2h's last-known fallback sums it the same way.
toxicIn :: Map Keyword Natural -> Natural
toxicIn keywords =
  let value keyword count = case keyword of
        Keyword.Type.Toxic n -> n * count
        _ -> 0
   in sum (Map.elems (Map.mapWithKey value keywords))

-- One control-granting static ability, flattened: the source and the timestamp
-- its effect takes (CR 613.7a).
data ControlGrant = MkControlGrant
  { cgSource :: ObjectId,
    cgAffected :: Affected.Affected,
    cgTimestamp :: Timestamp
  }
  deriving (Eq, Ord, Show)

-- Every layer-2 control-granting STATIC ability on the battlefield, gathered
-- once. NOT `gather`, and PROJECTION-FREE throughout: affects reads controllerOf
-- to supply CR 109.5's "you", so a controlGrants that consulted the layers would
-- be mutually recursive with it. That is why controlNames below reads copiable
-- values rather than the projection, and why no CR 305.7 gate is applied here.
-- Hoisted for the same reason setLandSubtypeEffects is: `controls` calls
-- controllerOf once per battlefield object.
--
-- Not implemented: CR 604.2's "as long as" gate, which setLandSubtypeEffects
-- does ask -- the same mutual recursion rules it out here (#1529).
controlGrants :: GameState -> [ControlGrant]
controlGrants gs =
  let grantsOf permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.faceOf permId gs of
          Nothing -> []
          Just face ->
            let isControl sa = any isControlOp (StaticAbility.modifications sa)
                isControlOp m = case m of
                  Modification.SetControllerToSource -> True
                  _ -> False
                toGrant sa =
                  MkControlGrant
                    { cgSource = permId,
                      cgAffected = StaticAbility.affected sa,
                      cgTimestamp = Object.timestamp permObj
                    }
             in fmap toGrant (filter (\sa -> isControl sa && functionsFromZone Zone.Battlefield sa) (Face.staticAbilities face))
   in concatMap grantsOf (abilitySources gs)

-- CR 303.4b: WHICH object this one is attached to -- what an Aura "enchants".
-- Nothing where it is attached to nothing, and where it is attached to a PLAYER
-- (CR 303.4's other destination), which is why Affected.Attached and
-- Affected.AttachedPlayerControls are two arms. No projection at all, so a
-- caller inside the layer fold may ask it -- which is what lets
-- Filter.IsHostOfSource be answered anywhere a source and a GameState are in hand.
hostOf :: ObjectId -> GameState -> Maybe ObjectId
hostOf oid gs = Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf

-- CR 108.4 / 613.1b: an object's controller is its owner, overridden by layer-2
-- control effects, last timestamp wins (CR 613.7). Stored continuous effects and
-- control-granting static abilities both carry a Timestamp and merge into one
-- maximum. A lean fold rather than the full projection: control precedes P/T.
controllerOf :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOf oid gs = controllerOfGiven (controlGrants gs) Set.empty oid gs

-- controllerOf with the grant list PRECOMPUTED and a visited set. The visited set
-- is a CR 613.8b loop-escape analog, not an implementation of it (#946):
-- deriving a grant's player asks for its SOURCE's controller, which can re-enter
-- this function, and re-entering an object already under question returns its
-- owner so a cycle grants nothing.
controllerOfGiven :: [ControlGrant] -> Set ObjectId -> ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOfGiven grants visited oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj ->
    if Set.member oid visited
      then Just (defaultControllerOf obj)
      else
        let visited' = Set.insert oid visited
            -- Does an affected set carried by `source` name `oid`? controlNames
            -- below is the enumeration this membership test reads off.
            namesFrom source a = Set.member oid (controlNames grants visited' gs source a)
            storedSetter eff = case ContinuousEffect.modification eff of
              Modification.SetController pid
                | namesFrom (ContinuousEffect.source eff) (ContinuousEffect.affected eff) ->
                    Just (ContinuousEffect.timestamp eff, pid)
              _ -> Nothing
            stored = Maybe.mapMaybe storedSetter (GameState.continuousEffects gs)
            fromGrant g =
              if not (namesFrom (cgSource g) (cgAffected g))
                then Nothing
                else case controllerOfGiven grants visited' (cgSource g) gs of
                  Nothing -> Nothing
                  Just who -> Just (cgTimestamp g, who)
            derived = Maybe.mapMaybe fromGrant grants
         in case stored <> derived of
              [] -> Just (defaultControllerOf obj)
              setters -> Just (snd (List.maximumBy (Ord.comparing fst) setters))

-- Which objects an affected set NAMES, for the CR 613.1b layer-2 control fold.
-- Parameterized by the source because Affected.Attached asks about the SOURCE's
-- state, and by the grant list and the caller's visited set because a PREDICATE
-- set has to answer control questions of its own -- CR 109.5's "you" for the
-- filter's perspective, and the candidate's own controller for a filter that
-- asks. Both go back through controllerOfGiven, never through the projection
-- (see controlGrants).
--
-- Not implemented: MatchingAnywhere, MatchingOffBattlefield and
-- AttachedPlayerControls, which stay empty and so grant nothing (#1927).
controlNames :: [ControlGrant] -> Set ObjectId -> GameState -> ObjectId -> Affected.Affected -> Set ObjectId
controlNames grants visited gs source a = case a of
  Affected.TheseObjects s -> s
  -- CR 303.4m: the source's own attachment, with no projection needed.
  Affected.Attached -> maybe Set.empty Set.singleton (hostOf source gs)
  -- CR 611.3a: a static ability's effect is not locked in, so the set is
  -- re-derived from the battlefield at every projection and a permanent that
  -- enters later is in it. CR 613.1a/613.2c: layer 2 reads an object's COPIABLE
  -- values, since layer 1 is the only layer before it and CR 613.8a confines
  -- dependency to one layer -- so no layer-4 type change feeds this test, which
  -- is what lets it run without projecting.
  Affected.Matching f -> Set.filter (matchesLeanly grants visited gs source f) (GameState.battlefield gs)
  Affected.MatchingAnywhere _ -> Set.empty
  Affected.MatchingOffBattlefield _ -> Set.empty
  Affected.AttachedPlayerControls _ -> Set.empty

-- Does `oid` match a layer-2 affected set's Filter, read at the copiable values
-- controlNames explains and with CR 109.5's "you" bound to the SOURCE's
-- controller? Projection-free throughout: every controller it needs comes from
-- controllerOfGiven carrying the caller's visited set, which answers an object's
-- owner once it revisits that object, so a control-dependent conjunct terminates
-- rather than re-entering the fold that is asking (#946). Termination is
-- structural here, not a matter of Filter.View's laziness -- which is what
-- separates this path from the liveness gate #197 describes.
--
-- Both of those controller reads are REGRESSION FENCES rather than proved
-- behaviour: a filter only forces either one by asking about control, and
-- `data/cards/`'s one predicate control grant is Synthetic Goblin Dominion's
-- "You control all Goblins", whose filter does not. Blanking the perspective
-- leaves the suite green. The card that would prove them is #197's shape, a
-- control-dependent conjunct under a control grant, and it would also be the
-- first board on which this recursion is more than linear: each candidate whose
-- controller is forced re-enters the fold, which walks the battlefield again.
matchesLeanly :: [ControlGrant] -> Set ObjectId -> GameState -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesLeanly grants visited gs source f oid =
  Filter.matches
    (Filter.contextFor (controllerOfGiven grants visited source gs) (Just source))
    (leanViewOf grants visited gs oid)
    f

-- The Filter.View a layer-2 affected set reads: copiable characteristics
-- (CR 613.2c) and a controller from the lean fold. The counterpart to
-- viewOfObjectGiven for a caller that must not project at all; a host is read
-- the same way, which is finite for the reason viewOfObjectGiven gives.
leanViewOf :: [ControlGrant] -> Set ObjectId -> GameState -> ObjectId -> Filter.View
leanViewOf grants visited gs oid =
  viewOfCharacteristics
    (Just . leanViewOf grants visited gs)
    oid
    (copiableCharacteristics oid gs)
    (controllerOfGiven grants visited oid gs)
    (countersOf oid gs)
    gs

-- CR 110.2 / 108.4a: the controller a CR 613.1b layer-2 effect OVERRIDES. A
-- permanent's default controller is whoever it entered under (CR 110.2), and
-- Object.enteredUnder is Nothing outside the two zones CR 109.4 gives a
-- controller, leaving CR 108.4a's owner.
defaultControllerOf :: Object.Object -> PlayerId.PlayerId
defaultControllerOf obj = Maybe.fromMaybe (Object.owner obj) (Object.enteredUnder obj)

-- The battlefield permanents a player controls (CR 108.4). Computes the grant
-- list ONCE and threads it, which is linear rather than quadratic.
controls :: PlayerId.PlayerId -> GameState -> [ObjectId]
controls pid gs = controlsGiven (controlGrants gs) pid gs

-- controls with the grant list PRECOMPUTED, so a caller that then asks
-- controllerOfGiven about the permanents keeps from rebuilding it per candidate.
controlsGiven :: [ControlGrant] -> PlayerId.PlayerId -> GameState -> [ObjectId]
controlsGiven grants pid gs =
  filter (\oid -> controllerOfGiven grants Set.empty oid gs == Just pid) (Set.toList (GameState.battlefield gs))

-- CR 800.4a: does this stored effect give `pid` control of an object?
-- SetController's payload IS the player who gains control.
givesControlTo :: PlayerId.PlayerId -> ContinuousEffect.ContinuousEffect Card.Type.Card -> Bool
givesControlTo pid eff = case ContinuousEffect.modification eff of
  Modification.SetController who -> who == pid
  -- Names no player, so it cannot be classified from the effect alone: CR 109.5
  -- makes its player the current controller of the source.
  Modification.SetControllerToSource -> False
  _ -> False
