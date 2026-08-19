module Pawl.Types.ProjectedCharacteristics where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | The characteristics of an object after the layer fold (design.md §2.5). Maybe
-- P/T because a land has none. cardTypes/subtypes are the projected type line
-- (CR 613 layer 4). Ord derived so a copy snapshot can ride a Binding (CR 707.2).
--
-- There is no "are this object's rules-text abilities live" flag. CR 305.7's
-- strip is not a condition to be recorded and consulted later: the ability
-- fields below simply come back EMPTY, exactly as they do for CR 613.1f's layer-6
-- removal, and every reader sees the strip without having to know it happened.
data ProjectedCharacteristics = MkProjectedCharacteristics
  { -- | CR 201.1: the object's names after the layer fold. Copiable (CR 707.2),
    -- which is what earns them a place here rather than a read of the printed
    -- card: a Clone's name is the name it copied, which is what makes CR 704.5j
    -- reach a copy.
    --
    -- A SET, because an object does not have one name. CR 709.4a: "Each split
    -- card has two names ... An object has the chosen name if one of its names
    -- is the chosen name" -- so the only question the rules ever ask of this
    -- field is MEMBERSHIP, and every reader goes through
    -- Pawl.Engine.Projection.hasName rather than comparing to a string. A Room
    -- permanent has one name per unlocked door (CR 709.5), a split card off the
    -- stack has both halves' (CR 709.4), a spell on the stack has the cast
    -- half's alone (CR 709.3b), and a face-down object has NONE (CR 708.2a) --
    -- an empty set rather than an empty name.
    --
    -- Set and not a predicate, which CR 612.7's Spy Kit will eventually want
    -- (#887): a Set keeps Eq/Ord/Show and a codec, and since every reader asks
    -- membership the representation can widen behind them without a caller
    -- changing.
    --
    -- Carried, not folded: rule 613's layer 3 could change them (a text-changing
    -- effect naming a name) but nothing in the pool does, so no layer touches
    -- them after the seed.
    names :: Set.Set CardName.CardName,
    -- | CR 205.4a: the object's supertypes after the layer fold -- the third part
    -- of the layer-4 type line. Copiable for name's reason: a Clone of a legend
    -- is itself legendary, without which the legend rule would spare every copy.
    --
    -- Its own Set rather than more CardType constructors, because CR 205.4a makes
    -- supertypes a separate part of the type line: a permanent can be Legendary
    -- and a Creature at once, and "is it legendary" must not be answerable by the
    -- same question as "is it a creature".
    supertypes :: Set.Set Supertype.Supertype,
    -- | CR 702: this object's keyword abilities after the layer fold, COUNTED PER
    -- KEYWORD. An object can have the same keyword ability more than once, so
    -- counts are the honest model: CR 702.164b sums the N values of every toxic
    -- ability a creature has, which a Set could not say.
    --
    -- Redundancy (CR 702.3c defender, CR 702.9c flying) is a fact about the
    -- QUESTION the reader asks and not about what is stored: hasKeyword asks
    -- membership, so a doubled flying is still just flying.
    keywords :: Map.Map Keyword.Keyword Natural.Natural,
    -- | CR 105.2 / 613.1e layer 5: the object's colours after the layer system.
    -- A Set, not a sum with a Colorless arm: CR 105.2c says a colourless object
    -- has NO colour, and CR 105.4 denies that colourless is a colour at all.
    colors :: Set.Set Color.Color,
    -- | CR 202.3 / 613.2a: the object's mana value, DERIVED from the mana cost at
    -- the seed rather than stored as the cost itself -- colors above take the
    -- same posture, and for the same reason: every reader wants the derived
    -- number, and nothing in the layer system rewrites a mana cost.
    --
    -- Copiable (CR 707.2 names mana cost in its list), which is what earns it a
    -- place here rather than a read of the printed card: a Clone entering as a
    -- copy of Darksteel Myr has mana value 3, not the 4 its own {3}{U} would
    -- give.
    --
    -- Carried, not folded: no Modification writes a mana cost, so no layer
    -- touches this after the seed. Nothing means "no object to ask" -- an ability
    -- on the stack has no card and so no mana value at all, which is a different
    -- claim from CR 202.3a's 0 (#674).
    manaValue :: Maybe Integer,
    power :: Maybe Integer,
    toughness :: Maybe Integer,
    -- | CR 306.5 / 109.3: the object's PRINTED loyalty, copiable (CR 707.2) -- so
    -- a Clone entering as a copy of a planeswalker has the copy's loyalty here.
    --
    -- Seeded from the card and touched by NO layer, unlike power/toughness above:
    -- the CR 613 layer system has no sublayer for loyalty. What a permanent's
    -- loyalty actually is comes from CR 306.5c -- its CounterKind.Loyalty
    -- counters -- so this field answers only CR 306.5a's question, and the one
    -- reader is CR 306.5b's intrinsic enters-with replacement.
    loyalty :: Maybe Loyalty.Loyalty,
    -- | CR 310.4 / 109.3: the object's PRINTED defense -- so a Clone entering as a
    -- copy of a battle has the copy's defense here. The loyalty field above in
    -- every respect except its authority: CR 707.2's list of copiable values names
    -- loyalty and not defense (see Pawl.Types.Defense).
    --
    -- Seeded from the card and touched by NO layer: the CR 613 layer system has
    -- no sublayer for defense either. What a permanent's defense actually is comes
    -- from CR 310.4c -- its CounterKind.Defense counters -- so this field answers
    -- only CR 310.4a's question, and the one reader is CR 310.4b's intrinsic
    -- enters-with replacement.
    defense :: Maybe Defense.Defense,
    -- | CR 613.4a layer 7a: the object's characteristic-defining P/T, as the pair
    -- of UNEVALUATED quantities with the printed star already substituted. Seeded
    -- from the card, so it rides copiableCharacteristics and a Clone acquires the
    -- ability rather than the number (CR 707.2a); emptied by LoseAllAbilities at
    -- layer 6 and by CR 305.7's strip at layer 4, both of which are BEFORE 7a.
    characteristicPT :: Maybe CharacteristicPT.CharacteristicPT,
    cardTypes :: Set.Set CardType.CardType,
    subtypes :: Set.Set Subtype.Subtype,
    -- | CR 602 / 613 layer 6: the object's activated abilities after the layer
    -- system. Seeded from the card; emptied by LoseAllAbilities (Humility) and by
    -- CR 305.7's strip at layer 4 (Blood Moon), as are the two fields below.
    activatedAbilities :: [ActivatedAbility.ActivatedAbility Card.Card],
    -- | CR 614 layer 6: the object's replacement effects after the layer system,
    -- the same projection posture as activatedAbilities, emptied by the same two.
    replacementEffects :: [PrintedReplacement.PrintedReplacement (Effect.Effect Card.Card)],
    -- | CR 603 layer 6: the object's triggered abilities after the layer system,
    -- the same projection posture as activatedAbilities, emptied by the same two.
    triggeredAbilities :: [TriggeredAbility.TriggeredAbility Card.Card],
    -- | CR 612.1 layer 3: the subtype word swaps applied to this object, in the
    -- order they were applied. A RECORD of what layer 3 did, where every field
    -- above is the RESULT of it -- kept because rule 702's abilities are minted
    -- from the finished keyword counts, after the fold, so the mint has no other
    -- way to learn that the words it is about to write were changed (CR 612.2a,
    -- Pawl.Engine.Projection.mintedTriggeredAbilitiesOf).
    --
    -- A list rather than a set or a map: two swaps compose in order (Faerie ->
    -- Elf then Elf -> Goblin is not the same pair of effects as the reverse),
    -- which is CR 613.1's timestamp order the fold already walks in.
    --
    -- Not copiable, and structurally so: CR 707.2's list of copiable values holds
    -- no text change, and this is written by the layer fold rather than by the
    -- seed, so copiableCharacteristics never carries one.
    subtypeWordChanges :: [ChangeSubtypeWord.ChangeSubtypeWord]
  }
  deriving (Eq, Ord, Show)
