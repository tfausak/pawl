module Pawl.Type.ProjectedCharacteristics where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Pawl.Type.ActivatedAbility (ActivatedAbility)
import Pawl.Type.Card (Card)
import Pawl.Type.CardType (CardType)
import Pawl.Type.Color (Color)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.Quantity (Quantity)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.Subtype (Subtype)
import Pawl.Type.Supertype (Supertype)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

-- The characteristics of an object after the layer fold (design.md §2.5). Maybe
-- P/T because a land has none. cardTypes/subtypes are the projected type line
-- (CR 613 layer 4). Ord derived so a copy snapshot can ride a Binding (CR 707.2,
-- P2).
--
-- There is no "are this object's rules-text abilities live" flag. CR 305.7's
-- strip is not a condition to be recorded and consulted later: the ability
-- fields below simply come back EMPTY, exactly as they do for CR 613.1f's layer-6
-- removal, and every reader sees the strip without having to know it happened.
data ProjectedCharacteristics = MkProjectedCharacteristics
  { -- CR 201.1: the object's name after the layer fold. Copiable -- CR 707.2 lists
    -- name first among the copiable values -- and
    -- that is what earns it a place here rather than a read of the printed card:
    -- a Clone's name is the name it copied, which is precisely what makes CR
    -- 704.5j's "two legendary permanents with the SAME NAME" reach a copy.
    --
    -- Carried, not folded: rule 613's layer 3 could change it (a text-changing
    -- effect naming a name) but nothing in the pool does, so no layer touches it
    -- after the seed.
    name :: Text,
    -- CR 205.4a: the object's supertypes after the layer fold -- the third part of
    -- the layer-4 type line, alongside cardTypes and subtypes. Copiable for the
    -- same reason as name: a Clone of a legend is itself legendary, without which
    -- the legend rule would silently spare every copy.
    --
    -- Its own Set rather than more CardType constructors, because CR 205.4a makes
    -- supertypes a separate part of the type line: a permanent can be Legendary
    -- and a Creature at once, and "is it legendary" must not be answerable by the
    -- same question as "is it a creature".
    supertypes :: Set Supertype,
    -- CR 702: this object's keyword abilities after the layer fold, COUNTED PER
    -- KEYWORD. A multiset in Pawl.Type.Deck's shape, and for its reason: an
    -- object can have the same keyword ability more than once, so counts are
    -- the honest model. CR 702.164b reads the count directly -- "the sum of all
    -- N values of toxic abilities that creature has" is 2 for a creature with
    -- toxic 1 twice, which a Set could not say (it collapsed the second grant).
    --
    -- Redundancy (CR 702.3c defender, CR 702.9c flying) is a fact about the
    -- QUESTION the reader asks and not about what is stored: Pawl.Projection's
    -- hasKeyword asks membership, so a doubled flying is still just flying.
    keywords :: Map Keyword Natural,
    -- CR 105.2 / 613.1e layer 5: the object's colours after the layer system.
    -- A Set, not a sum with a Colorless arm: CR 105.2c says a colourless object
    -- has NO colour, and CR 105.4 denies that colourless is a colour at all.
    colors :: Set Color,
    power :: Maybe Integer,
    toughness :: Maybe Integer,
    -- CR 613.4a layer 7a: the object's characteristic-defining P/T, as the pair of
    -- UNEVALUATED quantities (power, toughness) with the printed star already
    -- substituted. Seeded from the card, so it rides copiableCharacteristics and a
    -- Clone acquires the ability rather than the number (CR 707.2a); emptied by
    -- LoseAllAbilities at layer 6 and by CR 305.7's SetLandSubtype at layer 4,
    -- both of which are BEFORE 7a.
    characteristicPT :: Maybe (Quantity, Quantity),
    cardTypes :: Set CardType,
    subtypes :: Set Subtype,
    -- CR 602 / 613 layer 6: the object's activated abilities after the layer
    -- system. Seeded from the card; emptied by LoseAllAbilities (Humility) and
    -- by CR 305.7's SetLandSubtype at layer 4 (Blood Moon), as are the three
    -- ability fields around it.
    activatedAbilities :: [ActivatedAbility Card],
    -- CR 614 layer 6: the object's replacement effects after the layer system,
    -- the same projection posture as activatedAbilities, emptied by the same two.
    replacementEffects :: [ReplacementEffect],
    -- CR 603 layer 6: the object's triggered abilities after the layer system,
    -- the same projection posture as activatedAbilities, emptied by the same two.
    triggeredAbilities :: [TriggeredAbility Card]
  }
  deriving (Eq, Ord, Show)
