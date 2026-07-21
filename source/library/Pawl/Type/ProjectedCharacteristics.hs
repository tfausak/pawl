module Pawl.Type.ProjectedCharacteristics where

import Data.Set (Set)
import Pawl.Type.ActivatedAbility (ActivatedAbility)
import Pawl.Type.Card (Card)
import Pawl.Type.CardType (CardType)
import Pawl.Type.Color (Color)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.Subtype (Subtype)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

-- The characteristics of an object after the layer fold (design.md §2.5). Maybe
-- P/T because a land has none. cardTypes/subtypes are the projected type line
-- (CR 613 layer 4). rulesTextActive is CR 305.7: False once an effect SETS this
-- object's land subtype to a basic type, stripping its rules-text abilities.
-- Ord derived so a copy snapshot can ride a Binding (CR 707.2, P2).
data ProjectedCharacteristics = MkProjectedCharacteristics
  { keywords :: Set Keyword,
    -- CR 105.2 / 613.1e layer 5: the object's colours after the layer system.
    -- A Set, not a sum with a Colorless arm: CR 105.2c says a colourless object
    -- has NO colour, and CR 105.4 denies that colourless is a colour at all.
    colors :: Set Color,
    power :: Maybe Integer,
    toughness :: Maybe Integer,
    cardTypes :: Set CardType,
    subtypes :: Set Subtype,
    rulesTextActive :: Bool,
    -- CR 602 / 613 layer 6: the object's activated abilities after the layer
    -- system. Seeded from the card; emptied by LoseAllAbilities (Humility).
    activatedAbilities :: [ActivatedAbility Card],
    -- CR 614 layer 6: the object's replacement effects after the layer system,
    -- the same projection posture as activatedAbilities. Emptied by LoseAllAbilities.
    replacementEffects :: [ReplacementEffect],
    -- CR 603 layer 6: the object's triggered abilities after the layer system,
    -- the same projection posture as activatedAbilities. Emptied by LoseAllAbilities.
    triggeredAbilities :: [TriggeredAbility Card]
  }
  deriving (Eq, Ord, Show)
