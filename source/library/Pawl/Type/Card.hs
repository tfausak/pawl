module Pawl.Type.Card where

import Data.Set (Set)
import Data.Text (Text)
import Pawl.Type.ActivatedAbility (ActivatedAbility)
import Pawl.Type.CastingPermission (CastingPermission)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.ManaCost (ManaCost)
import Pawl.Type.Modal (Modal)
import Pawl.Type.Power (Power)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.StaticAbility (StaticAbility)
import Pawl.Type.Toughness (Toughness)
import Pawl.Type.TriggeredAbility (TriggeredAbility)
import Pawl.Type.TypeLine (TypeLine)

data Card = MkCard
  { name :: Text,
    -- Nothing, not a zero cost: CR 202.1, a land has no mana cost at all.
    manaCost :: Maybe ManaCost,
    typeLine :: TypeLine,
    -- Only creatures have these.
    power :: Maybe Power,
    toughness :: Maybe Toughness,
    -- CR 702. A Set because CR 702.9c and 702.3c say multiple instances are
    -- redundant -- a per-keyword fact, true of everything through M2c, and NOT
    -- true out in the tail (two Wards both trigger; Rampage stacks).
    --
    -- The closed half must read this through Pawl.Projection.keywordsOf, never
    -- directly: layer 6 grants and removes abilities at M3.
    keywords :: Set Keyword,
    -- CR 604.1/604.2: this card's static continuous abilities (Humility). Empty
    -- for everything but the few printings that generate a continuous effect just
    -- by being on the battlefield. The projection gathers these live.
    staticAbilities :: [StaticAbility],
    -- The card's spell payload as data: what casting this card does when it
    -- resolves, as one or more modes (CR 700.2). A non-modal card -- every card
    -- before M4g -- is a single mode with ChooseExactly 1 (forced, unprompted). A
    -- land or vanilla creature is a single EMPTY mode (no spell effects; resolution
    -- just enters the battlefield). Card ties Modal's `card` knot at `Modal Card`.
    spell :: Modal Card,
    -- CR 602: this card's printed activated abilities. Empty for all but the few
    -- printings that grant one. The closed half reads these through
    -- Pawl.Projection.abilitiesOf (Task 9), never directly: layer 6 (Humility)
    -- removes abilities.
    activatedAbilities :: [ActivatedAbility Card],
    -- CR 614: this card's replacement effects, active while it is on the
    -- battlefield. Read through Pawl.Projection.replacementsOf (never directly)
    -- so layer 6 LoseAllAbilities strips them uniformly. Empty for all but Rest
    -- in Peace.
    replacementEffects :: [ReplacementEffect],
    -- CR 603: this card's triggered abilities, read through
    -- Pawl.Projection.triggeredAbilitiesOf. Empty for all but Rest in Peace.
    triggeredAbilities :: [TriggeredAbility Card],
    -- CR 601.3: this card's casting permissions -- zone/condition exceptions to
    -- normal timing. Read directly from the card (NOT the projection): the
    -- permission functions in the library (CR 113.6), where the CR 613 layer
    -- system does not reach. Empty for all but Panglacial Wurm.
    castingPermissions :: [CastingPermission],
    -- CR 707.9a / 614.1c: True iff this card enters the battlefield AS A COPY of
    -- another permanent by its controller's choice (Clone). A closed-half
    -- classification (like a keyword citation), read by Event.placeObject and the
    -- as-enters drain -- never the card's identity. False for every card but Clone.
    copyOnEnter :: Bool
  }
  deriving (Eq, Ord, Show)
