module Pawl.Types.Modification where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | The open-half continuous-effect vocabulary: continuous-effect
-- specifications, classified by layer, distinct from Effect. Within the RULES
-- CORE, Pawl.Engine.Projection is the sole module that may case on a
-- constructor -- the same standing Pawl.Engine.Resolve has over Effect.
-- Pawl.CardSpec's lints also case on it, legitimately: a test-suite lint that
-- walks the card pool is not rules core. GainKeyword carries a Keyword, a
-- closed-half CITATION, so casing on it is not an invariant violation. P/T
-- constructors carry signed Quantity. The layer-4 arms below reach card types,
-- subtypes and supertypes, which CR 205.4b keeps independent of one another.
data Modification
  = GainKeyword Keyword.Keyword -- layer 6 (Serpent's Gift)
  | LoseAllAbilities -- layer 6 (Humility)
  | SetBasePowerToughness Quantity.Quantity Quantity.Quantity -- layer 7b (Humility 1/1; Opalescence mana value)
  | ModifyPowerToughness Quantity.Quantity Quantity.Quantity -- layer 7c (Giant Growth +3/+3)
  | SetLandSubtype Subtype.Subtype -- layer 4, CR 305.7 set (Blood Moon -> Mountain)
  | -- | layer 4, CR 613.1d / 305.7: set this object's land subtype to the basic
    -- land type chosen for THIS effect's SOURCE as that source entered
    -- (Object.chosenSubtype). Convincing Mirage's "enchanted land is the chosen
    -- type".
    --
    -- Payload-free because the subtype is DERIVED at projection time from the
    -- source rather than baked into card data, the posture AddChosenColor takes
    -- toward Object.chosenColor: a static ability's modification is card data
    -- and cannot name a type a player will choose, which is why this is a second
    -- constructor rather than a field on SetLandSubtype above.
    --
    -- Carries CR 305.7's ability strip in full, exactly as SetLandSubtype does:
    -- Pawl.Engine.Projection routes both through setLandSubtypeTo, and its
    -- setLandSubtypeEffects answers True for both, so the fold half and the
    -- candidate-list-gate half of that rule cannot drift apart.
    SetLandSubtypeToChosen
  | AddLandSubtype Subtype.Subtype -- layer 4, CR 305.7 add (Urborg -> Swamp)
  | -- | layer 4, CR 205.1a/205.1b set (Turn to Frog -> Frog). A SET over the
    -- CREATURE types only, which is narrower than either land arm above: CR
    -- 205.1b keeps the object's other card types and subtypes and replaces only
    -- its creature types. CR 205.3m is the list of what that reaches.
    --
    -- AddCreatureSubtype sits beside it, the way AddColor sits beside SetColor
    -- below. CR 205.1b allows several creature types and this carries exactly
    -- one, the same narrowing SetLandSubtype takes.
    SetCreatureSubtype Subtype.Subtype
  | -- | layer 4, CR 205.1b add (Life and Limb -> Saproling). The ADD beside the
    -- SET above, standing to it as AddLandSubtype stands to SetLandSubtype: the
    -- object keeps every creature type it had and gains this one.
    --
    -- No ability clause on this arm or on the set above. CR 305.7's strip is the
    -- land arms' alone, which is why neither creature-type arm routes through
    -- setLandSubtypeTo.
    AddCreatureSubtype Subtype.Subtype
  | AddCardType CardType.CardType -- layer 4 (Opalescence -> Creature)
  | -- | layer 4, CR 613.1d / 205.4b: this object gains a supertype (Leyline of
    -- Singularity's "All nonland permanents are legendary"). An ADD and never a
    -- set, because CR 205.4b says so outright -- "when an object gains or loses a
    -- supertype, it retains any other supertypes it had" -- so no supertype arm
    -- has the SetLandSubtype/AddLandSubtype pairing the subtypes need.
    --
    -- Carries one Supertype rather than a set, the narrowing SetLandSubtype and
    -- SetCreatureSubtype already take: no printing grants two at once, and a card
    -- that did would author two modifications.
    AddSupertype Supertype.Supertype
  | -- | layer 4, CR 613.1d / 205.4b: this object loses a supertype (Arcum's
    -- Weathervane's "Target snow land is no longer snow"). The removal beside the
    -- grant above, and the same rule governs it: the object's OTHER supertypes
    -- survive, and neither its card types nor its subtypes move.
    RemoveSupertype Supertype.Supertype
  | ChangeSubtypeWord Subtype.Subtype Subtype.Subtype -- layer 3, CR 612 (Magical Hack, Artificial Evolution: from -> to)
  | -- | layer 2, CR 613.1b: set this object's controller. The PlayerId is BAKED at
    -- effect creation (CR 611.2c) by Resolve.applyEffect (GainControl) -- it is
    -- the effect's source's controller, never chosen. Applied only by
    -- Projection.controllerOf.
    --
    -- Meant to be runtime-only, and nothing ENFORCES that in the type: the
    -- codec round-trips the PlayerId, so card JSON could author one into an
    -- Effect.ModifyTarget. Baking a PlayerId into static card text is
    -- meaningless, so Pawl.CardSpec lints the pool against it (#199).
    SetController PlayerId.PlayerId
  | -- | layer 2, CR 613.1b: this object's controller becomes the controller of
    -- THIS effect's SOURCE (Control Magic). Payload-free because the player is
    -- DERIVED at projection time, the contrast with SetController above, whose
    -- PlayerId CR 611.2c fixes at resolution. A static ability's modification is
    -- CARD DATA and cannot name a PlayerId, so this is the only shape in which a
    -- printed card can grant control.
    --
    -- CR 303.4e: an Aura's controller and the enchanted object's controller are
    -- separate. Deriving from the SOURCE's controller is what keeps them so --
    -- gaining control of the creature does not gain control of the Aura, and
    -- gaining control of the Aura DOES move the creature.
    SetControllerToSource
  | -- | layer 5, CR 613.1e / 105.3: this object becomes exactly these colours. A
    -- SET, not an add: CR 105.3 says a new colour REPLACES all previous colours
    -- unless the effect says "in addition". SetColor with an empty set is
    -- "becomes colourless" (CR 105.2c).
    SetColor (Set.Set Color.Color)
  | -- | layer 5, CR 613.1e / 105.3: this object becomes these colours IN
    -- ADDITION to the ones it already has -- CR 105.3's parenthetical, so this
    -- unions where SetColor replaces. Indigo Faerie.
    AddColor (Set.Set Color.Color)
  | -- | layer 5, CR 613.1e / 105.3: this object gains, IN ADDITION to its other
    -- colours, the colour chosen for THIS effect's SOURCE as that source entered
    -- (Object.chosenColor). Painter's Servant's "the chosen color".
    --
    -- Payload-free because the colour is DERIVED at projection time from the
    -- source rather than baked into card data: a static ability's modification
    -- cannot name a colour a player will choose, which is why this is a
    -- constructor beside AddColor rather than a value it could carry.
    AddChosenColor
  | -- | layer 7d, CR 613.4d: switch this object's power and toughness. It acts
    -- on whatever 7a, 7b and 7c already produced, not on the printed box.
    -- Carries no payload: two applications return the object to normal for free.
    SwitchPowerToughness
  deriving (Eq, Ord, Show)
