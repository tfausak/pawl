module Pawl.Types.Modification where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Subtype as Subtype

-- | The open-half continuous-effect vocabulary -- its own leaf family (design.md's
-- M3g note: "continuous-effect specifications, classified by layer"), distinct
-- from Effect. The ONLY module that may case on a constructor is Pawl.Engine.Projection
-- (Projection.layer classifies it; Projection.applyModification applies it) --
-- the same standing Pawl.Engine.Resolve has over Effect. GainKeyword carries a Keyword,
-- a closed-half CITATION (casing on it is not an invariant violation -- see the
-- M2a spec). P/T constructors carry signed Quantity (+3/+3 or a future -1/-1).
-- No arm adds or removes a SUPERTYPE (#311), the case CR 205.4b is written for;
-- the layer-4 arms below reach card types and subtypes only.
data Modification
  = GainKeyword Keyword.Keyword -- layer 6 (Serpent's Gift)
  | LoseAllAbilities -- layer 6 (Humility)
  | SetBasePowerToughness Quantity.Quantity Quantity.Quantity -- layer 7b (Humility 1/1; Opalescence mana value)
  | ModifyPowerToughness Quantity.Quantity Quantity.Quantity -- layer 7c (Giant Growth +3/+3)
  | SetLandSubtype Subtype.Subtype -- layer 4, CR 305.7 set (Blood Moon -> Mountain)
  | AddLandSubtype Subtype.Subtype -- layer 4, CR 305.7 add (Urborg -> Swamp)
  | AddCardType CardType.CardType -- layer 4 (Opalescence -> Creature)
  | ChangeSubtypeWord Subtype.Subtype Subtype.Subtype -- layer 3, CR 612 (Magical Hack: from -> to)
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
  | -- | layer 2, CR 613.1b: this object's controller becomes the controller of THIS
    -- effect's SOURCE. Payload-free because the player is DERIVED at projection
    -- time, not baked -- the contrast with SetController above, whose PlayerId is
    -- fixed at resolution by CR 611.2c because a resolution effect's answer is
    -- determined once.
    --
    -- A static ability's modification is CARD DATA and cannot name a PlayerId, so
    -- this is the only shape in which a printed card can grant control. Control
    -- Magic's "You control enchanted creature."
    --
    -- CR 303.4e: an Aura's controller and the enchanted object's controller are
    -- separate. Deriving from the SOURCE's controller is what keeps them so --
    -- gaining control of the creature does not gain control of the Aura, and
    -- gaining control of the Aura DOES move the creature.
    SetControllerToSource
  | -- | layer 5, CR 613.1e / 105.3: this object becomes exactly these colours. A
    -- SET, not an add: CR 105.3 says a new colour REPLACES all previous colours
    -- unless the effect says "in addition" -- and no card in the pool does, so
    -- there is deliberately no AddColor constructor. SetColor with an empty set
    -- is "becomes colourless" (CR 105.2c).
    SetColor (Set.Set Color.Color)
  | -- | layer 7d, CR 613.4d: switch this object's power and toughness. Takes the
    -- value of power and applies it to toughness, and vice versa -- so it acts on
    -- whatever 7a, 7b and 7c already produced, not on the printed box. Carries no
    -- payload: two applications return the object to normal for free.
    SwitchPowerToughness
  deriving (Eq, Ord, Show)
