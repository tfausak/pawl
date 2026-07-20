module Pawl.Type.Modification where

import Pawl.Type.CardType (CardType)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Quantity (Quantity)
import Pawl.Type.Subtype (Subtype)

-- The open-half continuous-effect vocabulary -- its own leaf family (design.md's
-- M3g note: "continuous-effect specifications, classified by layer"), distinct
-- from Effect. The ONLY module that may case on a constructor is Pawl.Projection
-- (Projection.layer classifies it; Projection.applyModification applies it) --
-- the same standing Pawl.Resolve has over Effect. GainKeyword carries a Keyword,
-- a closed-half CITATION (casing on it is not an invariant violation -- see the
-- M2a spec). P/T constructors carry signed Quantity (+3/+3 or a future -1/-1).
data Modification
  = GainKeyword Keyword -- layer 6 (Serpent's Gift)
  | LoseAllAbilities -- layer 6 (Humility)
  | SetBasePowerToughness Quantity Quantity -- layer 7b (Humility 1/1; Opalescence mana value)
  | ModifyPowerToughness Quantity Quantity -- layer 7c (Giant Growth +3/+3)
  | SetLandSubtype Subtype -- layer 4, CR 305.7 set (Blood Moon -> Mountain)
  | AddLandSubtype Subtype -- layer 4, CR 305.7 add (Urborg -> Swamp)
  | AddCardType CardType -- layer 4 (Opalescence -> Creature)
  | ChangeSubtypeWord Subtype Subtype -- layer 3, CR 612 (Magical Hack: from -> to)
  | -- layer 2, CR 613.1b: set this object's controller. The PlayerId is BAKED at
    -- effect creation (CR 611.2c) by Resolve.applyEffect (GainControl) -- it is
    -- the effect's source's controller, never chosen. Applied only by
    -- Projection.controllerOf. Never appears in card JSON (runtime-only).
    SetController PlayerId
  deriving (Eq, Ord, Show)
