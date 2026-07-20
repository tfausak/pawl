module Pawl.Type.TargetSpec where

-- What a target slot may legally hold. Classification data, never a predicate
-- function. AnyTarget is "any target": a creature or a player (CR 115.4's
-- damageable set, minus the card types that do not exist yet -- planeswalkers
-- and battles grow this).
data TargetSpec
  = AnyTarget
  | -- CR 115.1a: "target creature" -- a creature on the battlefield, no players.
    -- The first spec whose legal set can be EMPTY, which falsifies M3a's
    -- CR 601.2c targeting gate (Giant Growth with no creature is uncastable).
    CreatureTarget
  | -- CR 115: "target spell or permanent" -- any object on the stack, or any
    -- permanent on the battlefield. The first target that reaches the stack.
    SpellOrPermanentTarget
  | -- A land permanent on the battlefield (projected card-type Land). Used by the
    -- M3d fixture "target land becomes ...".
    LandTarget
  | -- CR 115: "target player" -- a player still in the game. The players-only
    -- restriction AnyTarget does not express (Mindslaver, M3g).
    PlayerTarget
  | -- CR 115: "target creature or enchantment" (Angelic Edict). The first spec
    -- admitting a non-creature permanent -- named as ToObject, like LandTarget.
    CreatureOrEnchantmentTarget
  | -- CR 115: "target spell" -- an object on the stack that is a spell (a card on
    -- the stack, CR 112.1). Narrower than SpellOrPermanentTarget: Cancel cannot
    -- target a permanent or an ability. The first spec that reaches ONLY the stack.
    SpellTarget
  | -- CR 115.1a / 700.2c: "target Wall" (Chaos Charm) -- a creature whose PROJECTED
    -- subtypes (M3c) include Wall. A specific subtype-restricted spec (the LandTarget
    -- posture); a general "target <subtype>" is future.
    WallTarget
  | -- CR 115 / 305.1: "target nonland permanent" -- a permanent on the battlefield
    -- whose PROJECTED card types (M3c) do not include Land. Defined SELF-EXCLUDING
    -- ("another target nonland permanent", Aether Channeler): the exclusion is
    -- applied by Target.legalSetsExcluding, not here (this arm is source-blind). A
    -- non-excluding variant splits the spec when a card needs it (the WallTarget
    -- specific-then-general posture).
    NonlandPermanentTarget
  deriving (Eq, Ord, Show)
