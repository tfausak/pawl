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
  | -- CR 115.1a / 109.2 / 110.4: "target nonland permanent" -- a type description
    -- without "card"/"spell" means a permanent of that type on the battlefield (CR
    -- 109.2), and land is one of the permanent types (CR 110.4), so this is a
    -- permanent (CR 110.1) whose PROJECTED card types (M3c) do not include Land.
    -- Defined SELF-EXCLUDING
    -- ("another target nonland permanent", Aether Channeler): the exclusion is
    -- applied by Target.legalSetsExcluding, not here (this arm is source-blind). A
    -- non-excluding variant splits the spec when a card needs it (the WallTarget
    -- specific-then-general posture).
    NonlandPermanentTarget
  | -- CR 115.1a: "target nonblack creature" (Doom Blade) -- a creature on the
    -- battlefield whose PROJECTED colours (CR 613 layer 5) do not include black.
    -- Reads the projection, never the mana cost: a devoid creature with {B} in
    -- its cost is nonblack, and a creature made black by a colour-changing effect
    -- is not.
    --
    -- The WallTarget posture: one hand-carved variant, specific before general.
    -- P9's criterion/filter language replaces the whole family of colour- and
    -- type-restricted specs with data (#40).
    NonblackCreatureTarget
  | -- CR 115.1a: "target artifact" -- a permanent on the battlefield whose
    -- PROJECTED card types (M3c layer 4) include Artifact. Reads the
    -- projection, never Card.typeLine: a permanent made an artifact by a
    -- type-changing effect is a legal target and a printed artifact that lost
    -- the type is not. Master Thief's slot.
    --
    -- The WallTarget posture: one hand-carved variant, specific before general.
    -- P9's criterion/filter language replaces the whole family (#40).
    ArtifactTarget
  | -- CR 115.1a with CR 109.5: "target creature an opponent controls" -- a
    -- creature on the battlefield whose PROJECTED controller (CR 613.1b) is not
    -- the targeting source's controller. The first spec whose legal set depends
    -- on WHO IS CHOOSING, which is what makes Pawl.Target source-relative. Hag
    -- of Inner Weakness's slot. Retired with the rest of the family (#40).
    OpponentCreatureTarget
  deriving (Eq, Ord, Show)
