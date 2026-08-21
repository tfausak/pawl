module Pawl.Types.AttackCostScope where

-- | WHICH ANNOUNCEMENTS a printed cost to attack covers -- the object axis of
-- Pawl.Types.AttackCost, read against CR 508.1b's announcement.
--
-- Two of the three arms say what the cost PROTECTS, and they split two ways and
-- no further. Ghostly Prison's unqualified "you" is CR 109.5's player and nothing
-- else, which its own ruling spells out: "a creature that can't attack you can
-- still attack a planeswalker you control." Baird, Steward of Argive, Norn's
-- Annex, Sphere of Safety and Archangel of Tithes all print "you or
-- planeswalkers you control", which is that ruling's "unless some effect
-- explicitly says otherwise" -- and CR 306.6, which makes a planeswalker a
-- separate thing to attack, is why the wide arm is a
-- second answer rather than a consequence of the first.
--
-- NOT a Filter. What is protected is a player together with the permanents that
-- player controls, which is not a statement about an object's characteristics,
-- and CR 508.1b's announcement arrives as a Pawl.Types.AttackTarget rather than
-- as a candidate to be matched.
--
-- The third arm protects nothing at all. Oppressive Rays taxes the creature it
-- enchants and Exalted Dragon taxes itself, rather than either of them protecting
-- a player it is pointed at, so such a sentence has no "you" to resolve and no
-- announcement it lets through. That is a different KIND of
-- printing rather than a wider protection, which is why it cannot be spelled as
-- either arm above: under both of those the cost is judged against the source's
-- controller, and an Oppressive Rays taxes an attack on anyone, its own
-- controller included.
--
-- No arm for a battle under either PROTECTING scope, and that is a rule rather
-- than an omission: CR 310.9b makes a battle attackable by any player for whom
-- its protector is a defending player, so attacking a battle is not attacking
-- that protector.
--
-- Bounded and Enum for Pawl.Codec.AttackCostScope, which derives its arm list
-- from the type.
data AttackCostScope
  = -- | Ghostly Prison, Propaganda, Windborn Muse, Collective Restraint.
    Controller
  | -- | Baird, Steward of Argive, Norn's Annex, Sphere of Safety, Archangel of
    -- Tithes.
    ControllerAndPlaneswalkers
  | -- | Oppressive Rays, Exalted Dragon. Every announcement is taxed, whatever
    -- CR 508.1b named and whoever controls the taxing permanent -- so this arm
    -- reads no controller, where both arms above must have one.
    EveryAttack
  deriving (Bounded, Enum, Eq, Ord, Show)
