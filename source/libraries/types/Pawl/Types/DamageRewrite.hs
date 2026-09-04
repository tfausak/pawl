module Pawl.Types.DamageRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scaling as Scaling

-- | CR 614.1a / 615.1: how a replacement or prevention effect rewrites a damage
-- event. PreventAll cancels it outright -- CR 615.6, a prevented event never
-- happens. Two producers, and they differ only in what the pattern beside this
-- rewrite says: Fog authors one on the card and shields nobody in particular,
-- while Effect.PreventAllDamage bakes one over a named recipient (Selfless
-- Squire).
--
-- CR 615.1a is what makes the Prevent arms below a different KIND of rewrite from
-- the ones under them, rather than merely a different amount: an effect that uses
-- the word "prevent" is a prevention effect, so only those prevent anything, and
-- only they fire CR 615.13's triggers.
-- Pawl.Engine.Replacement.prevents is that classification.
--
-- PreventNext is CR 615.7's shield (Mending Hands), and its Natural is the
-- REMAINING amount, rewritten in place on the row that carries it and dropped
-- once it reaches 0. It lives here rather than as a counted arm of
-- Pawl.Types.Uses because CR 615.7 counts only the amount of damage, not the
-- number of events: a counted Uses would spend a shield partially covering a
-- 5-damage event as though it had covered the whole of it. Engine-baked, never
-- authored, since a shield names the permanent or player it shields, chosen at
-- resolution; Effect.PreventNextDamage is the one producer.
--
-- SetAmount is CR 614.1a's "instead" with a flat number (Galvanic Blast). A
-- Natural rather than a Quantity because every printed instead-amount in the pool
-- is a literal, and a variable one would need the whole quantity-evaluation
-- environment inside the CR 616.1 loop, which nothing asks for yet.
--
-- Scale is Furnace of Rath's "double that damage ... instead", reusing
-- Pawl.Types.Scaling rather than a Double arm -- the difference between doubling
-- and tripling is a number, and CounterR and TokenR speak the same vocabulary.
--
-- Redirect is CR 614.9's redirection effect (Turn the Tables): the event's
-- RECIPIENT is replaced and nothing else is. A rule-614 replacement, not a rule
-- 615 prevention -- it never says "prevent" (CR 615.1a) -- so `prevents` refuses
-- it and CR 615.13's trigger never sees it. Its Recipient is engine-baked and
-- never authored, exactly as DamagePattern.whichRecipient is: card data cannot
-- name an ObjectId, so Resolve's RedirectDamage arm is the one producer, and
-- Pawl.CardSpec's engineOnlyOffends is what keeps the corpus off it.
--
-- RedirectMatching is the CARD-PRINTED half of that same question, and the two
-- are not one arm for DamagePattern's `whichRecipient` / `whatRecipient`
-- reason: Redirect names an id the engine baked, where this one DESCRIBES the
-- destination by characteristic -- Pariah's "is dealt to enchanted creature
-- instead" is `Filter.IsHostOfSource`, and a permanent redirecting to itself
-- (Palisade Giant) would write `Filter.IsSource`.
data DamageRewrite
  = PreventAll
  | -- | CR 122.1c: "if damage would be dealt to this permanent, prevent that
    -- damage and remove a shield counter from it". A prevention (it says
    -- "prevent", CR 615.1a), so `prevents` admits it and CR 615.13's trigger sees
    -- it.
    --
    -- A separate arm from PreventAll rather than PreventAll plus CR 615.5's
    -- rider, because the counter removal is not an additional EFFECT: rule 122.1c
    -- is one sentence of the rulebook, so what it removes is fixed and needs
    -- neither Pawl.Types.PreventionRider's snapshotted targets nor its
    -- controller. So it needs none of DamageR.riders either: this pair is
    -- minted rather than printed, and a rule's own removal is not a card's
    -- additional effect.
    --
    -- Prevents the WHOLE event for one counter, whatever its amount: unlike CR
    -- 615.7's PreventNext below, rule 122.1c counts events rather than damage, so
    -- one shield counter covers one damage event of any size.
    PreventRemovingShieldCounter
  | PreventNext Natural.Natural
  | -- | CR 615.10: a static prevention that leaves this much of each applicable
    -- damage event standing and prevents the rest -- Temple Altisaur's "prevent
    -- all but 1 of that damage".
    --
    -- A FLOOR on what survives, and thereby the opposite of PreventNext's
    -- ceiling on what is stopped: the rule's shield "will apply separately to
    -- damage from other applicable events", so nothing is written back and
    -- Pawl.Engine.Replacement.contestedResource gives it no supply.
    --
    -- Not SetAmount with the same number, which is what makes this its own arm:
    -- that one never says "prevent", so CR 615.1a would classify it out of
    -- `prevents` and out of CR 615.13's trigger. The two also disagree on an
    -- event SMALLER than the number -- SetAmount would raise a 1 to a 3.
    PreventAllBut Natural.Natural
  | SetAmount Natural.Natural
  | Scale Scaling.Scaling
  | Redirect Recipient.Recipient
  | -- | CR 614.9's redirection with a REMAINING amount, PreventNext's counted twin
    -- -- Harm's Way's "the next 2 damage ... is dealt to any target instead".
    -- The Natural is rewritten in place as it is spent and the row dropped at 0,
    -- CR 615.7's shape; a redirect, not a prevention (CR 615.1a), so `prevents`
    -- refuses it as it does Redirect. Engine-baked for Redirect's reason.
    RedirectNext Natural.Natural Recipient.Recipient
  | -- | CR 614.9's redirection with a PRINTED destination: the damage is dealt
    -- instead to the one permanent on the battlefield this Filter describes.
    --
    -- A Filter rather than an identity enum beside it, for the reason
    -- DamagePattern's `whatRecipient` gives and #163 states: "the enchanted
    -- creature" and "this permanent" are one relation asked in two directions,
    -- and Pawl.Types.Filter already spells both (IsHostOfSource, IsSource).
    --
    -- The rule redirects to ONE recipient -- "the same damage dealt to another
    -- battle, creature, planeswalker, or player" -- so the destination is the
    -- UNIQUE match, and a filter admitting none or several redirects nowhere:
    -- CR 614.9's "the effect does nothing", which
    -- Pawl.Engine.Replacement.printedDestination answers. Nothing in this type
    -- stops a card describing several; the pool's one destination is
    -- Filter.IsHostOfSource, which cannot.
    --
    -- A PLAYER destination has no spelling here: a Filter describes objects and
    -- CR 120.3's other kind of recipient is not one, which is why DamagePattern
    -- splits its printed recipient across `whatRecipient` and `whoRecipient`
    -- rather than widening the Filter.
    --
    -- Nothing is lost by it today. The printings that redirect damage to a
    -- player -- Blood of the Martyr, Sivvi's Valor and Vassal's Duty, Scryfall
    -- `o:/dealt to you instead/`, 2026-08-28 -- are all one-shot RESOLUTIONS, so
    -- their destination is baked by Effect.RedirectDamage into the Redirect
    -- above. A card that printed a static one on a permanent would refute this,
    -- and would want a player half here the way DamagePattern has one.
    RedirectMatching (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
