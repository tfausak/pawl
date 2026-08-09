module Pawl.Types.DamageRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scaling as Scaling

-- | CR 614.1a / 615.1: how a replacement or prevention effect rewrites a damage
-- event. PreventAll cancels it outright -- CR 615.6, a prevented event never
-- happens. Two producers, and they differ only in what the pattern beside this
-- rewrite says: Fog authors one on the card and shields nobody in particular,
-- while Effect.PreventAllDamage bakes one over a named recipient (Selfless
-- Squire).
--
-- CR 615.1a is what makes the two Prevent arms below a different KIND of rewrite
-- from the two under them, rather than merely a different amount: an effect that
-- uses the word "prevent" is a prevention effect, so only these two prevent
-- anything, and only these two fire CR 615.13's triggers.
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
-- name an ObjectId, so Resolve's RedirectDamage arm is the one producer.
--
-- Not implemented: a redirect with a remaining AMOUNT, PreventNext's counted
-- twin -- Harm's Way's "the next 2 damage ... is dealt to any target instead"
-- (#1098).
data DamageRewrite
  = PreventAll
  | PreventNext Natural.Natural
  | SetAmount Natural.Natural
  | Scale Scaling.Scaling
  | Redirect Recipient.Recipient
  deriving (Eq, Ord, Show)
