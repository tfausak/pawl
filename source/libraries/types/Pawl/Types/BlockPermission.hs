module Pawl.Types.BlockPermission where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Quantity as Quantity

-- | CR 509.1a: one printed BLOCKING PERMISSION -- an effect saying a creature
-- can block more creatures than the rule's one. Foriysian Brigade and Lairwatch
-- Giant print it about themselves, High Ground about a whole team, Palace Guard
-- with no number at all, Kemba's Legion with a number it counts off the
-- battlefield.
--
-- The SEVENTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement, Pawl.Types.AttackRequirement,
-- Pawl.Types.CombatRestriction and Pawl.Types.AttackCost. CR 613.11 puts it
-- after the layer system for Pawl.Types.BlockRequirement's reasons, which hold
-- here unchanged: how many creatures a creature may block is not a
-- characteristic, and the subject is an object rather than a player.
--
-- NOT an arm of Pawl.Types.CombatRestriction, and the reason is arithmetic
-- rather than naming. A restriction is a BOUND, and CR 509.1b makes bounds
-- cumulative by taking the TIGHTEST -- two "no more than one creature can block"
-- still allow one. These ADD: two High Grounds let each creature block three
-- attackers, so one reader could not answer both. CantBlockMoreThan is also
-- about the size of the whole declaration, where this is about one creature.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture every sibling takes, so an Equipment leaving lifts the permission it
-- granted. CR 509.1b's note that an evasion ability gained after a legal block
-- does not affect that block is the rules saying the same of the other side.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.BlockPermission is the only module that reads it, and it hands
-- Pawl.Engine.Combat a NUMBER per creature and never a card.
data BlockPermission = MkBlockPermission
  { -- | Which creatures may block the extra creatures. An Affected for
    -- Pawl.Types.BlockRequirement's reason: the pool already names the subject
    -- two ways -- a creature's own text is Affected.Matching Filter.IsSource
    -- (Foriysian Brigade), an Equipment's is Affected.Attached (Echo Circlet).
    affected :: Affected.Affected,
    -- | How many creatures BEYOND CR 509.1a's one this permission adds --
    -- Quantity.Literal 1 for "an additional creature", 7 for Watcher in the Web.
    --
    -- A QUANTITY and not a literal, because Kemba's Legion counts its own
    -- Equipment: "an additional creature each combat for each Equipment attached
    -- to this creature" is a Quantity.Count over the battlefield, re-read on every
    -- look like every other field here, so an Equipment moving away lowers the
    -- arity at once. Evaluated against the permission's SOURCE, which is the "this
    -- creature" every counted printing in the pool means and the same object CR
    -- 109.5 fixes `while`'s "you" by.
    --
    -- NOTHING is "any number of creatures" (Palace Guard) -- no bound at all,
    -- which is Pawl.Engine.CombatRestriction.blockLimit's spelling of the same
    -- word and combines the same way: unbounded absorbs, since a creature that
    -- may block any number still may after a second permission adds one.
    -- Deliberately not a huge literal, which would be a number the card does not
    -- print and would still refuse the board that exceeded it.
    additional :: Maybe Quantity.Quantity,
    -- | CR 604.2's "as long as" clause -- Entourage of Trest's "as long as
    -- you're the monarch". Nothing is the ungated permission (Foriysian
    -- Brigade).
    --
    -- The OPPOSITE polarity to Pawl.Types.CombatRestriction's gate, which is an
    -- "unless": there a gate that HOLDS lifts the restriction, here one that
    -- holds is what grants the permission. Same type, same CR 604.2 re-reading
    -- on every look, and the "you" inside it is CR 109.5's -- the controller of
    -- the permanent printing the sentence.
    while :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
