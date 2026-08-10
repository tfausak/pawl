module Pawl.Types.BlockPermission where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Affected as Affected

-- | CR 509.1a: one printed BLOCKING PERMISSION -- an effect saying a creature
-- can block an additional creature. Foriysian Brigade and Lairwatch Giant print
-- it about themselves, High Ground about a whole team.
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
-- Three printed shapes do not fit and are not carried: "any number of creatures"
-- (Guardian of the Gateless) has no number, a gated one (Entourage of Trest) has
-- no CR 604.2 "as long as" clause here, and a counted one (Kemba's Legion) would
-- need a Quantity where this holds a literal (#1153).
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
    -- | How many creatures BEYOND CR 509.1a's one this permission adds. One for
    -- "an additional creature", seven for Watcher in the Web.
    additional :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
