module Pawl.Types.PreventNextDamage where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

-- | CR 615.7's prevention SHIELD: over whom, of what kind, how much, for how
-- long, and CR 615.5's additional effect riding it.

-- Parametric in the EFFECT rather than in the card, which is what keeps this out
-- of a module cycle: Pawl.Types.Effect holds this record and this record holds
-- effects, so naming Effect here would need an hs-boot file. The parameter is
-- instantiated at `Effect card` where the arm is declared.
data PreventNextDamage effect = MkPreventNextDamage
  { duration :: Duration.Duration,
    -- | PRINTED, not assumed -- Decorated Griffin says "the next 1 COMBAT
    -- damage", where Mending Hands says only "damage". Nothing is a shield
    -- naming no kind, taking combat and noncombat alike, and is elided rather
    -- than written null.
    kind :: Maybe DamageKind.DamageKind,
    -- | The recipients this RESOLUTION names -- Mending Hands' "any target" --
    -- one CR 615.7 shield each. Nothing for a shield that describes its
    -- recipients instead, in the two fields below; the two spellings are
    -- alternatives, and a card writing both is read as the description alone
    -- (Pawl.Engine.Resolve's arm). A shield writing NEITHER surrounds nothing
    -- (CR 615.1) and installs no row at all, which Pawl.CardSpec's
    -- shieldNamingNothingOffends rejects.
    ref :: Maybe ObjectRef.ObjectRef,
    -- | CR 611.2c's LIVE description of the OBJECTS one shared shield covers --
    -- Divine Deflection's "permanents you control", which is not a set swept when
    -- the effect is created: a prevention effect modifies neither
    -- characteristics nor controller, so a permanent that comes under your
    -- control afterwards is covered too.
    --
    -- ONE shield over the whole description, never one per object: CR 615.7's
    -- shield "counts only the amount of damage; the number of events or sources
    -- dealing it doesn't matter". CR 615.11's per-creature shields are the other
    -- shape, and the rule scopes itself to a card saying "each", which this
    -- clause does not.
    whatRecipient :: Maybe (Filter.Filter Keyword.Keyword),
    -- | The PLAYER half of that same description -- Divine Deflection's "dealt to
    -- you" -- as CR 109.5's relation, since no Filter describes a player (CR
    -- 120.3a). DISJOINED with the field above on the row this installs, so "you
    -- and/or permanents you control" is one shield covering both.
    whoRecipient :: Maybe PlayerRelation.PlayerRelation,
    -- | CR 609.7a's "by a source of your choice" (Healing Grace), as the
    -- PROPERTIES the chosen source must have. Nothing is a shield naming no
    -- source at all (Mending Hands), which watches every source; `Just` makes the
    -- shield's controller choose ONE source when the effect is created, and
    -- Pawl.Engine.Resolve bakes that id into
    -- Pawl.Types.DamagePattern.whichSource.
    --
    -- The Filter is BOTH halves of CR 615.9: it narrows the candidates offered,
    -- and it is written into DamagePattern.whatSource so CR 609.7b's recheck
    -- happens at the damage event rather than at the choice. Healing Grace says
    -- only "a source", so its Filter is the trivial `And []` -- present, not
    -- absent, because "any source of your choice" is still a choice.
    chosenSource :: Maybe (Filter.Filter Keyword.Keyword),
    quantity :: Quantity.Quantity,
    -- | CR 615.5's additional effect -- Test of Faith's "for each 1 damage
    -- prevented this way, put a +1/+1 counter on that creature". Empty for a
    -- shield with no such clause, which is every other prevention in the pool,
    -- so the key is elided rather than written as an empty array.
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
