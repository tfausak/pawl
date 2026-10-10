module Pawl.Types.PreventAllDamage where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 615.1 / 615.3's UNBOUNDED prevention shield: over whom, of what kind,
-- for how long, and CR 615.5's additional effect riding it.

-- Pawl.Types.PreventNextDamage with the Quantity removed.
--
-- Parametric in the EFFECT rather than in the card, for
-- Pawl.Types.PreventNextDamage's reason: Pawl.Types.Effect holds this record and
-- this record holds effects, so naming Effect here would need an hs-boot file.
-- The parameter is instantiated at `Effect card` where the arm is declared.
data PreventAllDamage effect = MkPreventAllDamage
  { duration :: Duration.Duration,
    -- | PRINTED, not assumed -- Inkshield says "all COMBAT damage", where
    -- Selfless Squire says only "damage". Nothing is a shield naming no kind,
    -- taking combat and noncombat alike, and is elided rather than written null.
    kind :: Maybe DamageKind.DamageKind,
    -- | The objects or players this RESOLUTION names -- Selfless Squire's
    -- "permanents you control", swept as the effect is created. Nothing for a
    -- shield that DESCRIBES its recipients in @whatRecipient@ below instead; on
    -- the DealtTo side the two spellings are alternatives, and a card writing
    -- both is read as the description alone (Pawl.Engine.Resolve's arm) -- but
    -- beside DealtBy this field names the SOURCE and the two are read together,
    -- which is @whatRecipient@'s own note. That alternation is exactly how
    -- Pawl.Types.PreventNextDamage reads its pair. A shield writing NEITHER
    -- covers EVERY recipient (Pay No Heed), the row's Nothing recipient having
    -- meant that all along; it names a SOURCE instead, since a shield naming
    -- neither end is Fog's shape and is authored as an Effect.Replace. That is
    -- Pawl.CardSpec's shieldNamingNothingOffends.
    ref :: Maybe ObjectRef.ObjectRef,
    -- | CR 611.2c's LIVE description of the objects one shared shield covers --
    -- Pack Leader's "to Dogs you control", which is not a set swept when the
    -- effect is created: a prevention effect modifies neither characteristics
    -- nor controller, so a permanent that BECOMES a Dog you control afterwards is
    -- covered too and one that stops being one is not. That is the whole reason
    -- this is not spelled as an @EachMatching@ ref above.
    --
    -- Read on BOTH sides of @direction@: CR 615.1's shield watches a damage
    -- EVENT, so a card may narrow the source end and the recipient end at once --
    -- Goblin Furrier's "prevent all damage that this creature would deal to snow
    -- creatures", printed as a STATIC ability and so carried by
    -- Pawl.Types.PrintedReplacement rather than by this opcode. Beside DealtBy
    -- this description and @ref@ are the two ends rather than two spellings of
    -- one, which Synthetic Selective Muzzle is what proves. What that costs on the DealtTo side
    -- is that the two ARE alternatives there, @ref@ being a recipient too.
    whatRecipient :: Maybe (Filter.Filter Keyword.Keyword),
    -- | The PLAYER half of that same description -- Comeuppance's "dealt to you"
    -- -- as CR 109.5's relation, DISJOINED with @whatRecipient@ on the one row
    -- this installs, as Pawl.Types.PreventNextDamage's field of the same name
    -- is. One row rather than two because CR 615.5's rider runs per application
    -- per source: one source hitting you and your planeswalker at once is one
    -- application of Comeuppance's shield, and Pawl.DamageReplacementSpec's
    -- Comeuppance group proves the rider runs once for it. Read on the DealtTo
    -- side only, like @chosenSource@.
    whoRecipient :: Maybe PlayerRelation.PlayerRelation,
    -- | Which SIDE of the damage event the objects @ref@ names sit on -- the
    -- recipients (Inkshield, Selfless Squire) or the SOURCE (Dovin, Hand of
    -- Control's "and dealt by target permanent"). DealtTo for every shield that
    -- names only a recipient, so the key is elided rather than written on all
    -- but the by-direction.
    direction :: DamageDirection.DamageDirection,
    -- | CR 609.7a's "by a source of your choice" (Auriok Replica), as the
    -- PROPERTIES the chosen source must have, exactly as
    -- Pawl.Types.PreventNextDamage carries it on the countdown shield. Nothing is
    -- a shield naming no source at all (Inkshield), which watches every source;
    -- `Just` makes the shield's controller choose ONE source when the effect is
    -- created, and Pawl.Engine.Resolve bakes that id into
    -- Pawl.Types.DamagePattern.whichSource.
    --
    -- The Filter is BOTH halves of CR 615.9: it narrows the candidates offered,
    -- and it is written into DamagePattern.whatSource so CR 609.7b's recheck
    -- happens at the damage event rather than at the choice. Auriok Replica says
    -- only "a source", so its Filter is the trivial `And []` -- present, not
    -- absent, because "any source of your choice" is still a choice.
    --
    -- A DealtBy @direction@ beside a `Just` here is a contradiction -- that
    -- direction already fills the source half of the row off @ref@ (Dovin, Hand
    -- of Control) -- and Pawl.Engine.Resolve resolves it by asking CR 609.7a's
    -- choice only on the DealtTo side.
    chosenSource :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 609.7b's property-named source: the shield watches every source with
    -- these characteristics, and NOBODY is asked to choose one -- Scarecrow's
    -- "by creatures with flying". @chosenSource@ above is the other question
    -- entirely: that one names the properties CR 609.7a's chooser is held to,
    -- and the shield then watches the ONE object the choice landed on.
    --
    -- `And []` is the trivial predicate, so a shield saying nothing about its
    -- source needs no "any source" arm and the key is elided -- the shape
    -- Pawl.Types.DamagePattern.whatSource takes, which is where
    -- Pawl.Engine.Resolve writes this. Evaluated at the damage event rather than
    -- here, which is what makes CR 609.7b's recheck fall out.
    --
    -- Unlike @chosenSource@ it is read on BOTH sides of @direction@: a property
    -- is a predicate the row can carry beside a DealtBy ref's id, where a CR
    -- 609.7a choice beside one would be naming two different sources. No card in
    -- data\/cards\/ writes it beside DealtBy, so every by-direction row still
    -- gets the trivial predicate.
    whatSource :: Filter.Filter Keyword.Keyword,
    -- | CR 615.5's additional effect -- Brace for Impact's "for each 1 damage
    -- prevented this way, put a +1/+1 counter on that creature". Unlike CR
    -- 615.7's countdown shield this one has no amount to spend, so "the damage
    -- prevented this way" is per APPLICATION rather than a running total. Empty
    -- for a shield with no such clause, so the key is elided rather than written
    -- as an empty array.
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
