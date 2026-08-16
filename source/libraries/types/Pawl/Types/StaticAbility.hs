module Pawl.Types.StaticAbility where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Modification as Modification

-- | A card's printed static continuous ability (CR 604.1/604.2: a static ability
-- creates a continuous effect active while its permanent is on the battlefield).
-- Gathered live from every battlefield permanent by the projection, with the
-- permanent's own timestamp (CR 613.7a: a static ability's continuous effect has
-- the same timestamp as the object it is on).
--
-- One affected set, MANY modifications, because CR 613.6 makes that the unit the
-- layer system reasons about: an effect applying in several layers keeps applying
-- to the same set of objects in each. Humility is ONE ability whose parts land in
-- layers 6 and 7b, and its set is chosen once; declaring it as two abilities let
-- the projection ask the filter once per layer and get two answers (#233).
--
-- NonEmpty rather than a list validated at the boundary: an ability with no parts
-- does nothing, which no card means, and a malformed `"modifications": []` then
-- fails to decode rather than quietly producing a permanent that under-performs
-- its own text.
--
-- Its order is the card's PRINTED order, not the application order --
-- Projection.layer decides that, per CR 613.1.
-- Parametric in `card` only so that a modification can grant a whole quoted
-- ability -- see the note on Pawl.Types.Modification. Pawl.Types.Face ties the
-- knot at `StaticAbility card` alongside its own.
data StaticAbility card = MkStaticAbility
  { affected :: Affected.Affected,
    -- | The ability's "as long as" clause -- Kird Ape's "as long as you control
    -- a Forest" -- or Nothing for an ability that functions unconditionally,
    -- which is most of them.
    --
    -- A SECOND gate, on top of the one CR 604.2 already states: that rule keeps
    -- the effect active while the permanent is on the battlefield and has the
    -- ability, which Projection.gatherStatic applies by walking the battlefield,
    -- and this narrows it further. CR 604.1 is why it can be nothing more than a
    -- predicate over game state -- a static ability is "simply true", so there is
    -- no moment at which the clause is checked and latched.
    --
    -- NOT a duration, and the distinction is CR 611.2c's parenthetical: a "for as
    -- long as" duration (CR 611.2b, Pawl.Types.Duration.ForAsLongAs) ENDS a stored
    -- effect that a resolution created, once and for good, while this gate is
    -- re-asked on every projection (CR 613.5) and so turns the same effect off and
    -- on again as the board moves, with no resolution and no trigger in between.
    condition :: Maybe Condition.Condition,
    -- | Titania's Song's second sentence: "If this enchantment leaves the
    -- battlefield, this effect continues until end of turn." Nothing -- almost
    -- every ability -- is CR 604.2 as written, the effect ending with the
    -- permanent; Just d is the card overriding that with a duration of its own.
    --
    -- The handover is Pawl.Engine.Event's, at the moment the permanent leaves,
    -- and what it hands over is a STORED effect (CR 611.2). So the clause moves
    -- the ability's effect from one side of CR 611.2c to the other: the live
    -- `affected` set below is resolved to the concrete objects it names right
    -- then, and never re-derived again.
    --
    -- A Duration and not a Bool, because the clause states one in words the
    -- printed vocabulary already has -- "until end of turn" is
    -- Duration.UntilEndOfTurn, the same value a spell would print. A card
    -- naming a different one needs no new field.
    lingers :: Maybe Duration.Duration,
    modifications :: NonEmpty.NonEmpty (Modification.Modification (ActivatedAbility.ActivatedAbility card))
  }
  deriving (Eq, Ord, Show)
