module Pawl.Types.SpecialAction where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

-- | CR 116.2: a special action a face's printed text grants -- something a
-- player may do with priority that does not use the stack. WHICH player is the
-- constructor's own question: CR 116.2e's is the card's holder, CR 116.2d's is
-- anyone.
--
-- Named for the OPERATION and never for the card. CR 116.2e is one card by name,
-- but the engine must not learn that name: Pawl.Engine.Action reads this
-- permission off the card data and Pawl.Types.Action carries the rulebook's own
-- vocabulary, so neither half cases on an identity.
--
-- The carrier for the rows of CR 116.2 a card grants IN PROSE. The rules grant
-- CR 116.2a's land play to every player, so it needs no printed permission; CR
-- 116.2f, CR 116.2h and CR 116.2k are granted by KEYWORDS (suspend, foretell,
-- plot), so they belong to Pawl.Types.Keyword rather than here whenever those
-- land; and CR 116.2c's producers are the twelve Licids, which need a creature
-- to animate itself into an Aura first (#875).
data SpecialAction
  = -- | CR 116.2e: "You may discard this card any time you could cast an
    -- instant." Circling Vultures is the card the rule names.
    --
    -- The timing is NOT carried, because CR 116.2e's own last sentence overrides
    -- the printed wording: "a player can take such an action any time they have
    -- priority". So the permission is unconditional and nothing reads casting
    -- timing to offer it.
    DiscardThisAnyTime
  | -- | CR 116.2d: "some effects from static abilities allow a player to take an
    -- action to ignore the effect from that ability for a duration". Leonin
    -- Arbiter's "any player may pay {2} for that player to ignore this effect
    -- until end of turn" is one; Damping Engine, Volrath's Curse and Lost in
    -- Thought are the only others printed.
    --
    -- WHICH effect is named by the SOURCE PERMANENT and not by an index into
    -- Face.playerAbilities, so taking this ignores every player-axis static
    -- ability the permanent has. Exact for all four producers, none of which
    -- grants the permission on more than one ability (#1139).
    --
    -- The DURATION is not carried: all four print "until end of turn", and CR
    -- 116.2d states only "for a duration" rather than fixing one, so a card that
    -- printed another would want a Pawl.Types.Duration here.
    --
    -- WHO may take it is not carried either, and that is the axis on which the
    -- other three producers differ from this one: Leonin Arbiter says "any
    -- player", Volrath's Curse says "that creature's controller" and Damping
    -- Engine says "that player" of a condition no filter here could state
    -- (#1139).
    --
    -- The COST is carried, since it is what the printed sentence varies most
    -- freely -- {2}, sacrificing a permanent, exiling three cards from a
    -- graveyard. Paid through Pawl.Engine.Cost like every other, and never
    -- totalled through CR 601.2f: a special action is neither a spell being cast
    -- nor an ability being activated.
    IgnoreThisUntilEndOfTurn (Cost.Cost Keyword.Keyword)
  deriving (Eq, Ord, Show)
