module Pawl.Types.Affected where

import qualified Data.Set as Set
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId

-- | What a continuous effect applies to. CR 611.2c: a resolution effect's set is
-- LOCKED when it begins (TheseObjects -- a bounced-and-returned creature is a new
-- id the effect no longer names). A static ability's set is DYNAMIC: Matching and
-- MatchingAnywhere (any object currently matching a Filter, on the battlefield or
-- in any projected zone), Attached (the one object the source is attached to, if
-- any) and AttachedPlayerControls (what the enchanted PLAYER controls) -- all
-- four are re-derived each projection, never captured once.
--
-- The one crossing between the two is StaticAbility.lingers: an ability whose
-- text keeps its effect alive past its own permanent hands a STORED effect over
-- as the permanent leaves, and Pawl.Engine.Projection.frozenStaticParts resolves
-- its dynamic set to a TheseObjects at that instant -- CR 611.2c, which now
-- governs it.
data Affected
  = -- | CR 611.2c: a fixed id set, NOT a predicate.
    TheseObjects (Set.Set ObjectId.ObjectId)
  | -- | Dynamic: any object matching the Filter, re-derived each projection
    -- against the PARTIAL projection accumulated so far, so it reads each axis as
    -- of whichever layers have already applied (CR 613) -- a layer-4 type change
    -- is visible to a later layer. A card's own "each other" text is
    -- Filter.Not Filter.IsSource inside the Filter rather than a separate field.
    Matching (Filter.Filter Keyword.Keyword)
  | -- | Matching, without the battlefield gate. Painter's Servant's "all cards
    -- that aren't on the battlefield, spells, and permanents" is the first
    -- affected set in the pool that is not scoped to the battlefield, and the
    -- ONLY reason this is a separate arm rather than a zone payload on Matching:
    -- every other card in the pool depends on that gate, since Bad Moon's "black
    -- creatures get +1/+1" must not reach a creature card in a graveyard.
    --
    -- The set the CR describes is every object in every zone, and both readers
    -- now answer that way: Projection.viewOfObject has never had a zone gate, and
    -- Projection.viewUpTo -- the reader inside the CR 613 fold -- projects a card
    -- in a hand, library, graveyard or exile rather than falling back to its
    -- printed characteristics. Pawl.ProjectionSpec's Maskwood Nexus pair is what
    -- proves the second.
    --
    -- Not implemented, the symmetric OVER-reach, recorded here because the card's
    -- JSON cannot carry a comment: Painter's own filter is And [], which matches
    -- EVERY object, while the card scopes to three kinds of thing. So this also
    -- reaches an ability on the stack (CR 113.1c) and an emblem in the command
    -- zone (CR 114.5). Neither is observable today (#1551).
    MatchingAnywhere (Filter.Filter Keyword.Keyword)
  | -- | Matching with the battlefield gate INVERTED: any object matching the
    -- Filter that is not on the battlefield. Teferi, Mage of Zhalfir's "creature
    -- cards you own that aren't on the battlefield" is the pool's first such
    -- clause standing alone, which is why it is an arm of its own: Maskwood
    -- Nexus and Arcane Adaptation print the same words BESIDE a battlefield
    -- clause, so their two sentences together are exactly MatchingAnywhere.
    --
    -- Reaches every zone but the battlefield -- a hand, a library, a graveyard,
    -- exile, the command zone and the stack -- which is what the printed phrase
    -- says. CR 400.1 lists the zones; nothing narrows the phrase further.
    MatchingOffBattlefield (Filter.Filter Keyword.Keyword)
  | -- | CR 303.4m: the object this ability's SOURCE is attached to -- "enchanted
    -- creature". Neither a fixed id set nor a predicate over candidates:
    -- TheseObjects is fixed at resolution (CR 611.2c) and Matching /
    -- MatchingAnywhere are predicates re-derived per candidate, while this is
    -- re-derived from the SOURCE's own state.
    --
    -- The set is {o} when the source is attached to o, and EMPTY when it is
    -- unattached -- an Aura in the graveyard, or one the CR 704.5m sweep has not
    -- reached yet. Payload-free: CR 303.4m defines it for any permanent, Aura or
    -- not, so there is nothing to parameterize.
    Attached
  | -- | CR 303.4b / 303.4m: the objects matching the Filter that the ENCHANTED
    -- PLAYER controls -- Curse of Death's Hold's "creatures enchanted player
    -- controls". The player-side twin of Attached, and a separate arm rather
    -- than a payload on it because the two name different things: Attached names
    -- the attached-to object ITSELF, while an enchant-player Aura (CR 702.5d) is
    -- attached to no object at all and reaches the battlefield only through the
    -- player it enchants.
    --
    -- Dynamic in BOTH of its inputs, and re-derived each projection: the source's
    -- attachment can change, and so can who controls a candidate (CR 613.1b's
    -- layer 2 applies before the layers this set feeds).
    --
    -- The set is EMPTY when the source is unattached or attached to an object
    -- rather than a player.
    --
    -- Filtered rather than payload-free, unlike Attached: "creatures enchanted
    -- player controls" narrows by card type, where CR 303.4m's phrase names only
    -- whose permanents they are.
    AttachedPlayerControls (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
