module Pawl.Types.PlayerEffect where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | CR 611.1's third clause: a continuous effect affecting players or the rules
-- of the game rather than the characteristics of an object. The player analogue
-- of Pawl.Types.Modification, and NOT a member of it: CR 613.1's layers compute
-- an OBJECT's characteristics, while CR 613.10 and 613.11 apply these AFTER that
-- machine has run. There is no Layer constructor here and Pawl.Engine.Projection
-- never sees this type.
--
-- Open-half card data. Pawl.Engine.PlayerEffect is the ONLY module that may case
-- on it.
data PlayerEffect
  = -- | CR 601.3 / Silence: this player can't cast spells at all.
    CantCastSpells
  | -- | CR 601.3 / Rule of Law: this player can't cast more than this many spells
    -- each turn. The limit is carried, not hardcoded: Rule of Law and Arcane
    -- Laboratory both say one, and a card that says two must not need a sibling
    -- constructor.
    CantCastMoreThan Natural.Natural
  | -- | CR 601.3 / Null Chamber: this player can't cast a spell whose name is one
    -- of the names chosen as this effect's SOURCE entered ("Spells with the
    -- chosen names can't be cast").
    --
    -- NULLARY, carrying no name, for UnderSourceControl's reason on the entry
    -- side: a card can write a CardName (its own face's, a token definition's)
    -- but not THIS one, which is not known until CR 614.1c's choice is made --
    -- the same reason that arm carries no PlayerId. The names come from
    -- Object.chosenNames on the source instead, which is how
    -- Modification.AddChosenColor already reads a colour.
    --
    -- The quality is the SPELL's name, which makes this CR 601.3a's shape rather
    -- than CantCastSpells' -- see Pawl.Engine.PlayerEffect.prohibitsCasting for
    -- what that costs and what is still missing (#95).
    CantCastChosenName
  | -- | CR 305.1 / Null Chamber: this player can't PLAY a land whose name is one
    -- of the names chosen as this effect's source entered ("lands with the
    -- chosen names can't be played").
    --
    -- The sibling of CantCastChosenName above, and deliberately not folded into
    -- it even though one printed sentence carries both: CR 305.1 makes playing a
    -- land a special action that never uses the stack, so a land is never cast
    -- and the two prohibitions are read by two different gates
    -- (Pawl.Engine.Action.playableLands, Pawl.Engine.Cast.castable). A card
    -- printing only one half (Nevermore prints only the cast half) declares only
    -- one arm.
    CantPlayLandChosenName
  | -- | CR 613.11 / 601.2f / Thalia: matching spells cost this much more generic
    -- mana to cast.
    IncreaseSpellCost (Filter.Filter Keyword.Keyword) Natural.Natural
  | -- | CR 613.11 / 601.2f / Sapphire Medallion, Edgewalker: matching spells cost
    -- this much less to cast.
    --
    -- A SEPARATE constructor from IncreaseSpellCost, never one signed delta. The
    -- rules distinguish them in two ways a signed integer cannot express: CR
    -- 601.2f applies every increase BEFORE any reduction, and CR 118.7a confines
    -- a reduction by GENERIC mana to the generic component, a restriction an
    -- increase does not have.
    --
    -- An AMOUNT OF MANA rather than a bare number, because CR 118.7 reduces by
    -- mana of a stated type and not only by generic: the Medallion's {1} and
    -- Edgewalker's {W}{B} are the same shape of thing.
    -- Pawl.Engine.Cost.applyAdjustments reads it component by component.
    --
    -- An EXCESS typed symbol is dropped rather than spilling onto the generic
    -- component, which is Edgewalker's "This effect reduces only the amount of
    -- colored mana you pay" and not CR 118.7b-d (#309).
    ReduceSpellCost (Filter.Filter Keyword.Keyword) ManaCost.ManaCost
  | -- | CR 402.2 / Reliquary Tower: this player has no maximum hand size.
    NoMaximumHandSize
  | -- | CR 500.5 / 703.4q / Upwelling, Omnath Locus of Mana: this player does not
    -- lose the mana the filter names, out of the unspent mana in their mana pool,
    -- as a step or phase ends.
    --
    -- CR 106.4 supplies the verb -- a player LOSES the mana their pool empties --
    -- which is the wording modern Oracle text uses, and why this is stated as a
    -- player-axis effect rather than as a property of the pool.
    --
    -- The filter is what separates the two producers, on the axis this
    -- constructor owns: Upwelling keeps every type of mana (ManaFilter.Any) and
    -- Omnath keeps only green (ManaFilter.OfType). WHOSE mana is the other axis
    -- and is the carrier's, not this constructor's -- Upwelling is
    -- PlayerScope.EachPlayer and Omnath is PlayerScope.You.
    --
    -- Shizuko and Karn, Legacy Reforged keep only the mana they just added, which
    -- is not a player-axis property at all and so is not reachable by widening
    -- this filter (#352).
    DontLoseUnspentMana ManaFilter.ManaFilter
  | -- | CR 702.18a / 702.11c: this player can't be the target of spells or
    -- abilities controlled by the players the scope names. Ivory Mask ("You have
    -- shroud") and Leyline of Sanctity ("You have hexproof") are the two
    -- producers.
    --
    -- The PLAYER halves of two keywords, which is why they are here and not in
    -- Pawl.Types.Keyword: rule 702's keywords live on objects and fold through
    -- the CR 613.1-613.7 layers, and a player has no characteristics for that
    -- machine to compute. CR 613.10/613.11 is the player axis.
    --
    -- ONE constructor carrying a PlayerScope rather than a HasShroud and a
    -- HasHexproof, because the two rules differ in exactly the set of players
    -- whose spells are stopped and in nothing else: CR 702.18a names no player,
    -- so the protected player's own spells are stopped too (EachPlayer), while
    -- CR 702.11c stops only opponents' (Opponents).
    --
    -- The scope is read against the PROTECTED player -- CR 702.11c's "you" --
    -- and NOT against the effect's controller, which is the anchor
    -- PlayerStaticAbility.scope uses. The two coincide for both cards in the pool
    -- and would come apart for a card that gave an OPPONENT hexproof.
    --
    -- Both scopes have a producer. PlayerScope.You is the third value and has
    -- none: it would mean only your own spells can't target you, which no rule
    -- 702 keyword states.
    --
    -- Redundancy is not counted, and CR 702.18b and CR 702.11h say so for the
    -- PLAYER case and not only the permanent one. A membership question, never a
    -- tally.
    CantBeTargetedBy PlayerScope.PlayerScope
  | -- | CR 601.3b / Vedalken Orrery: this player may cast a matching spell as
    -- though it had flash -- which by CR 702.8a and CR 117.1a's first sentence
    -- means any time they have priority.
    --
    -- NOT Pawl.Types.Keyword.Flash and not a second producer of it. CR 702.8a's
    -- flash is a static ability an object has about casting ITSELF ("the card
    -- it's on"), and Vedalken Orrery gives itself nothing. That the rules spend
    -- CR 601.3b, 601.3c and 601.3d on "as though it had flash" is the point: it
    -- is a distinct mechanism, and it belongs on the CR 613.11 player axis this
    -- type is.
    --
    -- NOT a Pawl.Types.CastingPermission either: every arm of that type names a
    -- ZONE a card may be cast from (CR 601.3), and this names a TIME.
    --
    -- The filter is CR 601.3b's "a spell with certain qualities", and is the axis
    -- that separates the producers: Vedalken Orrery says "spells" and so matches
    -- everything (`And []`), while Yeva, Nature's Herald says "green creature
    -- spells" and would narrow it.
    --
    -- A PERMISSION, so Pawl.Engine.PlayerEffect reads it as a disjunction, unlike
    -- the CR 101.2 disjunction the prohibitions fold as -- there being nothing
    -- for a second permission to outvote.
    CastAsThoughItHadFlash (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
