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
  | -- | CR 305.2 / Exploration, Azusa Lost but Seeking: this player may play this
    -- many lands each turn OVER the one CR 305.2 normally allows.
    --
    -- An ADDITIONAL amount and not a total, which is what both cards print ("an
    -- additional land", "two additional lands") and what CR 305.2 describes --
    -- continuous effects INCREASE the number, they do not set it. Two of these
    -- therefore add up rather than one winning, and nothing here is a
    -- redundancy question: CR 702.18b's "multiple instances are redundant" is a
    -- rule about a keyword, and CR 305.2 states no such rule.
    --
    -- The amount is CARRIED for CantCastMoreThan's reason: Exploration says one
    -- and Azusa says two, and a card that says three must not need a sibling
    -- constructor. That is also what makes the gate a COUNT rather than a
    -- boolean-plus-one -- see Pawl.Engine.PlayerEffect.landPlaysAllowed.
    --
    -- The "on each of YOUR TURNS" both cards print is not modelled as a turn
    -- restriction, and that is exact rather than a shortcut about Magic:
    -- Pawl.Engine.Action.legalActions gates every land play on being the active
    -- player, unconditionally, so a grant that also applied on another player's
    -- turn is unobservable until something can play a land there at all (#566).
    PlayAdditionalLands Natural.Natural
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
    -- is a distinct mechanism, and it belongs on the CR 613.11 player axis that
    -- this type is.
    --
    -- NOT a Pawl.Types.CastingPermission either: every arm of that type names a
    -- ZONE a card may be cast from (CR 601.3), and this names a TIME.
    --
    -- The filter is CR 601.3b's "a spell with certain qualities", and is the axis
    -- that separates the producers: Vedalken Orrery says "spells" and so matches
    -- everything (`And []`), while Yeva, Nature's Herald says "green creature
    -- spells" and would narrow it.
    --
    -- A PERMISSION, which Pawl.Engine.PlayerEffect folds as a disjunction -- the
    -- same shape the prohibitions take, for the opposite reason. CR 101.2 makes
    -- one applicable prohibition enough because nothing outvotes a "can't"; one
    -- applicable permission is enough because there is nothing for a second to
    -- outvote.
    CastAsThoughItHadFlash (Filter.Filter Keyword.Keyword)
  | -- | CR 701.6a / 613.11 / Spider-Punk: the spells and the abilities on the
    -- stack controlled by the players this effect's scope names can't be
    -- countered.
    --
    -- NOT Pawl.Types.Counterability, and the two are not redundant. That one is
    -- CR 113.6g -- "an object's ability that states IT can't be countered ...
    -- functions on the stack" -- a self-referential ability of the spell itself,
    -- which is why it rides the card (Rending Volley). This one is an ability of
    -- a BATTLEFIELD permanent about OTHER objects, so CR 113.6 leaves it
    -- functioning from the battlefield in the ordinary way and CR 611.1's third
    -- clause makes it a rules-modifying continuous effect. Pawl.Engine.PlayerEffect.applying
    -- walks the battlefield, so it can gather this one and could never gather
    -- the other: a spell on the stack is not a permanent.
    --
    -- BOTH subjects of CR 701.6a at once -- "to counter a spell or ability" --
    -- and one constructor rather than two, because Spider-Punk's one sentence
    -- says both and the Filter below already tells them apart where a card
    -- narrows. CR 113.9 keeps an ability from being a spell for the COUNTERER's
    -- sake (a Stifle must not reach a spell), which is the other side of the
    -- question and is not what this constructor answers.
    --
    -- WHOSE spells is the carrier's scope and not this constructor's, exactly as
    -- it is for every other arm here: Spider-Punk says "spells and abilities"
    -- with no possessive (PlayerScope.EachPlayer), while Prowling Serpopard says
    -- "you control" (PlayerScope.You).
    --
    -- WHICH spells is the Filter, the same shape IncreaseSpellCost and
    -- CastAsThoughItHadFlash carry and read through the same
    -- Pawl.Engine.PlayerEffect.matchesSpell. Spider-Punk narrows by nothing and
    -- so writes `And []`; Prowling Serpopard's "creature spells" writes
    -- HasCardType Creature.
    --
    -- The filter is read against BOTH of CR 701.6a's subjects and gets to decide
    -- for itself whether it reaches an ability, because an ability on the stack
    -- has no card behind it and so no characteristics: `And []` matches it, and
    -- any atom naming a quality does not. That is why Spider-Punk still stops a
    -- Stifle and Prowling Serpopard does not -- neither the type nor the engine
    -- states a rule about abilities, the empty predicate simply happens to be
    -- true of one.
    CantBeCountered (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
