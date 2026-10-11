module Pawl.Types.PlayerEffect where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AlternativeActivationCost as AlternativeActivationCost
import qualified Pawl.Types.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.Types.CastFromZone as CastFromZone
import qualified Pawl.Types.CostModifier as CostModifier
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ModifiedRoll as ModifiedRoll
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlotFromZone as PlotFromZone
import qualified Pawl.Types.SpendManaAsThough as SpendManaAsThough
import qualified Pawl.Types.StatedFlip as StatedFlip

-- | CR 611.1's third clause: a continuous effect affecting players or the rules
-- of the game rather than the characteristics of an object. The player analogue
-- of Pawl.Types.Modification, and NOT a member of it: CR 613.1's layers compute
-- an OBJECT's characteristics, while CR 613.10 and 613.11 apply these AFTER that
-- machine has run. There is no Layer constructor here.
--
-- Open-half card data. Pawl.Engine.PlayerEffect is the only module that may case
-- on it to ask what an effect MEANS. Pawl.Engine.Projection cases on it in
-- rewritePlayerEffect alone, and only for CR 612.1's word swap, which walks the
-- STRUCTURE for a Filter and asks nothing else -- the same standing rewriteEffect
-- has over Pawl.Types.Effect.
data PlayerEffect
  = -- | CR 601.3 / Silence: this player can't cast spells at all.
    CantCastSpells
  | -- | CR 602.5 / Sen Triplets: this player can't activate abilities at all.
    --
    -- The player-axis twin of CR 701.35a's detain, which stamps one OBJECT
    -- (Pawl.Types.Object.detainedUntil), and of Pawl.Types.ActivationProhibition,
    -- which is the printed sentence aimed at one: this names nobody's permanent
    -- and refuses every activation the player would make. A MANA ABILITY too,
    -- which is why detain is the twin rather than split second (CR 702.61b):
    -- the sentence carves nothing out, so
    -- Pawl.Engine.Cost.manaActivationsGiven reads it beside detain for CR
    -- 605.3a's windows.
    --
    -- Nothing is every ability; Just a designator narrows it to the abilities
    -- under that rule-702 keyword (Pawl.Types.ActivatedAbility.keyword), Kang the
    -- Conqueror's "power-up abilities can't be activated".
    CantActivateAbilities (Maybe (KeywordDesignator.KeywordDesignator Keyword.Keyword))
  | -- | CR 601.3 / Rule of Law: this player can't cast more than this many spells
    -- each turn.
    CantCastMoreThan Natural.Natural
  | -- | CR 613.11 / 601.2f / 602.2b / Thalia, Heartstone, Drought: matching
    -- spells or activated abilities cost more, less or an additional cost.
    ModifyCost CostModifier.CostModifier
  | -- | CR 118.9 / 602.2b / Kíli the Resourceful: this player may pay this cost
    -- rather than the activation cost of a matching keyword ability.
    AlternativeActivationCost AlternativeActivationCost.AlternativeActivationCost
  | -- | CR 305.2 / Exploration, Azusa Lost but Seeking: this player may play this
    -- many lands each turn OVER the one CR 305.2 normally allows.
    --
    -- The "on each of YOUR TURNS" both cards print needs no turn restriction: CR
    -- 305.3 forbids playing a land on another player's turn for any reason, as
    -- Pawl.GameSpec's "CR 305.3 flash does not let a land be played on another
    -- player's turn" proves.
    PlayAdditionalLands Natural.Natural
  | -- | CR 402.2 / Reliquary Tower: this player has no maximum hand size.
    NoMaximumHandSize
  | -- | CR 402.2 / The Ten Rings: this player's maximum hand size IS this number.
    SetMaximumHandSize Natural.Natural
  | -- | CR 402.2 / 613.11 / Minamo Scrollkeeper: this player's maximum hand size
    -- is INCREASED by this number.
    IncreaseMaximumHandSize Natural.Natural
  | -- | CR 402.2 / 613.11 / Gnat Miser: this player's maximum hand size is REDUCED
    -- by this number, floored at zero by CR 107.1b.
    ReduceMaximumHandSize Natural.Natural
  | -- | CR 500.5 / 703.4q / Upwelling, Omnath Locus of Mana: this player does not
    -- lose the mana the filter names as a step or phase ends.
    DontLoseUnspentMana ManaFilter.ManaFilter
  | -- | CR 500.5 / 119.3 / Yurlok of Scorch Thrash: as this player loses unspent
    -- mana, they lose that much life.
    LoseLifeForUnspentMana
  | -- | CR 609.4b / 613.11 / Celestial Dawn: this player may spend the mana the
    -- payload's filter names as though it were mana of the types it names.
    SpendManaAsThough SpendManaAsThough.SpendManaAsThough
  | -- | CR 702.18a / 702.11c / Ivory Mask, Leyline of Sanctity: this player can't
    -- be the target of spells or abilities controlled by the players the scope
    -- names -- the shroud and hexproof scopes respectively.
    CantBeTargetedBy PlayerScope.PlayerScope
  | -- | CR 702.16a / 702.16j / The Stasis Coffin, Runed Halo, Absolute Virtue:
    -- this player has protection from the quality the Filter states. Rule
    -- 702.16e's prevention
    -- reaches the player through Pawl.Engine.PlayerEffect.protectionCarriers,
    -- proven by Pawl.CastProhibitionSpec's "CR 702.16e" Runed Halo case.
    HasProtectionFrom (Filter.Filter Keyword.Keyword)
  | -- | CR 601.3b / Vedalken Orrery: this player may cast a matching spell as
    -- though it had flash.
    CastAsThoughItHadFlash (Filter.Filter Keyword.Keyword)
  | -- | CR 601.1a / 601.3b / Scout's Warning: this player may PLAY a matching card
    -- as though it had flash, which reaches a land where the arm above does not.
    MayPlayAsThoughItHadFlash (Filter.Filter Keyword.Keyword)
  | -- | CR 602.5d / 602.5e / Leonin Shikari: this player may activate abilities
    -- under this rule-702 keyword any time they could cast an instant.
    ActivateKeywordAtInstantSpeed (KeywordDesignator.KeywordDesignator Keyword.Keyword)
  | -- | CR 606.3 / The Wandering Emperor: this player may activate the loyalty
    -- abilities of a matching permanent any time they could cast an instant.
    ActivateLoyaltyAtInstantSpeed (Filter.Filter Keyword.Keyword)
  | -- | CR 701.6a / 613.11 / Spider-Punk: the matching spells and abilities on the
    -- stack controlled by the players this effect's scope names can't be
    -- countered.
    CantBeCountered (Filter.Filter Keyword.Keyword)
  | -- | CR 615.12 / 613.11 / Spider-Punk: damage matching the pattern can't be
    -- prevented. Pawl.ReplacementSpec's questingBeastSpec proves the pattern's
    -- source and kind limbs against each other. Pawl.CardSpec lints the pool
    -- for a narrowed PlayerScope, which is what makes
    -- Pawl.Engine.PlayerEffect.unpreventable's board-wide fold exact.
    -- Whippoorwill's "that creature" aims the pattern at the recipient its own
    -- resolution chose, which Pawl.PreventionSpec's Whippoorwill case proves is
    -- narrower than the board.
    DamageCantBePrevented DamagePattern.DamagePattern
  | -- | CR 614.9 / 613.11 / Lava Burst: damage matching the pattern can't be dealt
    -- instead to another permanent or player -- the redirection twin of the arm
    -- above.
    DamageCantBeRedirected DamagePattern.DamagePattern
  | -- | CR 701.23 / 613.11 / Leonin Arbiter: this player can't search libraries.
    CantSearchLibraries CantSearchLibraries.CantSearchLibraries
  | -- | CR 725.1 / 101.2 / Jared Carthalion, True Heir: this player can't become
    -- the monarch.
    CantBecomeMonarch
  | -- | CR 701.32 / 904.9 / All in Good Time: schemes can't be set in motion.
    CantSetSchemesInMotion
  | -- | CR 508.1c / Angelic Arbiter: this player can't attack with creatures.
    CantAttackWithCreatures
  | -- | CR 601.3a / Damping Engine: this player can't cast a spell matching the
    -- Filter, read against the proposal's projection.
    CantCastMatching (Filter.Filter Keyword.Keyword)
  | -- | CR 307.5 / Teferi, Mage of Zhalfir: this player can cast spells only at the
    -- moment CR 307.5 defines -- a main phase of their own turn with an empty
    -- stack, priority in hand.
    CastOnlyAtSorcerySpeed
  | -- | CR 305.1 / Damping Engine, City in a Bottle: this player can't play a land
    -- matching the Filter, which Damping Engine's unrestricted sentence writes as
    -- @And []@.
    CantPlayLands (Filter.Filter Keyword.Keyword)
  | -- | CR 601.3 / Yawgmoth's Will, Garruk's Horde, Sen Triplets: this player may
    -- cast a matching card from the zone the payload names.
    --
    -- ONE arm for every zone a CR 601.3 permission can open, the zone and its
    -- owner being the payload's (Pawl.Types.CastFromZone). The graveyard and the
    -- top of a library had an arm each, and the second was the first with a
    -- different zone written into its name: the narrowing to the TOP card is
    -- Pawl.Engine.Cast.pileCandidates' and never a Filter's, so nothing but the
    -- zone told the two apart.
    --
    -- Read as a DISJUNCTION (Pawl.Engine.PlayerEffect.mayCastFrom): one
    -- applicable permission is enough, so CR 613.11's timestamp order has nothing
    -- to order.
    CastFrom CastFromZone.CastFromZone
  | -- | CR 305.1 / Crucible of Worlds: this player may play lands from the zone the
    -- reference names -- the play half of CastFrom above, since a land is played
    -- and never cast.
    --
    -- A library reference means its TOP CARD, the narrowing
    -- Pawl.Engine.Cast.pileCandidates states for both halves of Future Sight's
    -- sentence; Pawl.CastPermissionSpec's FutureSight group proves it.
    PlayLandsFrom InZone.InZone
  | -- | CR 702.170f / Fblthp, Lost on the Range: a matching card in the named
    -- zone may be plotted from there, for its own plot cost or its mana cost.
    PlotFrom PlotFromZone.PlotFromZone
  | -- | CR 118.9 / Omniscience: this player may cast a matching spell from their
    -- hand without paying its mana cost.
    --
    -- What a narrowing filter would see of a card in a hand is unobserved (#4712,
    -- the same gap the graveyard arm records).
    CastFromHandWithoutPayingManaCost (Filter.Filter Keyword.Keyword)
  | -- | CR 101.2 / 122.1 / Solemnity, Melira Sylvok Outcast: this player can't get
    -- counters -- of the kind the Maybe names, or of every kind where it is Nothing.
    CantGetCounters (Maybe PlayerCounterKind.PlayerCounterKind)
  | -- | CR 705.3 / Edgar, King of Figaro: an effect stating that a coin flip this
    -- player flips has a certain result and\/or that this player wins it.
    StateCoinFlip StatedFlip.StatedFlip
  | -- | CR 706.2 / Clam-I-Am, Wall of Fortune: a modifier this player's die rolls
    -- take from a source other than the instruction that ordered them.
    --
    -- StateCoinFlip's sibling one rule over: rule 705.3's statement and rule
    -- 706.2's other-source modifier both reach a piece of randomness a
    -- resolution is in the middle of, and neither names an object to hang on.
    ModifyDieRoll ModifiedRoll.ModifiedRoll
  | -- | CR 701.38d / Brago's Representative: this player gets this many votes
    -- beyond the one CR 701.38a gives every seat, cast at the same time they
    -- would otherwise have voted.
    AdditionalVotes Natural.Natural
  | -- | CR 701.25b / Enhanced Surveillance: this player may look at this many
    -- more cards each time they surveil.
    AdditionalSurveilCards Natural.Natural
  | -- | CR 119.7 / Giant Cindermaw, Platinum Emperion: this player can't gain
    -- life.
    --
    -- A STATIC restriction rather than a replacement, which is why
    -- Pawl.Engine.Event.resolveLifeGain consults it AHEAD of the proposal;
    -- Pawl.PlayerEffectSpec's GiantCindermaw group proves it.
    CantGainLife
  | -- | CR 119.8 / Platinum Emperion: this player can't lose life.
    --
    -- Damage is still DEALT to such a player -- CR 120.3a's RESULT is what does
    -- not happen -- and CR 119.8's last sentence makes a pay-life cost
    -- unpayable. Pawl.PlayerEffectSpec's PlatinumEmperion and GreedUnderEmperion
    -- groups prove the two.
    CantLoseLife
  deriving (Eq, Ord, Show)
