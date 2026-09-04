module Pawl.Types.PlayerEffect where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.AddSpellCost as AddSpellCost
import qualified Pawl.Types.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.IncreaseActivationCost as IncreaseActivationCost
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost
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
  | -- | CR 601.3 / Rule of Law: this player can't cast more than this many spells
    -- each turn.
    CantCastMoreThan Natural.Natural
  | -- | CR 601.3a / Null Chamber: this player can't cast a spell whose name is one
    -- of the names chosen for this effect's source, read off Object.chosenNames
    -- there (CR 608.2h once it has left).
    CantCastChosenName
  | -- | CR 305.1 / Null Chamber: this player can't PLAY a land whose name is one
    -- of the names chosen for this effect's source.
    CantPlayLandChosenName
  | -- | CR 613.11 / 601.2f / Thalia: matching spells cost this much more generic
    -- mana to cast.
    IncreaseSpellCost IncreaseSpellCost.IncreaseSpellCost
  | -- | CR 613.11 / 601.2f / 602.2b / Oppressive Rays: the activated abilities
    -- of matching permanents cost this much more generic mana to activate.
    IncreaseActivationCost IncreaseActivationCost.IncreaseActivationCost
  | -- | CR 613.11 / 601.2f / Sapphire Medallion, Edgewalker: matching spells cost
    -- this much less to cast.
    ReduceSpellCost ReduceSpellCost.ReduceSpellCost
  | -- | CR 613.11 / 601.2f / 602.2b / Heartstone, Training Grounds: the
    -- activated abilities of matching permanents cost this much less to
    -- activate, and this effect may not reduce the mana left in such a cost
    -- below the Natural.
    ReduceActivationCost ReduceActivationCost.ReduceActivationCost
  | -- | CR 118.8 / 602.2b / Brutal Suppression: the activated abilities of
    -- matching permanents cost these additional non-mana components to activate.
    AddActivationCost AddActivationCost.AddActivationCost
  | -- | CR 118.8 / Drought: matching spells cost these additional non-mana
    -- components to cast.
    AddSpellCost AddSpellCost.AddSpellCost
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
  | -- | CR 609.4b / 613.11 / Celestial Dawn: this player may spend the mana the
    -- payload's filter names as though it were mana of the types it names.
    SpendManaAsThough SpendManaAsThough.SpendManaAsThough
  | -- | CR 702.18a / 702.11c / Ivory Mask, Leyline of Sanctity: this player can't
    -- be the target of spells or abilities controlled by the players the scope
    -- names -- the shroud and hexproof scopes respectively.
    CantBeTargetedBy PlayerScope.PlayerScope
  | -- | CR 702.16c / 702.16b / Runed Halo: this player has protection from the
    -- card name the source has chosen. Rule 702.16e's prevention reaches the
    -- player through Pawl.Engine.PlayerEffect.protectionCarriers, proven by
    -- Pawl.PlayerEffectSpec's "CR 702.16e" Runed Halo case.
    --
    -- Not implemented: protection from a quality the card itself states, which
    -- would want a Filter-carrying sibling and has no player-side producer
    -- (#3048).
    HasProtectionFromChosenName
  | -- | CR 601.3b / Vedalken Orrery: this player may cast a matching spell as
    -- though it had flash.
    CastAsThoughItHadFlash (Filter.Filter Keyword.Keyword)
  | -- | CR 601.1a / 601.3b / Scout's Warning: this player may PLAY a matching card
    -- as though it had flash, which reaches a land where the arm above does not.
    MayPlayAsThoughItHadFlash (Filter.Filter Keyword.Keyword)
  | -- | CR 701.6a / 613.11 / Spider-Punk: the matching spells and abilities on the
    -- stack controlled by the players this effect's scope names can't be
    -- countered.
    CantBeCountered (Filter.Filter Keyword.Keyword)
  | -- | CR 615.12 / 613.11 / Spider-Punk: damage matching the pattern can't be
    -- prevented. Pawl.ReplacementSpec's questingBeastSpec proves the pattern's
    -- source and kind limbs against each other.
    --
    -- Not implemented: Whippoorwill's recipient limb has no site to bake a
    -- recipient into this pattern (#845).
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
  | -- | CR 601.3a / Damping Engine: this player can't cast a spell matching the
    -- Filter, read against the proposal's projection.
    CantCastMatching (Filter.Filter Keyword.Keyword)
  | -- | CR 307.5 / Teferi, Mage of Zhalfir: this player can cast spells only at the
    -- moment CR 307.5 defines -- a main phase of their own turn with an empty
    -- stack, priority in hand.
    CastOnlyAtSorcerySpeed
  | -- | CR 305.1 / Damping Engine: this player can't play lands.
    CantPlayLands
  | -- | CR 601.3 / Yawgmoth's Will: this player may cast a matching card from their
    -- graveyard.
    --
    -- The Filter reads the PRINTED card in the graveyard, so a continuous effect
    -- changing a graveyard card's own characteristics is invisible to the
    -- narrowing (#1859).
    CastFromGraveyard (Filter.Filter Keyword.Keyword)
  | -- | CR 305.1 / Crucible of Worlds: this player may play lands from their
    -- graveyard -- the play half of CastFromGraveyard above, since a land is
    -- played and never cast.
    PlayLandsFromGraveyard
  | -- | CR 118.9 / Omniscience: this player may cast a matching spell from their
    -- hand without paying its mana cost.
    --
    -- What a narrowing filter would see of a card in a hand is unobserved (#1859,
    -- the same gap the graveyard arm records).
    CastFromHandWithoutPayingManaCost (Filter.Filter Keyword.Keyword)
  | -- | CR 101.2 / 122.1 / Solemnity, Melira Sylvok Outcast: this player can't get
    -- counters -- of the kind the Maybe names, or of every kind where it is Nothing.
    CantGetCounters (Maybe PlayerCounterKind.PlayerCounterKind)
  | -- | CR 705.3 / Edgar, King of Figaro: an effect stating that a coin flip this
    -- player flips has a certain result and\/or that this player wins it.
    StateCoinFlip StatedFlip.StatedFlip
  deriving (Eq, Ord, Show)
