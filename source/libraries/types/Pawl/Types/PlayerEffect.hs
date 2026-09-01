module Pawl.Types.PlayerEffect where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.AddSpellCost as AddSpellCost
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
    -- each turn. The limit is carried, not hardcoded: Rule of Law and Arcane
    -- Laboratory both say one, and a card that says two must not need a sibling
    -- constructor.
    CantCastMoreThan Natural.Natural
  | -- | CR 601.3 / Null Chamber: this player can't cast a spell whose name is one
    -- of the names chosen for this effect's SOURCE ("Spells with the chosen names
    -- can't be cast"), as it entered (CR 614.1c) or, on the stored carrier, while
    -- it resolved (CR 608.2c; Conjurer's Ban).
    --
    -- NULLARY, carrying no name, for UnderSourceControl's reason on the entry
    -- side: a card can write a CardName (its own face's, a token definition's)
    -- but not THIS one, which is not known until the choice is made -- the same
    -- reason that arm carries no PlayerId. The names come from
    -- Object.chosenNames on the source instead, which is how
    -- Modification.AddChosenColor already reads a colour, and CR 608.2h is what
    -- answers once that source has left (Pawl.Engine.PlayerEffect.chosenNamesOf).
    --
    -- The quality is the SPELL's name, which makes this CR 601.3a's shape rather
    -- than CantCastSpells' -- see Pawl.Engine.PlayerEffect.prohibitsCasting for
    -- what that costs. CR 601.3a's lookahead reaches this arm not at all: a
    -- spell's name is fixed by the half and the facing, and both are chosen
    -- before the prohibition is asked.
    CantCastChosenName
  | -- | CR 305.1 / Null Chamber: this player can't PLAY a land whose name is one
    -- of the names chosen for this effect's source ("lands with the chosen names
    -- can't be played"), read exactly as CantCastChosenName above reads them.
    --
    -- The sibling of CantCastChosenName above, and deliberately not folded into
    -- it even though one printed sentence carries both: CR 305.1 makes playing a
    -- land a special action that never uses the stack, so a land is never cast
    -- and the two prohibitions are read by two different gates
    -- (Pawl.Engine.Action.playableLands, Pawl.Engine.Cast.castable). A card
    -- printing only one half (Nevermore prints only the cast half) declares only
    -- one arm; Null Chamber and Conjurer's Ban each print both.
    CantPlayLandChosenName
  | -- | CR 613.11 / 601.2f / Thalia: matching spells cost this much more generic
    -- mana to cast.
    IncreaseSpellCost IncreaseSpellCost.IncreaseSpellCost
  | -- | CR 613.11 / 601.2f / Oppressive Rays: the activated abilities of matching
    -- permanents cost this much more generic mana to activate, which CR 602.2b
    -- makes an activation cost's rule as much as a spell's.
    --
    -- A SEPARATE constructor from IncreaseSpellCost for the reason
    -- ReduceActivationCost is separate from ReduceSpellCost, stated in full
    -- below: the Filter classifies an OBJECT, so nothing in it can say "and it
    -- is a spell", which leaves the MOMENT an arm is asked at the constructor's
    -- to say. Thalia does not tax an activation, and Oppressive Rays does not
    -- tax a spell.
    IncreaseActivationCost IncreaseActivationCost.IncreaseActivationCost
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
    -- An EXCESS typed symbol comes off the generic component (CR 118.7b-d)
    -- unless the payload's `coloredOnly` says otherwise, which is Edgewalker's
    -- "This effect reduces only the amount of colored mana you pay".
    ReduceSpellCost ReduceSpellCost.ReduceSpellCost
  | -- | CR 613.11 / 601.2f / Heartstone, Training Grounds: the activated abilities
    -- of matching permanents cost this much less to activate, and this effect may
    -- not reduce the mana left in such a cost below the Natural.
    --
    -- A SEPARATE constructor from ReduceSpellCost, and that separation is the
    -- whole of pawl's spell-vs-ability discriminator: the Filter these arms carry
    -- classifies an OBJECT (Pawl.Engine.PlayerEffect.matchesObject reads a
    -- projection), so nothing in a Filter can say "and it is a spell". Thalia's
    -- IncreaseSpellCost narrows by `Not (HasCardType Creature)`, which a
    -- noncreature PERMANENT matches -- so were one constructor asked at both
    -- moments, Thalia would tax Mindslaver's activation. Which MOMENT an arm is
    -- asked at is therefore the constructor's to say, and each of the two is
    -- gathered by its own function (Pawl.Engine.PlayerEffect.spellCostAdjustments,
    -- activationCostAdjustments).
    --
    -- The Filter is matched against the ability's SOURCE PERMANENT and not
    -- against the ability, which is what these printings name: Heartstone says
    -- "activated abilities of creatures", Training Grounds "of creatures you
    -- control", Blossoming Tortoise "of lands you control" (HasCardType Creature
    -- or Land, with the possessive riding the carrier's PlayerScope as every
    -- other arm's does). The KIND of ability is a second criterion beside it --
    -- Fluctuator's "cycling abilities you activate" -- and rides
    -- ReduceActivationCost.grantedBy, which names a rule-702 family rather than a
    -- Filter; see that type for why the two cannot be one field.
    --
    -- A THIRD criterion is CR 605.1a's mana-ability classification, which rides
    -- ReduceActivationCost.whichKind -- Zirda, the Dawnwaker's "abilities you
    -- activate that aren't mana abilities". Neither a Filter nor a rule-702
    -- family, and IncreaseActivationCost above carries the same field for the
    -- same reason.
    --
    -- A FOURTH criterion names what the ability TARGETS -- Dwarven Mauler's
    -- "equip abilities you activate that target this creature" -- and rides
    -- ReduceActivationCost.whichTargets. It is a Filter like the first, and an
    -- object question like the first, but asked of CR 601.2c's chosen target
    -- rather than of the ability's source, and therefore asked at a later moment
    -- than any of the others.
    --
    -- The FLOOR is carried rather than assumed, because it is card text (CR
    -- 101.1) and not a rule: Heartstone, Training Grounds and Zirda all say
    -- "This effect can't reduce the mana in that cost to less than one mana" and
    -- so carry 1, while an activation-cost reducer that does not say it
    -- (Blossoming Tortoise's "Activated abilities of lands you control cost {1}
    -- less to activate") carries 0. See Pawl.Types.CostAdjustments.reductions
    -- for what zero means, why a floor never raises a cost, and why the two
    -- kinds cannot share one floor over the pool.
    ReduceActivationCost ReduceActivationCost.ReduceActivationCost
  | -- | CR 613.11 / 601.2f / Brutal Suppression: the activated abilities of
    -- matching permanents cost these additional NON-MANA components to activate
    -- ("Activated abilities of nontoken Rebels cost an additional \"Sacrifice a
    -- land\" to activate"). CR 601.2f's "plus all additional costs" clause,
    -- reaching an activation cost by CR 602.2b.
    --
    -- A THIRD constructor beside ReduceActivationCost above rather than a field
    -- on it, and not a non-mana twin of IncreaseSpellCost either. The two
    -- reasons are the ones this whole family is split on: which MOMENT an arm is
    -- asked at is the constructor's to say (a spell's additional costs come off
    -- its own card text and never from here), and CR 601.2f's arithmetic --
    -- increases, then reductions, then the {0} floor -- has nothing to do to a
    -- component. Nothing reduces a "sacrifice a land" away.
    --
    -- The Filter is matched against the ability's SOURCE PERMANENT, exactly as
    -- ReduceActivationCost's is, and for its reason: that is what the printing
    -- narrows ("nontoken Rebels" is `And [HasSubtype Rebel, Not IsToken]`).
    --
    -- A LIST of components, matching Pawl.Types.Cost.components: one sentence
    -- can name several actions ("sacrifice a land and discard a card"), and a
    -- list needs no sibling constructor when one does.
    --
    -- How many times that list joins the cost is the payload's CostScale, which
    -- is what Drought's "for each black mana symbol in their activation costs"
    -- writes; Pawl.Engine.Cost.plusComponents resolves it, since only that
    -- function holds the cost being adjusted.
    AddActivationCost AddActivationCost.AddActivationCost
  | -- | CR 613.11 / 601.2f / Drought: matching SPELLS cost these additional
    -- NON-MANA components to cast ("Spells cost an additional \"Sacrifice a
    -- Swamp\" to cast for each black mana symbol in their mana costs"). CR
    -- 118.8's "or applied to a spell or ability from another effect", which is
    -- the half a spell's own card text (Pawl.Types.Face.additionalCosts) cannot
    -- state.
    --
    -- The SPELL-side twin of AddActivationCost above, and a separate constructor
    -- for that arm's stated reason: the Filter classifies an OBJECT, so nothing
    -- in it can say "and it is a spell", and which moment an arm is asked at is
    -- therefore the constructor's to say.
    AddSpellCost AddSpellCost.AddSpellCost
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
    -- restriction, and that is exact rather than a shortcut about Magic: CR
    -- 305.3 forbids playing a land on another player's turn "for any reason", so
    -- Pawl.Engine.Action.legalActions gates every land play on being the active
    -- player and a grant reaching another player's turn could never be observed.
    -- Nor is that waiting on a timing widening: CR 702.8a's flash lifts CR
    -- 116.2a's phase and empty-stack conjuncts and leaves CR 305.3 standing
    -- (Pawl.GameSpec's "CR 305.3 flash does not let a land be played on another
    -- player's turn").
    PlayAdditionalLands Natural.Natural
  | -- | CR 402.2 / Reliquary Tower: this player has no maximum hand size.
    NoMaximumHandSize
  | -- | CR 402.2 / The Ten Rings: this player's maximum hand size IS this number.
    --
    -- A SEPARATE constructor from NoMaximumHandSize above, and hand size is the
    -- one axis here whose arms CR 613.11's timestamp order can tell apart: a set
    -- and a removal disagree, and the later one wins (Reliquary Tower's own ruling
    -- names the pair). Every arm on every other axis folds order-independently,
    -- which is why Pawl.Engine.PlayerEffect.maximumHandSize is the module's only
    -- ordered fold.
    --
    -- The number is CARRIED for CantCastMoreThan's reason: The Ten Rings says ten,
    -- Cursed Rack says four and Null Profusion says two, and a card that says
    -- another number must not need a sibling constructor.
    --
    -- A SET, not an adjustment: the two arms below are the adjustments, and they
    -- compose with whatever number they find instead of replacing it.
    SetMaximumHandSize Natural.Natural
  | -- | CR 402.2 / 613.11 / Minamo Scrollkeeper: this player's maximum hand size
    -- is INCREASED by this number.
    --
    -- An adjustment rather than a set, which is what makes CR 613.11's timestamp
    -- order decide a third answer on this axis: an increase applied after
    -- SetMaximumHandSize raises the number that was set, and one applied after
    -- NoMaximumHandSize raises nothing at all, because there is no number left to
    -- raise.
    IncreaseMaximumHandSize Natural.Natural
  | -- | CR 402.2 / 613.11 / Gnat Miser: this player's maximum hand size is
    -- REDUCED by this number.
    --
    -- A SEPARATE constructor from IncreaseMaximumHandSize rather than that one
    -- carrying a signed delta, and for the reason IncreaseSpellCost and
    -- ReduceSpellCost are two constructors: the two directions do not have the
    -- same range. CR 107.1b floors the reduction at zero -- a maximum hand size
    -- is a calculation determining the result of an effect, and none of that
    -- rule's exceptions is a hand size -- where an increase has no ceiling.
    -- Natural says exactly that, and a signed delta would additionally spell one
    -- effect two ways.
    ReduceMaximumHandSize Natural.Natural
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
    -- Shizuko, Caller of Autumn and Karn, Legacy Reforged keep only the mana
    -- they just added, which is not a player-axis property at all and so is not
    -- reachable by widening this filter. That retention rides the mana unit
    -- instead (Pawl.Types.ManaRetention), and the two carriers coexist: this one
    -- answers a clause about a player's whole pool, that one a clause about the
    -- mana one instruction added.
    DontLoseUnspentMana ManaFilter.ManaFilter
  | -- | CR 609.4b / 613.11 / Celestial Dawn: this player may spend the mana the
    -- payload's filter names as though it were mana of the types the payload
    -- names.
    --
    -- A PLAYER-AXIS continuous effect, where Pawl.Types.ManaSpending is CR
    -- 118.14's per-cost one. Rule 118.14's last sentence scopes that permission
    -- to the spells cast under the effect that granted it, so it rides
    -- Pawl.Types.ExilePlayPermission; Celestial Dawn grants no cast permission
    -- and covers every cost its controller pays, which is this carrier.
    --
    -- The SUPPLY side and not the demand side, which is why the two cannot be
    -- one type. Pawl.Engine.Mana.relax applies a ManaSpending to a cost's
    -- DEMANDS -- "this cost accepts anything" -- and a transform of a demand
    -- cannot depend on which unit is being spent. Celestial Dawn's sentence says
    -- different things about two manas of one pool, so it is applied to the
    -- SUPPLY each unit offers (Pawl.Engine.Mana.rewriteSupply).
    --
    -- ONE clause per entry, so Celestial Dawn writes two: the permission over
    -- white and the restriction over "other mana". That is how the card prints
    -- them, and it keeps the payload from encoding one card's sentence
    -- structure.
    --
    -- WHOSE mana is the carrier's PlayerScope, as for every arm here --
    -- Celestial Dawn says "you may spend" and so writes PlayerScope.You.
    SpendManaAsThough SpendManaAsThough.SpendManaAsThough
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
  | -- | CR 702.16c / 702.16b / Runed Halo: this player has protection from the
    -- card name the source has chosen.
    --
    -- The PLAYER half of rule 702.16, here for CantBeTargetedBy's reason above:
    -- rule 702's keywords live on objects and fold through the CR 613.1-613.7
    -- layers, and a player has no characteristics for that machine to compute.
    --
    -- NO quality payload, CantCastChosenName's shape and for its reason: CR
    -- 201.4's chosen name is read off the source's Object.chosenNames rather than
    -- written by the card, so there is nothing here for a Filter to carry. A
    -- protection ability whose quality IS card data -- rule 702.16j's "protection
    -- from everything", or a stated colour -- wants a Filter-carrying constructor
    -- beside this one and has no player-side producer yet (#2229).
    --
    -- Rule 702.16e's "any damage ... is prevented" reaches the protected PLAYER
    -- through Pawl.Engine.Replacement.collect's third segment, which mints a CR
    -- 615.1 shield off Pawl.Engine.PlayerEffect.protectionCarriers -- the player
    -- twin of the row Pawl.Engine.Keyword mints from the keyword. Proven by
    -- Pawl.PlayerEffectSpec's "CR 702.16e" Runed Halo case.
    HasProtectionFromChosenName
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
    -- everything (`And []`), while Sigarda's Aid says "Aura and Equipment spells"
    -- and narrows it to two subtypes. A QUALIFIED filter is what gives rule
    -- 601.3b's second sentence something to search, which
    -- Pawl.Engine.PlayerEffect.choiceCouldApply does.
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
    -- Pawl.Engine.PlayerEffect.matchesObject. Spider-Punk narrows by nothing and
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
  | -- | CR 615.12 / 613.11 / Spider-Punk: damage can't be prevented.
    --
    -- CR 611.1's third clause in its purest form -- a continuous effect that
    -- modifies the RULES OF THE GAME and no player's or object's
    -- characteristics. That is why it sits here beside the player-axis arms
    -- rather than in Pawl.Types.Modification: CR 613.1 computes an object's
    -- characteristics, and "damage can't be prevented" is not a characteristic
    -- of anything -- it is a standing edit to what CR 615.1's shields do.
    --
    -- WHICH damage is a Pawl.Types.DamagePattern, the same type CR 615.1's
    -- shields and CR 614.1a's damage replacements are patterned by, because the
    -- printed narrowings of this sentence narrow exactly what that type speaks:
    -- Excruciator's "damage that would be dealt by this creature" is a SOURCE,
    -- Frenzied Baloth's "combat damage" is a KIND, and Whippoorwill's "damage
    -- that would be dealt to that creature" is a RECIPIENT. Spider-Punk's
    -- sentence names none of the three and so carries the pattern that admits
    -- everything, which Leyline of Punishment, Everlasting Torment and Sunspine
    -- Lynx print too.
    --
    -- Pattern rather than an arm per printed clause, so this stays a
    -- CLASSIFICATION: the engine asks the pattern whether it admits the event
    -- and never asks which card wrote it.
    --
    -- Questing Beast's "combat damage that would be dealt by CREATURES YOU
    -- CONTROL" narrows the source by a characteristic rather than by identity,
    -- and its KIND alongside it, so it is the pool's card that makes both halves
    -- of the pattern observable at once -- Pawl.ReplacementSpec's
    -- questingBeastSpec proves each of the two against the other's control.
    --
    -- Not implemented: Whippoorwill's recipient limb has no site to bake a
    -- recipient into this pattern (#845).
    --
    -- Lava Burst's self-referential "that damage can't be prevented" is this
    -- same arm on the CR 611.2c stored carrier, whose `source` is the resolving
    -- spell -- so Filter.IsSource in the pattern names the spell itself and
    -- nothing new is carried. See Pawl.Engine.PlayerEffect.applying, which
    -- threads that id out.
    --
    -- A DURATION is not carried, and needs nothing new: Skullcrack's
    -- "damage can't be prevented this turn" is this same effect on the CR 611.2c
    -- stored carrier (Pawl.Types.ActivePlayerEffect), whose expiry is the
    -- duration, exactly as Silence's is.
    --
    -- WHOSE damage is not a question this arm's carrier answers, and it is the
    -- one arm here of which that is true: CR 615.12's sentence is about a damage
    -- EVENT, which may run between two creatures and involve no player at all,
    -- so there is nobody for a PlayerScope to select. Spider-Punk's
    -- possessive-free sentence accordingly writes PlayerScope.EachPlayer, and no
    -- card may write another: Pawl.CardSpec lints the pool for a narrowed
    -- carrier and rejects one, which is what makes
    -- Pawl.Engine.PlayerEffect.unpreventable's board-wide fold exact rather than
    -- approximate. The narrowing rides in the pattern above instead, where CR
    -- 615.12's own subject is.
    DamageCantBePrevented DamagePattern.DamagePattern
  | -- | CR 614.9 / 613.11 / Lava Burst: damage can't be dealt instead to another
    -- permanent or player -- the REDIRECTION twin of the arm above.
    --
    -- CR 611.1's third clause again, and a separate arm rather than a flag on
    -- the one above because the two sentences are separable: Spider-Punk,
    -- Excruciator, Questing Beast and Malignus all say "can't be prevented" and
    -- nothing about redirection, so a single carrier meaning both would make
    -- Spider-Punk stop Turn the Tables. Lava Burst pairs the two limbs in one
    -- sentence and writes both arms.
    --
    -- WHICH damage is the same Pawl.Types.DamagePattern, for the same reason and
    -- read by the same Pawl.Engine.Replacement.matchesDamagePattern: Lava Burst's
    -- "if Lava Burst would deal damage to a creature" names a SOURCE
    -- (Filter.IsSource) and a RECIPIENT (`whatRecipient`) at once.
    --
    -- The rules CONSEQUENCE differs from the prevention arm's, and CR 614.9 is
    -- why: CR 615.12 says an inapplicable prevention is "still applied ... any
    -- additional effects they have will take place", where CR 614.9 states no
    -- such twin -- its only "the effect does nothing" case is a destination that
    -- left the battlefield or a player who left the game. So a prohibited
    -- redirection is not applicable at all and Pawl.Engine.Replacement.applies
    -- filters it out of CR 616.1's choice, rather than applying it inertly.
    --
    -- WHOSE damage is not a question this carrier answers either, exactly as
    -- above: CR 614.9's subject is a damage event, so PlayerScope.EachPlayer is
    -- the only scope a card may write and Pawl.CardSpec lints for it.
    DamageCantBeRedirected DamagePattern.DamagePattern
  | -- | CR 701.23 / 613.11 / Leonin Arbiter: this player can't search libraries.
    --
    -- CR 611.1's third clause again -- searching is something the RULES let a
    -- player do while following an instruction, not a characteristic of any
    -- object -- which is why it sits here rather than in
    -- Pawl.Types.Modification.
    --
    -- WHICH library is not carried, and Leonin Arbiter's sentence names none
    -- either: it stops the player from searching a library, whoever owns it,
    -- which is exactly what an unqualified prohibition on the SEARCHER does. It
    -- is a prohibition on LIBRARIES, so Pawl.Engine.Resolve removes that one zone
    -- from a Search.zones naming others rather than stopping the instruction. A
    -- card that prohibited searching only some libraries would want a filter here
    -- (#1269).
    --
    -- WHOSE searching is the carrier's PlayerScope, as for every arm here:
    -- Leonin Arbiter says "players" with no possessive, so EachPlayer.
    CantSearchLibraries
  | -- | CR 725 / 101.2 / Jared Carthalion, True Heir: this player can't become the
    -- monarch.
    --
    -- CR 611.1's third clause once more, and here in the same shape
    -- CantSearchLibraries takes: CR 725.1 makes the monarch a DESIGNATION a
    -- player has rather than a characteristic of any object, so nothing in the CR
    -- 613.1 layers computes it and a restriction on taking it belongs on CR
    -- 613.11's rules axis.
    --
    -- CR 101.2 is what makes it bite, because neither CR 725.1 nor CR 725.3 gates
    -- WHO may be crowned: an effect instructing this player to become the monarch
    -- is allowed by the rules and stopped by this "can't". CR 725.4's "the next
    -- player in turn order who can become the monarch" is the one place the
    -- rulebook asks the question itself.
    --
    -- WHOSE crown is the carrier's PlayerScope, as for every arm here. Jared's
    -- "You can't become the monarch this turn" writes PlayerScope.You on the
    -- stored CR 611.2c carrier, whose expiry is the duration -- exactly the shape
    -- Silence takes, and the reason no duration is carried here.
    --
    -- NULLARY, and rule 725 is why: the designation has no parts, so there is
    -- nothing for a payload to narrow. Not "can't become the monarch from a
    -- particular source" either -- CR 725.1 leaves the naming to the card and the
    -- restriction is on the PLAYER, so every route is stopped at once (the
    -- ordinary effect, and CR 725.2's sourceless steal).
    CantBecomeMonarch
  | -- | CR 601.3a / Damping Engine: this player can't cast a spell matching the
    -- Filter ("can't ... cast artifact, creature, or enchantment spells").
    --
    -- NOT a widening of CantCastSpells above, and the split is the one CR 601.3
    -- itself draws in the shape CantCastChosenName already follows: that rule's
    -- bare "can't cast spells" names no quality of the spell, while CR 601.3a is
    -- about a prohibition that DOES, and the quality has to be read off the spell
    -- being proposed. Silence keeps the quality-free arm and asks nothing about
    -- the card; this one is answered by Pawl.Engine.PlayerEffect.matchesObject
    -- against the proposal's projection, which is why
    -- Pawl.Engine.PlayerEffect.prohibitsCasting takes an ObjectId beside the name.
    --
    -- Reading the PROJECTION rather than the printed face is what makes CR 708.4
    -- fall out: a morph proposal is stamped face down before this is asked, so a
    -- prohibition on creature spells stops the face-up cast of a creature card and
    -- not its 2/2 face-down one.
    --
    -- The quality may be the ZONE the spell is cast FROM, which Filter.IsInZone
    -- states -- Drannith Magistrate's "from anywhere other than their hands",
    -- Grafdigger's Cage's "from graveyards or libraries". That works because CR
    -- 601.2 casts a spell "from where it is" and this is asked before CR 601.2a
    -- moves the card to the stack, so the object still lies in the zone the cast
    -- is from. No second constructor for the zone axis: the prohibition is the
    -- same one, and CR 601.3a reads whatever quality the sentence names.
    CantCastMatching (Filter.Filter Keyword.Keyword)
  | -- | CR 307.5 / Teferi, Mage of Zhalfir: this player can cast spells only at
    -- the moment CR 307.5 defines -- a main phase of their own turn with an
    -- empty stack, priority in hand.
    --
    -- A PROHIBITION and not a window, which is what puts it in this family
    -- rather than beside CastAsThoughItHadFlash. The printed sentence reads as a
    -- permission ("can cast spells only ..."), but CR 601.3's two limbs are
    -- allow and prohibit and "only" is the prohibit one: outside that moment no
    -- cast is permitted. CR 101.2 then settles every collision -- a Vedalken
    -- Orrery, a flash keyword, or an instant's own CR 117.1a window is a "can"
    -- this "can't" outvotes, which is Teferi, Time Raveler's own ruling about a
    -- second Teferi's +1.
    --
    -- Not a Duration either: the clause lasts as long as its carrier does, which
    -- is what every arm here already means.
    --
    -- NULLARY, for CantPlayLands' reason. Scryfall
    -- @o:"only any time they could cast a sorcery"@, 2026-08-24, returns exactly
    -- two cards -- this Teferi and Teferi, Time Raveler -- and neither narrows
    -- WHICH spells, so there is nothing for a Filter to state. One that said
    -- "creature spells" would want one here.
    --
    -- WHOSE casting is the carrier's PlayerScope, as for every arm here: both
    -- printings say "each opponent", so PlayerScope.Opponents.
    CastOnlyAtSorcerySpeed
  | -- | CR 305.1 / Damping Engine: this player can't play lands.
    --
    -- The unrestricted twin of CantPlayLandChosenName above, and a separate arm
    -- for that arm's own reason once more: a land is played and never cast (CR
    -- 305.1), so CantCastMatching stops no land however its Filter reads. Damping
    -- Engine's one printed sentence declares both, exactly as Null Chamber's does.
    --
    -- NULLARY, where its sibling carries the chosen names: no printed sentence
    -- narrows WHICH land beyond a name, and a card that said "can't play
    -- nonbasic lands" would want a Filter here rather than a third arm.
    CantPlayLands
  | -- | CR 601.3 / Yawgmoth's Will: this player may cast a matching card from
    -- their graveyard.
    --
    -- CR 601.3's ALLOW half on the PLAYER axis, where every arm of
    -- Pawl.Types.CastingPermission is that same half on the OBJECT axis: those
    -- are permissions a card grants about ITSELF (flashback's "cast this card
    -- from your graveyard"), and this is a permission an effect grants a player
    -- about whatever their graveyard happens to hold. The two are read BESIDE
    -- each other in Pawl.Engine.Cast.permitsCastFromGraveyard, and neither is
    -- folded into the other -- widening the object-scoped one instead would say
    -- Yawgmoth's Will printed flashback onto every card in the graveyard, which
    -- CR 702.34a does not mean.
    --
    -- A ZONE, where CastAsThoughItHadFlash beside it names a TIME. Both are CR
    -- 601.3 permissions on the CR 613.11 player axis and both carry a Filter,
    -- but nothing composes them: a card cast from a graveyard still waits for
    -- its own timing window, which is the flashback ruling ("you can cast a
    -- sorcery using flashback only when you could normally cast a sorcery") and
    -- what keeps this arm out of Pawl.Engine.Cast.timingOk.
    --
    -- The Filter is the axis that separates the producers, as it is for
    -- CastAsThoughItHadFlash: Yawgmoth's Will says "spells" and so matches
    -- everything (`And []`), while Liliana, Untouched by Death's "Zombie spells"
    -- narrows it and Haakon, Stromgald Scourge's "Knight spells" would. Read
    -- through Pawl.Engine.PlayerEffect.matchesObject, the same read
    -- CantCastMatching makes of a card in a hand.
    --
    -- That read takes the PRINTED card in the graveyard rather than the projected
    -- one, so a continuous effect changing a graveyard CARD's own characteristics
    -- is invisible to the narrowing (#1859). CR 612.1's word swap on the ABILITY
    -- is the other axis and does reach it, through
    -- Pawl.Engine.Projection.rewritePlayerEffect.
    --
    -- A PERMISSION, folded as a disjunction for CastAsThoughItHadFlash's reason:
    -- there is nothing for a second permission to outvote.
    --
    -- The PLAY-LANDS half of the same sentence ("you may play lands and cast
    -- spells from your graveyard") is PlayLandsFromGraveyard below: a land is
    -- played and never cast (CR 305.1), so this arm reaches no land however its
    -- Filter reads.
    CastFromGraveyard (Filter.Filter Keyword.Keyword)
  | -- | CR 305.1 / Crucible of Worlds: this player may play lands from their
    -- graveyard.
    --
    -- The same zone widening CastFromGraveyard above states for a cast, stated
    -- for a PLAY, and a separate arm for the reason CantPlayLandChosenName is
    -- separate from CantCastChosenName: CR 305.1 makes playing a land a special
    -- action that never uses the stack, so the two are read by two different
    -- gates (Pawl.Engine.Action.playableLands, Pawl.Engine.Cast.castableZones).
    -- Yawgmoth's Will's one printed sentence declares both, exactly as Null
    -- Chamber's and Damping Engine's do, and Crucible of Worlds declares only
    -- this one.
    --
    -- NOT a widening of PlayAdditionalLands either. That arm is CR 305.2's COUNT
    -- and names no zone; this one names a zone and no count. They compose in
    -- Pawl.Engine.Action.legalActions without either knowing of the other -- the
    -- count settles how many plays the whole list allows, this one says which
    -- piles the list is drawn from -- so Crucible of Worlds alone still allows
    -- only one land play a turn, and Exploration beside it makes the second
    -- play available out of either zone.
    --
    -- NULLARY, where CastFromGraveyard carries a Filter. A land play has already
    -- fixed the card type (CR 305.1's "land card"), and no printed sentence
    -- narrows which land beyond that; a card that said "you may play basic lands
    -- from your graveyard" would want a Filter here rather than a second arm.
    --
    -- WHOSE graveyard is the carrier's PlayerScope, as for every arm here, and
    -- WHICH graveyard needs no field of its own: CR 400.1 makes a graveyard a
    -- per-player zone and both printings say "your graveyard", which is the pile
    -- Pawl.Engine.Action.playableLands hands the permission.
    PlayLandsFromGraveyard
  | -- | CR 118.9 / Omniscience: this player may cast a matching spell from their
    -- hand without paying its mana cost.
    --
    -- CR 118.9's "or applied to it from another effect", on the PLAYER axis. Not
    -- a permission and so not a sibling of CastFromGraveyard above, whatever the
    -- shared prefix suggests: a card in a hand is already castable, and this
    -- adds one more CR 601.2b candidate beside the ones the card itself offers.
    -- The one-shot half of the same rule is Pawl.Types.CastOffer's
    -- `withoutPayingManaCost`, which an Effect.OfferCast hands to ONE object it
    -- has already chosen; this is the standing grant, which no per-card list can
    -- hold because the effect never names the cards it will apply to.
    --
    -- BESIDE the card's own candidates rather than instead of them (CR 118.9a
    -- lets the controller announce which one), which is why
    -- Pawl.Engine.Cost.candidateCostsFor appends it: a flashback card cast free
    -- from hand still has flashback in the graveyard later, and a Fireblast
    -- under this grant may still sacrifice two Mountains instead.
    --
    -- THE HAND is in the constructor and not in the Filter, for the reason
    -- CastFromGraveyard names its zone. Filter.IsInZone could spell it
    -- (`And [criterion, IsInZone Hand]`), and does not, because the zone is the
    -- GRANT's and not the card's: a printing that left the conjunct out would
    -- then be read in every zone and make Yawgmoth's Will's graveyard casts free
    -- -- WEAKER than either printing, which is the disqualifying direction.
    --
    -- CR 107.3b's "the only legal choice for X is 0" needs no clause here. The
    -- cost this grant offers is Pawl.Engine.Cost.withoutPayingManaCost, an empty
    -- ManaCost carrying no variable at all, so Pawl.Engine.Cast.castProposed
    -- never reaches its Prompt.ChooseX and the spell resolves with X unset.
    --
    -- The Filter is the axis that separates the producers, as it is for
    -- CastFromGraveyard: Omniscience says "spells" and so matches everything
    -- (`And []`), while a narrowing printing would name the quality here. What a
    -- narrowing filter would see of a card in a hand is unobserved (#1859, the
    -- same gap the graveyard arm records).
    CastFromHandWithoutPayingManaCost (Filter.Filter Keyword.Keyword)
  | -- | CR 101.2 / 122.1 / Solemnity, Melira Sylvok Outcast: this player can't get
    -- counters. Solemnity's first sentence names no kind ("players can't get
    -- counters"); Melira's names one ("you can't get poison counters"), which is
    -- what the Maybe holds -- Nothing for every kind.
    --
    -- The PLAYER half of the two cards whose OBJECT half is
    -- Pawl.Types.CounterRestriction. Two carriers rather than one because the two
    -- kind domains are disjoint (Pawl.Types.PlayerCounterKind's header says why),
    -- and because the object half is scoped by a Filter over objects while this
    -- is scoped by the PlayerScope every row of this type already carries -- the
    -- reason Pawl.Types.EntryRestriction gives for not being a PlayerEffect,
    -- read the other way round.
    --
    -- Read by Pawl.Engine.PlayerEffect.prohibitsCounters at
    -- Pawl.Engine.Event.putPlayerCounters, the one door a player's counters go
    -- up through, and asked AFTER CR 616.1's loop has settled the placement for
    -- Pawl.Engine.CounterRestriction's reason: rule 614 replaces events that
    -- would happen, and rule 101.2 then refuses what rule 614 settled on.
    --
    -- A prohibition and not a replacement, the distinction
    -- Pawl.Types.CounterRestriction's header draws: a CR 614.16 row scaling a
    -- placement to zero describes an event that was possible and had something
    -- put instead, where "can't" is CR 101.2's word.
    CantGetCounters (Maybe PlayerCounterKind.PlayerCounterKind)
  | -- | CR 705.3: an effect stating that a coin flip this player flips has a
    -- certain result and\/or that this player wins it (Edgar, King of Figaro).
    --
    -- The rule's "ignore the actual results of that flip" is what makes this a
    -- PlayerEffect rather than a CR 614 replacement: nothing about the flip
    -- EVENT changes, and the result the rule states is read where the outcome is
    -- settled -- Pawl.Engine.Coin, the one road both writers of CR 705.1's flip
    -- take.
    StateCoinFlip StatedFlip.StatedFlip
  deriving (Eq, Ord, Show)
