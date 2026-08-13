module Pawl.Types.PlayerEffect where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost

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
    -- what that costs. CR 601.3a's lookahead reaches this arm not at all: a
    -- spell's name is fixed by the half and the facing, and both are chosen
    -- before the prohibition is asked.
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
    -- other arm's does).
    --
    -- Not expressible: a reducer that narrows by the KIND of ability rather than
    -- by its source -- Fluctuator's "cycling abilities you activate", Helitrooper's
    -- "equip abilities you activate that target this creature". This arm can only
    -- state them as "the activated abilities of a permanent matching X", which is
    -- WEAKER than each of them prints, so no such card belongs in the pool
    -- (#1431).
    --
    -- The FLOOR is carried rather than assumed, because it is card text (CR
    -- 101.1) and not a rule: both printings say "This effect can't reduce the
    -- mana in that cost to less than one mana" and so carry 1, while an
    -- activation-cost reducer that does not say it (Blossoming Tortoise's
    -- "Activated abilities of lands you control cost {1} less to activate")
    -- carries 0. See Pawl.Types.CostAdjustments.reductions for what zero means,
    -- why a floor never raises a cost, and why the two kinds cannot share one
    -- floor over the pool.
    --
    -- Not implemented: nothing INCREASES an activation cost (Suppression Field),
    -- which would be this arm's sibling and needs the "unless they're mana
    -- abilities" rider besides (#1242).
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
    -- Not implemented: an added component whose count scales with the cost being
    -- adjusted -- Drought's "for each black mana symbol in their activation
    -- costs" -- which needs the components to be a function of the cost rather
    -- than a fixed list (#1417).
    AddActivationCost (Filter.Filter Keyword.Keyword) [CostComponent.CostComponent Keyword.Keyword]
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
  | -- | CR 402.2 / The Ten Rings: this player's maximum hand size IS this number.
    --
    -- A SEPARATE constructor from NoMaximumHandSize above, and the pair is the one
    -- place on this axis where CR 613.11's timestamp order decides an answer: a
    -- set and a removal disagree, and the later one wins (Reliquary Tower's own
    -- ruling names the pair). Every other arm here folds order-independently, which
    -- is why Pawl.Engine.PlayerEffect.maximumHandSize is the axis's only ordered
    -- fold.
    --
    -- The number is CARRIED for CantCastMoreThan's reason: The Ten Rings says ten,
    -- Cursed Rack says four and Null Profusion says two, and a card that says
    -- another number must not need a sibling constructor.
    --
    -- A SET, not an adjustment. "Your maximum hand size is increased by two"
    -- (Trusted Advisor) and "reduced by three" (Thought Eater) are a third shape
    -- again -- they compose with whatever the current number is instead of
    -- replacing it, and CR 613.11 orders them against this arm -- and no
    -- constructor here can express one (#1238).
    SetMaximumHandSize Natural.Natural
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
    -- Not implemented: Questing Beast's "combat damage that would be dealt by
    -- CREATURES YOU CONTROL" narrows the source by a characteristic rather than
    -- by identity, which is the same gap CR 615.1's shields have on that axis
    -- (#588). Whippoorwill's recipient limb has no site to bake a recipient into
    -- this pattern (#845). Banefire's "the damage can't be prevented" is a
    -- different carrier again -- a self-referential clause of one resolution,
    -- the shape Pawl.Types.Counterability takes (#844).
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
  | -- | CR 701.23 / 613.11 / Leonin Arbiter: this player can't search libraries.
    --
    -- CR 611.1's third clause again -- searching is something the RULES let a
    -- player do while following an instruction, not a characteristic of any
    -- object -- which is why it sits here rather than in
    -- Pawl.Types.Modification.
    --
    -- WHICH library is not carried, and Leonin Arbiter's sentence names none
    -- either: it stops the player from searching, whoever owns the library, which
    -- is exactly what an unqualified prohibition on the SEARCHER does. A card
    -- that prohibited searching only some libraries would want a filter here
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
    CantCastMatching (Filter.Filter Keyword.Keyword)
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
    -- everything (`And []`), while Haakon, Stromgald Scourge's "Knight spells"
    -- and Liliana, Untouched by Death's "Zombie spells" would narrow it. Read
    -- through Pawl.Engine.PlayerEffect.matchesObject, the same read
    -- CantCastMatching makes of a card in a hand. What a NARROWING filter sees
    -- of a card in a graveyard is unobserved, since the one producer writes the
    -- predicate that is true of everything -- pawl's projection does not reach
    -- that zone (#160).
    --
    -- A PERMISSION, folded as a disjunction for CastAsThoughItHadFlash's reason:
    -- there is nothing for a second permission to outvote.
    --
    -- Not implemented: the PLAY-LANDS half of the same sentence ("you may play
    -- lands and cast spells from your graveyard"). A land is played and never
    -- cast (CR 305.1), so this arm reaches no land however its Filter reads, and
    -- the play side has no zone permission at all (#1364).
    CastFromGraveyard (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
