-- | CR 709.1 / 712.8 / 715.2: one set of printed characteristics. A card has one
-- or more of these; which of them are live depends on the card's Layout and on
-- where the object is, which is Pawl.Engine.Card's question and not this
-- module's.
--
-- Parametric in `card` so Pawl.Types.Card can close the loop at itself: the six
-- fields below that carry card-shaped payloads (a token a spell defines, an
-- ability's own modal payload) name a whole CARD, not a face, because CR 707.8a's
-- double-faced tokens carry two faces of their own and pawl represents a token
-- with the very same Card value. That keeps every `Modal Card` and `Effect Card`
-- in the codebase unchanged and avoids an hs-boot cycle.
module Pawl.Types.Face where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.UntapRestriction as UntapRestriction

data Face card = MkFace
  { name :: CardName.CardName,
    -- | Nothing, not a zero cost: CR 202.1, a land has no mana cost at all.
    manaCost :: Maybe ManaCost.ManaCost,
    typeLine :: TypeLine.TypeLine,
    -- | Only creatures have these.
    power :: Maybe Power.Power,
    toughness :: Maybe Toughness.Toughness,
    -- | CR 306.5 / 306.5a: Nothing for every card that is not a planeswalker; the
    -- CardSpec lint family holds that biconditional in both directions.
    --
    -- Read like power/toughness through Pawl.Engine.Projection, never directly,
    -- because CR 707.2 lists loyalty among the copiable values a Clone acquires.
    -- What reads it is CR 306.5b's intrinsic enters-with replacement, which needs
    -- the loyalty of the permanent as it WOULD exist on the battlefield (CR
    -- 614.12) rather than the loyalty printed on whatever card is underneath.
    loyalty :: Maybe Loyalty.Loyalty,
    -- | CR 210.1 / 310.4a: the number in the card's lower right corner. Nothing
    -- for every card that is not a battle; the CardSpec lint family holds that
    -- biconditional in both directions, exactly as it does for loyalty above.
    --
    -- Read through Pawl.Engine.Projection, never directly, for a REASON WEAKER
    -- than loyalty's: CR 707.2's list of copiable values names loyalty and stops
    -- short of defense. See Pawl.Types.Defense for why the projection is still
    -- where this belongs. What reads it is CR 310.4b's intrinsic enters-with
    -- replacement, which needs the defense of the permanent as it WOULD exist on
    -- the battlefield (CR 614.12).
    defense :: Maybe Defense.Defense,
    -- | CR 702. A Set because this is PRINTED text: a card names each keyword
    -- ability it has once, so there is no printed multiplicity to lose. Where
    -- multiplicity does arise -- the same ability printed and granted, which CR
    -- 702.164b's toxic SUMS rather than treating as redundant (contrast CR
    -- 702.3c/702.9c) -- it arises after the layer fold, and
    -- Pawl.Types.ProjectedCharacteristics.keywords counts it there.
    --
    -- The closed half must read this through Pawl.Engine.Projection.keywordsOf, never
    -- directly, since layer 6 grants and removes abilities. The exception is a
    -- keyword whose ability functions in a zone where no pool effect changes a
    -- card's keywords (#160) -- a HAND, where flash is read here -- the same carve-out
    -- castingPermissions and additionalCosts take. A GRAVEYARD is no longer one
    -- of those zones: rule 702.34a's flashback is read there through the
    -- projection, so a granted one reaches the cost (Pawl.Engine.Cost.costsFor).
    keywords :: Set.Set Keyword.Keyword,
    -- | CR 204.1/204.2: the colour indicator printed left of the type line. An
    -- object is each colour it denotes, IN ADDITION to the colours of its mana
    -- cost (CR 202.2/202.2e). Empty for a card whose colours come from its cost
    -- alone. Also where a TOKEN's colour lives, since CR 111.3 makes the creating
    -- effect's stated characteristics stand in for printed ones and a token has no
    -- mana cost. Read through Pawl.Engine.Projection.colorsOf, never directly.
    colorIndicator :: Set.Set Color.Color,
    -- | CR 604.3 / 208.2a: this face's characteristic-defining P/T ability -- what
    -- a printed star (Quantity.Star) in its power/toughness box stands for. A CDA
    -- is an ABILITY, not a number: the projection seeds it unevaluated so a copy
    -- acquires the ability (CR 707.2a) and layer 7a recomputes it every time. Read
    -- through Pawl.Engine.Projection, never directly.
    characteristicPT :: Maybe Quantity.Quantity,
    -- | CR 604.1/604.2: this face's static continuous abilities (Humility), which
    -- the projection gathers live.
    staticAbilities :: [StaticAbility.StaticAbility card],
    -- | This face's spell payload as data: what casting it does when it
    -- resolves, as one or more modes (CR 700.2). A non-modal card is a single mode
    -- with ChooseExactly 1 (forced, unprompted); a land or vanilla creature is a
    -- single EMPTY mode. Pawl.Types.Card ties Modal's `card` knot at `Modal Card`.
    spell :: Modal.Modal card,
    -- | CR 602: this face's printed activated abilities. The closed half reads
    -- these through Pawl.Engine.Projection.abilitiesOf, never directly: layer 6
    -- (Humility) removes abilities.
    activatedAbilities :: [ActivatedAbility.ActivatedAbility card],
    -- | CR 614: this face's replacement effects, active while it is on the
    -- battlefield. Read through Pawl.Engine.Projection.replacementsOf (never
    -- directly) so layer 6 LoseAllAbilities strips them uniformly, and so CR
    -- 604.2's "as long as" clause on each is asked against the live board.
    replacementEffects :: [PrintedReplacement.PrintedReplacement (Effect.Effect card)],
    -- | CR 603: this face's triggered abilities, read through
    -- Pawl.Engine.Projection.triggeredAbilitiesOf.
    triggeredAbilities :: [TriggeredAbility.TriggeredAbility card],
    -- | CR 603.7: this face's DELAYED triggered abilities, keyed by name -- the
    -- payloads an Effect.ArmDelayedTrigger in this face's own text arms. Card
    -- DATA, not an opcode payload: Effect is first-order and non-recursive
    -- (design.md section 1), and Effect -> TriggeredAbility -> Modal -> Mode ->
    -- Effect is a genuine module cycle.
    --
    -- Read straight from the card, never through the projection: a delayed
    -- ability is not ON the source object -- CR 603.7d gives it no source
    -- permanent to lose, so layer 6 cannot strip it.
    delayedAbilities :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility card),
    -- | CR 309.4: this face's rooms, topmost first -- the room graph of a dungeon
    -- card, and empty for every card that is not a dungeon. The CardSpec lint
    -- family holds that biconditional in both directions, as it does for loyalty
    -- and defense above.
    --
    -- ORDERED, where every other ability list here is ordered only by printing:
    -- CR 309.4a starts the venture marker on the topmost room and CR 309.6 ends the
    -- dungeon on the bottommost, so position is the rule's own handle on a room
    -- (Pawl.Types.RoomIndex).
    --
    -- Card DATA and not rulebook text, unlike the emblem Pawl.Engine.Ring mints:
    -- CR 309.1 makes a dungeon a card, and rooms are what is printed on it.
    rooms :: Seq.Seq (DungeonRoom.DungeonRoom card),
    -- | CR 601.3: this face's PRINTED casting permissions -- zone or condition
    -- exceptions to normal timing (Panglacial Wurm). Read directly from the card
    -- and NOT the projection: the permission functions in the library (CR 113.6),
    -- which Projection.gather does not reach -- it walks the battlefield, the
    -- command zone, the stack and the graveyard, and no other zone (#160).
    -- Not a claim about the rules: CR 613.1 names no zone, and CR 122.1b / 613.1f
    -- reach a card outside the battlefield.
    --
    -- Not the whole list: rule 702.34a gives a card with flashback a
    -- cast-from-your-graveyard permission that is never printed here, minted
    -- instead by Pawl.Engine.Keyword.castingPermissionsOf. Pawl.Engine.Cast is the
    -- one place the two are put together; a reader that wants every permission a
    -- card has must do the same.
    castingPermissions :: [CastingPermission.CastingPermission],
    -- | CR 601.3: this face's PRINTED casting restrictions -- the clauses of a
    -- "Cast this spell only during ..." that WITHHOLD a cast the rules would
    -- otherwise allow. Rally the Troops is
    -- `[DuringPhase (Combat DeclareAttackers), AttackedThisStep]`.
    --
    -- The mirror of castingPermissions above, running the other way, which is why
    -- it is a second field and not more arms on that one: CR 601.3 is one sentence
    -- with two halves (a rule or effect allows / no rule or effect prohibits), and
    -- Pawl.Engine.Cast has to know which half an entry belongs to. ALL of these
    -- must hold for a cast to be legal, where one permission suffices.
    --
    -- Read directly from the card, the castingPermissions precedent; CR 113.6e is
    -- the rule that puts such an ability in the hand, which pawl's projection does
    -- not reach (#160).
    --
    -- SELF-scoped and printed-only, which is the whole difference between this and
    -- the other producer CR 601.3's "rule or effect" names. A prohibition aimed at
    -- a PLAYER -- Rule of Law, Silence -- is a continuous effect on the CR 613.11
    -- axis (playerAbilities / Effect.AffectPlayers); every entry here is a card
    -- restricting only itself.
    castingRestrictions :: [CastingRestriction.CastingRestriction],
    -- | CR 702.5a: this face's `enchant` abilities, restricting what an Aura spell
    -- can target and what an Aura can enchant. Empty for every card that is not an
    -- Aura; the CardSpec lint family holds the biconditional both ways.
    --
    -- TargetSlots, not Filters, because CR 702.5d's enchant-player Auras need the
    -- Pool axis and TargetSlot already is {pool, filter}. A LIST, because CR 702.5c
    -- makes multiple instances of enchant all apply -- printed order, the order the
    -- other ability lists on this record keep. Pawl.Engine.Card.enchantTargetSlot
    -- is the conjunction every reader goes through, and the one place that rule is
    -- applied.
    enchant :: [TargetSlot.TargetSlot],
    -- | CR 113.6g: a can't-be-countered ability functions on the stack (Rending
    -- Volley). Read straight off the card by Event.counter rather than through the
    -- projection -- the castingPermissions precedent: a spell on the stack gets no
    -- projection in pawl either, since gather's static-ability sources are
    -- battlefield permanents and every dynamic affected set is battlefield-gated
    -- (#160). CR 613 itself does reach the stack; this is a fact about the engine,
    -- not about the rules.
    --
    -- CR 113.6g's SELF-referential clause only. A permanent's static ability
    -- about OTHER objects being uncounterable (Spider-Punk) rides
    -- playerAbilities below instead; Pawl.Types.Counterability argues why the two
    -- carriers cannot be merged.
    counterability :: Counterability.Counterability,
    -- | CR 118.8: this face's printed additional costs, paid at the same time as
    -- the spell's mana cost (Village Rites). Read directly from the card, the
    -- castingPermissions precedent: a cost is consulted while the object is in
    -- hand, where no pool effect changes a card's costs (#160). CR 118.8d: this does not
    -- change the card's mana cost, so 'manaCost' above and every reader of mana
    -- value is unaffected.
    --
    -- CR 118.8c reads these through Pawl.Engine.Cost.statesHiddenQuality, which is
    -- why a cost naming cards of a stated quality in a hidden zone (Magmatic
    -- Insight's discarded land) excuses a cast an effect instructs "if able".
    additionalCosts :: [CostComponent.CostComponent Keyword.Keyword],
    -- | CR 118.9: this face's printed alternative costs, which its controller MAY
    -- pay rather than the spell's mana cost (Fireblast). CR 118.9c: this does not
    -- change the card's mana cost.
    --
    -- A LIST because a card may print more than one, not because more than one may
    -- be paid: CR 118.9a applies only one alternative cost to any one spell, which
    -- is what makes Pawl.Engine.Cost.costsFor's list a list of CANDIDATES the
    -- caster picks from. Each carries its OWN mana part, which is how CR 118.6a's
    -- second sentence falls out of the shape -- Fireblast's is Just [], a real,
    -- taxable {0}, not Nothing, and Asmoranomardicadaistinaculdacar pays a {B/R}
    -- for a printed cost that is Nothing.
    --
    -- Printed-only, and deliberately so: an effect that APPLIES an alternative
    -- cost to a spell carries it elsewhere -- one-shot in Pawl.Types.CastOffer,
    -- standing in Pawl.Types.PlayerEffect's CastFromHandWithoutPayingManaCost --
    -- and Pawl.Engine.Cost.candidateCostsFor offers all three side by side. CR
    -- 118.9 is about SPELLS, so this lives on a face and never on
    -- ActivatedAbility -- a rules fact, not an elision.
    --
    -- CONDITIONED only where the card states a condition (CR 604.2), which is
    -- Pawl.Types.AlternativeCost's own field. Rule 702.34a's flashback cost is
    -- still deliberately NOT one of these: its gate is a ZONE rather than a
    -- condition over game state, so it rides its keyword and
    -- Pawl.Engine.Cost.costsFor offers it by zone.
    alternativeCosts :: [AlternativeCost.AlternativeCost],
    -- | CR 601.2f / 113.6d: this face's printed reductions of ITS OWN cost to
    -- cast -- Thrasta, Tempest's Roar's "This spell costs {3} less to cast for
    -- each other spell cast this turn". Pawl.Types.CostReduction argues why
    -- playerAbilities below cannot hold one.
    --
    -- The third member of the additionalCosts/alternativeCosts family above, and
    -- read the way they are: straight off the card, never through the projection
    -- (#160). Pawl.Engine.Cost.selfReductions is the one reader, and it folds these in
    -- alongside the CR 613.11 reductions other permanents generate, so CR
    -- 601.2f's "minus all cost reductions" is applied once over both.
    --
    -- A LIST because nothing in CR 601.2f caps how many such lines a face may
    -- print; every one of them applies (Edgewalker's ruling, one type over).
    costReductions :: [CostReduction.CostReduction],
    -- | CR 604.1/604.2 / 611.1: this face's printed PLAYER and RULES-modifying
    -- static abilities (Rule of Law, Thalia, Sapphire Medallion, Reliquary Tower).
    -- The sibling of staticAbilities on the axis CR 613.10/613.11 put OUTSIDE the
    -- layer system, so these are read by Pawl.Engine.PlayerEffect and never by
    -- Pawl.Engine.Projection.
    playerAbilities :: [PlayerStaticAbility.PlayerStaticAbility],
    -- | CR 604.1/604.2 / 509.1c: this face's printed BLOCKING REQUIREMENTS -- "all
    -- creatures able to block enchanted creature do so" (Lure).
    -- Pawl.Types.BlockRequirement argues why neither staticAbilities nor
    -- playerAbilities can hold one. Read by Pawl.Engine.BlockRequirement and never
    -- by Pawl.Engine.Projection, since CR 613.11 applies these after the layer
    -- system rather than inside it.
    blockRequirements :: [BlockRequirement.BlockRequirement],
    -- | CR 604.1/604.2 / 509.1a: this face's printed BLOCKING PERMISSIONS --
    -- "this creature can block an additional creature each combat" (Foriysian
    -- Brigade); read by Pawl.Engine.BlockPermission, never by
    -- Pawl.Engine.Projection, for blockRequirements' CR 613.11 reason.
    --
    -- Its own field rather than an arm of combatRestrictions below, because
    -- these ADD where a restriction BINDS: Pawl.Types.BlockPermission's header
    -- works that out.
    blockPermissions :: [BlockPermission.BlockPermission],
    -- | CR 604.1/604.2 / 508.1d: this face's printed ATTACKING REQUIREMENTS --
    -- "creatures enchanted player controls attack each combat if able" (Curse of
    -- the Nightly Hunt). The twin of blockRequirements on the other side of the
    -- combat phase; read by Pawl.Engine.AttackRequirement, never by
    -- Pawl.Engine.Projection, for that field's CR 613.11 reason.
    attackRequirements :: [AttackRequirement.AttackRequirement],
    -- | CR 604.1/604.2 / 508.1c / 509.1b: this face's printed COMBAT RESTRICTIONS
    -- -- "enchanted creature can't attack or block" (Pacifism); read by
    -- Pawl.Engine.CombatRestriction, never by Pawl.Engine.Projection, for
    -- blockRequirements' CR 613.11 reason. ONE list for both of Pacifism's halves,
    -- where the requirements take two fields: Pawl.Types.CombatRestriction argues
    -- why the axis that split those is absent here.
    combatRestrictions :: [CombatRestriction.CombatRestriction],
    -- | CR 604.1/604.2 / 701.21a / 101.2: this face's printed SACRIFICE
    -- PROHIBITIONS -- "creatures you control but don't own ... can't be
    -- sacrificed" (Garland, Royal Kidnapper); read by
    -- Pawl.Engine.SacrificeRestriction, never by Pawl.Engine.Projection, for
    -- blockRequirements' CR 613.11 reason.
    --
    -- Its own field rather than an arm of combatRestrictions above, because the
    -- two forbid unrelated game actions: a combat restriction is read only by
    -- the two declarations CR 508.1 and CR 509.1 describe, and this is read
    -- wherever CR 701.21a's sacrifice is reached from.
    sacrificeRestrictions :: [SacrificeRestriction.SacrificeRestriction],
    -- | CR 604.1/604.2 / 502.3 / 101.2: this face's printed UNTAP PROHIBITIONS --
    -- "each land with an activated ability that isn't a mana ability doesn't untap
    -- during its controller's untap step" (Tsabo's Web); read by
    -- Pawl.Engine.UntapRestriction, never by Pawl.Engine.Projection, for
    -- blockRequirements' CR 613.11 reason.
    --
    -- Its own field rather than an arm of sacrificeRestrictions above, for the
    -- reason that field gives one line up: the two forbid unrelated game actions,
    -- and this one is read at exactly one site, CR 502.3's turn-based action.
    --
    -- The STATIC prohibition only. The one-shots -- "doesn't untap during its
    -- controller's NEXT untap step" and CR 701.43a's exert -- ride the victim, as
    -- Object.doesNotUntapNext and Object.exertedBy, and neither reaches this
    -- field.
    untapRestrictions :: [UntapRestriction.UntapRestriction],
    -- | CR 604.1/604.2 / 508.1c / 508.1h: this face's printed COSTS TO ATTACK --
    -- Ghostly Prison's {2} per attacking creature; read by Pawl.Engine.AttackCost,
    -- never by Pawl.Engine.Projection, for blockRequirements' CR 613.11 reason.
    --
    -- Its own field rather than an arm of combatRestrictions above, for the reason
    -- Pawl.Types.AttackCost's header gives: a creature under one of these CAN
    -- attack, so it must stay on CR 508.1a's candidate list, where a
    -- CombatRestriction takes its subject off.
    attackCosts :: [AttackCost.AttackCost],
    -- | CR 103.5b: this face's "any time you could mulligan" actions, in printed
    -- order, each one its own list of effects in written order (Serum Powder).
    -- Read directly from the card, the castingPermissions precedent: the ability
    -- functions in the HAND (CR 113.6), where no pool effect changes a card's
    -- abilities (#160).
    --
    -- A LIST OF ACTIONS and not one action's effects: nothing in CR 103 caps how
    -- many such actions a face may grant, and two are two separate offers a
    -- player picks between, which is why Pawl.Types.HandActionIndex exists.
    -- An empty OUTER list means the face grants no such action at all, which is
    -- how a card that says nothing about this window is never offered one.
    mulliganActions :: [[Effect.Effect card]],
    -- | CR 103.6 / 103.6a: this face's opening-hand actions, shaped exactly like
    -- mulliganActions above and read the same way (CR 113.6, #160).
    --
    -- The SIBLING of mulliganActions, not a reuse: the two windows are at different
    -- times (CR 103.5b sits AT a declaration, CR 103.6 opens once the whole
    -- mulligan process is complete), and a card that acts at one must not be
    -- offered at the other.
    --
    -- No card grants two of these, so the two-action offer is proved on the CR
    -- 103.5b window only (#803).
    openingHandActions :: [[Effect.Effect card]],
    -- | CR 116.2: the special actions this face's printed text grants -- CR
    -- 116.2e's "you may discard this card any time you could cast an instant"
    -- (Circling Vultures). Read directly from the card, the castingPermissions
    -- precedent: the ability functions in the HAND (CR 113.6), where no pool
    -- effect changes a card's abilities (#160).
    --
    -- A LIST rather than a flag, matching every neighbouring permission field:
    -- nothing in CR 116.2 caps how many such lines a face may print, and
    -- Pawl.Types.SpecialAction says which rows will land here.
    specialActions :: [SpecialAction.SpecialAction]
  }
  deriving (Eq, Ord, Show)

-- | What a face that says nothing about its spell means: one mode with no
-- effects and no targets, chosen. Every land and vanilla creature has exactly
-- this, which is why Pawl.Codec.Face makes it the default rather than a required
-- key.
--
-- Its selection must agree with Pawl.Codec.Modal.defaultSelection, or the
-- "selection" key would be written out for every vanilla card in the corpus.
defaultSpell :: Modal.Modal card
defaultSpell =
  Modal.MkModal
    (Seq.singleton (Mode.MkMode Seq.empty Map.empty))
    (ModeSelection.ChooseExactly 1)
