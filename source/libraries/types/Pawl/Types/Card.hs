module Pawl.Types.Card where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TypeLine as TypeLine

data Card = MkCard
  { name :: Text.Text,
    -- | Nothing, not a zero cost: CR 202.1, a land has no mana cost at all.
    manaCost :: Maybe ManaCost.ManaCost,
    typeLine :: TypeLine.TypeLine,
    -- | Only creatures have these.
    power :: Maybe Power.Power,
    toughness :: Maybe Toughness.Toughness,
    -- | CR 306.5 / 306.5a: "Loyalty is a characteristic only planeswalkers have",
    -- and a planeswalker card's is "the number printed in its lower right
    -- corner". Nothing for every card that is not a planeswalker; the CardSpec
    -- lint family holds that biconditional in both directions.
    --
    -- The sibling of power/toughness above, and read the same way: through
    -- Pawl.Engine.Projection (ProjectedCharacteristics.loyalty), never directly,
    -- because CR 707.2 lists loyalty among the copiable values a Clone acquires.
    -- What reads it is CR 306.5b's intrinsic enters-with replacement, which needs
    -- the loyalty of the permanent as it WOULD exist on the battlefield (CR
    -- 614.12) rather than the loyalty printed on whatever card is underneath.
    loyalty :: Maybe Loyalty.Loyalty,
    -- | CR 702. A Set because this is PRINTED text: a card names each keyword
    -- ability it has once, so there is no printed multiplicity to lose. Where
    -- multiplicity does arise -- the same ability twice, once printed and once
    -- granted, which CR 702.164b's toxic SUMS rather than treating as redundant
    -- (contrast CR 702.3c/702.9c) -- it arises after the layer fold, and
    -- Pawl.Types.ProjectedCharacteristics.keywords counts it there.
    --
    -- The closed half must read this through Pawl.Engine.Projection.keywordsOf, never
    -- directly: layer 6 grants and removes abilities at M3. The exception is a
    -- keyword whose ability functions in a zone pawl's projection does not reach
    -- (#160) -- rule 702.34a's flashback, read here by Pawl.Engine.Cast and Pawl.Engine.Cost
    -- via Pawl.Engine.Keyword while the card sits in a graveyard -- which is the same
    -- carve-out castingPermissions and additionalCosts below already take.
    keywords :: Set.Set Keyword.Keyword,
    -- | CR 204.1/204.2: the colour indicator printed left of the type line. An
    -- object is each colour it denotes, IN ADDITION to the colours of its mana
    -- cost (CR 202.2/202.2e). Empty for a card whose colours come from its cost
    -- alone. This is also where a TOKEN's colour lives: CR 111.3 makes the
    -- creating effect's stated characteristics "functionally equivalent to the
    -- characteristic values that are printed on a card", and a token has no mana
    -- cost. Read through Pawl.Engine.Projection.colorsOf, never directly.
    colorIndicator :: Set.Set Color.Color,
    -- | CR 604.3 / 208.2a: this card's characteristic-defining P/T ability -- the
    -- quantity a printed star (Quantity.Star) in its power/toughness box stands
    -- for. Nothing for every card without a star. A CDA is an ABILITY, not a
    -- number: the projection seeds it unevaluated so a copy acquires the ability
    -- (CR 707.2a) and layer 7a recomputes it on every projection. Read through
    -- Pawl.Engine.Projection, never directly.
    characteristicPT :: Maybe Quantity.Quantity,
    -- | CR 604.1/604.2: this card's static continuous abilities (Humility). Empty
    -- for everything but the few printings that generate a continuous effect just
    -- by being on the battlefield. The projection gathers these live.
    staticAbilities :: [StaticAbility.StaticAbility],
    -- | The card's spell payload as data: what casting this card does when it
    -- resolves, as one or more modes (CR 700.2). A non-modal card -- every card
    -- before M4g -- is a single mode with ChooseExactly 1 (forced, unprompted). A
    -- land or vanilla creature is a single EMPTY mode (no spell effects; resolution
    -- just enters the battlefield). Card ties Modal's `card` knot at `Modal Card`.
    spell :: Modal.Modal Card,
    -- | CR 602: this card's printed activated abilities. Empty for all but the few
    -- printings that grant one. The closed half reads these through
    -- Pawl.Engine.Projection.abilitiesOf (Task 9), never directly: layer 6 (Humility)
    -- removes abilities.
    activatedAbilities :: [ActivatedAbility.ActivatedAbility Card],
    -- | CR 614: this card's replacement effects, active while it is on the
    -- battlefield. Read through Pawl.Engine.Projection.replacementsOf (never directly)
    -- so layer 6 LoseAllAbilities strips them uniformly. Empty for all but Rest
    -- in Peace.
    replacementEffects :: [ReplacementEffect.ReplacementEffect],
    -- | CR 603: this card's triggered abilities, read through
    -- Pawl.Engine.Projection.triggeredAbilitiesOf. Empty for all but Rest in Peace.
    triggeredAbilities :: [TriggeredAbility.TriggeredAbility Card],
    -- | CR 603.7: this card's DELAYED triggered abilities, keyed by name -- the
    -- payloads an Effect.ArmDelayedTrigger in this card's own text arms. Card
    -- DATA, not an opcode payload: Effect is first-order and non-recursive
    -- (design.md section 1), and Effect -> TriggeredAbility -> Modal -> Mode ->
    -- Effect is a genuine module cycle. Empty for all but Tidal Wave, Full
    -- Throttle and Meandering Towershell.
    --
    -- Read straight from the card, never through the projection: a delayed
    -- ability is not ON the source object -- CR 603.7d gives it no source
    -- permanent to lose, so layer 6 cannot strip it.
    delayedAbilities :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility Card),
    -- | CR 601.3: this card's PRINTED casting permissions -- zone/condition
    -- exceptions to normal timing. Read directly from the card (NOT the
    -- projection): the permission functions in the library (CR 113.6), which
    -- pawl's projection does not reach -- Projection.gather walks the
    -- battlefield only, so a projected read there returns exactly the printed
    -- seed (#160). Not a claim about the rules: CR 613.1 names no zone, and CR
    -- 122.1b / 613.1f reach a card outside the battlefield. Empty for all but
    -- Panglacial Wurm.
    --
    -- Not the whole list: rule 702.34a gives a card with flashback a
    -- cast-from-your-graveyard permission that is never printed here, which
    -- Pawl.Engine.Keyword.castingPermissionsOf mints from the keyword. Pawl.Engine.Cast
    -- (permissionsOf) is the one place the two are put together; a reader that
    -- wants every permission a card has must do the same.
    castingPermissions :: [CastingPermission.CastingPermission],
    -- | CR 601.3: this card's PRINTED casting restrictions -- the clauses of a
    -- "Cast this spell only during ..." that WITHHOLD a cast the rules would
    -- otherwise allow. Rally the Troops is
    -- `[DuringPhase (Combat DeclareAttackers), AttackedThisStep]`; empty for
    -- every other printing.
    --
    -- The mirror of castingPermissions above, running the other way, which is why
    -- it is a second field and not more arms on that one: CR 601.3 is one sentence
    -- with two halves ("a rule or effect allows" / "no rule or effect prohibits"),
    -- and Pawl.Engine.Cast has to know which half an entry belongs to. ALL of these must
    -- hold for a cast to be legal, where one permission suffices.
    --
    -- Read directly from the card and never through the projection, the
    -- castingPermissions precedent. CR 113.6e is the rule that says so in as many
    -- words: "an object's ability that restricts or modifies how that particular
    -- object can be played or cast functions in any zone from which it could be
    -- played or cast and also on the stack" -- a hand, which pawl's projection
    -- does not reach (#160).
    --
    -- SELF-scoped and printed-only, which is the whole difference between this
    -- and the other producer CR 601.3's "rule or effect" names. A prohibition
    -- aimed at a PLAYER -- Rule of Law, Silence -- is a continuous effect on the
    -- CR 613.11 axis (playerAbilities / Effect.AffectPlayers, read by
    -- Pawl.Engine.PlayerEffect); every entry here is a card restricting only itself.
    castingRestrictions :: [CastingRestriction.CastingRestriction],
    -- | CR 702.5a: this card's `enchant` ability -- "Enchant [object or player]"
    -- -- which "restricts what an Aura spell can target and what an Aura can
    -- enchant". Nothing for every card that is not an Aura; the CardSpec lint
    -- family holds the biconditional (Aura iff enchant) in both directions.
    --
    -- A TargetSpec, not a Filter, because CR 702.5d's enchant-player Auras need
    -- the Pool axis and TargetSpec already is {pool, filter}.
    --
    -- SINGULAR: CR 702.5c's "multiple instances of enchant, all of them apply"
    -- is unrepresentable, and no card in this pool prints two (#189).
    enchant :: Maybe TargetSpec.TargetSpec,
    -- | CR 113.6g: "an object's ability that states it can't be countered …
    -- functions on the stack" (Rending Volley). Counterable for every other
    -- printing, and read straight off the card by Event.counter rather than
    -- through the projection -- the castingPermissions precedent: a spell on the
    -- stack gets no projection in pawl either, since gather's static-ability
    -- sources are battlefield permanents and every dynamic affected set is
    -- battlefield-gated (#160). CR 613 itself does reach the stack; this is a
    -- fact about the engine, not about the rules.
    counterability :: Counterability.Counterability,
    -- | CR 118.8: this card's printed additional costs -- "a cost listed in a
    -- spell's rules text ... that its controller must pay at the same time they
    -- pay the spell's mana cost" (Village Rites). Empty for every other
    -- printing.
    --
    -- Read DIRECTLY from the card and never through the projection, the
    -- castingPermissions precedent: a cost is consulted while the object is in
    -- hand, which pawl's projection does not reach (#160).
    --
    -- CR 118.8d: this does not change the card's mana cost. Card.manaCost, and
    -- every reader of mana value, is unaffected.
    additionalCosts :: [CostComponent.CostComponent Keyword.Keyword],
    -- | CR 118.9: this card's printed alternative costs -- "a cost listed in a
    -- spell's text ... that its controller MAY pay rather than paying the
    -- spell's mana cost" (Fireblast). Empty for every other printing.
    --
    -- A LIST because a card may print more than one, not because more than one
    -- may be paid: CR 118.9a says "only one alternative cost can be applied to
    -- any one spell as it's being cast", which is what makes
    -- Pawl.Engine.Cost.costsFor's list a list of CANDIDATES the caster picks from.
    --
    -- Each carries its OWN mana part, which is how CR 118.6a's second sentence
    -- ("if an alternative cost is applied to an unpayable cost ... the
    -- alternative cost may be paid") falls out of the shape. Fireblast's is
    -- Just [] -- a real, taxable {0}, not Nothing.
    --
    -- CR 118.9c: this does not change the card's mana cost.
    --
    -- Printed-only: an effect that GRANTS an alternative cost has no carrier
    -- here (#103). CR 118.9's first sentence is "Some SPELLS have alternative
    -- costs", so this lives on Card and never on ActivatedAbility -- a rules
    -- fact, not an elision.
    --
    -- UNCONDITIONED, which is why rule 702.34a's flashback cost is deliberately
    -- NOT one of these: a cost here is payable wherever the card can be cast
    -- from, and flashback's may be paid only from the graveyard. It rides its
    -- keyword instead, and Pawl.Engine.Cost.costsFor offers it by zone.
    alternativeCosts :: [Cost.Cost Keyword.Keyword],
    -- | CR 604.1/604.2 / 611.1: this card's printed PLAYER and RULES-modifying
    -- static abilities (Rule of Law, Thalia, Sapphire Medallion, Reliquary
    -- Tower). The sibling of staticAbilities on the axis CR 613.10/613.11 put
    -- OUTSIDE the layer system, so these are read by Pawl.Engine.PlayerEffect and never
    -- by Pawl.Engine.Projection. Empty for every other printing.
    playerAbilities :: [PlayerStaticAbility.PlayerStaticAbility],
    -- | CR 604.1/604.2 / 509.1c: this card's printed BLOCKING REQUIREMENTS -- "all
    -- creatures able to block enchanted creature do so" (Lure). The THIRD
    -- printed-static-ability field, alongside staticAbilities and
    -- playerAbilities; Pawl.Types.BlockRequirement argues why neither of those two
    -- can hold one. Read by Pawl.Engine.BlockRequirement and never by Pawl.Engine.Projection,
    -- since CR 613.11 applies these after the layer system rather than inside it.
    -- Empty for every other printing.
    blockRequirements :: [BlockRequirement.BlockRequirement],
    -- | CR 604.1/604.2 / 508.1d: this card's printed ATTACKING REQUIREMENTS --
    -- "creatures enchanted player controls attack each combat if able" (Curse of
    -- the Nightly Hunt). The FOURTH printed-static-ability field, and the twin of
    -- blockRequirements on the other side of the combat phase; read by
    -- Pawl.Engine.AttackRequirement and never by Pawl.Engine.Projection, since CR 613.11 applies
    -- these after the layer system rather than inside it. Empty for every other
    -- printing.
    attackRequirements :: [AttackRequirement.AttackRequirement],
    -- | CR 604.1/604.2 / 508.1c / 509.1b: this card's printed COMBAT RESTRICTIONS
    -- -- "enchanted creature can't attack or block" (Pacifism). The FIFTH
    -- printed-static-ability field; read by Pawl.Engine.CombatRestriction and never by
    -- Pawl.Engine.Projection, since CR 613.11 applies these after the layer system
    -- rather than inside it. Empty for every other printing.
    --
    -- ONE list for both of Pacifism's halves, where the requirements take two
    -- fields: Pawl.Types.CombatRestriction argues why the axis that split those is
    -- absent here.
    combatRestrictions :: [CombatRestriction.CombatRestriction],
    -- | CR 103.5b: the effects of this card's "any time you could mulligan"
    -- action, in written order. Empty for every printing but Serum Powder.
    --
    -- Read DIRECTLY from the card and never through the projection -- the
    -- castingPermissions / additionalCosts precedent: the ability functions in
    -- the HAND (CR 113.6), which pawl's projection does not reach (#160).
    --
    -- An empty list means NO action, not an action that does nothing: the two
    -- are indistinguishable in play, so the ambiguity costs nothing.
    --
    -- One action per card. A printing declaring two is unrepresentable (#183).
    mulliganAction :: [Effect.Effect Card],
    -- | CR 103.6: the effects of this card's opening-hand action, in written
    -- order -- what "you may begin the game with it on the battlefield" (CR
    -- 103.6a) does when the player takes it. Empty for every printing but
    -- Leyline of the Void.
    --
    -- Read DIRECTLY from the card and never through the projection, the
    -- mulliganAction / castingPermissions precedent: the ability functions in
    -- the HAND (CR 113.6), which pawl's projection does not reach (#160).
    --
    -- The SIBLING of mulliganAction, not a reuse: the two windows are at
    -- different times (CR 103.5b sits AT a declaration, CR 103.6 opens once the
    -- whole mulligan process is complete), and a card that acts at one must not
    -- be offered at the other.
    --
    -- One action per card, the same shape and the same caveat (#183).
    openingHandAction :: [Effect.Effect Card]
  }
  deriving (Eq, Ord, Show)
