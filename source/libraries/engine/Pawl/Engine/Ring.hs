-- | CR 701.54, "the Ring tempts you": the Ring-bearer designation, the emblem
-- named The Ring, and the count of how often a player has been tempted.
--
-- The per-player sibling of Pawl.Engine.Monarch. Both hold a designation that
-- rides no card's text, and both are named by a rule number, so
-- casing on either here is casing on the RULEBOOK -- rule 701 is a keyword-action
-- rule exactly as rule 702 is a keyword rule, and Pawl.Engine.Keyword's standing
-- to mint an ability from a Keyword is this module's standing to mint the emblem
-- from CR 701.54c. The closed/open invariant forbids the rules core casing on an
-- EFFECT's identity, and nothing here does: Pawl.Engine.Resolve's
-- Effect.TemptWithTheRing arm calls `tempt` and asks nothing about which effect it
-- came from.
--
-- Where they DIVERGE is WHAT the designation is on, and the two rules say so in
-- the same words about different things: CR 701.54b, "Ring-bearer is a designation
-- A PERMANENT can have", against CR 725.1, "the monarch is a designation A PLAYER
-- can have". The storage follows from that rather than the other way round -- this
-- rides Object.ringBearerFor and GameState grows no field, where the monarch is a
-- GameState field. The count rides Player.ringTemptations, being a player's.
module Pawl.Engine.Ring where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone

-- CR 701.54c: the emblem's name, and the whole of CR 701.54c's presence test.
-- CR 114.3's "most emblems also have no name" is the rule making room for this
-- one to have one.
theRingName :: CardName.CardName
theRingName = CardName.MkCardName (Text.pack "The Ring")

-- | CR 701.54c: the emblem named The Ring, as the card whose abilities it is (CR
-- 114.3: an emblem's characteristics ARE its abilities). Minted by this module
-- rather than carried in card data, on Pawl.Engine.Keyword's terms: the emblem's
-- text is printed in the comprehensive rules, not on Birthday Escape.
--
-- Every field but the name is at its empty value, which is CR 114.3 stated
-- directly -- "an emblem has no types, no mana cost, and no color".
--
-- CR 701.54c's base ability is both clauses of one sentence, carried in the two
-- places their two shapes belong: "your Ring-bearer is legendary" is
-- `theRingIsLegendary` below, the whole of `staticAbilities`, and "and can't be
-- blocked by creatures with greater power" is
-- `theRingCantBeBlockedByGreaterPower`, the whole of `combatRestrictions` -- CR
-- 613.11 puts a combat restriction outside the layers, so no static ability could
-- have held it. CR 114.4 is what makes either do anything at all on an object in
-- the command zone -- "abilities of emblems function in the command zone" -- and
-- Pawl.Engine.Projection.gatherGiven,
-- Pawl.Engine.CombatRestriction.inForce and Pawl.Engine.Event.eventTriggers each
-- walk the command zone for exactly that.
--
-- CR 701.54c's three remaining sentences each begin "as long as the Ring has
-- tempted that player N or more times, it HAS", so the emblem's ability set is a
-- function of the count and this takes it as an argument. `tempt` re-mints the
-- emblem's card at every temptation, which is why the count needs to reach no
-- reader but this one.
--
-- Not implemented: the two- and three-temptation abilities (#706). Both need a
-- TriggerCondition arm that does not exist -- a bystander "a permanent the Filter
-- admits attacks" and a bystander "becomes blocked by a creature"; every attack
-- and block arm the type has today is self-scoped. The scan that gathers the
-- four-temptation one gathers those too once they exist: Event.eventTriggers'
-- `inCommand` source offers every triggered ability of an emblem, unfiltered.
theRingEmblem :: Natural.Natural -> Card.Card
theRingEmblem temptations =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          Face.MkFace
            { Face.name = theRingName,
              Face.manaCost = Nothing,
              Face.typeLine = TypeLine.empty,
              Face.power = Nothing,
              Face.toughness = Nothing,
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.empty,
              Face.characteristicPT = Nothing,
              Face.staticAbilities = [theRingIsLegendary],
              Face.spell = Face.defaultSpell,
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              -- CR 701.54c's fourth sentence, and only once its threshold is
              -- met: "as long as the Ring has tempted that player four or more
              -- times, it has ...". Written as the rule's own inequality rather
              -- than an equality, so a fifth temptation does not take the ability
              -- away again.
              Face.triggeredAbilities = [theRingDrainsOnCombatDamage | temptations >= 4],
              Face.delayedAbilities = Map.empty,
              Face.rooms = Seq.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.maximumX = Nothing,
              Face.alternativeCosts = [],
              Face.costReductions = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.blockPermissions = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [theRingCantBeBlockedByGreaterPower],
              Face.sacrificeRestrictions = [],
              Face.untapRestrictions = [],
              Face.attachRestrictions = [],
              Face.attackCosts = [],
              Face.blockCosts = [],
              Face.mulliganActions = [],
              Face.openingHandActions = [],
              Face.specialActions = []
            }
    }

-- | CR 701.54e's "is your Ring-bearer", as a Filter: the two conjuncts a Filter
-- can carry, beside the battlefield one Affected.Matching and
-- TriggerCondition.PermanentDealsCombatDamageToPlayer each supply themselves.
-- ControlledBy You is "under your control", whose "you" is CR 109.5's -- the
-- emblem's controller, which CR 114.2 makes the player it was minted for; and
-- IsRingBearer is "has the Ring-bearer designation", asked of that same
-- perspective. The control conjunct is the RULE and not belt-and-braces: CR
-- 701.54e states it in as many words.
--
-- ONE binding for all three of the emblem's abilities, because every one of them
-- says "your Ring-bearer" and they must not drift apart: a clause reaching a
-- creature another does not would be a Ring-bearer that is legendary but
-- blockable, or that connects but drains nobody.
--
-- No creature-ness conjunct, for the reason isRingBearerOf below gives at length:
-- CR 701.54e's "a creature" is the antecedent of the phrase, not a fourth
-- condition, and none of rule 701.54c's own sentences adds one either. A
-- Ring-bearer turned into a land by Song of the Dryads is a legendary land.
yourRingBearer :: Filter.Filter Keyword.Keyword
yourRingBearer =
  Filter.And
    [ Filter.IsRingBearer,
      Filter.ControlledBy PlayerRelation.You
    ]

-- | CR 701.54c's four-temptation sentence: "Whenever your Ring-bearer deals combat
-- damage to a player, each opponent loses 3 life." Minted here rather than carried
-- in card data for theRingIsLegendary's reason.
--
-- TriggerCondition.PermanentDealsCombatDamageToPlayer is a BYSTANDER condition --
-- the emblem is not the damager, and CR 114.3 leaves it no power to deal damage
-- with -- so the Filter is what "your Ring-bearer" comes to, and it is the shared
-- `yourRingBearer` above. That condition's Filter is read against the DAMAGER
-- with the trigger's controller as CR 109.5's perspective, which for an emblem is
-- its owner (CR 114.2), so "your" lands on the tempted player.
--
-- Effect.LoseLife and not damage: rule 701.54c says "loses 3 life", so this is CR
-- 119.3's life loss and none of CR 120's damage machinery sees it. PlayerRef's
-- Relative Opponent is "each opponent" -- CR 102.2's other player, and CR 102.3's
-- every player not on your team -- read against that same controller, and NOT
-- against the player who was dealt the damage: the rule drains the whole table
-- and only coincides with the damaged seat in a two-player game.
--
-- Not proved in a two-player suite, which is the only one Pawl.RingSpec builds
-- here: "each opponent" and "the defending player" are one seat there, so the
-- clause is a regression fence rather than a proved behaviour.
theRingDrainsOnCombatDamage :: TriggeredAbility.TriggeredAbility Card.Card
theRingDrainsOnCombatDamage =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.PermanentDealsCombatDamageToPlayer yourRingBearer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      -- No intervening "if" (CR 603.4): rule 701.54c gives the emblem the ability
      -- or does not, and an ability that exists and declines to trigger is a
      -- different thing.
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.LoseLife
        ( PlayerQuantity.MkPlayerQuantity
            (PlayerRef.Relative PlayerRelation.Opponent)
            (Quantity.Literal 3)
        )

-- | CR 701.54c's first clause, "Your Ring-bearer is legendary", as the emblem's one
-- static ability. Rulebook text minted here rather than card data, on this module's
-- own terms: rule 701.54c prints The Ring's text, and Birthday Escape does not.
--
-- The affected set is `yourRingBearer` above, beside the battlefield conjunct
-- Affected.Matching carries itself -- CR 701.54e's three, and no more.
--
-- One AddSupertype and no more, per CR 205.4b: a supertype is gained and never set,
-- so the Ring-bearer keeps whatever supertypes it already had. Unconditional, since
-- CR 701.54c gates the two-, three- and four-temptation abilities on a temptation
-- count and this one on nothing.
theRingIsLegendary :: StaticAbility.StaticAbility Card.Card
theRingIsLegendary =
  StaticAbility.MkStaticAbility
    { StaticAbility.affected = Affected.Matching yourRingBearer,
      StaticAbility.condition = Nothing,
      -- CR 113.6b: the emblem's ability states no zone, so CR 114.4's command
      -- zone -- the default gatherGiven's emblem walk supplies -- stands.
      StaticAbility.functionsFrom = Set.empty,
      -- CR 604.2 as written: the emblem's ability is not a card's text saying
      -- its effect outlives the emblem, and CR 114.4 keeps the emblem in the
      -- command zone anyway.
      StaticAbility.lingers = Nothing,
      StaticAbility.modifications = NonEmpty.singleton (Modification.AddSupertype Supertype.Legendary)
    }

-- | CR 701.54c's second clause, "and can't be blocked by creatures with greater
-- power", as the emblem's one combat restriction. Rulebook text minted here for
-- theRingIsLegendary's reason.
--
-- The SAME affected set as theRingIsLegendary, because it is the same sentence's
-- subject: `yourRingBearer` above, which is why that binding exists rather than
-- the filter being written out twice.
--
-- Filter.PowerGreaterThanSource is "with greater power" exactly, and strictly:
-- CR 701.54c says greater, so a blocker of EQUAL power blocks legally. The source
-- it compares against is the Ring-bearer, supplied by
-- Pawl.Engine.CombatRestriction.cantBeBlockedBy -- the emblem has no power to
-- compare against (CR 114.3).
--
-- Ungated, since CR 701.54c gates the two-, three- and four-temptation abilities
-- on a temptation count and this one on nothing.
theRingCantBeBlockedByGreaterPower :: CombatRestriction.CombatRestriction
theRingCantBeBlockedByGreaterPower =
  CombatRestriction.CantBeBlockedBy
    CantBeBlockedBy.MkCantBeBlockedBy
      { CantBeBlockedBy.affected = Affected.Matching yourRingBearer,
        CantBeBlockedBy.blockers = Filter.PowerGreaterThanSource,
        CantBeBlockedBy.unless = Nothing
      }

-- CR 701.54c: this player's emblems named The Ring.
--
-- By NAME, which is the rule's own test, rather than by comparing the whole card
-- against theRingEmblem above. Comparing cards would answer alike for a fixed
-- emblem and stop the moment its abilities change under it -- a card is only
-- equal to itself, while the rule asks about the name however the emblem's text
-- grows. The text now DOES grow, `theRingEmblem` being a function of the count,
-- so this is load-bearing rather than anticipatory.
--
-- zoneMembers slices the command zone by OWNER, which is the right cut: CR 114.2
-- makes an emblem owned and controlled by the player who got it, and nothing
-- moves an emblem afterwards.
theRings :: PlayerId -> GameState.GameState -> [ObjectId]
theRings pid gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just theRingName
   in filter named (Game.zoneMembers Zone.Command pid gs)

-- CR 701.54c: does this player already have an emblem named The Ring?
hasTheRing :: PlayerId -> GameState.GameState -> Bool
hasTheRing pid gs = not (null (theRings pid gs))

-- CR 701.54c: how many times the Ring has tempted this player. Zero for a player
-- not in the game, which no caller here reaches.
temptationsOf :: PlayerId -> GameState.GameState -> Natural.Natural
temptationsOf pid gs = maybe 0 Player.ringTemptations (Map.lookup pid (GameState.players gs))

-- CR 701.54c's three "as long as" sentences, applied: rewrite this player's
-- emblem to the card their CURRENT temptation count entitles them to.
--
-- A rewrite of the existing object's Source.OfEmblem card rather than a fresh
-- emblem, so CR 613.7a's entry timestamp and the object's identity both survive
-- -- "it HAS" says the emblem gains an ability, not that it is replaced. Nothing
-- else about the object changes, an emblem carrying no per-incarnation state
-- worth preserving beyond that.
--
-- The alternative rejected here: mint all four abilities up front and gate the
-- three tiers with an intervening "if" (CR 603.4). The two are observationally
-- equivalent only because ringTemptations never decreases -- an ability the
-- emblem does not have and one that exists and declines to trigger differ the
-- moment a count can fall -- and CR 701.54c says the emblem does not have it.
refreshTheRing :: PlayerId -> Game ()
refreshTheRing pid =
  State.modify' $ \gs ->
    let card = theRingEmblem (temptationsOf pid gs)
        rewrite :: ObjectId -> Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
        rewrite = Map.adjust (\o -> o {Object.source = Source.OfEmblem card})
     in gs {GameState.objects = foldr rewrite (GameState.objects gs) (theRings pid gs)}

-- CR 701.54a: this creature becomes that player's Ring-bearer.
--
-- The clear-then-set is CR 701.54a's FIRST ending, "until another creature becomes
-- your Ring-bearer": a player has at most one Ring-bearer, so the mark is lifted
-- from whatever carried it for THIS player as the new one takes it. Scoped to that
-- player, never a blanket clear -- two players each have their own Ring-bearer, and
-- one temptation must not disturb the other's.
--
-- Sweeps every object rather than only the battlefield, so a designated permanent
-- that has since left play cannot keep a mark the sweep would miss. Nothing else
-- would notice today (CR 400.7 makes the leaver a new object and Object.newIncarnation
-- already clears it), which is the point: the invariant is "at most one per
-- player" and it should not depend on that.
designate :: PlayerId -> ObjectId -> Game ()
designate pid oid =
  State.modify' $ \gs ->
    let unmark o = if Object.ringBearerFor o == Just pid then o {Object.ringBearerFor = Nothing} else o
        mark o = o {Object.ringBearerFor = Just pid}
     in gs {GameState.objects = Map.adjust mark oid (fmap unmark (GameState.objects gs))}

-- | CR 701.54: the Ring tempts a player. The whole keyword action, in the order
-- CR 701.54c fixes -- the emblem BEFORE the choice ("they get an emblem named The
-- Ring before choosing a creature to be their Ring-bearer"), so a player being
-- tempted for the first time has the emblem while they choose.
--
-- Runs to the end whatever happens in the middle, which is CR 701.54d: "the Ring
-- tempts a player whenever they complete the actions in 701.54a, EVEN IF SOME OR
-- ALL OF THOSE ACTIONS WERE IMPOSSIBLE". A player controlling no creature is
-- tempted, gets the emblem, is asked nothing, designates nothing, and still has
-- their count go up. That is why the count is bumped unconditionally at the end
-- rather than inside the branch that designates.
--
-- The candidate list is ascending, so both the single-candidate shortcut and a
-- transcript are deterministic -- Resolve's AttachTarget and PlayerSacrifices
-- posture. Creature-ness is the PROJECTED question (CR 613.1d), so an
-- Opalescence'd enchantment is a legal choice.
--
-- Not implemented: nothing records that a temptation happened, so CR 701.54d's
-- "Whenever the Ring tempts you" abilities cannot trigger (#708).
tempt :: PlayerId -> Game ()
tempt pid = do
  gs0 <- State.get
  -- CR 701.54c, first: the emblem, and only if they have none. Minted at the
  -- count they have BEFORE this temptation, which refreshTheRing below then
  -- raises -- the emblem arrives before the choice, and this temptation has not
  -- been completed (CR 701.54d) until the choice is made.
  Monad.unless (hasTheRing pid gs0) (Monad.void (Event.createEmblem pid (theRingEmblem (temptationsOf pid gs0))))
  gs1 <- State.get
  let candidates = List.sort (filter (\oid -> Projection.isCreatureOf oid gs1) (Projection.controls pid gs1))
  case candidates of
    -- CR 701.54d: an impossible choice is not a failed temptation.
    [] -> pure ()
    first : rest -> do
      chosen <- case rest of
        -- One creature is the whole of "a creature you control", and rule 701.54a
        -- is not a "may" -- where the rules leave nothing to ask, don't prompt.
        [] -> pure first
        second : more -> do
          let offered = first NonEmpty.:| (second : more)
          -- FILTERED, NOT TRUSTED, the ChooseAttachment posture: an answer naming
          -- something never offered falls back to the first candidate, since the
          -- action is mandatory and must designate someone.
          answer <- Game.choose (Prompt.ChooseRingBearer (Decide.deciderFor pid gs1) pid offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      designate pid chosen
  -- CR 701.54d: the temptation itself, which is what a count of temptations
  -- counts.
  State.modify'
    ( \g ->
        g
          { GameState.players =
              Map.adjust (\p -> p {Player.ringTemptations = Player.ringTemptations p + 1}) pid (GameState.players g)
          }
    )
  -- CR 701.54c's "as long as", re-read against the count this temptation just
  -- raised: the emblem gains a tier the moment its threshold is crossed.
  refreshTheRing pid

-- | CR 701.54a's SECOND ending: "until ... another player gains control of it".
-- Lift the designation from every permanent whose controller is no longer the
-- player it was designated for.
--
-- A SAMPLE of derived state, and a near-clone of Engine.checkControlContinuity
-- (CR 302.6) for the same reason: control is computed by CR 613.1b's layer 2, so
-- nothing announces a change to it, while this designation is stored.
-- Engine.sampleControl mints a GameEvent.ControlChanged off the same difference for
-- CR 603.2's benefit; this stays a sampler for the reason given there.
--
-- Running in the settle loop is indistinguishable from checking continuously
-- TODAY, and the argument is now about the readers there are rather than about
-- there being none. Both of them -- theRingIsLegendary above, and isRingBearerOf
-- below -- ask CR 701.54e's "under your control" alongside the designation, so a
-- mark this sweep has not yet lifted is a mark whose controller has already
-- changed, and both answer False on it regardless. Pawl.RingSpec pins that window
-- open with an unsettled Act of Treason. What the sweep buys is that the mark does
-- not come BACK when control does; no single projection can ask that.
--
-- The claim has to be re-argued for the first reader that drops the control
-- conjunct -- CR 704.3 is about when state-based actions are CHECKED and settles
-- nothing about how finely a designation can be observed.
--
-- ONLY EVER CLEARS. That is the rule, not a conservatism: 701.54a ENDS the
-- designation when another player gains control, so a creature borrowed and handed
-- back (Act of Treason) is not the Ring-bearer again when it comes home. Reading
-- CR 701.54e's "under your control" at each use instead of clearing here would give
-- it back, which is why the mark remembers the player it was made for rather than
-- being a bare flag.
--
-- Scoped to the battlefield, matching CR 701.54e's own scope: an object elsewhere
-- has no controller to compare, and CR 400.7 already dropped the mark on its way
-- out.
--
-- The DESIGNATED permanents are gathered first, off a stored field, and the
-- control read happens only if there are any (#200's posture, at the one place a
-- third per-settle sample was added to a loop that already pays two). Almost every
-- board has no Ring-bearer at all, and such a board pays one battlefield walk and
-- no projection.
endOnControlChange :: Game ()
endOnControlChange = do
  gs <- State.get
  let marked =
        Maybe.mapMaybe
          (\oid -> fmap ((,) oid) (Game.lookupObject oid gs >>= Object.ringBearerFor))
          (Set.toList (GameState.battlefield gs))
  Monad.unless (null marked) $ do
    let grants = Projection.controlGrants gs
        lapsed (oid, p) objs =
          if Projection.controllerOfGiven grants Set.empty oid gs == Just p
            then objs
            else Map.adjust (\o -> o {Object.ringBearerFor = Nothing}) oid objs
    State.put gs {GameState.objects = foldr lapsed (GameState.objects gs) marked}

-- | CR 701.54e: is this permanent that player's Ring-bearer? True for one on the
-- battlefield under their control carrying the designation, which is the rule's
-- three conjuncts and no more.
--
-- No creature-ness test, deliberately. CR 701.54e's subject is "a creature", but
-- that is the ANTECEDENT -- "some abilities check to see if a creature 'is your
-- Ring-bearer'" -- and the condition it then states is exactly these three. The
-- designation itself outlives creature-ness, since CR 701.54a ends it only when
-- another creature takes it or another player gains control; a Ring-bearer turned
-- into a land by Song of the Dryads is still designated. The caller supplies the
-- creature restriction its own printed sentence carries.
--
-- The IMPERATIVE spelling of the same three conjuncts `yourRingBearer` above
-- spells as a Filter, for a caller with no Filter and no projection fold in hand.
-- The two must not drift: this one reads Projection.controllerOf where the Filter's
-- ControlledBy reads the partial projection's controller, which is CR 613.1b's
-- layer 2 either way.
--
-- Still read by no RULE -- what reads CR 701.54e in anger is the emblem's own
-- text, and every ability of it that pawl mints reaches the designation through
-- `yourRingBearer` instead. Pawl.RingSpec proves the designation through it.
isRingBearerOf :: PlayerId -> ObjectId -> GameState.GameState -> Bool
isRingBearerOf pid oid gs =
  Set.member oid (GameState.battlefield gs)
    && Projection.controllerOf oid gs == Just pid
    && fmap Object.ringBearerFor (Game.lookupObject oid gs) == Just (Just pid)
