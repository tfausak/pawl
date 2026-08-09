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
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Supertype as Supertype
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
-- CR 701.54c's base ability is `theRingIsLegendary` below, which is the whole of
-- `staticAbilities`. CR 114.4 is what makes one ability on an object in the command
-- zone do anything at all -- "abilities of emblems function in the command zone" --
-- and Pawl.Engine.Projection.gatherGiven walks the command zone for exactly that.
--
-- Not implemented: the SECOND clause of that base ability, "and can't be blocked by
-- creatures with greater power" (#707). The two-, three- and four-temptation
-- abilities are triggered, and an emblem's triggered ability never fires because
-- the trigger scans read only the battlefield (#709).
theRingEmblem :: Card.Card
theRingEmblem =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          Face.MkFace
            { Face.name = theRingName,
              Face.manaCost = Nothing,
              Face.typeLine = TypeLine.MkTypeLine Set.empty Set.empty Set.empty,
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
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.alternativeCosts = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.sacrificeRestrictions = [],
              Face.attackCosts = [],
              Face.mulliganActions = [],
              Face.openingHandActions = []
            }
    }

-- | CR 701.54c's first clause, "Your Ring-bearer is legendary", as the emblem's one
-- static ability. Rulebook text minted here rather than card data, on this module's
-- own terms: rule 701.54c prints The Ring's text, and Birthday Escape does not.
--
-- The affected set is CR 701.54e's definition of "is your Ring-bearer", spelled
-- across the three places that hold its three conjuncts. Affected.Matching carries
-- the "on the battlefield" one itself; ControlledBy You is "under your control",
-- whose "you" is CR 109.5's -- the emblem's controller, which CR 114.2 makes the
-- player it was minted for; and IsRingBearer is "has the Ring-bearer designation",
-- asked of that same perspective. The control conjunct is the RULE and not
-- belt-and-braces: CR 701.54e states it in as many words.
--
-- No creature-ness conjunct, for the reason isRingBearerOf below gives at length:
-- CR 701.54e's "a creature" is the antecedent of the phrase, not a fourth
-- condition, and rule 701.54c's own sentence adds none either. A Ring-bearer turned
-- into a land by Song of the Dryads is a legendary land.
--
-- One AddSupertype and no more, per CR 205.4b: a supertype is gained and never set,
-- so the Ring-bearer keeps whatever supertypes it already had. Unconditional, since
-- CR 701.54c gates the two-, three- and four-temptation abilities on a temptation
-- count and this one on nothing.
theRingIsLegendary :: StaticAbility.StaticAbility
theRingIsLegendary =
  StaticAbility.MkStaticAbility
    { StaticAbility.affected =
        Affected.Matching
          ( Filter.And
              [ Filter.IsRingBearer,
                Filter.ControlledBy PlayerRelation.You
              ]
          ),
      StaticAbility.condition = Nothing,
      -- CR 604.2 as written: the emblem's ability is not a card's text saying
      -- its effect outlives the emblem, and CR 114.4 keeps the emblem in the
      -- command zone anyway.
      StaticAbility.lingers = Nothing,
      StaticAbility.modifications = NonEmpty.singleton (Modification.AddSupertype Supertype.Legendary)
    }

-- CR 701.54c: does this player already have an emblem named The Ring?
--
-- By NAME, which is the rule's own test, rather than by comparing the whole card
-- against theRingEmblem above. Comparing cards would answer alike today and stop
-- the moment the emblem's abilities change under it -- a card is only equal to
-- itself, while the rule asks about the name however the emblem's text grows.
--
-- zoneMembers slices the command zone by OWNER, which is the right cut: CR 114.2
-- makes an emblem owned and controlled by the player who got it, and nothing
-- moves an emblem afterwards.
hasTheRing :: PlayerId -> GameState.GameState -> Bool
hasTheRing pid gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just theRingName
   in any named (Game.zoneMembers Zone.Command pid gs)

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
  -- CR 701.54c, first: the emblem, and only if they have none.
  Monad.unless (hasTheRing pid gs0) (Monad.void (Event.createEmblem pid theRingEmblem))
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

-- | CR 701.54a's SECOND ending: "until ... another player gains control of it".
-- Lift the designation from every permanent whose controller is no longer the
-- player it was designated for.
--
-- A SAMPLE of derived state, and a near-clone of Engine.checkControlContinuity
-- (CR 302.6) for the same reason: control is computed by CR 613.1b's layer 2 and
-- so CHANGES with no event to notice it, while this designation is stored.
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
-- The IMPERATIVE spelling of the same three conjuncts theRingIsLegendary above
-- spells as a Filter, for a caller with no Filter and no projection fold in hand.
-- The two must not drift: this one reads Projection.controllerOf where the Filter's
-- ControlledBy reads the partial projection's controller, which is CR 613.1b's
-- layer 2 either way.
--
-- Still read by no RULE -- what would read it is the emblem's remaining abilities,
-- and none of those has a carrier (#707's blocking clause, #709). Pawl.RingSpec
-- proves the designation through it.
isRingBearerOf :: PlayerId -> ObjectId -> GameState.GameState -> Bool
isRingBearerOf pid oid gs =
  Set.member oid (GameState.battlefield gs)
    && Projection.controllerOf oid gs == Just pid
    && fmap Object.ringBearerFor (Game.lookupObject oid gs) == Just (Just pid)
