-- | CR 701.54, "the Ring tempts you": the Ring-bearer designation, the emblem
-- named The Ring, and the count of how often a player has been tempted.
--
-- The per-player sibling of Pawl.Engine.Monarch. Both hold a designation the
-- rules give a player rather than a card, and both are named by a rule number, so
-- casing on either here is casing on the RULEBOOK -- rule 701 is a keyword-action
-- rule exactly as rule 702 is a keyword rule, and Pawl.Engine.Keyword's standing
-- to mint an ability from a Keyword is this module's standing to mint the emblem
-- from CR 701.54c. The closed/open invariant forbids the rules core casing on an
-- EFFECT's identity, and nothing here does: Pawl.Engine.Resolve's
-- Effect.TemptWithTheRing arm calls `tempt` and asks nothing about which effect it
-- came from.
--
-- Where they DIVERGE is the storage, and CR 701.54b is why: "Ring-bearer is a
-- designation A PERMANENT can have", where CR 725.1's monarch is one the game has.
-- So this rides Object.ringBearerFor and GameState grows no field, while the count
-- rides Player.ringTemptations.
module Pawl.Engine.Ring where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
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
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
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
-- Not implemented: all four of CR 701.54c's abilities, so `staticAbilities` is
-- empty. The base one is "Your Ring-bearer is legendary and can't be blocked by
-- creatures with greater power" (#707); the two-, three- and four-temptation ones
-- are triggered, which an emblem cannot carry at all today (#706).
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
              Face.keywords = Set.empty,
              Face.colorIndicator = Set.empty,
              Face.characteristicPT = Nothing,
              Face.staticAbilities = [],
              Face.spell = Face.defaultSpell,
              Face.activatedAbilities = [],
              Face.replacementEffects = [],
              Face.triggeredAbilities = [],
              Face.delayedAbilities = Map.empty,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = Nothing,
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.alternativeCosts = [],
              Face.playerAbilities = [],
              Face.blockRequirements = [],
              Face.attackRequirements = [],
              Face.combatRestrictions = [],
              Face.attackCosts = [],
              Face.mulliganAction = [],
              Face.openingHandAction = []
            }
    }

-- CR 701.54c: does this player already have an emblem named The Ring?
--
-- By NAME, which is the rule's own test, rather than by comparing the whole card
-- against theRingEmblem above. The two agree today and would stop agreeing the
-- moment #707 gives the emblem its abilities, since a card is only equal to
-- itself -- and the rule would still be asking about the name.
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
          answer <- Trans.lift (Program.prompt (Prompt.ChooseRingBearer (Decide.deciderFor pid gs1) pid offered))
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
-- so CHANGES with no event to notice it, while this designation is stored. CR
-- 704.3 makes "whenever a player would get priority" the coarsest moment anything
-- can observe the condition, so running in the settle loop is indistinguishable
-- from checking continuously.
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

-- | CR 701.54e: is this creature that player's Ring-bearer? True only for a
-- creature on the battlefield under their control carrying the designation.
--
-- Read by no rule yet -- what would read it is the emblem's own abilities, and
-- none of the four has a carrier (#707, #706). It is here because it is the rule's
-- own predicate and Pawl.RingSpec proves the designation through it.
isRingBearerOf :: PlayerId -> ObjectId -> GameState.GameState -> Bool
isRingBearerOf pid oid gs =
  Set.member oid (GameState.battlefield gs)
    && Projection.controllerOf oid gs == Just pid
    && fmap Object.ringBearerFor (Game.lookupObject oid gs) == Just (Just pid)
