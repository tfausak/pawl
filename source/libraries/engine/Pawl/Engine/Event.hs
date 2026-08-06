-- The event pipeline (CR 603/614/616). Owns the single zone-change funnel, CR
-- 616.1's loop and the `apply` that carries out a chosen replacement, plus the
-- sole casing on TriggerCondition.
--
-- The loop and the funnel share a module because the rules make them mutually
-- recursive, not because either is convenient here: a zone change raises its
-- event through the loop, because CR 614.1's replacement effects "watch for a
-- particular event that would happen" and a zone change is one of those events,
-- and applying a chosen rewrite can itself
-- change zones -- CR 614.1c's "as this permanent enters, sacrifice any number of
-- permanents" is a replacement whose application is a CR 701.21a sacrifice. The
-- SELECTION half -- which effects exist, which apply, how they bucket, who
-- chooses, how a row is spent -- stays in Pawl.Engine.Replacement, which this
-- module calls down into and which must never import this one.
--
-- changeZone lives here rather than in Pawl.Engine.Game so it can read the
-- projection -- Projection imports Game, so a Game.changeZone reading it would be
-- an import cycle.
module Pawl.Engine.Event where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.EffectZone as EffectZone
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Affected as Affected
import Pawl.Types.Binding (Binding)
import Pawl.Types.CandidateId (CandidateId)
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Countering as Countering
import Pawl.Types.DamageEvent (DamageEvent)
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import Pawl.Types.DelayedTrigger (DelayedTrigger)
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.Onset (Onset)
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import Pawl.Types.PhaseSelector (PhaseSelector)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import Pawl.Types.Prevention (Prevention)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.ProposedEvent (ProposedEvent)
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import Pawl.Types.ReplacementCandidate (ReplacementCandidate)
import qualified Pawl.Types.ReplacementCandidate as ReplacementCandidate
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import Pawl.Types.TurnWindow (TurnWindow)
import qualified Pawl.Types.TurnWindow as TurnWindow
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 608.2i: append one entry to the turn-scoped log. The single APPEND point,
-- which is also what lets it be where CR 603.3a's controller is sampled: an event
-- that OPENS a batch takes GameState.controlWhenTriggered from the board as it
-- stands, before anything between here and the CR 117.5 scan can move control.
-- Nothing is sampled mid-batch, and the pre-append state is the same board.
--
-- One sample per BATCH rather than per event; the two differ only for a batch
-- whose own events straddle a control change, which nothing in this pool produces
-- (#603).
--
-- On a hot path, and paid once per batch: controlOverrides costs one controlGrants
-- walk plus one controllerOfGiven per object a layer-2 effect names, and the
-- common board names none. Measured below the benchmark suite's noise floor.
recordEvent :: GameEvent -> GameState -> GameState
recordEvent event gs =
  let recorded = gs {GameState.events = GameState.events gs Seq.|> event}
   in if GameState.scannedThrough gs < Natural.length (GameState.events gs)
        then recorded
        else recorded {GameState.controlWhenTriggered = Projection.controlOverrides gs}

-- The zone change an event describes, if it is one.
movedOf :: GameEvent -> Maybe ZoneChange
movedOf event = case event of
  GameEvent.Moved zc _ -> Just zc
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  -- The Moved event emitted by the same discard is the zone change; this one
  -- says the move WAS a discard (CR 701.9a).
  GameEvent.Discarded {} -> Nothing
  -- CR 701.20b: a reveal is never a zone change, even when the card is about to
  -- make one.
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing
  -- The Moved event `counter` records alongside this one is rule 701.6a's zone
  -- change; this one only says the move WAS a countering. The Discarded case.
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost _ _ -> Nothing
  GameEvent.LifeGained _ _ -> Nothing
  GameEvent.CountersPut {} -> Nothing

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.DamagePrevented _ _ -> Nothing
  GameEvent.Moved _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Revealed _ _ -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost _ _ -> Nothing
  GameEvent.LifeGained _ _ -> Nothing
  GameEvent.CountersPut {} -> Nothing

-- Who revealed what, if the event is a reveal (CR 701.20a).
revealOf :: GameEvent -> Maybe (PlayerId, PC.ProjectedCharacteristics)
revealOf event = case event of
  GameEvent.Revealed pid snapshot -> Just (pid, snapshot)
  GameEvent.Moved _ _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.AttackerDeclared _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost _ _ -> Nothing
  GameEvent.LifeGained _ _ -> Nothing
  GameEvent.CountersPut {} -> Nothing

-- CR 117.5: the events the trigger scan has not yet consumed.
unscannedEvents :: GameState -> [GameEvent]
unscannedEvents gs =
  Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.scannedThrough gs)) (GameState.events gs))

-- CR 704.5h: the damage the state-based-action check has not yet consumed.
unscannedDamage :: GameState -> [DamageEvent]
unscannedDamage gs =
  Maybe.mapMaybe damageOf (Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.damageScannedThrough gs)) (GameState.events gs)))

-- Insert a freshly-built object into `dest` under a new id and timestamp, and
-- return that id. The common tail of changeZone (a moved incarnation) and
-- createTokens (a token from nothing). `mkObj` receives the fresh timestamp so the
-- object records when it entered (CR 613.7d). The Moved event is emitted by the
-- CALLER: only it knows which state the CR 608.2h snapshot must be taken against.
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> Game ObjectId
placeObject pid mkObj dest = do
  gs <- State.get
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = mkObj ts
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
  State.put (Game.insertIntoZone dest pid newId gs3)
  pure newId

-- CR 114.2: a player gets an emblem with the given abilities, put into the
-- command zone and both owned and controlled by them. CR 613.7a: its entry
-- timestamp is what the projection reads when ordering the continuous effect of
-- any static ability it carries.
--
-- Here rather than in Pawl.Engine.Resolve, because there are two minting sites
-- and only one of them is an opcode: Effect.CreateEmblem is what a CARD says,
-- and Pawl.Engine.Ring.tempt is what CR 701.54c says. An emblem built two ways
-- would be an emblem that could differ.
--
-- Inert per-incarnation fields (an emblem is never tapped, damaged or
-- countered): harmless, nothing reads them here. `enteredUnder = Nothing` is
-- what makes Projection.defaultControllerOf answer the owner, which is CR
-- 109.4c and so CR 114.2's last sentence.
createEmblem :: PlayerId -> Card -> Game ObjectId
createEmblem pid card =
  let mkObj ts =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfEmblem card,
            Object.zone = Zone.Command,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.playableFromExileBy = Nothing,
            Object.ringBearerFor = Nothing
          }
   in placeObject pid mkObj Zone.Command

-- CR 614: settle a proposed zone change. Nothing means the move does not happen.
-- The typed door changeZoneAttaching below uses, so the funnel itself never cases
-- on a ProposedEvent.
--
-- `asOf` is applyReplacementsIn's: Nothing for a lone move, Just the pre-batch
-- board when this move is one member of a CR 608.2f / 704.3 batch.
resolveZoneChange :: Maybe GameState -> ZoneChange -> Game (Maybe ZoneChange)
resolveZoneChange asOf zc = do
  outcome <- applyReplacementsIn asOf Set.empty (ProposedEvent.WouldChangeZone zc)
  pure (outcome >>= Replacement.asZoneChange)

-- CR 616.1's loop. `Nothing` means the event DOES NOT HAPPEN (CR 615.6, CR
-- 701.19a). A rewrite that cancels an event has already performed its own
-- consequences by the time it returns Nothing.
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = applyReplacementsIn Nothing Set.empty

-- CR 608.2f / 704.3: `asOf` is the board a BATCH's candidates are read from --
-- `Just` the state the batch began in, or `Nothing` for the live board. Only the
-- destroy funnel passes `Just` (`destroy`, `destroyInBatch` below), along with
-- the graveyard moves it and Pawl.Engine.Sba's put-into-graveyard batch make
-- through `changeZoneInBatch`. Everything else is a lone event and wants the
-- live board. CR 608.2f and CR 704.3 make a batch ONE event, so CR 614.4 asks
-- which effects existed before the BATCH, not before the member being processed
-- -- otherwise Rest in Peace animated by Opalescence and swept by Day of
-- Judgment answers according to an order CR 608.2f gives nobody the right to
-- decide.
--
-- `asOf` and `batch` both name a batch, and they are OPPOSITES: `asOf` widens
-- the candidate set to include effects of permanents the batch is itself
-- removing, while `batch` NARROWS the copy-target set to exclude permanents
-- entering beside the loop's subject. Deliberately not one parameter: different
-- batches, different readers, and no call site ever supplies both.
--
-- What `asOf` does NOT freeze: the FLOATING store stays live (see
-- Replacement.collect), because CR 614.3 has Replacement.consume spend a one-shot
-- as it applies and a frozen
-- store would hand a spent regeneration shield to the next member of the batch;
-- the loop still RE-COLLECTS every iteration, so CR 616.1f and CR 616.2 are
-- untouched; and `apply`'s writes and Replacement.choose's chooser lookup read the LIVE
-- state. A permanent that ENTERED after the batch began therefore contributes
-- nothing, which is CR 614.4 read the other way. No producer today, so that half
-- is unexercised.
--
-- `batch` is the set of ids entering the battlefield AT THE SAME TIME as this
-- loop's subject, and TWO of `apply`'s entry arms narrow by it, on two different
-- rules:
--
--   * COPY TARGETS. CR 614.12a puts the choice BEFORE the permanent enters, and
--     Clone may only copy a creature already ON the battlefield, so a sibling
--     entering in the same batch is not there yet at the moment the choice is
--     made. No rule states that exclusion outright: it follows from 614.12a's
--     timing plus the copy effect's own wording. CR 614.13a is the wrong cite for
--     it -- that rule is about an entry effect moving OTHER objects to a
--     different zone, and a copy target never changes zones.
--   * THE AS-ENTERS SACRIFICE (EntryRewrite.SacrificeAnyNumber). Here CR 614.13a
--     is exactly the rule, because a sacrificed permanent does change zones:
--     "You can't choose the object that will become that permanent or any other
--     object entering the battlefield at the same time as that object."
--
-- Both arms exclude the loop's own subject themselves -- legalCopyTargets'
-- `self`, and the sacrifice arm's `entering` -- never through this set.
--
-- `changeZone` handles one entering object at a time and passes `Set.empty`. The
-- non-empty case is `createTokens` below, which materializes every token of a
-- Create BEFORE running any of their entry loops (CR 614.16's doubled count is
-- settled once, up front), so a later token's entry loop would otherwise find
-- its siblings already sitting on the battlefield.
--
-- A simultaneously-entering sibling can reach a later token's entry loop through
-- three channels; only the first needs this explicit exclusion:
--   1. Copy targets -- excluded by `batch`. IMPLEMENTED BUT UNTESTED: no card in
--      the pool puts two copy-choosers onto the battlefield at once (#73).
--   2. Candidate collection -- unreachable regardless of `batch`, though no
--      longer impossible by construction. Every entry replacement a PERMANENT
--      carries in this pool is CR 614.1c's self-only `IsSource` (Clone, Primal
--      Plasma, CR 306.5b's loyalty), which no sibling can satisfy; CR 614.1d's
--      other-objects form exists (Gather Specimens) but as a FLOATING row rather
--      than a sibling's ability. A permanent printing a 614.1d entry replacement
--      (Essence of the Wild) would reach a sibling here, correctly and by the
--      card's own text.
--   3. Projection -- a sibling's STATIC ABILITIES would be visible to a later
--      token's projection, and nothing here excludes them the way `batch`
--      excludes copy targets. CR 614.12 does not sanction this: a
--      simultaneously-entering sibling's static abilities do not already exist
--      relative to it. NOT IMPLEMENTED AT ALL, and unreached today only because
--      every token card in the pool has empty `staticAbilities` (#78).
applyReplacementsIn :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacementsIn asOf batch = fmap fst . applyReplacementsReporting asOf batch

-- The same loop, answering CR 615.13's second question as well: WHICH prevention
-- effects applied on the way, and how much each of them prevented.
--
-- A separate entry rather than a wider applyReplacementsIn because only the
-- damage class can answer anything but the empty list -- CR 615.1 makes a
-- prevention effect a thing that watches a DAMAGE event -- so every other caller
-- would be threading a value it knows is empty.
applyReplacementsReporting :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent, [Prevention])
applyReplacementsReporting asOf batch = loop asOf batch Set.empty []

loop :: Maybe GameState -> Set ObjectId -> Set CandidateId -> [Prevention] -> ProposedEvent -> Game (Maybe ProposedEvent, [Prevention])
loop asOf batch applied prevented event = do
  gs <- State.get
  -- From scratch each iteration: collect against the CURRENT state (or, for a
  -- CR 608.2f batch, the state the batch began in), minus CR 614.5's
  -- already-applied set. Re-collecting is what makes CR 616.2 work.
  let unused candidate = not (Set.member (ReplacementCandidate.identity candidate) applied)
      fresh = filter unused (Replacement.applicable asOf gs event)
  case Replacement.highestBucket fresh of
    -- CR 616.1f / 614.6: no candidate remains, so the surviving event happens.
    [] -> pure (Just event, prevented)
    bucket -> do
      picked <- Replacement.choose gs event bucket
      case picked of
        -- Unreachable: highestBucket returns [] for an empty input, so `bucket`
        -- is non-empty and `choose` always picks. Total rather than partial.
        Nothing -> pure (Just event, prevented)
        Just candidate -> do
          -- CR 615.12: the chosen effect is a prevention effect and this damage
          -- can't be prevented (Spider-Punk), so it is APPLIED and prevents none
          -- of it. The event comes back untouched and no shield is written down
          -- -- "existing damage prevention shields won't be reduced by damage
          -- that can't be prevented" -- while the recursive call below still
          -- records it as applied, which is CR 615.12a's "just once" and the
          -- reason this does not spin.
          --
          -- CR 615.3's use count is skipped too, which no card notices: every
          -- prevention row pawl installs is Uses.Unlimited (Resolve.installShield
          -- says why, and Fog's authored row says Unlimited as well), so the
          -- `consume` this bypasses would have been a no-op anyway.
          --
          -- HERE rather than inside `apply`, because CR 615.12 is a fact about
          -- the (effect, event) PAIR and not about any one rewrite: `apply`'s
          -- arms answer "what does this rewrite do", and the answer is unchanged
          -- -- the rule stops the application from reaching them at all. CR
          -- 614.1a's replacements are untouched, so a Furnace of Rath still
          -- doubles unpreventable damage.
          --
          -- Not implemented: CR 615.12's middle clause, "any additional effects
          -- they have will take place". No prevention row can carry one, so
          -- there is nothing here to run (#689).
          outcome <-
            if Replacement.inertPrevention gs candidate event
              then pure (Just event)
              else apply batch candidate event
          -- CR 615.13: read OUTSIDE `apply`, from the event before and after, so
          -- no arm of that fold has to report anything and none can forget to.
          -- What makes it exact rather than a guess is Replacement.prevents: only a
          -- PREVENTION rewrite's shrinkage is prevention, where CR 614.1a's
          -- SetAmount and Scale shrink an event without preventing a point of it.
          let prevented1 = prevented <> Maybe.maybeToList (Replacement.preventionBy candidate event outcome)
          case outcome of
            Nothing -> pure (Nothing, prevented1)
            Just rewritten -> loop asOf batch (Set.insert (ReplacementCandidate.identity candidate) applied) prevented1 rewritten

-- CR 614.6: apply one chosen effect. Nothing means the event does not happen.
--
-- One arm per ReplacementEffect constructor, same shape as `applies`, so a new
-- constructor breaks the build HERE too. A wildcard fallback would defeat that:
-- an author who teaches `applies` a new arm but forgets this one gets a silent
-- no-op replacement. Every arm below either rewrites its paired event or, for a
-- pair `applies` already excludes, falls through to `pure (Just event)` --
-- unreachable in practice, but present so the match stays total per constructor
-- rather than total by wildcard.
--
-- The same discipline applies one level down, to each arm's inner SUM type
-- (DamageRewrite, DestructionRewrite, EntryRewrite, Scaling), never to the
-- pattern RECORDS, which are read for their fields rather than cased. An arm
-- must CASE on the inner sum, not bind it with `_`: `_` is exhaustive
-- UNCONDITIONALLY, so it raises no build failure and no warning when a new
-- constructor is added, silently treating a real rewrite as a no-op.
--
-- CounterR's and TokenR's arms delegate Scaling whole to `scale`, which is where
-- the exhaustive case lives -- a new Scaling constructor breaks `scale`'s build
-- and both arms' transitively, so casing it again inline would not strengthen
-- anything.
apply :: Set ObjectId -> ReplacementCandidate -> ProposedEvent -> Game (Maybe ProposedEvent)
apply batch candidate event =
  case (ReplacementCandidate.effect candidate, event) of
    (ReplacementEffect.ZoneChangeR _ toDest, ProposedEvent.WouldChangeZone zc) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldChangeZone zc {ZoneChange.to = toDest}))
    -- Unreachable: `applies` admits ZoneChangeR only against WouldChangeZone.
    (ReplacementEffect.ZoneChangeR _ _, _) -> pure (Just event)
    -- CR 707.5 / 614.1c / 614.12a: the entering object's controller chooses a
    -- permanent to copy, and its copiable characteristics are stamped as this
    -- object's copy snapshot. Writing to the COPIABLE layer (CR 613.1a) is what
    -- makes CR 707.2 fall out for free: a later Clone of this object copies the
    -- stamped values with no further machinery. Clone's "may" is real: Nothing
    -- leaves the object as its printed self (a 0/0, which CR 704.5f then buries).
    --
    -- The class match is on the OUTER tuple, so the INNER `case rewrite of` is
    -- what carries the exhaustiveness obligation -- a wildcard-bound `_` on the
    -- outer pattern would let a new EntryRewrite constructor fall through
    -- silently whenever it happened to pair with WouldEnter.
    (ReplacementEffect.EntryR _ rewrite, ProposedEvent.WouldEnter oid) -> case rewrite of
      EntryRewrite.AsCopy -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable: the object is materialized on the battlefield before this
          -- loop runs, so controllerOf falls back to its owner. Defensive: make no
          -- unprompted copy choice.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            let legal = Replacement.legalCopyTargets batch oid gs
            answer <- Game.choose (Prompt.ChooseCopyTarget decider controller oid legal)
            -- FILTERED, NOT TRUSTED (#222). legalCopyTargets is the ONLY thing
            -- enforcing CR 614.12a's same-batch exclusion, so honouring an
            -- unoffered answer would let a Clone copy a sibling token entering
            -- beside it.
            let chosen = case answer of
                  Just src | List.elem src legal -> Just src
                  _ -> Nothing
            case chosen of
              Nothing -> pure (Just event)
              Just src2 -> do
                State.modify' $ \g ->
                  let stamp o = o {Object.bindings = Binding.setCopy (Projection.copiableCharacteristics src2 g) (Object.bindings o)}
                   in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
                pure (Just event)
      -- CR 614.1c / 208.2b: Primal Plasma's choice of which printed
      -- power/toughness-and-keywords option to become. Written into the COPIABLE
      -- snapshot (applyEntryOption), which is what makes CR 616.2 fall out for
      -- free: a Clone that copies Primal Plasma also copies this ability (CR
      -- 707.5), and the loop's next iteration finds it newly applicable.
      EntryRewrite.ChoiceOf options -> do
        gs <- State.get
        case options of
          -- Malformed card data: an as-enters choice with nothing to choose
          -- from. No-op rather than a partial function, but still consumed -- a
          -- floating one-shot must not survive to apply again.
          [] -> do
            Replacement.consume (ReplacementCandidate.identity candidate)
            pure (Just event)
          first : rest -> do
            picked <-
              if null rest
                then -- One option is not a choice; where the rules leave
                -- nothing to ask, don't prompt.
                  pure first
                else case Projection.controllerOf oid gs of
                  -- Unreachable: the object is materialized on the
                  -- battlefield before this loop runs, so controllerOf falls
                  -- back to its owner. Defensive: make no unprompted choice.
                  Nothing -> pure first
                  Just controller -> do
                    let decider = Decide.deciderFor controller gs
                    answer <- Game.choose (Prompt.ChooseEntryOption decider controller oid options)
                    pure (Replacement.at options answer first)
            Replacement.consume (ReplacementCandidate.identity candidate)
            State.modify' (Replacement.applyEntryOption oid picked)
            pure (Just event)
      -- CR 614.1c: Painter's Servant's as-enters colour choice. Unlike ChoiceOf
      -- above, this is asked every time the entering object has a controller to
      -- ask: CR 105.1's five colours are always all legal and always
      -- distinguishable, so there is no one-option case to elide.
      --
      -- Written to Object.chosenColor, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseColor.
      EntryRewrite.ChooseColor -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. A WEAKER fallback than
          -- ChoiceOf's, and the one place on this path the engine would decide
          -- something: there is no colour the card named to default to, so white
          -- is conjured. It stands only because the branch cannot be reached.
          Nothing -> pure Color.White
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Game.choose (Prompt.ChooseColor decider controller oid)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenColor = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c: Convincing Mirage's as-enters basic land type choice. Asked
      -- every time the entering object has a controller to ask, for
      -- ChooseColor's reason just above: CR 305.6's five basic land types are
      -- always all legal and always distinguishable, so there is no one-option
      -- case to elide.
      --
      -- Written to Object.chosenSubtype, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseBasicLandType.
      EntryRewrite.ChooseBasicLandType -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. The same WEAKER fallback
          -- ChooseColor's arm carries, and for the same reason: no type the card
          -- named to default to, so Mountain is conjured.
          Nothing -> pure Subtype.Mountain
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Game.choose (Prompt.ChooseBasicLandType decider controller oid)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenSubtype = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c with CR 201.4: Null Chamber's as-enters name choices. Unlike
      -- the two arms above, this one has to settle WHO is asked before it can
      -- ask anything: the card names its controller and one opponent, and CR
      -- 101.4 puts their two simultaneous choices in APNAP order.
      --
      -- Written to Object.chosenNames, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseCardNames.
      EntryRewrite.ChooseCardNames restriction -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Names NOTHING rather than
          -- conjuring a name, which the two arms above cannot do -- their
          -- fallbacks pick from a fixed five, and CR 201.4's offer is every card
          -- in the Oracle card reference.
          Nothing -> pure Set.empty
          Just controller -> do
            -- CR 102.1 makes a player one of the people IN the game, and CR
            -- 104.3a lets one leave at any time -- so the offer is
            -- Game.stillPlaying and not GameState.turnOrder, which keeps a
            -- departed seat.
            let opponents = filter (/= controller) (Game.stillPlaying gs)
            opponent <- case opponents of
              -- CR 102.2: a two-player game leaves exactly one opponent, and
              -- one option is not a choice. The empty case is a game whose
              -- other seats have all left (CR 104.2a) -- nobody to ask, and no
              -- second name.
              [] -> pure Nothing
              [sole] -> pure (Just sole)
              first : second : rest -> do
                let offered = first NonEmpty.:| (second : rest)
                answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor controller gs) controller oid offered)
                -- FILTERED, NOT TRUSTED, the posture Sba.chooseLegendVictims
                -- takes: an answer naming somebody who is not an opponent would
                -- otherwise hand a second name to a player the card never asked,
                -- so it falls back to the head.
                pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
            -- CR 101.4: the active player chooses first, then the rest in turn
            -- order. Both names are chosen as one event, so the order is the
            -- rule's and not the card's reading order.
            let choosers = filter (\pid -> pid == controller || Just pid == opponent) (Game.apnapOrder gs)
                ask pid = Game.choose (Prompt.ChooseCardName (Decide.deciderFor pid gs) pid oid restriction)
            fmap Set.fromList (Monad.mapM ask choosers)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenNames = picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 306.5b via CR 614.1c: this permanent enters with N counters. Through
      -- Event.putCounters, the CR 122.6 funnel, and NOT a direct write to
      -- Object.counters, because CR 614.16 makes a counter-scaling replacement
      -- apply even when the original event was not itself an effect -- so
      -- Doubling Season has to see these. That nested CR 616.1 loop is why the
      -- counters are placed here rather than folded into the entry event's own
      -- payload. Consumed like every other arm, so CR 614.5 keeps the loop's next
      -- iteration from placing them twice.
      EntryRewrite.WithCounters kind n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        putCounters CounterCause.ByEffect oid kind n
        pure (Just event)
      -- CR 616.1b / 110.2: Gather Specimens. The entering object's CR 110.2
      -- DEFAULT controller becomes CR 109.5's "you" -- the candidate's
      -- controller, baked when the row was installed -- and that is a permanent
      -- change: the card's "this turn" bounds how long the REPLACEMENT is around
      -- to catch entries, never how long the creature stays yours.
      --
      -- Written to the object rather than to the surviving ProposedEvent, which
      -- is why WouldEnter still carries only an ObjectId. This engine
      -- materializes the entering permanent BEFORE running the entry loop (see
      -- runEntry, and CR 614.12), so the would-be controller is exactly
      -- Projection.controllerOf on the live board. That is also what makes CR
      -- 616.2 fall out: the loop's next iteration re-matches against a board
      -- where the control has already changed, which a value parked on the event
      -- would not show it. All five arms above land on the object for the same
      -- reason.
      --
      -- No prompt, and none is owed: CR 616.1b's rewrite has no choice in it,
      -- and the choice the rule DOES describe -- which of several
      -- control-modifying effects to apply -- is `choose`'s, one level up.
      --
      -- Not implemented: `you` is not checked against CR 800.4a, so this can
      -- hand a permanent to a player who has left the game (#592).
      EntryRewrite.UnderSourceControl -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        case ReplacementCandidate.controller candidate of
          -- CR 109.5 has no answer: a permanent-sourced instance whose source
          -- has left the board. Defensive, with no producer today, and it leaves
          -- the entry alone rather than guessing at a player.
          Nothing -> pure (Just event)
          Just you -> do
            State.modify' $ \gs ->
              let claim obj = obj {Object.enteredUnder = Just you}
               in gs {GameState.objects = Map.adjust claim oid (GameState.objects gs)}
            pure (Just event)
      -- CR 614.1c: Shimatsu the Bloodcloaked. "As this creature enters, sacrifice
      -- any number of permanents. This creature enters with that many +1/+1
      -- counters on it." -- one rewrite, because the count the second sentence
      -- uses is the answer to the first.
      --
      -- Wood Elemental is the same rewrite reading its count somewhere else: "As
      -- this creature enters, sacrifice any number of untapped Forests. Wood
      -- Elemental's power and toughness are each equal to the number of Forests
      -- sacrificed as it entered." No counters, so `kind` is Nothing and the count
      -- is left for the card's characteristic-defining ability (CR 208.2a) to read
      -- back out of Binding.sacrificedCount.
      --
      -- The only entry arm that PERFORMS a game action rather than stamping a
      -- value, which is why this module and not Pawl.Engine.Replacement holds
      -- `apply`: the sacrifice is `sacrifice` below, CR 701.21a's one funnel, and
      -- the counters go through `putCounters`, CR 122.6's. Both are the ordinary
      -- doors, so Rest in Peace redirects a sacrificed permanent and Doubling
      -- Season (CR 614.16) doubles the counters, with nothing written here to
      -- make either happen.
      --
      -- THE ENTERING OBJECT IS NOT A CANDIDATE, nor is a permanent entering
      -- beside it. CR 614.13a states it outright -- "you can't choose the object
      -- that will become that permanent or any other object entering the
      -- battlefield at the same time as that object" -- and that rule reaches
      -- this arm and not the copy arm beside it, because a sacrificed permanent
      -- CHANGES ZONES and a copy target does not (see applyReplacementsIn). This
      -- engine materializes the entering object before running the entry loop
      -- (see runEntry), so `sacrifice` would otherwise happily take it: the
      -- exclusion has to be written, not inherited.
      --
      -- Not implemented: CR 614.12b's combined-affordability check when two such
      -- permanents enter at once (#72).
      EntryRewrite.SacrificeAnyNumber criterion kind -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Sacrifices nothing rather than
          -- guessing at a player, and so places no counters and records no count.
          Nothing -> pure (Just event)
          Just controller -> do
            let entering oid2 = oid2 == oid || Set.member oid2 batch
                offered = filter (not . entering) (Replacement.sacrificeCandidates controller criterion gs)
            chosen <-
              -- Where the rules leave nothing to ask, don't prompt: with no
              -- candidate the empty set is the only answer. ONE candidate is
              -- still asked, unlike Prompt.ChooseSacrifices' elision -- "any
              -- number" leaves two distinguishable answers there.
              if null offered
                then pure Set.empty
                else do
                  let decider = Decide.deciderFor controller gs
                  answer <- Game.choose (Prompt.ChooseAnyNumberToSacrifice decider controller oid offered)
                  -- FILTERED, NOT TRUSTED (#222): an answer naming a permanent
                  -- that was never offered would otherwise sacrifice it and pay
                  -- for a counter with it.
                  pure (Set.intersection answer (Set.fromList offered))
            Monad.mapM_ (sacrifice controller) (Set.toAscList chosen)
            -- "That many": the permanents CHOSEN, which is also the permanents
            -- sacrificed -- every member was on the battlefield under this
            -- player's control when it was offered, and nothing between there and
            -- here moves one.
            let many = Natural.length chosen
            -- Recorded on the entering permanent BEFORE the counters, and
            -- unconditionally, so a card that reads the count rather than
            -- spending it on counters has it (Wood Elemental). See
            -- Binding.sacrificedCount for why 0 is recorded rather than left
            -- absent.
            State.modify' $ \gs2 ->
              let note obj = obj {Object.bindings = Map.insert Binding.sacrificedCount (Binding.toAmount many) (Object.bindings obj)}
               in gs2 {GameState.objects = Map.adjust note oid (GameState.objects gs2)}
            Monad.mapM_ (\k -> putCounters CounterCause.ByEffect oid k many) kind
            pure (Just event)
      -- CR 702.136a: riot. "You may have this permanent enter with an additional
      -- +1/+1 counter on it. If you don't, it gains haste."
      --
      -- NEVER ELIDED. A +1/+1 counter and haste are two outcomes a player can
      -- tell apart on any board -- the whole reason the keyword exists -- so this
      -- prompt is raised every time the entering object has a controller to ask,
      -- the posture ChooseColor's arm takes and not ChoiceOf's one-option
      -- elision.
      --
      -- The counter goes through putCounters, CR 122.6's funnel, exactly as the
      -- WithCounters arm above does, so CR 614.16 applies to it and Doubling
      -- Season sees riot's counter.
      --
      -- The haste is a STORED continuous effect (CR 611.2) rather than a stamp on
      -- the object: rule 702.136a says the permanent "gains haste" and names no
      -- end, which is CR 611.2a's rest-of-the-game duration, and a stored effect
      -- is what puts the grant in CR 613.1f's layer 6 with a timestamp for
      -- Humility and every other ability-remover to be ordered against. Its
      -- source is the entering permanent itself, the object whose riot ability
      -- generated it (CR 113.7).
      --
      -- The timestamp is a FRESH one, taken here. CR 613.7a would give a static
      -- ability's continuous effect the timestamp of the object the ability is
      -- on, which for this one is the permanent that entered a moment ago and has
      -- the newest object timestamp on the board -- so the two coincide at every
      -- ordering question a card in this pool can ask.
      EntryRewrite.Riot -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Neither half is applied rather
          -- than one being chosen unasked -- the engine makes no player's choice,
          -- and both halves here are choices.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            answer <- Game.choose (Prompt.ChooseRiot decider controller oid)
            case answer of
              OptionalDecision.Exercises -> putCounters CounterCause.ByEffect oid CounterKind.PlusOnePlusOne 1
              OptionalDecision.Declines ->
                State.modify' $ \gs2 ->
                  -- CR 611.2a: "gains haste" with no stated end lasts until the
                  -- game does. Armed through Pawl.Engine.Expiry rather than naming
                  -- Expiry.Never here, the posture Resolve's storing arms take;
                  -- Indefinite always arms, so the Nothing branch is unreachable
                  -- and is written out only because arm is total over Duration.
                  case Expiry.arm controller oid Duration.Indefinite gs2 of
                    Nothing -> gs2
                    Just expiry ->
                      let (ts, gs3) = Game.freshTimestamp gs2
                          eff =
                            ContinuousEffect.MkContinuousEffect
                              { ContinuousEffect.source = oid,
                                ContinuousEffect.timestamp = ts,
                                ContinuousEffect.expiry = expiry,
                                ContinuousEffect.modification = Modification.GainKeyword Keyword.Type.Haste,
                                -- CR 611.2c: a fixed set of one, settled here --
                                -- the permanent that entered, not whatever
                                -- matches a filter later.
                                ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
                              }
                       in gs3 {GameState.continuousEffects = eff : GameState.continuousEffects gs3}
            pure (Just event)
    -- Unreachable: `applies` admits EntryR only against WouldEnter.
    (ReplacementEffect.EntryR _ _, _) -> pure (Just event)
    (ReplacementEffect.DamageR pat rewrite, ProposedEvent.WouldDealDamage de) -> case rewrite of
      -- CR 615.6: a prevented event never happens -- it is not marked, not
      -- drained, and never recorded, so no deathtouch bit exists for the CR
      -- 704.5h SBA to read.
      DamageRewrite.PreventAll -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure Nothing
      -- CR 615.7's shield covers as much of THIS event as it has left, and
      -- whatever it could not cover survives as a smaller event of the same
      -- source, recipient and riders. Nothing when it covered all of it, which
      -- is CR 615.6: a prevented event never happens.
      --
      -- No choice is made here, and none is owed: within one event CR 615.7
      -- leaves nothing to decide, since the prevention is neither optional nor
      -- divisible by anyone's say-so. The choice the rule DOES describe -- which
      -- of several simultaneous events the shield covers -- is asked one level
      -- up, in resolveDamageBatch.
      --
      -- NOT `consume`. That spends a row per APPLICATION, while CR 615.7's unit
      -- is the amount of damage rather than the number of events or sources
      -- dealing it. `setShield` writes the remainder back and drops the row at 0.
      DamageRewrite.PreventNext remaining -> do
        let amount = DamageEvent.amount de
            -- Both subtractions below are total on Natural: `prevented` is a min
            -- of the two operands, so it is no greater than either.
            prevented = min remaining amount
        Replacement.setShield (ReplacementCandidate.identity candidate) pat (remaining - prevented)
        if prevented >= amount
          then pure Nothing
          else pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = amount - prevented}))
      -- CR 614.1a's "instead" with a flat amount (Galvanic Blast). Only the
      -- AMOUNT is rewritten, and that is the rule rather than economy: a
      -- replaced damage event keeps its source, its recipient and every
      -- deal-time rider it was proposed with.
      DamageRewrite.SetAmount n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = n}))
      -- CR 614.1a: Furnace of Rath's "it deals double that damage ... instead".
      -- Through the same `scale` the counter and token rewrites use, so a
      -- doubling means one thing across every event class.
      DamageRewrite.Scale scaling -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = Replacement.scale scaling (DamageEvent.amount de)}))
    -- Unreachable: `applies` admits DamageR only against WouldDealDamage.
    (ReplacementEffect.DamageR _ _, _) -> pure (Just event)
    -- CR 701.19a: regeneration removes marked damage, taps the permanent and
    -- removes it from combat. The DESTRUCTION does not happen, so nothing
    -- downstream of it (a put-into-graveyard, and therefore Rest in Peace's
    -- redirect) ever runs.
    (ReplacementEffect.DestructionR rewrite, ProposedEvent.WouldBeDestroyed oid _) -> case rewrite of
      DestructionRewrite.Regenerate -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \gs ->
          let healTap obj = obj {Object.damage = 0, Object.tapped = TapState.Tapped}
              healed = gs {GameState.objects = Map.adjust healTap oid (GameState.objects gs)}
           in Game.removeFromCombat oid healed
        pure Nothing
    -- Unreachable: `applies` admits DestructionR only against WouldBeDestroyed.
    (ReplacementEffect.DestructionR _, _) -> pure (Just event)
    -- CR 122.6/614.16: Hardened Scales/Doubling Season scale a counter placement.
    (ReplacementEffect.CounterR _ scaling, ProposedEvent.WouldPutCounters oid kind n) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldPutCounters oid kind (Replacement.scale scaling n)))
    -- Unreachable: `applies` admits CounterR only against WouldPutCounters.
    (ReplacementEffect.CounterR _ _, _) -> pure (Just event)
    -- CR 614.16: Doubling Season scales token creation.
    (ReplacementEffect.TokenR _ scaling, ProposedEvent.WouldCreateTokens pid card n) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldCreateTokens pid card (Replacement.scale scaling n)))
    -- Unreachable: `applies` admits TokenR only against WouldCreateTokens.
    (ReplacementEffect.TokenR _ _, _) -> pure (Just event)
    -- CR 614.1b / 614.10: a skip is "instead of doing X, do nothing", so the
    -- step or phase simply does not begin. Nothing is done first, unlike
    -- DamageRewrite.PreventAll's sibling arm: a skip has no consequence of its
    -- own to perform before it cancels.
    --
    -- The obligation the doc above places on every arm -- case on the inner sum
    -- rather than bind it with `_` -- has nothing to bind here: PhaseR carries a
    -- pattern and no rewrite, because CR 614.1b leaves a skip only one possible
    -- outcome. The day a PhaseRewrite exists, this arm owes it a case.
    --
    -- CR 614.10a's arithmetic -- two skip effects mean two occurrences skipped,
    -- one per instance -- falls out of the floating store's SHAPE rather than
    -- out of care taken here. Two Fatigues prepend two ActiveReplacements, and a
    -- list of instances with distinct timestamps cannot coalesce the way a Set of
    -- patterns or a Boolean flag would; Replacement.consume deletes by (source,
    -- timestamp), so it spends exactly the one that applied; and returning
    -- Nothing ENDS the CR 616.1 loop, so no second skip can be spent on the same
    -- step.
    --
    -- The occurrence skipped is the one the PATTERN named, which for Stonehorn
    -- Dignitary is a whole combat phase rather than a step of one. Nothing here
    -- has to know that: Engine.runStep raises the phase question exactly once per
    -- phase, so a whole-phase skip gets exactly one chance to apply.
    --
    -- Eon Hub's PhaseR reaches the same arm and consumes nothing: it is a
    -- permanent's static ability, so its CandidateId is OfPermanent and `consume`
    -- is a no-op for it. It is the store, not this arm, that tells the two apart.
    (ReplacementEffect.PhaseR _, ProposedEvent.WouldBeginPhase _ _) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure Nothing
    -- Unreachable: `applies` admits PhaseR only against WouldBeginPhase.
    (ReplacementEffect.PhaseR _, _) -> pure (Just event)

-- CR 614.1c / 614.12: run the entry loop for an object that has just been
-- materialized on the battlefield.
--
-- The object is in GameState.objects and its zone index BEFORE this runs,
-- because CR 614.12 asks for the permanent's characteristics AS IT WOULD EXIST
-- ON THE BATTLEFIELD -- a projection of the object in the state where it has
-- entered, so the cheapest correct implementation is to put it there and project
-- it normally. Nothing observes the interim object: this finishes before the
-- Moved event is recorded, so no trigger scan and no state-based action can see
-- it.
--
-- `Monad.void` discards the `Nothing` that means the event does not happen. Safe
-- here: every EntryR arm always returns `Just`, and only DamageR/DestructionR
-- ever return `Nothing`, neither of which pairs with WouldEnter -- the only
-- event this loop proposes.
--
-- Always the LIVE board (`Nothing`), even when the zone change containing this
-- entry belongs to a CR 608.2f batch: the entering object is not on the
-- pre-batch board at all, and CR 614.12 asks about now rather than about when
-- the containing event began. CR 616.1g recognizes an entry like this as an
-- event CONTAINED within another rather than a second member of the batch, but
-- speaks only to the ORDER the two events' effects are chosen in, not to which
-- board each collects from. That a contained event keeps its own footing is this
-- engine's reading, resting on CR 614.12; no rule states it outright.
runEntry :: Set ObjectId -> ObjectId -> Game ()
runEntry batch oid = Monad.void (applyReplacementsIn Nothing batch (ProposedEvent.WouldEnter oid))

-- CR 615: settle one proposed damage event. Nothing means it does not happen;
-- the second answer is CR 615.13's, one entry per prevention effect that applied
-- to THIS event and prevented some of it.
resolveDamage :: DamageEvent.DamageEvent -> Game (Maybe DamageEvent.DamageEvent, [Prevention])
resolveDamage de = do
  (outcome, prevented) <- applyReplacementsReporting Nothing Set.empty (ProposedEvent.WouldDealDamage de)
  pure (outcome >>= Replacement.asDamageEvent, prevented)

-- CR 608.2f / 510.2: settle a whole batch of SIMULTANEOUS damage events, and
-- answer the survivors. The typed door Pawl.Engine.Damage uses, so Damage never
-- cases on a ProposedEvent or on a ReplacementEffect.
--
-- Each event still runs its OWN CR 616.1 loop and the loop's unit is still one
-- event, which is what CR 614.5 and CR 615.10 both describe. Two things this
-- adds over calling resolveDamage per event, and both are rules the BATCH is the
-- only place to state:
--
--   * CR 615.7's ORDER, because the shield is a single resource allocated across
--     the whole batch and the rule gives that choice to the shielded side.
--   * CR 615.13's GROUPING, because that rule fires an ability "each time a
--     prevention effect is applied to one or more simultaneous damage events",
--     so one instance reaching three of this batch's events is ONE prevention of
--     the total rather than three.
resolveDamageBatch :: [DamageEvent.DamageEvent] -> Game ([DamageEvent.DamageEvent], [Prevention])
resolveDamageBatch events = do
  ordered <- Replacement.orderForShields events
  settled <- Monad.mapM resolveDamage ordered
  pure (Maybe.mapMaybe fst settled, Replacement.groupPreventions (concatMap snd settled))

-- CR 701.8 / 614.8: settle a proposed destruction. `Just` is the object actually
-- destroyed -- which need not be the one asked about, since a rewrite may
-- redirect it; `Nothing` means a replacement took the event (regeneration), and
-- that rewrite has already done its own work.
--
-- `asOf` is applyReplacementsIn's, and the destroy funnel always supplies it: CR
-- 608.2f gives even a single Doom Blade a one-element batch, and when that batch
-- is itself part of a CR 704.3 pass the board is the pass's rather than the
-- batch's (Event.destroyInBatch).
resolveDestruction :: Maybe GameState -> Regenerability.Regenerability -> ObjectId -> Game (Maybe ObjectId)
resolveDestruction asOf regenerability oid = do
  outcome <- applyReplacementsIn asOf Set.empty (ProposedEvent.WouldBeDestroyed oid regenerability)
  pure (outcome >>= Replacement.asDestruction)

-- The single counter-PLACEMENT funnel (CR 122.6: counters as markers on a
-- permanent -- not to be confused with `counter` below, CR 701.6's countering of
-- a spell). CR 122.6 makes this the right single seam, since it covers both
-- counters put on a permanent already on the battlefield and counters an object
-- is given as it enters. A zero count after the loop puts nothing on.
--
-- Beside the other change-and-emit funnels of this module, and `apply`'s
-- EntryRewrite arms call it directly for CR 122.6's as-it-enters clause. A copy
-- of the body anywhere else would be a second funnel, which is the one thing a
-- funnel must not have.
--
-- The CounterCause is CR 614.16's question and nothing else: it decides whether
-- the CR 616.1 loop runs at all. See resolveCounters.
putCounters :: CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind -> Natural -> Game ()
putCounters cause oid kind n = do
  resolved <- resolveCounters cause oid kind n
  case resolved of
    Nothing -> pure ()
    Just (target, settledKind, settledCount) ->
      Monad.when (settledCount > 0)
        . State.modify'
        $ \gs ->
          -- No write and no event for an object that is not there. Map.adjust on a
          -- missing id is a silent no-op, so proceeding would record a placement
          -- the state does not show. ONE lookup answers both questions -- whether
          -- the object exists, and how many counters of the kind it already had.
          case Game.lookupObject target gs of
            Nothing -> gs
            Just obj ->
              let before = Map.findWithDefault 0 settledKind (Object.counters obj)
                  bump o = o {Object.counters = Map.insertWith (+) settledKind settledCount (Object.counters o)}
                  bumped = gs {GameState.objects = Map.adjust bump target (GameState.objects gs)}
               in -- CR 122.6's placement, recorded AFTER the write and from the
                  -- SETTLED count, so a Doubling Season that turned one counter into
                  -- two records the crossing the board actually saw. The before/after
                  -- pair is what CR 714.2b's chapter ability reads.
                  --
                  -- Guarded by the same `settledCount > 0` the write is: an event
                  -- recorded for a placement that did not happen would fire a chapter
                  -- ability off nothing.
                  recordEvent (GameEvent.CountersPut target settledKind before (before + settledCount)) bumped

-- CR 122.6: settle a proposed counter placement. Nothing means none are put on.
--
-- CR 614.16 is the gate. Its replacement effects -- "if an effect would put one or
-- more counters on a permanent" -- reach a placement made by a resolving spell or
-- ability, or by another replacement or prevention effect, and nothing else; CR
-- 609.1 makes a turn-based action none of those. So a ByRule placement skips the
-- CR 616.1 loop and stands as proposed.
--
-- Skipping the WHOLE loop, rather than filtering CR 614.16's rows out of it, is an
-- equivalence that rests on a capability pawl lacks and not on a claim about
-- Magic: ReplacementEffect.CounterR is the only class `Replacement.matches` pairs
-- with a WouldPutCounters, and every CounterR is one of CR 614.16's two shapes (a
-- Scaling -- see Replacement.scale). A counter replacement OUTSIDE rule 614.16 --
-- Solemnity's "if one or more counters would be put on a permanent or player, they
-- aren't" -- has no representation and no printing in the pool, and the card that
-- brings one must move this gate from the loop's door into the row filter (#847).
resolveCounters :: CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind -> Natural -> Game (Maybe (ObjectId, CounterKind.CounterKind, Natural))
resolveCounters cause oid kind n = case cause of
  CounterCause.ByRule -> pure (Just (oid, kind, n))
  CounterCause.ByEffect -> do
    outcome <- applyReplacements (ProposedEvent.WouldPutCounters oid kind n)
    pure (outcome >>= Replacement.asCounters)

-- CR 111.1: settle a proposed token creation. Nothing means none are created.
resolveTokens :: PlayerId -> Card -> Natural -> Game (Maybe (PlayerId, Card, Natural))
resolveTokens pid card n = do
  outcome <- applyReplacements (ProposedEvent.WouldCreateTokens pid card n)
  pure (outcome >>= Replacement.asTokens)

-- CR 500.11 / 614.10: settle whether a step or phase begins at all, on the turn
-- of `pid`. False means a skip took it, and proceeding past it is then the
-- caller's whole obligation -- CR 614.1b replaces a skipped step with nothing, so
-- there is no rewritten event to carry out. How far past it reaches is the
-- caller's too: one schedule entry for a PhaseSelector.Step, the phase's
-- remaining entries for a whole phase (Engine.runStep, Turn.dropRestOfPhase).
--
-- Answers a Bool rather than the settled event, unlike resolveDestruction, whose
-- `Just` had to carry an identity because a rewrite can redirect which object is
-- destroyed. Nothing can rewrite a WouldBeginPhase: PhaseR is the only effect the
-- class admits and it only ever cancels.
--
-- The typed door Pawl.Engine.Engine uses, so Engine never cases on a
-- ProposedEvent.
beginsPhase :: PhaseSelector -> PlayerId -> Game Bool
beginsPhase selector pid = do
  outcome <- applyReplacements (ProposedEvent.WouldBeginPhase selector pid)
  pure (Maybe.isJust (outcome >>= Replacement.asPhaseBegin))

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
-- The Game () wrapper the ~30 existing callers use; changeZoneReturning below
-- carries the same body but hands back the freshly-minted incarnation id, which
-- Resolve's ExileUntilMonarch arm registers for its return sweep.
changeZone :: ObjectId -> Zone -> Game ()
changeZone oid requestedDest = Monad.void (changeZoneReturning oid requestedDest)

-- changeZoneReturning for a move whose effect says how the object ENTERS -- CR
-- 110.5b's tap state, and CR 110.2a's controller -- rather than taking the
-- rules' defaults.
--
-- A separate door rather than a fifth parameter on changeZone, as changeZoneInBatch
-- is: the ~30 callers moving under the default have no tap state to name. Handed
-- to the funnel rather than applied after it, so a permanent an effect says is
-- tapped is never untapped for an instant, and so a permanent an effect says
-- enters under someone's control never belongs to its owner for an instant --
-- CR 614.1c's entry replacements run inside this call and read both.
changeZoneEntering :: ObjectId -> Zone -> TapState.TapState -> Maybe PlayerId -> Game (Maybe ObjectId)
changeZoneEntering oid requestedDest tapped under = changeZoneAttaching Nothing oid requestedDest Nothing tapped under Nothing

-- changeZoneReturning for a move that puts the object into its destination
-- SHOWING one named face: CR 709.3's choice of which half of a split card is
-- being cast, which the rule makes "before putting it onto the stack", so the
-- move is what carries it. CR 712.11b words the modal double-faced card's
-- version of the same choice identically, and CR 712.11c its version of CR
-- 709.3a, so this door is the shape both layouts ask for even though only the
-- split one ships.
--
-- A separate door rather than a seventh parameter on changeZone, as
-- changeZoneEntering is: the ~30 callers moving an object that shows whatever its
-- layout gives it (CR 709.4's combined view, a single-face card's one face) have
-- no face to name.
--
-- Handed to the funnel rather than written onto the object the move returned,
-- for the reason CR 709.3a states: "only that half is considered to be put onto
-- the stack", so the CR 400.7 incarnation must never exist without it -- and,
-- since only a writer inside the move knows where the move actually landed, a
-- CR 616.1 redirect to another zone drops the face instead of carrying it there.
-- See the `face` note in changeZoneAttaching's mkObj, and Pawl.CastSpec's "a cast
-- redirected off the stack keeps both halves" for the case that proves it.
changeZoneShowing :: ObjectId -> Zone -> CardName.CardName -> Game (Maybe ObjectId)
changeZoneShowing oid requestedDest name = changeZoneAttaching Nothing oid requestedDest Nothing TapState.Untapped Nothing (Just name)

-- changeZone for one member of a batch of moves CR 608.2f or CR 704.3 processes
-- SIMULTANEOUSLY. `asOf` is the board the batch began in -- or, for a batch inside
-- a larger simultaneous event, that event's -- and is what its members' CR 616.1
-- loops collect replacement candidates from; see applyReplacementsIn above.
--
-- A separate door rather than a fourth parameter on changeZone: a batch is the
-- rare case, and for a single move the board it begins on IS the live one.
changeZoneInBatch :: GameState -> ObjectId -> Zone -> Game ()
changeZoneInBatch asOf oid requestedDest = Monad.void (changeZoneAttaching (Just asOf) oid requestedDest Nothing TapState.Untapped Nothing Nothing)

-- changeZoneReturning's body, returning the destination incarnation's id: Just
-- newId on a completed move (CR 400.7 minted a fresh id), Nothing when the id is
-- unknown or the CR 616.1 replacement loop cancelled the move (`resolved ==
-- Nothing`). changeZoneReturning itself is the `seed = Nothing` case below.
changeZoneReturning :: ObjectId -> Zone -> Game (Maybe ObjectId)
changeZoneReturning oid requestedDest = changeZoneAttaching Nothing oid requestedDest Nothing TapState.Untapped Nothing Nothing

-- changeZoneReturning with an attachment seed. Per CR 303.4 attachment is a
-- property of entering, not a step after it: the CR 614.1c entry replacement loop
-- and the Moved event both run before this returns, so an Aura attached afterward
-- would be unattached during both. No card in the pool observes the difference
-- today; the seed buys the ordering rather than a passing test.
--
-- Stack's Aura branch is the only caller supplying a seed, so an Aura entering by
-- any other route enters unattached and is buried on the next SBA pass by CR
-- 704.5m -- where CR 303.4g says it should instead stay in its current zone (#188).
--
-- `asOf` is changeZoneInBatch's batch board, Nothing otherwise. `tapped` is CR
-- 110.5b's status, Untapped for every door but changeZoneEntering. `under` is CR
-- 110.2a's entry controller, Nothing for every door but changeZoneEntering --
-- and Nothing there too for a move whose effect names no player, which by CR
-- 110.2 and CR 108.4a leaves the owner answering. `shown` is CR 709.3's chosen
-- half, Nothing for every door but changeZoneShowing.
changeZoneAttaching :: Maybe GameState -> ObjectId -> Zone -> Maybe Recipient.Recipient -> TapState.TapState -> Maybe PlayerId -> Maybe CardName.CardName -> Game (Maybe ObjectId)
changeZoneAttaching asOf oid requestedDest seed tapped under shown = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure Nothing
    Just obj -> do
      let pid = Object.owner obj
          fromZone = Object.zone obj
          -- CR 608.2h: last known information -- the object as it exists in the
          -- zone it is LEAVING, projected against the pre-move state. Forced
          -- eagerly (Moved's snapshot field is strict) rather than left as a thunk
          -- retaining the whole pre-move GameState for a turn. The price of an
          -- honest history: a token has no printed card to re-derive from (CR
          -- 111.1).
          snapshot = Projection.project oid gs
          -- CR 613.1b: the OTHER half of last known information, read from the
          -- same pre-move state. Control is not a characteristic (CR 109.3's
          -- list does not include it), so it cannot ride `snapshot`; it is kept
          -- because CR 603.3a asks "who controlled its source at the time it
          -- triggered" about sources that are already gone -- see
          -- eventTriggers below.
          --
          -- The `Object.owner` fallback is unreachable rather than a guess:
          -- Projection.controllerOfGiven's own base case returns
          -- `Just (Object.owner obj)` for any id that resolves, and `oid`
          -- resolves here (this branch matched `Just obj`). It is written as a
          -- fallback only because controllerOf's type is honest about ids that
          -- do not.
          --
          -- A second board walk on the same hot path as `snapshot` above
          -- (controllerOf rebuilds controlGrants and its liveGiven fixpoint).
          -- Measured on the tasty-bench suite, this commit's parent vs. this
          -- change (goldfish / casting / fighting / fighting-aura, 2p):
          -- 15.2/133/24.6/569 ms -> 15.5/134/25.2/575 ms -- every move inside
          -- one run-to-run stddev, so no gate was moved to buy it back.
          lastController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
      -- CR 614.4: replacements exist before the event, so the loop reads them from
      -- the PRE-MOVE state. CR 614.6: the modified event is what actually happens.
      --
      -- `obj` and `snapshot` are read from `gs` before this runs and still used
      -- after it returns, which is sound despite Replacement's AsCopy arm calling
      -- State.modify': this is a WouldChangeZone loop, restricted to ZoneChangeR
      -- candidates, so it cannot reach the EntryR arm AsCopy lives under -- and
      -- `gs` is an immutable value, so no downstream modify' can change what
      -- `snapshot` captured. Extending either loop to mutate state these bindings
      -- read would mean re-deriving them after that loop.
      --
      -- Both ids are `oid` in the PROPOSED event: nothing has moved yet.
      resolved <- resolveZoneChange asOf (ZoneChange.MkZoneChange oid oid fromZone requestedDest)
      case resolved of
        -- CR 614.6: nothing survived the loop, so no zone change happens. No
        -- producer today -- no card in the pool cancels a zone change outright --
        -- but Maybe is what "the event does not happen" means on this path.
        Nothing -> pure Nothing
        Just settled -> do
          let dest = ZoneChange.to settled
              -- CR 110.2a: "If an effect instructs a player to put an object onto
              -- the battlefield, that object enters the battlefield under that
              -- player's control unless the effect states otherwise." That
              -- control is BASE STATE -- CR 110.2 makes the entry controller a
              -- permanent's default controller thereafter, which is what
              -- Projection.defaultControllerOf reads -- and not a CR 613.1b
              -- layer-2 effect, which is the distinction CR 800.4c draws and
              -- which decides whether CR 800.4a's second clause can end it
              -- (Pawl.DepartureSpec's Meandering Towershell case is the proof).
              --
              -- BATTLEFIELD ONLY, the rule's own scope (CR 110.2, CR 110.5d):
              -- Projection.controllerOf answers for an object in any zone, so an
              -- ungated write would give a graveyard card a controller.
              --
              -- Gated on the SETTLED destination rather than the requested one,
              -- so a CR 616.1 rewrite that redirects the move decides this too
              -- (CR 614.6: the modified event is what happens). Indistinguishable
              -- from gating on the request today, and not because of a claim
              -- about Magic: no ReplacementEffect.ZoneChangeR in the pool names
              -- the battlefield as its destination (Leyline of the Void and Rest
              -- in Peace, the two that exist, both name exile).
              --
              -- CR 400.7: Object.newIncarnation is the whole forgetting -- the
              -- entry controller (CR 110.2), the as-enters choices (CR 614.1c),
              -- damage, counters, bindings and the rest all go back to their
              -- no-memory values there, and Setup's two hand-written moves into
              -- a library call the same function. What is set back here is only
              -- what this MOVE decides: the destination, CR 613.7d's moment of
              -- entry, CR 110.5b's "enters tapped" (meaningful only for a
              -- battlefield destination, CR 110.5a), CR 110.2a's entry
              -- controller, CR 701.3's attach-on-entry seed, and CR 709.3a's
              -- chosen half.
              --
              -- `face` is among what newIncarnation clears, which is right by
              -- default: whichever half CR 709.3b singled out belonged only to
              -- the incarnation that left, and CR 709.4 gives a split card its
              -- two halves combined everywhere but the stack. `shown` is the
              -- exception the rules name -- CR 709.3, where the choice of half
              -- is made BEFORE the card is put onto the stack, and CR 709.3a,
              -- where "only that half is considered to be put onto the stack".
              -- Set HERE rather than written onto the returned incarnation, so
              -- that no reader inside the move can see the CR 400.7 object
              -- without its half.
              --
              -- Gated on the move ARRIVING where it was headed, which is the
              -- reading only a writer inside the move can have: CR 709.3a's half
              -- is "considered to be put onto the stack", so a CR 616.1 redirect
              -- that settles on another destination (CR 614.6: the modified event
              -- is what happens) means the card was never put onto the stack at
              -- all, and CR 709.4 gives it its two halves combined wherever it
              -- did land. Pawl.CastSpec's "a cast redirected off the stack keeps
              -- both halves" is the proof, and fails under the pre-#781 ordering
              -- -- a stamp applied to whatever the move handed back cannot ask
              -- this question, because the caller named a destination the move
              -- was free to overrule.
              --
              -- The SETTLED destination against the REQUESTED one rather than
              -- against Zone.Stack: the rule is that the face describes the move
              -- the caller asked for, not that a face is only ever a stack half.
              -- CR 712.13's face carried out of the stack (#657) is the same
              -- shape with Battlefield in both slots.
              --
              -- What is still dropped is the face a move OUT of the stack should
              -- carry -- CR 712.13: "a resolving double-faced spell that becomes
              -- a permanent is put onto the battlefield with the same face up
              -- that was face up on the stack". Not implemented; the caller that
              -- would pass it is Pawl.Engine.Stack's permanent branch, and no
              -- printing in the pool makes the dropped face differ from the one
              -- the destination resolves anyway (#657).
              mkObj ts =
                (Object.newIncarnation obj)
                  { Object.zone = dest,
                    Object.timestamp = ts,
                    Object.tapped = tapped,
                    Object.attachedTo = seed,
                    Object.enteredUnder = if dest == Zone.Battlefield then under else Nothing,
                    Object.face = if dest == requestedDest then shown else Nothing
                  }
          State.modify' $ \g ->
            let g1 = Game.removeFromZones pid oid g
             in g1
                  { GameState.objects = Map.delete oid (GameState.objects g1),
                    -- CR 608.2h: the object ceases here, so this is the last
                    -- moment its information is known. Filed under the id it had
                    -- while it existed -- the id an ability on the stack still
                    -- carries as its source (CR 113.7) -- and from the same
                    -- `snapshot` the Moved event below records, so the two
                    -- readings of "what was it" cannot drift apart.
                    GameState.lastKnown = Map.insert oid (LastKnown.MkLastKnown snapshot lastController (Object.source obj)) (GameState.lastKnown g1)
                  }
          newId <- placeObject pid mkObj dest
          -- CR 614.1c-d: entry replacements apply to BATTLEFIELD entries and
          -- nowhere else. CR 616.1g's nesting of one event inside another is
          -- expressed as call nesting rather than a field. A lone entry has no
          -- same-batch siblings (CR 614.12a; see applyReplacementsIn
          -- for why 614.12a and not 614.13a).
          Monad.when (dest == Zone.Battlefield) (runEntry Set.empty newId)
          -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
          -- what an enters trigger scans -- alongside the id it had in `fromZone`,
          -- which is the key `lastKnown` is filed under and so the only route back
          -- once CR 400.7 has minted a new incarnation (CR 603.10a's look-back
          -- reads it). Recorded LAST, so the entry loop's choices are locked in
          -- before any trigger or SBA can observe the object.
          State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange oid newId fromZone dest) snapshot))
          pure (Just newId)

-- The single destruction funnel (CR 701.8 / 702.12b): the Destroy opcode and the
-- CR 704.5g/h state-based actions both flow through here.
--
-- Takes the WHOLE BATCH rather than one permanent at a time, because CR 608.2f
-- and CR 704.3 make the batch one simultaneous event. A lone destruction is the
-- one-element batch.
--
-- CR 702.12b's indestructible gate is judged for every member against the state
-- the batch began in, BEFORE any of them moves -- which is why this takes a list.
-- A permanent granting the others indestructible is still on the battlefield at
-- that moment even when it is itself in the batch, so it dies alone. Judging each
-- member against a board its predecessors had left would make the answer depend on
-- an order CR 608.2f gives nobody the right to decide.
--
-- The gate also precedes the replacement loop, per CR 614.7: an event that never
-- happens neither applies nor consumes a regeneration shield. Otherwise the
-- would-be-destroyed event is offered to CR 616.1, and a survivor goes to its
-- owner's graveyard via changeZone. Ungated for CR 701.19c (#42).
--
-- The door for a batch that is a whole event whose caller does not care what died;
-- destroyReturning is the same door for a caller that does, destroyInBatch for a
-- batch nested in a larger event, and destroyIn is the shared body.
destroy :: Regenerability.Regenerability -> [ObjectId] -> Game ()
destroy regenerability oids = Monad.void (destroyIn Nothing regenerability oids)

-- destroy, answering with the permanents it ACTUALLY destroyed (CR 701.8b), which
-- is emphatically not the batch it was handed: an indestructible member never
-- reaches the destruction event (CR 702.12b) and a regenerated one has it replaced
-- (CR 701.8c), so neither is in this answer.
--
-- Each surviving destruction reports the SETTLED object rather than the one asked
-- about, for the reason the graveyard move follows it -- a CR 616.1 rewrite may
-- redirect the destruction. Nothing in the pool does, so the lists are equal today.
--
-- A second door rather than a return type on `destroy`, the changeZoneReturning
-- posture: only the Destroy opcode's bound-count slot uses the answer.
destroyReturning :: Regenerability.Regenerability -> [ObjectId] -> Game [ObjectId]
destroyReturning = destroyIn Nothing

-- destroy for a batch that is one PART of a larger simultaneous event, whose board
-- is `asOf`. CR 704.3's state-based-action check is that event, and Sba is the
-- only caller: its put-into-graveyard and destruction batches are a sequence only
-- in the implementation, so both stand on the board the pass began in -- an
-- animated Rest in Peace the pass itself buries still exiles the card of the
-- creature the pass destroys.
--
-- A separate door rather than a parameter on `destroy`, for changeZoneInBatch's
-- reason: every other caller has no larger event to name.
destroyInBatch :: GameState -> Regenerability.Regenerability -> [ObjectId] -> Game ()
destroyInBatch asOf regenerability oids = Monad.void (destroyIn (Just asOf) regenerability oids)

-- The shared body. Three readers of a board, and they do NOT all get the same one:
--
--   1. The CR 616.1 replacement loops -- the destruction's and the
--      put-into-graveyard that follows -- collect from `gs`, the containing
--      event's board. That is CR 608.2f / 704.3's "single event" reading: the
--      effects in force are the ones that existed before it, so an effect
--      belonging to a permanent the same event is removing still applies.
--   2. The CR 702.12b gate reads `gs` too. Indestructibility is a fact about the
--      permanent when the event's conditions were judged; letting an earlier part
--      of the same event change it would make the answer depend on an order CR
--      608.2f gives nobody the right to decide.
--   3. The existence filter reads `live`, NOT `gs`, per CR 614.7: an object an
--      earlier part of the event already put into a graveyard is not on the
--      battlefield to be destroyed, so no destruction event happens for it and no
--      regeneration shield may be offered one. The reachable shape is an Aura
--      named by CR 704.5m and CR 704.5g in the same pass.
--
-- Only the graveyard move's loop can observe (1) today: every DestructionR in the
-- pool is a regeneration shield in the floating store rather than a permanent's
-- printed ability, so the frozen board holds nothing for the destruction loop to
-- find. It is passed anyway because the rule, not the pool, says the two loops are
-- one event.
destroyIn :: Maybe GameState -> Regenerability.Regenerability -> [ObjectId] -> Game [ObjectId]
destroyIn asOf regenerability oids = do
  live <- State.get
  let gs = Maybe.fromMaybe live asOf
      doomed = filter (\oid -> Maybe.isJust (Game.lookupObject oid live) && not (Projection.hasKeyword Keyword.Type.Indestructible oid gs)) oids
  fmap Maybe.catMaybes . Monad.forM doomed $ \oid -> do
    settled <- resolveDestruction (Just gs) regenerability oid
    case settled of
      -- CR 701.8c: a regeneration effect REPLACED the destruction, so nothing was
      -- destroyed here and this member is not in the answer.
      Nothing -> pure Nothing
      -- The graveyard move follows the SETTLED object, so a rewrite redirecting
      -- the destruction is honoured. changeZone is a no-op for an object already
      -- gone, which is what makes naming the batch's members up front safe.
      Just target -> do
        changeZoneInBatch gs target Zone.Graveyard
        pure (Just target)

-- The single countering funnel (CR 701.6a -- not to be confused with putCounters
-- above, CR 122.6's counter markers).
--
-- Two endings, because that rule's last sentence is about a SPELL and its first
-- two about "a spell or ability". Which applies is decided by Game.isAbility, a
-- classification of the object's kind and never a question about the countering
-- effect:
--
--   * a SPELL goes to its owner's graveyard through the changeZone funnel, so
--     Rest in Peace's redirect and CR 400.7's new incarnation still compose;
--   * an ABILITY ceases (CR 608.2n). Not a zone change: an ability has no owner's
--     graveyard, nothing arrives anywhere, and there is no destination for CR 614
--     to replace.
--
-- The ability branch records NO event, so no trigger can watch it (#541). Widening
-- GameEvent.SpellCountered is the wrong direction -- its one reader asks about
-- countering A SPELL and must stay silent here.
--
-- TWO "can't be countered" gates, one per carrier. CR 101.2 makes either the whole
-- story: the countering effect resolves and does nothing. Neither is targeting
-- immunity -- neither rule grants shroud -- which is why the gates are here and
-- not in Target. They precede the zone change for destroy's CR 614.7 reason.
--
--   * CR 113.6g's, read off the SPELL's own card (Rending Volley), since there is
--     no battlefield projection of a spell. Self-referential by that rule's own
--     wording -- "an object's ability that states IT can't be countered" -- so it
--     is asked only on the spell branch: an ability on the stack has no card for
--     it to be printed on, and Game.faceOf answers Nothing for one.
--   * CR 611.1 / 613.11's, asked of Pawl.Engine.PlayerEffect (Spider-Punk). That
--     one is an ability of a BATTLEFIELD PERMANENT about other objects, so it is
--     gathered from the battlefield like any other player effect and reaches BOTH
--     of CR 701.6a's subjects -- which is why it, unlike the gate above, is asked
--     ahead of the branch split rather than inside one branch. It is asked of the
--     VICTIM's controller: CR 113.8 for an ability, CR 601.2a for a spell.
--
-- On the spell branch, records a SpellCountered ALONGSIDE the zone change's Moved
-- event, never instead of it: the Moved event is the CR 400.7 change and this one
-- is what the change WAS. That is what distinguishes a countered spell from one
-- that RESOLVED into the same graveyard (CR 608.2n), and what survives Rest in
-- Peace redirecting the destination.
--
-- Nothing is recorded on any of the four paths that do NOT counter, which CR
-- 603.2g makes mandatory rather than tidy: an id with no object; either gate,
-- since through CR 101.2 such a spell was never countered; and a move the CR
-- 616.1 loop cancelled, which leaves the spell on the stack. The ability branch is
-- not a fifth -- that countering really happened, and its silence is #541.
--
-- `source` and `controller` are the countering spell or ability and its controller
-- (CR 405.4), taken from the caller rather than re-derived: by the time the CR
-- 117.5 scan reads this event the controller can no longer be asked for exactly
-- (see Pawl.Types.Countering).
counter :: ObjectId -> PlayerId -> ObjectId -> Game ()
counter source controller oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    -- CR 613.11's gate first, ahead of the branch split, because it is the one
    -- that reaches both of CR 701.6a's subjects.
    Just _ | protectedFromCountering oid gs -> pure ()
    -- CR 608.2n, reached before the CR 113.6g gate because that gate asks about a
    -- spell's own card and an ability has none -- Game.faceOf answers Nothing for
    -- one, so asking first would fall through to the graveyard move by accident.
    Just _ | Game.isAbility oid gs -> State.modify' (Game.cease oid)
    Just _ -> case fmap Face.counterability (Game.faceOf oid gs) of
      Just Counterability.CantBeCountered -> pure ()
      _ -> do
        moved <- changeZoneReturning oid Zone.Graveyard
        case moved of
          Nothing -> pure ()
          Just _ ->
            State.modify'
              . recordEvent
              $ GameEvent.SpellCountered
                Countering.MkCountering
                  { Countering.spell = oid,
                    Countering.source = source,
                    Countering.controller = controller
                  }

-- CR 611.1 / 613.11: does a rules-modifying continuous effect stop this spell or
-- ability from being countered (Spider-Punk, Prowling Serpopard)? The victim's
-- controller is the player the effect is anchored against -- CR 113.8 for an
-- ability on the stack, CR 601.2a for a spell -- and an object with no
-- controller is protected by nothing. The victim's own id rides along too, since
-- a narrowed effect names the victim's characteristics ("creature spells you
-- control can't be countered") and not only its controller.
--
-- The typed question, so this module never sees a PlayerEffect constructor;
-- Pawl.Engine.PlayerEffect.cantBeCountered is where the casing lives.
--
-- Projection.controllerOf, which is what every other reader of a stack object's
-- controller already asks (Replacement.decider, PlayerEffect.matchesSpell). For a
-- SPELL that is a re-derivation rather than the stored fact CR 405.4 describes,
-- and it falls back to the owner (#83); a spell cast from a zone its owner does
-- not hold would therefore be read against the wrong player here.
protectedFromCountering :: ObjectId -> GameState -> Bool
protectedFromCountering oid gs =
  maybe False (\pid -> PlayerEffect.cantBeCountered pid oid gs) (Projection.controllerOf oid gs)

-- CR 701.21/701.21a: the single sacrifice funnel. The permanent goes to its
-- OWNER's graveyard through changeZone, and -- unlike destroy -- with no
-- indestructible gate and no regeneration shield consulted, since sacrificing is
-- not destroying. Restricted to permanents on the battlefield, so anything else is
-- a no-op.
--
-- CR 701.21a also forbids sacrificing a permanent you do not control, which is why
-- this takes the sacrificing player. Enforced here at the one funnel rather than
-- trusted from each caller: a cost payment, a triggered ability's own source and
-- `apply`'s CR 614.1c as-enters sacrifice are controlled by the paying player by
-- construction, but an edict's victim is a permanent a PLAYER named.
sacrifice :: PlayerId -> ObjectId -> Game ()
sacrifice pid oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Object.zone obj of
      -- CR 701.21a: "A player can't sacrifice something that isn't a permanent, or
      -- something that's a permanent they don't control." The zone case below is
      -- the first clause; this is the second. Enforced HERE, at the one funnel,
      -- rather than trusted from each caller -- the callers are a cost payment, a
      -- trigger's own source, `apply`'s CR 614.1c as-enters sacrifice, and an
      -- edict whose victim a player NAMED, and only the last of those could ever
      -- be wrong.
      Zone.Battlefield
        | Projection.controllerOf oid gs /= Just pid -> pure ()
        | otherwise -> changeZone oid Zone.Graveyard
      Zone.Library -> pure ()
      Zone.Hand -> pure ()
      Zone.Graveyard -> pure ()
      Zone.Stack -> pure ()
      Zone.Exile -> pure ()
      -- CR 408.1: a command-zone object is not a permanent, so it is never
      -- sacrificed.
      Zone.Command -> pure ()

-- CR 111.2: create `n` tokens with the given effect-defined characteristics under
-- `controller`'s control, summoning-sick (CR 302.6). A token is created from
-- nothing, so changeZone cannot mint it. Uses from = Battlefield, where to == from
-- can never read as a leave, and emits the enters event so CR 603.6a triggers fire
-- on the path a resolved permanent uses.
--
-- Plural by rules requirement, not convenience: CR 614.1/614.16 replacements scope
-- to the CREATION EVENT rather than each token, so the count is settled once up
-- front. Every token is then materialized, and only then does each run its OWN
-- entry loop -- CR 616.1g's containment, since creating a token contains that
-- token entering. Each entry loop is handed the whole batch, which excludes
-- simultaneously-entering siblings from any copy choice (CR 614.12a; see
-- applyReplacementsIn for why 614.12a and not 614.13a).
--
-- That nesting is design intent no test exercises: every token card in the pool
-- has empty `replacementEffects`, so each entry loop returns immediately. CR
-- 616.1g's own worked example needs a token WITH an entry replacement (#73).
--
-- CR 800.4b / 800.4d: no token is created for a player who has left the game. The
-- two sentences coincide by CR 111.2, which makes a token's owner and controller
-- the same player, so one guard satisfies both. Checked BEFORE resolveTokens: the
-- rule says no token is created, so nothing may be minted and nothing spent
-- getting there -- resolveTokens consumes CR 614.3 use counts.
--
-- Not implemented: the guard reads the PARAMETER, so a CR 616.1b control rewrite
-- applied by the entry loop can still hand the finished token to a player who has
-- left (#592).
--
-- Inline rather than delegating to a `createTokensFor` body: the project writes no
-- export lists, so a second top-level name would be a public door past the check.
createTokens :: PlayerId -> Card -> Natural -> TapState.TapState -> Game [ObjectId]
createTokens controller card n tapped = do
  gs <- State.get
  if List.notElem controller (Game.stillPlaying gs)
    then pure []
    else do
      resolved <- resolveTokens controller card n
      case resolved of
        Nothing -> pure []
        Just (owner, tokenCard, count) -> do
          let mkObj ts =
                Object.MkObject
                  { Object.owner = owner,
                    Object.enteredUnder = Nothing,
                    Object.source = Source.OfToken tokenCard,
                    Object.zone = Zone.Battlefield,
                    -- CR 110.5b: untapped unless an effect says otherwise, which
                    -- is why the caller supplies this rather than the default
                    -- being taken and the token tapped after.
                    Object.tapped = tapped,
                    Object.damage = 0,
                    Object.sickness = Sickness.Sick,
                    Object.bindings = Map.empty,
                    Object.counters = Map.empty,
                    Object.attachedTo = Nothing,
                    Object.chosenColor = Nothing,
                    Object.chosenSubtype = Nothing,
                    Object.chosenNames = Set.empty,
                    Object.timestamp = ts,
                    Object.face = Nothing,
                    Object.turnedOverAt = Nothing,
                    Object.playableFromExileBy = Nothing,
                    Object.ringBearerFor = Nothing
                  }
          ids <- Monad.replicateM (Natural.toIntSaturating count) (placeObject owner mkObj Zone.Battlefield)
          Monad.mapM_ (runEntry (Set.fromList ids)) ids
          -- No prior incarnation to snapshot, so a token's last known information
          -- IS what it is now (CR 111.3). Recorded after every entry loop, so the
          -- events describe settled objects.
          Monad.mapM_ recordTokenEntry ids
          pure ids

-- Nothing departed, so `departed` is the token's own id. Harmless rather than a
-- fiction readers must know about: from == to == Battlefield already fails every
-- departure test (CR 603.6c), and a token has no `lastKnown` entry to find.
recordTokenEntry :: ObjectId -> Game ()
recordTokenEntry newId = do
  placed <- State.get
  let snapshot = Projection.project newId placed
  State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId newId Zone.Battlefield Zone.Battlefield) snapshot))

-- CR 121.1, one card at a time per CR 121.2. An empty library records the failed
-- draw, which CR 704.5b makes a loss at the next state-based-action check. Shared
-- by the draw step, opening hands and the Draw effect.
drawCard :: PlayerId -> Game ()
drawCard pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> State.put gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
    top : _ -> changeZone top Zone.Hand

-- The single discard funnel (CR 701.9a). `pid` is the discarding player, whom that
-- rule makes the card's owner either way, and `cause` is why (see DiscardCause).
--
-- The move goes through the CR 400.7 funnel, so a discarded card gets a new
-- incarnation and Rest in Peace's redirect composes. The EVENT is what this adds,
-- and it is not redundant with the Moved event: per CR 701.9c a redirected discard
-- is still a discard while its Moved event no longer reads hand-to-graveyard, so a
-- discard trigger reads this record and never the zone pair.
--
-- Recorded only when the move COMPLETED -- an unknown id or a cancelled CR 616.1
-- loop must record nothing (CR 603.2g). The id recorded is the one the funnel
-- MINTED, since CR 702.29c's abilities trigger from wherever the card winds up.
discard :: DiscardCause.DiscardCause -> PlayerId -> ObjectId -> Game ()
discard cause pid oid = do
  moved <- changeZoneReturning oid Zone.Graveyard
  case moved of
    Nothing -> pure ()
    Just newId -> State.modify' (recordEvent (GameEvent.Discarded pid newId cause))

-- The single reveal funnel (CR 701.20a): `pid` shows `oid` to all players, which
-- here means appending what was shown to the public log. No-op for an unknown id.
-- Per CR 701.20b nothing moves and nothing changes, so the event is the whole
-- effect -- the rule, not a shortcut.
--
-- The snapshot is Projection.project, deliberately not the printed-card view a
-- search filter matches a library card through. The two can disagree: CR 604.3
-- makes a CDA function in all zones, so a Tarmogoyf in a library has a power that
-- viewOfCard reports as Nothing. A search may ignore that; a reveal may not,
-- having to show what a player at the table would see. No card in the pool makes
-- them differ today.
reveal :: PlayerId -> ObjectId -> Game ()
reveal pid oid = do
  gs <- State.get
  Monad.when (Maybe.isJust (Game.lookupObject oid gs)) $
    State.modify' (recordEvent (GameEvent.Revealed pid (Projection.project oid gs)))

-- CR 508.3a / 608.2i: how many times this object has been declared as an attacker
-- this turn, read out of the turn-scoped event log. Only Combat.declareAttackers
-- appends the event, which keeps CR 508.4's creature put onto the battlefield
-- attacking -- one that never attacked -- out of the count.
declarationsOf :: ObjectId -> GameState -> Int
declarationsOf bearer gs =
  let declaredIt event = case event of
        GameEvent.AttackerDeclared oid -> oid == bearer
        _ -> False
   in length (Seq.filter declaredIt (GameState.events gs))

-- CR 603.2: does this condition fire on this event, for the permanent that bears
-- it? `bearer` is the object whose ability this is and `you` its controller (CR
-- 603.3a, CR 109.5); both are part of the match because the scan visits EVERY
-- permanent, not only the one an event names. The sole home of casing on
-- TriggerCondition for rules purposes -- Pawl.Codec also cases on every
-- constructor, but only at the JSON boundary.
matchesTrigger :: GameState -> ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTrigger gs bearer you cond event = case cond of
  -- CR 603.6a: the bearer's own object entered the battlefield.
  TriggerCondition.SelfEnters -> case event of
    GameEvent.Moved zc _ -> ZoneChange.object zc == bearer && ZoneChange.to zc == Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 603.6a's "whenever a [type] enters": a permanent the Filter admits
  -- entered the battlefield. The bearer frames the match rather than being it --
  -- it is the Filter.Context's source (so `Not IsSource` is Soul Warden's
  -- "another"), and its controller is the perspective CR 109.5 gives "you" in
  -- "a creature YOU CONTROL enters".
  TriggerCondition.PermanentEnters f -> case event of
    GameEvent.Moved zc _
      | ZoneChange.to zc == Zone.Battlefield ->
          -- Deliberately NOT the snapshot the Moved event carries: that is the
          -- object as it last existed in the zone it LEFT, and reading it here
          -- would answer CR 603.6b backwards. The entrant's characteristics come
          -- from the game as it stands, which is what CR 603.10 asks for.
          --
          -- viewWithLastKnown rather than viewOfObject, so an entrant that has
          -- already left again -- a creature entering as a 0/0 and buried by CR
          -- 704.5f before the CR 117.5 boundary -- is still read as it was on the
          -- battlefield (CR 608.2h) instead of vanishing from the match.
          --
          -- Recomputed per (bearer, entry event) pair rather than shared: this is
          -- handed the GameState and nothing else. Forced only inside this arm, so
          -- a board with no such ability pays nothing.
          --
          -- Nothing is an entrant that is gone AND filed no last known information,
          -- about which no Filter can honestly answer.
          let entrant = ZoneChange.object zc
           in case Projection.viewWithLastKnown entrant gs entrant of
                Nothing -> False
                Just view -> Filter.matches (Filter.MkContext (Just you) (Just bearer)) view f
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 603.2b: this step began, on a turn the scope admits.
  TriggerCondition.StepBegins wanted scope -> case event of
    GameEvent.StepBegan began active ->
      began == wanted && case scope of
        TurnScope.EachTurn -> True
        TurnScope.ControllersTurn -> active == you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 603.8: a state trigger is not an event trigger. It never matches an entry
  -- in the log; stateTriggers below is its whole story.
  TriggerCondition.StateIs _ -> False
  -- CR 510.1b / 510.2: the bearer dealt COMBAT damage to a PLAYER. Combat damage
  -- already records a DamageDealt event, so this is a filter over the log.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.source ev == bearer
        && DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
    GameEvent.Moved _ _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 725.2: never matched via a card's bearer -- the monarch's crown-steal is
  -- an inherent ability of no object, so its real match lives in
  -- Pawl.Engine.Monarch.inherentMatch, not here.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  -- CR 702.179d: the same, one rule over. The speed-increase ability hangs on no
  -- object either, so Pawl.Engine.Speed.inherentPending is where it is matched.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  -- CR 702.29c: the bearer IS the card that was cycled. The event carries the CR
  -- 400.7 incarnation, which is the object the scan offers as the bearer.
  --
  -- The CAUSE makes this narrower than the discard condition below, and is the
  -- whole of rule 702.29c's "to pay an activation cost of a cycling ability": an
  -- ordinary discard of a card that HAS cycling reaches the same graveyard through
  -- the same funnel and must fire nothing.
  TriggerCondition.SelfCycled -> case event of
    GameEvent.Discarded _ oid DiscardCause.ToPayCyclingCost -> oid == bearer
    GameEvent.Discarded _ _ DiscardCause.Ordinary -> False
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 701.9a: a card was discarded, by a player the relation admits. The
  -- discarding player comes from the event; CR 109.5 fixes "you" as the
  -- ability's controller (CR 603.3a), and Megrim's "an opponent" is every other
  -- player -- CR 806.1 in a free-for-all, CR 102.2 in a two-player game, the
  -- same /= either way. CR 102.3's teams are the one reading it is wrong for,
  -- and pawl has none to express (#175).
  --
  -- The bearer is NOT part of the match, unlike every Self- condition here: the
  -- enchantment watches someone else's hand and has nothing to do with the card
  -- that left it.
  --
  -- CR 702.29d -- "these abilities trigger only once when a card is cycled" --
  -- needs no clause of its own, and the DiscardCause is ignored for that reason
  -- rather than by omission. CR 702.29a makes cycling a discard, so a cycled
  -- card must fire this; the cycle is ONE Discarded event, so it fires it once.
  -- TriggerSpec's "CR 702.29d cycling a card fires the discard trigger exactly
  -- once" is the test that proves it.
  TriggerCondition.PlayerDiscards relation -> case event of
    GameEvent.Discarded discarder _ _ -> case relation of
      PlayerRelation.You -> discarder == you
      PlayerRelation.Opponent -> discarder /= you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 508.3a: the bearer was DECLARED as an attacker. Matched against the
  -- declaration event rather than Combat.attackers, which keeps that rule's last
  -- sentence true -- a creature put onto the battlefield attacking is in the
  -- record and has no event here.
  TriggerCondition.SelfAttacks frequency -> case event of
    GameEvent.AttackerDeclared oid ->
      oid == bearer && case frequency of
        TriggerFrequency.EveryTime -> True
        -- "For the first time each turn". The declaration being matched is
        -- already in the log when the scan reaches here, so "the first time" is
        -- "the only one so far", and the log's clearing at turn handoff is what
        -- makes it "each turn". Counted per BEARER, and CR 400.7 mints a new
        -- object on a zone change, so a creature that left and returned attacks
        -- for the first time again.
        TriggerFrequency.FirstTimeEachTurn -> declarationsOf bearer gs <= 1
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 603.6: a zone-change trigger matched on BOTH ends of the move, library to
  -- graveyard. The bearer is the incarnation the card became on arrival per CR
  -- 400.7e, a graveyard being public (CR 400.2). The pair is also what makes CR
  -- 113.6k put this ability in the graveyard rather than on the battlefield.
  --
  -- `from` is the half that does the work: the same card discarded out of a hand
  -- or dying off the battlefield reaches the same graveyard and must not trigger.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> case event of
    GameEvent.Moved zc _ ->
      ZoneChange.object zc == bearer
        && ZoneChange.from zc == Zone.Library
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 603.6 with NO origin zone: the destination is the whole condition, so a
  -- discard, a mill, a countered spell and a death all match. `from` is
  -- deliberately unread, which is the one line separating this from the two
  -- conditions on either side of it.
  --
  -- Matched on `object`, the arriving incarnation, and NOT on `departed`: CR
  -- 603.6c's last sentence takes this out of the leaves-the-battlefield family,
  -- so CR 603.10a's look-back does not reach it and CR 603.10's normal reading --
  -- the objects that exist immediately after the event -- applies. That is also
  -- what makes the graveyard the one zone the scan has to find the bearer in,
  -- however far away the card started.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> case event of
    GameEvent.Moved zc _ ->
      ZoneChange.object zc == bearer
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 603.6c narrowed by CR 700.4's definition of "dies": the bearer was put into
  -- a graveyard from the battlefield. Both ends are load-bearing -- `from` keeps a
  -- permanent DISCARDED out of a hand silent, and `to` keeps one EXILED off the
  -- battlefield silent, the latter having left the battlefield without dying.
  -- SelfLeavesTheBattlefield below is the other condition, and naming this one
  -- after the printed word is what keeps them apart.
  --
  -- Matched on `departed`, NOT `object`: CR 603.10a makes leaves-the-battlefield
  -- abilities look back in time, so the bearer offered here is the permanent as it
  -- was immediately before the event, never the CR 400.7 incarnation.
  TriggerCondition.SelfDies -> case event of
    GameEvent.Moved zc _ ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- The same rule and zone pair as SelfDies, watched by a BYSTANDER. The bearer
  -- frames the match rather than being it, as for PermanentEnters: it is the
  -- Filter.Context's source (so `Not IsSource` is "another"), and its controller
  -- is CR 109.5's "you".
  --
  -- Matched on `departed`, with characteristics from CR 608.2h last known
  -- information rather than a live read -- both CR 603.10a's look-back. The
  -- PermanentEnters arm makes the opposite choice, rightly, since CR 603.6b puts an
  -- entrant's continuous effects on the moment it is on the battlefield. A dead
  -- permanent has no live reading left, and reading the graveyard card instead
  -- would answer "you control" wrong rather than not at all: CR 108.4a substitutes
  -- the OWNER, so a stolen creature would be credited back to the player who no
  -- longer had it when it died.
  --
  -- viewWithLastKnown aimed at the deceased twice over, which is how it is asked
  -- for the snapshot: it takes the last-known branch only for the id it is
  -- anchored to, and only once that id is gone.
  --
  -- Nothing is a permanent that is gone AND filed no last known information, about
  -- which no Filter can honestly answer.
  TriggerCondition.PermanentDies f -> case event of
    GameEvent.Moved zc _
      | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard ->
          let deceased = ZoneChange.departed zc
           in case Projection.viewWithLastKnown deceased gs deceased of
                Nothing -> False
                Just view -> Filter.matches (Filter.MkContext (Just you) (Just bearer)) view f
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 603.6c taken whole. The `from` half matches SelfDies'; the `to` half is
  -- where they part company, this one asking only that the destination be ANOTHER
  -- zone.
  --
  -- The `to /= Battlefield` guard is that rule's own word "another", and is
  -- load-bearing: recordTokenEntry files a battlefield-to-battlefield pseudo-move
  -- whose `departed` is the token's own id, so a token bearing this condition
  -- would fire on its own creation without it.
  --
  -- Matched on `departed` for SelfDies' reason (CR 603.10a).
  --
  -- Not matched: CR 603.6c's other trigger event, a phased-in permanent leaving
  -- the game with its owner (#385).
  TriggerCondition.SelfLeavesTheBattlefield -> case event of
    GameEvent.Moved zc _ ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc /= Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 701.6a: a spell was countered, by a spell or ability whose controller the
  -- relation admits. The countering source's controller comes from the event,
  -- captured as the counter happened, and CR 109.5/603.3a fix "you" as the
  -- ability's controller.
  --
  -- The bearer is NOT part of the match -- the PlayerDiscards posture rather than
  -- any Self- condition's -- since the bearer is a permanent and the countering is
  -- done by a spell somewhere else.
  --
  -- Neither "can't be countered" gate needs a clause here -- CR 113.6g's on the
  -- spell, CR 613.11's on a permanent's static ability: a spell that can't be
  -- countered is not countered at all (CR 101.2), so `counter` records nothing
  -- and there is no event for this arm to see.
  TriggerCondition.SpellOrAbilityCounters relation -> case event of
    GameEvent.SpellCountered c -> case relation of
      PlayerRelation.You -> Countering.controller c == you
      PlayerRelation.Opponent -> Countering.controller c /= you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 615.13: a prevention effect was applied and prevented some damage, and the
  -- damage it prevented was addressed to a player the relation admits. CR 109.5 /
  -- 603.3a fix "you" as the ability's controller, exactly as PlayerDiscards and
  -- SpellOrAbilityCounters do.
  --
  -- The bearer is NOT part of the match: Selfless Squire is a creature watching
  -- damage addressed to its controller, and CR 615.13 says nothing about which
  -- object the ability is on.
  --
  -- ONE fire per recorded event, and the record is already grouped per prevention
  -- effect per batch (Replacement.groupPreventions), which is where CR 615.13's
  -- "one or more simultaneous damage events" is honoured. Nothing here has to
  -- count.
  --
  -- Damage prevented to a PERMANENT is silence rather than a miss: the printed
  -- sentence says "to you", and the recipient the event carries is what
  -- distinguishes the two.
  TriggerCondition.DamageToPlayerPrevented relation -> case event of
    GameEvent.DamagePrevented recipient _ -> case recipient of
      Recipient.ToPlayer pid -> case relation of
        PlayerRelation.You -> pid == you
        -- CR 102.2: no producer today -- a card watching an opponent's damage
        -- being prevented.
        PlayerRelation.Opponent -> pid /= you
      Recipient.ToCreature _ -> False
      Recipient.ToPlaneswalker _ -> False
      Recipient.ToObject _ -> False
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 119.9: a source caused a player the relation admits to gain life. The
  -- gaining player comes from the event; CR 109.5 / 603.3a fix "you" as the
  -- ability's controller, exactly as PlayerDiscards, SpellOrAbilityCounters and
  -- DamageToPlayerPrevented above do.
  --
  -- The bearer is NOT part of the match: Ajani's Pridemate is a creature watching
  -- its controller's life total, and CR 119.9 says nothing about which object the
  -- ability is on.
  --
  -- No zero check here. CR 119.9's "if a player gains 0 life, no life gain event
  -- has occurred" is enforced where the event is RECORDED -- Resolve's GainLife
  -- arm and Damage's lifelink pass both guard their own zero -- so a
  -- GameEvent.LifeGained in the log is by construction a gain of more than 0, and
  -- a second guard here would be a second place for that invariant to live.
  --
  -- LOSING life is not a near miss but a different event, and the LifeLost arm
  -- below is where that shows: one damage event can record both, and only the
  -- gain fires this.
  TriggerCondition.PlayerGainsLife relation -> case event of
    GameEvent.LifeGained pid _ -> case relation of
      PlayerRelation.You -> pid == you
      -- CR 102.2: no producer today -- a card watching an OPPONENT gain life.
      PlayerRelation.Opponent -> pid /= you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.CountersPut {} -> False
  -- A player the relation admits LOST life -- Exquisite Blood's "whenever an
  -- opponent loses life". The losing player comes from the event; CR 109.5 /
  -- 603.3a fix "you" as the ability's controller, exactly as PlayerGainsLife
  -- above does.
  --
  -- The bearer is NOT part of the match: Exquisite Blood is an enchantment
  -- watching somebody else's life total, and nothing about the condition names
  -- the object the ability is on.
  --
  -- Which life-total movements are a loss is settled at the RECORDING sites and
  -- not here, since the rules print no CR 119.9 for this direction: CR 119.3's
  -- instructed loss, CR 119.2 / 120.3a's damage, and CR 119.4's paid life all
  -- write GameEvent.LifeLost, while CR 120.3b's infect diversion, CR 615.6's
  -- prevented damage and damage taken by a permanent write none. See
  -- Pawl.Types.TriggerCondition.PlayerLosesLife.
  --
  -- No zero check either, for the reason the gain arm gives: every producer
  -- guards its own zero, so a GameEvent.LifeLost in the log is by construction a
  -- loss of more than 0.
  --
  -- GAINING life is a different event, not a signed version of this one: one
  -- damage event can record a loss and a lifelink gain together, and only the
  -- loss fires this.
  TriggerCondition.PlayerLosesLife relation -> case event of
    GameEvent.LifeLost pid _ -> case relation of
      -- No producer today -- a card watching its OWN controller lose life.
      PlayerRelation.You -> pid == you
      -- Exquisite Blood's half. CR 102.2 is what makes "not you" the right test
      -- on a two-player board, as it is for Megrim under PlayerDiscards.
      PlayerRelation.Opponent -> pid /= you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeGained _ _ -> False
    GameEvent.CountersPut {} -> False
  -- CR 714.2b: counters of this kind were put onto the BEARER, and the count
  -- crossed N going up. Both halves of the rule's sentence are here -- see
  -- Pawl.Types.TriggerCondition.SelfCountersReached for why the intervening "if"
  -- is not split off into TriggeredAbility.intervening.
  --
  -- `before < n` is what stops a chapter re-firing: History of Benalia going from
  -- two lore counters to three crosses III and nothing else, and a later counter
  -- taking it from three to four crosses none of its chapters again.
  --
  -- `n <= after` rather than `n == after`, because one placement can cross several
  -- thresholds at once (CR 714.2b says "at least N", not "exactly N"): a Saga
  -- given two lore counters while it has none fires chapters I and II together.
  -- Read ahead (CR 702.155a) is the mechanic that wants the equality instead, and
  -- it is not implemented (#841).
  TriggerCondition.SelfCountersReached wanted n -> case event of
    GameEvent.CountersPut oid kind before after -> oid == bearer && kind == wanted && Saga.crossed before after n
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented _ _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Revealed _ _ -> False
    GameEvent.AttackerDeclared _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost _ _ -> False
    GameEvent.LifeGained _ _ -> False

-- CR 603.2: the bindings the EVENT contributes to a trigger it has just fired --
-- the environment in which the ability's "that player" / "that creature" is read.
-- Called only for a pair `matchesTrigger` already accepted, so an arm may assume
-- its condition's shape matched; a mismatched pair contributes nothing.
--
-- Separate from `matchesTrigger` rather than folded into a `Maybe bindings`
-- return, the two having different customers: a DELAYED ability matches several
-- events at once and carries the environment captured when it was armed (CR
-- 603.7c). The parallel for a sourceless inherent ability is
-- Monarch.inherentMatch, which has no bearer to scope a shared matcher to.
eventBindings :: TriggerCondition -> GameEvent -> Map.Map SlotName.SlotName Binding
eventBindings cond event = case (cond, event) of
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  (TriggerCondition.SelfDealsCombatDamageToPlayer, GameEvent.DamageDealt ev) ->
    case DamageEvent.target ev of
      Recipient.ToPlayer pid -> Binding.setTriggerPlayer pid Map.empty
      Recipient.ToCreature _ -> Map.empty
      Recipient.ToPlaneswalker _ -> Map.empty
      Recipient.ToObject _ -> Map.empty
  -- CR 400.7e: a zone-change trigger can find the new object the card became in
  -- the zone it moved to, if that zone is public. CR 603.6c and CR 603.6e say it
  -- from the other side.
  --
  -- ZoneChange.object, NOT `departed`, which is the whole point of this arm:
  -- `departed` is what matchesTrigger matched the bearer against (CR 603.10a's
  -- look-back) and names an id CR 400.7 has deleted, so an effect handed it would
  -- move nothing. `object` is the card in the graveyard.
  --
  -- Bound ALONGSIDE the source, not instead of it: Engine.placeBorne stamps
  -- Binding.triggerSource over these and must keep stamping the departed id, that
  -- slot being CR 113.7a's source. One printed "it", two objects.
  --
  -- CR 400.7e's public-zone proviso holds by construction here, matchesTrigger's
  -- SelfDies arm having required `to == Graveyard`; the arm below is where it
  -- becomes a real test.
  (TriggerCondition.SelfDies, GameEvent.Moved zc _) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- The same rule, with its proviso doing real work for the first time: CR 603.6c's
  -- wider condition accepts ANY destination, and CR 400.2 makes two of them hidden.
  --
  -- The binding is ABSENT for a hidden destination rather than present-but-useless:
  -- ZoneChange.object names a real card in that hand, and stamping it would hand
  -- the ability an object the rule forbids it to find. Absence is what CardSpec's
  -- slot lint reads and what Resolve's arms treat as "nothing to act on".
  --
  -- Classified by the ZONE, never by whether the card is currently visible -- CR
  -- 400.2 draws exactly that distinction.
  (TriggerCondition.SelfLeavesTheBattlefield, GameEvent.Moved zc _)
    | not (Game.isHiddenZone (ZoneChange.to zc)) ->
        Binding.setBecame (ZoneChange.object zc) Map.empty
  -- CR 400.7e again, read in the ENTRY direction: the object that moved is the
  -- entrant, and what it became is the permanent now on the battlefield --
  -- ZoneChange.object, the field the SelfDies arm reads for the same reason.
  --
  -- The SAME slot as that arm, CR 400.7e being one rule with two readings. What
  -- differs is which object CR 113.7a's source happens to be, a fact about the
  -- CONDITION rather than the slot: SelfDies matches the departing incarnation, so
  -- `triggerSource` and `became` are two incarnations of one card, while here the
  -- bearer is another permanent entirely. Two slots would have to be kept apart by
  -- every reader for a distinction no rule draws -- and Resolve, where the slot is
  -- read, never learns which condition placed the ability.
  --
  -- The public-zone proviso holds by construction here too, `to == Battlefield`
  -- having already been required.
  --
  -- Bound whatever the Filter admits, creature or not: whether the entrant can
  -- RECEIVE what the payload does is the payload's question (CR 120.1a for
  -- damage), and a binding that existed only for creatures would make the slot's
  -- presence depend on the entrant, which eventBindingSlots cannot express.
  (TriggerCondition.PermanentEnters _, GameEvent.Moved zc _) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- "That player": the discarder, which CR 701.9a makes one player and the event
  -- carries directly. The same reserved slot CR 702.70a's poisonous uses, for the
  -- same reason -- a player the EVENT names, which CR 109.5's `you` cannot stand
  -- in for.
  (TriggerCondition.PlayerDiscards _, GameEvent.Discarded discarder _ _) ->
    Binding.setTriggerPlayer discarder Map.empty
  -- CR 615.13's "that many": how much this prevention effect prevented, which is
  -- the whole reason the event carries a number. The first reserved slot holding
  -- an AMOUNT rather than a reference, read back by Quantity.InSlot off the stack
  -- object these bindings are stamped on (see Binding.eventAmount).
  --
  -- The recipient is NOT bound alongside it. Every payload in the pool acts on
  -- the ability's own source (Selfless Squire counters itself), and the player
  -- the recipient names under this condition is CR 109.5's "you", already bound.
  (TriggerCondition.DamageToPlayerPrevented _, GameEvent.DamagePrevented _ amount) ->
    Binding.setEventAmount amount Map.empty
  -- CR 119.9's "that much": how much life the gain was, which CR 603.2 makes part
  -- of the event that fired the trigger -- Sanguine Bond's "target opponent loses
  -- that much life". The SAME slot the prevention arm above stamps, one printed
  -- phrase and one number (see Binding.eventAmount).
  --
  -- The AMOUNT the event recorded, never the gainer's life total: CR 119.3
  -- adjusts a total by the gain, so the two coincide only on a board that started
  -- at nothing, and the printed word means the gain.
  --
  -- The gaining PLAYER is not bound alongside it, for the reason
  -- eventBindingSlots' arm gives: under the one relation a card in the pool uses
  -- that player is CR 109.5's "you", whom Binding.setYou already names.
  (TriggerCondition.PlayerGainsLife _, GameEvent.LifeGained _ amount) ->
    Binding.setEventAmount amount Map.empty
  -- The other direction's "that much" -- Exquisite Blood's "you gain that much
  -- life". The same slot and the same reading as the gain arm above, off an
  -- event CR 603.2 makes the number part of.
  --
  -- The AMOUNT the event recorded, never the loser's life total. Under the one
  -- relation a card in the pool uses the two are not even the same player's
  -- number: Exquisite Blood's controller is bound as "you" while the loss is an
  -- opponent's.
  --
  -- The LOSING player alongside it, under the reserved slot CR 701.9a's discard
  -- trigger already stamps: Mindcrank's "that player mills that many cards" reads
  -- both halves of one event, and CR 603.2 makes both halves part of it.
  --
  -- Bound whichever relation matched, and that is a statement about the EVENT
  -- rather than about the relation -- eventBindingSlots below answers per
  -- condition with no relation in hand, so a slot it promises has to hold for
  -- every relation the condition admits. Under You the loser is also CR 109.5's
  -- "you", so the slot is a second name for one player there; that is a
  -- redundancy, not a wrong answer, and the alternative -- binding it only under
  -- Opponent -- would make the promise depend on the relation.
  (TriggerCondition.PlayerLosesLife _, GameEvent.LifeLost pid amount) ->
    Binding.setTriggerPlayer pid (Binding.setEventAmount amount Map.empty)
  _ -> Map.empty

-- Which slots eventBindings above can stamp for a condition, as a set. A
-- CLASSIFICATION of a rule 603 trigger condition -- the sibling of
-- functionsInGraveyard below, which asks the other structural question about the
-- same closed type -- so it never reaches an ability's payload and no reader of
-- it learns what any effect IS.
--
-- Its customer is the card lint (CardSpec's "every slot a triggered ability
-- reads is bound for its condition"): an effect naming CR 400.7e's `became` or
-- CR 702.70a's `thatPlayer` under a condition that binds neither would place its
-- trigger, miss the lookup and silently do nothing, which is the worst failure
-- mode card data has.
--
-- Exhaustive with no wildcard, deliberately unlike eventBindings' own
-- `_ -> Map.empty`: that case is over (condition, event) PAIRS, where a wildcard
-- is the only way to say "this pair does not match", while a new CONDITION here
-- must force a decision rather than defaulting to "binds nothing" -- the default
-- that would silently un-lint whatever slot the new condition binds.
--
-- A PARALLEL STATEMENT, PINNED BY A TEST. This says in one dimension what
-- eventBindings says in two, so the two can drift out of agreement. Deriving
-- this from that would mean fabricating a representative GameEvent per condition
-- inside the rules core, which is fixture work the engine has no other use for;
-- the agreement is therefore pinned from the test side instead, by TriggerSpec's
-- "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps for
-- EVERY event a condition admits", which runs every condition against the events
-- that genuinely fire it and intersects the Map.keysSet of each result against
-- the answer here.
--
-- Every slot named here is GUARANTEED given a match, the only reading that makes a
-- per-CONDITION set sound: the answer must hold for every event the condition
-- admits, the card lint having no event in hand. For most conditions the readings
-- coincide, matchesTrigger having already pinned the destination or recipient.
-- SelfLeavesTheBattlefield is where they come apart, and gets the floor.
eventBindingSlots :: TriggerCondition -> Set.Set SlotName.SlotName
eventBindingSlots cond = case cond of
  -- CR 603.6a's two written forms differ only in which object the bearer is.
  -- SelfEnters matches on `object == bearer`, so CR 113.7a's source slot already
  -- names the entrant and `became` would be a second name for one object.
  -- "Whenever a [type] enters" has no such luck.
  TriggerCondition.SelfEnters -> Set.empty
  TriggerCondition.PermanentEnters _ -> Set.singleton Binding.became
  -- CR 603.2b's step beginning names no object and no player but the active one,
  -- and the active player is not what CR 109.5's `you` means.
  TriggerCondition.StepBegins _ _ -> Set.empty
  -- CR 603.8: a state trigger matches a game STATE rather than an event
  -- (matchesTrigger's StateIs arm answers False for every event), so no event
  -- contributes anything to one.
  TriggerCondition.StateIs _ -> Set.empty
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Set.singleton Binding.triggerPlayer
  -- CR 725.2's inherent ability is borne by no card, and its bindings come from
  -- Monarch.inherentMatch rather than eventBindings -- so a card declaring this
  -- condition would honestly get nothing from the event.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Set.empty
  -- CR 702.179d's ability is borne by no card either, and binds nothing at all --
  -- "your speed" is the controller's, whom Binding.setYou already names.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> Set.empty
  -- CR 702.29c's cycled card is the bearer itself, already bound as CR 113.7's
  -- source, and rule 508.3a's declared attacker likewise.
  TriggerCondition.SelfCycled -> Set.empty
  -- CR 701.9a's discarding player, which is nobody the bearer already names --
  -- Megrim's "that player" is the opponent whose hand the card left.
  TriggerCondition.PlayerDiscards _ -> Set.singleton Binding.triggerPlayer
  TriggerCondition.SelfAttacks _ -> Set.empty
  -- CR 113.6k: the bearer of a library-to-graveyard trigger IS the arriving
  -- incarnation, so binding it again under `became` would be a second name for
  -- one object. Narcomoeba reads the source slot instead.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Set.empty
  -- The same answer for the same reason: with no look-back (CR 603.6c's last
  -- sentence), this condition's bearer already IS the arriving incarnation, so
  -- `became` would be a second name for one object. Serra Avatar's "shuffle IT"
  -- reads the source slot.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Set.empty
  -- CR 400.7e: the incarnation the card became, which CR 603.10a's look-back
  -- keeps out of the source slot.
  TriggerCondition.SelfDies -> Set.singleton Binding.became
  -- Empty, and NOT PermanentEnters' `became`, though the two are the same
  -- bystander shape pointed at opposite zone changes. CR 400.7e would supply the
  -- name, a graveyard being public, but no card in the pool says anything about
  -- the permanent that died. Binding a slot nothing reads is speculative
  -- construction, so the card that needs it is the one that adds it (#616).
  TriggerCondition.PermanentDies _ -> Set.empty
  -- The same slot and rule as SelfDies, but bound only for a PUBLIC destination
  -- (CR 400.7e's proviso over CR 400.2's hidden zones), so the guaranteed floor is
  -- empty. A card whose leaves-the-battlefield payload names `became` is therefore
  -- rejected by the lint (#505).
  TriggerCondition.SelfLeavesTheBattlefield -> Set.empty
  -- CR 701.6a's countering names two objects and a player and this binds none of
  -- them -- eventBindings has no arm for it. Empty by decision rather than
  -- default: both ids are dead by the time the trigger resolves, and CR 400.7e
  -- would name the countered card in its owner's graveyard. A card that says
  -- "exile it instead" is the one that must bind `became` here.
  TriggerCondition.SpellOrAbilityCounters _ -> Set.empty
  -- CR 615.13's amount, guaranteed given a match: the event carries a Natural
  -- unconditionally, so unlike SelfLeavesTheBattlefield's `became` there is no
  -- shape of the event that withholds it.
  TriggerCondition.DamageToPlayerPrevented _ -> Set.singleton Binding.eventAmount
  -- CR 119.9's amount, guaranteed given a match for the prevention arm's reason:
  -- GameEvent.LifeGained carries a Natural unconditionally, so no shape of the
  -- event withholds it. Sanguine Bond's "that much" is what reads it.
  --
  -- The gaining PLAYER gets no slot: under the one relation a card in the pool
  -- uses, that player is CR 109.5's "you", whom Binding.setYou already names, so a
  -- slot would be a second name for one player. PlayerDiscards binds one because
  -- Megrim's "that player" is somebody else's; a card watching an OPPONENT gain
  -- life is what would want the same here (#826).
  TriggerCondition.PlayerGainsLife _ -> Set.singleton Binding.eventAmount
  -- The loss condition's amount, guaranteed for the same reason:
  -- GameEvent.LifeLost carries a Natural unconditionally. Exquisite Blood's "you
  -- gain that much life" is what reads it.
  --
  -- And the LOSING player, which is what separates this from the gain arm above:
  -- under Exquisite Blood's Opponent relation that player is NOT the "you"
  -- Binding.setYou names, and Mindcrank's "that player mills that many cards"
  -- reads them. Guaranteed for the same reason the amount is -- GameEvent.LifeLost
  -- carries a PlayerId unconditionally, so the promise holds under either
  -- relation.
  TriggerCondition.PlayerLosesLife _ -> Set.fromList [Binding.eventAmount, Binding.triggerPlayer]
  -- CR 714.2b names one object -- the bearer -- which CR 113.7a's source slot
  -- already names, so `became` would be a second name for it. The counts the
  -- event carries are the CONDITION's, not the payload's: no chapter ability in
  -- print says "that many", and eventBindings has no arm for this condition.
  TriggerCondition.SelfCountersReached _ _ -> Set.empty

-- Whether a damage recipient is a player (CR 120.1): a total discriminator over
-- Recipient, so the combat-damage-to-player trigger matcher stays non-partial.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToPlaneswalker _ -> False
  Recipient.ToObject _ -> False

-- CR 603.6a: every event is checked against every permanent currently on the
-- battlefield, not only the object the event names -- a step trigger belongs to a
-- permanent with nothing to do with the event.
--
-- The candidate set is the same for every event, so it is projected ONCE via
-- projectAll rather than per (event, permanent) pair: Projection.project reruns
-- the whole-board `gather` fold on every call, which made this scan quadratic in
-- board size.
--
-- The battlefield is not the only scanned zone -- every GRAVEYARD is scanned for
-- the abilities CR 113.6k puts there. The hand, exile, the stack and the command
-- zone are unscanned (#348).
--
-- CR 603.10's FIRST sentence is a per-EVENT question, and the live battlefield set
-- answers a per-BOUNDARY one: the scan runs once at CR 117.5, after CR 704.5's
-- state-based actions, so every permanent that left anywhere inside the batch is
-- missing even for events it was plainly there for.
--
-- So each event contributes the permanents that left the battlefield LATER in the
-- same batch, read from CR 608.2h last known information -- `bystanders` below.
-- Four things make that exact rather than approximate:
--
--   * The same reading, one event later. A permanent removed by a later event
--     existed immediately after this one, which is what the rule asks. It reaches
--     the event's own newcomer for free: a creature entering as a 0/0 and buried
--     by CR 704.5f leaves at a later index than its entry.
--   * No double fire, structurally: `lastKnown` is written by the zone change that
--     DELETES an id, and CR 400.7 mints a fresh id per move, so no id is in both.
--   * The right snapshot: `lastKnown` holds the permanent as it was on the
--     battlefield, continuous effects applied, which CR 603.10 demands.
--   * A canonical place in the order: candidates are a Map keyed by ObjectId and
--     traversed ascending, so extras sort in rather than being appended.
--
-- CR 603.10a is the other half of that rule, the exception rather than the normal
-- case: a DEPARTURE event also contributes the permanent it took off the
-- battlefield, and for that one the last-known reading is what the rule asks for
-- rather than a repair for a late boundary.
--
-- Not reconstructed: a permanent that ENTERED later in the batch and left before
-- the boundary is still offered to the batch's earlier events (#441); nor is
-- `bystanders`' mirror, so a look-back condition borne by a permanent that dies
-- alongside the one it watches answers by object id (#615).
--
-- Events outer, permanents inner (ascending by id): the deterministic canonical
-- order the CR 603.3b ordering prompt indexes into.
eventTriggers :: [GameEvent] -> GameState -> [PendingTrigger]
eventTriggers events gs =
  let projected = Projection.projectAll gs
      -- The control-grant list is the same for every permanent in this scan
      -- (same reason projected is computed once): Projection.controllerOf
      -- would otherwise rebuild it, and re-run its liveGiven fixpoint, once
      -- per battlefield object.
      grants = Projection.controlGrants gs
      -- CR 702.70a: a keyword can BE a triggered ability, so a permanent's
      -- abilities are its printed-and-granted ones plus the ones rule 702 mints
      -- from its keywords. Derived from POST-LAYER counts, so Humility takes them
      -- away and a layer-6 grant adds them without special-casing. Shared by both
      -- candidate sources, so a live and a last-known permanent read alike.
      abilitiesOf pc = PC.triggeredAbilities pc <> Keyword.triggeredAbilitiesOf (PC.keywords pc)
      -- CR 113.6m's "functions ONLY in that zone", asked of a permanent read AS
      -- BEING ON THE BATTLEFIELD: a Squee, Goblin Nabob standing there does not
      -- see its own upkeep, because the ability that watches for it functions in
      -- the graveyard. The mirror of the filter
      -- Pawl.Engine.Activate.abilitiesForGiven puts on its battlefield arm.
      --
      -- Applied to BOTH readings that say "this permanent was on the
      -- battlefield": the live `onBattlefield` set, and the `bystanders` suffix
      -- union, which is CR 603.10's first sentence recovering a permanent that
      -- WAS on the battlefield at this event and has left by the CR 117.5
      -- boundary. Last known information (CR 608.2h) is how that permanent is
      -- read, not a statement about which zone it is being read IN, so the zone
      -- CR 113.6m compares against is the battlefield either way.
      --
      -- NOT applied to `leftBattlefield` -- CR 603.10a's look-back at the
      -- permanent THIS event removed. That is the same shape asking a different
      -- question, and CR 113.6m answers it differently: the rule's own "unless
      -- its trigger condition ... specifies that the object is put into that
      -- zone" exempts the dies triggers that arm serves, and that clause is not
      -- implemented (#819), so filtering there would read the rule's first half
      -- without its second.
      battlefieldAbilitiesOf pc = filter (functionsIn Zone.Battlefield) (abilitiesOf pc)
      onBattlefield =
        Map.fromList
          ( Maybe.mapMaybe
              ( \oid -> case Map.lookup oid projected of
                  -- Unreachable: projected (Projection.projectAll gs) is keyed on
                  -- the same GameState.battlefield set this list walks, so every
                  -- oid drawn from that set has an entry.
                  Nothing -> Nothing
                  -- CR 603.3a: a triggered ability is controlled by whoever
                  -- controlled its source AT THE TIME IT TRIGGERED, not at this
                  -- scan -- so recordEvent's batch-open sample is consulted first
                  -- and the live projection answers only for an id it does not
                  -- name. The two disagree for exactly one board shape, which is
                  -- what this exists for: a layer-2 control effect in force at the
                  -- event and gone by the scan (CR 514.2 is where the pool reaches
                  -- it).
                  --
                  -- Falling back LIVE rather than to the CR 110.2 default is not a
                  -- shortcut: an id the sample does not name had no layer-2
                  -- controller when the batch opened, so for a permanent already
                  -- there this is its default, and for one that ARRIVED mid-batch
                  -- it is the controller it arrived with -- which is what CR
                  -- 603.10's first sentence asks for.
                  Just pc ->
                    fmap
                      (\ctrl -> (oid, (ctrl, battlefieldAbilitiesOf pc)))
                      ( case Map.lookup oid (GameState.controlWhenTriggered gs) of
                          Just who -> Just who
                          Nothing -> Projection.controllerOfGiven grants Set.empty oid gs
                      )
              )
              (Set.toAscList (GameState.battlefield gs))
          )
      -- The permanent this event took OFF the battlefield, read from
      -- CR 608.2h last known information -- both the abilities and the objects'
      -- appearance immediately prior to the event, which is what CR 603.10 says
      -- looking back means. Both live in the single `lastKnown` record, written
      -- from the pre-move state by the zone change that deleted the id, so the
      -- ability is read as it existed on the battlefield and CR 603.3a's
      -- controller is who controlled the permanent as it left.
      --
      -- Possible only because Moved names BOTH ids: `object` is the CR 400.7
      -- incarnation in the destination zone, which `lastKnown` knows nothing
      -- about, while `departed` is the key it files under.
      --
      -- Keyed by that departing id, which by construction no longer exists, so
      -- this source collides with no other -- one entry per id means one pass of
      -- `forOne` without leaning on Map.unions' bias.
      --
      -- EVERY battlefield departure contributes, not only the deaths: which
      -- destinations a condition accepts is the CONDITION's business, and keeping
      -- that out of the candidate source is what let CR 603.6c's wider "leaves the
      -- battlefield" arrive as a matcher arm alone.
      --
      -- The `to /= Battlefield` guard is CR 603.6c's own word "another": the
      -- pseudo-move recordTokenEntry emits for a new token is not a departure.
      --
      -- The departing id is what the placed trigger carries as its SOURCE (CR
      -- 113.7a). CR 603.6c's arriving incarnation is a SECOND slot rather than a
      -- different value in this one -- eventBindings binds it under `became`.
      --
      -- Empty for a permanent that ceased without a zone change running over it,
      -- which files no last known information. That hole is `bystanders`' too.
      --
      -- Parameterized by which of the departed permanent's abilities to offer,
      -- because the two callers below want different sets out of one recovery:
      -- `leftBattlefield` is CR 603.10a and takes them all, `bystanders` is CR
      -- 603.10's first sentence and takes only the ones CR 113.6m leaves
      -- functioning on the battlefield.
      departedFrom pick event = case event of
        GameEvent.Moved zc _
          | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc /= Zone.Battlefield ->
              case Map.lookup (ZoneChange.departed zc) (GameState.lastKnown gs) of
                Nothing -> Map.empty
                Just lk ->
                  Map.singleton
                    (ZoneChange.departed zc)
                    (LastKnown.controller lk, pick (LastKnown.characteristics lk))
        GameEvent.Moved _ _ -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented _ _ -> Map.empty
        GameEvent.StepBegan _ _ -> Map.empty
        GameEvent.SpellCast _ -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Revealed _ _ -> Map.empty
        GameEvent.AttackerDeclared _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost _ _ -> Map.empty
        GameEvent.LifeGained _ _ -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
      -- CR 603.10a's look-back at the permanent this event removed: every
      -- ability it had, unfiltered, for the reason `battlefieldAbilitiesOf`
      -- above gives.
      leftBattlefield = departedFrom abilitiesOf
      -- CR 603.10's first sentence, per EVENT: the permanents still on the
      -- battlefield when each event happened that have left by the CR 117.5
      -- boundary. Entry i is the union of `leftBattlefield` over the events AFTER
      -- i -- strictly later, so an event's own departure is not in its own entry.
      -- That is not an optimisation: the departing permanent does not exist
      -- immediately after the event that removed it, and reaches that one event
      -- only through CR 603.10a's look-back, which `leftBattlefield` supplies
      -- separately.
      --
      -- A right scan rather than a lookup table: scanr shares each suffix's union
      -- with the one before, so the batch costs one pass. Building the union per
      -- event would be quadratic, and a combat damage step's batch is a whole
      -- board's worth of deaths. `drop 1` is the alignment, shifting scanr's
      -- "from i onward" to "from i+1 onward".
      --
      -- The controller and abilities are the ones the permanent had as it LEFT --
      -- one moment after the event that triggered them, not at it (#603). Nothing
      -- in this pool moves control or grants an ability in that window.
      --
      -- CR 113.6m applies here and not to `leftBattlefield`: this permanent WAS
      -- on the battlefield when the event happened, so one of its abilities that
      -- functions only in a graveyard was no more watching then than it is now.
      -- The proving case is Squee, Goblin Nabob leaving the battlefield after an
      -- upkeep began in the same batch, in Pawl.TriggerSpec's
      -- `bystanderZoneSpec`.
      bystanders = drop 1 (List.scanr (\event acc -> Map.union (departedFrom battlefieldAbilitiesOf event) acc) Map.empty events)
      -- CR 702.29c: the card that was just cycled, wherever it landed. The
      -- candidate source that is neither on the battlefield nor a permanent that
      -- left it -- which is exactly what that rule asks for:
      -- "these abilities trigger from whatever zone the card winds up in after
      -- it's cycled", the graveyard for every printing today.
      --
      -- Abilities come from the PRINTED card rather than a projection, pawl's
      -- projection reaching the battlefield only (#160). Rule 702's minted
      -- abilities are not consulted either -- none functions from a graveyard.
      --
      -- The controller is the OWNER, CR 113.8's second clause: a card in a
      -- graveyard has no controller (CR 108.4).
      --
      -- Scoped to the CYCLING cause, not every discard, rule 702.29c speaking
      -- about cycling specifically. An ordinary discard's card reaches the
      -- graveyard too and is offered by `inGraveyards` under CR 113.6k.
      cycledCard event = case event of
        GameEvent.Discarded _ oid DiscardCause.ToPayCyclingCost -> case Game.lookupObject oid gs of
          Nothing -> Map.empty
          Just obj -> case Game.faceOf oid gs of
            Nothing -> Map.empty
            Just face -> Map.singleton oid (Object.owner obj, Face.triggeredAbilities face)
        GameEvent.Discarded _ _ DiscardCause.Ordinary -> Map.empty
        GameEvent.Moved _ _ -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented _ _ -> Map.empty
        GameEvent.StepBegan _ _ -> Map.empty
        GameEvent.SpellCast _ -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        -- A reveal names no object at all, so there is nothing to hang an ability
        -- on; a card triggering on a reveal would need a condition first (#322).
        GameEvent.Revealed _ _ -> Map.empty
        GameEvent.AttackerDeclared _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost _ _ -> Map.empty
        GameEvent.LifeGained _ _ -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
      -- CR 113.6k and CR 113.6m: every card in every graveyard carrying at least
      -- one ability those rules put there. The one source that widens the SCANNED
      -- ZONE rather than recovering an object an event names, which is why it is
      -- computed once outside the event loop, as `onBattlefield` is.
      --
      -- Narrow by construction, which keeps a large graveyard cheap: membership is
      -- decided by `functionsIn` -- a total case over a closed condition type and a
      -- walk of the ability's own effects, no projection and no board walk. Cards
      -- contributing nothing are dropped rather than carried as empty entries.
      --
      -- Abilities come from the PRINTED card and the controller is the OWNER, for
      -- `cycledCard`'s reasons.
      --
      -- CR 603.10a does not apply to what this serves -- a card ENTERING a
      -- graveyard is on none of its look-back list -- so CR 603.10's normal first
      -- sentence governs and this live read is the game as it stands. A card that
      -- arrives in a graveyard and is gone again before the boundary is lost
      -- (#349).
      graveyardCandidate oid = case (Game.lookupObject oid gs, Game.faceOf oid gs) of
        (Just obj, Just face) ->
          case filter (functionsIn Zone.Graveyard) (Face.triggeredAbilities face) of
            [] -> Nothing
            abilities -> Just (oid, (Object.owner obj, abilities))
        _ -> Nothing
      inGraveyards =
        Map.fromList
          (concatMap (Maybe.mapMaybe graveyardCandidate . Foldable.toList) (Map.elems (GameState.graveyard gs)))
      forOne event (oid, (ctrl, abilities)) =
        let fires ab = matchesTrigger gs oid ctrl (TriggeredAbility.condition ab) event
            pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab (eventBindings (TriggeredAbility.condition ab) event)
         in fmap pend (filter fires abilities)
      -- Map.unions is left-biased, so a live battlefield reading wins over a
      -- last-known one, a cycled card and a graveyard reading. That rules out a
      -- double fire: one entry per id means one pass of `forOne` per id.
      --
      -- The first four sets are disjoint by construction, and the bias is belt and
      -- braces. `inGraveyards` genuinely overlaps `cycledCard` on purpose -- a card
      -- cycled into a graveyard is honestly a member of both -- and the winner
      -- offers that card's printed abilities unfiltered, a superset either way.
      candidates event gone = Map.toAscList (Map.unions [onBattlefield, leftBattlefield event, gone, cycledCard event, inGraveyards])
      scanOne (event, gone) = concatMap (forOne event) (candidates event gone)
   in concatMap scanOne (zip events bystanders)

-- CR 113.6m, read off a TRIGGERED ability: "an ability whose cost or effect
-- specifies that it moves the object it's on out of a particular zone functions
-- only in that zone". The rule says "an ability" -- Pawl.Engine.Activate's
-- namesake is the same sentence read off an activated one, and this is the
-- triggered half.
--
-- The EFFECT half alone. CR 602.1 gives an activated ability "a cost and an
-- effect"; CR 603.1 gives a triggered one "a trigger condition and an effect",
-- and no cost at all -- so Pawl.Engine.Cost, the other half of
-- Activate.zoneFunctionedFrom, has nothing to be asked here.
--
-- ALL MODES, in printed order, for Activate.zoneFunctionedFrom's reason: CR
-- 700.2 makes a modal ability's modes alternatives, so a zone stated by any of
-- them is a zone the ability can move its object out of.
--
-- Not a case on an effect's identity: Pawl.Engine.EffectZone answers the one
-- question, and this folds its answer.
--
-- Not implemented: CR 113.6m's "unless" clause, its Aura half, and its
-- delayed-triggered-ability sentence (#819).
zoneFunctionedFrom :: TriggeredAbility.TriggeredAbility Card -> Maybe Zone
zoneFunctionedFrom ability =
  Maybe.listToMaybe
    (Maybe.mapMaybe EffectZone.zoneFunctionedFrom (Modal.allEffects (TriggeredAbility.modal ability)))

-- CR 113.6, asked of one zone and one triggered ability: does it function from
-- there? Three sentences of that rule in precedence order.
--
-- CR 113.6m first, because it is the only one that can name a zone the condition
-- knows nothing about -- Squee, Goblin Nabob's "at the beginning of your upkeep"
-- triggers perfectly well from the battlefield, and only "return this card from
-- your graveyard" says otherwise. "Functions ONLY in that zone" is what makes
-- this an override rather than an addition.
--
-- CR 113.6k next, for a condition that cannot trigger from the battlefield at
-- all -- Narcomoeba's "put into your graveyard from your library".
--
-- CR 113.6's own default last: "abilities of all other objects usually function
-- only while that object is on the battlefield".
--
-- The two rules cannot presently disagree: no printing states an origin zone on
-- an ability whose condition already answers CR 113.6k, and if one did they
-- would both say graveyard. The order is written down so a future card meets a
-- decision rather than an accident.
functionsIn :: Zone -> TriggeredAbility.TriggeredAbility Card -> Bool
functionsIn zone ability = case zoneFunctionedFrom ability of
  Just named -> zone == named
  Nothing
    | functionsInGraveyard (TriggeredAbility.condition ability) -> zone == Zone.Graveyard
    | otherwise -> zone == Zone.Battlefield

-- CR 113.6k: a trigger condition that can't trigger from the battlefield functions
-- in all zones it can trigger from. Answered for one zone, the graveyard, the only
-- non-battlefield zone eventTriggers scans (#348).
--
-- One of the three sentences `functionsIn` above reads, and the only one that
-- looks at the CONDITION -- so an ability whose effect already names its zone
-- never reaches this, and no arm below has to think about CR 113.6m.
--
-- A CLASSIFICATION of a trigger condition rather than an effect: it asks which
-- zone a rule 603 condition functions in and never reaches the ability's payload.
--
-- The default is False, which is CR 113.6's own: abilities usually function only
-- from the battlefield. Every False arm below is that sentence, not an omission.
functionsInGraveyard :: TriggerCondition -> Bool
functionsInGraveyard cond = case cond of
  -- CR 603.6a is an enters-the-battlefield ability; its bearer is on the
  -- battlefield when it fires.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StepBegins _ _ -> False
  -- CR 603.8's state triggers are not event triggers, so this scan is not their
  -- reader in any zone; stateTriggers below gathers them from the battlefield.
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  -- CR 302.6 / 508.1a: only a permanent on the battlefield can be declared as an
  -- attacker, so CR 113.6k never reaches this.
  TriggerCondition.SelfAttacks _ -> False
  -- CR 702.29c: a cycling ability triggers from whatever zone the card winds up
  -- in, the graveyard for every printing in this pool, and a cycled card cannot be
  -- on the battlefield. eventTriggers' `cycledCard` is what actually serves it.
  TriggerCondition.SelfCycled -> True
  -- CR 113.6's default: the bearer watches from the battlefield, so a card in a
  -- graveyard does not see an opponent discard.
  TriggerCondition.PlayerDiscards _ -> False
  -- The condition this predicate exists for: a card cannot be put into a graveyard
  -- from a library while on the battlefield, so this can never trigger from there
  -- and the graveyard it lands in is the one zone it can.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> True
  -- True for a NEARER reason than the library condition's, and the one that
  -- matters: this condition CAN follow a battlefield-to-graveyard move, but CR
  -- 603.6c's last sentence denies it the leaves-the-battlefield look-back, so
  -- the bearer is never the permanent on the battlefield -- it is always the card
  -- that arrived in the graveyard. Nothing it can trigger from is the
  -- battlefield, so CR 113.6k puts it in every zone it can, and the graveyard is
  -- where the scan meets it whatever zone the card came from.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> True
  -- The mirror image, False for a reason rather than by default: a dies trigger CAN
  -- trigger from the battlefield, which CR 603.10a's look-back is what makes true
  -- of a permanent that is a graveyard card by the time the scan runs.
  -- `leftBattlefield` serves it from CR 608.2h; `inGraveyards` must NOT, or the
  -- ability would be read off the printed text and credited to its owner.
  TriggerCondition.SelfDies -> False
  -- The same answer one step further: this condition's bearer is not the permanent
  -- that died at all, and watches from the battlefield.
  TriggerCondition.PermanentDies _ -> False
  -- The same CR 603.10a answer as both dies conditions, and harder to miss here:
  -- the destination may be a hand or library, and an ability found in a GRAVEYARD
  -- could not be what fired for a permanent that went somewhere else.
  TriggerCondition.SelfLeavesTheBattlefield -> False
  -- CR 113.6's default again: the bearer watches from the battlefield.
  TriggerCondition.SpellOrAbilityCounters _ -> False
  -- The same default: Selfless Squire watches damage addressed to its controller from
  -- the battlefield, and a card in a graveyard sees nothing prevented.
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- CR 113.6's default once more: Ajani's Pridemate has to be on the battlefield
  -- to receive the counter its own ability puts on it.
  TriggerCondition.PlayerGainsLife _ -> False
  -- And once more: Exquisite Blood is an enchantment, and CR 113.6 leaves its
  -- ability functioning only where the permanent is.
  TriggerCondition.PlayerLosesLife _ -> False
  -- CR 122.1's first sentence puts counters on OBJECTS, and CR 714.3 keeps a
  -- Saga's lore counters on the permanent -- so CR 113.6's default holds and a
  -- chapter ability functions from the battlefield alone.
  TriggerCondition.SelfCountersReached _ _ -> False

-- CR 603.2b / 109.5: does this condition restrict the turn its event may occur
-- on to the ABILITY'S CONTROLLER's turn? True for "at the beginning of YOUR
-- <step>" and for nothing else.
--
-- A CLASSIFICATION of a trigger condition, the third of the same kind as
-- eventBindingSlots and functionsInGraveyard above.
--
-- Its customer is the card lint. Onset.FromYourNextTurn delivers both halves of
-- "your next turn" on its own, so this no longer guards the firing -- it guards
-- the DATA: a card arming that onset over an EachTurn condition would have its
-- printed "each" silently narrowed by the window.
--
-- Exhaustive with no wildcard, for eventBindingSlots' reason: a new condition must
-- force a decision rather than defaulting to False.
controllerTurnScoped :: TriggerCondition -> Bool
controllerTurnScoped cond = case cond of
  -- The one arm carrying a TurnScope, and the whole content of this
  -- classification (CR 603.3a, CR 109.5).
  TriggerCondition.StepBegins _ TurnScope.ControllersTurn -> True
  -- "Each <step>" admits every player's turn, the pairing the lint rejects.
  TriggerCondition.StepBegins _ TurnScope.EachTurn -> False
  -- CR 702.179d's "during YOUR turn" is the same restriction StepBegins spells
  -- with a TurnScope, written into the condition itself because rule 702.179d
  -- states it there. No card bears this condition, so the lint this feeds cannot
  -- reach it; answering False anyway would make the classification wrong for the
  -- sake of an unreachable case.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> True
  -- None of the rest is turn-scoped: each names an event that can happen on
  -- anybody's turn.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.PlayerDiscards _ -> False
  -- CR 508.1a makes this the ACTIVE player's turn, which is not the same thing:
  -- CR 109.5's "you" is the ability's controller, and a stolen creature attacks on
  -- its thief's turn.
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies _ -> False
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False
  -- Damage can be prevented on anybody's turn.
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- Life can be gained on anybody's turn. CR 702.179d's loss condition above says
  -- "during your turn" and this one does not, which is the two rules' own
  -- difference rather than an omission here.
  TriggerCondition.PlayerGainsLife _ -> False
  -- And life can be LOST on anybody's turn. This is the arm where CR 702.179d's
  -- condition is closest to being duplicated and is not: that one reads the very
  -- same GameEvent.LifeLost but only during its controller's turn, which is the
  -- printed speed rule rather than anything a card's "whenever an opponent loses
  -- life" says.
  TriggerCondition.PlayerLosesLife _ -> False
  -- CR 122.6 puts counters on at any time, and a Saga can receive one on an
  -- opponent's turn: CR 714.3a's entry replacement fires whenever the Saga enters,
  -- which a flash effect or an opponent's Sneak Attack could make happen. Only CR
  -- 714.3c's turn-based action is the controller's own turn, and that is the
  -- action's restriction rather than this condition's.
  TriggerCondition.SelfCountersReached _ _ -> False

-- CR 603.8: state triggers. For every battlefield permanent, each StateIs ability
-- it bears whose condition is currently TRUE and which has no instance of ITSELF
-- already on the stack -- counted, so an object carrying the same state-triggered
-- ability twice arms both (CR 603.2 makes each of them an ability in its own
-- right).
--
-- Armedness is DERIVED, never stored: CR 603.8's three outcomes are all "no longer
-- on the stack", so an instance sitting there is the whole suppression rule and
-- there is no bookkeeping field to leak. No triggered-but-not-yet-placed window
-- either, Engine.placePendingTriggers acting within the same settle step.
--
-- A trigger whose modes are all unfillable would be removed from the stack (CR
-- 603.3c) and re-trigger on the next settle pass while its condition held, which
-- would not terminate. No card in the pool can do that, and the first that could
-- is the one that must revisit this.
stateTriggers :: GameState -> [PendingTrigger]
stateTriggers gs
  -- A stack id whose object can't be found: fail CLOSED, not open. This runs
  -- inside the settleForPriority fixpoint, so a lost suppression loops forever
  -- -- a hang, not a wrong answer -- while failing closed costs at most one
  -- settle pass. Unreachable: Game.cease removes the stack entry and its object
  -- together. Hoisted to the whole function because that is what the per-ability
  -- check it replaces amounted to: one unreadable stack entry suppressed every
  -- ability of every source.
  | any (\sid -> Maybe.isNothing (Game.lookupObject sid gs)) (GameState.stack gs) = []
  | otherwise = concatMap forOne (Set.toAscList (GameState.battlefield gs))
  where
    -- The same hoist eventTriggers' `grants` binding makes.
    grants = Projection.controlGrants gs
    -- CR 603.8's suppression, COUNTED rather than tested. Scoped to (source,
    -- ability), so two permanents bearing the identical triggered ability
    -- suppress independently -- one instance per source, not one for the whole
    -- board.
    --
    -- A count rather than an "is there one?" because CR 603.2 makes each ability
    -- its own ability: one object may carry two identical state-triggered
    -- abilities, and CR 603.8 holds each back only until THAT ability's own
    -- instance leaves the stack. Object.source cannot tell those two instances
    -- apart -- they are equal values -- but it does not have to. Which of N
    -- identical abilities a given instance came from is unobservable, so N live
    -- copies minus K instances already on the stack is the exact answer: it
    -- reproduces the single-ability behavior at N = 1, and lets one of a twin
    -- pair re-arm while the other's instance still sits there
    -- (TriggerSpec, "one instance leaving re-arms ITS ability").
    instancesOnStack srcId ab =
      let isInstance sid = fmap Object.source (Game.lookupObject sid gs) == Just (Source.OfTrigger srcId ab)
       in length (filter isInstance (GameState.stack gs))
    forOne oid = case Projection.controllerOfGiven grants Set.empty oid gs of
      Nothing -> []
      -- CR 603.3a / 109.5: the ability's controller is its source's, and that is
      -- what "you" in the condition means. Outside the layer fold, so the ViewOf
      -- is the FULL projection rather than the layer-bounded one.
      Just ctrl ->
        let live ab = case TriggeredAbility.condition ab of
              TriggerCondition.StateIs cond ->
                Condition.holds (Projection.fullView gs) (Filter.MkContext (Just ctrl) (Just oid)) gs oid cond
              TriggerCondition.SelfEnters -> False
              -- CR 603.6a is an EVENT trigger, matched against the log; nothing
              -- about it is a CR 603.8 state.
              TriggerCondition.PermanentEnters _ -> False
              TriggerCondition.StepBegins _ _ -> False
              TriggerCondition.SelfDealsCombatDamageToPlayer -> False
              TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
              TriggerCondition.OpponentLostLifeDuringYourTurn -> False
              TriggerCondition.SelfAttacks _ -> False
              TriggerCondition.SelfCycled -> False
              TriggerCondition.PlayerDiscards _ -> False
              TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
              TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
              TriggerCondition.SelfDies -> False
              TriggerCondition.PermanentDies _ -> False
              TriggerCondition.SelfLeavesTheBattlefield -> False
              TriggerCondition.SpellOrAbilityCounters _ -> False
              TriggerCondition.DamageToPlayerPrevented _ -> False
              TriggerCondition.PlayerGainsLife _ -> False
              TriggerCondition.PlayerLosesLife _ -> False
              -- CR 714.2b is an EVENT trigger too: it fires on the moment counters
              -- are PUT ON, not on the count standing at or above N -- which is
              -- exactly the difference CR 603.8 draws, and the reason a Saga does
              -- not re-run its final chapter for as long as it sits there.
              TriggerCondition.SelfCountersReached _ _ -> False
            lives = filter live (Projection.triggeredAbilitiesOf oid gs)
            -- Each live copy against the copies of itself that came earlier in
            -- the list, which gives it a 1-based ordinal among its equals: the
            -- j-th copy is armed exactly when fewer than j instances of it are
            -- already on the stack. That is the N-minus-K subtraction
            -- instancesOnStack describes, written without ever needing an Ord on
            -- a triggered ability.
            armed (before, ab) = 1 + length (filter (ab ==) before) > instancesOnStack oid ab
            pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab Map.empty
         in fmap (pend . snd) (filter armed (zip (List.inits lives) lives))

-- CR 603.7: delayed abilities whose trigger event is among these events. An entry
-- that fires is REMOVED from the store (CR 603.7b) unless it carries a stated
-- duration, which is that rule's own exception -- one of Expiry's sweeps ends
-- those instead. The survivors are returned so the caller can store them back. CR
-- 603.7d-f: the controller travels with the entry, so a delayed ability resolves
-- under whoever controlled the spell that created it even once that spell's source
-- is gone.
--
-- `fires` matches its condition only against EVENTS, never live game state -- the
-- turn number `armed` reads is CR 603.7a's arming gate, which can only withhold a
-- match. So a stored entry whose condition is StateIs would never fire, and
-- without a stated duration would never leave the store. Not a live gap: no card
-- in this pool arms a delayed ability with a StateIs condition.
--
-- The surviving store is computed from the EVENT MATCH alone, before
-- gatherTriggers' CR 603.4 intervening-"if" filter runs -- so an entry whose
-- intervening "if" is false is removed here, spending CR 603.7b's one shot rather
-- than staying armed for the next occurrence (#48). That reaches only an entry
-- with no stated duration; one with a duration is not spent by firing at all.
delayedPending :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
delayedPending events gs =
  let -- CR 603.7a's floor is the watermark's job, and is all an ordinary entry
      -- needs. This is the card's OWN further restriction: an ability printed "on
      -- your next turn" fires on that one turn and no other, whatever its
      -- condition matches. Read against the LIVE turn number, so an entry with no
      -- onset is untouched.
      armed entry = case DelayedTrigger.window entry of
        TurnWindow.AnyTurn -> True
        -- The named turn has not begun, so no occurrence counts -- including one
        -- in the turn that armed the ability, which is why the onset exists.
        TurnWindow.ControllersNextTurn -> False
        -- EQUALITY, not a floor: CR 603.7a is a claim about ONE named turn, so the
        -- window has an upper end and not merely a lower one.
        TurnWindow.OnTurn n -> n == GameState.turnNumber gs
      fires entry =
        let cond = TriggeredAbility.condition (DelayedTrigger.ability entry)
         in armed entry && any (matchesTrigger gs (DelayedTrigger.source entry) (DelayedTrigger.controller entry) cond) events
      pend entry =
        PendingTrigger.MkPendingTrigger
          (TriggerSource.OfObject (DelayedTrigger.source entry))
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          (DelayedTrigger.bindings entry)
      store = GameState.delayedTriggers gs
      -- Firing spends the one shot only for an entry with no stated duration.
      spent entry = fires entry && Maybe.isNothing (DelayedTrigger.expiry entry)
   in (fmap pend (Foldable.toList (Seq.filter fires store)), Seq.filter (not . spent) store)

-- CR 603.7a: the printed Onset as the game first stores it. The delayed-trigger
-- twin of Expiry.arm, deliberately blind to the board -- unlike a duration, an
-- onset has nothing to bake in when the ability is created, "your next turn" being
-- a boundary that has not happened yet. settleOnsets supplies the number.
armOnset :: Onset -> TurnWindow
armOnset onset = case onset of
  Onset.Immediately -> TurnWindow.AnyTurn
  Onset.FromYourNextTurn -> TurnWindow.ControllersNextTurn

-- CR 603.7a: a turn has BEGUN, so settle every delayed entry waiting for one and
-- drop every entry whose turn is now over. Engine.beginTurnOf calls this once the
-- new turn's number and active player are in place, and only for a turn that
-- actually begins -- CR 614.10a read on the turn axis, so CR 800.4k's turn a
-- departed seat never begins is walked past without settling anything.
--
-- The turn handoff is the ONLY moment either transition can be made, which is what
-- makes this a boundary sweep rather than something derived on demand. Two of
-- them, in this order:
--
-- 1. WAITING -> THIS TURN, for an entry whose controller is the player whose turn
--    this is. The number is sampled here because nothing in GameState remembers
--    which player each earlier turn belonged to, so the question cannot be
--    answered later. Sampled once and thereafter only ever cleared.
--
-- 2. THIS TURN -> GONE, for an entry whose settled turn is behind us. Its trigger
--    event cannot occur again, so CR 603.7a has already decided the matter.
--    Dropping it is hygiene: an entry that can never fire and states no duration
--    would otherwise outlive the game.
--
-- Ordered settle-then-drop within one pass: an entry settled onto THIS turn
-- carries this turn's number, and the drop removes only numbers strictly behind.
--
-- CR 603.7b's one shot is untouched -- this ends entries by the CALENDAR, and
-- firing still ends them in delayedPending. An entry with a stated duration is
-- dropped here too, and rightly: a duration keeps an ability armed for its event's
-- next occurrence, not for a turn its printed text never named.
settleOnsets :: GameState -> GameState
settleOnsets gs =
  let settled entry = case DelayedTrigger.window entry of
        -- The turn that is beginning IS the one the printed phrase named exactly
        -- when it belongs to the entry's controller (CR 603.7d-f).
        TurnWindow.ControllersNextTurn
          | DelayedTrigger.controller entry == GameState.activePlayer gs ->
              entry {DelayedTrigger.window = TurnWindow.OnTurn (GameState.turnNumber gs)}
        -- Anyone else's turn, including an intervening opponent's: still waiting.
        TurnWindow.ControllersNextTurn -> entry
        TurnWindow.AnyTurn -> entry
        -- Already settled, and this is a later turn -- `live` is what ends it.
        TurnWindow.OnTurn _ -> entry
      live entry = case DelayedTrigger.window entry of
        TurnWindow.AnyTurn -> True
        TurnWindow.ControllersNextTurn -> True
        TurnWindow.OnTurn n -> n >= GameState.turnNumber gs
   in gs {GameState.delayedTriggers = Seq.filter live (fmap settled (GameState.delayedTriggers gs))}

-- Everything that has triggered and is not yet on the stack, from all three
-- sources, plus the delayed store as it stands afterwards. One function, so
-- Pawl.Engine.Engine never needs to know how many sources there are.
gatherTriggers :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
gatherTriggers events gs =
  let (fromDelayed, surviving) = delayedPending events gs
      all_ = eventTriggers events gs <> stateTriggers gs <> fromDelayed
   in (filter (interveningHolds gs) all_, surviving)

-- CR 603.4: the ability doesn't trigger at all when its intervening "if" is false
-- as the trigger event occurs. Checked at the gather rather than at placement,
-- because "doesn't trigger" must be indistinguishable from "no ability existed",
-- including to the CR 117.5 settle loop's re-run flag.
--
-- A SOURCELESS pending trigger never reaches this -- gatherTriggers is the only
-- caller and all three gatherers hang their triggers on an object, the inherent
-- ones being merged in afterwards by Pawl.Engine.Engine. The arm answers True
-- rather than failing because an inherent ability's own gatherer owns CR 603.4:
-- rule 725.2's pair has no intervening "if" at all, and CR 702.179d's does,
-- checked inside Pawl.Engine.Speed.inherentPending. A fourth gatherer must do the
-- same; there is no subject object to hand this function, so routing one here
-- would mean giving Condition.holds the ability object Pawl.Engine.Stack's CR
-- 608.2a re-check uses, which does not exist until placement.
--
-- CR 608.2h supplies the view rather than fullView, which for a look-back trigger
-- is the difference between reading the clause and reading nothing: CR 603.10a
-- makes the source the permanent as it was immediately before the event, whose id
-- CR 400.7 has since deleted -- and fullView describes a deleted id as an object
-- with no characteristics, quietly answering False to every clause. Stack's CR
-- 608.2a re-check reads the same way, and the two must agree or a trigger would be
-- placed and then removed for disagreeing with itself.
interveningHolds :: GameState -> PendingTrigger -> Bool
interveningHolds gs pending =
  case (TriggeredAbility.intervening (PendingTrigger.ability pending), PendingTrigger.source pending) of
    (Nothing, _) -> True
    (Just _, TriggerSource.Sourceless) -> True
    (Just cond, TriggerSource.OfObject oid) ->
      Condition.holds
        (Projection.viewWithLastKnown oid gs)
        (Filter.MkContext (Just (PendingTrigger.controller pending)) (Just oid))
        gs
        oid
        cond
