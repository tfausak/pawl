{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Replacement over damage prevention (CR 615) and the prevention
-- shields' referents (CR 609.7): step skipping, the shield cards from Mending
-- Hands to Test of Faith, and their text-changed forms. Split out of
-- Pawl.ReplacementSpec, which keeps the machinery.
module Pawl.PreventionSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- indistinguishable and correctly elided).
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

-- The single activated ability of a printing (Drudge Skeletons and Liquimetal
-- Coating each have exactly one). Total: the empty-ability fallback is
-- unreachable in this fixture.
-- Same shape as ActivateSpec.theAbility -- duplicated per this test suite's
-- existing convention of group-local helpers (ActivateSpec and ManaSpec
-- already duplicate singleModeAbility the same way) rather than centralizing
-- a helper this small in Support.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] 0 (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Activator.Controller Nothing Nothing Nothing

wasAskedToReplace :: [Response.Response] -> Bool
wasAskedToReplace responses =
  let isReplacement r = case r of
        Response.ChoseReplacement _ -> True
        _ -> False
   in any isReplacement responses

wasAskedForEntryOption :: [Response.Response] -> Bool
wasAskedForEntryOption responses =
  let isEntryOption r = case r of
        Response.ChoseEntryOption _ -> True
        _ -> False
   in any isEntryOption responses

-- alice controls one Forest plus `mine`; bob controls `theirs`; alice holds one
-- Battlegrowth ({G} instant: put a +1/+1 counter on target creature). Returns the
-- state, Battlegrowth's hand id, and the two id lists in the order given.
counterBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
counterBoard forest battlegrowth mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = S.addPermanent p pid g in (ids <> [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll S.alice mine (S.landsInPlay forest 1)
      (yours, gs2) = addAll S.bob theirs gs1
      (gs3, spellId) = S.handOne battlegrowth gs2
   in (gs3, spellId, ours, yours)

-- Aim every target slot at `victim`, and answer a CR 616.1 race by picking the
-- candidate whose SOURCE is `preferred` -- by id, so the assertion does not
-- depend on the engine's canonical candidate order.
raceAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
raceAnswer preferred victim p = case p of
  Prompt.ChooseReplacement _ _ entries -> maybe 0 Int.toNaturalSaturating (List.findIndex ((== preferred) . ReplacementEntry.source) entries)
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> S.identityAnswer p

-- Aim every target slot at one object. Recipient.ToObject, not ToCreature as
-- raceAnswer above uses: both slots this answers -- Liquimetal Coating's and
-- Skilled Animator's -- are Pool.Permanents, and a recipient tagged for the wrong
-- pool is not in the legal set at all.
aimObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- What each COUNTDOWN shield on the board has left (CR 615.7), read off the
-- floating rows themselves. An empty list is a shield spent to 0 and dropped,
-- which is why the count of rows would not say the same thing; a Fog-shaped or
-- counter-backed prevention does not appear here at all, having no amount on its
-- row to report.
shieldsLeft :: GameState.GameState -> [Natural.Natural]
shieldsLeft gs =
  let remaining re = case re of
        ReplacementEffect.DamageR (DamageR.MkDamageR _ (DamageRewrite.PreventNext n) _) -> Just n
        _ -> Nothing
   in Maybe.mapMaybe (remaining . ActiveReplacement.effect) (GameState.replacements gs)

-- The damage CR 615.5's rider dealt to `pid`, ONE ENTRY PER EVENT rather than
-- summed. What tells CR 615.13's one application apart from one application per
-- recipient: both throw the same total back, and only the number of events
-- differs.
riderHits :: PlayerId.PlayerId -> GameState.GameState -> [Natural.Natural]
riderHits pid gs =
  fmap DamageEvent.amount (filter (\de -> DamageEvent.target de == Recipient.ToPlayer pid) (S.damageEventsOf gs))

-- The CR 615.13 records a board holds, in the order Pawl.Engine.Damage wrote
-- them. One per applying instance, whatever the batch was addressed to.
preventionsRecorded :: GameState.GameState -> [DamagePrevented.DamagePrevented]
preventionsRecorded gs =
  let pick event = case event of
        GameEvent.DamagePrevented prevented -> Just prevented
        _ -> Nothing
   in Maybe.mapMaybe pick (S.eventsOf gs)

-- Was CR 615.7's batch-order question raised at all? The elision half of every
-- group below asserts the negative of this, so the boards that ask nothing are
-- told apart from the boards that ask.
wasAskedToOrderDamage :: [Response.Response] -> Bool
wasAskedToOrderDamage =
  let isOrder r = case r of
        Response.OrderedDamage _ -> True
        _ -> False
   in any isOrder

-- Was CR 615.7's batch-division question raised at all? The order question's
-- twin, for the countdowns counted in points rather than in whole events.
wasAskedToAllocateDamage :: [Response.Response] -> Bool
wasAskedToAllocateDamage =
  let isAllocation r = case r of
        Response.AllocatedDamage _ -> True
        _ -> False
   in any isAllocation

countersOn :: CounterKind.CounterKind Keyword.Keyword -> ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
countersOn kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

castAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castAndResolve answer gs spellId =
  S.runPure answer gs (S.cast S.alice spellId >> Stack.resolveTop)

-- Copy `wanted` when it is offered, decline otherwise.
copyOf :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copyOf wanted p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> if List.elem wanted legal then Just wanted else Nothing
  _ -> S.identityAnswer p

-- alice controls `n` untapped Islands in a main phase with priority, holding one
-- card of each printing in `hand`. Returns the state and the hand ids in order --
-- unlike S.handOne, which replaces the whole hand.
blueBoard :: Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
blueBoard island n hand =
  let base = S.landsInPlay island n
      addOne (ids, g) p = let (oid, g1) = S.addHandCard p S.alice g in (ids <> [oid], g1)
      (held, gs) = List.foldl' addOne ([], base) hand
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- The newest battlefield object whose printed card has this name.
newestNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

-- Leyline of the Void's redirect, as a floating replacement: any card headed for
-- an OPPONENT's graveyard is exiled instead. CR 400.3 makes that graveyard the
-- card's OWNER's, which is what Replacement.matchesZoneOwner tests.
leylineShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
leylineShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR (ZoneChangePattern.MkZoneChangePattern (Just Zone.Graveyard) ControllerRelation.Opponents (Filter.Type.And []) Nothing) Zone.Exile False False LibraryPosition.defaultValue False),
      ActiveReplacement.source = src,
      ActiveReplacement.controller = S.alice,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.Other,
      ActiveReplacement.condition = Nothing,
      ActiveReplacement.rider = Nothing,
      ActiveReplacement.slots = Map.empty
    }

-- Eon Hub {5} Artifact: "Players skip their upkeep steps."
--
-- CR 614.1b: "Effects that use the word 'skip' are replacement effects. These
-- replacement effects use the word 'skip' to indicate what events, steps,
-- phases, or turns will be replaced with nothing." So this is P5's carrier, not
-- a CR 613.11 rules-modifying continuous effect.
--
-- Sarcomancy is the discriminating observable. Its second ability is "at the
-- beginning of your upkeep, if there are no Zombies on the battlefield, this
-- enchantment deals 1 damage to you" (CR 603.2b), matched against the
-- GameEvent.StepBegan that Engine.runStep records as a step begins. A step
-- REPLACED WITH NOTHING records no such event, so the ability never triggers
-- (CR 614.6: "if an event is replaced, it never happens") -- as distinct from
-- triggering and resolving to nothing, which would still put an object on the
-- stack and still take the life. Each case below asserts on BOTH the event log
-- and the life total, so the two outcomes cannot be confused.
--
-- Sarcomancy is placed straight onto the battlefield, so its enters-trigger
-- never resolves and no Zombie token exists: CR 603.4's intervening "if" holds
-- and the upkeep ability really would fire.
stepSkipSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
stepSkipSpec s registry = Spec.describe s "Skip" $ do
  let untap = Phase.Beginning BeginningStep.Untap
      upkeep = Phase.Beginning BeginningStep.Upkeep
      drawStep = Phase.Beginning BeginningStep.DrawStep
      -- Alice's turn, positioned at her untap step with only the upkeep and draw
      -- steps left to schedule. Short on purpose: an empty schedule would hand
      -- the turn off and clear the event log these cases read, and the draw step
      -- is never entered, so nothing draws from the empty library.
      atUntap printings =
        let place g p = snd (S.addPermanent p S.alice g)
            placed = List.foldl' place (Setup.emptyGame S.bothPlayers) printings
         in placed
              { GameState.phase = untap,
                GameState.activePlayer = S.alice,
                GameState.remaining = Seq.fromList [upkeep, drawStep]
              }
      -- The untap step, then whatever the schedule says comes next.
      twoSteps = do
        Engine.runStep
        Engine.runStep
      runTwo gs = snd (Engine.runGamePure S.identityAnswer gs twoSteps)
      began step gs = List.elem (GameEvent.StepBegan (StepBegan.MkStepBegan step S.alice)) (S.eventsOf gs)
  -- Eon Hub is BOB's, and it is ALICE's upkeep being skipped: "players
  -- skip THEIR upkeep steps" is symmetric, so the effect is not scoped to
  -- its controller.
  Spec.it s "CR 614.1b Eon Hub replaces the upkeep step with nothing" $ do
    sarcomancy <- S.printingOf s registry "Sarcomancy"
    eonHub <- S.printingOf s registry "Eon Hub"
    let base = atUntap [sarcomancy]
        armed = snd (S.addPermanent eonHub S.bob base)
        after = runTwo armed
    Spec.assertBool s (began untap after) "the untap step still began"
    Spec.assertBool s (not (began upkeep after)) "the upkeep step never began"
    Spec.assertEqWith s "so nothing ever reached the stack" (GameState.stack after) []
    Spec.assertEqWith s "and alice took no damage" (S.lifeOf S.alice after) (Just 20)
    -- CR 500.11: "to skip a step, phase, or turn is to proceed past it as
    -- though it didn't exist" -- past it, not past the rest of the turn.
    Spec.assertEqWith s "the turn proceeded to the draw step" (GameState.phase after) drawStep
    Spec.assertEqWith s "having consumed exactly that one step" (GameState.remaining after) Seq.empty

-- Aim every target slot at one player. The player-side twin of `aimObject`
-- above; Fatigue's slot is Pool.Players, so a recipient tagged for any other
-- pool is not in its legal set at all.
aimPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer pid))) sets
  _ -> S.identityAnswer p

-- Fatigue {1}{U} Sorcery: "Target player skips their next draw step."
--
-- The three things Eon Hub above does not do, and this card does:
--
--   1. The skip is created by an EFFECT. Nothing on the battlefield carries it --
--      Fatigue is in a graveyard by the time the skip matters -- so it lives in
--      GameState.replacements, the floating store CR 614.3 describes as lasting
--      "until they're used up".
--   2. It is scoped to ONE player (PhasePattern.whosePhase), where Eon Hub's
--      "players skip their upkeep steps" is symmetric.
--   3. CR 614.10a: it is CONSUMED after one occurrence, and two of them
--      ACCUMULATE rather than coalescing -- "if two effects each cause a player
--      to skip their next occurrence, that player must skip the next two".
--
-- The observables are the same pair the Eon Hub cases use, read together so a
-- skipped step cannot be confused with a step that happened and drew nothing:
-- the CR 603.2b StepBegan record (CR 614.6, "if an event is replaced, it never
-- happens"), and alice's library, which a real draw step empties by one.
fatigueSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
fatigueSpec s registry = Spec.describe s "Fatigue" $ do
  let drawStep = Phase.Beginning BeginningStep.DrawStep
      -- alice's turn, positioned at her draw step with the precombat main phase
      -- scheduled after it. Not an empty schedule: that hands the turn off, which
      -- would clear the event log these cases count and move the active player.
      --
      -- CR 103.8a's first-turn skip is a TURN-BASED action (Engine.skipsDraw),
      -- not a replacement effect, and it would swallow the control case's draw;
      -- turn 2 puts it out of the way, which is also what makes this the SECOND
      -- turn's draw step -- exactly the "next" one Fatigue named.
      atDraw gs =
        gs
          { GameState.phase = drawStep,
            GameState.activePlayer = S.alice,
            GameState.remaining = Seq.singleton Phase.PrecombatMain,
            GameState.turnNumber = 2
          }
      -- One draw step. Applied repeatedly, each call is alice's NEXT draw step:
      -- the store under test is not turn-scoped (Expiry.Never, and no sweep ends
      -- it -- every Pawl.Engine.Expiry sweep keeps a Never), so what a real
      -- intervening turn would contribute is a longer log, not a different
      -- answer.
      runDraw gs = snd (Engine.runGamePure S.identityAnswer (atDraw gs) Engine.runStep)
      begun gs = length (filter (== GameEvent.StepBegan (StepBegan.MkStepBegan drawStep S.alice)) (S.eventsOf gs))
      libraryOf pid gs = length (Game.zoneMembers Zone.Library pid gs)
      -- alice: two Islands per Fatigue to cast, a stocked library to draw from,
      -- and `n` Fatigues in hand. Only alice's draw step is ever run below, so
      -- only her library needs stocking.
      board island piker fatigue n =
        let (base, held) = blueBoard island (2 * n) (replicate n fatigue)
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) base [1 .. (5 :: Int)]
         in (stocked, held)
  -- The control: the same board with the spell never cast. Without it
  -- alice's draw step begins and draws, which is what every case below is
  -- measured against.
  Spec.it s "CR 500.6 without a skip alice's draw step begins and draws" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    fatigue <- S.printingOf s registry "Fatigue"
    let (gs, _) = board island piker fatigue 1
        after = runDraw gs
    Spec.assertEqWith s "the draw step began" (begun after) 1
    Spec.assertEqWith s "and took a card off the library" (libraryOf S.alice after) 4

-- Run whole steps until `done` holds of the board, the game ends, or the bound
-- runs out. The bound is three turns' worth of steps, so a skip that dropped
-- more of the schedule than it should still terminates and fails an assertion
-- rather than hanging.
runUntil :: (GameState.GameState -> Bool) -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runUntil done answer gs0 =
  let go n g =
        if n <= (0 :: Int) || done g || Maybe.isJust (GameState.result g)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 40 gs0

-- Run whole steps until the turn hands off, leaving the board at the first step
-- of the next turn.
nextTurn :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
nextTurn answer gs = runUntil ((/= GameState.turnNumber gs) . GameState.turnNumber) answer gs

-- Cast whatever is offered, and otherwise pass. Read by the CR 614.1d case
-- below, where the one castable card in the game is the {B} creature whose
-- castability that case measures -- so "whatever is offered" is that card or
-- nothing at all.
castOrPassAnswer :: Prompt.Prompt r -> r
castOrPassAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast {} -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  _ -> S.identityAnswer p

-- castOrPassAnswer's sibling for the CR 614.1c pay-life-or-enter-tapped cases:
-- play the land, and answer its as-enters "you may pay N life" the given way.
-- Every other prompt falls through to S.playLandAnswer, so the two cases differ
-- in the OptionalDecision and in nothing else.
payLifeOnEntryAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
payLifeOnEntryAnswer decision p = case p of
  Prompt.ChoosePayLifeOnEntry {} -> decision
  _ -> S.playLandAnswer p

-- razorgrassBoard's sibling for Sea Gate, Reborn, parameterized by alice's life
-- total: the modal double-faced card and the {U} creature in hand, and NOTHING
-- else in the game. The life total is the ONLY axis, so a pair of boards built
-- here differ in it and in nothing else.
seaGateBoard :: Printing.Printing -> Printing.Printing -> Integer -> GameState.GameState
seaGateBoard seaGate warrior life =
  let (_, withGate) = S.addHandCard seaGate S.alice (Setup.emptyGame S.bothPlayers)
      (_, filled) = S.addHandCard warrior S.alice withGate
   in filled
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice,
          GameState.players = Map.adjust (\p -> p {Player.life = life}) S.alice (GameState.players filled)
        }

-- How many Tidal Warriors made it to the battlefield.
warriorOut :: GameState.GameState -> Int
warriorOut gs =
  let warriorName = CardName.MkCardName (Text.pack "Tidal Warrior")
   in length (filter (\o -> Projection.hasName warriorName o gs) (Set.toList (GameState.battlefield gs)))

-- payLifeOnEntryAnswer's sibling for the CR 614.1c REVEAL cases: play the land,
-- and answer its "you may reveal a Kithkin card from your hand" with this card.
-- PINNED, never a search of the offered list: an answerer that picked whatever
-- the engine offered would find a legal card again after a mutation widened the
-- offer, and the case would stay green while the engine's own filter was broken.
revealOnEntryAnswer :: Maybe ObjectId.ObjectId -> Prompt.Prompt r -> r
revealOnEntryAnswer shown p = case p of
  Prompt.ChooseRevealOnEntry {} -> shown
  _ -> S.playLandAnswer p

-- How many times the as-enters reveal was put to a player.
revealAsks :: [Response.Response] -> Int
revealAsks responses =
  let isReveal r = case r of
        Response.ChoseRevealOnEntry _ -> True
        _ -> False
   in length (filter isReveal responses)

-- alice's precombat main phase with the stack empty, Rustic Clachan and ONE {W}
-- creature in hand, and NOTHING else in the game: the land's "{T}: Add {W}" is
-- the only mana there will be, so the creature's fate reads the land's tap state
-- as Soul Warden's does for Razorgrass Ambush (data/scenarios/replacement). The
-- creature is the only axis a pair of boards from here differ on -- Mosquito
-- Guard and Benalish Hero are both {W} 1/1 Soldiers, and one is a Kithkin.
clachanBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
clachanBoard clachan creature =
  let (_, withLand) = S.addHandCard clachan S.alice (Setup.emptyGame S.bothPlayers)
      (creatureId, filled) = S.addHandCard creature S.alice withLand
   in ( creatureId,
        filled
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- How many permanents with this name made it to the battlefield.
namedOut :: String -> GameState.GameState -> Int
namedOut name gs =
  let cardName = CardName.MkCardName (Text.pack name)
   in length (filter (\o -> Projection.hasName cardName o gs) (Set.toList (GameState.battlefield gs)))

-- Whether a life loss of exactly this size, by this player, was RECORDED -- the
-- channel a card that watches for life loss reads, and the half of CR 119.4 a
-- bare subtraction from the life total would not satisfy.
lostLife :: PlayerId.PlayerId -> Natural.Natural -> GameState.GameState -> Bool
lostLife pid n gs = GameEvent.LifeLost (LifeChange.MkLifeChange pid n) `elem` S.eventsOf gs

-- CR 615.7's prevention shield, whose plainest producer in data/cards/ is Mending
-- Hands ({W} Instant: "Prevent the next 4 damage that would be dealt to any
-- target this turn") -- the same countdown shield as Healing Grace below, minus
-- CR 609.7a's chosen source.
--
-- Three properties, and they are the three halves of the rule: the shield is
-- spent in DAMAGE rather than in events ("such effects count only the amount of
-- damage; the number of events or sources dealing it doesn't matter"), it is
-- scoped to the recipient it shields, and where two simultaneous sources contend
-- for it the shielded side chooses which damage it prevents rather than the
-- engine.
--
-- The group's last case is not about CR 615.7 at all: this card is the pool's
-- floating replacement that grants nobody CONTROL, which is what makes it the
-- discriminating twin for CR 800.4a's second clause
-- (Departure.givesControlOnEntryTo). It says why it is here.
--
-- The DAMAGE BATCHES below are hand-built and the SPELL is not: casting Mending
-- Hands for real is what proves the card, and reaching a real combat-damage batch
-- of two attackers with different powers would mean driving a whole combat phase
-- to produce a fixture these assertions read straight off. The same split, and
-- the same reason, as the Fog case in the group above.
mendingHandsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mendingHandsSpec s registry = Spec.describe s "Mending Hands (CR 615.7)" $ do
  let -- One noncombat damage event, from `src`, at `n`.
      hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- Order a contested batch by preferring the event from `src`, by SOURCE id
      -- rather than by position, so the assertion does not depend on the order
      -- the batch was gathered in.
      -- Take 2 off every event the shield is offered. Pinned by shape rather
      -- than searched for, so a mutation cannot be repaired into an answer that
      -- happens to be legal.
      twoEach :: Prompt.Prompt r -> r
      twoEach p = case p of
        Prompt.AllocateDamage _ _ events _ -> fmap (const 2) events
        _ -> S.identityAnswer p
  -- CR 615.7's arithmetic, one event at a time: the shield takes what it can of
  -- each event and reduces by exactly that much. Three 3-damage events against a
  -- shield of 4 -- so the first is prevented whole, the second only partly, and
  -- the third not at all.
  Spec.it s "CR 615.7 the shield counts DAMAGE, not events: 4 covers one 3 and part of the next" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addPermanent pikerPrinting S.alice base
        (attacker, g2) = S.addPermanent pikerPrinting S.bob g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        strike g = S.runPure S.identityAnswer g (Damage.applyDamage [hit attacker (Recipient.ToCreature victim) 3])
        once = strike shielded
        twice = strike once
        thrice = strike twice
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
    -- CR 615.6: a fully prevented event never happens, so nothing is marked and
    -- nothing is recorded.
    Spec.assertEqWith s "the first 3 is prevented whole" (S.damageOf victim once) (Just 0)
    Spec.assertEqWith s "and no damage event happened at all" (amounts once) []
    Spec.assertEqWith s "the shield is still there, holding 1" (length (GameState.replacements once)) 1
    -- The partial case, which is what makes this a shield rather than a Fog: 1
    -- of the second 3 is prevented and the other 2 are dealt.
    Spec.assertEqWith s "1 of the second 3 is prevented, 2 are dealt" (S.damageOf victim twice) (Just 2)
    Spec.assertEqWith s "and the surviving event carries the reduced amount" (amounts twice) [2]
    -- CR 615.7: "once the shield has been reduced to 0, any remaining damage is
    -- dealt normally."
    Spec.assertEqWith s "the spent shield is gone" (GameState.replacements twice) []
    Spec.assertEqWith s "so the third 3 lands in full" (S.damageOf victim thrice) (Just 5)
  -- CR 615.7's "shielded permanent": a shield names ONE recipient, so a second
  -- creature is not covered by it and does not spend it. The discriminating twin
  -- of the case above -- a shield that ignored DamagePattern.whichRecipient,
  -- which is what every damage pattern in the pool did before this card, would
  -- prevent this damage and be spent doing it.
  Spec.it s "CR 615.7 a shield covers the recipient it names and no other" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (shieldedOne, g1) = S.addPermanent pikerPrinting S.alice base
        (bystander, g2) = S.addPermanent pikerPrinting S.alice g1
        (attacker, g3) = S.addPermanent pikerPrinting S.bob g2
        (g4, spellId) = S.handOne mendingHands g3
        shielded = castAndResolve (aimCreature shieldedOne) g4 spellId
        after = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit attacker (Recipient.ToCreature bystander) 3])
    Spec.assertEqWith s "the unshielded creature takes all of it" (S.damageOf bystander after) (Just 3)
    Spec.assertEqWith s "and the shield was not spent on damage it does not cover" (length (GameState.replacements after)) 1
  -- CR 615.7 by the POINT rather than by the event: "each 1 damage that would be
  -- dealt to the shielded permanent or player is prevented", so the shielded
  -- side may split the shield across the batch and not merely rank it. The same
  -- board as the case above, divided 2 and 2: both events survive, at 3 and at
  -- 1, which no ranking of a 5 and a 3 against a shield of 4 can produce -- the
  -- orders reach [1,3] and [4] and nothing else.
  Spec.it s "CR 615.7 bob splits the shield between the two events, which no order can do" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (big, g1) = S.addPermanent pikerPrinting S.alice base
        (small, g2) = S.addPermanent pikerPrinting S.alice g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimPlayer S.bob) g3 spellId
        batch = [hit big (Recipient.ToPlayer S.bob) 5, hit small (Recipient.ToPlayer S.bob) 3]
        halved = settleDamage twoEach shielded batch
    -- THE gameplay assertion: both events happened, each cut by 2.
    Spec.assertEqWith s "2 off each: a 3 and a 1 are dealt" (amounts halved) [3, 1]
    Spec.assertEqWith s "and the shield still prevented exactly 4" (S.lifeOf S.bob halved) (Just 16)
    Spec.assertEqWith s "and it is spent" (GameState.replacements halved) []
  -- The elision half, and the reason the prompt is gated rather than raised for
  -- every batch: a shield big enough to cover the whole batch prevents all of it
  -- in any order, so there is nothing to decide and nothing is asked.
  Spec.it s "CR 615.7 a shield that covers the whole batch asks nothing" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (big, g1) = S.addPermanent pikerPrinting S.alice base
        (small, g2) = S.addPermanent pikerPrinting S.alice g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimPlayer S.bob) g3 spellId
        batch = [hit big (Recipient.ToPlayer S.bob) 1, hit small (Recipient.ToPlayer S.bob) 2]
        after = S.runPure S.identityAnswer shielded (Damage.applyDamage batch)
    Spec.assertBool
      s
      (not (wasAskedToAllocateDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch))))
      "no AllocateDamage was raised: 4 covers 1 and 2 together"
    Spec.assertEqWith s "both events were prevented whole" (amounts after) []
    Spec.assertEqWith s "bob's life is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and 3 of the shield's 4 were spent, so 1 remains" (length (GameState.replacements after)) 1

-- Aim every target slot at `victim` and answer CR 609.7a's source choice with
-- `src`. FILTERED, not built: the id is taken from the offered set, so an answer
-- the prompt never offered cannot reach the engine -- and the group asserts on
-- the RECORDED response, so a `src` that was never offered fails the case rather
-- than quietly falling back.
aimAndChoose :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAndChoose victim src p = case p of
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> aimCreature victim p

-- Which sources CR 609.7a's prompt was answered with, in order. The proxy half
-- of the group below: it says a choice was MADE, where the damage assertions say
-- the shield watches what was chosen.
chosenSourcesIn :: [Response.Response] -> [ObjectId.ObjectId]
chosenSourcesIn =
  let chosen r = case r of
        Response.ChoseDamageSource oid -> Just oid
        _ -> Nothing
   in Maybe.mapMaybe chosen

-- CR 609.7a's player-CHOSEN source, whose producer is Healing Grace ({W}
-- Instant: "Prevent the next 3 damage that would be dealt to any target this turn
-- by a source of your choice. You gain 3 life").
--
-- Mending Hands above with one clause added, which is exactly the difference the
-- rule makes: that shield watches every source ("the number of events or sources
-- dealing it doesn't matter", CR 615.7), this one watches the ONE object its
-- controller chose when the effect was created. The engine bakes the id into
-- DamagePattern.whichSource, never choosing it (CR 609.7a: "if an effect requires
-- a player to choose a source of damage").
--
-- The chosen source is deliberately NOT the first candidate the prompt offers:
-- CR 609.7a's pool here is alice's Plains, the two creatures, the shielded one
-- and Healing Grace itself on the stack, sorted ascending, so an engine that
-- ignored the answer and took the head would shield against the Plains and both
-- damage assertions would read the other way round.
healingGraceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
healingGraceSpec s registry = Spec.describe s "Healing Grace (CR 609.7a)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Noncombat
  -- The discriminating twin, differing from the case above in the CARD alone:
  -- Mending Hands prints no "of your choice", so its shield watches every source
  -- and no choice is raised at all. Without it the case above could pass on a
  -- board where nothing but the chosen source ever dealt damage.
  Spec.it s "CR 615.7 a shield naming NO source watches every source, and asks nothing" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addPermanent pikerPrinting S.alice base
        (alpha, g2) = S.addPermanent pikerPrinting S.bob g1
        (_, g3) = S.addPermanent pikerPrinting S.bob g2
        (g4, spellId) = S.handOne mendingHands g3
        shielded = castAndResolve (aimCreature victim) g4 spellId
        strike src n g = S.runPure S.identityAnswer g (Damage.applyDamage [hit src (Recipient.ToCreature victim) n])
    Spec.assertEqWith s "the same unchosen source's 2 is prevented here" (S.damageOf victim (strike alpha 2 shielded)) (Just 0)
    Spec.assertEqWith s "and nobody was asked to choose a source" (chosenSourcesIn (answersFor (aimCreature victim) g4 (S.cast S.alice spellId >> Stack.resolveTop))) []

-- Answer CR 609.7a's source choice with `src` and take the default everywhere
-- else. FILTERED, not built, for aimAndChoose's reason; the group below asks for
-- no targets, so there is nothing else to aim.
chooseDamageSourceOf :: ObjectId.ObjectId -> Prompt.Prompt r -> r
chooseDamageSourceOf src p = case p of
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> S.identityAnswer p

-- CR 118.12's gate PAID, with its sacrifice pinned to one permanent: the delayed
-- trigger case below runs on a board where alice controls four creatures, so the
-- offered set is a real choice and a default answer would eat the Fire-Eater the
-- case is about. FILTERED rather than hand-built, the posture every
-- choose-don't-target answerer in this file takes.
payingGate :: ObjectId.ObjectId -> Prompt.Prompt r -> r
payingGate fodder p = case p of
  Prompt.ChooseToPay {} -> PaymentDecision.Pays
  Prompt.ChooseSacrifices _ _ _ offered _ _ -> Set.fromList (filter (== fodder) offered)
  _ -> S.identityAnswer p

-- chooseDamageSourceOf with every target slot aimed at one PLAYER: aimAndChoose's
-- player-side twin, for the group's third-class case, whose Fire-Eater ability
-- has to be aimed at the player Auriok Replica's "to you" shields.
choosePlayerAndSource :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
choosePlayerAndSource pid src p = case p of
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> aimPlayer pid p

-- CR 609.7a's chosen source on the UNBOUNDED shield, whose producer is Auriok
-- Replica ({3} Artifact Creature -- Cleric, 2/2: "{W}, Sacrifice this creature:
-- Prevent all damage a source of your choice would deal to you this turn").
--
-- Healing Grace above with the amount removed, which is the difference CR 615.3
-- makes: that shield is used up by the 3 damage it prevents, this one runs until
-- the duration expires however many times the chosen source strikes.
--
-- The chosen source is deliberately NOT the first candidate the prompt offers:
-- CR 609.7a's pool here is alice's Plains, the two Pikers and the sacrificed
-- Replica, sorted ascending, so an engine that ignored the answer and took the
-- head would shield against the Plains and both damage assertions would read the
-- other way round. The Replica is in the pool through the rule's THIRD class and
-- not because it is a permanent -- its own cost sacrificed it, and the ability
-- resolving on the stack still names it as its CR 113.7 source. The ability
-- OBJECT is not in the pool: CR 609.7a admits "a spell on the stack", and an
-- activated ability sharing that zone is none of the four classes.
auriokReplicaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auriokReplicaSpec s registry = Spec.describe s "Auriok Replica (CR 609.7a)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Noncombat
  -- CR 400.7c, and CR 609.7a's last sentence for the same board: a source chosen
  -- while it was a permanent SPELL keeps the shield when that spell resolves --
  -- "the effect will apply to any damage dealt by that spell and any damage dealt
  -- by the permanent that spell becomes when it resolves". CR 400.7 mints a NEW
  -- id for that permanent, so a shield still naming the spell's id would watch an
  -- object that no longer exists.
  --
  -- The card cast is a SECOND Auriok Replica, so the spell is a permanent spell
  -- (CR 110.4b) and Pawl.Engine.Stack's permanent-spell branch is the one that
  -- runs. Four Plains: three pay the spell's {3}, the fourth the ability's {W}.
  Spec.it s "CR 400.7c the shield follows the chosen permanent spell onto the battlefield" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    replica <- S.printingOf s registry "Auriok Replica"
    let base = S.landsInPlay plains 4
        (replicaId, g1) = S.addPermanent replica S.alice base
        (decoy, g2) = S.addPermanent pikerPrinting S.bob g1
        (g3, cardId) = S.handOne replica g2
        -- The SPELL's own id, read off the stack rather than reused from the
        -- hand: casting is a zone change, so CR 400.7 already made the card and
        -- the spell two objects and `cardId` names neither candidate.
        afterCast = S.runPure S.identityAnswer g3 (S.cast S.alice cardId)
        spellId = case GameState.stack afterCast of
          oid : _ -> oid
          [] -> cardId
        shieldAgainst src =
          S.runPure
            (chooseDamageSourceOf src)
            afterCast
            ( Activate.activateAbility S.alice replicaId (theAbility replica)
                Monad.>> Stack.resolveTop
                Monad.>> Stack.resolveTop
            )
        shielded = shieldAgainst spellId
        elsewhere = shieldAgainst decoy
        arrived gs = Set.toList (Set.difference (GameState.battlefield gs) (GameState.battlefield afterCast))
        -- The permanent the spell became. Taken as what the battlefield GAINED
        -- over the post-cast board, so nothing about it is assumed: the two
        -- setup assertions below pay for the fallback.
        became gs = case arrived gs of
          [oid] -> oid
          _ -> spellId
        strike src n gs = S.runPure S.identityAnswer gs (Damage.applyDamage [hit src (Recipient.ToPlayer S.alice) n])
    -- THE gameplay assertion: the shield alice aimed at the SPELL prevents the
    -- damage the PERMANENT deals. Before carryOver re-keyed the row this 3 landed
    -- in full, the shield left naming an id nothing on the board carries.
    Spec.assertEqWith s "the permanent the chosen spell became deals no damage" (S.lifeOf S.alice (strike (became shielded) 3 shielded)) (Just 20)
    -- Its twin, differing in the ANSWER alone: aim the same shield at the Piker
    -- and the same permanent's 3 lands, so the case cannot pass on an engine that
    -- carried the shield to whatever resolved.
    Spec.assertEqWith s "and aiming the same shield elsewhere leaves that permanent's 3 landing in full" (S.lifeOf S.alice (strike (became elsewhere) 3 elsewhere)) (Just 17)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "setup: exactly one permanent arrived from the resolution" (length (arrived shielded)) 1
    Spec.assertEqWith s "setup: CR 400.7 gave that permanent an id the spell did not have" (became shielded == spellId) False
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "alice was asked which source, and answered the spell" (chosenSourcesIn (answersFor (chooseDamageSourceOf spellId) afterCast (Activate.activateAbility S.alice replicaId (theAbility replica) Monad.>> Stack.resolveTop Monad.>> Stack.resolveTop))) [spellId]
  -- CR 609.7a's THIRD class, which none of the three cases above reaches: "any
  -- object referred to by an object on the stack, by a replacement or prevention
  -- effect that's waiting to apply, or by a delayed triggered ability that's
  -- waiting to trigger (even if that object is no longer in the zone it used to
  -- be in)". Ghitu Fire-Eater ({2}{R} Creature -- Human Nomad 2/2, "{T},
  -- Sacrifice this creature: It deals damage equal to its power to any target")
  -- pays its own departure as a COST, so while its ability waits on the stack the
  -- id that ability names as its CR 113.7 source is in no zone at all and is a
  -- permanent, a spell and a command-zone object under none of the other three
  -- classes.
  --
  -- WHY IT IS THE ONLY PLAY: Pawl.Engine.Resolve's DealDamage arm files that same
  -- departed id as the damage event's source (CR 113.7a), and
  -- Pawl.Engine.Replacement.matchesDamagePattern compares whichSource by id. So
  -- until the third class was offered, no answer alice could give installed a
  -- shield that ever matched this damage -- the choice was not merely narrowed,
  -- the correct play was unreachable.
  --
  -- ORDER is the whole of the setup: the Fire-Eater's ability goes on the stack
  -- FIRST and the Replica's on top of it, so the reference is still standing when
  -- the choice is made, and the stack is resolved top-by-top rather than to empty.
  --
  -- TWO boards differing in the ANSWER alone, because one cannot separate the
  -- readings: alice on 20 says the shield matched, and a board where nothing else
  -- deals damage would read 20 under an engine that shielded everything. The
  -- Piker is the other answer and it is bob's, so the two answers name objects
  -- with different controllers as well as different ids.
  Spec.it s "CR 609.7a a source only a waiting ability still refers to is offered, and shields the damage it deals" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    replica <- S.printingOf s registry "Auriok Replica"
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    let base = S.landsInPlay plains 1
        (fireEater, g1) = S.addPermanent ghitu S.alice base
        (replicaId, g2) = S.addPermanent replica S.alice g1
        (decoy, g3) = S.addPermanent pikerPrinting S.bob g2
        act =
          Activate.activateAbility S.alice fireEater (theAbility ghitu)
            Monad.>> Activate.activateAbility S.alice replicaId (theAbility replica)
            Monad.>> Stack.resolveTop
            Monad.>> Stack.resolveTop
        -- Rank-2, so the answerer is applied at each use rather than let-bound.
        after src = S.runPure (choosePlayerAndSource S.alice src) g3 act
    -- THE gameplay assertion: alice named the departed Fire-Eater and its 2 is
    -- prevented. Before the third class that id was not in the offered set, the
    -- answerer fell back to the head of the pool, and this read 18.
    Spec.assertEqWith s "the departed source alice chose deals nothing" (S.lifeOf S.alice (after fireEater)) (Just 20)
    -- Its twin on the same board, differing in the ANSWER alone: aim the shield
    -- at bob's Piker and the same 2 lands, so the case cannot pass on an engine
    -- that shielded whatever turned up.
    Spec.assertEqWith s "and aiming the same shield at the Piker leaves that 2 landing in full" (S.lifeOf S.alice (after decoy)) (Just 18)
    -- The proxies, after the behaviour.
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject fireEater (after fireEater))) "setup: the cost really did remove the source, so the id names nothing"
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements (after fireEater))) 1
    Spec.assertEqWith s "alice was OFFERED the departed source and answered it" (chosenSourcesIn (answersFor (choosePlayerAndSource S.alice fireEater) g3 act)) [fireEater]

  -- The SAME rule and the same exclusion, reached by CR 609.7a's OTHER
  -- binding-reading carrier: "any object referred to by ... a delayed triggered
  -- ability that's waiting to trigger". Effect.ArmDelayedTrigger captures the
  -- resolving object's whole binding environment (CR 603.7c), so an activated
  -- ability that arms one leaves its OWN id (Binding.thisAbility) in the entry --
  -- and the ability has ceased by then, which is exactly the parenthetical case
  -- "even if that object is no longer in the zone it used to be in" and so is not
  -- filtered out anywhere downstream.
  --
  -- Grist, the Hunger Tide's -2 is the pool's only activated ability that arms a
  -- delayed trigger ("You may sacrifice a creature. When you do, destroy target
  -- creature or planeswalker"), and the entry is still armed once the -2 has
  -- resolved -- the reflexive ability is placed at the next gather, which this
  -- case deliberately does not run.
  --
  -- The board and the reading are the case above's: alice names the ceased
  -- ability, the answerer falls back to the head of the ascending pool when that
  -- id is not offered, and the Fire-Eater is placed FIRST so the head is the one
  -- source whose damage this shield can prevent. Offering the ability installs a
  -- shield over an object no damage event carries and alice takes the 2.
  Spec.it s "CR 609.7a an activated ability a waiting delayed trigger still names is not offered as a source" $ do
    plains <- S.printingOf s registry "Plains"
    replica <- S.printingOf s registry "Auriok Replica"
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    grist <- S.printingOf s registry "Grist, the Hunger Tide"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    -- The Fire-Eater FIRST, so its id is the ascending pool's head and the
    -- fallback lands on it; the Piker LAST among the creatures, so Grist's CR
    -- 118.12 gate has exactly one candidate to eat.
    let (fireEater, g1) = S.addPermanent ghitu S.alice (Setup.emptyGame S.bothPlayers)
        (replicaId, g2) = S.addPermanent replica S.alice g1
        (gristId, g3) = S.addPermanent grist S.alice g2
        g4 = S.addCounter CounterKind.Loyalty 3 gristId g3
        (fodder, g5) = S.addPermanent pikerPrinting S.alice g4
        (decoy, g6) = S.addPermanent pikerPrinting S.bob g5
        board = S.landsFor plains S.alice 1 g6
        -- The card file's second ability, after the +1.
        minusTwo = case Face.activatedAbilities (S.combinedFace grist) of
          _ : ab : _ -> ab
          _ -> theAbility grist
        -- Grist's -2 alone, paid, so the delayed trigger is armed and its
        -- captured environment holds the ability's own id. Its reflexive trigger
        -- is left unplaced, which is what keeps the entry WAITING.
        armed = S.runPure (payingGate fodder) board (Activate.activateAbility S.alice gristId minusTwo Monad.>> Stack.resolveTop)
        -- The id the entry still names, read off the stack while the -2 is on it
        -- rather than guessed.
        onStack = S.runPure (payingGate fodder) board (Activate.activateAbility S.alice gristId minusTwo)
        abilityId = case GameState.stack onStack of
          oid : _ -> oid
          [] -> gristId
        rest =
          Activate.activateAbility S.alice fireEater (theAbility ghitu)
            Monad.>> Activate.activateAbility S.alice replicaId (theAbility replica)
            Monad.>> Stack.resolveTop
            Monad.>> Stack.resolveTop
        after src = S.runPure (choosePlayerAndSource S.alice src) armed rest
    -- THE gameplay assertion: naming the ceased ability gets alice the FALLBACK,
    -- and the fallback prevents the Fire-Eater's 2. An engine that let the
    -- delayed trigger's captured environment offer the ability would shield an
    -- object that deals no damage and leave alice on 18.
    Spec.assertEqWith s "the ability the delayed trigger names was not offered, so the fallback shielded the Fire-Eater" (S.lifeOf S.alice (after abilityId)) (Just 20)
    -- Its twin, differing in the ANSWER alone and naming something the rule DOES
    -- admit through this very carrier: the departed Fire-Eater, which the waiting
    -- ability on the stack refers to. So the 20 above is not "nothing was
    -- offered at all".
    Spec.assertEqWith s "naming the departed Fire-Eater, which IS offered, prevents the same 2" (S.lifeOf S.alice (after fireEater)) (Just 20)
    -- And the reading that separates them: aim the shield at bob's Piker -- an
    -- ordinary permanent, offered by CR 609.7a's first class, that deals no damage
    -- here -- and the 2 lands, so neither 20 above is "everything was prevented".
    Spec.assertEqWith s "aiming it at bob's Piker, which shields nothing that happens here, leaves the 2 landing" (S.lifeOf S.alice (after decoy)) (Just 18)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "setup: the -2 armed a delayed trigger and it is still waiting" (length (GameState.delayedTriggers armed)) 1
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject abilityId armed)) "setup: CR 608.2m the -2's ability object has ceased, so only the entry still names it"
    Spec.assertBool s (abilityId /= gristId && abilityId /= fireEater) "setup: the id named is the ability object, not Grist and not the Fire-Eater"
    Spec.assertBool s (decoy /= replicaId && decoy /= fireEater) "setup: the id the twin names is bob's Piker"
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject fodder armed) && Set.member fireEater (GameState.battlefield armed)) "setup: the gate ate the spare Piker and left the Fire-Eater standing"
    Spec.assertEqWith s "the answer recorded is the fallback, not the ability alice named" (chosenSourcesIn (answersFor (choosePlayerAndSource S.alice abilityId) armed rest)) [fireEater]

-- CR 611.2c's LIVE recipient set on the UNBOUNDED shield, whose producer is Pack
-- Leader ({1}{W} Creature -- Dog, 2/2: "Other Dogs you control get +1/+1.
-- Whenever this creature attacks, prevent all combat damage that would be dealt
-- this turn to Dogs you control.").
--
-- The shield DESCRIBES its recipients rather than naming them, and that is the
-- whole point of the card here: a prevention effect modifies no characteristic
-- and no controller, so CR 611.2c leaves its set live and the description is
-- re-asked at each damage event. Spelling the same clause as a ref over "each Dog
-- you control" would sweep the set as the trigger resolved -- weaker than printed
-- in one direction and stricter in the other -- which is what the third case
-- below tells apart.
--
-- Two seats is enough: the card says "you control" and names no opponent, so the
-- three-seat trap does not apply. bob's own Dog is what makes "you control" do
-- work, and it is a recipient rather than a role.
packLeaderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
packLeaderSpec s registry = Spec.describe s "Pack Leader (CR 611.2c)" $ do
  Spec.it s "CR 611.2c the shield covers whatever matches its description when the damage would happen" $ do
    leader <- S.printingOf s registry "Pack Leader"
    tracker <- S.printingOf s registry "Ainok Tracker"
    piker <- S.printingOf s registry "Goblin Piker"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    let (gs, mine, theirs) = S.combatBoardOf [leader, tracker, piker] [tracker, piker]
        -- The declare blockers step, Hanweir Garrison's vantage point: the
        -- trigger fired at the declaration (CR 508.2b) and resolved in the
        -- declare attackers step's priority round, so the shield is up and no
        -- combat damage has been dealt yet.
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        -- A Dog that comes under alice's control AFTER the shield resolved, which
        -- is exactly the object CR 611.2c's second sentence admits and a swept
        -- set could not.
        (latecomer, board) = S.addPermanent kinGuard S.alice atBlockers
        hit src recipient n =
          DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Combat
        -- ONE source for every case, so each pair below differs in the recipient
        -- alone: the shield says nothing about its source, and a case that
        -- swapped sources would not be a twin of the one beside it.
        strike src oid n = S.damageOf oid (S.runPure S.identityAnswer board (Damage.applyDamage [hit src (Recipient.ToCreature oid) n]))
    case (mine, theirs) of
      ([leaderId, ourDog, ourPiker], [theirDog, theirPiker]) -> do
        let strikeFrom = strike theirPiker
        -- THE gameplay assertion: a recipient the shield only DESCRIBES is
        -- covered, which is the field this card exists to exercise.
        Spec.assertEqWith s "the 3 combat damage to a Dog alice controls is prevented whole" (strikeFrom ourDog 3) (Just 0)
        -- Its twin, differing in the RECIPIENT alone and on the same board: a
        -- creature of alice's that is not a Dog takes its damage, so the case
        -- above cannot pass on a shield that covers everything.
        Spec.assertEqWith s "the 4 to alice's non-Dog Piker is marked in full" (strikeFrom ourPiker 4) (Just 4)
        -- CR 611.2c's own case: this Dog was not on the battlefield when the
        -- shield was created, and the shield covers it anyway.
        Spec.assertEqWith s "CR 611.2c the Dog that entered after the shield resolved is covered too" (strikeFrom latecomer 5) (Just 0)
        -- The other half of the description -- "you control" -- with the same
        -- creature type on the other side of the table.
        Spec.assertEqWith s "bob's own Dog is not covered, the clause saying you control" (strikeFrom theirDog 6) (Just 6)
        -- The proxies, after the behaviour.
        Spec.assertEqWith s "setup: the shield is one floating replacement" (length (GameState.replacements atBlockers)) 1
        Spec.assertEqWith s "setup: Pack Leader was declared as an attacker" (elem leaderId (S.attackerDeclarationsOf atBlockers)) True
        Spec.assertEqWith s "setup: the latecomer is on the battlefield and undamaged" (S.damageOf latecomer board) (Just 0)
      _ -> Spec.assertFailure s "fixture should have three creatures for alice and two for bob"

-- Aims a spell at `oid` by narrowing the offered set to it: the target is
-- FILTERED out of the offered set rather than rebuilt, since a hand-built
-- recipient would be dropped at CR 608.2b's re-read with no error.
targetingOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
targetingOnly oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- CR 609.7a's third class read the OTHER way -- what a waiting row does NOT refer
-- to -- whose producer is Synthetic Parting Ward ({1}{W} Instant: "Destroy target
-- creature. Prevent the next 2 combat damage that would be dealt to you this
-- turn."), with Auriok Replica supplying the later chooser and Ghitu Fire-Eater
-- the damage.
--
-- The rule admits "any object referred to by ... a replacement or prevention
-- effect that's waiting to apply". A floating shield carries a snapshot of the
-- installing resolution's bindings so its own Filters can still be answered at
-- the damage event (Pawl.Types.ActiveReplacement; Synthetic Communal Bulwark
-- is what needs it), and this card's shield names NONE of them: it
-- describes its recipient as a player relation, prints no source properties, asks
-- for no chosen source and carries no rider. So the creature its FIRST clause
-- destroyed is referred to by the spell and not by the row the spell left behind,
-- and it must not turn up in the Replica's pool.
--
-- WHY THE CREATURE HAS TO DIE: every one of CR 609.7a's other three classes would
-- otherwise offer it anyway. Destroyed, it is on no battlefield, is no spell, and
-- sits in no command zone, so the row's captured map is the only thing that could
-- put it in front of a chooser -- exactly the parenthesis's "even if that object
-- is no longer in the zone it used to be in", read as an exclusion.
--
-- SYNTHETIC because no printing binds a slot in one clause and installs a damage
-- row that ignores it in the next. Scryfall o:"prevent the next" (o:"destroy
-- target" or o:"exile target" or o:"return target" or o:"target creature card" or
-- o:"tap target") and o:"prevent all damage" with the same disjunction,
-- 2026-08-31: the hits either put the two clauses in different modes (Ivory
-- Charm, Rith's Charm, Dromoka's Command), in different halves of a split card
-- (Stand // Deliver), or aim both at ONE object (Enshrouding Mist, Inquisitor's
-- Snare), which the row then genuinely refers to. What would refute this is a
-- printing whose single resolution targets one object and shields another.
--
-- THE FALLBACK IS THE OBSERVABLE, chooseDamageSourceOf's filtered-not-trusted
-- posture: alice names the destroyed creature every time, so an engine that
-- offers it shields a corpse and takes the Fire-Eater's 2, and an engine that
-- declines it falls back to the head of the ascending pool. The Fire-Eater is
-- placed FIRST so that head is the one source whose damage this shield can
-- prevent.
--
-- Two seats is enough: the card says "you" and names no opponent. bob owns the
-- destroyed creature, so the offered id belongs to the other seat as well as to
-- another zone.
partingWardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
partingWardSpec s registry = Spec.describe s "Synthetic Parting Ward (CR 609.7a)" $ do
  Spec.it s "CR 609.7a an object bound by an unrelated clause of the row's own resolution is not offered" $ do
    plains <- S.printingOf s registry "Plains"
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    replica <- S.printingOf s registry "Auriok Replica"
    piker <- S.printingOf s registry "Goblin Piker"
    ward <- S.printingOf s registry "Synthetic Parting Ward"
    let (fireEater, g1) = S.addPermanent ghitu S.alice (Setup.emptyGame S.bothPlayers)
        (destroyed, g2) = S.addPermanent piker S.bob g1
        (replicaId, g3) = S.addPermanent replica S.alice g2
        -- Three Plains: two pay the Ward's {1}{W}, the third the Replica's {W}.
        g4 = S.landsFor plains S.alice 3 g3
        (wardId, g5) = S.addHandCard ward S.alice g4
        ready =
          g5
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        -- The Ward resolved: bob's creature is destroyed and the shield is
        -- waiting, holding whatever its installation captured.
        armed = S.runPure (targetingOnly destroyed) ready (S.cast S.alice wardId Monad.>> Stack.resolveTop)
        -- The Fire-Eater's ability goes on the stack FIRST and the Replica's on
        -- top of it, so the choice is made while the shield is still waiting and
        -- the Fire-Eater's damage is dealt afterwards.
        rest =
          Activate.activateAbility S.alice fireEater (theAbility ghitu)
            Monad.>> Activate.activateAbility S.alice replicaId (theAbility replica)
            Monad.>> Stack.resolveTop
            Monad.>> Stack.resolveTop
        after src = S.runPure (choosePlayerAndSource S.alice src) armed rest
        -- The lands were placed last, so the battlefield's highest id is a Plains.
        plainsId = Set.findMax (GameState.battlefield armed)
    -- THE gameplay assertion: naming the destroyed creature gets alice the
    -- FALLBACK, and the fallback shields the Fire-Eater whose 2 is the only damage
    -- on this board. An engine offering it shields an object that deals nothing
    -- and leaves alice on 18.
    Spec.assertEqWith s "the destroyed creature was not offered, so the fallback shielded the Fire-Eater" (S.lifeOf S.alice (after destroyed)) (Just 20)
    -- Its twin on the same board, differing in the ANSWER alone and naming
    -- something the rule DOES admit: a Plains is a permanent, is offered, and
    -- shields nothing that happens here -- so the 20 above is neither "everything
    -- was prevented" nor the Ward's own shield catching this damage.
    Spec.assertEqWith s "naming a Plains instead, which IS a legal choice, leaves the same 2 landing" (S.lifeOf S.alice (after plainsId)) (Just 18)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "the answer recorded is the fallback, not the destroyed creature alice named" (chosenSourcesIn (answersFor (choosePlayerAndSource S.alice destroyed) armed rest)) [fireEater]
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject destroyed armed)) "setup: the Ward really did destroy bob's creature, so the id names nothing"
    Spec.assertEqWith s "setup: the Ward installed exactly one floating row" (length (GameState.replacements armed)) 1
    Spec.assertEqWith s "setup: that row names no slot, so it captured none" (foldMap ActiveReplacement.slots (GameState.replacements armed)) Map.empty
    Spec.assertBool s (plainsId /= destroyed && plainsId /= fireEater) "setup: the id the twin names is a Plains and neither of the two creatures"

-- CR 609.7a's third class read off what a resolution BAKED into a waiting row
-- rather than off what the row captured, whose producers are Healing Grace ({W}
-- Instant: "Prevent the next 3 damage that would be dealt to any target this turn
-- by a source of your choice. You gain 3 life.") for the shield that does the
-- referring, Ghitu Fire-Eater for the object it refers to, and Auriok Replica for
-- the later chooser.
--
-- The rule admits "any object referred to by ... a replacement or prevention
-- effect that's waiting to apply (even if that object is no longer in the zone it
-- used to be in)". A shield built by Resolve.installDamageRow refers to three
-- objects no Filter and no slot map holds, because card data cannot name an
-- object by id: DamagePattern.whichSource, DamagePattern.whichRecipient and CR
-- 614.9's baked redirect destination. This case is the FIRST of the three; the
-- other two ride the same total (Resolve.referentsOfReplacement) and no board in
-- data/cards/ puts either in front of a chooser.
--
-- WHY THE FIRE-EATER HAS TO DEPART FIRST: the shield's chosen source is picked
-- while the Fire-Eater's ability is still on the stack, so CR 609.7a's third class
-- offers it then through the STACK. Once that ability resolves the stack is empty,
-- the creature is in no zone, is no spell and sits in no command zone -- and the
-- shield's own captured map is EMPTY, Healing Grace's pattern naming no slot, so
-- the baked id is the only thing left that can put it in front of the Replica's
-- chooser.
--
-- THE FALLBACK IS THE OBSERVABLE, chooseDamageSourceOf's filtered-not-trusted
-- posture: alice names the departed Fire-Eater every time, and an engine that
-- declines to offer it falls back to the head of the ascending pool. bob's Piker
-- is placed FIRST so that head is a permanent whose damage is not what this board
-- deals.
--
-- The shield Healing Grace left is aimed at the PIKER, so it never sees the strike
-- at alice below -- and the Fire-Eater's own 2 landing on that Piker is prevented,
-- which is the setup assertion proving the first shield really did bake the
-- departed id rather than watch everything.
healingGraceReferentSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
healingGraceReferentSpec s registry = Spec.describe s "Healing Grace and Auriok Replica (CR 609.7a)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Noncombat
  Spec.it s "CR 609.7a a source only a waiting shield's baked field still names is offered" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    replica <- S.printingOf s registry "Auriok Replica"
    healingGrace <- S.printingOf s registry "Healing Grace"
    let (victim, g1) = S.addPermanent pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
        (fireEater, g2) = S.addPermanent ghitu S.alice g1
        (replicaId, g3) = S.addPermanent replica S.alice g2
        -- Two Plains: one pays Healing Grace's {W}, the other the Replica's {W}.
        g4 = S.landsFor plains S.alice 2 g3
        (graceId, g5) = S.addHandCard healingGrace S.alice g4
        ready =
          g5
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        -- The Fire-Eater's ability goes on the stack FIRST, so the reference is
        -- still standing when Healing Grace bakes it; Healing Grace resolves next
        -- and the ability last, leaving an empty stack and a waiting shield.
        armed =
          S.runPure
            (aimAndChoose victim fireEater)
            ready
            ( Activate.activateAbility S.alice fireEater (theAbility ghitu)
                Monad.>> S.cast S.alice graceId
                Monad.>> Stack.resolveTop
                Monad.>> Stack.resolveTop
            )
        rest = Activate.activateAbility S.alice replicaId (theAbility replica) Monad.>> Stack.resolveTop
        after src = S.runPure (chooseDamageSourceOf src) armed rest
        strike gs = S.runPure S.identityAnswer gs (Damage.applyDamage [hit fireEater (Recipient.ToPlayer S.alice) 3])
        -- The lands were placed last, so the battlefield's highest id is a Plains.
        plainsId = Set.findMax (GameState.battlefield armed)
    -- THE gameplay assertion: alice named the Fire-Eater, which by now only the
    -- waiting shield's DamagePattern.whichSource still names, and the Replica's
    -- shield watches it. Before the baked ids joined the pool the answerer fell
    -- back and this read 20.
    Spec.assertEqWith s "the source only the waiting shield names is offered, and its 3 is prevented" (S.lifeOf S.alice (strike (after fireEater))) (Just 23)
    -- Its twin on the same board, differing in the ANSWER alone: a Plains is a
    -- permanent and always was a legal choice, and shields nothing here -- so the
    -- 23 above is not "everything was prevented".
    Spec.assertEqWith s "naming a Plains instead leaves that same 3 landing in full" (S.lifeOf S.alice (strike (after plainsId))) (Just 20)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "alice was OFFERED the baked source and answered it" (chosenSourcesIn (answersFor (chooseDamageSourceOf fireEater) armed rest)) [fireEater]
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject fireEater armed)) "setup: the cost really did remove the Fire-Eater, so the id names nothing"
    Spec.assertEqWith s "setup: Healing Grace left exactly one floating row" (length (GameState.replacements armed)) 1
    Spec.assertEqWith s "setup: that row captured no slot, so only its baked field can name the Fire-Eater" (foldMap ActiveReplacement.slots (GameState.replacements armed)) Map.empty
    Spec.assertEqWith s "setup: that row really watches the departed source, so the Fire-Eater's own 2 on the Piker was prevented" (S.damageOf victim armed) (Just 0)
    Spec.assertBool s (plainsId /= fireEater && plainsId /= victim) "setup: the id the twin names is a Plains and neither creature"

-- Synthetic Parting Ward's case one carrier over, on the row an Effect.Replace
-- installs rather than on a prevention shield, and with a REAL printing behind it:
-- Galvanic Blast ({R} Instant: "Galvanic Blast deals 2 damage to any target.
-- Metalcraft -- Galvanic Blast deals 4 damage instead if you control three or more
-- artifacts."), with Auriok Replica supplying the later chooser and Ghitu
-- Fire-Eater the damage.
--
-- The card is the shape the row's over-capture needed and the pool had all along:
-- metalcraft is a CR 614.15 self-replacement installed as its own Effect.Replace,
-- ahead of the DealDamage sharing its clause, and the resolution's `target` slot
-- -- bound at CR 601.2c, and read by that other effect alone -- is no part of what
-- the row describes.
-- Its pattern is Filter.IsSource, its rewrite a literal 4 and its clause a count
-- of artifacts; none names a slot, so the row refers to no object at all.
--
-- WITHOUT METALCRAFT ON PURPOSE: alice controls no artifact, so CR 614.1's clause
-- is false when the Blast's own damage arrives, the row is never used up (CR
-- 614.5) and it is still waiting when the Replica's chooser runs.
--
-- WHY THE TARGET HAS TO DIE: every one of CR 609.7a's other three classes would
-- otherwise offer it anyway. The Blast's 2 kills bob's 2/1 outright, so afterwards
-- it is on no battlefield, is no spell and sits in no command zone -- exactly the
-- parenthesis's "even if that object is no longer in the zone it used to be in",
-- read as an exclusion.
--
-- THE FALLBACK IS THE OBSERVABLE, Synthetic Parting Ward's posture: alice names the
-- dead creature every time, so an engine that offers it shields a corpse and takes
-- the Fire-Eater's 2, and one that declines it falls back to the head of the
-- ascending pool. The Fire-Eater is placed FIRST so that head is the one source
-- whose damage this shield can prevent.
galvanicBlastReferentSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
galvanicBlastReferentSpec s registry = Spec.describe s "Galvanic Blast (CR 609.7a)" $ do
  Spec.it s "CR 609.7a a slot the installing resolution bound but the row never names is not offered" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    replica <- S.printingOf s registry "Auriok Replica"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    blast <- S.printingOf s registry "Galvanic Blast"
    let (fireEater, g1) = S.addPermanent ghitu S.alice (Setup.emptyGame S.bothPlayers)
        (destroyed, g2) = S.addPermanent pikerPrinting S.bob g1
        (replicaId, g3) = S.addPermanent replica S.alice g2
        -- One Mountain for the Blast's {R}, one Plains for the Replica's {W}.
        g4 = S.landsFor mountain S.alice 1 g3
        g5 = S.landsFor plains S.alice 1 g4
        (blastId, g6) = S.addHandCard blast S.alice g5
        ready =
          g6
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        -- The Blast resolved: its metalcraft row is waiting, holding whatever its
        -- installation captured, and CR 704.5g has taken the 2/1 it killed.
        armed =
          S.runPure
            (targetingOnly destroyed)
            ready
            (S.cast S.alice blastId Monad.>> Stack.resolveTop Monad.>> Sba.checkStateBasedActions)
        rest =
          Activate.activateAbility S.alice fireEater (theAbility ghitu)
            Monad.>> Activate.activateAbility S.alice replicaId (theAbility replica)
            Monad.>> Stack.resolveTop
            Monad.>> Stack.resolveTop
        after src = S.runPure (choosePlayerAndSource S.alice src) armed rest
        -- The lands were placed last, so the battlefield's highest id is one.
        landId = Set.findMax (GameState.battlefield armed)
    -- THE gameplay assertion: naming the dead creature gets alice the FALLBACK, and
    -- the fallback shields the Fire-Eater whose 2 is the only damage on this board.
    -- An engine handing the row the whole resolution's bindings offers the creature,
    -- shields something that deals nothing, and leaves alice on 18.
    Spec.assertEqWith s "the slot the row never names was not offered, so the fallback shielded the Fire-Eater" (S.lifeOf S.alice (after destroyed)) (Just 20)
    -- Its twin on the same board, differing in the ANSWER alone and naming something
    -- the rule DOES admit: a land is a permanent, is offered, and shields nothing
    -- that happens here.
    Spec.assertEqWith s "naming a land instead, which IS a legal choice, leaves the same 2 landing" (S.lifeOf S.alice (after landId)) (Just 18)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "the answer recorded is the fallback, not the dead creature alice named" (chosenSourcesIn (answersFor (choosePlayerAndSource S.alice destroyed) armed rest)) [fireEater]
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject destroyed armed)) "setup: the Blast's 2 really did kill bob's 2/1, so the id names nothing"
    Spec.assertEqWith s "setup: metalcraft left exactly one floating row" (length (GameState.replacements armed)) 1
    Spec.assertEqWith s "setup: that row names no slot, so it captured none" (foldMap ActiveReplacement.slots (GameState.replacements armed)) Map.empty
    Spec.assertBool s (landId /= destroyed && landId /= fireEater) "setup: the id the twin names is a land and neither creature"

-- CR 609.7a's third class read off the SLOTS a waiting row captured rather than
-- off the ids a resolution baked into it, whose producers are Synthetic Communal
-- Bulwark ({1}{W} Instant: "Prevent the next 3 damage that would be dealt this
-- turn to you and to target creature.") for the row that refers, Ghitu Fire-Eater
-- for the object it refers to, and Healing Grace for the later chooser.
--
-- The captured map is the one carrier of a waiting row the two cases above cannot
-- reach: Healing Grace's row names no slot and Galvanic Blast's metalcraft row
-- names none either, which is what that case proves by exclusion. The Bulwark's
-- DamagePattern.whatRecipient is a Filter.IsBound over its target slot, so
-- Resolve.installDamageRow's restriction keeps exactly that slot and CR 609.7a's
-- "any object referred to by ... a replacement or prevention effect that's
-- waiting to apply" reaches the creature through it.
--
-- WHY THE FIRE-EATER HAS TO DEPART FIRST: every one of the rule's other three
-- classes would otherwise offer it anyway. Its own ability sacrifices it as a
-- cost and then resolves, so by the time Healing Grace is cast the id is on no
-- battlefield, is no spell and sits in no command zone -- the parenthesis's "even
-- if that object is no longer in the zone it used to be in".
--
-- HEALING GRACE rather than Auriok Replica as the chooser, which the two cases
-- above use: the Replica's shield covers "you" and so does the Bulwark's, so the
-- two would overlap and no life total could be attributed to one of them. Healing
-- Grace's shield is aimed at bob's Ainok Tracker, whom the Bulwark covers on
-- neither of its two ends.
--
-- Two seats is enough: neither card names an opponent. bob owns the creature the
-- damage lands on, so the shield's recipient is on the other side of the table
-- from its chosen source.
communalBulwarkReferentSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
communalBulwarkReferentSpec s registry = Spec.describe s "Synthetic Communal Bulwark and Healing Grace (CR 609.7a)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Noncombat
  Spec.it s "CR 609.7a a source only a waiting row's captured slot still names is offered" $ do
    plains <- S.printingOf s registry "Plains"
    ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
    bulwark <- S.printingOf s registry "Synthetic Communal Bulwark"
    healingGrace <- S.printingOf s registry "Healing Grace"
    trackerPrinting <- S.printingOf s registry "Ainok Tracker"
    let (fireEater, g1) = S.addPermanent ghitu S.alice (Setup.emptyGame S.bothPlayers)
        (victim, g2) = S.addPermanent trackerPrinting S.bob g1
        -- Three Plains: two pay the Bulwark's {1}{W}, the third Healing Grace's {W}.
        g3 = S.landsFor plains S.alice 3 g2
        (bulwarkId, g4) = S.addHandCard bulwark S.alice g3
        (graceId, g5) = S.addHandCard healingGrace S.alice g4
        ready =
          g5
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        -- The Bulwark resolved onto the Fire-Eater, so its row's captured map
        -- holds that one slot.
        shielded = S.runPure (targetingOnly fireEater) ready (S.cast S.alice bulwarkId Monad.>> Stack.resolveTop)
        -- Then the Fire-Eater pays itself as a cost and its ability resolves,
        -- leaving the stack empty and the id naming nothing. The 2 goes at BOB,
        -- whom the Bulwark's shield covers on neither end.
        armed = S.runPure (aimPlayer S.bob) shielded (Activate.activateAbility S.alice fireEater (theAbility ghitu) Monad.>> Stack.resolveTop)
        rest = S.cast S.alice graceId Monad.>> Stack.resolveTop
        after src = S.runPure (aimAndChoose victim src) armed rest
        strike gs = S.runPure S.identityAnswer gs (Damage.applyDamage [hit fireEater (Recipient.ToCreature victim) 3])
        -- The lands were placed last, so the battlefield's highest id is a Plains.
        plainsId = Set.findMax (GameState.battlefield armed)
    -- THE gameplay assertion: alice named the Fire-Eater, which by now only the
    -- Bulwark's captured slot still names, and Healing Grace's shield watches it.
    Spec.assertEqWith s "the source only the waiting row's captured slot names is offered, and its 3 is prevented" (S.damageOf victim (strike (after fireEater))) (Just 0)
    -- Its twin on the same board, differing in the ANSWER alone: a Plains is a
    -- permanent and always was a legal choice, and shields nothing here -- so the
    -- 0 above is not "everything was prevented".
    Spec.assertEqWith s "naming a Plains instead leaves that same 3 landing in full" (S.damageOf victim (strike (after plainsId))) (Just 3)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "alice was OFFERED the captured source and answered it" (chosenSourcesIn (answersFor (aimAndChoose victim fireEater) armed rest)) [fireEater]
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject fireEater armed)) "setup: the cost really did remove the Fire-Eater, so the id names nothing"
    Spec.assertEqWith s "setup: the Bulwark left exactly one floating row, and the stack is empty" (length (GameState.replacements armed), GameState.stack armed) (1, [])
    Spec.assertEqWith s "setup: that row captured the one slot its recipient names, holding the Fire-Eater" (foldMap (foldMap Set.toList . ActiveReplacement.slots) (GameState.replacements armed)) [fireEater]
    Spec.assertBool s (plainsId /= fireEater && plainsId /= victim) "setup: the id the twin names is a Plains and neither creature"

-- CR 609.7a's third class off its LAST carrier, whose producers are Come Back
-- Wrong ({2}{B} Sorcery: "Destroy target creature. If a creature card is put into
-- a graveyard this way, return it to the battlefield under your control.
-- Sacrifice it at the beginning of your next end step.") for the delayed
-- triggered ability that refers, Goblin Piker for the object it refers to, and
-- Auriok Replica for the later chooser.
--
-- The rule admits "any object referred to by ... a delayed triggered ability
-- that's waiting to trigger (even if that object is no longer in the zone it used
-- to be in)". CR 603.7c is why the entry carries an environment at all, and
-- Resolve's ArmDelayedTrigger arm captures the resolution's bindings into it, so
-- the `target` slot -- the id the creature had ON THE BATTLEFIELD, before CR
-- 400.7 made it two further objects in the graveyard and back -- rides the entry
-- and nothing else.
--
-- WHY THE ENTRY IS THE ONLY CARRIER HERE, which is what the setup assertions pay
-- for: this board never installs a replacement row at all, and the stack is empty
-- when the Replica's chooser runs. The battlefield holds the RETURNED permanent,
-- a third object with a third id, so the id under test is in no zone and named by
-- nothing but the waiting entry. Its trigger condition is the beginning of
-- alice's end step and this board never advances a step, so the entry is still
-- waiting.
--
-- Two seats is enough: neither card names an opponent, and bob owns the creature
-- Come Back Wrong destroys.
comeBackWrongSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
comeBackWrongSpec s registry = Spec.describe s "Come Back Wrong and Auriok Replica (CR 609.7a)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing Nothing mempty False DamageKind.Noncombat
  Spec.it s "CR 609.7a a source only a waiting delayed trigger still refers to is offered" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    replica <- S.printingOf s registry "Auriok Replica"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    comeBack <- S.printingOf s registry "Come Back Wrong"
    let (departed, g1) = S.addPermanent pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
        (replicaId, g2) = S.addPermanent replica S.alice g1
        -- Three Swamps for the Sorcery's {2}{B} and two Plains for the Replica's
        -- {W}: the Sorcery is paid first and spends at most three lands, so a
        -- Plains survives however the payment picks.
        g3 = S.landsFor swamp S.alice 3 g2
        g4 = S.landsFor plains S.alice 2 g3
        (comeBackId, g5) = S.addHandCard comeBack S.alice g4
        ready =
          g5
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        -- The lands were placed last, so `ready`'s highest battlefield id is a
        -- Plains. Read BEFORE the cast, since the returned permanent enters last
        -- of all and would otherwise be the maximum.
        plainsId = Set.findMax (GameState.battlefield ready)
        armed = S.runPure (targetingOnly departed) ready (S.cast S.alice comeBackId Monad.>> Stack.resolveTop)
        rest = Activate.activateAbility S.alice replicaId (theAbility replica) Monad.>> Stack.resolveTop
        after src = S.runPure (chooseDamageSourceOf src) armed rest
        strike gs = S.runPure S.identityAnswer gs (Damage.applyDamage [hit departed (Recipient.ToPlayer S.alice) 3])
    -- THE gameplay assertion: alice named the id only the waiting entry still
    -- refers to, and the Replica's shield watches it.
    Spec.assertEqWith s "the source only the waiting delayed trigger names is offered, and its 3 is prevented" (S.lifeOf S.alice (strike (after departed))) (Just 20)
    -- Its twin on the same board, differing in the ANSWER alone: a Plains is a
    -- permanent and always was a legal choice, and shields nothing here.
    Spec.assertEqWith s "naming a Plains instead leaves that same 3 landing in full" (S.lifeOf S.alice (strike (after plainsId))) (Just 17)
    -- The proxies, after the behaviour.
    Spec.assertEqWith s "alice was OFFERED the departed source and answered it" (chosenSourcesIn (answersFor (chooseDamageSourceOf departed) armed rest)) [departed]
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject departed armed)) "setup: CR 400.7 really did retire the battlefield id, so it names nothing"
    Spec.assertEqWith s "setup: the resolution armed exactly one delayed trigger" (Seq.length (GameState.delayedTriggers armed)) 1
    Spec.assertEqWith s "setup: no row is waiting and the stack is empty, so only that entry can name the id" (length (GameState.replacements armed), GameState.stack armed) (0, [])
    Spec.assertBool s (plainsId /= departed) "setup: the id the twin names is a Plains and not the departed creature"

-- CR 615.5's ADDITIONAL EFFECT, whose producer is Test of Faith ({1}{W} Instant:
-- "Prevent the next 3 damage that would be dealt to target creature this turn.
-- For each 1 damage prevented this way, put a +1/+1 counter on that creature").
--
-- Not a triggered ability, and that is the whole of what these cases
-- discriminate. Test of Faith's 2004-12-01 ruling: "The +1/+1 counters are put
-- onto the creature at the same time the damage is prevented. If a 1/1 creature
-- would be dealt 6 damage, 3 damage is prevented and three +1/+1 counters are
-- put on the creature." Under a triggered reading the counters would wait for
-- the stack and CR 704.5g would have destroyed the creature first.
--
-- Two seats is enough: no relational text is under test -- the rider names the
-- shielded creature, never "an opponent" or "the defending player" -- so the
-- three-seat trap does not apply.
--
-- The COMBAT case really runs the combat steps through Pawl.Engine.Engine, which
-- the other groups in this file avoid: the ordering under test is the one
-- between the rider and the step's state-based action check, and only the engine
-- has both.
testOfFaithSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
testOfFaithSpec s registry = Spec.describe s "Test of Faith (CR 615.5)" $ do
  let -- Put a board at declare attackers with alice active and bob defending,
  -- The other funnel: damage dealt by a RESOLVING ability rather than by combat,
  -- which drains the rider inside Pawl.Engine.Resolve instead of inside the
  -- combat damage step. One ping against a shield of 3 -- so the counter count
  -- is 1, the amount THIS application prevented, and not the shield's printed 3.
  Spec.it s "CR 615.5 a shield spent by a resolving ability runs its rider too" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    testOfFaith <- S.printingOf s registry "Test of Faith"
    let base = S.landsInPlay plains 2
        (victim, g1) = S.addPermanent pikerPrinting S.alice base
        (pinger, g2) = S.addPermanent sorcerer S.alice g1
        (g3, spellId) = S.handOne testOfFaith g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        ping g = S.runPure (aimCreature victim) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        control = ping g3
    Spec.assertEqWith s "setup: the shield holds 3" (shieldsLeft shielded) [3]
    Spec.assertEqWith s "the ping's 1 damage is prevented, so nothing is marked" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and exactly one counter goes on -- the amount prevented, not the shield" (countersOn CounterKind.PlusOnePlusOne victim after) 1
    Spec.assertEqWith s "so the 2/1 is a 3/2" (S.powerToughnessOf victim after) (Just (3, 2))
    Spec.assertEqWith s "with 2 of the shield left (CR 615.7)" (shieldsLeft after) [2]
    Spec.assertEqWith s "and unshielded the same ping marks 1 and puts no counter on" (S.damageOf victim control, countersOn CounterKind.PlusOnePlusOne victim control) (Just 1, 0)

-- How many UNBOUNDED shields (CR 615.1 / 615.3) are on the board. `shieldsLeft`
-- above is the wrong reader for these: it reports a countdown amount, and this
-- row has none, so it answers the empty list whether the shield is installed or
-- not.
preventAllRows :: GameState.GameState -> Int
preventAllRows gs =
  let isPreventAll re = case re of
        ReplacementEffect.DamageR (DamageR.MkDamageR _ DamageRewrite.PreventAll _) -> True
        _ -> False
   in length (filter (isPreventAll . ActiveReplacement.effect) (GameState.replacements gs))

-- Attacks with everything and blocks with NOTHING, so an attack's damage reaches
-- the defending player.
attackNoBlock :: Prompt.Prompt r -> r
attackNoBlock p = case p of
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.identityAnswer p

-- Put a board at declare attackers with BOB active and alice defending -- the
-- mirror of testOfFaithSpec's `atCombat`, since a shield over "you" is over the
-- player who activated it and it takes combat damage only when that player is
-- the one being attacked.
bobAttacks :: GameState.GameState -> GameState.GameState
bobAttacks gs =
  gs
    { GameState.activePlayer = S.bob,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      -- CR 703.4h has already happened on this board, so the defending player is
      -- stated rather than derived (S.combatBoardOf's posture).
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice]},
      GameState.remaining =
        Seq.fromList
          [ Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain
          ]
    }

-- Aim every target slot at one creature by FILTERING the offered set rather than
-- building a recipient, so a candidate the card's filter excludes cannot be
-- smuggled back in: an illegal aim leaves the slot empty instead of silently
-- becoming a legal one.
onlyCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
onlyCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature oid) . snd) sets
  _ -> S.identityAnswer p

-- CR 510.2 vs CR 608: a shield that names a KIND. Decorated Griffin ({4}{W}
-- Creature -- Griffin 2/3, flying) prints "{1}{W}: Prevent the next 1 combat
-- damage that would be dealt to you this turn" -- a counted shield (CR 615.7)
-- over a PLAYER, with no CR 615.5 clause, so the kind is the only thing under
-- test. Inkshield below is the same recipient WITH the CR 615.5 clause.
--
-- The discrimination needs both halves and a control each. A group using only
-- combat damage would pass identically on a shield that named no kind at all.
--
-- Numbers all distinct: the ping is 1 and the attack is 5, so alice ends on 19
-- where the kind is respected and 16 where the shield bites, against 20 and 15
-- for the two readings that are wrong.
decoratedGriffinSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
decoratedGriffinSpec s registry = Spec.describe s "Decorated Griffin (CR 510.2)" $ do
  Spec.it s "a combat-only shield leaves noncombat damage alone (CR 608)" $ do
    plains <- S.printingOf s registry "Plains"
    griffin <- S.printingOf s registry "Decorated Griffin"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let base = S.landsInPlay plains 2
        (bird, g1) = S.addPermanent griffin S.alice base
        (pinger, g2) = S.addPermanent sorcerer S.alice g1
        shielded = S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice bird (theAbility griffin) Monad.>> Stack.resolveTop)
        ping g = S.runPure (aimPlayer S.alice) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        -- The CONTROL is the same board with the ability never activated, so
        -- the only difference is the shield.
        control = ping g2
    Spec.assertEqWith s "setup: the activation installed a shield of 1" (shieldsLeft shielded) [1]
    Spec.assertEqWith s "the Sorcerer's noncombat 1 is dealt anyway (CR 608)" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "and the combat-only shield is untouched" (shieldsLeft after) [1]
    Spec.assertEqWith s "which is what the unshielded board does too" (S.lifeOf S.alice control) (Just 19)

-- CR 307.1: hand a player their own precombat main phase, so a sorcery they hold
-- is castable. `bobAttacks` above for the combat half.
inMainPhase :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
inMainPhase pid gs = gs {GameState.activePlayer = pid, GameState.phase = Phase.PrecombatMain}

-- Put one player at an exact life total, so a board can be built where the very
-- next event would carry them past Worship's floor.
atLife :: PlayerId.PlayerId -> Integer -> GameState.GameState -> GameState.GameState
atLife pid n gs = gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}

-- Apply one damage batch under a given interpreter. Top-level rather than a
-- `where` binding because the answer is rank-2 and GHC will not infer it.
settleDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> [DamageEvent.DamageEvent] -> GameState.GameState
settleDamage answer gs batch = S.runPure answer gs (Damage.applyDamage batch)

-- Aim every target slot at one creature. Mending Hands' slot is Pool.AnyTarget
-- (CR 115.4), whose creature members are tagged ToCreature.
aimCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- CR 615.12's prohibition AIMED at one recipient, whose only printing is
-- Whippoorwill ({G} Creature -- Bird: "{G}{G}, {T}: Target creature can't be
-- regenerated this turn. Damage that would be dealt to that creature this turn
-- can't be prevented or dealt instead to another permanent or player. When the
-- creature dies this turn, exile the creature." -- Oracle verified on Scryfall
-- 2026-09-07).
--
-- "That creature" is the recipient the ability's own resolution TARGETED (CR
-- 601.2c / 115.4), so the card writes DamagePattern.boundRecipient -- a slot --
-- and Pawl.Engine.Resolve.Effect bakes it into DamagePattern.whichRecipient as
-- the effect is stored. Every other producer of this effect in data/cards/ says
-- it of its own source (Lava Burst) or of nobody in particular (Spider-Punk),
-- so this is the pool's only board where the clause has to reach one creature
-- and stop.
--
-- The card's third sentence is a CR 603.7 delayed trigger watching the bound
-- creature's death, which the card writes as TriggerCondition.BoundDies over the
-- same "target" slot -- CR 700.4's one destination, where rule 701.66a's
-- earthbend sibling admits exile too. The last three cases below prove both
-- halves and CR 603.7b's duration.
whippoorwillSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
whippoorwillSpec s registry = Spec.describe s "Whippoorwill (CR 615.12)" $ do
  -- CR 603.7 / 700.4, the card's third sentence: "when the creature dies this
  -- turn, exile the creature". Its own fixture rather than shieldedPair's, whose
  -- two Mending Hands shields say nothing about a death.
  --
  -- CR 400.7 mints a fresh id for the graveyard card, so every census below goes
  -- by NAME; Game.zoneMembers indexes by owner (CR 108.3), alice for all three
  -- Pikers.
  let pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveTop gs = S.runPure S.identityAnswer gs Stack.resolveTop
      pikersIn zone gs = length (filter (\oid -> S.soleFaceName oid gs == pikerName) (Game.zoneMembers zone S.alice gs))
      -- alice's Bird, the Piker its ability names, and a second Piker beside it
      -- so a condition that watched ANY death rather than the bound one would be
      -- caught. Both 2/1, so one marked damage kills either.
      deathBoard plains forest pikerPrinting whippoorwill =
        let base = S.landsFor forest S.alice 2 (S.landsInPlay plains 2)
            (bird, g1) = S.addPermanent whippoorwill S.alice base
            (named, g2) = S.addPermanent pikerPrinting S.alice g1
            (bystander, g3) = S.addPermanent pikerPrinting S.alice g2
            aimed = S.runPure (aimCreature named) g3 (Activate.activateAbility S.alice bird (theAbility whippoorwill) Monad.>> Stack.resolveTop)
         in (aimed, named, bystander)
  -- THE PROVING CASE for the delayed half. The named Piker dies and its card is
  -- exiled; the bystander dies on the same board and its card stays in the
  -- graveyard, so the entry is bound to ONE object and not to the death event's
  -- shape.
  Spec.it s "CR 603.7 the creature the resolution named is exiled when it dies, and no other is" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    whippoorwill <- S.printingOf s registry "Whippoorwill"
    let (aimed, named, bystander) = deathBoard plains forest pikerPrinting whippoorwill
        killed = settle (S.markDamage bystander 1 (S.markDamage named 1 aimed))
        after = resolveTop killed
    Spec.assertEqWith s "CR 700.4 the named creature's card is in exile" (pikersIn Zone.Exile after) 1
    Spec.assertEqWith s "and the bystander that died beside it stays in the graveyard" (pikersIn Zone.Graveyard after) 1
    -- The preconditions those two rest on, read AFTER them so neither can absorb
    -- a mutation aimed at the condition.
    Spec.assertEqWith s "setup: the activation armed exactly one delayed ability" (Seq.length (GameState.delayedTriggers aimed)) 1
    Spec.assertEqWith s "setup: both creatures really died" (Game.lookupObject named killed, Game.lookupObject bystander killed) (Nothing, Nothing)
    Spec.assertEqWith s "setup: exactly one trigger reached the stack" (length (GameState.stack killed)) 1
  -- CR 700.4 read literally, which is the whole of why this condition is not
  -- TriggerCondition.BoundDiesOrIsExiled: the same creature removed to exile from
  -- the battlefield fires nothing. Two boards differing in exactly one thing --
  -- the destination -- off the same armed fixture.
  Spec.it s "CR 700.4 the same creature exiled from the battlefield instead fires nothing" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    whippoorwill <- S.printingOf s registry "Whippoorwill"
    let (aimed, named, _) = deathBoard plains forest pikerPrinting whippoorwill
        banished = settle (S.runPure S.identityAnswer aimed (Event.changeZone named Zone.Exile))
        died = settle (S.markDamage named 1 aimed)
    Spec.assertEqWith s "CR 700.4 an exile is not a death, so no trigger reached the stack" (length (GameState.stack banished)) 0
    Spec.assertEqWith s "control: the same creature dying instead puts one there" (length (GameState.stack died)) 1
    -- The precondition, after the two above: the creature really did leave the
    -- battlefield for exile rather than staying put.
    Spec.assertEqWith s "setup: the creature left the battlefield for exile" (Maybe.isNothing (Game.lookupObject named banished), pikersIn Zone.Exile banished) (True, 1)
  -- CR 603.7b's stated duration, and CR 514.2 ending it: "this turn" is the whole
  -- of how long the entry watches, so the same death one turn later exiles
  -- nothing. The discriminating twin of the proving case, one turn on.
  Spec.it s "CR 603.7b the entry is gone by the next turn, so the same death exiles nothing" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    whippoorwill <- S.printingOf s registry "Whippoorwill"
    let (aimed, named, _) = deathBoard plains forest pikerPrinting whippoorwill
        -- Both libraries stocked, so the draw steps this case runs through never
        -- reach CR 704.5b and decide the game before the assertion.
        stock g pid = List.foldl' (\h _ -> snd (S.addLibraryCard plains pid h)) g [1 .. (5 :: Int)]
        bobsTurn = nextTurn quiet (stock (stock aimed S.alice) S.bob)
        after = resolveTop (settle (S.markDamage named 1 bobsTurn))
    Spec.assertEqWith s "CR 514.2 the card stays in the graveyard, the entry having expired" (pikersIn Zone.Graveyard after, pikersIn Zone.Exile after) (1, 0)
    -- The preconditions, after it: the turn handed off and the store really emptied.
    Spec.assertEqWith s "setup: the turn really did hand off" (GameState.turnNumber bobsTurn /= GameState.turnNumber aimed) True
    Spec.assertEqWith s "setup: no delayed entry survived the cleanup" (Seq.length (GameState.delayedTriggers bobsTurn)) 0

-- Run a turn without a fight: nobody attacks and nobody blocks, so the only
-- damage in these cases is the batch the assertion builds.
quiet :: Prompt.Prompt r -> r
quiet p = case p of
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  stepSkipSpec s registry
  fatigueSpec s registry
  mendingHandsSpec s registry
  healingGraceSpec s registry
  auriokReplicaSpec s registry
  packLeaderSpec s registry
  partingWardSpec s registry
  healingGraceReferentSpec s registry
  galvanicBlastReferentSpec s registry
  communalBulwarkReferentSpec s registry
  comeBackWrongSpec s registry
  testOfFaithSpec s registry
  decoratedGriffinSpec s registry
  whippoorwillSpec s registry
