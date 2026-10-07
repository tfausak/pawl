{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.PlayerEffect over effects that forbid casting (CR 601.3): Silence
-- and its conditional forms, Liliana, Null Chamber, Runed Halo, The Stasis
-- Coffin, Conjurer's Ban.
-- Split out of Pawl.PlayerEffectSpec, which keeps the machinery.
module Pawl.CastProhibitionSpec where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Interpreter as Interpreter
import Pawl.PlayerEffectSpec (anySpellId, isCast, silenceAfter, swapAt, threeSeatSilenceBoard)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.VariableChoice as VariableChoice
import qualified Pawl.Types.While as While
import qualified Pawl.Types.Zone as Zone

-- Silence {W} Instant: "Your opponents can't cast spells this turn."
silenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
silenceSpec s registry =
  Spec.describe s "Silence" $ do
    Spec.it s "before Silence resolves, bob may cast his creature" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, pikerId, _, before, _) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertBool s (elem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.bob before)) "offered"

    Spec.it s "CR 514.2 the prohibition ends at cleanup" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
          ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
      Spec.assertEqWith s "nothing stored" (GameState.playerEffects ended) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId VariableChoice.Announced ended)) "bob may cast again"

-- CR 601.2c: the three-seat Cease-Fire board. alice has three Plains and the
-- Cease-Fire; all three seats have two Mountains, a Goblin Piker in hand and two
-- Plains in their library, and carol also holds a Lightning Bolt. The seats are
-- stocked IDENTICALLY on the creature axis on purpose -- the only thing that
-- differs between bob and carol afterwards is which of them the spell targeted --
-- and carol's Bolt is the second axis: same seat, same mana, a noncreature card.
--
-- THREE seats because two collapse "target player" onto "the opponent", which is
-- exactly the reading under test. carol is the far seat.
--
-- The libraries are stocked because the card draws: an empty one would lose alice
-- the game to CR 104.3c before the assertions ran, and the equal stock is what
-- makes the draw's own control (bob's library) honest.
--
-- Loaded fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
ceaseFireBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ceaseFireBoard plains ceaseFire mountain piker lightningBolt =
  let gs0 = Setup.emptyGame S.threePlayers
      repeatedly f n gs = List.foldl' (\g _ -> f g) gs [1 .. n :: Int]
      addLands printing pid = repeatedly (snd . S.addPermanent printing pid)
      stockLibrary pid = repeatedly (snd . S.addLibraryCard plains pid) (2 :: Int)
      gs1 = addLands plains S.alice (3 :: Int) gs0
      gs2 = addLands mountain S.alice (2 :: Int) gs1
      (ceaseFireId, gs3) = S.addHandCard ceaseFire S.alice gs2
      (alicesPiker, gs4) = S.addHandCard piker S.alice gs3
      gs5 = addLands mountain S.bob (2 :: Int) gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      gs7 = addLands mountain S.carol (2 :: Int) gs6
      (carolsPiker, gs8) = S.addHandCard piker S.carol gs7
      (carolsBolt, gs9) = S.addHandCard lightningBolt S.carol gs8
      stocked = stockLibrary S.carol (stockLibrary S.bob (stockLibrary S.alice gs9))
   in ( ceaseFireId,
        alicesPiker,
        bobsPiker,
        carolsPiker,
        carolsBolt,
        -- carol's own main phase, which is when the card is really cast: its
        -- effect lasts "this turn", so aiming it at the player whose turn it is
        -- is what makes it bite at all.
        stocked {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.carol, GameState.priority = Just S.alice}
      )

-- Every target prompt answers with CAROL -- pinned rather than searched, so a
-- broken bake cannot be repaired by an answerer that hunts for a legal option.
ceaseFireAtCarol :: Prompt.Prompt r -> r
ceaseFireAtCarol p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.carol))) sets
  _ -> S.identityAnswer p

-- alice casts Cease-Fire at carol and it resolves.
ceaseFireAfter :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
ceaseFireAfter plains ceaseFire mountain piker lightningBolt =
  let (ceaseFireId, alicesPiker, bobsPiker, carolsPiker, carolsBolt, before) = ceaseFireBoard plains ceaseFire mountain piker lightningBolt
      onStack = S.runPure ceaseFireAtCarol before (S.cast S.alice ceaseFireId)
      after = S.runPure ceaseFireAtCarol onStack Engine.priorityLoop
   in (alicesPiker, bobsPiker, carolsPiker, carolsBolt, before, after)

-- Is a cast of this card offered to this seat, on a board staged as that seat's
-- own main phase? CR 302.1 offers a creature spell only to the active player, so
-- the flip is what keeps the three seats comparable -- the posture the three-seat
-- Silence case above already takes, and nothing else about the board changes.
offersCast :: PlayerId.PlayerId -> ObjectId.ObjectId -> Printing.Printing -> GameState.GameState -> Bool
offersCast pid oid printing gs =
  elem
    (Action.Type.Cast oid (S.printingName printing) Facing.FaceUp)
    (Action.legalActions pid (gs {GameState.activePlayer = pid}))

-- Sphinx's Decree {1}{W} Sorcery: "Each opponent can't cast instant or sorcery
-- spells during that player's next turn." (Oracle checked against Scryfall
-- 2026-09-25.) CR 611.2a's window, one per opponent: each opponent's next turn
-- is a different turn from three seats on, so each is barred on their own turn
-- and not on the other's. Turn order is alice, bob, carol; bob and carol each
-- hold a Lightning Bolt and two Mountains, so a cast is offered whenever nothing
-- bars it.
sphinxsDecreeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
sphinxsDecreeSpec s registry =
  Spec.describe s "Sphinx's Decree" $ do
    Spec.it s "CR 611.2a each opponent is barred during their own next turn and no other" $ do
      plains <- S.printingOf s registry "Plains"
      decree <- S.printingOf s registry "Sphinx's Decree"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (decreeId, bobsBolt, carolsBolt, before) = threeSeatSilenceBoard plains decree mountain bolt
          resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice decreeId)) Engine.priorityLoop
          handoff gs = (S.runPure S.identityAnswer (Expiry.dropAtCleanup gs) Engine.handoffTurn) {GameState.phase = Phase.PrecombatMain}
          bobsTurn = handoff resolved
          carolsTurn = handoff bobsTurn
          offered pid oid gs = elem (Action.Type.Cast oid (S.printingName bolt) Facing.FaceUp) (Action.legalActions pid (gs {GameState.priority = Just pid}))
      Spec.assertBool s (offered S.bob bobsBolt resolved) "the window has not begun on alice's turn, so bob may still cast his Bolt"
      Spec.assertEqWith s "on bob's turn bob is barred and carol is not" (offered S.bob bobsBolt bobsTurn, offered S.carol carolsBolt bobsTurn) (False, True)
      Spec.assertEqWith s "on carol's turn carol is barred and bob is not" (offered S.carol carolsBolt carolsTurn, offered S.bob bobsBolt carolsTurn) (False, True)
      -- Preconditions, ordered behind the gameplay assertions.
      Spec.assertEqWith s "the Decree resolved on alice's turn 1, into a turn each for bob and carol" (fmap GameState.activePlayer [resolved, bobsTurn, carolsTurn], GameState.turnNumber carolsTurn) ([S.alice, S.bob, S.carol], 3)
      Spec.assertEqWith s "one row per opponent, each over that opponent alone" (List.sort (fmap ActivePlayerEffect.scope (GameState.playerEffects resolved))) [AffectedPlayers.Named S.bob, AffectedPlayers.Named S.carol]

-- Cease-Fire {2}{W} Instant: "Target player can't cast creature spells this
-- turn. Draw a card." The first card in the pool to store a player effect on a
-- TARGETED seat.
ceaseFireSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ceaseFireSpec s registry =
  Spec.describe s "CeaseFire" $ do
    -- The positive control both negatives are read against: before the spell
    -- resolves, every seat may cast its Goblin Piker.
    Spec.it s "before Cease-Fire resolves, all three seats may cast their creature" $ do
      plains <- S.printingOf s registry "Plains"
      ceaseFire <- S.printingOf s registry "Cease-Fire"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (alicesPiker, bobsPiker, carolsPiker, _, before, _) = ceaseFireAfter plains ceaseFire mountain piker lightningBolt
      Spec.assertEqWith s "three seats" (length (GameState.turnOrder before)) 3
      Spec.assertBool s (offersCast S.alice alicesPiker piker before) "alice could cast"
      Spec.assertBool s (offersCast S.bob bobsPiker piker before) "bob could cast"
      Spec.assertBool s (offersCast S.carol carolsPiker piker before) "carol could cast"

    -- THE DISCRIMINATOR: the restriction lands on the seat CR 601.2c chose and on
    -- no other. No PlayerScope can say this -- Opponents would stop bob too, and
    -- You would stop alice.
    Spec.it s "CR 601.2c the restriction lands on the targeted seat alone" $ do
      plains <- S.printingOf s registry "Plains"
      ceaseFire <- S.printingOf s registry "Cease-Fire"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (alicesPiker, bobsPiker, carolsPiker, _, _, after) = ceaseFireAfter plains ceaseFire mountain piker lightningBolt
      -- The gameplay reading first, so a mutation is answered by the offers
      -- rather than by the store's shape alone.
      Spec.assertBool s (not (offersCast S.carol carolsPiker piker after)) "carol may not cast her creature"
      Spec.assertBool s (offersCast S.bob bobsPiker piker after) "bob, the untargeted opponent, still may"
      Spec.assertBool s (offersCast S.alice alicesPiker piker after) "and so may alice, who cast it"
      Spec.assertEqWith s "one stored effect" (length (GameState.playerEffects after)) 1
      Spec.assertEqWith
        s
        "stored against the seat itself, not a scope"
        (fmap ActivePlayerEffect.scope (GameState.playerEffects after))
        [AffectedPlayers.Named S.carol]

    -- CR 514.2: "this turn" ends at cleanup, so the restriction has to end with
    -- it. Driven through the cleanup step's own turn-based actions rather than
    -- the priority loop -- the narrowest path that ends the effect.
    Spec.it s "CR 514.2 the restriction ends at cleanup" $ do
      plains <- S.printingOf s registry "Plains"
      ceaseFire <- S.printingOf s registry "Cease-Fire"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (_, _, carolsPiker, _, _, after) = ceaseFireAfter plains ceaseFire mountain piker lightningBolt
          ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
      Spec.assertEqWith s "nothing stored" (GameState.playerEffects ended) []
      Spec.assertBool s (offersCast S.carol carolsPiker piker ended) "carol may cast her creature again"

-- CR 611.2a's board: three seats, and the SEAT COUNT is load-bearing twice
-- over. "Until your next turn" has to pass two other seats before it ends, so a
-- two-player board cannot tell "the next turn" from "your next turn"; and CR
-- 702.11c's opponents are two players, not one.
--
-- alice has one Plains and Blossoming Calm in hand. bob has one Mountain and a
-- Lightning Bolt. carol has nothing -- she is a seat to pass and a rival target,
-- both of which she is by existing. Mana is held EQUAL across every case below,
-- because each one casts the same Bolt off the same untapped Mountain: the only
-- thing that ever differs is whether alice's stored effect is still there.
--
-- Loaded fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
blossomingCalmBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
blossomingCalmBoard plains calm mountain bolt =
  let gs0 = Setup.emptyGame S.threePlayers
      (_, gs1) = S.addPermanent plains S.alice gs0
      (calmId, gs2) = S.addHandCard calm S.alice gs1
      (_, gs3) = S.addPermanent mountain S.bob gs2
      (boltId, gs4) = S.addHandCard bolt S.bob gs3
   in ( calmId,
        boltId,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- alice casts Blossoming Calm and it resolves.
blossomingCalmAfter :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
blossomingCalmAfter calmId before =
  S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice calmId)) Engine.priorityLoop

-- bob casts his Bolt with an answerer that aims at alice whenever the engine
-- offers her, so "alice was never offered" is the only way the damage can land
-- anywhere else -- Pawl.TargetSpec's prefersBob, pointed the other way.
--
-- The phase and priority are restated rather than inherited, so a board that has
-- been handed off two seats is cast on under exactly the conditions the
-- un-handed-off one was.
blossomingCalmBolt :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
blossomingCalmBolt boltId gs =
  let prefersAlice :: Prompt.Prompt r -> r
      prefersAlice p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer S.alice) sets
        _ -> S.identityAnswer p
      staged = gs {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob}
   in S.runPure prefersAlice staged (S.cast S.bob boltId >> Stack.resolveTop)

-- CR 611.2a's turn boundary, as Engine.handoffTurn -- the call every "until your
-- next turn" duration is ended by (Expiry.dropAtTurnOf), and the one a test can
-- make without running whole turns and decking the fixture (CR 104.3c).
blossomingCalmHandoff :: GameState.GameState -> GameState.GameState
blossomingCalmHandoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn

-- Blossoming Calm {W} Instant: "You gain hexproof until your next turn. You gain
-- 2 life." The stored player-effect carrier's turn-relative expiry, end to end:
-- Pawl.Engine.Resolve stamps Expiry.AtTurnOf off the resolution's controller
-- (CR 109.5) and Expiry.dropAtTurnOf ends it at alice's seat, two handoffs later.
--
-- The card's third line is rebound (CR 702.88), which Pawl.CastSpec's Rebound
-- group proves on Staggershock; nothing below reaches it.
blossomingCalmSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blossomingCalmSpec s registry =
  Spec.describe s "Blossoming Calm" $ do
    -- CR 611.1: the resolution stores the effect, and CR 702.11c's player
    -- hexproof takes alice out of the Bolt's candidate set. The life gain is the
    -- second clause of the same spell, and it is asserted for its own sake: it is
    -- what shows the spell RESOLVED rather than fizzling somewhere earlier.
    Spec.it s "CR 702.11c once it resolves, alice has gained 2 and bob's Bolt cannot reach her" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          resolved = blossomingCalmAfter calmId before
          burned = blossomingCalmBolt boltId resolved
      Spec.assertEqWith s "one stored player effect" (length (GameState.playerEffects resolved)) 1
      Spec.assertEqWith s "and it ends at alice's next turn" (fmap ActivePlayerEffect.expiry (GameState.playerEffects resolved)) [Expiry.Type.AtTurnOf S.alice]
      Spec.assertEqWith s "alice gained 2" (S.lifeOf S.alice resolved) (Just 22)
      Spec.assertEqWith s "and takes nothing from the Bolt" (S.lifeOf S.alice burned) (Just 22)
      Spec.assertEqWith s "which landed on bob, the lowest candidate left" (S.lifeOf S.bob burned) (Just 17)

    -- CR 514.2 is the wrong sweep for this duration, and this is where an
    -- UntilEndOfTurn mis-arming would show: the effect has to outlive the
    -- cleanup of the very turn it was cast in.
    Spec.it s "CR 514.2 the hexproof outlives the cleanup of the turn it was cast in" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          swept = Expiry.dropAtCleanup (blossomingCalmAfter calmId before)
          burned = blossomingCalmBolt boltId swept
      Spec.assertEqWith s "still stored" (length (GameState.playerEffects swept)) 1
      Spec.assertEqWith s "and alice still takes nothing" (S.lifeOf S.alice burned) (Just 22)

    -- THE UNIT'S POINT. Two handoffs pass and the effect survives both; the
    -- third begins alice's own turn and ends it. A duration keyed to the next
    -- turn, or to the victim rather than to CR 109.5's "you", would end at the
    -- first handoff -- which is why the assertion is made at every seat rather
    -- than only at the last.
    Spec.it s "CR 611.2a it survives bob's turn and carol's turn, and ends as alice's next turn begins" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          resolved = blossomingCalmAfter calmId before
          bobsTurn = blossomingCalmHandoff resolved
          carolsTurn = blossomingCalmHandoff bobsTurn
          alicesTurn = blossomingCalmHandoff carolsTurn
      Spec.assertEqWith s "bob's turn begins" (GameState.activePlayer bobsTurn) S.bob
      Spec.assertEqWith s "and the effect is still stored" (length (GameState.playerEffects bobsTurn)) 1
      Spec.assertEqWith s "alice takes nothing on bob's turn" (S.lifeOf S.alice (blossomingCalmBolt boltId bobsTurn)) (Just 22)
      Spec.assertEqWith s "carol's turn begins" (GameState.activePlayer carolsTurn) S.carol
      Spec.assertEqWith s "and the effect is still stored" (length (GameState.playerEffects carolsTurn)) 1
      Spec.assertEqWith s "alice takes nothing on carol's turn either" (S.lifeOf S.alice (blossomingCalmBolt boltId carolsTurn)) (Just 22)
      Spec.assertEqWith s "alice's own next turn begins" (GameState.activePlayer alicesTurn) S.alice
      Spec.assertEqWith s "and the effect is gone" (GameState.playerEffects alicesTurn) []
      Spec.assertEqWith s "so the same Bolt now reaches her" (S.lifeOf S.alice (blossomingCalmBolt boltId alicesTurn)) (Just 19)

-- CR 611.2b's board, and the SWAMP is the only thing about it that varies. alice
-- has an Island (which pays for the spell) and, on the holding board, a Swamp
-- (which the condition counts); bob and carol each have two Mountains and a
-- Goblin Piker, so both opponents can genuinely cast before the spell resolves.
--
-- Paying and gating are deliberately split across two lands: with one land doing
-- both, "the condition holds" and "she had mana" would be the same fact, and the
-- never-starts case below could not hold mana equal while removing the Swamp.
--
-- The Swamp's id is S.noSource on the board that has no Swamp: the one case built
-- that way never names it, and there is nothing on the battlefield for it to
-- collide with.
--
-- Loaded fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
conditionalSilenceBoard :: Bool -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
conditionalSilenceBoard withSwamp island swamp hush mountain piker =
  let gs0 = Setup.emptyGame S.threePlayers
      (_, gs1) = S.addPermanent island S.alice gs0
      (swampId, gs2) =
        if withSwamp
          then S.addPermanent swamp S.alice gs1
          else (S.noSource, gs1)
      (hushId, gs3) = S.addHandCard hush S.alice gs2
      (_, gs4) = S.addPermanent mountain S.bob gs3
      (_, gs5) = S.addPermanent mountain S.bob gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      (_, gs7) = S.addPermanent mountain S.carol gs6
      (_, gs8) = S.addPermanent mountain S.carol gs7
      (carolsPiker, gs9) = S.addHandCard piker S.carol gs8
   in ( hushId,
        swampId,
        bobsPiker,
        carolsPiker,
        gs9
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- alice casts it and it resolves.
conditionalSilenceAfter :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
conditionalSilenceAfter hushId before =
  S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice hushId)) Engine.priorityLoop

-- Goblin Piker is a creature, so CR 302.1 offers it only to the ACTIVE player.
-- The board is alice's own main phase, so each opponent's cast is read off a copy
-- with activePlayer flipped to them and nothing else changed -- threeSeatSilenceBoard's
-- device.
conditionalSilenceCasts :: PlayerId.PlayerId -> GameState.GameState -> [Action.Type.Action]
conditionalSilenceCasts who gs = filter isCast (Action.legalActions who (gs {GameState.activePlayer = who}))

-- SYNTHETIC. "Synthetic Conditional Silence" {U} Instant: "For as long as you
-- control a Swamp, your opponents can't cast spells." CR 611.2b's duration on the
-- stored player-effect carrier (Pawl.Types.ActivePlayerEffect), which no printed
-- card reaches: a "for as long as" effect that changes what a PLAYER may do is
-- printed as a static ability on a permanent, and that rides the other carrier
-- (Pawl.Types.PlayerStaticAbility) -- Grand Abolisher, Rule of Law and Damping
-- Engine are all statics. Every printed spell or ability that stores a
-- player-axis effect states a TURN-relative duration instead (Silence, Blossoming
-- Calm, Hope of Ghirapur, Academic Probation). Nothing in CR 611.2b confines the
-- duration to one carrier, so the card is legitimate and only unprinted.
conditionalSilenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
conditionalSilenceSpec s registry =
  Spec.describe s "Synthetic Conditional Silence" $ do
    -- THE CONTROL TWIN: both opponents really could cast, so a later empty list
    -- is the prohibition and not an unaffordable Piker.
    Spec.it s "before it resolves, both opponents may cast" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, bobsPiker, carolsPiker, before) = conditionalSilenceBoard True island swamp hush mountain piker
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob before)) "bob is offered his Piker"
      Spec.assertBool s (elem (Action.Type.Cast carolsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.carol before)) "and carol hers"

    -- CR 611.2b: the duration began, so the effect is stored -- keyed to CR
    -- 109.5's "you", which Expiry.arm bakes in because the sweep that re-reads
    -- the condition has no resolution left to read a controller off.
    Spec.it s "CR 611.2b it is stored while the condition holds, and stops both opponents" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, _, _, _, before) = conditionalSilenceBoard True island swamp hush mountain piker
          resolved = conditionalSilenceAfter hushId before
      case fmap ActivePlayerEffect.expiry (GameState.playerEffects resolved) of
        [Expiry.Type.While (While.MkWhile who _)] -> Spec.assertEqWith s "the duration is keyed to its controller" who S.alice
        other -> Spec.assertFailure s ("expected one conditional player effect, got " <> show other)
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId VariableChoice.Announced resolved) "bob is prohibited"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.carol anySpellId VariableChoice.Announced resolved) "carol is prohibited too"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId VariableChoice.Announced resolved)) "alice is not"
      Spec.assertEqWith s "and nothing is offered to either" (conditionalSilenceCasts S.bob resolved <> conditionalSilenceCasts S.carol resolved) []

    -- THE POSITIVE HALF of the sweep. Without it, "deletes when the condition
    -- fails" is indistinguishable from "deletes at the first settle".
    Spec.it s "CR 611.2b a sweep with the Swamp still there changes nothing" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, _, _, _, before) = conditionalSilenceBoard True island swamp hush mountain piker
          resolved = conditionalSilenceAfter hushId before
          (changed, swept) = Engine.runGamePure S.identityAnswer resolved Expiry.sweepConditional
      Spec.assertBool s (not changed) "the sweep reports no change"
      Spec.assertEqWith s "still stored" (length (GameState.playerEffects swept)) 1
      Spec.assertEqWith s "and bob is still stopped" (conditionalSilenceCasts S.bob swept) []

    -- CR 611.2b's first sentence: a duration that never STARTS means the effect
    -- does nothing at all. The board differs from the holding one by the Swamp
    -- alone -- the Island that pays for the spell is on both -- so this is the
    -- Nothing arm of Expiry.arm and not an unaffordable cast.
    Spec.it s "CR 611.2b with no Swamp the duration never starts and nothing is stored" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, _, bobsPiker, _, before) = conditionalSilenceBoard False island swamp hush mountain piker
          -- Stack.resolveTop and NOT the priority loop the other cases use. A
          -- settle runs Expiry.sweepConditional, which deletes an effect whose
          -- condition is already false -- so a loop cannot tell "the duration
          -- never started" from "it started and was swept an instant later", and
          -- an arm that stored the effect unconditionally would leave this case
          -- green. The bare resolution can tell them apart.
          resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice hushId)) Stack.resolveTop
          settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
      Spec.assertBool s (notElem hushId (GameState.stack resolved)) "the spell really did resolve"
      Spec.assertEqWith s "nothing stored" (GameState.playerEffects resolved) []
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob settled)) "so bob may cast"

-- Aims a text changer's one target slot at `oid` -- the SpellsAndPermanents
-- pool's recipient shape -- and answers the basic-land-type swap with
-- (from, to). The offered set is FILTERED rather than rebuilt, so CR 608.2b's
-- re-read at resolution sees the recipient the engine itself offered rather than
-- a hand-built one that merely looks the same.
hackSpellAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
hackSpellAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== Recipient.ToObject oid) candidates) sets
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- conditionalSilenceBoard's no-Swamp shape, plus the two things a Magical Hack
-- needs: a SECOND Island (two {U} spells are cast, so two lands pay) and the
-- Hack in hand. alice controls no Swamp on either board here, so the
-- Silence's printed "for as long as you control a Swamp" can never start and the
-- Islands are the only thing the hacked word can count.
--
-- bob and carol each hold a Goblin Piker over two Mountains, so both opponents
-- genuinely could cast -- an empty action list later is the prohibition and not
-- an unaffordable Piker.
--
-- Returns the Silence, the Hack, alice's two Islands, bob's Piker and carol's.
hackedSilenceBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hackedSilenceBoard island hush magicalHack mountain piker =
  let gs0 = Setup.emptyGame S.threePlayers
      (firstIsland, gs1) = S.addPermanent island S.alice gs0
      (secondIsland, gs2) = S.addPermanent island S.alice gs1
      (hushId, gs3) = S.addHandCard hush S.alice gs2
      (hackId, gs4) = S.addHandCard magicalHack S.alice gs3
      (_, gs5) = S.addPermanent mountain S.bob gs4
      (_, gs6) = S.addPermanent mountain S.bob gs5
      (bobsPiker, gs7) = S.addHandCard piker S.bob gs6
      (_, gs8) = S.addPermanent mountain S.carol gs7
      (_, gs9) = S.addPermanent mountain S.carol gs8
      (carolsPiker, gs10) = S.addHandCard piker S.carol gs9
   in ( hushId,
        hackId,
        firstIsland,
        secondIsland,
        bobsPiker,
        carolsPiker,
        gs10
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- alice casts the Silence; when `hack`, she then casts the Magical Hack at the
-- Silence SPELL on the stack and lets it resolve, swapping Swamp -> Island; then
-- the Silence itself resolves. The Hack is cast SECOND so it resolves first and
-- the Silence resolves already rewritten -- theftChain's ordering.
--
-- Stack.resolveTop and not the priority loop, for the reason
-- conditionalSilenceSpec's never-starts case gives: a settle runs
-- Expiry.sweepConditional, which deletes an effect whose condition is already
-- false, so a loop cannot tell "the duration never started" from "it started and
-- was swept an instant later".
hackedSilenceAfter :: Bool -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
hackedSilenceAfter hack hushId hackId before =
  let onStack = S.runPure S.identityAnswer before (S.cast S.alice hushId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> S.noSource
      hacked =
        if hack
          then S.runPure (hackSpellAt spellId Subtype.Swamp Subtype.Island) onStack $ do
            S.cast S.alice hackId
            Stack.resolveTop
          else onStack
   in S.runPure S.identityAnswer hacked Stack.resolveTop

-- CR 612.1 reaching the DURATION a spell stores over players. The restriction's
-- own Filter is the other half a word swap can touch, and Liliana, Untouched by
-- Death's group below proves it; the players axis between them is a PlayerScope
-- or a SlotName, neither of which is a word.
--
-- The printed carrier already had this -- Edgewalker under a Magical Hack, above
-- -- so what is new here is the STORED one: Synthetic Conditional Silence hacked
-- while it sits on the stack.
hackedSilenceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
hackedSilenceSpec s registry =
  Spec.describe s "Synthetic Conditional Silence" $ do
    -- THE CONTROL TWIN, differing from the case below in the Hack alone: it sits
    -- unspent in alice's hand and her second Island stays untapped. Without a
    -- Swamp the printed duration never starts, so nothing is stored.
    Spec.it s "CR 611.2b unhacked, with no Swamp the duration never starts" $ do
      island <- S.printingOf s registry "Island"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      magicalHack <- S.printingOf s registry "Magical Hack"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, hackId, _, _, bobsPiker, carolsPiker, before) = hackedSilenceBoard island hush magicalHack mountain piker
          after = hackedSilenceAfter False hushId hackId before
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob after)) "bob is still offered his Piker"
      Spec.assertBool s (elem (Action.Type.Cast carolsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.carol after)) "and carol hers"
      Spec.assertEqWith s "the Silence really did resolve" (length (GameState.stack after)) 0
      Spec.assertEqWith s "and nothing is stored" (GameState.playerEffects after) []

-- The one activated ability at index `n` of what the PROJECTION hands out for
-- `oid` -- not Face.activatedAbilities, which is the printed list a text change
-- has not reached. Projection.abilitiesOf is the list Activate itself offers
-- from, so this is the same ability a player would be given.
projectedAbility :: Int -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
projectedAbility n oid gs = case drop n (Projection.abilitiesOf oid gs) of
  ability : _ -> Just ability
  [] -> Nothing

-- alice controls Liliana with four loyalty counters, one untapped Island (the
-- {U} the changer costs) and two untapped Swamps; she holds an Artificial
-- Evolution. Her graveyard holds a Whipstitched Zombie ({1}{B} Creature --
-- Zombie 2/2) and a Cabal Evangel ({1}{B} Creature -- Human Cleric 2/2). The two
-- graveyard cards cost the SAME, so no assertion below can turn on mana, and
-- they differ in the subtype word alone -- which is the word the -3 names.
--
-- Two Swamps rather than three: the Island pays the {U} on the hacked board and
-- goes unspent on the control, so both boards can still afford exactly one
-- {1}{B} cast out of the graveyard.
--
-- Returns Liliana, the changer, the Zombie card, the Cleric card and the board.
lilianaBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
lilianaBoard island swamp liliana evolution zombie evangel =
  let (_, g1) = S.addPermanent island S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addPermanent swamp S.alice g1
      (_, g3) = S.addPermanent swamp S.alice g2
      (lilianaId, g4) = S.addPermanent liliana S.alice g3
      (evolutionId, g5) = S.addHandCard evolution S.alice g4
      (zombieId, g6) = S.addGraveyardCard zombie S.alice g5
      (evangelId, g7) = S.addGraveyardCard evangel S.alice g6
   in ( lilianaId,
        evolutionId,
        zombieId,
        evangelId,
        (S.addCounter CounterKind.Loyalty 4 lilianaId g7)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- alice casts the Artificial Evolution at Liliana THE PERMANENT and lets it
-- resolve. Aimed at the permanent rather than at a spell because Liliana is
-- already on the battlefield -- Pawl.ActivateSpec's Tidal Warrior chain is the
-- same road, and the reason the swap is visible to an ability activated
-- afterwards is that the ability is enumerated off the projected permanent.
hackLiliana :: ObjectId.ObjectId -> ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> GameState.GameState -> GameState.GameState
hackLiliana lilianaId evolutionId from to before =
  S.runPure (swapAt lilianaId from to) before $ do
    S.cast S.alice evolutionId
    Stack.resolveTop

-- alice activates the loyalty ability at index `n` of what the projection hands
-- out and resolves it. A board where Liliana has no such ability is returned
-- untouched, which every case below catches by asserting on what the resolution
-- did rather than only on what it did not.
activateLoyalty :: (forall r. Prompt.Prompt r -> r) -> Int -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateLoyalty answer n lilianaId gs = case projectedAbility n lilianaId gs of
  Nothing -> gs
  Just ability ->
    S.runPure answer gs $ do
      Activate.activateAbility S.alice lilianaId ability
      Stack.resolveTop

-- Liliana, Untouched by Death {2}{B}{B} Legendary Planeswalker -- Liliana,
-- loyalty 4 (Oracle text checked against Scryfall, 2026-08-27):
--   +1: Mill three cards. If at least one Zombie card is milled this way, each
--       opponent loses 2 life and you gain 2 life.
--   -2: Target creature gets -X/-X until end of turn, where X is the number of
--       Zombies you control.
--   -3: You may cast Zombie spells from your graveyard this turn.
--
-- THE UNIT'S POINT is the -3 under an Artificial Evolution: CR 612.1's word swap
-- has to reach the Filter inside the restriction a RESOLUTION stores over
-- players, which is the half of Effect.AffectPlayers the duration case above
-- leaves. The board is an ACTIVATE-chain rather than a cast-chain: Scryfall
-- `oracle:/cast [A-Z][a-z]+ spells/ -t:instant -t:sorcery` and
-- `oracle:/(cast|costs?|counter)[^.]*this turn/ -t:instant -t:sorcery`, read
-- 2026-08-27, turned up no instant or sorcery naming a subtype in a player
-- restriction -- Cherished Hatchling's dies-trigger is the nearest other
-- producer, and it is a permanent's too. Both reach Projection.rewriteEffect by
-- the same road, through Projection.abilitiesOf.
--
-- The +1 comes with it because it is the card's other subtype word, and because
-- it is the first card in `data/cards/` to write a MillTally at all: its "if at
-- least one" is a clause condition comparing Quantity.InSlot against a literal,
-- and the whole tally-then-gate road had no producer before it.
lilianaSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
lilianaSpec s registry =
  let board = do
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        liliana <- S.printingOf s registry "Liliana, Untouched by Death"
        evolution <- S.printingOf s registry "Artificial Evolution"
        zombie <- S.printingOf s registry "Whipstitched Zombie"
        evangel <- S.printingOf s registry "Cabal Evangel"
        pure (lilianaBoard island swamp liliana evolution zombie evangel)
   in Spec.describe s "LilianaUntouchedByDeath" $ do
        -- THE UNIT'S POINT. The same board with Zombie swapped for Cleric on
        -- Liliana herself. The gameplay assertions lead, and they lead in BOTH
        -- directions: an arm that dropped the descent would leave the restriction
        -- naming Zombie, so the Zombie would still be castable and the Cleric
        -- would not -- which is exactly the case above. Two graveyard cards of
        -- the same cost are what separates "rewrote the word" from "dropped the
        -- restriction", since dropping it would make both castable.
        Spec.it s "CR 612.1/612.2 an Artificial Evolution on Liliana moves the -3 onto the new word" $ do
          (lilianaId, evolutionId, zombieId, evangelId, before) <- board
          let after = activateLoyalty S.identityAnswer 2 lilianaId (hackLiliana lilianaId evolutionId Subtype.Zombie Subtype.Cleric before)
          Spec.assertBool s (S.castable S.alice evangelId after) "the Cleric is castable out of the graveyard"
          Spec.assertBool s (not (S.castable S.alice zombieId after)) "and the Zombie no longer is"
          Spec.assertBool s (any (S.isCastOf evangelId) (Action.legalActions S.alice after)) "the Cleric is offered"
          Spec.assertBool s (not (any (S.isCastOf zombieId) (Action.legalActions S.alice after))) "while the Zombie is not"
          Spec.assertBool s (PlayerEffect.mayCastFrom S.alice Zone.Graveyard evangelId after) "the typed question agrees"
          Spec.assertEqWith s "and exactly one restriction is stored" (length (GameState.playerEffects after)) 1

-- Loaded fresh inside each case that needs it -- equivalent because loading
-- is deterministic and cached (batch-recipe.md).
matchesObjectBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
matchesObjectBoard lightningBolt piker =
  let base = Setup.emptyGame S.bothPlayers
      (bolt, withBolt) = S.spellOnStack lightningBolt S.alice base
      (pikerId, gs) = S.spellOnStack piker S.alice withBolt
   in (bolt, pikerId, gs)

-- The spell-match half of the cost-adjustment axis, now expressed as a Filter
-- over the PROJECTED view (CR 613.1d layer 4 for a card type, CR 613.1e layer 5
-- for a colour) rather than the retired SpellCriterion. A noncreature spell is
-- Filter.Not (Filter.HasCardType Creature); a coloured spell is Filter.HasColor.
matchesObjectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
matchesObjectSpec s registry =
  Spec.describe s "matchesObject" $ do
    let noncreature = Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)

    Spec.it s "CR 613.1d Thalia's noncreature criterion admits an instant" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) noncreature bolt gs) "Lightning Bolt is a noncreature spell"

    Spec.it s "CR 613.1d a creature spell fails the noncreature criterion" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) noncreature pikerId gs)) "Goblin Piker is a creature spell"

    Spec.it s "CR 613.1e a colour criterion admits a matching-colour spell" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) (Filter.Type.HasColor Color.Red) bolt gs) "Lightning Bolt is red"

    Spec.it s "CR 613.1e a colour criterion rejects a non-matching colour" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObjectFrom (PlayerEffect.liveSource Nothing) (Filter.Type.HasColor Color.Blue) bolt gs)) "Lightning Bolt is not blue"

-- Null Chamber {3}{W} World Enchantment: "As this enchantment enters, you and an
-- opponent each choose a card name other than a basic land card name. Spells
-- with the chosen names can't be cast and lands with the chosen names can't be
-- played."
--
-- alice has eight untapped Plains and four Mountains (mana is never the reason a
-- cast is unavailable, before or after the Chamber's own {3}{W} is paid) and the
-- Chamber in hand, in her own precombat main phase with an empty stack.
nullChamberBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
nullChamberBoard plains mountain nullChamber =
  let addMountain board _ = snd (S.addPermanent mountain S.alice board)
      lands = List.foldl' addMountain (S.landsInPlay plains 8) [1 .. 4 :: Int]
      (gs, oid) = S.handOne nullChamber lands
   in (oid, gs)

-- CR 201.4 answered PER CHOOSER, which is the whole of what makes Null Chamber
-- worth testing: `pick` is asked WHO is choosing, so a case can put the
-- controller's name and the opponent's on different cards. `opponent` settles
-- the card's other open choice -- which opponent is asked at all -- and is never
-- reached at two seats. Everything else is the shared interpreter.
chamberAnswer :: PlayerId.PlayerId -> (PlayerId.PlayerId -> CardName.CardName) -> Prompt.Prompt r -> r
chamberAnswer opponent pick p = case p of
  Prompt.ChooseCardName _ chooser _ _ _ -> pick chooser
  Prompt.ChooseOpponent {} -> opponent
  _ -> S.identityAnswer p

-- chamberAnswer, also RECORDING each name ask as the (chooser, restriction) pair
-- it arrived as. Both halves are invisible from the finished board:
-- Object.chosenNames is a set and has forgotten CR 101.4's order, and CR 201.4a's
-- restriction is never written to the board at all. Reading the prompt is the
-- only way to see either.
recordingChamberAnswer ::
  PlayerId.PlayerId ->
  (PlayerId.PlayerId -> CardName.CardName) ->
  Prompt.Prompt r ->
  State.State [(PlayerId.PlayerId, Filter.Type.Filter Keyword.Keyword)] r
recordingChamberAnswer opponent pick p = case p of
  Prompt.ChooseCardName _ chooser _ restriction _ -> do
    State.modify' (<> [(chooser, restriction)])
    pure (pick chooser)
  _ -> pure (chamberAnswer opponent pick p)

-- chamberAnswer, taking each name off a QUEUE rather than off a function of the
-- chooser. CR 201.4 refuses an illegal name and the same chooser is asked again,
-- so the two cases below need an answerer whose second answer differs from its
-- first, which a pure Prompt r -> r cannot be -- State-threaded, as
-- Pawl.CopySpec's countingAnswer is.
--
-- Over Asked, because Pawl.Interpreter.policingCardNames wraps the primitive
-- seam (Pawl.Engine.Engine.runGameAsked); nothing here reads the tag.
--
-- An exhausted queue answers `fallback` rather than looping or failing: a legal
-- name neither case expects, so drawing past the end reddens the chosenNames
-- assertion instead of hanging the re-ask.
queuedChamberAnswer ::
  (Monad m) =>
  PlayerId.PlayerId ->
  CardName.CardName ->
  Asked.Asked r ->
  State.StateT [CardName.CardName] m r
queuedChamberAnswer opponent fallback asked = case Asked.prompt asked of
  Prompt.ChooseCardName {} -> do
    queue <- State.get
    case queue of
      [] -> pure fallback
      name : rest -> do
        State.put rest
        pure name
  p -> pure (chamberAnswer opponent (const fallback) p)

-- castChamber, with the name answers coming off `queue` through
-- Pawl.Interpreter.policingCardNames -- the wrapper an interpreter installs over
-- its own answerer. Hands back the finished board and what the queue has LEFT,
-- which is how the cases below count the asks.
--
-- The registry is lifted into the answerer's own monad: Registry.Registry is
-- parameterized over the monad a lookup works in exactly so that a caller can.
policedChamber ::
  (Monad m) =>
  Registry.Registry m ->
  PlayerId.PlayerId ->
  CardName.CardName ->
  [CardName.CardName] ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  m (GameState.GameState, [CardName.CardName])
policedChamber registry opponent fallback queue gs oid = do
  let lifted = Registry.MkRegistry {Registry.fetchCard = Trans.lift . Registry.fetchCard registry, Registry.cards = Trans.lift (Registry.cards registry)}
      play = Engine.runGameAsked (Interpreter.policingCardNames lifted (queuedChamberAnswer opponent fallback)) gs (S.cast S.alice oid >> Stack.resolveTop)
  ((_, after), left) <- State.runStateT play queue
  pure (after, left)

-- CR 201.4a's restriction as Null Chamber prints it: "other than a basic land
-- card name", which is a supertype and a card type together (CR 205.4a: a basic
-- land card is the one carrying both).
nonBasicLandName :: Filter.Type.Filter Keyword.Keyword
nonBasicLandName = Filter.Type.Not (Filter.Type.And [Filter.Type.HasSupertype Supertype.Basic, Filter.Type.HasCardType CardType.Land])

-- Goblin Piker's SLUG -- the second spelling Pawl.Registry.named's haddock says
-- fetches the same card, Pawl.Registry.slugFor mapping the card's own name onto
-- it. A slug is not what CR 201.4's reference holds, and the gap between the two
-- lookups is what Pawl.Interpreter.legalCardName's exact face-name comparison
-- closes.
pikerSlug :: CardName.CardName
pikerSlug = CardName.MkCardName (Text.pack "goblin-piker")

-- A name CR 201.4's reference does not have, which the refusal case needs one of.
-- Scryfall !"No Such Card", 2026-09-05, no hit; `data/cards/` has no file for it
-- either, and Pawl.Registry is the reference Pawl.Interpreter.legalCardName reads.
noSuchCard :: CardName.CardName
noSuchCard = CardName.MkCardName (Text.pack "No Such Card")

-- Cast the Chamber and let it resolve, answering both name choices.
--
-- CAST rather than S.addPermanent, because the choice happens only on the entry
-- path (Event.runEntry): a Chamber placed straight onto the battlefield
-- has an empty chosenNames and prohibits nothing.
castChamber :: PlayerId.PlayerId -> (PlayerId.PlayerId -> CardName.CardName) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castChamber opponent pick gs oid =
  let answer :: Prompt.Prompt r -> r
      answer = chamberAnswer opponent pick
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice oid))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- The one object that reached the battlefield between two states -- the Chamber
-- itself, whose id the cast never handed back (CR 400.7 mints a new one).
enteredOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
enteredOne before after = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
  [oid] -> Just oid
  _ -> Nothing

nullChamberSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nullChamberSpec s registry =
  Spec.describe s "NullChamber" $ do
    -- CR 101.4: "If multiple players would make choices . . . at the same time,
    -- the active player . . . makes any choices required, then the next player
    -- in turn order". Both names are chosen as one event, so the order is the
    -- rule's.
    --
    -- NOT yet a discriminating test of CR 101.4 against "the controller first":
    -- the only way a permanent enters in this pool is its controller casting it,
    -- and a sorcery-speed enchantment is cast on its controller's own turn, so
    -- the two orders name the same player. A card that put a permanent onto the
    -- battlefield under another player's control would separate them.
    Spec.it s "CR 101.4 the active player is asked to name a card first" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName cancel
          asked =
            State.execState
              (Engine.runGame (recordingChamberAnswer S.bob picks) board (S.cast S.alice oid >> Stack.resolveTop))
              []
      Spec.assertEqWith s "alice is active" (GameState.activePlayer board) S.alice
      Spec.assertEqWith s "alice names first, then bob" (fmap fst asked) [S.alice, S.bob]

    -- CR 101.4b: a pair of casts differing only in alice's name. bob names
    -- whatever his prompt says alice named, and Cancel when it says nothing, so
    -- the Chamber holds one name exactly when bob was told hers.
    Spec.it s "CR 101.4b the later chooser knows the name the earlier one chose" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      giant <- S.printingOf s registry "Hill Giant"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          echoing :: CardName.CardName -> Prompt.Prompt r -> r
          echoing hers p = case p of
            Prompt.ChooseCardName _ chooser _ _ earlier
              | chooser == S.alice -> hers
              | otherwise -> maybe (S.printingName cancel) snd (List.find ((== S.alice) . fst) earlier)
            _ -> chamberAnswer S.bob (const (S.printingName cancel)) p
          run hers =
            let cast = snd (Engine.runGamePure (echoing hers) board (S.cast S.alice oid))
                after = snd (Engine.runGamePure (echoing hers) cast Stack.resolveTop)
             in fmap Object.chosenNames (enteredOne board after >>= \chamber -> Game.lookupObject chamber after)
      Spec.assertEqWith s "CR 101.4b told alice named Goblin Piker, bob named it too" (run (S.printingName piker)) (Just (Set.singleton (S.printingName piker)))
      Spec.assertEqWith s "CR 101.4b told alice named Hill Giant, bob named it too" (run (S.printingName giant)) (Just (Set.singleton (S.printingName giant)))

    -- CR 201.4: "the player must choose the name of a card in the Oracle card
    -- reference." Pawl.Registry is that reference, and it sits on the far side of
    -- Pawl.Engine.Engine.runGameAsked, so the refusal is
    -- Pawl.Interpreter.policingCardNames: an answer no card answers to is not
    -- recorded and the same chooser is asked again.
    --
    -- THE FALSIFIER is the queue's length. Alice is asked twice and bob once, so
    -- an unpoliced answerer records the name no card has AND leaves bob holding
    -- alice's second answer -- two names, both wrong, and one answer unconsumed.
    Spec.it s "CR 201.4 a name no card has is refused and the chooser is asked again" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          queue = [noSuchCard, S.printingName piker, S.printingName cancel]
      (after, left) <- policedChamber registry S.bob (S.printingName lightningBolt) queue board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber -> do
          Spec.assertEqWith
            s
            "the two legal names, and not the name no card has"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName cancel])
          Spec.assertEqWith s "every queued answer was drawn, so alice was asked twice" left []

    -- CR 201.4's OTHER refusal, which the case above cannot reach: a string the
    -- registry answers to that is still not a card's name. The file registry
    -- looks up a slug, so "goblin-piker" fetches the Goblin Piker; rule 201.4
    -- asks for the name of a card, and Pawl.Engine.Filter's HasChosenName
    -- compares names exactly, so admitting the slug would write a name into
    -- Object.chosenNames that prohibits nothing.
    --
    -- THE DISCRIMINATOR is the last assertion: without it this case passes for
    -- the case above's reason -- a name the registry cannot resolve at all.
    Spec.it s "CR 201.4 a slug the registry answers to is not a card's name" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          queue = [pikerSlug, S.printingName piker, S.printingName cancel]
      fetched <- Registry.fetchCard registry pikerSlug
      (after, left) <- policedChamber registry S.bob (S.printingName lightningBolt) queue board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber -> do
          Spec.assertEqWith
            s
            "the card's own name, and not the slug that fetches it"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName cancel])
          Spec.assertEqWith s "every queued answer was drawn, so alice was asked twice" left []
      Spec.assertBool s (Maybe.isJust fetched) "the registry does answer to the slug"

    -- CR 201.4a's own half, which the case above cannot reach: Island is a name
    -- the reference HAS, and it is refused only because Null Chamber's
    -- restriction forbids a basic land card name. The two legalCardName
    -- assertions are what keep the rules apart -- without them a check that
    -- resolved no name at all would pass this case too -- and they sit LAST so
    -- that a mutation reaches the board assertion first.
    Spec.it s "CR 201.4a a real card the restriction forbids is refused and the chooser is asked again" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          queue = [S.printingName island, S.printingName piker, S.printingName cancel]
      unrestricted <- Interpreter.legalCardName registry board S.alice (Filter.Type.And []) (S.printingName island)
      restricted <- Interpreter.legalCardName registry board S.alice nonBasicLandName (S.printingName island)
      (after, left) <- policedChamber registry S.bob (S.printingName lightningBolt) queue board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber -> do
          Spec.assertEqWith
            s
            "the two legal names, and not the basic land the card forbids"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName cancel])
          Spec.assertEqWith s "every queued answer was drawn, so alice was asked twice" left []
      Spec.assertBool s unrestricted "Island is a name the reference has"
      Spec.assertBool s (not restricted) "and the restriction is the only thing refusing it"

    -- CR 709.3a / 709.3b: only the half being cast is asked about, so naming
    -- "Wax" stops Wax and leaves Wane castable. The prohibition reads the
    -- proposal's view (Filter.HasChosenName), where CR 709.4a's combined view in
    -- the hand would carry both names and stop both halves. A Forest, a Piker for
    -- Wax to target and the Chamber itself for Wane keep either half castable.
    Spec.it s "CR 709.3a naming one half of a split card stops that half and not the other" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      forest <- S.printingOf s registry "Forest"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      waxWane <- S.printingOf s registry "Wax"
      cancel <- S.printingOf s registry "Cancel"
      let wax = CardName.MkCardName (Text.pack "Wax")
          wane = CardName.MkCardName (Text.pack "Wane")
          (oid, board) = nullChamberBoard plains mountain nullChamber
          withSplit aliceName =
            let after = castChamber S.bob (\pid -> if pid == S.alice then aliceName else S.printingName cancel) board oid
                (_, withForest) = S.addPermanent forest S.alice after
                (_, withPiker) = S.addPermanent piker S.alice withForest
             in S.addHandCard waxWane S.alice withPiker
          (splitId, gs) = withSplit wax
          offered = Action.legalActions S.alice gs
          -- The control differs in alice's name alone: the Piker, which she is
          -- not holding.
          (controlId, control) = withSplit (S.printingName piker)
      Spec.assertBool s (notElem (Action.Type.Cast splitId wax Facing.FaceUp) offered) "the named Wax half is not offered"
      Spec.assertBool s (elem (Action.Type.Cast splitId wane Facing.FaceUp) offered) "the Wane half still is"
      Spec.assertBool s (elem (Action.Type.Cast controlId wax Facing.FaceUp) (Action.legalActions S.alice control)) "and Wax is offered when nobody named it"

    -- CR 604.2: the effect is re-derived from the battlefield on every read, so
    -- destroying the Chamber lifts both halves with nothing to unwind.
    --
    -- The names go with it too -- CR 400.7 mints a new incarnation in the
    -- graveyard and Event.changeZone empties its chosenNames, CR 608.2h's record
    -- of them staying behind under the OLD id -- but that is a separate fact and
    -- NOT what this case observes: `applying` walks only the battlefield, so both
    -- prohibitions would lift here even if the names had survived the move.
    Spec.it s "CR 604.2 destroying the Chamber lifts both prohibitions" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      ashBarrens <- S.printingOf s registry "Ash Barrens"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName ashBarrens
          after = castChamber S.bob picks board oid
          (pikerId, withPiker) = S.addHandCard piker S.alice after
          (barrensId, gs) = S.addHandCard ashBarrens S.alice withPiker
      case enteredOne board after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber -> do
          let gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [chamber])
          Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice pikerId VariableChoice.Announced gs) "prohibited while it stands"
          Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice pikerId VariableChoice.Announced gone)) "not prohibited once it is gone"
          Spec.assertBool s (elem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.alice gone)) "and the cast is offered again"
          Spec.assertBool s (elem (barrensId, Nothing) (Action.playableLands S.alice gone)) "and the land may be played again"

-- `active` is the active player in their own precombat main phase with an empty
-- stack (CR 305.1's window) holding FIVE Mountains, while `grantors` are already
-- on the battlefield under ALICE's control.
--
-- Five is deliberately more than any case below plays. Every "and no more"
-- assertion is otherwise satisfiable by an empty hand, which is the trap this
-- whole group is built to avoid: each case checks the leftover hand as well as
-- the lands that landed.
--
-- The grantors go under alice while the HAND is the argument's, so the one case
-- that makes bob active reads alice's Exploration against bob's land plays --
-- CR 109.5's You scope with the two players actually pulled apart.
landDropBoard :: Printing.Printing -> [Printing.Printing] -> PlayerId.PlayerId -> GameState.GameState
landDropBoard mountain grantors active =
  let put g printing = snd (S.addPermanent printing S.alice g)
      withGrantors = List.foldl' put (Setup.emptyGame S.bothPlayers) grantors
      add g _ = snd (S.addHandCard mountain active g)
      withHand = List.foldl' add withGrantors [1 .. 5 :: Int]
   in withHand
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = active,
          GameState.priority = Just active
        }

-- Take every land play the board allows and stop. S.playLandAnswer plays a land
-- whenever one is offered and passes otherwise, so the loop halts exactly when
-- CR 305.2a's comparison refuses -- the whole gate, through the real priority
-- loop, rather than a direct call to Action.legalActions.
playEveryLand :: GameState.GameState -> GameState.GameState
playEveryLand gs = S.runPure S.playLandAnswer gs Engine.priorityLoop

-- CR 601.3b's board, shared by the two groups below it -- Vedalken Orrery's and
-- Sigarda's Aid's -- since what a permission is read off is the caller's `extra`.
--
-- One board, built twice. alice holds `hand` -- a creature card, so CR 302.1 and
-- CR 117.1a's second sentence give it the sorcery-speed window -- and a
-- Mountain, behind nine untapped Mountains so that mana is never the reason a
-- cast is unavailable. It is BOB's precombat main phase and the stack is empty,
-- so alice's own sorcery-speed window is shut. `extra` goes onto the battlefield
-- under alice, and is the only thing a pair of boards here ever differs by.
-- Returns the hand card, the ids of `extra` in the order given, and the board.
flashBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
flashBoard mountain hand extra =
  let (base, oid) = S.pikerInHand mountain hand 9 Phase.PrecombatMain
      withLand = snd (S.addHandCard mountain S.alice base)
      put (ids, g) printing = let (i, g1) = S.addPermanent printing S.alice g in (ids <> [i], g1)
      (extraIds, withExtra) = List.foldl' put ([], withLand) extra
   in ( oid,
        extraIds,
        withExtra
          { GameState.activePlayer = S.bob,
            GameState.priority = Just S.alice
          }
      )

-- The same board back on ALICE's turn, which is the control every refusal below
-- is measured against: it is what says the card in hand is affordable, offered
-- and unblocked by anything the permission under test is not responsible for.
flashOnOwnTurn :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
flashOnOwnTurn mountain hand extra =
  let (oid, _, board) = flashBoard mountain hand extra
   in (oid, board {GameState.activePlayer = S.alice})

-- CR 109.5's You scope from the other seat: it is ALICE's turn, BOB holds
-- priority with a Piker of his own, and both players have nine untapped
-- Mountains. `owner` is who controls the Orrery, and is the only thing that
-- varies.
orreryScopeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
orreryScopeBoard mountain piker orrery owner =
  let base = S.landsInPlay mountain 9
      withBobsLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.bob g)) base [1 .. 9 :: Int]
      (bobsPiker, withBobsPiker) = S.addHandCard piker S.bob withBobsLands
      withOrrery = snd (S.addPermanent orrery owner withBobsPiker)
   in ( bobsPiker,
        withOrrery
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.bob
          }
      )

-- CR 307.5's window, on the same axis and carried by something else entirely:
-- alice controls a Goblin Piker to equip, a Bonesplitter to equip it with
-- (data/cards/bonesplitter.json declares the equip ability SorcerySpeed) and
-- whatever `extra` names, with nine untapped Mountains for the {1}. `active` is
-- whose turn it is. Returns the Equipment.
equipBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
equipBoard mountain piker bonesplitter extra active =
  let base = S.landsInPlay mountain 9
      withPiker = snd (S.addPermanent piker S.alice base)
      (equipment, withEquipment) = S.addPermanent bonesplitter S.alice withPiker
      withExtra = List.foldl' (\g printing -> snd (S.addPermanent printing S.alice g)) withEquipment extra
   in ( equipment,
        withExtra
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = active,
            GameState.priority = Just S.alice
          }
      )

isActivateOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isActivateOf oid action = case action of
  Action.Type.Activate o _ -> o == oid
  Action.Type.Cast {} -> False
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

isPlay :: Action.Type.Action -> Bool
isPlay action = case action of
  Action.Type.Play {} -> True
  Action.Type.Cast {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

-- Runed Halo {W}{W} Enchantment: "As this enchantment enters, choose a card
-- name. You have protection from the chosen card name." The pool's one card that
-- gives a PLAYER protection from a CHOSEN NAME -- The Stasis Coffin below states
-- its quality on the card instead -- and so the producer of both halves this
-- group proves: CR 702.16b's targeting bar and CR 702.16c's enchanting bar, each
-- read off Pawl.Engine.PlayerEffect.protectedFrom.
--
-- Curse of Vitality is the Aura on the other side, and it has to be an
-- enchant-PLAYER one (CR 702.5d): rule 702.16c's player half is the clause under
-- test, and an Aura that enchants a creature never reaches it.
--
-- THREE SEATS, and that is what makes each case discriminating rather than
-- vacuous: alice holds the Halo, bob holds the Curse, and carol holds nothing --
-- so "bob may not enchant alice" is told apart from "bob may not enchant
-- anybody" on one board, and from "the Curse is illegal" on one pass.
--
-- Every case is boards differing in ONE thing -- the chosen NAME, or which seat
-- the case asks about. A board where the Halo names Goblin Piker is the same
-- board in every other respect, which is what keeps the mana, the phase and the
-- Curse's own legality out of the answer.
runedHaloBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
runedHaloBoard plains halo curse =
  let lands = S.landsFor plains S.bob 3 (S.landsFor plains S.alice 2 S.threePlayerGame)
      (haloId, g1) = S.addHandCard halo S.alice lands
      (curseId, g2) = S.addHandCard curse S.bob g1
   in ( haloId,
        curseId,
        g2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- runedHaloBoard carried through one combat: the Halo enters naming `name`, bob's
-- Goblin Piker attacks `defender` alone, and the pair (before, after) comes back
-- so a case can read the life it cost. Nothing blocks -- neither alice nor carol
-- has a creature -- so CR 510.1b assigns the Piker's 2 to the player it attacks.
--
-- COMBAT damage and not a burn spell, which is the whole reason this helper
-- exists: rule 702.16b already stops a spell with the chosen name from TARGETING
-- the protected player, so a Lightning Bolt aimed at alice never reaches rule
-- 702.16e -- and a case built on one stays green with the prevention deleted,
-- which is how this one was found. CR 510.2's combat damage is a turn-based
-- action that uses no stack and chooses no CR 115.1 target, so rule 702.16e is
-- the only clause of rule 702.16 that can stop it.
--
-- The attacker is bob's, so it is bob's combat: the board is re-pointed at him
-- after the Halo resolves on alice's own main phase, which is the only order
-- CR 614.1c's as-enters choice can happen in.
runedHaloCombat ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  CardName.CardName ->
  PlayerId.PlayerId ->
  (GameState.GameState, GameState.GameState)
runedHaloCombat plains halo curse piker name defender =
  let (haloId, _, base) = runedHaloBoard plains halo curse
      (_, withPiker) = S.addPermanent piker S.bob (castHalo name base haloId)
      before =
        withPiker
          { GameState.activePlayer = S.bob,
            GameState.priority = Just S.bob,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [defender]}
          }
      fight = Combat.declareAttackers S.manaPerformer S.bob >> Combat.declareBlockers S.manaPerformer >> Damage.dealCombatDamage
   in (before, S.settleSba (S.runPure (S.attackTo defender) before fight))

-- Cast the Halo and let it resolve, naming `name`.
--
-- CAST rather than S.addPermanent, castChamber's reason: the choice happens only
-- on the entry path (Event.runEntry), so a Halo placed straight onto the
-- battlefield has an empty chosenNames and protects from nothing.
castHalo :: CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castHalo name gs oid =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseCardName {} -> name
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice oid))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- castHalo through Pawl.Interpreter.lookingUpCards over the suite's registry, so
-- Prompt.LookUpCard is answered the way an interpreter holding the reference
-- answers it. castHalo is the same cast with the reference never consulted.
lookedUpHalo :: (Monad m) => Registry.Registry m -> CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> m GameState.GameState
lookedUpHalo registry name gs oid = do
  (_, after) <- Engine.runGameAsked (Interpreter.lookingUpCards registry (namingAnswer name)) gs (S.cast S.alice oid >> Stack.resolveTop)
  pure after

-- Names `name` whenever a card name is asked for, and otherwise S.identityAnswer.
namingAnswer :: (Monad m) => CardName.CardName -> Asked.Asked r -> m r
namingAnswer name asked = case Asked.prompt asked of
  Prompt.ChooseCardName {} -> pure name
  p -> pure (S.identityAnswer p)

-- A Hill Giant of bob's, carrying `kit` when there is one, attacks alice alone
-- off a board the Halo has already entered; the pair (before, after) comes back
-- for the life it cost. runedHaloCombat's shape, with an attacker that is no
-- Goblin Piker.
spyKitCombat :: Printing.Printing -> Maybe Printing.Printing -> GameState.GameState -> (GameState.GameState, GameState.GameState)
spyKitCombat giant kit haloed =
  let (giantId, withGiant) = S.addPermanent giant S.bob haloed
      equipped = case kit of
        Nothing -> withGiant
        Just k ->
          let (kitId, g) = S.addPermanent k S.bob withGiant
           in S.attachTo kitId (Recipient.ToObject giantId) g
      before =
        equipped
          { GameState.activePlayer = S.bob,
            GameState.priority = Just S.bob,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice]}
          }
      fight = Combat.declareAttackers S.manaPerformer S.bob >> Combat.declareBlockers S.manaPerformer >> Damage.dealCombatDamage
   in (before, S.settleSba (S.runPure (S.attackTo S.alice) before fight))

runedHaloSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
runedHaloSpec s registry =
  Spec.describe s "RunedHalo" $ do
    -- CR 702.16e's PLAYER half: "any damage that would be dealt by sources that
    -- have the stated quality to a permanent or player with protection is
    -- prevented." The clause that had no mint at all until
    -- Pawl.Engine.Replacement.collect grew a segment reading this axis: the
    -- targeting and Aura bars above are gates a caller asks, where a prevention
    -- has to be a CR 615.1 row before the damage event is proposed.
    --
    -- THREE combats, each differing from the first in exactly one thing. alice
    -- takes none of the Piker's 2 (the shield exists); carol takes all of it off
    -- the same named board (the shield is scoped to the Halo's controller, and is
    -- not a Fog); and alice takes all of it when the Halo named the Curse instead
    -- (the shield reads CR 201.4's chosen name rather than firing on every
    -- source). The second is also what says combat damage really flowed.
    Spec.it s "CR 702.16e combat damage from a source with the chosen name is prevented, and the same attacker otherwise connects" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      piker <- S.printingOf s registry "Goblin Piker"
      let combat = runedHaloCombat plains halo curse piker
          (beforeNamed, named) = combat (S.printingName piker) S.alice
          (beforeCarol, atCarol) = combat (S.printingName piker) S.carol
          (beforeOther, other) = combat (S.printingName curse) S.alice
      Spec.assertEqWith s "with the Piker named, alice takes none of its 2" (S.lifeOf S.alice named) (S.lifeOf S.alice beforeNamed)
      Spec.assertEqWith s "carol takes the whole 2 with the same name chosen -- the Halo protects its controller alone" (S.lifeOf S.carol atCarol) (fmap (subtract 2) (S.lifeOf S.carol beforeCarol))
      Spec.assertEqWith s "and with the Curse named instead, so does alice" (S.lifeOf S.alice other) (fmap (subtract 2) (S.lifeOf S.alice beforeOther))
    -- CR 612.7 with CR 108.1: Spy Kit's host has every nonlegendary creature
    -- card's name in the Oracle card reference, not just the game's. The Halo
    -- names Goblin Piker, which is in no zone, so only Prompt.LookUpCard's answer
    -- puts it in reach; the attacker is a Hill Giant.
    --
    -- FOUR combats differing in one thing each: no Kit, a legendary creature
    -- card named instead (the filter, not "every name"), and the reference never
    -- consulted (the lookup is load-bearing).
    Spec.it s "CR 612.7 / 702.16e a Spy Kit host has the chosen name of a card in no zone, and its damage is prevented" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      kit <- S.printingOf s registry "Spy Kit"
      giant <- S.printingOf s registry "Hill Giant"
      jedit <- S.printingOf s registry "Jedit Ojanen"
      let (haloId, _, board) = runedHaloBoard plains halo curse
          pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      namedPiker <- lookedUpHalo registry pikerName board haloId
      namedJedit <- lookedUpHalo registry (S.printingName jedit) board haloId
      let (beforeKit, withKit) = spyKitCombat giant (Just kit) namedPiker
          (beforeBare, bare) = spyKitCombat giant Nothing namedPiker
          (beforeLegend, legend) = spyKitCombat giant (Just kit) namedJedit
          (beforeUnasked, unasked) = spyKitCombat giant (Just kit) (castHalo pikerName board haloId)
      Spec.assertEqWith s "the Kit's host is named Goblin Piker, so alice takes none of its 4" (S.lifeOf S.alice withKit) (S.lifeOf S.alice beforeKit)
      Spec.assertEqWith s "unequipped, the Giant is no Goblin Piker and deals its 3" (S.lifeOf S.alice bare) (fmap (subtract 3) (S.lifeOf S.alice beforeBare))
      Spec.assertEqWith s "a legendary creature card's name is not the host's, so alice takes 4" (S.lifeOf S.alice legend) (fmap (subtract 4) (S.lifeOf S.alice beforeLegend))
      Spec.assertEqWith s "with the reference never asked, alice takes 4" (S.lifeOf S.alice unasked) (fmap (subtract 4) (S.lifeOf S.alice beforeUnasked))
      Spec.assertBool s (not (Map.member pikerName (Game.referenceFaces board))) "no Goblin Piker card is anywhere in the game before the Halo names it"
    -- CR 612.7 / 206.3a: City in a Bottle's state trigger (CR 603.8) sweeps the
    -- other nontoken permanents with a name originally printed in Arabian Nights,
    -- and a Spy Kit host has every such nonlegendary creature card's name --
    -- Kird Ape's among them -- though no such card is in the game. Two boards
    -- differing in one thing: the reference answered by the suite's registry
    -- (Pawl.Interpreter.lookingUpCards), and never consulted.
    Spec.it s "CR 612.7 / 206.3a a Spy Kit host has the Arabian Nights names of cards the game has never seen, and City in a Bottle sweeps it" $ do
      bottle <- S.printingOf s registry "City in a Bottle"
      kit <- S.printingOf s registry "Spy Kit"
      giant <- S.printingOf s registry "Hill Giant"
      let (bottleId, g1) = S.addPermanent bottle S.alice S.threePlayerGame
          (giantId, g2) = S.addPermanent giant S.alice g1
          (kitId, g3) = S.addPermanent kit S.alice g2
          board =
            (S.attachTo kitId (Recipient.ToObject giantId) g3)
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
          kirdApe = CardName.MkCardName (Text.pack "Kird Ape")
          unasked = S.runPure S.identityAnswer board Engine.priorityLoop
      (_, asked) <- Engine.runGameAsked (Interpreter.lookingUpCards registry (pure . S.identityAnswer . Asked.prompt)) board Engine.priorityLoop
      Spec.assertBool s (not (S.onBattlefield giantId asked)) "CR 603.8 the host, named Kird Ape among others, was sacrificed"
      Spec.assertBool s (S.onBattlefield giantId unasked) "with the reference never consulted it survives"
      Spec.assertBool s (S.onBattlefield bottleId asked && S.onBattlefield kitId asked) "the Bottle and the Kit themselves stay"
      Spec.assertBool s (not (Map.member kirdApe (Game.referenceFaces board))) "no Kird Ape card is anywhere in the game"
    -- CR 612.7 / 709.4a/c: a split card with a creature half is a nonlegendary
    -- creature card with two names, so a Spy Kit host has its instant half's
    -- name too. The split card sits in bob's hand, since a synthetic is no card
    -- of the registry's reference. Two boards differing in one thing: the split
    -- card in the game, and not.
    Spec.it s "CR 612.7 / 709.4a a Spy Kit host has both names of a split card with a creature half" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      kit <- S.printingOf s registry "Spy Kit"
      giant <- S.printingOf s registry "Hill Giant"
      split <- S.printingOf s registry "Synthetic Mirror Rite"
      let (haloId, _, board) = runedHaloBoard plains halo curse
          (_, withSplit) = S.addHandCard split S.bob board
          rite = CardName.MkCardName (Text.pack "Synthetic Mirror Rite")
          (beforeSplit, withSplitAfter) = spyKitCombat giant (Just kit) (castHalo rite withSplit haloId)
          (beforeAbsent, absent) = spyKitCombat giant (Just kit) (castHalo rite board haloId)
      Spec.assertEqWith s "the host is named Synthetic Mirror Rite, so alice takes none of its 4" (S.lifeOf S.alice withSplitAfter) (S.lifeOf S.alice beforeSplit)
      Spec.assertEqWith s "with no such split card in the game, alice takes 4" (S.lifeOf S.alice absent) (fmap (subtract 4) (S.lifeOf S.alice beforeAbsent))

-- The Stasis Coffin {3} Legendary Artifact: "{2}, {T}, Exile The Stasis Coffin:
-- You gain protection from everything until your next turn." The pool's one card
-- that gives a PLAYER protection from a quality the CARD states
-- (PlayerEffect.HasProtectionFrom), where Runed Halo above states CR 201.4's
-- chosen name instead -- and CR 702.16j's "everything" is that quality written as
-- the empty conjunction.
--
-- The STORED carrier, which is the half Runed Halo cannot reach: the Coffin
-- exiles itself to pay for its own ability (CR 601.2h), so by the time any of the
-- three prohibitions is read the source is in exile and the quality can only have
-- come off the row.
--
-- Curse of Vitality is the enchant-PLAYER Aura on the other side of rules 702.16b
-- and 702.16c, as it is for Runed Halo; the Goblin Piker is rule 702.16e's
-- attacker.
--
-- THREE SEATS, for runedHaloBoard's reason: alice holds the Coffin, bob holds the
-- Curse and the Piker, and carol holds nothing, so "bob may not target alice" is
-- told apart from "bob may not target anybody".
--
-- alice's two Plains are exactly the ability's {2} and bob's three are exactly the
-- Curse's {2}{W}, so no case below can turn on an unaffordable cost.
stasisCoffinBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
stasisCoffinBoard plains coffin curse piker =
  let lands = S.landsFor plains S.bob 3 (S.landsFor plains S.alice 2 S.threePlayerGame)
      (coffinId, g1) = S.addPermanent coffin S.alice lands
      (curseId, g2) = S.addHandCard curse S.bob g1
      (pikerId, g3) = S.addPermanent piker S.bob g2
   in ( coffinId,
        curseId,
        pikerId,
        g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Activate the Coffin's one ability and let it resolve. The ability is read off
-- the PROJECTION rather than the printed face, Pawl.AuraSpec's fortify cases'
-- spelling.
activateCoffin :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateCoffin coffinId gs = case Projection.abilitiesOf coffinId gs of
  [ability] -> S.runPure S.identityAnswer gs (Activate.activateAbility S.alice coffinId ability >> Stack.resolveTop)
  _ -> gs

stasisCoffinSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
stasisCoffinSpec s registry =
  Spec.describe s "TheStasisCoffin" $ do
    -- CR 702.16j with CR 702.16c's second sentence: "such Auras attached to the
    -- permanent or player with protection will be put into their owners'
    -- graveyards as a state-based action" (CR 704.5m), which
    -- Pawl.Engine.Sba.fallsOff answers through
    -- Pawl.Engine.PlayerEffect.protectedFrom.
    --
    -- The Curse is attached BEFORE the ability resolves, which is the order the
    -- rule is about.
    Spec.it s "CR 702.16j / 702.16c an Aura already enchanting alice is buried once she gains protection from everything" $ do
      plains <- S.printingOf s registry "Plains"
      coffin <- S.printingOf s registry "The Stasis Coffin"
      curse <- S.printingOf s registry "Curse of Vitality"
      piker <- S.printingOf s registry "Goblin Piker"
      let (coffinId, _, _, base) = stasisCoffinBoard plains coffin curse piker
          (aura, withAura) = S.addPermanent curse S.bob base
          cursed = S.attachTo aura (Recipient.ToPlayer S.alice) withAura
          after = S.settleSba (activateCoffin coffinId cursed)
      Spec.assertBool s (not (S.onBattlefield aura after)) "the Curse is off the battlefield after one pass"
      Spec.assertEqWith s "in its OWNER's graveyard, and bob owns it" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertBool s (S.onBattlefield aura (S.settleSba cursed)) "and without the ability it stays where it is"

-- Conjurer's Ban {W}{B} Sorcery: "Choose a card name. Until your next turn,
-- spells with the chosen name can't be cast and lands with the chosen name can't
-- be played. Draw a card."
--
-- The STORED twin of nullChamberSpec above, and the whole reason this group
-- exists: it is the pool's only writer of a chosen-name prohibition as a
-- resolution (CR 611.2c) rather than as a permanent's printed ability, so it is
-- the only card whose Filter.HasChosenName is read through a
-- GameState.playerEffects row. The name is chosen by CR 201.4 during the resolution (Effect
-- .ChooseCardName) and the two rows stored a clause later read it back.
--
-- alice has four Plains, four Mountains and four Swamps -- mana is never the
-- reason a cast is unavailable, before or after the Ban's own {W}{B} is paid --
-- the Ban in hand, and a Plains in her library for the card it draws.
conjurersBanBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
conjurersBanBoard plains mountain swamp ban =
  let lands = S.landsFor swamp S.alice 4 (S.landsFor mountain S.alice 4 (S.landsInPlay plains 4))
      (gs, oid) = S.handOne ban lands
   in (oid, snd (S.addLibraryCard plains S.alice gs))

-- Cast the Ban and let it resolve, answering CR 201.4's one name choice.
castBan :: CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castBan name gs oid =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseCardName {} -> name
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice oid))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

conjurersBanSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
conjurersBanSpec s registry =
  Spec.describe s "ConjurersBan" $ do
    -- CR 601.3's prohibit half carrying a QUALITY, read off a stored row. The
    -- Bolt is the falsifier twice over: a blanket prohibition would take it away
    -- too, and it is castable on the same board with the same untapped lands, so
    -- mana is not what stops the Piker.
    Spec.it s "CR 611.2c a spell with the name the resolution chose can't be cast, and its neighbour still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      swamp <- S.printingOf s registry "Swamp"
      ban <- S.printingOf s registry "Conjurer's Ban"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = conjurersBanBoard plains mountain swamp ban
          (pikerId, withPiker) = S.addHandCard piker S.alice board
          (boltId, before) = S.addHandCard lightningBolt S.alice withPiker
          after = castBan (S.printingName piker) before oid
          castPiker = Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp
          castBolt = Action.Type.Cast boltId (S.printingName lightningBolt) Facing.FaceUp
      Spec.assertBool s (elem castPiker (Action.legalActions S.alice before)) "alice may cast her Piker before the Ban resolves"
      Spec.assertBool s (notElem castPiker (Action.legalActions S.alice after)) "and may not once it has"
      Spec.assertBool s (elem castBolt (Action.legalActions S.alice after)) "the unnamed Bolt still is offered"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice pikerId VariableChoice.Announced after) "and the prohibition is the named one"
      -- The carrier, asserted AFTER the behaviour: nothing this card makes is a
      -- permanent, so both rows are CR 611.2c stored ones.
      Spec.assertEqWith s "two stored rows" (length (GameState.playerEffects after)) 2

-- City in a Bottle {2} Artifact -- second sentence: "Players can't cast spells
-- or play lands with a name originally printed in the Arabian Nights
-- expansion." (name, cost, type line and Oracle text checked against
-- api.scryfall.com, 2026-09-06.)
--
-- CR 206.3a defines the name list, so both halves are one
-- Filter.HasNameOriginallyPrintedIn; what is new is the PLAY half. CR 305.1 makes playing a land a
-- special action that never uses the stack, so the cast-side prohibition beside
-- it reaches no land however its Filter reads -- which is why
-- PlayerEffect.CantPlayLands now carries a Filter of its own and
-- Action.playableLands hands prohibitsPlayingLand the object it narrows by.
--
-- Desert is the listed land and the Mountain is the falsifier: they differ only
-- in whether CR 206.3a names them, and a prohibition that narrowed nothing --
-- Damping Engine's reading -- would take the Mountain too. The pair of BOARDS
-- differing in one thing is the Bottle itself, which is what says the denial is
-- the card's rather than the fixture's.
--
-- bob is asked as well, because CR 611.1's "players" is every seat and not the
-- Bottle's controller alone.
cityInABottleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cityInABottleSpec s registry =
  Spec.describe s "CityInABottle" $ do
    Spec.it s "CR 305.1 a land whose name CR 206.3a lists can't be played, and its neighbour still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      desert <- S.printingOf s registry "Desert"
      bottle <- S.printingOf s registry "City in a Bottle"
      let hands base =
            let (aliceDesert, g1) = S.addHandCard desert S.alice base
                (aliceMountain, g2) = S.addHandCard mountain S.alice g1
                (bobDesert, g3) = S.addHandCard desert S.bob g2
             in (aliceDesert, aliceMountain, bobDesert, g3)
          -- alice's own main phase with priority in hand, which is what makes
          -- the OFFER assertions below say something: CR 305.1's window is
          -- Action.legalActions', and outside it no land is offered at all.
          mainPhase base =
            base
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
          (_, withBottle) = S.addPermanent bottle S.alice (mainPhase (S.landsInPlay plains 4))
          (aliceDesertId, aliceMountainId, bobDesertId, gs) = hands withBottle
          (freeDesertId, _, _, before) = hands (mainPhase (S.landsInPlay plains 4))
          playable = Action.playableLands S.alice gs
      Spec.assertBool s (notElem (aliceDesertId, Nothing) playable) "CR 305.1 alice's Desert, whose name CR 206.3a lists, is not playable"
      Spec.assertBool s (elem (aliceMountainId, Nothing) playable) "her Mountain, which that list does not name, still is"
      Spec.assertBool s (notElem (Action.Type.Play aliceDesertId Nothing) (Action.legalActions S.alice gs)) "and no Play is offered for the Desert"
      Spec.assertBool s (elem (Action.Type.Play aliceMountainId Nothing) (Action.legalActions S.alice gs)) "while the Mountain is offered"
      Spec.assertBool s (notElem (bobDesertId, Nothing) (Action.playableLands S.bob gs)) "CR 611.1 bob's Desert is stopped too, the sentence naming every seat"
      Spec.assertBool s (elem (freeDesertId, Nothing) (Action.playableLands S.alice before)) "and the same Desert is playable on the board the Bottle is missing from"

    -- The cast half on the same board, which the arm above cannot answer for: CR
    -- 305.1 keeps the two gates apart, so a Filter on either one alone would
    -- leave the other sentence unwritten. Asked through prohibitsCasting rather
    -- than through legalActions, so that the negative cannot pass for want of
    -- mana.
    Spec.it s "CR 601.3a a spell whose name CR 206.3a lists can't be cast, and its neighbour still can" $ do
      plains <- S.printingOf s registry "Plains"
      kirdApe <- S.printingOf s registry "Kird Ape"
      piker <- S.printingOf s registry "Goblin Piker"
      bottle <- S.printingOf s registry "City in a Bottle"
      let (_, withBottle) = S.addPermanent bottle S.alice (S.landsInPlay plains 4)
          (apeId, withApe) = S.addHandCard kirdApe S.alice withBottle
          (pikerId, gs) = S.addHandCard piker S.alice withApe
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice apeId VariableChoice.Announced gs) "CR 601.3a alice may not cast her Kird Ape, whose name CR 206.3a lists"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice pikerId VariableChoice.Announced gs)) "and her Goblin Piker, which that list does not name, is untouched"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PlayerEffect" $ do
  nullChamberSpec s registry
  runedHaloSpec s registry
  stasisCoffinSpec s registry
  conjurersBanSpec s registry
  cityInABottleSpec s registry
  silenceSpec s registry
  sphinxsDecreeSpec s registry
  ceaseFireSpec s registry
  blossomingCalmSpec s registry
  conditionalSilenceSpec s registry
  hackedSilenceSpec s registry
  lilianaSpec s registry
  matchesObjectSpec s registry
