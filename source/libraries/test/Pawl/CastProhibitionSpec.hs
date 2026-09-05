{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.PlayerEffect over effects that forbid casting (CR 601.3): Silence
-- and its conditional forms, Liliana, Null Chamber, Runed Halo, Conjurer's Ban.
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
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator Pawl.Engine.Filter already claims the alias Filter.
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
-- Aliased Card.Type, per the project-wide convention (CardSpec): the logic
-- module Pawl.Engine.Card may later be imported and must not collide.

import qualified Pawl.Interpreter as Interpreter
import Pawl.PlayerEffectSpec (anySpell, anySpellId, isCast, isSilenceActivate, silenceAfter, swapAt, threeSeatSilenceBoard)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.BeginningStep as BeginningStep
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

    -- CR 611.2c, THE FALSIFIER: nothing bob owns is a spell when Silence
    -- resolves -- the stack holds only Silence itself. Freeze the affected
    -- set and this card does literally nothing.
    Spec.it s "CR 611.2c the effect reaches a spell that did not exist when it began" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertEqWith s "one stored effect" (length (GameState.playerEffects after)) 1
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced after) "bob is prohibited"
      Spec.assertEqWith
        s
        "and no cast is offered"
        (filter isCast (Action.legalActions S.bob after))
        []

    -- CR 109.5: "your opponents" is scoped off Silence's controller, which
    -- is baked into the stored effect because its source is in a graveyard.
    Spec.it s "CR 109.5 the Opponents scope spares the caster" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, silence2Id, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced after)) "alice is not prohibited"
      Spec.assertBool s (S.castable S.alice silence2Id after) "and may cast her second Silence"

    -- Ruling: "The only thing Silence stops is casting spells. Your
    -- opponents can still activate abilities ... they can still play lands,
    -- and so on."
    Spec.it s "CR 601.3 only casting is stopped" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, _, landId, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertBool s (elem (Action.Type.Play landId Nothing) (Action.legalActions S.bob after)) "bob may still play a land"
      Spec.assertBool s (any isSilenceActivate (Action.legalActions S.bob after)) "and still activate an ability"

    Spec.it s "CR 514.2 the prohibition ends at cleanup" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
          ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
      Spec.assertEqWith s "nothing stored" (GameState.playerEffects ended) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced ended)) "bob may cast again"

    -- CR 806.1: in a free-for-all the players compete as individuals, so the
    -- card's your-opponents is EVERY other player, not the next seat. This is
    -- the first Silence fixture that can tell those apart.
    Spec.it s "CR 806.1 at three seats Silence stops BOTH opponents, and still spares the caster" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (silenceId, bobsPiker, carolsPiker, before) = threeSeatSilenceBoard plains silence mountain piker
          -- Goblin Piker is a creature, so CR 302.1's timing applies: it is
          -- offered only to the ACTIVE player (Cast.sorcerySpeed). `before` is
          -- alice's own main phase (she needs no such window: Silence is an
          -- instant), so bob and carol's positive controls are checked against a
          -- copy with the activePlayer field flipped to each of them in turn --
          -- nothing else about the board changes. Directly poking activePlayer via
          -- record update to stage a hypothetical turn already appears above
          -- (thaliaBoard, ruleOfLawBoard's nextOwnTurn).
          bobsTurn = before {GameState.activePlayer = S.bob}
          carolsTurn = before {GameState.activePlayer = S.carol}
          resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice silenceId)) Engine.priorityLoop
          resolvedBobsTurn = resolved {GameState.activePlayer = S.bob}
          resolvedCarolsTurn = resolved {GameState.activePlayer = S.carol}
      -- The fixture really is three-seat and both opponents really could cast,
      -- given their own main phase.
      Spec.assertEqWith s "three seats" (length (GameState.turnOrder before)) 3
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (Action.legalActions S.bob bobsTurn)) "bob could cast before it resolved"
      Spec.assertBool s (elem (Action.Type.Cast carolsPiker (S.printingName piker) Facing.FaceUp) (Action.legalActions S.carol carolsTurn)) "carol could cast before it resolved"
      Spec.assertEqWith s "one stored effect" (length (GameState.playerEffects resolved)) 1
      -- THE DISCRIMINATOR. carol is the far seat: an Opponents scope resolved as
      -- "the next player in turn order" prohibits bob and leaves carol free, and
      -- that is the reading the doc comments claimed was in here.
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced resolved) "bob is prohibited"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.carol anySpellId anySpell VariableChoice.Announced resolved) "carol is prohibited too"
      Spec.assertEqWith
        s
        "and nothing is offered to either, even on their own main phase"
        (filter isCast (Action.legalActions S.bob resolvedBobsTurn) <> filter isCast (Action.legalActions S.carol resolvedCarolsTurn))
        []
      -- CR 109.5: the scope is resolved off the effect's controller.
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced resolved)) "alice is not prohibited"

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
      addLands printing pid = repeatedly (snd . S.addCreature printing pid)
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

-- How many cards this seat's library holds.
librarySize :: PlayerId.PlayerId -> GameState.GameState -> Int
librarySize pid gs = length (Map.findWithDefault mempty pid (GameState.library gs))

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

    -- The other axis of the same restriction, on the SAME seat and the same
    -- board: "creature spells" is a Filter over the spell, so carol's instant is
    -- untouched. Without this the case above would pass for a prohibition that
    -- stopped carol casting anything at all.
    Spec.it s "CR 601.3a the targeted seat may still cast a noncreature spell" $ do
      plains <- S.printingOf s registry "Plains"
      ceaseFire <- S.printingOf s registry "Cease-Fire"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (_, _, _, carolsBolt, _, after) = ceaseFireAfter plains ceaseFire mountain piker lightningBolt
      Spec.assertBool s (offersCast S.carol carolsBolt lightningBolt after) "carol may still cast her Lightning Bolt"

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

    -- The card's second clause. Its control is the two untargeted libraries,
    -- which the same resolution must leave alone.
    Spec.it s "the draw is its controller's, not the targeted player's" $ do
      plains <- S.printingOf s registry "Plains"
      ceaseFire <- S.printingOf s registry "Cease-Fire"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (_, _, _, _, before, after) = ceaseFireAfter plains ceaseFire mountain piker lightningBolt
      Spec.assertEqWith s "every library starts equal" (fmap (`librarySize` before) [S.alice, S.bob, S.carol]) [2, 2, 2]
      Spec.assertEqWith s "alice drew one; nobody else drew" (fmap (`librarySize` after) [S.alice, S.bob, S.carol]) [1, 2, 2]

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
      (_, gs1) = S.addCreature plains S.alice gs0
      (calmId, gs2) = S.addHandCard calm S.alice gs1
      (_, gs3) = S.addCreature mountain S.bob gs2
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
-- Not implemented: the card's third line is rebound (CR 702.88), which pawl has
-- no representation for (#877). The omission runs against the controller -- pawl's
-- Blossoming Calm is cast once where the printed one is cast twice -- so nothing
-- here is weaker than printed.
blossomingCalmSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blossomingCalmSpec s registry =
  Spec.describe s "Blossoming Calm" $ do
    -- THE CONTROL TWIN. Same seats, same Mountain, same Bolt, same answerer --
    -- the only difference from the case below is that alice never cast her
    -- instant. Without this, "alice took no damage" could mean the Bolt was
    -- never cast at all.
    Spec.it s "with no Calm cast, bob's Bolt reaches alice" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (_, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          burned = blossomingCalmBolt boltId before
      Spec.assertEqWith s "alice takes the Bolt" (S.lifeOf S.alice burned) (Just 17)
      Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob burned) (Just 20)
      Spec.assertEqWith s "and so is carol" (S.lifeOf S.carol burned) (Just 20)

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

    -- HEXPROOF, NOT SHROUD, on the stored carrier: CR 702.11c names only
    -- opponents, so alice remains a legal target for her own spells and carol
    -- remains a legal target for everyone. An implementation that read the
    -- payload as EachPlayer passes the case above and fails this one.
    Spec.it s "CR 702.11c the stored effect stops alice's opponents and nobody else" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, _, before) = blossomingCalmBoard plains calm mountain bolt
          resolved = blossomingCalmAfter calmId before
      case S.spellTargetSlot bolt of
        Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
        Just theSlot -> do
          let legalFor who = Target.legalRecipients (Just who) S.noSource theSlot
          Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob before)) "before the Calm, bob may bolt alice"
          Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob resolved))) "after it, he may not"
          Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) (legalFor S.carol resolved))) "and neither may carol -- both opponents, not just the next seat"
          Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.alice resolved)) "but alice may still target herself"
          Spec.assertBool s (Set.member (Recipient.ToPlayer S.carol) (legalFor S.bob resolved)) "and carol is targetable as ever"

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
      (_, gs1) = S.addCreature island S.alice gs0
      (swampId, gs2) =
        if withSwamp
          then S.addCreature swamp S.alice gs1
          else (S.noSource, gs1)
      (hushId, gs3) = S.addHandCard hush S.alice gs2
      (_, gs4) = S.addCreature mountain S.bob gs3
      (_, gs5) = S.addCreature mountain S.bob gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      (_, gs7) = S.addCreature mountain S.carol gs6
      (_, gs8) = S.addCreature mountain S.carol gs7
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
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced resolved) "bob is prohibited"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.carol anySpellId anySpell VariableChoice.Announced resolved) "carol is prohibited too"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced resolved)) "alice is not"
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

    -- THE NEGATIVE, on the same board with exactly one difference: the Swamp
    -- changes hands (CR 613.1b), so alice no longer controls one and CR 611.2b's
    -- period is over. The effect is DELETED, and Engine.settleForPriority is what
    -- runs the sweep in a live game.
    Spec.it s "CR 611.2b when the Swamp changes hands the effect is deleted" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, swampId, bobsPiker, _, before) = conditionalSilenceBoard True island swamp hush mountain piker
          resolved = conditionalSilenceAfter hushId before
          stolen = S.giveControl swampId S.bob resolved
          settled = S.runPure S.identityAnswer stolen Engine.settleForPriority
      Spec.assertEqWith s "one stored before the Swamp moved" (length (GameState.playerEffects resolved)) 1
      Spec.assertEqWith s "none after" (GameState.playerEffects settled) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced settled)) "bob is no longer prohibited"
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob settled)) "and his Piker is offered again"

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
      (firstIsland, gs1) = S.addCreature island S.alice gs0
      (secondIsland, gs2) = S.addCreature island S.alice gs1
      (hushId, gs3) = S.addHandCard hush S.alice gs2
      (hackId, gs4) = S.addHandCard magicalHack S.alice gs3
      (_, gs5) = S.addCreature mountain S.bob gs4
      (_, gs6) = S.addCreature mountain S.bob gs5
      (bobsPiker, gs7) = S.addHandCard piker S.bob gs6
      (_, gs8) = S.addCreature mountain S.carol gs7
      (_, gs9) = S.addCreature mountain S.carol gs8
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
hackedSilenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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

    -- THE UNIT'S POINT. The same Swamp-less board, with the word the duration
    -- names swapped for one alice does control. The gameplay assertion leads: an
    -- arm that dropped the rewrite would leave the clause counting Swamps, the
    -- duration would never start, and both opponents would still be offered their
    -- Pikers.
    Spec.it s "CR 612.1 a Magical Hack on the Silence rewrites the duration's own word" $ do
      island <- S.printingOf s registry "Island"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      magicalHack <- S.printingOf s registry "Magical Hack"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, hackId, _, _, _, _, before) = hackedSilenceBoard island hush magicalHack mountain piker
          after = hackedSilenceAfter True hushId hackId before
      Spec.assertEqWith s "nothing is offered to either opponent" (conditionalSilenceCasts S.bob after <> conditionalSilenceCasts S.carol after) []
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced after) "bob is prohibited"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.carol anySpellId anySpell VariableChoice.Announced after) "carol is prohibited too"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced after)) "alice is not"
      Spec.assertEqWith s "and both spells resolved" (length (GameState.stack after)) 0
      case fmap ActivePlayerEffect.expiry (GameState.playerEffects after) of
        [Expiry.Type.While (While.MkWhile who _)] -> Spec.assertEqWith s "the duration is keyed to its controller" who S.alice
        other -> Spec.assertFailure s ("expected one conditional player effect, got " <> show other)

    -- And the rewritten clause is still a CONDITION rather than an open-ended
    -- one: hand alice's two Islands to bob (CR 613.1b) and CR 611.2b's period is
    -- over, so the effect is deleted. Without this an arm that stored the effect
    -- unconditionally would pass the case above.
    Spec.it s "CR 611.2b the rewritten clause counts Islands, so losing them ends it" $ do
      island <- S.printingOf s registry "Island"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      magicalHack <- S.printingOf s registry "Magical Hack"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, hackId, firstIsland, secondIsland, bobsPiker, _, before) = hackedSilenceBoard island hush magicalHack mountain piker
          after = hackedSilenceAfter True hushId hackId before
          stolen = S.giveControl secondIsland S.bob (S.giveControl firstIsland S.bob after)
          settled = S.runPure S.identityAnswer stolen Engine.settleForPriority
          kept = S.runPure S.identityAnswer after Engine.settleForPriority
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob settled)) "bob may cast once the Islands are his"
      Spec.assertEqWith s "and nothing is stored any more" (GameState.playerEffects settled) []
      Spec.assertEqWith s "while a sweep that leaves them with alice changes nothing" (length (GameState.playerEffects kept)) 1
      Spec.assertEqWith s "so bob is still stopped there" (conditionalSilenceCasts S.bob kept) []

-- The one activated ability at index `n` of what the PROJECTION hands out for
-- `oid` -- not Face.activatedAbilities, which is the printed list a text change
-- has not reached. Projection.abilitiesOf is the list Activate itself offers
-- from, so this is the same ability a player would be given.
projectedAbility :: Int -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
projectedAbility n oid gs = case drop n (Projection.abilitiesOf oid gs) of
  ability : _ -> Just ability
  [] -> Nothing

-- Filters the offered set down to the recipient naming `oid`, whatever arm the
-- pool wrapped it in -- a Pool.Creatures slot offers Recipient.ToCreature and a
-- hand-built ToObject of the same permanent is a different recipient that CR
-- 608.2b's re-read drops with no error.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

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
  let (_, g1) = S.addCreature island S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addCreature swamp S.alice g1
      (_, g3) = S.addCreature swamp S.alice g2
      (lilianaId, g4) = S.addCreature liliana S.alice g3
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

-- alice controls Liliana with four loyalty counters over three untapped Swamps,
-- and her library holds five copies of `stock` -- five rather than three so the
-- +1's mill leaves cards behind and CR 104.3c decides nothing.
lilianaMillBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
lilianaMillBoard swamp liliana stock =
  let (lilianaId, g1) = S.addCreature liliana S.alice (S.landsInPlay swamp 3)
      g2 = List.foldl' (\g _ -> snd (S.addLibraryCard stock S.alice g)) g1 [1 :: Int .. 5]
   in ( lilianaId,
        (S.addCounter CounterKind.Loyalty 4 lilianaId g2)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

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
lilianaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lilianaSpec s registry =
  let board = do
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        liliana <- S.printingOf s registry "Liliana, Untouched by Death"
        evolution <- S.printingOf s registry "Artificial Evolution"
        zombie <- S.printingOf s registry "Whipstitched Zombie"
        evangel <- S.printingOf s registry "Cabal Evangel"
        pure (lilianaBoard island swamp liliana evolution zombie evangel)
      millBoard stockName = do
        swamp <- S.printingOf s registry "Swamp"
        liliana <- S.printingOf s registry "Liliana, Untouched by Death"
        stock <- S.printingOf s registry stockName
        pure (lilianaMillBoard swamp liliana stock)
   in Spec.describe s "LilianaUntouchedByDeath" $ do
        -- The control, differing from the case below in the Artificial Evolution
        -- alone: unhacked, the printed word stands and the ZOMBIE is the card
        -- that becomes castable.
        Spec.it s "CR 601.3 unhacked the -3 reaches the Zombie card and not the Cleric" $ do
          (lilianaId, _, zombieId, evangelId, before) <- board
          let after = activateLoyalty S.identityAnswer 2 lilianaId before
          Spec.assertBool s (S.castable S.alice zombieId after) "the Zombie is castable out of the graveyard"
          Spec.assertBool s (not (S.castable S.alice evangelId after)) "the Cleric is not"
          Spec.assertBool s (any (S.isCastOf zombieId) (Action.legalActions S.alice after)) "and the Zombie is offered"
          Spec.assertBool s (not (any (S.isCastOf evangelId) (Action.legalActions S.alice after))) "while the Cleric is not"
          Spec.assertEqWith s "the ability really did store something" (length (GameState.playerEffects after)) 1

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

        -- CR 612.2 again from the other side, and the reason the counts are TWO
        -- and ONE rather than one apiece: alice controls two Whipstitched Zombies
        -- and one Cabal Evangel, so the printed word measures -2/-2 and the
        -- swapped one measures -1/-1. A board with one of each cannot tell them
        -- apart. Berserkers of Blood Ridge is 4/4, so it survives either reading
        -- and no state-based action has to run before the assertion.
        Spec.it s "CR 612.2 the same hack moves the -2's count onto the new word" $ do
          swamp <- S.printingOf s registry "Swamp"
          liliana <- S.printingOf s registry "Liliana, Untouched by Death"
          evolution <- S.printingOf s registry "Artificial Evolution"
          island <- S.printingOf s registry "Island"
          zombie <- S.printingOf s registry "Whipstitched Zombie"
          evangel <- S.printingOf s registry "Cabal Evangel"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          let (_, g1) = S.addCreature island S.alice (S.landsInPlay swamp 2)
              (lilianaId, g2) = S.addCreature liliana S.alice g1
              (_, g3) = S.addCreature zombie S.alice g2
              (_, g4) = S.addCreature zombie S.alice g3
              (_, g5) = S.addCreature evangel S.alice g4
              (victim, g6) = S.addCreature berserkers S.bob g5
              (evolutionId, g7) = S.addHandCard evolution S.alice g6
              ready =
                (S.addCounter CounterKind.Loyalty 4 lilianaId g7)
                  { GameState.phase = Phase.PrecombatMain,
                    GameState.activePlayer = S.alice,
                    GameState.priority = Just S.alice
                  }
              plain = activateLoyalty (aimAtCreature victim) 1 lilianaId ready
              swapped = activateLoyalty (aimAtCreature victim) 1 lilianaId (hackLiliana lilianaId evolutionId Subtype.Zombie Subtype.Cleric ready)
          Spec.assertEqWith s "unhacked the two Zombies are counted" (Projection.powerOf victim plain) (Just 2)
          Spec.assertEqWith s "hacked the one Cleric is counted instead" (Projection.powerOf victim swapped) (Just 3)
          Spec.assertEqWith s "and the toughness follows the same count" (Projection.toughnessOf victim swapped) (Just 3)

        -- The +1's tally HIT: three Zombie cards milled, so the clause condition
        -- comparing Quantity.InSlot against one holds and the drain happens.
        Spec.it s "CR 701.17 the +1 drains when a Zombie card is milled" $ do
          (lilianaId, before) <- millBoard "Whipstitched Zombie"
          let after = activateLoyalty S.identityAnswer 0 lilianaId before
          Spec.assertEqWith s "bob lost two life" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "alice gained two" (S.lifeOf S.alice after) (Just 22)
          Spec.assertEqWith s "and three cards were milled" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 3

        -- The tally MISS, the same board with the milled cards' subtype the only
        -- difference: nothing counted, so the gate refuses and neither life total
        -- moves. Without this the case above would pass on an arm that ignored
        -- the condition entirely.
        Spec.it s "CR 701.17 the +1 does not drain when no Zombie card is milled" $ do
          (lilianaId, before) <- millBoard "Swamp"
          let after = activateLoyalty S.identityAnswer 0 lilianaId before
          Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "so is alice" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and three cards were still milled" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 3

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
      Spec.assertBool s (PlayerEffect.matchesObjectFrom Nothing noncreature bolt gs) "Lightning Bolt is a noncreature spell"

    Spec.it s "CR 613.1d a creature spell fails the noncreature criterion" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObjectFrom Nothing noncreature pikerId gs)) "Goblin Piker is a creature spell"

    Spec.it s "CR 613.1e a colour criterion admits a matching-colour spell" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesObjectFrom Nothing (Filter.Type.HasColor Color.Red) bolt gs) "Lightning Bolt is red"

    Spec.it s "CR 613.1e a colour criterion rejects a non-matching colour" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObjectFrom Nothing (Filter.Type.HasColor Color.Blue) bolt gs)) "Lightning Bolt is not blue"

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
  let addMountain board _ = snd (S.addCreature mountain S.alice board)
      lands = List.foldl' addMountain (S.landsInPlay plains 8) [1 .. 4 :: Int]
      (gs, oid) = S.handOne nullChamber lands
   in (oid, gs)

-- The same board at THREE seats, which is the only shape where "an opponent" is
-- a choice at all (CR 102.2 leaves a two-player game one opponent). Four Plains
-- pay the Chamber's {3}{W}; nothing else is in play.
threeSeatBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
threeSeatBoard plains nullChamber =
  let addLand seat _ = snd (S.addCreature plains S.alice seat)
      lands = List.foldl' addLand (Setup.emptyGame S.threePlayers) [1 .. 4 :: Int]
      (oid, seated) = S.addHandCard nullChamber S.alice lands
   in ( oid,
        seated
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 201.4 answered PER CHOOSER, which is the whole of what makes Null Chamber
-- worth testing: `pick` is asked WHO is choosing, so a case can put the
-- controller's name and the opponent's on different cards. `opponent` settles
-- the card's other open choice -- which opponent is asked at all -- and is never
-- reached at two seats. Everything else is the shared interpreter.
chamberAnswer :: PlayerId.PlayerId -> (PlayerId.PlayerId -> CardName.CardName) -> Prompt.Prompt r -> r
chamberAnswer opponent pick p = case p of
  Prompt.ChooseCardName _ chooser _ _ -> pick chooser
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
  Prompt.ChooseCardName _ chooser _ restriction -> do
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
  let lifted = Registry.MkRegistry (Trans.lift . Registry.fetchCard registry)
      play = Engine.runGameAsked (Interpreter.policingCardNames lifted (queuedChamberAnswer opponent fallback)) gs (S.cast S.alice oid >> Stack.resolveTop)
  ((_, after), left) <- State.runStateT play queue
  pure (after, left)

-- CR 201.4a's restriction as Null Chamber prints it: "other than a basic land
-- card name", which is a supertype and a card type together (CR 205.4a: a basic
-- land card is the one carrying both).
nonBasicLandName :: Filter.Type.Filter Keyword.Keyword
nonBasicLandName = Filter.Type.Not (Filter.Type.And [Filter.Type.HasSupertype Supertype.Basic, Filter.Type.HasCardType CardType.Land])

-- Goblin Piker's SLUG, which Pawl.Registry.slugFor maps the card's own name onto
-- and which therefore fetches the card -- and which is not a name in CR 201.4's
-- reference, no printed card being called that. The gap between the two lookups
-- is what Pawl.Interpreter.legalCardName's exact face-name comparison closes.
pikerSlug :: CardName.CardName
pikerSlug = CardName.MkCardName (Text.pack "goblin-piker")

-- A name CR 201.4's reference does not have, which the refusal case needs one of.
-- Scryfall !"No Such Card", 2026-09-05, no hit; `data/cards/` has no file for it
-- either, and Pawl.Registry is the reference Pawl.Interpreter.legalCardName reads.
noSuchCard :: CardName.CardName
noSuchCard = CardName.MkCardName (Text.pack "No Such Card")

-- Cast the Chamber and let it resolve, answering both name choices.
--
-- CAST rather than S.addCreature, because the choice happens only on the entry
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
    -- CR 614.1c: "As [this permanent] enters . . ." is a replacement effect, and
    -- CR 614.12a makes its choice happen before the permanent enters. TWO
    -- choices, by two players, which is what no other as-enters card in the pool
    -- does -- Painter's Servant and Convincing Mirage each ask their controller
    -- and nobody else.
    Spec.it s "CR 614.1c both the controller and an opponent name a card as it enters" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName cancel
          after = castChamber S.bob picks board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber ->
          Spec.assertEqWith
            s
            "both names, and only those two"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName cancel])

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

    -- CR 201.4a: "If a player is instructed to choose a card name with certain
    -- characteristics, the player must choose the name of a card whose Oracle
    -- text matches those characteristics." Null Chamber's characteristics are
    -- "other than a basic land card name". The engine hands them to the prompt
    -- and does not judge the answer -- it cannot resolve a name at all -- so this
    -- case proves the half the engine owns: the restriction reaches the player
    -- being asked, unaltered, for BOTH choosers. The two cases after it prove the
    -- half the interpreter owns.
    --
    -- Nothing else in this group would notice its loss: the restriction is never
    -- written to the board, so authoring Filter.And [] -- the trivial predicate,
    -- which forbids nothing -- would leave every other case here green.
    Spec.it s "CR 201.4a the printed restriction reaches both choosers" $ do
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
      Spec.assertEqWith s "the card's own restriction, on both asks" (fmap snd asked) [nonBasicLandName, nonBasicLandName]

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

    -- CR 613.10 / PlayerScope.EachPlayer: neither printed prohibition names a
    -- player -- "Spells with the chosen names can't be cast and lands with the
    -- chosen names can't be played" -- so both are SYMMETRIC, and reach the
    -- Chamber's controller and its opponents alike.
    --
    -- THE DISCRIMINATING CASE for that, and the only one: every other case in
    -- this group asks about alice, who controls the Chamber, so narrowing either
    -- ability's scope to PlayerScope.You would leave them all green. Both halves
    -- are asked of bob here, each with its own before/after pair so that "bob
    -- cannot do it" cannot be satisfied by bob never having been able to.
    --
    -- A Lightning Bolt rather than a creature, because CR 304.1 lets bob cast an
    -- instant on alice's turn: what keeps it off his list after the Chamber
    -- lands is the name and not CR 307.1's sorcery-speed window.
    Spec.it s "CR 613.10 both prohibitions reach the opponent, not only the controller" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      ashBarrens <- S.printingOf s registry "Ash Barrens"
      let (oid, alices) = nullChamberBoard plains mountain nullChamber
          (_, bobHasMana) = S.addCreature mountain S.bob alices
          (bobsBolt, bobHasBolt) = S.addHandCard lightningBolt S.bob bobHasMana
          (bobsBarrens, before) = S.addHandCard ashBarrens S.bob bobHasBolt
          -- alice names the spell, bob names the land: each prohibition is then
          -- carried by a name its own chooser did not pick, which is the same
          -- symmetry read on the other axis.
          picks pid = if pid == S.alice then S.printingName lightningBolt else S.printingName ashBarrens
          after = castChamber S.bob picks before oid
          casts = Action.legalActions S.bob
      Spec.assertBool s (elem (Action.Type.Cast bobsBolt (S.printingName lightningBolt) Facing.FaceUp) (casts before)) "bob may cast his Bolt before the Chamber lands"
      Spec.assertBool s (notElem (Action.Type.Cast bobsBolt (S.printingName lightningBolt) Facing.FaceUp) (casts after)) "and may not once it has"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId (S.printingName lightningBolt) VariableChoice.Announced after) "bob is prohibited by alice's name"
      Spec.assertBool s (elem (bobsBarrens, Nothing) (Action.playableLands S.bob before)) "bob's land is playable before the Chamber lands"
      Spec.assertBool s (notElem (bobsBarrens, Nothing) (Action.playableLands S.bob after)) "and not once it has"

    -- CR 601.3's prohibit half, now carrying a QUALITY: "no rule or effect
    -- prohibits" is asked of one named spell rather than of casting in general.
    -- The Lightning Bolt is the falsifier -- a blanket prohibition, or one that
    -- compared nothing, would take it away too.
    Spec.it s "CR 601.3 a spell with the chosen name can't be cast, and its neighbour still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName cancel
          after = castChamber S.bob picks board oid
          (pikerId, withPiker) = S.addHandCard piker S.alice after
          (boltId, gs) = S.addHandCard lightningBolt S.alice withPiker
          offered = Action.legalActions S.alice gs
      Spec.assertBool s (notElem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) offered) "the named Piker is not offered"
      Spec.assertBool s (elem (Action.Type.Cast boltId (S.printingName lightningBolt) Facing.FaceUp) offered) "the unnamed Bolt still is"

    -- The OPPONENT's name binds the Chamber's controller, which is the half a
    -- one-chooser reading of the card would lose: bob names the Piker, and it is
    -- alice who may no longer cast one.
    Spec.it s "CR 601.3 the opponent's chosen name prohibits the controller too" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          -- The reverse of the case above: alice names something she is not
          -- holding, bob names the card she is.
          picks pid = if pid == S.alice then S.printingName cancel else S.printingName piker
          after = castChamber S.bob picks board oid
          (pikerId, gs) = S.addHandCard piker S.alice after
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId (S.printingName piker) VariableChoice.Announced gs) "alice is prohibited by bob's name"
      Spec.assertBool s (notElem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.alice gs)) "and no cast is offered"

    -- CR 305.1: playing a land is a SPECIAL ACTION that never uses the stack, so
    -- the land half of the card is a different gate from the cast half --
    -- Action.playableLands rather than Cast.castable.
    --
    -- The Plains is the falsifier, and it is also why the named land has to be a
    -- nonbasic one: the card forbids naming a basic land card and CR 201.4a is
    -- what makes that restriction binding, so a basic land
    -- is the one land this card can never stop.
    Spec.it s "CR 305.1 a land with the chosen name can't be played, and a basic land still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      ashBarrens <- S.printingOf s registry "Ash Barrens"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName ashBarrens else S.printingName cancel
          after = castChamber S.bob picks board oid
          (barrensId, withBarrens) = S.addHandCard ashBarrens S.alice after
          (plainsId, gs) = S.addHandCard plains S.alice withBarrens
          playable = Action.playableLands S.alice gs
      Spec.assertBool s (notElem (barrensId, Nothing) playable) "the named Ash Barrens is not playable"
      Spec.assertBool s (elem (plainsId, Nothing) playable) "the Plains still is"
      Spec.assertBool s (elem (Action.Type.Play plainsId Nothing) (Action.legalActions S.alice gs)) "and the Plains is offered"
      Spec.assertBool s (notElem (Action.Type.Play barrensId Nothing) (Action.legalActions S.alice gs)) "while the Barrens is not"

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
          Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId (S.printingName piker) VariableChoice.Announced gs) "prohibited while it stands"
          Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId (S.printingName piker) VariableChoice.Announced gone)) "not prohibited once it is gone"
          Spec.assertBool s (elem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.alice gone)) "and the cast is offered again"
          Spec.assertBool s (elem (barrensId, Nothing) (Action.playableLands S.alice gone)) "and the land may be played again"

    -- REJECT-NOT-REPAIR on the opponent answer, which only a three-seat board
    -- can reach: an answer naming somebody who is not an opponent -- here the
    -- Chamber's own controller -- falls back to the head of the offered list,
    -- the posture Sba.chooseLegendVictims takes toward an out-of-group legend.
    --
    -- THE FALSIFIER is the second name. An unfiltered answer would make alice
    -- both choosers, and since she is asked once the Chamber would enter with
    -- ONE name -- so the card would quietly prohibit half of what it says.
    Spec.it s "CR 102.2 an answer naming no opponent falls back to the head of the offer" $ do
      plains <- S.printingOf s registry "Plains"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = threeSeatBoard plains nullChamber
          picks pid
            | pid == S.alice = S.printingName piker
            | pid == S.bob = S.printingName cancel
            | otherwise = S.printingName lightningBolt
          -- alice controls the Chamber, so naming her names no opponent at all.
          after = castChamber S.alice picks board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber ->
          Spec.assertEqWith
            s
            "alice's name and bob's, bob being the head of [bob, carol]"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName cancel])

    -- "An opponent" is a choice the card leaves open and no rule assigns, so
    -- pawl gives it to the ability's controller -- CR 109.5's "you", the player
    -- the card's other half already names -- and at three seats that choice is
    -- real. The third player is asked NOTHING, which a reading of "you and an
    -- opponent" as the whole table (or as PlayerScope.EachPlayer, which
    -- coincides with the card at two seats) would get wrong.
    Spec.it s "CR 102.2 at three seats the controller picks which opponent names a card" $ do
      plains <- S.printingOf s registry "Plains"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = threeSeatBoard plains nullChamber
          -- Each seat names a different card, so chosenNames says exactly who
          -- was asked.
          picks pid
            | pid == S.alice = S.printingName piker
            | pid == S.bob = S.printingName cancel
            | otherwise = S.printingName lightningBolt
          after = castChamber S.carol picks board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber ->
          Spec.assertEqWith
            s
            "alice's name and carol's, and nothing bob named"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName lightningBolt])

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
  let put g printing = snd (S.addCreature printing S.alice g)
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

-- alice's next turn, as far as CR 305.2 can see it: her untap step, which is
-- where Engine.runTurnBasedActions resets the per-turn tally -- "during their
-- turn" in CR 305.2 has to start over somewhere, and that is the first moment of
-- the new one.
nextTurnFor :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
nextTurnFor pid gs =
  let untap = Phase.Beginning BeginningStep.Untap
      untapped = S.runPure S.identityAnswer (gs {GameState.activePlayer = pid, GameState.phase = untap}) (Engine.runTurnBasedActions untap)
   in untapped {GameState.phase = Phase.PrecombatMain, GameState.priority = Just pid, GameState.passes = 0}

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
      put (ids, g) printing = let (i, g1) = S.addCreature printing S.alice g in (ids <> [i], g1)
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
      withBobsLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.bob g)) base [1 .. 9 :: Int]
      (bobsPiker, withBobsPiker) = S.addHandCard piker S.bob withBobsLands
      withOrrery = snd (S.addCreature orrery owner withBobsPiker)
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
      withPiker = snd (S.addCreature piker S.alice base)
      (equipment, withEquipment) = S.addCreature bonesplitter S.alice withPiker
      withExtra = List.foldl' (\g printing -> snd (S.addCreature printing S.alice g)) withEquipment extra
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
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.PutCompanionIntoHand -> False
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
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

-- Runed Halo {W}{W} Enchantment: "As this enchantment enters, choose a card
-- name. You have protection from the chosen card name." The pool's one card that
-- gives a PLAYER a rule 702.16 protection ability, and so the producer of both
-- halves this group proves: CR 702.16b's targeting bar and CR 702.16c's
-- enchanting bar, each read off Pawl.Engine.PlayerEffect.protectedFrom.
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
      (_, withPiker) = S.addCreature piker S.bob (castHalo name base haloId)
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
-- CAST rather than S.addCreature, castChamber's reason: the choice happens only
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

-- castHalo, also recording WHO was asked to name a card. Invisible from the
-- finished board -- Object.chosenNames is a set of names and remembers no
-- chooser -- and it is the whole difference between rule 614.1c's one-chooser
-- rewrite and Null Chamber's two-chooser one above.
recordingCastHalo :: CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> [PlayerId.PlayerId]
recordingCastHalo name gs oid =
  let answer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
      answer p = case p of
        Prompt.ChooseCardName _ chooser _ _ -> do
          State.modify' (<> [chooser])
          pure name
        _ -> pure (S.identityAnswer p)
   in State.execState (Engine.runGame answer gs (S.cast S.alice oid >> Stack.resolveTop)) []

runedHaloSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
runedHaloSpec s registry =
  Spec.describe s "RunedHalo" $ do
    -- CR 614.1c with CR 201.4: the as-enters choice, and the one thing that
    -- tells EntryRewrite.ChooseCardName from EntryRewrite.ChooseCardNames beside
    -- it -- Runed Halo says "choose a card name" and names nobody else, where
    -- Null Chamber says "you and an opponent each choose".
    Spec.it s "CR 614.1c the controller alone names a card as the Halo enters" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      let (haloId, _, board) = runedHaloBoard plains halo curse
          after = castHalo (S.printingName curse) board haloId
          asked = recordingCastHalo (S.printingName curse) board haloId
      Spec.assertEqWith s "alice was asked, and nobody else" asked [S.alice]
      case enteredOne board after >>= \oid -> Game.lookupObject oid after of
        Nothing -> Spec.assertFailure s "Runed Halo did not reach the battlefield"
        Just entered ->
          Spec.assertEqWith
            s
            "one chosen name, and it is the one she picked"
            (Object.chosenNames entered)
            (Set.singleton (S.printingName curse))
    -- CR 702.16b's PLAYER half: "a permanent or player with protection can't be
    -- targeted by spells with the stated quality". CR 702.5a makes the enchant
    -- ability a targeting restriction, so the Curse's own target slot is where
    -- the rule lands.
    Spec.it s "CR 702.16b the protected player is not a legal target for a spell with the chosen name" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      piker <- S.printingOf s registry "Goblin Piker"
      let (haloId, curseId, board) = runedHaloBoard plains halo curse
          offered g = fmap (\theSlot -> Target.legalRecipients (Just S.bob) curseId theSlot g) (Card.enchantTargetSlot (S.combinedFace curse))
          named = castHalo (S.printingName curse) board haloId
          other = castHalo (S.printingName piker) board haloId
      Spec.assertEqWith s "with the Curse named, alice is off the Curse's target list" (fmap (Set.member (Recipient.ToPlayer S.alice)) (offered named)) (Just False)
      Spec.assertEqWith s "carol is still on it -- the Halo protects its controller alone" (fmap (Set.member (Recipient.ToPlayer S.carol)) (offered named)) (Just True)
      Spec.assertEqWith s "and with another card named, so is alice" (fmap (Set.member (Recipient.ToPlayer S.alice)) (offered other)) (Just True)
    -- CR 702.16c's second sentence, the clause that had nowhere to live until
    -- a player could carry protection (see #2387): "such
    -- Auras attached to the permanent or player with protection will be put into
    -- their owners' graveyards as a state-based action" (CR 704.5m).
    --
    -- The Curse is attached BEFORE the Halo enters, which is the order the rule
    -- is about: an Aura already there when the protection starts to apply falls
    -- off on the next pass.
    Spec.it s "CR 702.16c / 704.5m an Aura already enchanting the player is buried once she gains protection from its name" $ do
      plains <- S.printingOf s registry "Plains"
      halo <- S.printingOf s registry "Runed Halo"
      curse <- S.printingOf s registry "Curse of Vitality"
      piker <- S.printingOf s registry "Goblin Piker"
      let (haloId, _, board) = runedHaloBoard plains halo curse
          (aura, withAura) = S.addCreature curse S.bob board
          cursed = S.attachTo aura (Recipient.ToPlayer S.alice) withAura
          named = S.settleSba (castHalo (S.printingName curse) cursed haloId)
          other = S.settleSba (castHalo (S.printingName piker) cursed haloId)
      Spec.assertBool s (Set.member aura (GameState.battlefield (S.settleSba cursed))) "before the Halo the Curse is legally attached to alice"
      Spec.assertBool s (not (Set.member aura (GameState.battlefield named))) "the Halo names it, and it is off the battlefield after one pass"
      Spec.assertEqWith s "in its OWNER's graveyard, and bob owns it" (length (Game.zoneMembers Zone.Graveyard S.bob named)) 1
      Spec.assertBool s (Set.member aura (GameState.battlefield other)) "with another card named it stays where it is"
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

-- Conjurer's Ban {W}{B} Sorcery: "Choose a card name. Until your next turn,
-- spells with the chosen name can't be cast and lands with the chosen name can't
-- be played. Draw a card."
--
-- The STORED twin of nullChamberSpec above, and the whole reason this group
-- exists: it is the pool's only writer of either chosen-name arm as a resolution
-- (CR 611.2c) rather than as a permanent's printed ability, so it is the only
-- card that reaches PlayerEffect.chosenNamesOf through a GameState.playerEffects
-- row. The name is chosen by CR 201.4 during the resolution (Effect
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

conjurersBanSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId (S.printingName piker) VariableChoice.Announced after) "and the prohibition is the named one"
      -- The carrier, asserted AFTER the behaviour: nothing this card makes is a
      -- permanent, so both rows are CR 611.2c stored ones.
      Spec.assertEqWith s "two stored rows" (length (GameState.playerEffects after)) 2

    -- CR 305.1: the land half is a special action and a different gate
    -- (Action.playableLands), exactly as Null Chamber's is. The Plains is the
    -- falsifier.
    Spec.it s "CR 305.1 a land with the chosen name can't be played, and an unnamed one still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      swamp <- S.printingOf s registry "Swamp"
      ban <- S.printingOf s registry "Conjurer's Ban"
      ashBarrens <- S.printingOf s registry "Ash Barrens"
      let (oid, board) = conjurersBanBoard plains mountain swamp ban
          (barrensId, withBarrens) = S.addHandCard ashBarrens S.alice board
          (plainsId, before) = S.addHandCard plains S.alice withBarrens
          after = castBan (S.printingName ashBarrens) before oid
      Spec.assertBool s (elem (barrensId, Nothing) (Action.playableLands S.alice before)) "alice may play her Ash Barrens before the Ban resolves"
      Spec.assertBool s (notElem (barrensId, Nothing) (Action.playableLands S.alice after)) "and may not once it has"
      Spec.assertBool s (elem (plainsId, Nothing) (Action.playableLands S.alice after)) "the unnamed Plains still is playable"
      Spec.assertBool s (notElem (Action.Type.Play barrensId Nothing) (Action.legalActions S.alice after)) "and no Play is offered for the Barrens"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PlayerEffect" $ do
  nullChamberSpec s registry
  runedHaloSpec s registry
  conjurersBanSpec s registry
  silenceSpec s registry
  ceaseFireSpec s registry
  blossomingCalmSpec s registry
  conditionalSilenceSpec s registry
  hackedSilenceSpec s registry
  lilianaSpec s registry
  matchesObjectSpec s registry
