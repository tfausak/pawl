-- Covers Pawl.Engine.Condition, Pawl.Types.Condition and Pawl.Types.Comparison,
-- including what Condition.holds makes of Pawl.Engine.Quantity's IsMonarch,
-- EnteredThisTurn, EnteredFrom and WasCastFrom.
module Pawl.ConditionSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Count every battlefield object; the stub view decides how many match.
everyPermanent :: Count.Type.Count Quantity.Type.Quantity
everyPermanent =
  Count.Type.MkCount
    (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
    (Filter.Type.And [])
    Aggregation.Members

-- n Swamps on the battlefield, and a ViewOf (via S.stubView, Pawl.Support's
-- second consumer per Task 3) describing each of them.
boardOf :: Printing.Printing -> Integer -> (Count.ViewOf, GameState.GameState)
boardOf swamp n =
  let gs0 = Setup.emptyGame S.bothPlayers
      step (ids, g) _ =
        let (oid, g') = S.addCreature swamp S.alice g
         in (ids <> [oid], g')
      (oids, gs) = foldl step ([], gs0) [1 .. n]
      table = fmap (\oid -> (oid, Set.empty, Set.singleton Subtype.Swamp, Just S.alice)) oids
   in (S.stubView table, gs)

context :: Filter.Context
context = Filter.contextFor (Just S.alice) (Just (ObjectId.MkObjectId 0))

check :: Printing.Printing -> Integer -> Comparison.Comparison -> Integer -> Bool
check swamp n comparison threshold =
  let (viewOf, gs) = boardOf swamp n
   in Condition.holds
        viewOf
        context
        gs
        (ObjectId.MkObjectId 0)
        (Condition.Type.Compares (Compares.MkCompares (Quantity.Type.Count everyPermanent) comparison (Quantity.Type.Literal threshold)))

-- Queen Marchesa's upkeep trigger: "if an opponent is the monarch" is
-- Quantity.IsMonarch (Relative Opponent), which names EVERY opponent -- one
-- player on two seats, two on three. CR 725.3 makes the monarch unique, so the
-- honest reading is a disjunction over them.
monarchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monarchSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Alice's upkeep with Queen Marchesa already out, on `seats`, after
      -- `crown` has settled the monarchy.
      upkeepWith marchesa seats crown =
        let (_, gs0) = S.addCreature marchesa S.alice (Setup.emptyGame seats)
         in resolveAll (settle (beginUpkeep (crown gs0)))
      noToken after = Spec.assertEqWith s "no token was created" (S.tokensOf after) []
      oneAssassin after = case S.tokensOf after of
        [tok] -> do
          Spec.assertEqWith s "1/1" (Projection.powerOf tok after, Projection.toughnessOf tok after) (Just 1, Just 1)
          Spec.assertEqWith s "black" (Projection.colorsOf tok after) (Set.singleton Color.Black)
          Spec.assertBool s (Set.member Subtype.Assassin (Projection.subtypesOf tok after)) "an Assassin"
          Spec.assertEqWith s "deathtouch and haste" (Map.keysSet (Projection.keywordsOf tok after)) (Set.fromList [Keyword.Deathtouch, Keyword.Haste])
          Spec.assertEqWith s "alice's" (Projection.controllerOf tok after) (Just S.alice)
        other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
   in Spec.describe s "IsMonarch" $ do
        -- Three seats: Relative Opponent names bob AND carol, which is the whole
        -- bug. On two seats the pre-change code already passed.
        Spec.it s "CR 725.3 an opponent is the monarch on three seats" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          oneAssassin (upkeepWith marchesa S.threePlayers (S.withMonarch S.carol))
        -- The same board with the crown moved to alice: this is what says the
        -- disjunction did not degenerate into "is there a monarch?".
        Spec.it s "CR 603.4 the controller holding the crown makes the clause false" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          noToken (upkeepWith marchesa S.threePlayers (S.withMonarch S.alice))
        -- Regression fence for the existing CR 725.5 arm: no monarch answers 0,
        -- not "undeterminable".
        Spec.it s "CR 725.5 no monarch at all makes the clause false" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          noToken (upkeepWith marchesa S.threePlayers id)
        -- Two seats, where the old arity restriction already answered: the fix
        -- must not move this.
        Spec.it s "CR 725.3 the two-seat answer is unchanged" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          oneAssassin (upkeepWith marchesa S.bothPlayers (S.withMonarch S.bob))
        -- The whole card, crowned by its own resolved ETB rather than by a
        -- fixture write.
        Spec.it s "CR 725.1 her enters trigger crowns her controller, so no token follows" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          let (oid, gs0) = S.addCreature marchesa S.alice (Setup.emptyGame S.threePlayers)
              entered = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
              crowned = resolveAll (settle (S.withEvents [GameEvent.Moved (Moved.moved entered (Projection.project oid gs0))] gs0))
          Spec.assertEqWith s "alice is the monarch" (GameState.monarch crowned) (Just S.alice)
          noToken (resolveAll (settle (beginUpkeep crowned)))

-- Thrasta, Tempest's Roar's "Thrasta has hexproof as long as it entered this
-- turn": a CR 604.1 static ability whose CR 604.2 clause is
-- Quantity.EnteredThisTurn compared against 1, granting CR 702.11b's hexproof to
-- the permanent itself.
--
-- ONE BOARD, TWO GAMES. Alice hard-casts Thrasta for its printed {10}{G}{G} off
-- twelve Forests, and bob's Doom Blade is offered a target set twice: on the turn
-- Thrasta arrives, and after Engine.handoffTurn. That handoff is the ONLY
-- difference between the two -- same seats, same lands, same spell, same
-- permanent, same id -- and it is what clears the event log the measurement reads
-- (Engine.beginTurnOf), so a reading that answered off anything else (a
-- timestamp, summoning sickness, "the most recent permanent") cannot tell the two
-- games apart.
--
-- A Goblin Piker stands beside Thrasta throughout as the within-board control: it
-- is on the battlefield without having entered (S.addCreature files no zone
-- change), so it is a legal target in BOTH games. Without it the negative could
-- pass because the offer was empty -- a Doom Blade that reached nothing at all.
--
-- The keyword assertions are the direct reading of the static ability, and the
-- targeting ones are what the rule is for; both are made, because a projection
-- that granted hexproof and a targeting check that ignored it would each pass
-- half of this.
enteredThisTurnSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
enteredThisTurnSpec s registry = Spec.describe s "EnteredThisTurn" $ do
  Spec.it s "CR 604.2 Thrasta has hexproof the turn it enters, and loses it at the handoff" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, staged) = S.addCreature piker S.alice (S.landsInPlay forest 12)
        (start, spellId) = S.handOne thrasta staged
        cast = snd (Engine.runGamePure S.identityAnswer start (S.cast S.alice spellId))
        arrived = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- One whole turn later, through the real handoff -- the same idiom
        -- Pawl.ExpirySpec's `handoff` uses.
        later = S.runPure S.identityAnswer arrived Engine.handoffTurn
        named gs = [oid | oid <- Set.toList (GameState.battlefield gs), S.soleFaceName oid gs == S.printingName thrasta]
        hexproof gs oid = Map.member (Keyword.Hexproof Nothing) (Projection.keywordsOf oid gs)
        reaches gs oid = case S.spellTargetSlot doomBlade of
          Nothing -> False
          Just theSlot ->
            let (blade, onStack) = S.spellOnStack doomBlade S.bob gs
             in Set.member (Recipient.ToCreature oid) (Target.legalRecipients (Just S.bob) blade theSlot onStack)
    case named arrived of
      [thrastaId] -> do
        Spec.assertBool s (elem thrastaId (named later)) "CR 400.7: the same incarnation is still there next turn"
        Spec.assertBool s (hexproof arrived thrastaId) "the turn it entered, Thrasta has hexproof"
        Spec.assertBool s (not (hexproof later thrastaId)) "and next turn it does not"
        Spec.assertBool s (not (reaches arrived thrastaId)) "CR 702.11b: bob's Doom Blade cannot target it the turn it entered"
        Spec.assertBool s (reaches later thrastaId) "and can on the next turn"
        Spec.assertBool s (reaches arrived pikerId) "the Piker beside it was targetable all along"
        Spec.assertBool s (reaches later pikerId) "in both games"
      other -> Spec.assertFailure s ("expected exactly one Thrasta on the battlefield, got " <> show (length other))

-- Archfiend's Vessel: "When this creature enters, if it entered from your
-- graveyard or you cast it from your graveyard, exile it. If you do, create a 5/5
-- black Demon creature token with flying." CR 603.4's intervening "if" as a
-- Condition.Any of Quantity.EnteredFrom and Quantity.WasCastFrom, each naming
-- Zone.Graveyard scoped to the ability's controller.
--
-- Every case turns on the DEMON, which is the gameplay-level reading: the clause
-- is what decides whether the trigger fires at all, and the token is the only
-- thing the resolution puts on the board.
enteredFromSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
enteredFromSpec s registry =
  let cast pid oid gs = S.runPure S.identityAnswer gs (S.cast pid oid)
      resolveTop gs = S.runPure S.identityAnswer gs Stack.resolveTop
      settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      -- Cast it and drain the stack, settling before each resolution so CR 603.3
      -- has placed whatever the last one triggered. Draining rather than
      -- resolving twice is what lets the DEMON COUNT answer: two Vessels
      -- triggering leave two abilities on the stack, and a single resolveTop
      -- would run one of them and read the same one token either way.
      drain n gs =
        let settled = settle gs
         in if n <= (0 :: Int) || null (GameState.stack settled)
              then settled
              else drain (n - 1) (resolveTop settled)
      play pid oid gs = drain 8 (cast pid oid gs)
      ownerOf oid gs = fmap Object.owner (Map.lookup oid (GameState.objects gs))
      vesselsOwnedBy pid vessel gs =
        [ oid
        | oid <- Set.toList (GameState.battlefield gs),
          S.soleFaceName oid gs == S.printingName vessel,
          ownerOf oid gs == Just pid
        ]
      oneDemon after = case S.tokensOf after of
        [tok] -> do
          Spec.assertEqWith s "5/5" (Projection.powerOf tok after, Projection.toughnessOf tok after) (Just 5, Just 5)
          Spec.assertEqWith s "black" (Projection.colorsOf tok after) (Set.singleton Color.Black)
          Spec.assertBool s (Set.member Subtype.Demon (Projection.subtypesOf tok after)) "a Demon"
          Spec.assertEqWith s "flying" (Map.keysSet (Projection.keywordsOf tok after)) (Set.singleton Keyword.Flying)
        other -> Spec.assertFailure s ("expected exactly one Demon token, got " <> show (length other))
   in Spec.describe s "EnteredFrom" $ do
        -- ONE BOARD, ONE EVENT. Rise of the Dark Realms puts every creature card
        -- in EVERY graveyard onto the battlefield under ALICE's control, so the
        -- two Vessels enter simultaneously, under one controller, off one
        -- resolution. The only thing that differs is whose graveyard each came
        -- out of, which CR 400.3 makes its owner's -- so this is the "your" in
        -- "your graveyard" and nothing else.
        Spec.it s "CR 400.3 only the Vessel raised from YOUR graveyard triggers" $ do
          swamp <- S.printingOf s registry "Swamp"
          vessel <- S.printingOf s registry "Archfiend's Vessel"
          rise <- S.printingOf s registry "Rise of the Dark Realms"
          let (start, riseId) = S.handOne rise (S.landsInPlay swamp 9)
              staged = snd (S.addGraveyardCard vessel S.bob (snd (S.addGraveyardCard vessel S.alice start)))
              after = play S.alice riseId staged
          oneDemon after
          Spec.assertEqWith s "CR 603.4 alice's Vessel exiled itself" (length (vesselsOwnedBy S.alice vessel after)) 0
          -- The board, recorded after the behaviour: bob's Vessel is alice's
          -- permanent, so the two abilities had ONE controller and only the
          -- graveyard each came out of told them apart.
          case vesselsOwnedBy S.bob vessel after of
            [bobs] -> Spec.assertEqWith s "and bob's is on the battlefield under alice's control" (Projection.controllerOf bobs after) (Just S.alice)
            other -> Spec.assertFailure s ("expected exactly one Vessel bob owns on the battlefield, got " <> show (length other))

        -- TWO GAMES, ONE DIFFERENCE: the zone the Vessel starts in. Yawgmoth's
        -- Will is resolved first in BOTH, so the same permission, the same mana
        -- and the same cast happen either way -- only the origin differs. The
        -- Vessel enters from the STACK in both, which is what makes this the
        -- WasCastFrom half rather than the EnteredFrom one.
        Spec.it s "CR 601.2a a Vessel cast from your graveyard triggers" $ do
          swamp <- S.printingOf s registry "Swamp"
          vessel <- S.printingOf s registry "Archfiend's Vessel"
          will <- S.printingOf s registry "Yawgmoth's Will"
          let (start, willId) = S.handOne will (S.landsInPlay swamp 4)
              (vesselId, staged) = S.addGraveyardCard vessel S.alice start
              permitted = drain 8 (cast S.alice willId staged)
              after = play S.alice vesselId permitted
          Spec.assertBool s (S.castable S.alice vesselId permitted) "Yawgmoth's Will made the Vessel castable from the graveyard"
          oneDemon after

        Spec.it s "and the same cast from HAND does not" $ do
          swamp <- S.printingOf s registry "Swamp"
          vessel <- S.printingOf s registry "Archfiend's Vessel"
          will <- S.printingOf s registry "Yawgmoth's Will"
          let (start, willId) = S.handOne will (S.landsInPlay swamp 4)
              (vesselId, staged) = S.addHandCard vessel S.alice start
              permitted = drain 8 (cast S.alice willId staged)
              after = play S.alice vesselId permitted
          Spec.assertEqWith s "CR 603.4 the clause is false, so no Demon" (length (S.tokensOf after)) 0
          Spec.assertEqWith s "and the Vessel stayed on the battlefield" (length (vesselsOwnedBy S.alice vessel after)) 1

-- Breathless Knight {1}{W}{B} Creature -- Spirit Knight 2/2 (oracle text checked
-- against Scryfall, 2026-08-30): "Flying, lifelink. Whenever this creature or
-- another creature you control enters, if that creature entered from a graveyard
-- or you cast it from a graveyard, put a +1/+1 counter on this creature."
--
-- The family's OTHER reading of CR 603.4, and the one Archfiend's Vessel above
-- cannot supply. The Vessel's clause and its effect name the same object, so a
-- Vessel that left the battlefield fails CR 603.6's find and makes no Demon
-- whichever way the clause was read. The Knight's clause is about the ENTRANT --
-- Quantity.AgainstSlot at Binding.became -- while its effect acts on the KNIGHT,
-- so CR 603.6 stops nothing and killing the entrant between CR 603.4's two checks
-- separates a log read from a live-board one. That is the second case below.
--
-- Which way the rule goes: the log read is CR 603.4's answer and not merely
-- pawl's. "Entered from a graveyard" is a completed action, and CR 608.2i lets a
-- check needing information about a previous game action find its object wherever
-- it now is, so long as the check takes no action on it -- this one takes its
-- action on the Knight. Nothing a board can do makes a past entry un-happen, so
-- for this clause CR 603.4's second check can never answer differently from its
-- first. The observable fact is therefore not that the two checks disagree but
-- that the second reads the LOG: kill the entrant and the counter still lands.
--
-- Stricter than printed in one half. Pawl.Types.InZone carries ONE PlayerRef and
-- Pawl.Engine.Quantity's WasCastFrom arm asks it of the caster and of the
-- entrant's owner alike, so "you cast it from A graveyard" has no exact spelling:
-- PlayerRef.EachPlayer would read "anyone cast it" (weaker) and PlayerRef.Relative
-- You reads "you cast it from your graveyard" (stricter). The card takes the
-- stricter one, so a creature cast out of an OPPONENT's graveyard does not grow
-- the Knight (#2689). The EnteredFrom half needs no such call and is exact:
-- EachPlayer makes its owner conjunct vacuous, which is the printed "a
-- graveyard", and every case below drives that half. The last case drives the
-- WasCastFrom disjunct's negative -- the Skeleton is cast from a HAND -- and
-- Archfiend's Vessel above is where its positive is proved.
--
-- Reassembling Skeleton supplies the entrant: "{1}{B}: Return this card from your
-- graveyard to the battlefield tapped" is one activation, so the entry comes out
-- of a graveyard with no reanimation spell in the way, and its 1 toughness is what
-- lets one marked damage kill it while the trigger waits.
interveningRecheckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
interveningRecheckSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveTop gs = S.runPure S.identityAnswer gs Stack.resolveTop
      -- alice, holding priority in her own main phase, with the Knight settled on
      -- the battlefield and two untapped Swamps -- the {1}{B} both the activation
      -- and the cast below are paid with, so the three boards differ only in where
      -- the Skeleton starts and whether it is killed.
      board knight swamp =
        let (knightId, withKnight) = S.addCreature knight S.alice (S.landsInPlay swamp 2)
         in ( knightId,
              withKnight
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      -- CR 400.7 mints a fresh id for the returned card, so the entrant is found
      -- by name rather than by the id the ability was activated from.
      skeletonsOn skeleton gs =
        [ oid
        | oid <- Set.toList (GameState.battlefield gs),
          S.soleFaceName oid gs == S.printingName skeleton
        ]
      sizeOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)
      -- One damage on a 1/1 and a state-based-action pass: CR 704.5g destroys the
      -- entrant where it stands, with the Knight's trigger already on the stack
      -- and untouched.
      kill oid gs = S.settleSba (S.markDamage oid 1 gs)
      -- Activate the Skeleton's graveyard ability, resolve it, and settle so CR
      -- 603.3 has placed whatever the entry triggered.
      raiseWith skeleton knight swamp k = do
        let (knightId, base) = board knight swamp
            (gyId, staged) = S.addGraveyardCard skeleton S.alice base
        case Activate.abilitiesFor gyId staged of
          [ability] -> k knightId (settle (resolveTop (S.runPure S.identityAnswer staged (Activate.activateAbility S.alice gyId ability))))
          abilities -> Spec.assertEqWith s "exactly one ability to activate" (length abilities) 1
   in Spec.describe s "InterveningRecheck" $ do
        -- The control the killed board is read against: the same entry, the same
        -- trigger, nothing killed. If this were red the case below would prove
        -- nothing about the second check.
        Spec.it s "CR 603.4 a creature returning from your graveyard grows the Knight" $ do
          knight <- S.printingOf s registry "Breathless Knight"
          skeleton <- S.printingOf s registry "Reassembling Skeleton"
          swamp <- S.printingOf s registry "Swamp"
          raiseWith skeleton knight swamp $ \knightId raised -> do
            Spec.assertEqWith s "the Knight took the +1/+1 counter" (sizeOf knightId (resolveTop raised)) (Just 3, Just 3)
            Spec.assertEqWith s "off exactly one trigger" (length (GameState.stack raised)) 1
            Spec.assertEqWith s "and the entrant was still there" (length (skeletonsOn skeleton raised)) 1

        -- THE CASE THIS GROUP EXISTS FOR. One difference from the control: the
        -- entrant is killed with the trigger on the stack, so CR 603.4's second
        -- check runs with the object its clause is about already gone. A
        -- live-board reading answers "it did not enter from a graveyard, there is
        -- no it" and the counter never lands; the log reading CR 608.2i asks for
        -- still answers yes.
        Spec.it s "CR 608.2i the entrant killed between the two checks still grows the Knight" $ do
          knight <- S.printingOf s registry "Breathless Knight"
          skeleton <- S.printingOf s registry "Reassembling Skeleton"
          swamp <- S.printingOf s registry "Swamp"
          raiseWith skeleton knight swamp $ \knightId raised ->
            case skeletonsOn skeleton raised of
              [entrant] -> do
                let killed = kill entrant raised
                Spec.assertEqWith s "the Knight took the +1/+1 counter anyway" (sizeOf knightId (resolveTop killed)) (Just 3, Just 3)
                -- The board, recorded after the behaviour: the entrant really was
                -- gone and the trigger really was still waiting, so the read above
                -- happened at the second check and not the first.
                Spec.assertEqWith s "the entrant was off the battlefield first" (skeletonsOn skeleton killed) []
                Spec.assertEqWith s "with its trigger still on the stack" (length (GameState.stack killed)) 1
              entrants -> Spec.assertEqWith s "exactly one Skeleton entered" (length entrants) 1

        -- The negative, one difference from the case above: the Skeleton starts in
        -- alice's HAND, so the same {1}{B} casts it and it enters from the STACK.
        -- Killed the same way, so "no counter" cannot be the kill's doing.
        Spec.it s "CR 603.4 a creature entering from anywhere else does not" $ do
          knight <- S.printingOf s registry "Breathless Knight"
          skeleton <- S.printingOf s registry "Reassembling Skeleton"
          swamp <- S.printingOf s registry "Swamp"
          let (knightId, base) = board knight swamp
              (handId, staged) = S.addHandCard skeleton S.alice base
              raised = settle (resolveTop (S.runPure S.identityAnswer staged (S.cast S.alice handId)))
          case skeletonsOn skeleton raised of
            [entrant] -> do
              let killed = kill entrant raised
              Spec.assertEqWith s "the Knight is still 2/2" (sizeOf knightId (resolveTop killed)) (Just 2, Just 2)
              Spec.assertEqWith s "because CR 603.4's clause was false, so nothing triggered" (length (GameState.stack raised)) 0
            entrants -> Spec.assertEqWith s "exactly one Skeleton entered" (length entrants) 1

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Condition" $ do
  Spec.describe s "Exactly" $ do
    Spec.it s "CR 603.8 holds when the count equals the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 0 Comparison.Exactly 0) "0 == 0"

    Spec.it s "CR 603.8 fails when the count differs" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (not (check swamp 1 Comparison.Exactly 0)) "1 /= 0"

  Spec.describe s "AtLeast" $ do
    Spec.it s "holds at the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 3 Comparison.AtLeast 3) "3 >= 3"

    Spec.it s "fails below the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (not (check swamp 2 Comparison.AtLeast 3)) "2 < 3"

  Spec.describe s "AtMost" $ do
    Spec.it s "holds below the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 0 Comparison.AtMost 1) "0 <= 1"

    Spec.it s "holds at the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 1 Comparison.AtMost 1) "1 <= 1"

    Spec.it s "fails above the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (not (check swamp 2 Comparison.AtMost 1)) "2 > 1"

  Spec.describe s "an undeterminable side is false, never true" $ do
    Spec.it s "when the MEASURED side cannot be evaluated" $ do
      -- Relative with no perspective: Count.evaluate is Nothing, and a
      -- total holds must collapse that to False (CR 611.2b's conservative
      -- reading), not to a vacuous True.
      swamp <- S.printingOf s registry "Swamp"
      let (viewOf, gs) = boardOf swamp 0
          count =
            Count.Type.MkCount
              (Scope.InZone (InZone.MkInZone Zone.Hand (PlayerRef.Relative PlayerRelation.You)))
              (Filter.Type.And [])
              Aggregation.Members
      Spec.assertBool
        s
        ( not $
            Condition.holds
              viewOf
              (Filter.contextFor Nothing Nothing)
              gs
              (ObjectId.MkObjectId 0)
              (Condition.Type.Compares (Compares.MkCompares (Quantity.Type.Count count) Comparison.Exactly (Quantity.Type.Literal 0)))
        )
        "false"

    Spec.it s "when the THRESHOLD side cannot be evaluated" $ do
      -- Quantity.Type.InSlot Binding.variableX with no binding on the object:
      -- same collapse.
      swamp <- S.printingOf s registry "Swamp"
      let (viewOf, gs) = boardOf swamp 0
      Spec.assertBool
        s
        ( not $
            Condition.holds
              viewOf
              context
              gs
              (ObjectId.MkObjectId 0)
              (Condition.Type.Compares (Compares.MkCompares (Quantity.Type.Count everyPermanent) Comparison.Exactly (Quantity.Type.InSlot Binding.variableX)))
        )
        "false"

  monarchSpec s registry
  enteredThisTurnSpec s registry
  enteredFromSpec s registry
  interveningRecheckSpec s registry
