{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Condition, Pawl.Types.Condition and Pawl.Types.Comparison,
-- including what Condition.holds makes of Pawl.Engine.Quantity's IsMonarch,
-- EnteredThisTurn, EnteredFrom, WasCastFrom, WasToken, WasBlocking,
-- DamageDealtToThisTurn, WasBlockedThisTurn and TimesResolvedThisTurn.
module Pawl.ConditionSpec where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Teams as Teams
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
        let (oid, g2) = S.addPermanent swamp S.alice g
         in (ids <> [oid], g2)
      (oids, gs) = List.foldl' step ([], gs0) [1 .. n]
      table = fmap (\oid -> (oid, Set.empty, Set.singleton Subtype.Swamp, Just S.alice)) oids
   in (S.stubView table, gs)

context :: Filter.Context
context = Filter.contextFor Teams.none (Just S.alice) (Just (ObjectId.MkObjectId 0))

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
        let (_, gs0) = S.addPermanent marchesa S.alice (Setup.emptyGame seats)
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
          let (oid, gs0) = S.addPermanent marchesa S.alice (Setup.emptyGame S.threePlayers)
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
-- is on the battlefield without having entered (S.addPermanent files no zone
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
    let (pikerId, staged) = S.addPermanent piker S.alice (S.landsInPlay forest 12)
        (start, spellId) = S.handOne thrasta staged
        cast = snd (Engine.runGamePure S.identityAnswer start (S.cast S.alice spellId))
        arrived = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- One whole turn later, through the real handoff -- the same idiom
        -- Pawl.ExpirySpec's `handoff` uses.
        later = S.runPure S.identityAnswer arrived Engine.handoffTurn
        named gs = filter (\oid -> S.soleFaceName oid gs == S.printingName thrasta) (Set.toList (GameState.battlefield gs))
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
        filter
          (\oid -> S.soleFaceName oid gs == S.printingName vessel && ownerOf oid gs == Just pid)
          (Set.toList (GameState.battlefield gs))
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
-- The two disjuncts take their references for different reasons. The EnteredFrom
-- half is exact on its face: PlayerRef.EachPlayer makes its owner conjunct
-- vacuous, which is the printed "a graveyard", and every case below drives that
-- half. The WasCastFrom half is caster PlayerRef.Relative You over
-- PlayerRef.EachPlayer's graveyards -- "you cast it", out of anybody's pile --
-- which is what Pawl.Types.CastFrom's two references are for; foreignGraveyardCastSpec
-- below is where the two halves name different seats and the reading is proved.
-- The last case here drives the WasCastFrom disjunct's negative -- the Skeleton is
-- cast from a HAND -- and Archfiend's Vessel above is where its positive is proved.
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
        let (knightId, withKnight) = S.addPermanent knight S.alice (S.landsInPlay swamp 2)
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
        filter (\oid -> S.soleFaceName oid gs == S.printingName skeleton) (Set.toList (GameState.battlefield gs))
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
        case Activatable.abilitiesFor gyId staged of
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

-- Pins the trigger's target to one card by FILTERING the offered set, takes CR
-- 608.2g's "may", and attacks bob with everything otherwise.
--
-- The target is filtered rather than built, so CR 608.2b's re-read at resolution
-- cannot drop it, and it is pinned by identity rather than searched for: an
-- answerer picking whatever was legal would find the same card again after a
-- mutation.
stealing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
stealing wanted p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just wanted) . Recipient.objectOf) legal) sets
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  _ -> S.attackTo S.bob p

-- CR 601.2a's caster and CR 400.3's zone owner on DIFFERENT seats: the board
-- Pawl.Engine.Quantity's WasCastFrom arm needed and pawl could not build until
-- Tinybones, the Pickpocket landed. alice's Tinybones deals combat damage to bob
-- and casts an Archfiend's Vessel out of BOB's graveyard, so the spell's caster
-- is alice and the graveyard -- and by CR 400.3 the card -- is bob's.
--
-- TWO READERS ON ONE BOARD, answering opposite ways off that one cast:
--
--   * Breathless Knight's "you cast it from A graveyard" is caster You over
--     PlayerRef.EachPlayer's graveyards, and it holds -- the Knight takes its
--     +1/+1 counter.
--   * The Vessel's own "you cast it from YOUR graveyard" is caster You over your
--     own graveyard, and it does not -- no Demon token.
--
-- One reference could not have told them apart: naming alice it would have made
-- both false, naming the table both true, and the pair is what makes a fix that
-- merely widens the reference visible. The EnteredFrom disjunct each card carries
-- beside its WasCastFrom one is false either way: a permanent spell enters the
-- battlefield out of the STACK (CR 608.3), not out of any graveyard.
foreignGraveyardCastSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
foreignGraveyardCastSpec s registry =
  Spec.describe s "ForeignGraveyardCast"
    . Spec.it s "CR 601.2a a creature cast out of ANOTHER player's graveyard grows the Knight"
    $ do
      swamp <- S.printingOf s registry "Swamp"
      tinybones <- S.printingOf s registry "Tinybones, the Pickpocket"
      knight <- S.printingOf s registry "Breathless Knight"
      vessel <- S.printingOf s registry "Archfiend's Vessel"
      amalgam <- S.printingOf s registry "Prized Amalgam"
      let (combat, mine, _) = S.combatBoardOf [tinybones, knight] []
          (stolen, staged) = S.addGraveyardCard vessel S.bob (S.landsFor swamp S.alice 2 combat)
          after = S.runCombat (stealing stolen) (snd (S.addGraveyardCard amalgam S.bob staged))
          vessels owner =
            filter
              (\oid -> S.soleFaceName oid after == S.printingName vessel && fmap Object.owner (Map.lookup oid (GameState.objects after)) == Just owner)
              (Set.toList (GameState.battlefield after))
      case mine of
        [_, knightId] -> do
          Spec.assertEqWith s "CR 603.4 the Knight grew, so it read the cast out of bob's graveyard" (Projection.powerOf knightId after, Projection.toughnessOf knightId after) (Just 3, Just 3)
          -- The same cast, read by the card whose clause names ONE graveyard.
          -- Without this a fix that widened the reference instead of splitting it
          -- would pass the assertion above.
          Spec.assertEqWith s "CR 603.4 and the Vessel's own YOUR graveyard clause was false, so no Demon" (length (S.tokensOf after)) 0
          -- CR 601.2a's CASTER, on the one board where it comes apart from the
          -- zone's owner. bob's own Prized Amalgam reads "you cast it from your
          -- graveyard" -- his graveyard, and the card was cast out of it, but by
          -- ALICE. Its other two conjuncts both hold, which leaves the caster the
          -- only one that can answer no, and its delayed ability is what an
          -- answer of yes would have armed.
          Spec.assertEqWith s "CR 601.2a bob did not cast it, so his Amalgam armed nothing" (Seq.length (GameState.delayedTriggers after)) 0
          -- The board, recorded after the behaviour: the cast really happened and
          -- really was out of a pile that is not alice's, so the negatives above
          -- are the clauses' answers rather than a cast that never took place.
          case vessels S.bob of
            [taken] -> Spec.assertEqWith s "CR 400.3 the Vessel bob owns is alice's permanent" (Projection.controllerOf taken after) (Just S.alice)
            other -> Spec.assertFailure s ("expected exactly one Vessel bob owns on the battlefield, got " <> show (length other))
        other -> Spec.assertFailure s ("expected alice's two creatures, got " <> show (length other))

-- Aims an "up to one" target slot at `victim` by FILTERING the offered set, so a
-- hand-built recipient cannot miss CR 608.2b's re-read. S.identityAnswer declines,
-- which for a least-of-zero slot is a legal answer of no target at all.
bouncing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
bouncing victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter ((== Just victim) . Recipient.objectOf) candidates) sets
  _ -> S.identityAnswer p

-- S.aggressiveAnswer with its block declaration switched off when `blocks` is
-- False; every other prompt is answered identically, so the pair of boards below
-- differs in CR 509.1's declaration and nothing else.
combatAnswer :: Bool -> Prompt.Prompt r -> r
combatAnswer blocks p = case p of
  Prompt.DeclareBlockers {} -> if blocks then S.aggressiveAnswer p else Map.empty
  _ -> S.aggressiveAnswer p

-- CR 608.2h's second clause -- "if the effect has moved it from a public zone to
-- a hidden zone, the effect uses the object's last known information" -- read for
-- CR 111.6's token status.
--
-- Sunpearl Kirin {1}{W} 2/1 Kirin: "Flash. Flying. When this creature enters,
-- return up to one other target nonland permanent you control to its owner's
-- hand. If it was a token, draw a card." (data/cards/sunpearl-kirin.json; Oracle
-- text checked against api.scryfall.com, 2026-08-31.) The bounce is the FIRST
-- clause of the resolution and the token read is the second, so CR 400.7 has
-- already deleted the id the target slot holds by the time the condition is asked.
--
-- Measured on alice's LIBRARY and not on her hand: the nontoken leg puts the
-- bounced card into that hand, so a hand count cannot tell the draw from the
-- bounce.
--
-- The two legs are one board differing in ONE thing -- the victim is a Goblin
-- Piker token or a Goblin Piker card. Same name, same box, same controller, both
-- Settled on the battlefield.
lastKnownTokenSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastKnownTokenSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      librarySize gs = maybe 0 length (Map.lookup S.alice (GameState.library gs))
      run kirin swamp place k =
        let (_, stocked) = S.addLibraryCard swamp S.alice (Setup.emptyGame S.bothPlayers)
            (victimId, withVictim) = place stocked
            (_, entered) = S.entersWithTrigger kirin S.alice withVictim
            onStack = settle entered
         in k victimId onStack (S.runPure (bouncing victimId) onStack Stack.resolveTop)
   in Spec.describe s "LastKnownToken" $ do
        -- THE PROVING TEST for #1102. A live read of the bounced id answers "not a
        -- token, there is no it" and the draw never happens.
        Spec.it s "CR 608.2h the token Sunpearl Kirin bounced was a token, so alice draws" $ do
          kirin <- S.printingOf s registry "Sunpearl Kirin"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.cardOf s registry "Goblin Piker"
          run kirin swamp (S.addToken piker S.alice) $ \victimId onStack after -> do
            Spec.assertEqWith s "alice drew her one library card" (librarySize after) 0
            Spec.assertEqWith s "off a library that held exactly one" (librarySize onStack) 1
            Spec.assertEqWith s "and the token really did leave the battlefield" (Set.member victimId (GameState.battlefield after)) False
            Spec.assertEqWith s "with the Kirin's trigger on the stack before it resolved" (length (GameState.stack onStack)) 1

        -- The negative, one difference from the case above: the victim is a CARD.
        -- Bounced by the same clause into the same hidden zone, so "no draw" cannot
        -- be the bounce failing.
        Spec.it s "CR 111.6 a Goblin Piker card bounced the same way is no token, so she does not" $ do
          kirin <- S.printingOf s registry "Sunpearl Kirin"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          run kirin swamp (S.addPermanent piker S.alice) $ \victimId onStack after -> do
            Spec.assertEqWith s "alice's library is untouched" (librarySize after) 1
            Spec.assertEqWith s "though the card really did leave the battlefield" (Set.member victimId (GameState.battlefield after)) False
            Spec.assertEqWith s "with the Kirin's trigger on the stack before it resolved" (length (GameState.stack onStack)) 1

-- CR 608.2h read for CR 509.1g's combat status, on CR 603.4's intervening "if".
--
-- Guildsworn Prowler {1}{B} 2/1 Tiefling Rogue Assassin: "Deathtouch. When this
-- creature dies, if it wasn't blocking, draw a card."
-- (data/cards/guildsworn-prowler.json; Oracle text checked against
-- api.scryfall.com, 2026-08-31.) CR 400.7 deletes the id before rule 603.4's
-- first check runs, and CR 506.4 takes the creature out of GameState.combat, so
-- the live read answers "not blocking" for exactly the creature that was.
--
-- The two legs are one board differing in ONE thing -- whether bob declares the
-- block. The Prowler dies to the SAME three marked damage in both (CR 704.5g on a
-- 1-toughness creature), declared blockers or not, so the draw cannot be the
-- kill's doing. Combat damage is never dealt: the fixture stops at the top of the
-- combat damage step.
lastKnownBlockingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastKnownBlockingSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      run giant prowler swamp blocks k = case S.combatBoardOf [giant] [prowler] of
        (base, _, [prowlerId]) ->
          let (_, stocked) = S.addLibraryCard swamp S.bob base
              declared = S.runToStep (Phase.Combat CombatStep.CombatDamage) (combatAnswer blocks) stocked
              killed = S.settleSba (S.markDamage prowlerId 3 declared)
              onStack = settle killed
           in k prowlerId declared killed (S.runPure S.identityAnswer onStack Stack.resolveTop)
        _ -> Spec.assertFailure s "combatBoardOf should place exactly one Prowler"
   in Spec.describe s "LastKnownBlocking" $ do
        -- THE PROVING TEST for CR 509.1g's half of the record. A live read of the
        -- dead id answers "it wasn't blocking" and bob draws off a creature that
        -- spent the step blocking; lastKnownAttackingSpec below is CR 508.1k's.
        Spec.it s "CR 608.2h a blocking Guildsworn Prowler that dies draws nothing" $ do
          giant <- S.printingOf s registry "Hill Giant"
          prowler <- S.printingOf s registry "Guildsworn Prowler"
          swamp <- S.printingOf s registry "Swamp"
          run giant prowler swamp True $ \prowlerId declared killed after -> do
            Spec.assertEqWith s "bob's hand is empty, so CR 603.4's clause was false" (S.handSize S.bob after) 0
            Spec.assertEqWith s "and it really was blocking before it died" (fmap Filter.blocking (Projection.viewWithLastKnownAnywhere declared prowlerId)) (Just True)
            Spec.assertEqWith s "and it really did die" (Set.member prowlerId (GameState.battlefield killed)) False
            Spec.assertEqWith s "so nothing was gathered onto the stack" (length (GameState.stack (settle killed))) 0

        -- The negative's twin, and the control the case above is read against: bob
        -- declines the block and the same kill draws him a card.
        Spec.it s "CR 509.1g the same Prowler that never blocked draws one" $ do
          giant <- S.printingOf s registry "Hill Giant"
          prowler <- S.printingOf s registry "Guildsworn Prowler"
          swamp <- S.printingOf s registry "Swamp"
          run giant prowler swamp False $ \prowlerId declared killed after -> do
            Spec.assertEqWith s "bob drew his one library card" (S.handSize S.bob after) 1
            Spec.assertEqWith s "off a Prowler that was never blocking" (fmap Filter.blocking (Projection.viewWithLastKnownAnywhere declared prowlerId)) (Just False)
            Spec.assertEqWith s "and died the same way" (Set.member prowlerId (GameState.battlefield killed)) False

-- Declares exactly `victim` as an attacker, or nobody at all: the ONE thing the
-- two legs of lastKnownAttackingSpec differ in. Filters the offered set rather
-- than naming the id, so a board that never offers it fails rather than passing
-- vacuously.
attackingOnly :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
attackingOnly attacks victim p = case p of
  Prompt.DeclareAttackers _ _ ids -> if attacks then List.filter (== victim) ids else []
  _ -> S.aggressiveAnswer p

-- CR 608.2h read for CR 508.1k's combat status, on an ordinary English "if"
-- gating one clause of a resolution (CR 608.2c) rather than on CR 603.4's
-- intervening one.
--
-- Garna, Bloodfist of Keld {1}{B}{R}{R} 4/3 Legendary Human Berserker: "Whenever
-- another creature you control dies, draw a card if it was attacking. Otherwise,
-- Garna deals 1 damage to each opponent."
-- (data/cards/garna-bloodfist-of-keld.json; Oracle text checked against
-- api.scryfall.com, 2026-09-10.) CR 400.7 deletes the Hill Giant's id before the
-- ability resolves and CR 506.4 takes it out of GameState.combat, so the live
-- read answers "not attacking" for exactly the creature that was.
--
-- The two legs are one board differing in ONE thing -- whether alice declares the
-- Giant as an attacker. It dies to the SAME three marked damage either way (CR
-- 704.5g on a 3/3), so the draw cannot be the kill's doing, and Garna is never
-- declared, so the card that reads the record is not itself in combat.
--
-- Both halves of the printed sentence are asserted on each leg, which is what
-- makes the pair a proof rather than two one-sided reads: the draw AND bob's life
-- total, since "otherwise" means exactly one of them moves.
--
-- Combat damage is never dealt: the fixture stops at the top of the combat damage
-- step, so the attacking leg's Giant does not hit bob and his life total answers
-- for Garna's 1 damage alone.
lastKnownAttackingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastKnownAttackingSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      run garna giant swamp attacks k = case S.combatBoardOf [garna, giant] [] of
        (base, [_, giantId], _) ->
          let (_, stocked) = S.addLibraryCard swamp S.alice base
              declared = S.runToStep (Phase.Combat CombatStep.CombatDamage) (attackingOnly attacks giantId) stocked
              killed = S.settleSba (S.markDamage giantId 3 declared)
              onStack = settle killed
           in k giantId killed (S.runPure S.identityAnswer onStack Stack.resolveTop)
        _ -> Spec.assertFailure s "combatBoardOf should place Garna and one Hill Giant"
   in Spec.describe s "LastKnownAttacking" $ do
        Spec.it s "CR 608.2h an attacking Hill Giant that dies draws Garna a card" $ do
          garna <- S.printingOf s registry "Garna, Bloodfist of Keld"
          giant <- S.printingOf s registry "Hill Giant"
          swamp <- S.printingOf s registry "Swamp"
          run garna giant swamp True $ \giantId killed after -> do
            Spec.assertEqWith s "alice drew her one library card" (S.handSize S.alice after) 1
            Spec.assertEqWith s "and bob is untouched, so the otherwise clause was skipped" (S.lifeOf S.bob after) (Just 20)
            Spec.assertEqWith s "off a Giant CR 608.2h still says was attacking" (fmap Filter.attacking (Projection.viewWithLastKnownAnywhere killed giantId)) (Just True)
            Spec.assertEqWith s "and it really did die" (Set.member giantId (GameState.battlefield killed)) False

        -- The positive's twin, and the control it is read against: alice declines
        -- the attack and the same kill takes bob's life instead of filling her hand.
        Spec.it s "CR 508.1k the same Giant that never attacked deals bob 1 instead" $ do
          garna <- S.printingOf s registry "Garna, Bloodfist of Keld"
          giant <- S.printingOf s registry "Hill Giant"
          swamp <- S.printingOf s registry "Swamp"
          run garna giant swamp False $ \giantId killed after -> do
            Spec.assertEqWith s "bob took Garna's 1 damage" (S.lifeOf S.bob after) (Just 19)
            Spec.assertEqWith s "and alice's hand is empty, so the draw clause was skipped" (S.handSize S.alice after) 0
            Spec.assertEqWith s "off a Giant that was never attacking" (fmap Filter.attacking (Projection.viewWithLastKnownAnywhere killed giantId)) (Just False)
            Spec.assertEqWith s "and died the same way" (Set.member giantId (GameState.battlefield killed)) False

-- Aims a target slot at one OBJECT by filtering the offered set, `bouncing`'s
-- shape for a slot that must be answered: a hand-built recipient could miss CR
-- 608.2b's re-read, and a fixture that merely PREFERS the victim would repair a
-- mutation by finding another legal target.
damaging :: ObjectId.ObjectId -> Prompt.Prompt r -> r
damaging victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter ((== Just victim) . Recipient.objectOf) candidates) sets
  _ -> S.identityAnswer p

-- Its player-shaped twin, for CR 120.1's fourth recipient.
damagingPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
damagingPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== Recipient.ToPlayer pid) candidates) sets
  _ -> S.identityAnswer p

-- CR 120.1 / 608.2i read on CR 603.4's intervening "if", for a creature CR 400.7
-- has already deleted.
--
-- Burning-Eye Zubera {2}{R}{R} 3/3 Zubera Spirit: "When this creature dies, if 4
-- or more damage was dealt to it this turn, this creature deals 3 damage to any
-- target." (data/cards/burning-eye-zubera.json; Oracle text checked against
-- api.scryfall.com, 2026-09-01.)
--
-- CR 608.2i and not CR 608.2h: "was dealt this turn" looks back in time, so the
-- answer comes off GameState.events, which the death does not touch, rather than
-- off a Pawl.Types.LastKnown field. Object.damage could not answer it either --
-- CR 400.7 deletes the object the marks were on.
--
-- The two legs are one board differing in ONE thing: alice's Prodigal Sorcerer
-- pings the Zubera or the Hill Giant standing beside it. The same Lightning Bolt
-- kills the Zubera in both (CR 704.5g -- 3 is lethal to a 3/3 whether or not the
-- ping came first), so the 3 damage alice takes cannot be the kill's doing; only
-- the ping's one point moves the turn's total from 3 to 4.
damageDealtToItSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageDealtToItSpec s registry =
  let run sorcerer zubera giant mountain bolt pingsZubera k =
        case Face.activatedAbilities (S.combinedFace sorcerer) of
          ping : _ ->
            let (sorcererId, gs1) = S.addPermanent sorcerer S.alice (S.landsInPlay mountain 1)
                (zuberaId, gs2) = S.addPermanent zubera S.bob gs1
                (giantId, gs3) = S.addPermanent giant S.bob gs2
                (staged, boltId) = S.handOne bolt gs3
                ready = staged {GameState.priority = Just S.alice}
                pingedAt = if pingsZubera then zuberaId else giantId
                pinged = S.runPure (damaging pingedAt) ready (do Activate.activateAbility S.alice sorcererId ping; Stack.resolveTop)
                cast = S.runPure (damaging zuberaId) pinged (S.cast S.alice boltId)
                killed = S.settleSba (S.runPure (damaging zuberaId) cast Stack.resolveTop)
                onStack = S.runPure (damagingPlayer S.alice) killed Engine.settleForPriority
             in k zuberaId pinged killed onStack (S.runPure S.identityAnswer onStack Stack.resolveTop)
          [] -> Spec.assertFailure s "Prodigal Sorcerer should print an activated ability"
   in Spec.describe s "DamageDealtToThisTurn" $ do
        -- THE PROVING TEST for #992. Nothing on the board can be asked how much
        -- damage the Zubera took: CR 400.7 deleted the object its marks were on.
        Spec.it s "CR 608.2i a Zubera dealt 4 damage before it died deals 3 to alice" $ do
          sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
          zubera <- S.printingOf s registry "Burning-Eye Zubera"
          giant <- S.printingOf s registry "Hill Giant"
          mountain <- S.printingOf s registry "Mountain"
          bolt <- S.printingOf s registry "Lightning Bolt"
          run sorcerer zubera giant mountain bolt True $ \zuberaId pinged killed onStack after -> do
            Spec.assertEqWith s "alice took the Zubera's 3 damage, so CR 603.4's clause was true" (S.lifeOf S.alice after) (Just 17)
            -- The preconditions, after the behaviour so none of them can absorb a
            -- mutation of the atom.
            Spec.assertEqWith s "off a Zubera the ping really had damaged first" (S.damageOf zuberaId pinged) (Just 1)
            Spec.assertEqWith s "which really did die" (Set.member zuberaId (GameState.battlefield killed)) False
            Spec.assertEqWith s "with its trigger on the stack before it resolved" (length (GameState.stack onStack)) 1

        -- The negative's twin, one difference: the Sorcerer pings the Hill Giant
        -- instead, so the Zubera is dealt 3 this turn rather than 4.
        Spec.it s "CR 603.4 the same Zubera dealt only 3 deals nothing" $ do
          sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
          zubera <- S.printingOf s registry "Burning-Eye Zubera"
          giant <- S.printingOf s registry "Hill Giant"
          mountain <- S.printingOf s registry "Mountain"
          bolt <- S.printingOf s registry "Lightning Bolt"
          run sorcerer zubera giant mountain bolt False $ \zuberaId pinged killed onStack after -> do
            Spec.assertEqWith s "alice is untouched at 20" (S.lifeOf S.alice after) (Just 20)
            Spec.assertEqWith s "off a Zubera nothing had pinged" (S.damageOf zuberaId pinged) (Just 0)
            Spec.assertEqWith s "which died the same way" (Set.member zuberaId (GameState.battlefield killed)) False
            Spec.assertEqWith s "and nothing was gathered onto the stack" (length (GameState.stack onStack)) 0

-- Attacks with `attacker` alone, and blocks it or not: the pair's ONE difference.
-- aggressiveAnswer's head otherwise, so the two legs agree on every other prompt.
attackAndBlock :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
attackAndBlock blocks attacker p = case p of
  Prompt.DeclareAttackers _ _ ids -> List.filter (== attacker) ids
  Prompt.DeclareBlockers {} -> if blocks then S.aggressiveAnswer p else Map.empty
  _ -> S.aggressiveAnswer p

-- CR 509.1h / 608.2i read on CR 603.4's intervening "if", for a creature CR 400.7
-- has already deleted.
--
-- Fyndhorn Druid {2}{G} 2/2 Elf Druid: "When this creature dies, if it was blocked
-- this turn, you gain 4 life." (data/cards/fyndhorn-druid.json; Oracle text
-- checked against api.scryfall.com, 2026-09-16.)
--
-- Filter.IsBlocked cannot answer it and neither can a Pawl.Types.LastKnown field:
-- the question is asked of an object that is gone, and the printed clause says
-- THIS TURN rather than "as it died". Only the turn's GameEvent.AttackerBlocked
-- log still holds it.
--
-- The first two legs are one board differing in ONE thing, whether bob's Wall of
-- Stone blocks. The Wall deals no damage, so in both of them the Druid survives
-- combat and is killed in the postcombat main phase -- after CR 511.3 has taken
-- every creature out of combat and Pawl.Engine.Combat.clearCombat has emptied the
-- record, which is what makes the pair a proof about the TURN rather than about
-- the combat. The third leg is the ordinary board, the Druid dying inside combat
-- with the block still live.
wasBlockedThisTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wasBlockedThisTurnSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      run druid blocker blocks afterCombat k = case S.combatBoardOf [druid] [blocker] of
        (base, [druidId], [blockerId]) ->
          let declared = S.runToStep (Phase.Combat CombatStep.CombatDamage) (attackAndBlock blocks druidId) base
              fought = if afterCombat then S.runCombat (attackAndBlock blocks druidId) declared else declared
              -- 2 damage marked on a 2/2, CR 704.5g's lethal, in all three legs.
              killed = S.settleSba (S.markDamage druidId 2 fought)
              onStack = settle killed
           in k druidId blockerId declared fought killed onStack (S.runPure S.identityAnswer onStack Stack.resolveTop)
        _ -> Spec.assertFailure s "combatBoardOf should place the Druid and one blocker"
      unblockedIn gs druidId = Set.null (Map.findWithDefault Set.empty druidId (Combat.Type.blockers (GameState.combat gs)))
   in Spec.describe s "WasBlockedThisTurn" $ do
        -- THE PROVING TEST for #3037. Nothing on the board can be asked whether the
        -- Druid was blocked: combat is over and its record emptied, and CR 400.7
        -- then deleted the object the status was on.
        Spec.it s "CR 608.2i a Druid blocked earlier in the turn gains alice 4 as it dies" $ do
          druid <- S.printingOf s registry "Fyndhorn Druid"
          wall <- S.printingOf s registry "Wall of Stone"
          run druid wall True True $ \druidId _ declared fought killed onStack after -> do
            Spec.assertEqWith s "alice gained the Druid's 4 life, so CR 603.4's clause was true" (S.lifeOf S.alice after) (Just 24)
            -- The preconditions, after the behaviour so none of them can absorb a
            -- mutation of the atom.
            Spec.assertEqWith s "off a Druid the Wall really had blocked" (unblockedIn declared druidId) False
            Spec.assertEqWith s "whose block CR 511.3 had since taken off the record" (unblockedIn fought druidId) True
            Spec.assertEqWith s "and which really did die" (Set.member druidId (GameState.battlefield killed)) False
            Spec.assertEqWith s "with its trigger on the stack before it resolved" (length (GameState.stack onStack)) 1

        -- The positive's twin, one difference: bob declines the block, so the Druid
        -- dies unblocked and CR 603.4 never puts the trigger onto the stack.
        Spec.it s "CR 603.4 the same Druid that was never blocked gains nothing" $ do
          druid <- S.printingOf s registry "Fyndhorn Druid"
          wall <- S.printingOf s registry "Wall of Stone"
          run druid wall False True $ \druidId _ declared _ killed onStack after -> do
            Spec.assertEqWith s "alice is untouched at 20" (S.lifeOf S.alice after) (Just 20)
            Spec.assertEqWith s "off a Druid nothing had blocked" (unblockedIn declared druidId) True
            Spec.assertEqWith s "which died the same way" (Set.member druidId (GameState.battlefield killed)) False
            Spec.assertEqWith s "and nothing was gathered onto the stack" (length (GameState.stack onStack)) 0

        -- The ordinary board: a Hill Giant blocks and the Druid dies in the combat
        -- damage step, with CR 509.1h's status still live on the record.
        Spec.it s "CR 509.1h a Druid that dies inside combat gains the 4 too" $ do
          druid <- S.printingOf s registry "Fyndhorn Druid"
          giant <- S.printingOf s registry "Hill Giant"
          run druid giant True False $ \druidId giantId declared _ killed onStack after -> do
            Spec.assertEqWith s "alice gained the 4 all the same" (S.lifeOf S.alice after) (Just 24)
            Spec.assertEqWith s "off a Druid the Giant had blocked" (unblockedIn declared druidId) False
            Spec.assertEqWith s "which died with the Giant still beside it" (Set.member giantId (GameState.battlefield killed)) True
            Spec.assertEqWith s "and itself gone" (Set.member druidId (GameState.battlefield killed)) False
            Spec.assertEqWith s "with its trigger on the stack before it resolved" (length (GameState.stack onStack)) 1

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
              (Filter.contextFor Teams.none Nothing Nothing)
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
  foreignGraveyardCastSpec s registry
  lastKnownTokenSpec s registry
  lastKnownBlockingSpec s registry
  lastKnownAttackingSpec s registry
  damageDealtToItSpec s registry
  wasBlockedThisTurnSpec s registry
  ashlingSpec s registry
  guidingSpiritSpec s registry

-- CR 608.2n / 608.2i: how many times an ABILITY has resolved this turn, which
-- Quantity.TimesResolvedThisTurn folds off GameEvent.ActivatedAbilityResolved.
--
-- Ashling the Pilgrim ({1}{R} Legendary Creature -- Elemental Shaman 1/1,
-- "{1}{R}: Put a +1/+1 counter on Ashling. If this is the third time this
-- ability has resolved this turn, remove all +1/+1 counters from Ashling, and it
-- deals that much damage to each creature and each player.") is the fixture, and
-- the only one the pool needs: the clause is a printed "if" over CR 602.2a's
-- "this ability", read through Binding.thisAbility.
--
-- THE BOARD SHAPE that makes the cases discriminating. bob's Blind-Spot Giant is
-- 4/3 and his Palace Guard 1/4, so 3 damage takes exactly one of them and the
-- survivor is the proof the amount was 3 rather than "enough" -- and Ashling
-- itself is 1/1 once its counters come off, so it dies to its own sweep, which is
-- what CR 608.2c's printed order means and a damage-before-removal reading would
-- not show. alice holds SIX Mountains, two per activation, so the third
-- activation in the reset case below is paid for out of what turn one left.
--
-- Asserted on the BOARD rather than on the log: the counts, the life totals and
-- who is left standing are what the card prints.
ashlingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ashlingSpec s registry = Spec.describe s "Ashling the Pilgrim (CR 608.2n)" $ do
  let giantName = CardName.MkCardName (Text.pack "Blind-Spot Giant")
      guardName = CardName.MkCardName (Text.pack "Palace Guard")
      ashlingName = CardName.MkCardName (Text.pack "Ashling the Pilgrim")
      -- alice's Ashling and six untapped Mountains against bob's two creatures.
      setUp = do
        ashling <- S.printingOf s registry "Ashling the Pilgrim"
        mountain <- S.printingOf s registry "Mountain"
        giant <- S.printingOf s registry "Blind-Spot Giant"
        guard_ <- S.printingOf s registry "Palace Guard"
        let (ashlingId, withAshling) = S.addPermanent ashling S.alice (S.landsInPlay mountain 6)
            (_, withGiant) = S.addPermanent giant S.bob withAshling
            (_, withGuard) = S.addPermanent guard_ S.bob withGiant
        pure (ashlingId, withGuard)
  Spec.it s "CR 608.2n the third resolution this turn removes the counters and deals that much damage" $ do
    (ashlingId, board) <- setUp
    case Maybe.listToMaybe (Projection.abilitiesOf ashlingId board) of
      Nothing -> Spec.assertFailure s "Ashling the Pilgrim should declare one activated ability"
      Just pump -> do
        let after = List.foldl' (\gs _ -> activateAshling ashlingId pump gs) board [1 .. (3 :: Int)]
        Spec.assertEqWith s "CR 608.2n the 4/3 Blind-Spot Giant was dealt the 3 damage of the three counters removed" (S.countOnBattlefieldByName giantName S.bob after) 0
        Spec.assertEqWith s "and the 1/4 Palace Guard survived it, so the amount was 3 rather than lethal to everything" (S.countOnBattlefieldByName guardName S.bob after) 1
        Spec.assertEqWith s "CR 608.2c the counters came off first, so Ashling met its own 3 damage as a 1/1" (S.countOnBattlefieldByName ashlingName S.alice after) 0
        Spec.assertEqWith s "and each player was dealt 3" (S.lifeOf S.bob after) (Just 17)
        Spec.assertEqWith s "alice included, the sweep naming each player and not each opponent" (S.lifeOf S.alice after) (Just 17)
  -- The negative, on the SAME board with one activation fewer: the clause reads
  -- "the third time", so the second resolution is not it.
  Spec.it s "CR 608.2n the second resolution this turn does not" $ do
    (ashlingId, board) <- setUp
    case Maybe.listToMaybe (Projection.abilitiesOf ashlingId board) of
      Nothing -> Spec.assertFailure s "Ashling the Pilgrim should declare one activated ability"
      Just pump -> do
        let after = List.foldl' (\gs _ -> activateAshling ashlingId pump gs) board [1 .. (2 :: Int)]
        Spec.assertEqWith s "nobody was dealt anything" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "and the Blind-Spot Giant is untouched" (S.countOnBattlefieldByName giantName S.bob after) 1
        Spec.assertEqWith s "the counters are still on Ashling" (S.counterOf CounterKind.PlusOnePlusOne ashlingId after) 2
  -- CR 608.2i's extent: GameState.events is cleared at the turn handoff, so the
  -- tally is of THIS turn. Two resolutions, the handoff, then a third -- which a
  -- game-long count would make the third time and a counters-on-Ashling reading
  -- would too, Ashling carrying three of them by then.
  Spec.it s "CR 608.2i the turn handoff resets the tally" $ do
    (ashlingId, board) <- setUp
    case Maybe.listToMaybe (Projection.abilitiesOf ashlingId board) of
      Nothing -> Spec.assertFailure s "Ashling the Pilgrim should declare one activated ability"
      Just pump -> do
        let twice = List.foldl' (\gs _ -> activateAshling ashlingId pump gs) board [1 .. (2 :: Int)]
            after = activateAshling ashlingId pump (Engine.beginTurnOf S.bob twice)
        Spec.assertEqWith s "CR 608.2i the resolution on bob's turn is the first of it, so nothing was dealt" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "and the Blind-Spot Giant is untouched" (S.countOnBattlefieldByName giantName S.bob after) 1
        Spec.assertEqWith s "Ashling carries all three counters, which is what a count of them rather than of resolutions would have read" (S.counterOf CounterKind.PlusOnePlusOne ashlingId after) 3

-- One activation of Ashling's ability by alice, resolved and settled. alice is
-- given priority because a board does not imply a window (S.priorityGame's
-- sentence), and the mana comes off her untapped Mountains.
activateAshling :: ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState.GameState -> GameState.GameState
activateAshling ashlingId pump gs =
  let activated = S.runPure S.identityAnswer gs {GameState.priority = Just S.alice} (Activate.activateAbility S.alice ashlingId pump)
   in snd (Engine.runGamePure S.identityAnswer activated (Stack.resolveTop >> Engine.settleForPriority))

-- CR 404.1 read from inside a CONDITION: Scope.TopOfGraveyard, which names ONE
-- position in a graveyard so that a Filter over it TESTS that card rather than
-- sweeping the pile.
--
-- Guiding Spirit {1}{W}{U} Creature -- Angel Spirit 1/2 -- "Flying. {T}: If the
-- top card of target player's graveyard is a creature card, put that card on top
-- of that player's library." (name, cost, type line, power, toughness and Oracle
-- text checked against api.scryfall.com, 2026-09-21). The flying is inert here;
-- the one activated ability is what these assertions read.
--
-- The board is built so that the readings of the clause are told apart, since a
-- board that cannot distinguish them proves nothing:
--
--   * THE TOP CARD versus ANY card. bob's graveyard is stocked with a creature
--     card UNDER a noncreature one in the second case, so a Count over the whole
--     zone filtered to creature cards holds where this one must not -- and the
--     effect would then move the noncreature card, which nothing about the
--     printed sentence permits.
--   * THE TOP CARD versus the OLDEST. A graveyard's top is its LAST member (CR
--     404.1), so each pile is asserted in its stored order and the card that
--     moved is named.
--   * TARGET PLAYER'S versus YOUR own. alice's own graveyard is topped by a
--     creature card and must be untouched -- Soldevi Digger's self-scoped read
--     of the same position is what this one is not.
--   * The TOP of the library versus its bottom. bob's library already holds a
--     card, so the two ends are different positions.
guidingSpiritSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
guidingSpiritSpec s registry =
  let -- alice's Guiding Spirit, settled so CR 302.6 leaves its {T} payable, with
      -- `buried` OLDEST FIRST in bob's graveyard (S.addGraveyardCard puts each
      -- on top, so the last name given is the top card). bob's library holds one
      -- Benalish Hero and alice's graveyard one Hill Giant, a creature card on
      -- top of a graveyard the ability never names.
      board buried = do
        spirit <- S.printingOf s registry "Guiding Spirit"
        giant <- S.printingOf s registry "Hill Giant"
        hero <- S.printingOf s registry "Benalish Hero"
        stocked <- mapM (S.printingOf s registry) buried
        let (spiritId, g1) = S.addPermanent spirit S.alice (Setup.emptyGame S.bothPlayers)
            g2 = List.foldl' (\g p -> snd (S.addGraveyardCard p S.bob g)) g1 stocked
            g3 = snd (S.addGraveyardCard giant S.alice g2)
            g4 = snd (S.addLibraryCard hero S.bob g3)
        pure (spirit, spiritId, g4 {GameState.priority = Just S.alice})
      named = CardName.MkCardName . Text.pack
      -- Both piles in their own stored order: a graveyard reads OLDEST FIRST (CR
      -- 404.1's arrival end is the last member) and a library TOP FIRST, which is
      -- why an arriving card is expected at opposite ends of the two lists.
      pilesOf pid gs = (zoneNames Zone.Graveyard pid gs, zoneNames Zone.Library pid gs)
      -- One activation of the Spirit's ability by alice, aimed at bob, resolved.
      activate spirit spiritId gs = case Maybe.listToMaybe (Face.activatedAbilities (S.combinedFace spirit)) of
        Nothing -> Nothing
        Just ability ->
          let activated = snd (Engine.runGamePure (aimAtPlayer S.bob) gs (Activate.activateAbility S.alice spiritId ability))
           in Just (snd (Engine.runGamePure (aimAtPlayer S.bob) activated Stack.resolveTop))
   in Spec.describe s "Guiding Spirit (CR 404.1)" $ do
        Spec.it s "CR 404.1 a creature card on top of the targeted graveyard goes on top of that player's library" $ do
          (spirit, spiritId, before) <- board ["Forest", "Goblin Piker"]
          case activate spirit spiritId before of
            Nothing -> Spec.assertFailure s "Guiding Spirit should print exactly one activated ability"
            Just after -> do
              Spec.assertEqWith
                s
                "the fixture really buried the Goblin Piker last, on top of the Forest"
                (fst (pilesOf S.bob before))
                [named "Forest", named "Goblin Piker"]
              Spec.assertEqWith
                s
                "the Goblin Piker is on top of bob's library, above the Benalish Hero, and the Forest is left behind"
                (pilesOf S.bob after)
                ([named "Forest"], [named "Goblin Piker", named "Benalish Hero"])
              Spec.assertEqWith
                s
                "alice's own graveyard is untouched, so the reference is the TARGETED player's rather than her own"
                (pilesOf S.alice after)
                ([named "Hill Giant"], [])
        -- THE discriminating case, and the one a Count over the whole graveyard
        -- gets wrong: a creature card is in the pile but is not on top, so the
        -- printed "if" is false and nothing moves.
        Spec.it s "CR 404.1 a creature card UNDER the top card does not satisfy the condition" $ do
          (spirit, spiritId, before) <- board ["Goblin Piker", "Forest"]
          case activate spirit spiritId before of
            Nothing -> Spec.assertFailure s "Guiding Spirit should print exactly one activated ability"
            Just after -> do
              Spec.assertEqWith
                s
                "the fixture really buried the Forest last, on top of the Goblin Piker"
                (fst (pilesOf S.bob before))
                [named "Goblin Piker", named "Forest"]
              Spec.assertEqWith
                s
                "both cards are still in bob's graveyard and his library still holds only the Benalish Hero"
                (pilesOf S.bob after)
                ([named "Goblin Piker", named "Forest"], [named "Benalish Hero"])
              Spec.assertEqWith
                s
                "the Spirit is alice's one permanent and it is tapped, so the ability was activated and resolved and the CONDITION is what stopped the move"
                (S.tappedCount S.alice after)
                1
        -- CR 404.1's empty pile has no top card, so the count is 0 and the clause
        -- is skipped -- the same answer as the case above by a different road.
        Spec.it s "CR 404.1 an empty graveyard has no top card, so nothing moves" $ do
          (spirit, spiritId, before) <- board []
          case activate spirit spiritId before of
            Nothing -> Spec.assertFailure s "Guiding Spirit should print exactly one activated ability"
            Just after ->
              Spec.assertEqWith
                s
                "bob's graveyard is still empty and his library is untouched"
                (pilesOf S.bob after)
                ([], [named "Benalish Hero"])

-- A zone's members as names, in the zone's own STORED order -- the sorted
-- readings elsewhere would hide which end of a graveyard or a library a card is
-- at, which is the whole question here.
zoneNames :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
zoneNames zone pid gs = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs)

-- Answers every target slot with that player. Guiding Spirit's one slot is
-- Pool.Players, so this is the whole announcement.
aimAtPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimAtPlayer who p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
  _ -> S.identityAnswer p
