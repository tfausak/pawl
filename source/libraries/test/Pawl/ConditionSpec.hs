{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Condition, Pawl.Types.Condition and Pawl.Types.Comparison,
-- including what Condition.holds makes of Pawl.Engine.Quantity's IsMonarch,
-- EnteredThisTurn, EnteredFrom, WasCastFrom, WasToken, WasBlocking,
-- DamageDealtToThisTurn, WasBlockedThisTurn and TimesResolvedThisTurn.
module Pawl.ConditionSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
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
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerActed as PlayerActed
import qualified Pawl.Types.PlayerAction as PlayerAction
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
    (Scope.InZone (InZone.MkInZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.AnyPlayer)))
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
--     PlayerRef.Relative AnyPlayer's graveyards, and it holds -- the Knight takes its
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
lastKnownAttackingSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
        -- The twin of cr-608-2h-an-attacking-hill-giant-that-dies-draws-garna-a.json,
        -- and the control it is read against: alice declines
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
  enteredFromSpec s registry
  foreignGraveyardCastSpec s registry
  lastKnownTokenSpec s registry
  lastKnownAttackingSpec s registry
  wasBlockedThisTurnSpec s registry
  ashlingSpec s registry
  rumorGathererSpec s registry
  omnathSpec s registry
  necrobloomSpec s registry
  guidingSpiritSpec s registry

-- CR 608.2n / 608.2i: how many times an ACTIVATED ability has resolved this
-- turn, which Quantity.TimesResolvedThisTurn folds off
-- GameEvent.ActivatedAbilityResolved.
--
-- Ashling the Pilgrim ({1}{R} Legendary Creature -- Elemental Shaman 1/1,
-- "{1}{R}: Put a +1/+1 counter on Ashling. If this is the third time this
-- ability has resolved this turn, remove all +1/+1 counters from Ashling, and it
-- deals that much damage to each creature and each player.") is the fixture:
-- the clause is a printed "if" over CR 602.2a's "this ability", read through
-- Binding.thisAbility. rumorGathererSpec and omnathSpec below are the triggered
-- twin.
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

-- One activation of Ashling's ability by alice, resolved and settled. alice is
-- given priority because a board does not imply a window (S.priorityGame's
-- sentence), and the mana comes off her untapped Mountains.
activateAshling :: ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState.GameState -> GameState.GameState
activateAshling ashlingId pump gs =
  let activated = S.runPure S.identityAnswer gs {GameState.priority = Just S.alice} (Activate.activateAbility S.alice ashlingId pump)
   in snd (Engine.runGamePure S.identityAnswer activated (Stack.resolveTop >> Engine.settleForPriority))

-- CR 608.2n / 603.3: the same count on a TRIGGERED ability, read through the
-- thisAbility slot Pawl.Engine.Engine.placeBorne stamps and filed by
-- GameEvent.TriggeredAbilityResolved.
--
-- Rumor Gatherer ({1}{W}{W} Creature -- Elf Wizard 2/1, "Alliance -- Whenever
-- another creature you control enters, scry 1. If this is the second time this
-- ability has resolved this turn, draw a card instead.", Oracle text verified
-- Scryfall 2026-09-27). Three Goblin Pikers enter from alice's hand one at a
-- time, each trigger resolving before the next enters. The hand and the scry
-- log part company on each resolution: the first and third scry and draw
-- nothing, the second draws and does not scry -- so an engine that counted
-- nothing, or counted every trigger as the first, leaves a readable wrong board.
rumorGathererSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
rumorGathererSpec s registry = Spec.describe s "Rumor Gatherer (CR 608.2n)"
  . Spec.it s "CR 608.2n the second resolution this turn draws instead of scrying, and the first and third scry"
  $ do
    gatherer <- S.printingOf s registry "Rumor Gatherer"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    let (_, withGatherer) = S.addPermanent gatherer S.alice (Setup.emptyGame S.bothPlayers)
        stocked = List.foldl' (\g _ -> snd (S.addLibraryCard plains S.alice g)) withGatherer [1 .. (5 :: Int)]
        (first, g1) = S.addHandCard piker S.alice stocked
        (second, g2) = S.addHandCard piker S.alice g1
        (third, board) = S.addHandCard piker S.alice g2
        once = enterFromHand first board
        twice = enterFromHand second once
        thrice = enterFromHand third twice
    Spec.assertEqWith s "CR 608.2n the second resolution drew a card, so alice still holds two cards after putting a second one down" (S.handSize S.alice twice) 2
    Spec.assertEqWith s "and it scried nothing, the draw being instead of the scry" (scriesBy S.alice twice) 1
    Spec.assertEqWith s "the third resolution scried again" (scriesBy S.alice thrice) 2
    Spec.assertEqWith s "and drew nothing" (S.handSize S.alice thrice) 1
    Spec.assertEqWith s "the first resolution scried" (scriesBy S.alice once) 1
    Spec.assertEqWith s "and drew nothing" (S.handSize S.alice once) 2

-- Omnath, Locus of Creation ({R}{G}{W}{U} Legendary Creature -- Elemental 4/4,
-- "When Omnath enters, draw a card." and "Landfall -- Whenever a land you
-- control enters, you gain 4 life if this is the first time this ability has
-- resolved this turn. If it's the second time, add {R}{G}{W}{U}. If it's the
-- third time, Omnath deals 4 damage to each opponent and each planeswalker you
-- don't control.", Oracle text verified Scryfall 2026-09-27). Four Forests enter
-- from alice's hand in turn: each of the first three resolutions does one thing
-- and only that, and the fourth does nothing. Ajani Steadfast under each seat
-- tells "you don't control" from "each planeswalker".
omnathSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
omnathSpec s registry = Spec.describe s "Omnath, Locus of Creation (CR 608.2n)"
  . Spec.it s "CR 608.2n each landfall resolution this turn does what its count names"
  $ do
    omnath <- S.printingOf s registry "Omnath, Locus of Creation"
    forest <- S.printingOf s registry "Forest"
    ajani <- S.printingOf s registry "Ajani Steadfast"
    let (_, withOmnath) = S.addPermanent omnath S.alice (Setup.emptyGame S.bothPlayers)
        (bobsAjani, withBobs) = S.addPermanent ajani S.bob withOmnath
        (alicesAjani, withAlices) = S.addPermanent ajani S.alice withBobs
        loyal = S.addCounter CounterKind.Loyalty 6 alicesAjani (S.addCounter CounterKind.Loyalty 9 bobsAjani withAlices)
        (lands, board) = List.foldl' (\(acc, g) _ -> let (oid, g') = S.addHandCard forest S.alice g in (acc <> [oid], g')) ([], loyal) [1 .. (4 :: Int)]
        steps = drop 1 (List.scanl (flip enterFromHand) board lands)
        poolSize pid gs = maybe 0 (length . Mana.Type.unwrap) (Map.lookup pid (GameState.manaPool gs))
    case steps of
      [once, twice, thrice, fourth] -> do
        Spec.assertEqWith s "CR 608.2n the third resolution dealt bob 4" (S.lifeOf S.bob thrice) (Just 16)
        Spec.assertEqWith s "and 4 to the planeswalker alice does not control" (S.counterOf CounterKind.Loyalty bobsAjani thrice) 5
        Spec.assertEqWith s "and none to the one she does" (S.counterOf CounterKind.Loyalty alicesAjani thrice) 6
        Spec.assertEqWith s "the second resolution added {R}{G}{W}{U} and gained nothing" (poolSize S.alice twice, S.lifeOf S.alice twice) (4, Just 24)
        Spec.assertEqWith s "the first resolution gained 4 and added nothing" (poolSize S.alice once, S.lifeOf S.alice once) (0, Just 24)
        Spec.assertEqWith s "the fourth resolution did nothing at all" (S.lifeOf S.alice fourth, S.lifeOf S.bob fourth, poolSize S.alice fourth) (Just 24, Just 16, 4)
      _ -> Spec.assertFailure s "four Forests should have entered"

-- CR 201.2b through a condition: The Necrobloom ({1}{W}{B}{G} Legendary
-- Creature -- Plant 2/7, "Landfall -- Whenever a land you control enters,
-- create a 0/1 green Plant creature token. If you control seven or more lands
-- with different names, create a 2/2 black Zombie creature token instead.",
-- Oracle text verified Scryfall 2026-09-30). alice controls six lands with six
-- names; a pair of boards differing only in the seventh land to enter -- a
-- Snow-Covered Forest, a seventh name, or a second Forest, seven lands with six
-- names.
necrobloomSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
necrobloomSpec s registry = Spec.describe s "The Necrobloom (CR 201.2b)"
  . Spec.it s "CR 201.2b seven lands make a Zombie only when they have seven names"
  $ do
    bloom <- S.printingOf s registry "The Necrobloom"
    lands <- mapM (S.printingOf s registry) ["Plains", "Island", "Swamp", "Mountain", "Forest", "Evolving Wilds"]
    forest <- S.printingOf s registry "Forest"
    snowy <- S.printingOf s registry "Snow-Covered Forest"
    let (_, withBloom) = S.addPermanent bloom S.alice (Setup.emptyGame S.bothPlayers)
        board = List.foldl' (\g land -> snd (S.addPermanent land S.alice g)) withBloom lands
        tokens land =
          let (oid, g) = S.addHandCard land S.alice board
              after = enterFromHand oid g
              count name = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name)) S.alice after
           in (count "Zombie Token", count "Plant Token")
    Spec.assertEqWith s "CR 201.2b a seventh name makes a Zombie and no Plant" (tokens snowy) (1, 0)
    Spec.assertEqWith s "CR 201.2b a second Forest leaves six names, so a Plant" (tokens forest) (0, 1)

-- CR 603.2 / 603.3: one card alice holds is put onto the battlefield, the
-- abilities it triggers are placed at the next settle, and the top one resolves.
enterFromHand :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
enterFromHand oid gs =
  S.runPure S.identityAnswer gs {GameState.priority = Just S.alice} (Event.changeZone oid Zone.Battlefield >> Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)

-- How many times a player has scried this turn (CR 701.22b's scry event).
scriesBy :: PlayerId.PlayerId -> GameState.GameState -> Int
scriesBy pid gs = length [() | GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.Scry p) <- fmap LoggedEvent.event (Foldable.toList (GameState.events gs)), p == pid]

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
