{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Rad (CR 728, "Rad Counters"), PlayerCounterKind.Rad on
-- Player.counters (CR 122.1i), Pawl.Engine.Quantity's PlayerCounters arm, the
-- MillTally that Pawl.Engine.Resolve's Mill arm binds,
-- Effect.RemovePlayerCounters, and CR 728.1a's LifeLossCause.ByRadiation with the
-- LifeLossRewrite.GainInstead that reads it.
--
-- Gameplay-level. The Master, Transcendent is the producer -- {1}{B}{G}{U}
-- Legendary Artifact Creature, "When The Master enters, target player gets two
-- rad counters" -- cast off four lands, so the counters that rule 728.1's ability
-- then eats were put there by a card rather than written into the state. It is
-- also the pool's first card to give a TARGET player counters, which is why it
-- aims at bob: a recipient plumbed to the resolving controller would put them on
-- alice instead and pass a test that named neither.
--
-- The Master's OTHER ability -- "{T}: Put target creature card in a graveyard
-- that was milled this turn onto the battlefield under your control. It's a
-- green Mutant with base power and toughness 3/3." -- has the last group, since
-- rule 728.1's own mill is what stocks the graveyard it reads (CR 701.17a,
-- Filter.MilledThisTurn).
--
-- The groups after the first arrange the counters directly (S.addPlayerCounter),
-- because what they vary is the LIBRARY -- how many nonland cards the mill turned
-- up -- and casting the producer again would prove nothing new about that.
module Pawl.RadSpec where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Rad counters" $ do
  producerSpec s registry
  abilitySpec s registry

-- CR 122.1i through the card that hands the counters out.
producerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
producerSpec s registry = Spec.describe s "The Master, Transcendent" $ do
  Spec.it s "CR 122.1i its enters trigger gives the TARGETED player two rad counters" $ do
    master <- S.printingOf s registry "The Master, Transcendent"
    lands <- fourColorLands s registry
    let (gs, spellId) = S.handOne master (boardOf lands)
        cast = S.runPure (targeting S.bob) gs (S.cast S.alice spellId)
        after = S.runPure (targeting S.bob) cast Engine.priorityLoop
    Spec.assertEqWith s "bob has two rad counters" (radOf S.bob after) 2
    -- The falsifier for a recipient plumbed to the resolving controller (#120):
    -- alice cast it, and alice gets nothing.
    Spec.assertEqWith s "and alice, who cast it, has none" (radOf S.alice after) 0

-- CR 728.1's ability on its own, over the boards that vary what the mill finds.
abilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
abilitySpec s registry = Spec.describe s "CR 728.1's inherent ability" $ do
  -- CR 603.4's intervening "if". A player with none does not trigger at all.
  Spec.it s "CR 728.1 a player with no rad counters mills nothing" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    let stocked = libraryTopped [bolt, bolt] S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer (precombatMainOf S.alice stocked) (Engine.runStep >> Engine.priorityLoop)
    Spec.assertEqWith s "the library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 2
    Spec.assertEqWith s "and no life is lost" (S.lifeOf S.alice after) (Just 20)
    -- CR 603.4 is checked as the event OCCURS, so the ability is never PLACED at
    -- all -- a difference other players can see, and one that survives the CR
    -- 608.2a recheck doing the same job on resolution. Read at the settle point,
    -- because Engine.runStep's own priority round would have resolved it away.
    Spec.assertEqWith s "and no ability went on the stack at all" (length (GameState.stack (settledAtPrecombatMain S.alice stocked))) 0
  -- The same reading in the positive direction, so the case above cannot pass by
  -- the ability never existing.
  Spec.it s "CR 603.4 a player WITH rad counters does put it on the stack" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.addPlayerCounter PlayerCounterKind.Rad 1 S.alice (Setup.emptyGame S.bothPlayers)
        stocked = libraryTopped [bolt, bolt] S.alice base
    Spec.assertEqWith s "one ability on the stack" (length (GameState.stack (settledAtPrecombatMain S.alice stocked))) 1

-- alice activates The Master's one activated ability and everything resolves.
-- S.identityAnswer picks the least recipient offered, which is deliberately NOT
-- the milled card -- see the group's note.
activated :: ObjectId.ObjectId -> Printing.Printing -> GameState.GameState -> GameState.GameState
activated masterId master gs =
  S.runPure S.identityAnswer gs (Activate.activateAbility S.alice masterId (theReanimation master) >> Engine.priorityLoop)

-- The Master's sole activated ability, off its printed face.
theReanimation :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)
theReanimation printing = case Face.activatedAbilities (S.combinedFace printing) of
  ability : _ -> ability
  [] -> error "Pawl.RadSpec: The Master, Transcendent has no activated ability"

-- How many rad counters this player has (CR 122.1i), zero for a player who has
-- never had one.
radOf :: PlayerId.PlayerId -> GameState.GameState -> Natural.Natural
radOf = S.playerCounterOf PlayerCounterKind.Rad

-- The four lands The Master's {1}{B}{G}{U} needs, one of each colour it asks for
-- plus one to pay the generic.
fourColorLands :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m [Printing.Printing]
fourColorLands s registry = traverse (S.printingOf s registry) ["Swamp", "Forest", "Island", "Mountain"]

-- alice's side of the board: one untapped land of each printing.
boardOf :: [Printing.Printing] -> GameState.GameState
boardOf = foldr (\land gs -> snd (S.addPermanent land S.alice gs)) (Setup.emptyGame S.bothPlayers)

-- The given printings in pid's library, FIRST ONE ON TOP -- S.addLibraryCard
-- puts each new card at the front, so the list is laid down back to front.
libraryTopped :: [Printing.Printing] -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
libraryTopped printings pid gs = List.foldl' (\g p -> snd (S.addLibraryCard p pid g)) gs (reverse printings)

-- The board just after pid's precombat main phase began, settled for priority:
-- the moment CR 603.4 has decided whether rule 728.1's ability is on the stack,
-- and before any priority round could resolve it away. The StepBegan record is
-- staged directly, as Pawl.TriggerSpec's StepBegins cases stage theirs, because
-- Engine.runStep writes it and then runs that very priority round.
settledAtPrecombatMain :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
settledAtPrecombatMain pid gs =
  let staged = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan Phase.PrecombatMain pid)] (precombatMainOf pid gs)
   in S.runPure S.identityAnswer staged Engine.settleForPriority

-- A board sitting in pid's precombat main phase, the moment CR 728.1 names.
precombatMainOf :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
precombatMainOf pid gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = pid,
      GameState.priority = Just pid
    }

-- An interpreter that aims every target slot at one player, where
-- S.identityAnswer takes the least recipient -- which on this board is alice,
-- the caster.
targeting :: PlayerId.PlayerId -> Prompt.Prompt r -> r
targeting pid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap (\(_, legal) -> Set.filter (== Recipient.ToPlayer pid) legal) sets
  _ -> S.identityAnswer p
