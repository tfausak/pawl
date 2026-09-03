{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Vanguard and the CR 902 readings its callers make:
-- Pawl.Engine.Setup's starting life, command zone and rebuild paths,
-- Pawl.Engine.Mulligan's starting hand size, and Pawl.Engine.PlayerEffect's
-- maximum hand size. Also the five walks that let a vanguard's abilities function
-- from the command zone, all of which ask Vanguard.functionsFromCommandZone:
-- Pawl.Engine.Event's triggered one, Pawl.Engine.Projection's static and
-- replacement ones, Pawl.Engine.CombatRestriction.inForce, and
-- Pawl.Engine.Activate's activated one.
module Pawl.VanguardSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

-- A deck of thirty Mountains, plus whichever vanguard card the leg brings. Every
-- leg below is a pair of boards built from this and differing in exactly that
-- one argument.
deckOf :: Printing.Printing -> Maybe Printing.Printing -> Deck.Deck
deckOf mountain vanguard =
  Deck.MkDeck
    { Deck.cards = Map.singleton mountain 30,
      Deck.commander = Nothing,
      Deck.vanguard = vanguard,
      Deck.dungeons = Set.empty,
      Deck.sideboard = Map.empty
    }

-- Alice brings `vanguard`, bob brings none. Bob's deck is built too, so his
-- library is stocked for the opening draws (CR 104.3c would otherwise deck him)
-- and he is the in-board control for every per-player assertion.
built :: Printing.Printing -> Maybe Printing.Printing -> GameState.GameState
built mountain vanguard =
  S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) $ do
    Setup.createDeck S.alice (deckOf mountain vanguard)
    Setup.createDeck S.bob (deckOf mountain Nothing)

-- Keep the first hand, so the opening draw is what the hand size assertions read.
keepAnswer :: Prompt.Prompt r -> r
keepAnswer p = case p of
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Mulligan exactly once, then keep, bottoming whatever the engine offers first.
mulliganOnceAnswer :: Prompt.Prompt r -> r
mulliganOnceAnswer p = case p of
  Prompt.DeclareMulligan _ pid offer ->
    if pid == S.alice && MulliganOffer.taken offer == 0 then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

opened :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
opened answer gs = S.runPure answer gs (Mulligan.openingHands S.performer [S.alice, S.bob])

handSize :: GameState.GameState -> Int
handSize gs = length (Game.zoneMembers Zone.Hand S.alice gs)

commandNames :: GameState.GameState -> [CardName.CardName]
commandNames gs = fmap (\oid -> maybe (CardName.MkCardName (Text.pack "?")) S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Command S.alice gs)

-- Is this menu entry an activation of that object's ability? EXHAUSTIVE, so a new
-- kind of action has to be classified rather than counted as one of these.
isActivationOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isActivationOf oid action = case action of
  Action.Type.Activate srcId _ -> srcId == oid
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False
  Action.Type.Play _ _ -> False
  Action.Type.Cast {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.EndEffect _ -> False

-- Takes the command-zone object's ability from the priority menu ONCE --
-- sacrificing `fodder`, aiming at `victim` -- and passes at every window after
-- that. Threaded through State because a pure answerer cannot tell the second
-- offer from the first: alice still holds a permanent to sacrifice and a creature
-- of her own to bounce once this one has resolved, so "take it whenever offered"
-- would keep going until she had nothing left.
takesOnce :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
takesOnce srcId fodder victim p = case p of
  Prompt.ChooseAction _ _ actions -> do
    taken <- State.get
    case filter (isActivationOf srcId) actions of
      offer : _ | taken == 0 -> do
        State.put 1
        pure offer
      _ -> pure Action.Type.Pass
  -- FILTERS the offered recipients rather than building one: CR 608.2b re-reads
  -- the targets at resolution, and a hand-built recipient of the same permanent
  -- is a different one and would be dropped there with no error.
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (Set.filter (== Recipient.ToCreature victim) . snd) sets)
  Prompt.ChooseSacrifices _ _ _ candidates _ -> pure (Set.fromList (filter (== fodder) candidates))
  _ -> pure (S.identityAnswer p)

-- One noncombat hit, the shape Pawl.Engine.Damage.applyDamage takes.
hit :: ObjectId.ObjectId -> Recipient.Recipient -> Natural -> DamageEvent.DamageEvent
hit src recipient n = DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat

-- How much life `pid` lost between the two boards. A difference rather than a
-- total, because the vanguard's own CR 902.4 modifier moves the starting number
-- and the pair here differs in exactly the vanguard.
lifeLost :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState -> Maybe Integer
lifeLost pid before after = (-) <$> S.lifeOf pid before <*> S.lifeOf pid after

-- `built`'s board moved into a declare-attackers step with alice active and bob
-- defending, which is Support.combatBoardOf's shape -- rebuilt here rather than
-- called, because that fixture starts from an empty game and this one has to keep
-- the command zone Setup.createDeck filled.
declaringAttackers :: GameState.GameState -> GameState.GameState
declaringAttackers gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]}
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Vanguard" $ do
  -- CR 902.3 / CR 313.2: the card is placed beside the library before the game
  -- begins, which is the command zone, and it is NOT one of the deck's cards --
  -- the library is the thirty Mountains and nothing more.
  Spec.it s "CR 902.3 a vanguard card begins the game face up in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    let with = built mountain (Just gerrard)
        without = built mountain Nothing
    Spec.assertEqWith s "the vanguard is in alice's command zone" (commandNames with) [CardName.MkCardName (Text.pack "Gerrard")]
    Spec.assertEqWith s "face up" (fmap (\oid -> fmap Object.facing (Game.lookupObject oid with)) (Game.zoneMembers Zone.Command S.alice with)) [Just Facing.FaceUp]
    Spec.assertEqWith s "and not among the thirty cards of her library" (length (Game.zoneMembers Zone.Library S.alice with)) 30
    Spec.assertEqWith s "bob, who brought none, has an empty command zone" (Game.zoneMembers Zone.Command S.bob with) []
    Spec.assertEqWith s "and neither has alice on the deck that differs only in that field" (commandNames without) []

  -- CR 902.4 / CR 313.7: twenty plus or minus the life modifier. Three legs on
  -- one deck: Selenia's +7, Gerrard's printed +0 -- which must NOT be read as
  -- "no vanguard" -- and no vanguard at all.
  Spec.it s "CR 902.4 the starting life total is twenty plus the life modifier" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    selenia <- S.printingOf s registry "Selenia"
    Spec.assertEqWith s "Selenia's +7" (S.lifeOf S.alice (built mountain (Just selenia))) (Just 27)
    Spec.assertEqWith s "Gerrard's +0" (S.lifeOf S.alice (built mountain (Just gerrard))) (Just 20)
    Spec.assertEqWith s "no vanguard at all" (S.lifeOf S.alice (built mountain Nothing)) (Just 20)
    Spec.assertEqWith s "and bob, on the same board, keeps CR 103.4's twenty" (S.lifeOf S.bob (built mountain (Just selenia))) (Just 20)

  -- CR 902.5 / CR 313.6: seven cards, as modified. Gerrard's -4 and Selenia's +1
  -- move it in both directions off the same board.
  Spec.it s "CR 902.5 the opening hand is seven cards as modified by the hand modifier" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    selenia <- S.printingOf s registry "Selenia"
    Spec.assertEqWith s "Gerrard's -4 draws three" (handSize (opened keepAnswer (built mountain (Just gerrard)))) 3
    Spec.assertEqWith s "Selenia's +1 draws eight" (handSize (opened keepAnswer (built mountain (Just selenia)))) 8
    Spec.assertEqWith s "no vanguard draws CR 103.5's seven" (handSize (opened keepAnswer (built mountain Nothing))) 7
    Spec.assertEqWith s "and bob draws seven beside her" (length (Game.zoneMembers Zone.Hand S.bob (opened keepAnswer (built mountain (Just gerrard))))) 7

  -- CR 902.5a: a mulligan "draws a new hand equal to their starting hand size",
  -- which is the modified number and not seven. At two seats no mulligan is free
  -- (CR 103.5c), so the first one bottoms one card: three drawn less one bottomed
  -- against seven drawn less one bottomed.
  Spec.it s "CR 902.5a a mulligan redraws to the modified starting hand size" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    Spec.assertEqWith s "Gerrard: three redrawn, one bottomed" (handSize (opened mulliganOnceAnswer (built mountain (Just gerrard)))) 2
    Spec.assertEqWith s "no vanguard: seven redrawn, one bottomed" (handSize (opened mulliganOnceAnswer (built mountain Nothing))) 6

  -- CR 902.5b / CR 402.2: the maximum hand size takes the same modifier, and CR
  -- 514.1's cleanup discard is where a player pays it. Driven through
  -- Engine.discardToHandSize rather than asserted off
  -- PlayerEffect.maximumHandSize, so the number is read where the rule spends it.
  Spec.it s "CR 902.5b the maximum hand size is seven as modified" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    let -- Both legs enter the cleanup holding SEVEN, which is what makes the pair
        -- differ in one thing: Gerrard's opening hand is three, so it is topped up
        -- to the seven the control draws on its own.
        sevenInHand vanguard =
          let board = opened keepAnswer (built mountain vanguard)
           in List.foldl' (\g _ -> snd (S.addHandCard mountain S.alice g)) board (replicate (7 - handSize board) ())
        trimmed vanguard = handSize (S.runPure S.identityAnswer (sevenInHand vanguard) (Engine.discardToHandSize S.alice))
    Spec.assertEqWith s "both legs hold seven going in" (fmap (handSize . sevenInHand) [Just gerrard, Nothing]) [7, 7]
    Spec.assertEqWith s "Gerrard's -4 trims her to three" (trimmed (Just gerrard)) 3
    Spec.assertEqWith s "and without him CR 402.2's seven takes nothing" (trimmed Nothing) 7
    Spec.assertEqWith s "the number CR 402.2's fold seeds itself with, for Gerrard" (PlayerEffect.maximumHandSize S.alice (built mountain (Just gerrard))) (Just 3)
    Spec.assertEqWith s "and for bob beside him" (PlayerEffect.maximumHandSize S.bob (built mountain (Just gerrard))) (Just 7)

  -- CR 902.7 / CR 313.4: "its triggered abilities may trigger" from the command
  -- zone. Gerrard's draw-step trigger against the turn-based draw of CR 504.1:
  -- one draw is the step's own, two is the step's plus his.
  Spec.it s "CR 902.7 a vanguard's triggered ability triggers from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    gerrard <- S.printingOf s registry "Gerrard"
    let drawStep vanguard =
          let board = opened keepAnswer (built mountain vanguard)
              before = handSize board
              -- Turn two, not turn one: CR 103.8a has the starting player skip the
              -- draw of their FIRST turn, which would leave the control leg drawing
              -- nothing and the difference resting on that rule instead of this one.
              after = S.runPure S.identityAnswer (board {GameState.phase = Phase.Beginning BeginningStep.DrawStep, GameState.turnNumber = 2}) Engine.runStep
           in handSize after - before
    Spec.assertEqWith s "with Gerrard, alice draws two" (drawStep (Just gerrard)) 2
    Spec.assertEqWith s "without him, CR 504.1's one" (drawStep Nothing) 1

  -- CR 902.7 / CR 313.4 again: "its static abilities affect the game" from the
  -- command zone. Selenia's grant reaches alice's creature and not bob's, so the
  -- ability's controller is CR 902.6's owner and the walk is not handing the
  -- keyword to the board.
  Spec.it s "CR 902.7 a vanguard's static ability functions from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    selenia <- S.printingOf s registry "Selenia"
    let place vanguard =
          let (mine, g1) = S.addCreature piker S.alice (built mountain vanguard)
              (theirs, g2) = S.addCreature piker S.bob g1
           in (mine, theirs, g2)
        (aliceCreature, bobCreature, board) = place (Just selenia)
        (aliceControl, _, control) = place Nothing
    Spec.assertBool s (Projection.hasKeyword Keyword.Vigilance aliceCreature board) "alice's creature has vigilance"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Vigilance bobCreature board)) "bob's, on the same board, does not"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Vigilance aliceControl control)) "and neither does alice's on the board without the vanguard"

  -- CR 902.7 / CR 313.4, third kind of printed ability: a replacement effect.
  -- Mishra (Vanguard, hand +0 / life -3, "If a creature you control would deal
  -- damage, it deals double that damage instead" -- checked against Scryfall,
  -- 2026-09-01) against a batch holding one hit from alice's creature and one
  -- from bob's, so the rewrite's own "you control" half is read on the same
  -- board and cannot be what makes the pair differ.
  Spec.it s "CR 902.7 a vanguard's replacement effect functions from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    mishra <- S.printingOf s registry "Mishra"
    let place vanguard =
          let (mine, g1) = S.addCreature piker S.alice (built mountain vanguard)
              (theirs, board) = S.addCreature piker S.bob g1
              after = S.runPure S.identityAnswer board (Damage.applyDamage [hit mine (Recipient.ToPlayer S.bob) 3, hit theirs (Recipient.ToPlayer S.alice) 5])
           in (lifeLost S.bob board after, lifeLost S.alice board after)
        (bobLost, aliceLost) = place (Just mishra)
        (bobControl, aliceControl) = place Nothing
    Spec.assertEqWith s "with Mishra, her creature's 3 reaches bob as 6" bobLost (Just 6)
    Spec.assertEqWith s "and bob's 5, on the same board, is not doubled" aliceLost (Just 5)
    Spec.assertEqWith s "without him the 3 stays a 3" bobControl (Just 3)
    Spec.assertEqWith s "and the 5 stays a 5" aliceControl (Just 5)

  -- CR 902.7 / CR 313.4, fourth kind: a combat restriction. No printed vanguard
  -- states one -- every vanguard whose text names attacking or blocking either
  -- grants a keyword (Lyna's shadow, Two-Headed Giant of Foriys Avatar's menace),
  -- pumps (Goblin Warchief Avatar), or states a CR 509.1c REQUIREMENT rather than
  -- a restriction (Sisters of Stone Death Avatar's "must be blocked if able"),
  -- checked over Scryfall's whole `t:vanguard` list on 2026-09-01 -- so the
  -- producer is synthetic, which CR 313.4's "any number of static ... abilities"
  -- is what permits.
  Spec.it s "CR 902.7 a vanguard's combat restriction functions from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    avatar <- S.printingOf s registry "Synthetic Pacifist Avatar"
    let place vanguard =
          let (mine, placed) = S.addCreature piker S.alice (built mountain vanguard)
           in (mine, declaringAttackers placed)
        (barred, board) = place (Just avatar)
        (free, control) = place Nothing
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [barred] board)) "CR 508.1c: her Piker may not be declared attacking"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [free] control) "and may on the board that differs only in the vanguard"
    Spec.assertBool s (not (Combat.canAttack S.alice barred board)) "so it is not a CR 508.1a candidate either"
    Spec.assertEqWith s "where without the vanguard it is the one offered" (Combat.legalAttackers S.alice control) [free]

  -- CR 902.7 / CR 313.4, third limb of rule 902.7's sentence: "its activated
  -- abilities may be activated". Barrin (Vanguard, hand +0 / life +6, "Sacrifice
  -- a permanent: Return target creature to its owner's hand" -- Scryfall pvan,
  -- paper, checked 2026-09-02), whose cost holds no mana, so the board needs no
  -- source. End to end through Engine.priorityLoop: the activation is one alice
  -- takes off the offered menu, so Action.legalActions naming the command-zone
  -- card is what the bounce below rests on.
  --
  -- Three creatures and not two: alice keeps a spare, so the CR 701.21a sacrifice
  -- is a genuine choice rather than a forced one the engine elides, and the spare
  -- surviving is what pins her answer. Bob's is the victim, so CR 400.3's "its
  -- owner's hand" lands somewhere alice's hand does not.
  Spec.it s "CR 902.7 a vanguard's activated ability may be activated from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    barrin <- S.printingOf s registry "Barrin"
    let place vanguard =
          let (fodder, g1) = S.addCreature piker S.alice (built mountain vanguard)
              (spare, g2) = S.addCreature piker S.alice g1
              (victim, g3) = S.addCreature piker S.bob g2
           in (fodder, spare, victim, g3 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})
        (fodderId, spareId, victimId, board) = place (Just barrin)
        -- The control: the same three creatures, the same answerer, and no
        -- vanguard for it to take -- so every assertion below reads the other way
        -- on a board differing in exactly that.
        (controlFodder, _, controlVictim, control) = place Nothing
    case Game.zoneMembers Zone.Command S.alice board of
      [barrinId] -> do
        let played srcId fodder victim gs = snd (State.evalState (Engine.runGame (takesOnce srcId fodder victim) gs Engine.priorityLoop) 0)
            after = played barrinId fodderId victimId board
            without = played barrinId controlFodder controlVictim control
        Spec.assertEqWith s "CR 902.7 bob's Piker is in his hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
        Spec.assertEqWith s "and without the vanguard nothing bounced it" (length (Game.zoneMembers Zone.Hand S.bob without)) 0
        Spec.assertBool s (not (Set.member victimId (GameState.battlefield after))) "so it is off the battlefield"
        Spec.assertBool s (not (Set.member fodderId (GameState.battlefield after))) "CR 701.21a: the permanent she chose was sacrificed"
        Spec.assertBool s (Set.member spareId (GameState.battlefield after)) "and the one she did not choose is still there"
        Spec.assertBool s (any (isActivationOf barrinId) (Action.legalActions S.alice board)) "CR 602.2: alice is offered the ability"
        Spec.assertBool s (not (any (isActivationOf barrinId) (Action.legalActions S.bob board))) "and bob, on the same board, is not"
      _ -> Spec.assertFailure s "the fixture should have put Barrin in alice's command zone"

  -- CR 313.2 against CR 727.2: a restart puts every card into its owner's new
  -- library, and the vanguard card is the exception -- it stays where it is, and
  -- rule 902.4's life total is dealt out again from it.
  Spec.it s "CR 313.2 / 727.1 a restarted game keeps the vanguard in the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    selenia <- S.printingOf s registry "Selenia"
    let restarted = S.runPure keepAnswer (built mountain (Just selenia)) (Setup.restartGame S.performer Set.empty S.alice)
    Spec.assertEqWith s "still in the command zone" (commandNames restarted) [CardName.MkCardName (Text.pack "Selenia")]
    Spec.assertEqWith s "and CR 902.4's twenty-seven is dealt out again" (S.lifeOf S.alice restarted) (Just 27)
    Spec.assertEqWith s "her opening hand is eight again" (handSize restarted) 8

  -- CR 729.2b and CR 729.5b: the vanguard card crosses into a subgame and comes
  -- back out of it, which is the pair CR 729.2c and CR 729.5c make for a
  -- commander.
  Spec.it s "CR 729.2b / 729.5b a subgame takes the vanguard card and gives it back" $ do
    mountain <- S.printingOf s registry "Mountain"
    selenia <- S.printingOf s registry "Selenia"
    let parent = built mountain (Just selenia)
        sub = S.runPure keepAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty)
        back = Setup.funnelBack sub parent
    Spec.assertEqWith s "the subgame's command zone holds it" (commandNames sub) [CardName.MkCardName (Text.pack "Selenia")]
    Spec.assertEqWith s "and CR 902.4 applies inside the subgame too" (S.lifeOf S.alice sub) (Just 27)
    Spec.assertEqWith s "the main game has it back when the subgame ends" (commandNames back) [CardName.MkCardName (Text.pack "Selenia")]
