{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Commander (CR 903.3's designation, CR 903.6's starting zone,
-- CR 903.8's permission and tax, CR 903.9a's state-based action), the
-- Player.commander and Player.commanderCasts fields, Deck's commander, the
-- Zone.Command arm of Pawl.Engine.Cast.castableZones, and the CR 903.8 increase
-- Pawl.Engine.Cost.allAdjustments folds into CR 601.2f. Also the commander half
-- of Pawl.Engine.Setup's subgame pair -- CR 729.2c in and CR 729.5c out -- and
-- the commander half of its restart, CR 727.5a. Those live here rather than in
-- Pawl.SetupSpec because they need a designated commander and this is the file
-- that builds one.
--
-- Shimatsu the Bloodcloaked is the card pool for every group but CR 903.10a's,
-- which needs a commander that can attack and says why it uses its own: {3}{R}
-- Legendary Creature -- Demon Spirit 0/0, "As Shimatsu enters, sacrifice any
-- number of permanents. Shimatsu enters with that many +1/+1 counters on it."
--
-- Chosen because it KILLS ITSELF. Sacrificing nothing leaves a 0/0, which CR
-- 704.5f buries on the next state-based action check -- so the whole CR 903 cycle
-- (cast from the command zone, resolve, die, be offered back, be recast dearer)
-- runs on one card with no removal spell and no second colour of mana.
--
-- Deliberately not Kokusho, the Evening Star, the obvious pick: its dies trigger
-- would have to be modeled honestly, and "you gain life equal to the life lost
-- this way" is a dynamic quantity pawl cannot express. Shimatsu reaches rule 903
-- without reaching for it.
--
-- Its printed cost is {3}{R}, so the tax is directly readable in the mana spent:
-- four the first time, six the second (CR 903.8's {2}), eight the third.
module Pawl.CommanderSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- Alice's board: `lands` Mountains, and Shimatsu designated as her commander and
-- so started in the command zone (CR 903.6). Built through Setup.createDeck, the
-- real path, rather than by placing the object directly -- the designation and the
-- zone are what this file is about.
commanderBoard :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
commanderBoard mountain shimatsu lands =
  let deck = Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = Just shimatsu, Deck.dungeon = Nothing}
      -- A precombat main phase with alice holding priority and an empty stack,
      -- which is Support.handOne's shape. CR 302.1 -- a creature card is cast
      -- "during a main phase of their turn when the stack is empty" -- is a
      -- precondition of the whole file rather than anything rule 903 says: rule
      -- 903.8 changes the ZONE a commander may be cast from, never the timing.
      board =
        (S.landsInPlay mountain lands)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
   in S.runPure S.identityAnswer board (Setup.createDeck S.alice deck)

-- What is in the command zone, in id order -- one object on most boards here,
-- two where a second player is designated one.
inCommandZone :: GameState.GameState -> [ObjectId.ObjectId]
inCommandZone = Set.toAscList . GameState.command

commanderCastsOf :: GameState.GameState -> Maybe Integer
commanderCastsOf gs = fmap (toInteger . Player.commanderCasts) (Map.lookup S.alice (GameState.players gs))

-- How many lands are tapped -- what the mana actually spent on a cast is read off,
-- since every land here is a Mountain tapping for one.
tappedCount :: GameState.GameState -> Int
tappedCount gs =
  length [() | oid <- Set.toList (GameState.battlefield gs), fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Commander" $ do
  designationSpec s registry
  castSpec s registry
  taxSpec s registry
  commanderDamageSpec s registry
  subgameSpec s registry
  restartSpec s registry

designationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
designationSpec s registry = Spec.describe s "Designation" $ do
  -- CR 903.6: "at the start of the game, each player puts their commander from
  -- their deck face up into the command zone", and CR 903.3's designation is
  -- recorded on the player because it "is an attribute of the card itself" that
  -- "the card retains even when it changes zones".
  Spec.it s "CR 903.3/903.6 the commander starts in the command zone, designated" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 4
    Spec.assertEqWith s "one card in the command zone" (length (inCommandZone gs)) 1
    Spec.assertEqWith s "alice is designated it" (fmap Player.commander (Map.lookup S.alice (GameState.players gs))) (Just (Just shimatsu))
    Spec.assertEqWith s "and it is her commander" (fmap (\oid -> Commander.isCommander oid gs) (inCommandZone gs)) [True]
    Spec.assertEqWith s "having cast it no times yet" (commanderCastsOf gs) (Just 0)
  -- The falsifier for a commander smuggled into the library: CR 903.6 puts it in
  -- the command zone and shuffles "the REMAINING cards of their deck" into the
  -- library, so the commander is in exactly one of the two.
  Spec.it s "CR 903.6 the commander is not also in the library" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 4
    Spec.assertEqWith s "alice's library is empty" (length (Game.zoneMembers Zone.Library S.alice gs)) 0
  -- CR 903.5 counts the deck "including its commander", so a deck whose only card
  -- IS the commander is one card, not zero.
  Spec.it s "CR 903.5 the commander counts toward the deck's size" $ do
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let deck = Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = Just shimatsu, Deck.dungeon = Nothing}
    Spec.assertEqWith s "one card" (Setup.deckSize deck) 1
    Spec.assertEqWith s "and none without a commander" (Setup.deckSize (Deck.fromCards Map.empty)) 0

castSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
castSpec s registry = Spec.describe s "Cast" $ do
  -- CR 903.8's first sentence: "a player may cast a commander they own from the
  -- command zone". The command zone is not a castable zone for anything else, so
  -- this is the whole of the permission.
  Spec.it s "CR 903.8 the owner may cast their commander from the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 4
    case inCommandZone gs of
      [oid] -> do
        Spec.assertEqWith s "alice may cast it" (S.castable S.alice oid gs) True
        -- CR 903.8 says "a commander THEY OWN": bob may not cast alice's.
        Spec.assertEqWith s "bob may not" (S.castable S.bob oid gs) False
      _ -> Spec.assertBool s False "expected one commander"
  -- The falsifier for a command zone that became castable for everything: an
  -- object in the command zone that is not a commander is not castable from there.
  Spec.it s "CR 903.8 a non-commander in the command zone is not castable" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 4
        -- Strip the designation, leaving the same object in the same zone.
        undesignated = gs {GameState.players = Map.adjust (\p -> p {Player.commander = Nothing}) S.alice (GameState.players gs)}
    case inCommandZone gs of
      [oid] -> Spec.assertEqWith s "not castable once it is nobody's commander" (S.castable S.alice oid undesignated) False
      _ -> Spec.assertBool s False "expected one commander"

taxSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
taxSpec s registry = Spec.describe s "Tax" $ do
  -- CR 903.8's second sentence, at zero: the FIRST cast from the command zone has
  -- no previous cast to pay for, so it costs the printed {3}{R} and nothing more.
  Spec.it s "CR 903.8 the first cast costs the printed cost" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 10
    case inCommandZone gs of
      [oid] -> do
        Spec.assertEqWith s "no tax yet" (Commander.tax S.alice oid gs) 0
        let after = S.runPure S.identityAnswer gs (S.cast S.alice oid)
        Spec.assertEqWith s "four Mountains paid" (tappedCount after) 4
        Spec.assertEqWith s "and the cast is counted" (commanderCastsOf after) (Just 1)
      _ -> Spec.assertBool s False "expected one commander"
  -- CR 903.9a: Shimatsu resolves as a 0/0, CR 704.5f buries it, and the same CR
  -- 704.3 settle loop then offers its owner the command zone. The whole rule in one
  -- board.
  Spec.it s "CR 903.9a a commander that dies is offered back to the command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 10
    case inCommandZone gs of
      [oid] -> do
        let back = castAndSettle reclaiming oid gs
        Spec.assertEqWith s "it is in the command zone again" (length (inCommandZone back)) 1
        Spec.assertEqWith s "and not in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice back)) 0
        Spec.assertEqWith s "nor on the battlefield" (S.creaturesInPlay S.alice back) 0
      _ -> Spec.assertBool s False "expected one commander"
  -- CR 903.9a is a "may". The default answerer declines, and the commander then
  -- stays in the graveyard -- the falsifier for an engine that moved it without
  -- asking, which would make the case above pass for the wrong reason.
  Spec.it s "CR 903.9a declining leaves it in the graveyard" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 10
    case inCommandZone gs of
      [oid] -> do
        let after = castAndSettle S.identityAnswer oid gs
        Spec.assertEqWith s "the command zone is empty" (length (inCommandZone after)) 0
        Spec.assertEqWith s "and it is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      _ -> Spec.assertBool s False "expected one commander"
  -- CR 903.8's whole point: the SECOND cast from the
  -- command zone costs {2} more. Ten Mountains, four spent on the first cast and
  -- six on the second -- {3}{R} then {5}{R}.
  Spec.it s "CR 903.8 the second cast from the command zone costs {2} more" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 10
    case inCommandZone gs of
      [oid] -> do
        let back = castAndSettle reclaiming oid gs
        Spec.assertEqWith s "one cast so far" (commanderCastsOf back) (Just 1)
        Spec.assertEqWith s "four Mountains spent on it" (tappedCount back) 4
        case inCommandZone back of
          [oid2] -> do
            Spec.assertEqWith s "the tax is now {2}" (Commander.tax S.alice oid2 back) 2
            let twice = castAndSettle reclaiming oid2 back
            Spec.assertEqWith s "two casts now" (commanderCastsOf twice) (Just 2)
            Spec.assertEqWith s "ten Mountains spent in total: four then six" (tappedCount twice) 10
            Spec.assertEqWith s "and the tax is {4} for the next one" (fmap (\o -> Commander.tax S.alice o twice) (inCommandZone twice)) [4]
          _ -> Spec.assertBool s False "expected it back in the command zone"
      _ -> Spec.assertBool s False "expected one commander"

-- CR 903.10a's pool, which Shimatsu cannot serve: a 0/0 dies to CR 704.5f before
-- it can attack.
--
--   * Kalakscion, Hunger Tyrant -- {1}{B}{B} Legendary Creature -- Crocodile 7/2,
--     vanilla. Three attacks deal exactly 21, rule 903.10a's threshold hit on the
--     nose rather than overshot, and 21 against CR 903.7's forty leaves the victim
--     at 19 -- comfortably alive, so CR 704.5a is provably not what killed them. A
--     commander that could deal 21 in one swing would leave the two state-based
--     actions racing.
--   * Jedit Ojanen -- {4}{W}{W}{U} Legendary Creature -- Cat Warrior 5/5, vanilla.
--     The second commander the "same commander" case needs, at a power distinct
--     from 7 so the two tallies can never be confused.
--
-- Every seat gets a commander designated even when it never leaves the command
-- zone, because CR 903.7's forty life is what the victim needs.

-- CR 903.3: designate each seat's commander through Setup.createDeck, the real
-- path -- which is also where CR 903.7's forty life comes from.
designating :: [(PlayerId.PlayerId, Printing.Printing)] -> GameState.GameState -> GameState.GameState
designating seats gs0 =
  let one g (pid, printing) =
        S.runPure S.identityAnswer g $
          Setup.createDeck pid Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = Just printing, Deck.dungeon = Nothing}
   in List.foldl' one gs0 seats

-- Move a player's commander out of the command zone onto the battlefield through
-- Event.changeZone. Deliberately not CAST: CR 903.8's cast is the Tax group's
-- business, and this group needs nothing but the commander in play.
intoPlay :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
intoPlay pid gs =
  let mine = filter (\oid -> fmap Object.owner (Game.lookupObject oid gs) == Just pid) (inCommandZone gs)
   in List.foldl' (\g oid -> S.runPure S.identityAnswer g (Event.changeZone oid Zone.Battlefield)) gs mine

-- alice's commander on the battlefield, bob's still in the command zone.
commanderDuel :: Printing.Printing -> Printing.Printing -> GameState.GameState
commanderDuel mine theirs =
  intoPlay S.alice (designating [(S.alice, mine), (S.bob, theirs)] (Setup.emptyGame S.bothPlayers))

-- The commander `pid` owns on the battlefield, if any.
commanderInPlay :: PlayerId.PlayerId -> GameState.GameState -> Maybe ObjectId.ObjectId
commanderInPlay pid gs =
  let ours oid = Commander.isCommander oid gs && fmap Object.owner (Game.lookupObject oid gs) == Just pid
   in List.find ours (Set.toList (GameState.battlefield gs))

-- CR 903.10a's tally: the combat damage `victim` has been dealt by the commander
-- `owner` brought.
tallyFrom :: PlayerId.PlayerId -> PlayerId.PlayerId -> GameState.GameState -> Natural
tallyFrom owner victim gs =
  maybe 0 (Map.findWithDefault 0 owner . Player.commanderDamage) (Map.lookup victim (GameState.players gs))

-- One more combat with `attacker` active: CR 502.3's untap, which also ends CR
-- 302.6's summoning sickness, and then the combat phase run step by step through
-- the engine. Everything else on the board carries over, which is what rule
-- 903.10a's "over the course of the game" is about.
--
-- The steps between combats are skipped rather than played, so nobody draws and
-- CR 104.3c cannot decide a case here.
swing :: (forall r. Prompt.Prompt r -> r) -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
swing answer attacker gs =
  let untapped =
        S.runPure answer gs {GameState.activePlayer = attacker} $
          Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap)
   in S.runCombat
        answer
        untapped
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.combat = Combat.emptyCombat,
            GameState.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareAttackers,
                  Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
          }

statusOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Status.Status
statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))

commanderDamageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
commanderDamageSpec s registry = Spec.describe s "CommanderDamage" $ do
  -- CR 903.10a counts damage "by the same commander", so a creature that is not
  -- one contributes nothing however much it deals. Both attack in one combat, so
  -- the two amounts are told apart by the tally alone.
  Spec.it s "CR 903.10a only the commander's combat damage is tallied" $ do
    kalakscion <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, board) = S.addCreature piker S.alice (commanderDuel kalakscion jedit)
        after = swing S.aggressiveAnswer S.alice board
    Spec.assertEqWith s "bob took 7 from the commander and 2 from the Piker" (S.lifeOf S.bob after) (Just 31)
    Spec.assertEqWith s "and only the 7 was tallied" (tallyFrom S.alice S.bob after) 7
  -- CR 903.10a counts COMBAT damage, so the same commander dealing the same
  -- twenty-one points outside combat tallies nothing -- and the victim, at the
  -- same 19 the lethal case below leaves them at, is still playing.
  Spec.it s "CR 903.10a noncombat damage from a commander is not tallied" $ do
    kalakscion <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    let board = commanderDuel kalakscion jedit
    case commanderInPlay S.alice board of
      Nothing -> Spec.assertBool s False "expected alice's commander on the battlefield"
      Just oid -> do
        let event = Damage.damageEvent board DamageKind.Noncombat oid (Recipient.ToPlayer S.bob) 21
            after = S.settleSba (S.runPure S.identityAnswer board (Damage.applyDamage [event]))
        Spec.assertEqWith s "bob lost the 21 life" (S.lifeOf S.bob after) (Just 19)
        Spec.assertEqWith s "nothing was tallied" (tallyFrom S.alice S.bob after) 0
        Spec.assertEqWith s "and he is still playing" (statusOf S.bob after) (Just Status.Playing)
  -- CR 903.10a itself: three 7-point swings are exactly 21.
  Spec.it s "CR 903.10a twenty-one combat damage from one commander loses the game" $ do
    kalakscion <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    let board = commanderDuel kalakscion jedit
        after = List.foldl' (\g _ -> swing S.aggressiveAnswer S.alice g) board [1 .. 3 :: Int]
    Spec.assertEqWith s "bob's tally is 21" (tallyFrom S.alice S.bob after) 21
    -- CR 903.7's forty is what makes this assertion the load-bearing one: at 19
    -- CR 704.5a has not fired, so rule 903.10a is the only rule that can have.
    Spec.assertEqWith s "and his life is 19, so CR 704.5a did not kill him" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "bob lost" (statusOf S.bob after) (Just (Status.Departed Departure.Type.Lost))
    Spec.assertEqWith s "so alice won" (GameState.result after) (Just (Result.Won S.alice))
  -- The negative control, on the board above stopped one swing earlier: 14 is not
  -- 21, and nothing else about the board differs.
  Spec.it s "CR 903.10a fourteen does not" $ do
    kalakscion <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    let board = commanderDuel kalakscion jedit
        after = List.foldl' (\g _ -> swing S.aggressiveAnswer S.alice g) board [1 .. 2 :: Int]
    Spec.assertEqWith s "bob's tally is 14" (tallyFrom S.alice S.bob after) 14
    Spec.assertEqWith s "he is at 26" (S.lifeOf S.bob after) (Just 26)
    Spec.assertEqWith s "still playing" (statusOf S.bob after) (Just Status.Playing)
    Spec.assertEqWith s "and the game has no result" (GameState.result after) Nothing
  -- "By the SAME commander", which two seats cannot tell from "by commanders":
  -- carol is dealt 14 by alice's Kalakscion and 10 by bob's Jedit, which is 24 in
  -- all and neither tally at 21.
  Spec.it s "CR 903.10a damage from two different commanders does not combine" $ do
    kalakscion <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let seated = designating [(S.alice, kalakscion), (S.bob, jedit), (S.carol, shimatsu)] S.threePlayerGame
        board = intoPlay S.bob (intoPlay S.alice seated)
        after = List.foldl' (flip (swing (S.attackTo S.carol))) board [S.alice, S.alice, S.bob, S.bob]
    Spec.assertEqWith s "alice's commander dealt carol 14" (tallyFrom S.alice S.carol after) 14
    Spec.assertEqWith s "bob's dealt her 10" (tallyFrom S.bob S.carol after) 10
    Spec.assertEqWith s "24 in all, so she is at 16" (S.lifeOf S.carol after) (Just 16)
    Spec.assertEqWith s "and neither tally reaches 21, so she is still playing" (statusOf S.carol after) (Just Status.Playing)
    Spec.assertEqWith s "with no result" (GameState.result after) Nothing

-- Accepts CR 903.9a's offer; everything else is the identity answerer. The
-- default (Script.declining, via Replay.defaultAnswer) LEAVES the commander where
-- it is, so a test that wants the return has to say so -- which is what makes the
-- pair of cases below discriminating.
reclaiming :: Prompt.Prompt r -> r
reclaiming p = case p of
  Prompt.ReturnCommander {} -> CommandZoneDecision.Returns
  _ -> S.identityAnswer p

-- Cast the commander and let it resolve, then settle state-based actions -- which
-- is where CR 704.5f buries the 0/0 and CR 903.9a offers it back.
castAndSettle :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castAndSettle answer oid gs =
  let cast = S.runPure answer gs (S.cast S.alice oid)
      resolved = S.runPure answer cast Stack.resolveTop
   in S.runPure answer resolved Engine.settleForPriority

-- Alice's board with her LIBRARY stocked, which commanderBoard's is not: its
-- deck holds nothing but the commander, so every library count below would
-- compare 0 to 0 and pass vacuously. Five Mountains in the deck and two on the
-- battlefield make the library the other destination this group tells apart
-- from the command zone, and give the parent non-library survivors besides.
subgameParent :: Printing.Printing -> Printing.Printing -> GameState.GameState
subgameParent mountain shimatsu =
  let deck = Deck.MkDeck {Deck.cards = Map.singleton mountain 5, Deck.commander = Just shimatsu, Deck.dungeon = Nothing}
   in S.runPure S.identityAnswer (S.landsInPlay mountain 2) (Setup.createDeck S.alice deck)

-- The subgame as playSubgame builds it: CR 729.2 / 729.2c's move in, then CR
-- 103's setup. Both halves, because startGameFromCards is what would funnel a
-- commander that entered the subgame's command zone straight into a library.
playedSubgame :: GameState.GameState -> GameState.GameState
playedSubgame parent =
  snd (Engine.runGamePure S.identityAnswer (Setup.subgameStateFrom S.alice parent) (Setup.startGameFromCards S.performer Set.empty))

-- Take the commander out of the subgame's command zone and put it in alice's
-- subgame graveyard, which is CR 729.5c's "(if it's there)" being false. Written
-- by hand rather than cast and killed: what rule 729.5 reads is the ZONE the
-- card is in as the subgame ends, and a subgame cast is a different unit's path.
--
-- Under a FRESH id, because CR 400.7 mints one on every zone change and a
-- commander that really left would have crossed two of them. That is the shape
-- funnelBack has to survive: the id the parent's command zone still names no
-- longer exists in the subgame, so keeping the parent's copy would leave a
-- second object for one card.
outOfCommandZone :: GameState.GameState -> GameState.GameState
outOfCommandZone gs = case inCommandZone gs of
  [] -> gs
  oid : _ -> case Game.lookupObject oid gs of
    Nothing -> gs
    Just obj ->
      let (new, gs1) = Game.freshObjectId gs
          dead = (Object.newIncarnation obj) {Object.zone = Zone.Graveyard}
       in gs1
            { GameState.command = Set.delete oid (GameState.command gs1),
              GameState.graveyard = Map.insertWith (<>) S.alice (Seq.singleton new) (GameState.graveyard gs1),
              GameState.objects = Map.insert new dead (Map.delete oid (GameState.objects gs1))
            }

subgameSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
subgameSpec s registry = Spec.describe s "Subgame" $ do
  -- CR 729.2c: "as a subgame of a Commander game starts, each player moves their
  -- commander from the main-game command zone (if it's there) to the subgame
  -- command zone". CR 729.2's pool is the library cards, so the commander is the
  -- one card that enters a subgame from anywhere else.
  Spec.it s "CR 729.2c the commander enters the subgame's command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let parent = subgameParent mountain shimatsu
        moved = Setup.subgameStateFrom S.alice parent
        sub = playedSubgame parent
    -- Pins the fixture: an empty parent command zone or an empty library would
    -- make everything below pass for the wrong reason.
    Spec.assertEqWith s "the parent really has one commander in its command zone" (length (inCommandZone parent)) 1
    Spec.assertEqWith s "and a five-card library to tell it apart from" (length (Game.zoneMembers Zone.Library S.alice parent)) 5
    -- The move itself, before CR 103's setup runs: subgameStateFrom is the half
    -- of playSubgame that rule 729.2c names, and it has to leave the subgame
    -- state self-consistent -- the object in the pool AND in the zone set.
    Spec.assertEqWith s "the moved-in state already has it in the subgame command zone" (inCommandZone moved) (inCommandZone parent)
    Spec.assertEqWith s "and the object came with it" (fmap (`Map.member` GameState.objects moved) (inCommandZone parent)) [True]
    Spec.assertEqWith s "the subgame's command zone holds one card too" (length (inCommandZone sub)) 1
    Spec.assertEqWith s "the subgame knows it as alice's commander" (fmap (\oid -> Commander.isCommander oid sub) (inCommandZone sub)) [True]
    Spec.assertEqWith s "and it sits in the subgame's command zone, not a library" (fmap (\oid -> fmap Object.zone (Game.lookupObject oid sub)) (inCommandZone sub)) [Just Zone.Command]
    -- CR 729.1a: the parent is untouched while the subgame runs, so the move in
    -- is a copy at this point; funnelBack is what settles where the card ends up.
    Spec.assertEqWith s "the main-game command zone is unchanged by the subgame" (inCommandZone parent) (inCommandZone (subgameParent mountain shimatsu))
  -- CR 729.5c: "at the end of a subgame of a Commander game, each player moves
  -- their commander from the subgame command zone (if it's there) to the
  -- main-game command zone" -- and CR 729.5's first sentence excludes it from the
  -- cards that go to the library. The library is the destination the old code
  -- chose, so the two counts together are what discriminate.
  Spec.it s "CR 729.5/729.5c a commander in the subgame command zone comes back to the main-game command zone" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let parent = subgameParent mountain shimatsu
        sub = playedSubgame parent
        after = Setup.funnelBack sub parent
    Spec.assertEqWith s "it really ended the subgame in the subgame command zone" (length (inCommandZone sub)) 1
    Spec.assertEqWith s "one card in the main-game command zone" (length (inCommandZone after)) 1
    Spec.assertEqWith s "and it is still alice's commander" (fmap (\oid -> Commander.isCommander oid after) (inCommandZone after)) [True]
    Spec.assertEqWith
      s
      "alice's library is exactly the size it was, so the commander is not in it"
      (length (Game.zoneMembers Zone.Library S.alice after))
      (length (Game.zoneMembers Zone.Library S.alice parent))
    Spec.assertEqWith s "the parent's two battlefield lands survived (CR 729.5's untouched main game)" (Set.size (GameState.battlefield after)) 2
    Spec.assertEqWith s "and no object id collides: the card is in exactly one zone" (Map.size (GameState.objects after)) (2 + 5 + 1)
  -- CR 729.5c's "(if it's there)" is a real condition, not a licence to spare
  -- every commander: one that ended the subgame anywhere else is an ordinary
  -- traditional card and CR 729.5's first sentence puts it in the main-game
  -- library. The board differs from the case above in the subgame ZONE alone.
  Spec.it s "CR 729.5 a commander that left the subgame command zone goes to the main-game library" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let parent = subgameParent mountain shimatsu
        played = playedSubgame parent
        sub = outOfCommandZone played
        after = Setup.funnelBack sub parent
    -- The pair: `played` and `sub` differ in the commander's subgame zone and in
    -- nothing else, so the first assertion is what keeps the second from holding
    -- because there was never a commander there to move.
    Spec.assertEqWith s "it was in the subgame's command zone to begin with" (length (inCommandZone played)) 1
    Spec.assertEqWith s "it really left the subgame's command zone" (length (inCommandZone sub)) 0
    Spec.assertEqWith s "nothing is left in the main-game command zone" (length (inCommandZone after)) 0
    Spec.assertEqWith
      s
      "alice's library is one card bigger: the commander came back to it instead"
      (length (Game.zoneMembers Zone.Library S.alice after))
      (length (Game.zoneMembers Zone.Library S.alice parent) + 1)
    Spec.assertEqWith s "she is still designated it (CR 903.3 survives the subgame)" (fmap Player.commander (Map.lookup S.alice (GameState.players after))) (Just (Just shimatsu))
    Spec.assertEqWith s "and no copy is left behind" (Map.size (GameState.objects after)) (2 + 5 + 1)
  -- CR 729.1b: nothing that happened in the subgame means anything in the main
  -- game, so a player who LOST the subgame is still playing the main one and
  -- their commander is still in its command zone. Three seats, because CR 800.1
  -- makes only a multiplayer subgame reach CR 800.4a's object removal -- which
  -- deletes bob's subgame commander outright, leaving funnelBack nothing to move
  -- back and the parent's own copy as the only source.
  Spec.it s "CR 729.5c a commander whose owner departed inside a multiplayer subgame is still in the main-game command zone" $ do
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    let parent = designating [(S.alice, shimatsu), (S.bob, jedit)] (Setup.emptyGame S.threePlayers)
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, seated) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer Set.empty)
        departed = Departure.depart Departure.Type.Lost S.bob seated
        after = Setup.funnelBack departed parent
    Spec.assertEqWith s "two commanders went in" (length (inCommandZone parent)) 2
    Spec.assertEqWith s "the subgame really was multiplayer, so CR 800.4a's removal fired" (Departure.continuesAfterDeparture departed) True
    Spec.assertEqWith s "and bob has nothing left in the subgame" (Map.keys (Map.filter (\o -> Object.owner o == S.bob) (GameState.objects departed))) []
    Spec.assertEqWith s "both are back in the main-game command zone" (fmap (\oid -> fmap Object.owner (Game.lookupObject oid after)) (inCommandZone after)) [Just S.alice, Just S.bob]
    Spec.assertEqWith s "and neither was funnelled into a library" (length (Game.zoneMembers Zone.Library S.alice after) + length (Game.zoneMembers Zone.Library S.bob after)) 0

restartSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
restartSpec s registry = Spec.describe s "Restart" $ do
  -- CR 727.5a: "in a Commander game, a commander that has been exempted from the
  -- procedure that restarts the game won't begin the new game in the command
  -- zone. However, it remains that deck's commander for the new game."
  --
  -- One sentence each, on a pair of restarts that differ in the exempt set and
  -- in nothing else. Both halves live in Pawl.Engine.Setup and this is what
  -- proves them: the first in startGameFromCards, which drops the
  -- exempt objects before it picks each owner's commander out of the rebuilt
  -- pool; the second in resetPlayers, which deliberately leaves Player.commander
  -- alone.
  Spec.it s "CR 727.5a an exempted commander does not begin the new game in the command zone but is still the deck's commander" $ do
    mountain <- S.printingOf s registry "Mountain"
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let gs = commanderBoard mountain shimatsu 4
    case inCommandZone gs of
      [cmd] -> do
        -- Exile first, because CR 727.5's only producer (Karn Liberated)
        -- exiles before it restarts, and because startGameFromCards intersects
        -- the exempt set with GameState.exile -- an exemption naming a card in
        -- the command zone reaches nothing at all.
        let exiled = S.runPure S.identityAnswer gs (Event.changeZone cmd Zone.Exile)
            -- CR 400.7 mints a fresh id on the way to exile, so the exempt set
            -- has to name the POST-move object, read back out of the zone.
            oid = case Set.toAscList (GameState.exile exiled) of
              o : _ -> o
              [] -> S.noSource
            after = S.runPure S.identityAnswer exiled (Setup.restartGame S.performer (Set.singleton oid) S.alice)
            kept = S.runPure S.identityAnswer exiled (Setup.restartGame S.performer Set.empty S.alice)
        -- Pins the fixture: without these every assertion below could hold
        -- because there was no commander in exile to exempt.
        Spec.assertEqWith s "the commander really reached exile, as one object" (Set.size (GameState.exile exiled)) 1
        Spec.assertEqWith s "and is still alice's commander there" (Commander.isCommander oid exiled) True
        -- CR 727.5a's first sentence: the discriminating assertion.
        Spec.assertEqWith s "the exempted commander is not in the new game's command zone" (inCommandZone after) []
        Spec.assertEqWith s "CR 727.5: it stayed in exile, where the exemption left it" (Set.member oid (GameState.exile after)) True
        -- CR 727.5a's second sentence, read both off the player and off the
        -- object, since Commander.isCommander is what every other rule asks.
        Spec.assertEqWith s "it remains that deck's commander" (fmap Player.commander (Map.lookup S.alice (GameState.players after))) (Just (Just shimatsu))
        Spec.assertEqWith s "so the exiled card is still recognised as her commander" (Commander.isCommander oid after) True
        -- The control leg, and the reason the first assertion is not passing on
        -- an engine that never refills the command zone: the SAME board and the
        -- SAME restart, exempting nothing, puts the very same object back (CR
        -- 903.6). startGameFromCards reuses the object as a key, so the id is
        -- comparable across the rebuild.
        Spec.assertEqWith s "control leg: unexempted, CR 903.6 puts it back in the command zone" (inCommandZone kept) [oid]
        Spec.assertEqWith s "and it left exile to get there" (Set.member oid (GameState.exile kept)) False
      _ -> Spec.assertFailure s "fixture should give alice one commander in the command zone"
