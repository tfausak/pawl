{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Replacement over enters-the-battlefield replacements (CR 614.1c,
-- CR 614.12, CR 122.6): riot, unleash and bloodthirst, entering tapped or with
-- counters, and the as-enters choices from Agent's Toolkit to Squad Captain.
-- Split out of Pawl.ReplacementSpec, which keeps the machinery.
module Pawl.EntryReplacementSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import Pawl.DamageReplacementSpec (tapStateOf)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Int as Int
import Pawl.PreventionSpec (answersFor, castAndResolve, counterBoard, countersOn, newestNamed, nextTurn, raceAnswer, theAbility, wasAskedToReplace)
import qualified Pawl.Registry as Registry
import Pawl.ReplacementSpec (atDeclareAttackers, attackersIn, controlledNamed, declineLastRiot, riotAsks, riotBoard, riotChoosing, wasAskedForRiot)
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure

riotSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
riotSpec s registry = Spec.describe s "Riot (CR 702.136)" $ do
  -- Zhur-Taa Goblin and not Spider-Punk, whose file also carries "spells and
  -- abilities can't be countered" and a riot-granting static ability: the
  -- keyword is what is under test, and this printing is nothing but the keyword.
  Spec.it s "CR 702.136a taking the counter enters a 3/3 with no haste" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
    case held of
      goblinCard : _ ->
        let after = S.runPure (riotChoosing OptionalDecision.Exercises) gs (S.cast S.alice goblinCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne goblin after) 1
                -- Printed 2/2, so the counter is visible in the projection (CR
                -- 613.4c, layer 7c).
                Spec.assertEqWith s "power" (Projection.powerOf goblin after) (Just 3)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf goblin after) (Just 3)
                -- CR 702.136a's "if you don't" is what grants haste, so taking
                -- the counter must not.
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste goblin after)) "no haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  Spec.it s "CR 702.136a declining the counter grants haste instead" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
    case held of
      goblinCard : _ ->
        let after = S.runPure (riotChoosing OptionalDecision.Declines) gs (S.cast S.alice goblinCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne goblin after) 0
                Spec.assertEqWith s "power" (Projection.powerOf goblin after) (Just 2)
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste goblin after) "haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- THE PAIR THAT MAKES THE HASTE REAL. CR 302.6 keeps a creature that entered
  -- this turn from attacking, and CR 702.10b is the exception riot buys.
  Spec.it s "CR 702.10b the goblin that took haste attacks the turn it entered" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        answer = riotChoosing OptionalDecision.Declines
    case held of
      goblinCard : _ ->
        let entered = S.runPure answer gs (S.cast S.alice goblinCard >> Stack.resolveTop)
            after = S.runPure answer (atDeclareAttackers entered) (Combat.declareAttackers S.manaPerformer S.alice)
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> Spec.assertEqWith s "attacks" (attackersIn after) [goblin]
      _ -> Spec.assertFailure s "fixture did not deal a card"
  Spec.it s "CR 302.6 the goblin that took the counter cannot attack that turn" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        answer = riotChoosing OptionalDecision.Exercises
    case held of
      goblinCard : _ ->
        let entered = S.runPure answer gs (S.cast S.alice goblinCard >> Stack.resolveTop)
            after = S.runPure answer (atDeclareAttackers entered) (Combat.declareAttackers S.manaPerformer S.alice)
         in Spec.assertEqWith s "no attackers" (attackersIn after) []
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- THE CHOICE IS THE ANSWERER'S. Both outcomes above are reachable only through
  -- a prompt, and the prompt is never elided: CR 702.136a's two halves are
  -- distinguishable on every board.
  Spec.it s "CR 702.136a the controller is asked, and the engine chooses nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
    case held of
      goblinCard : _ ->
        let asked = answersFor S.identityAnswer gs (S.cast S.alice goblinCard >> Stack.resolveTop)
         in Spec.assertBool s (wasAskedForRiot asked) "a ChooseRiot was raised"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- The control that keeps the case above from passing for the wrong reason: a
  -- creature WITHOUT riot enters through the same funnel and is asked nothing.
  Spec.it s "CR 702.136a a creature without riot raises no riot prompt" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, held) = riotBoard mountain 3 forest 0 [pikerPrinting]
    case held of
      pikerCard : _ ->
        let asked = answersFor S.identityAnswer gs (S.cast S.alice pikerCard >> Stack.resolveTop)
         in Spec.assertBool s (not (wasAskedForRiot asked)) "no ChooseRiot was raised"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.136a through a GRANT rather than a printing: Spider-Punk's "other
  -- Spiders you control have riot". The keyword reaches the entering Giant
  -- Spider through layer 6 (CR 613.1f), and the replacement is minted off that
  -- post-layer projection -- which is the whole reason the mint lives there.
  Spec.it s "CR 702.136a Spider-Punk gives another Spider riot as it enters" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spiderPunk <- S.printingOf s registry "Spider-Punk"
    giantSpider <- S.printingOf s registry "Giant Spider"
    let (gs, held) = riotBoard mountain 3 forest 1 [giantSpider]
        (_, board) = S.addPermanent spiderPunk S.alice gs
        answer = riotChoosing OptionalDecision.Declines
    case held of
      spiderCard : _ ->
        let asked = answersFor answer board (S.cast S.alice spiderCard >> Stack.resolveTop)
            after = S.runPure answer board (S.cast S.alice spiderCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Giant Spider") after of
              Nothing -> Spec.assertFailure s "Giant Spider did not reach the battlefield"
              Just spider -> do
                Spec.assertBool s (wasAskedForRiot asked) "a ChooseRiot was raised for the granted riot"
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste spider after) "haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- The grant from a permanent that has no riot of its own: Rhythm of the Wild
  -- is an enchantment whose whole riot contribution is "nontoken creatures you
  -- control have riot", so it is the case Spider-Punk cannot make -- the
  -- entering creature's base face prints no riot AND the granting permanent's
  -- prints none either, which is what Projection.grantsKeywordWhere is for.
  Spec.it s "CR 702.136a Rhythm of the Wild gives an entering creature riot" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    rhythm <- S.printingOf s registry "Rhythm of the Wild"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, held) = riotBoard mountain 2 forest 0 [pikerPrinting]
        (_, board) = S.addPermanent rhythm S.alice gs
        answer = riotChoosing OptionalDecision.Declines
    case held of
      pikerCard : _ ->
        let asked = answersFor answer board (S.cast S.alice pikerCard >> Stack.resolveTop)
            after = S.runPure answer board (S.cast S.alice pikerCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Goblin Piker") after of
              Nothing -> Spec.assertFailure s "Goblin Piker did not reach the battlefield"
              Just piker -> do
                Spec.assertBool s (wasAskedForRiot asked) "a ChooseRiot was raised for the granted riot"
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste piker after) "haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- The control for the grant: the same Spider on the same board with no
  -- Spider-Punk is asked nothing and gains nothing.
  Spec.it s "CR 702.136a without Spider-Punk that Spider has no riot" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    giantSpider <- S.printingOf s registry "Giant Spider"
    let (gs, held) = riotBoard mountain 3 forest 1 [giantSpider]
        answer = riotChoosing OptionalDecision.Declines
    case held of
      spiderCard : _ ->
        let asked = answersFor answer gs (S.cast S.alice spiderCard >> Stack.resolveTop)
            after = S.runPure answer gs (S.cast S.alice spiderCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Giant Spider") after of
              Nothing -> Spec.assertFailure s "Giant Spider did not reach the battlefield"
              Just spider -> do
                Spec.assertBool s (not (wasAskedForRiot asked)) "no ChooseRiot was raised"
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste spider after)) "no haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.136b: "If a permanent has multiple instances of riot, each works
  -- separately." Zhur-Taa Goblin PRINTS riot and Rhythm of the Wild GRANTS it to
  -- her nontoken creatures, so the goblin enters holding riot twice -- two
  -- textually identical replacement abilities on ONE source, which is the shape
  -- CR 614.5's identity had to learn to tell apart.
  --
  -- The single-riot leg in the same case is what keeps the counter count from
  -- being read as a doubling: one instance still asks once and still yields one
  -- counter, on a board differing only in the enchantment.
  Spec.it s "CR 702.136b riot twice is asked twice, and both counters land" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    rhythm <- S.printingOf s registry "Rhythm of the Wild"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        (_, board) = S.addPermanent rhythm S.alice gs
        answer = riotChoosing OptionalDecision.Exercises
    case held of
      goblinCard : _ ->
        let play = S.cast S.alice goblinCard >> Stack.resolveTop
            after = S.runPure answer board play
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                Spec.assertEqWith s "two ChooseRiot were raised" (riotAsks (answersFor answer board play)) 2
                Spec.assertEqWith s "one ChooseRiot without the grant" (riotAsks (answersFor answer gs play)) 1
                Spec.assertEqWith s "two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne goblin after) 2
                -- Printed 2/2 (CR 613.4c, layer 7c).
                Spec.assertEqWith s "power" (Projection.powerOf goblin after) (Just 4)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf goblin after) (Just 4)
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste goblin after)) "no haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.136b's "each works separately" read at its sharpest: the rule's own
  -- consequence is that one instance may take the counter while the other takes
  -- haste, which no single application of riot can produce -- CR 702.136a's two
  -- halves are exclusive.
  Spec.it s "CR 702.136b one instance takes the counter and the other takes haste" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    rhythm <- S.printingOf s registry "Rhythm of the Wild"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        (_, board) = S.addPermanent rhythm S.alice gs
        answer = riotChoosing OptionalDecision.Exercises
    case held of
      goblinCard : _ ->
        let play = S.cast S.alice goblinCard >> Stack.resolveTop
            script = declineLastRiot (answersFor answer board play)
            ((_, after), desync) = Replay.replay script board play
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                -- An exhausted or mismatched transcript would fall back to
                -- Replay.defaultAnswer, which would decide riot itself.
                Spec.assertBool s (Maybe.isNothing desync) "the transcript answered every prompt"
                Spec.assertEqWith s "the exercised instance's counter" (countersOn CounterKind.PlusOnePlusOne goblin after) 1
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste goblin after) "and the declined instance's haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"

-- Answer unleash's "may" one way, and everything else the way S.aggressiveAnswer
-- does -- riotChoosing's shape, one keyword over.
unleashChoosing :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
unleashChoosing choice p = case p of
  Prompt.ChooseUnleash {} -> choice
  _ -> S.aggressiveAnswer p

-- How many times unleash's "may" was put to a player.
unleashAsks :: [Response.Response] -> Int
unleashAsks responses =
  let isUnleash r = case r of
        Response.ChoseUnleash _ -> True
        _ -> False
   in length (filter isUnleash responses)

-- CR 702.98a's two static abilities, on Gore-House Chainwalker ({1}{R} 2/1 with
-- unleash) -- the first keyword in pawl to mint a COMBAT RESTRICTION, where riot
-- above mints only a replacement effect.
--
-- Gameplay-level throughout: every case casts the card and answers the prompt, so
-- the counter and the restriction are both reached the way a game reaches them.
-- The two answers are asserted against each other on boards differing in nothing
-- but the answer, which is what keeps `canBlock` answering False for the reason
-- under test rather than for a tapped or missing creature.
unleashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
unleashSpec s registry = Spec.describe s "Unleash (CR 702.98)" $ do
  Spec.it s "CR 702.98a taking the counter makes it bigger and stops it blocking" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    spider <- S.printingOf s registry "Giant Spider"
    let (gs0, held) = riotBoard mountain 2 forest 0 [chainwalker]
        -- A second creature on the SAME board, so a blanket "nobody may block"
        -- bug cannot pass: the minted restriction has to be narrow to the
        -- permanent holding the keyword. Giant Spider rather than a second
        -- Chainwalker, and 2/1 against 2/5, so no reading of the board confuses
        -- the two.
        (bystander, gs) = S.addPermanent spider S.alice gs0
        answer = unleashChoosing OptionalDecision.Exercises
    case held of
      card : _ ->
        let play = S.cast S.alice card >> Stack.resolveTop
            after = S.runPure answer gs play
         in case newestNamed (CardName.MkCardName $ Text.pack "Gore-House Chainwalker") after of
              Nothing -> Spec.assertFailure s "Gore-House Chainwalker did not reach the battlefield"
              Just walker -> do
                Spec.assertEqWith s "one ChooseUnleash was raised" (unleashAsks (answersFor answer gs play)) 1
                Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne walker after) 1
                -- Printed 2/1, so the counter shows in the projection (CR 613.4c,
                -- layer 7c).
                Spec.assertEqWith s "power" (Projection.powerOf walker after) (Just 3)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf walker after) (Just 2)
                -- CR 509.1b through rule 702.98a's second static ability.
                Spec.assertBool s (not (Combat.canBlock S.alice walker after)) "it cannot block"
                Spec.assertBool s (Combat.canBlock S.alice bystander after) "the Spider beside it can"
                Spec.assertEqWith s "and only the Spider is offered" (Combat.legalBlockers S.alice after) [bystander]
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- THE PAIR THAT MAKES THE RESTRICTION REAL. Same board, same fixture, opposite
  -- answer: rule 702.98a's second ability keys on the counter, so declining puts
  -- the creature back among the legal blockers.
  Spec.it s "CR 702.98a declining leaves a 2/1 that can block" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    spider <- S.printingOf s registry "Giant Spider"
    let (gs0, held) = riotBoard mountain 2 forest 0 [chainwalker]
        (bystander, gs) = S.addPermanent spider S.alice gs0
        answer = unleashChoosing OptionalDecision.Declines
    case held of
      card : _ ->
        let after = S.runPure answer gs (S.cast S.alice card >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Gore-House Chainwalker") after of
              Nothing -> Spec.assertFailure s "Gore-House Chainwalker did not reach the battlefield"
              Just walker -> do
                Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne walker after) 0
                Spec.assertEqWith s "power" (Projection.powerOf walker after) (Just 2)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf walker after) (Just 1)
                -- Rule 702.98a states no consequence for declining, where riot
                -- grants haste, so the board holds nothing but a 2/1.
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste walker after)) "no haste"
                Spec.assertBool s (Combat.canBlock S.alice walker after) "it can block"
                Spec.assertEqWith s "and both creatures are offered" (List.sort (Combat.legalBlockers S.alice after)) (List.sort [bystander, walker])
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.98a says "a +1/+1 counter", not "that counter", so the restriction is
  -- not a fact about how the permanent entered. A counter arriving later shuts
  -- blocking off on a board where the controller DECLINED, which no reading tied
  -- to the entry replacement can produce.
  Spec.it s "CR 702.98a a +1/+1 counter arriving later shuts blocking off too" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    spider <- S.printingOf s registry "Giant Spider"
    let (gs0, held) = riotBoard mountain 2 forest 0 [chainwalker]
        (bystander, gs) = S.addPermanent spider S.alice gs0
        answer = unleashChoosing OptionalDecision.Declines
    case held of
      card : _ ->
        let after = S.runPure answer gs (S.cast S.alice card >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Gore-House Chainwalker") after of
              Nothing -> Spec.assertFailure s "Gore-House Chainwalker did not reach the battlefield"
              Just walker -> do
                -- A STATE fixture for the counters, S.addCounter's documented use:
                -- nothing in the pool puts a +1/+1 counter on each of two
                -- creatures at a moment this board can reach.
                let counted = S.addCounter CounterKind.PlusOnePlusOne 1 bystander (S.addCounter CounterKind.PlusOnePlusOne 1 walker after)
                Spec.assertBool s (Combat.canBlock S.alice walker after) "without the counter it blocks"
                Spec.assertBool s (not (Combat.canBlock S.alice walker counted)) "with one it cannot"
                -- THE COUNTER IS NOT THE WHOLE CONDITION. The Spider carries the
                -- same counter and no unleash, so the restriction has to name its
                -- own source (CR 109.5) rather than every counter-bearing
                -- creature on the board.
                Spec.assertBool s (Combat.canBlock S.alice bystander counted) "the Spider with the same counter still blocks"
      _ -> Spec.assertFailure s "fixture did not deal a card"

-- alice controls three untapped Swamps on a THREE-SEAT board and holds one
-- Bloodrage Vampire, in her precombat main phase with priority; bob controls one
-- Ogre Sentry. Returns the state, the card in hand and the Sentry.
--
-- Three seats rather than riotBoard's two, and for rule 702.54a's word "an
-- opponent": a two-player board collapses "an opponent" onto the only other
-- player, so a reading that admitted the entry whenever ANY player was dealt
-- damage could not be told from one that admitted it only for an opponent's.
bloodthirstBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
bloodthirstBoard swamp vampire sentry =
  let lands = S.landsFor swamp S.alice 3 S.threePlayerGame
      (bobsSentry, withSentry) = S.addPermanent sentry S.bob lands
      (held, gs) = S.addHandCard vampire S.alice withSentry
   in (readyForAlice gs, held, bobsSentry)

-- alice controls seven untapped Forests on a THREE-SEAT board and holds one
-- Petrified Wood-Kin ({6}{G} 3/3, "This spell can't be countered. / Bloodthirst X
-- / Protection from instants"), in her precombat main phase with priority; bob
-- controls one Ogre Sentry, the source every damage event in the group names.
-- Returns the state, the card in hand and the Sentry.
--
-- Three seats for bloodthirstBoard's reason and for one more: rule 702.54b's
-- "your opponents" is a SUM ACROSS PLAYERS, which two seats cannot tell from one
-- player's own total.
bloodthirstXBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
bloodthirstXBoard forest woodKin sentry =
  let lands = S.landsFor forest S.alice 7 S.threePlayerGame
      (bobsSentry, withSentry) = S.addPermanent sentry S.bob lands
      (held, gs) = S.addHandCard woodKin S.alice withSentry
   in (readyForAlice gs, held, bobsSentry)

-- alice, in her precombat main phase, with priority. Applied by the fixture above
-- and again after a turn handoff, which leaves the game in CR 502's untap step.
readyForAlice :: GameState.GameState -> GameState.GameState
readyForAlice gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

-- CR 702.54: bloodthirst, on Bloodrage Vampire ({2}{B} 3/1 Vampire, "Bloodthirst
-- 1" and nothing else) for rule 702.54a's N, and on Petrified Wood-Kin ({6}{G}
-- 3/3, "Bloodthirst X") for rule 702.54b's X. The second minted entry replacement
-- whose own rule states a condition, after rule 702.145b's daybound, which is why
-- Pawl.Engine.Replacement.admitsEntry now has two arms that are not `True` --
-- and rule 702.54b's X is not one of them, since that variant states none.
--
-- ONE BOARD PER FORM, and every case differs from its siblings in nothing but
-- what happened before the cast: the same three seats, the same lands, the same
-- card cast the same way. The one exception is the CR 616.1e pair below, which
-- differs in one permanent -- bob's Kismet -- and says so. The permanent ENTERS
-- in every case, so what the assertions tell apart is "entered with counters"
-- from "entered", never "entered" from "did not".
--
-- Distinct numbers everywhere, so no two readings coincide: bloodthirst 1 on a
-- printed 3/1 shows as a 4/2, and the damage amounts are 4 at bob, 5 at alice, 2
-- at bob's Sentry (which a 3/3 survives) and 6 of life paid. The X cases pick
-- theirs the same way, and each case says which readings its numbers separate.
--
-- The damage goes in through Damage.applyDamage, the funnel that records the
-- event, exactly as Pawl.TriggerSpec's Furious Spinesplitter group does -- and
-- everything downstream of the record is the card's own, driven through a real
-- cast and a real resolution.
bloodthirstSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bloodthirstSpec s registry =
  let hit src target amount gs =
        S.runPure
          S.identityAnswer
          gs
          (Damage.applyDamage [DamageEvent.MkDamageEvent src target amount False False False 0 Nothing DamageKind.Noncombat])
      enters = castAndResolve S.aggressiveAnswer
      vampireIn = newestNamed (CardName.MkCardName $ Text.pack "Bloodrage Vampire")
      woodKinIn = newestNamed (CardName.MkCardName $ Text.pack "Petrified Wood-Kin")
   in Spec.describe s "Bloodthirst (CR 702.54)" $ do
        Spec.it s "CR 702.54a nobody was dealt damage, so it enters a 3/1" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs, held, _) = bloodthirstBoard swamp vampire sentry
              after = enters gs held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
              Spec.assertEqWith s "power" (Projection.powerOf vamp after) (Just 3)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf vamp after) (Just 1)
        -- THE PAIR THAT MAKES THE CONDITION REAL. The board above with one damage
        -- event added and nothing else changed.
        Spec.it s "CR 702.54a an opponent was dealt damage, so it enters a 4/2" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              after = enters (hit bobsSentry (Recipient.ToPlayer S.bob) 4 gs0) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne vamp after) 1
              -- Printed 3/1, so the counter shows in the projection (CR 613.4c,
              -- layer 7c).
              Spec.assertEqWith s "power" (Projection.powerOf vamp after) (Just 4)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf vamp after) (Just 2)
        -- CR 102.2 / 109.5: "an opponent" is a player other than the entering
        -- permanent's controller, so alice's own damage is not an opponent's.
        -- Unreachable on a two-seat board, which is why this group takes three.
        Spec.it s "CR 102.2 damage to the controller herself does not turn it on" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              after = enters (hit bobsSentry (Recipient.ToPlayer S.alice) 5 gs0) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 120.3a names the PLAYER recipient; a creature bob controls is not bob.
        Spec.it s "CR 120.3a damage to an opponent's creature does not either" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              damaged = hit bobsSentry (Recipient.ToCreature bobsSentry) 2 gs0
              after = enters damaged held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
              Spec.assertBool s (Set.member bobsSentry (GameState.battlefield after)) "and the 3/3 Sentry survived the 2, so the board is otherwise the same"
        -- CR 119.4's life loss is not CR 120.1's damage, and the log records the
        -- two separately. Without this the condition could be reading
        -- GameEvent.LifeLost and pass every case above, since damage to a player
        -- files one of those too.
        Spec.it s "CR 119.4 life lost without damage is not damage" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, _) = bloodthirstBoard swamp vampire sentry
              after = enters (S.runPure S.identityAnswer gs0 (Event.payLife S.bob 6)) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 608.2i: the window is THIS turn. Without this a lifetime tally passes
        -- every case above. The turn goes all the way round to alice again, so the
        -- only difference from the positive case is which turn it is.
        Spec.it s "CR 608.2i the damage is THIS turn's: the handoff clears it" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              damaged = hit bobsSentry (Recipient.ToPlayer S.bob) 4 gs0
              handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
              roundAgain = readyForAlice (handoff (handoff (handoff damaged)))
              after = enters roundAgain held
          Spec.assertEqWith s "the turn came back to alice" (GameState.activePlayer roundAgain) S.alice
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters a turn cycle later" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 702.54c: "if an object has multiple instances of bloodthirst, each
        -- applies separately." Asserted at the mint rather than at gameplay level,
        -- because nothing in the pool prints or grants a second instance -- the
        -- same footing Pawl.TriggerSpec's vanishing group states its rule 702.63c
        -- claim on.
        Spec.it s "CR 702.54c each instance is its own row" $
          Spec.assertEqWith
            s
            "bloodthirst 1 held twice mints two rows"
            (Keyword.Engine.mintedReplacementsFor (Keyword.Bloodthirst (Just 1)) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.Bloodthirst (Just 1)))))
        -- CR 702.54b: bloodthirst X on Petrified Wood-Kin. THE PAIR IS THIS CASE
        -- AND THE ONE BELOW, one board apart in nothing but what was dealt.
        --
        -- 3 at bob and 2 at carol, so every reading of the rule reaches a
        -- different number: rule 702.54b's sum is 5, rule 702.54a's tally of
        -- damaged opponents is 2, one seat's total is 3, and a lifetime count of
        -- damage events is 2. A printed 3/3 makes the projection 8/8, which no
        -- other reading produces either.
        Spec.it s "CR 702.54b X is the total damage its controller's opponents were dealt" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstXBoard forest woodKin sentry
              damaged = hit bobsSentry (Recipient.ToPlayer S.carol) 2 (hit bobsSentry (Recipient.ToPlayer S.bob) 3 gs0)
              after = enters damaged held
          case woodKinIn after of
            Nothing -> Spec.assertFailure s "Petrified Wood-Kin did not reach the battlefield"
            Just kin -> do
              Spec.assertEqWith s "five +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne kin after) 5
              -- Printed 3/3, so the counters show in the projection (CR 613.4c,
              -- layer 7c).
              Spec.assertEqWith s "power" (Projection.powerOf kin after) (Just 8)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf kin after) (Just 8)
        -- CR 702.54b states NO CONDITION, unlike rule 702.54a: with nothing dealt
        -- X is zero and the permanent still enters. This case reads the zero, not
        -- the absence of the condition -- the counters cannot tell the two
        -- readings apart on any board, which the Kismet pair below is for.
        Spec.it s "CR 702.54b nobody was dealt damage, so X is zero" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, _) = bloodthirstXBoard forest woodKin sentry
              after = enters gs0 held
          case woodKinIn after of
            Nothing -> Spec.assertFailure s "Petrified Wood-Kin did not reach the battlefield"
            Just kin -> do
              Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne kin after) 0
              Spec.assertEqWith s "power" (Projection.powerOf kin after) (Just 3)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf kin after) (Just 3)
        -- CR 616.1e: what SEPARATES rule 702.54b's unconditional row from rule
        -- 702.54a's conditional one, since the counters above cannot. bob's Kismet
        -- ({3}{W} Enchantment, "Artifacts, creatures, and lands your opponents
        -- control enter tapped", EntryRewrite.Tapped) is a second CR 616.1e
        -- candidate for alice's entering Wood-Kin, and NOTHING was dealt this turn
        -- -- so rule 702.54b admits its row and there are two rows to order, where
        -- rule 702.54a's condition would refuse it and leave one row with nothing
        -- to ask.
        --
        -- The two orders converge on one board -- CR 616.1f re-collects, so the
        -- Wood-Kin ends up tapped with zero counters whichever applies first -- so
        -- the prompt is the only observable, exactly as kismetSpec's cases are.
        -- THE PAIR IS THIS CASE AND THE ONE BELOW, one board apart in nothing but
        -- Kismet.
        --
        -- That WAS the choice was asked is all this proves: Response.ChoseReplacement
        -- records the chosen index alone, so the replay log names no asked player and
        -- CR 616.1's "the affected object's controller" is not observable here. The
        -- rule's chooser is kismetSpec's question, not this case's.
        Spec.it s "CR 616.1e with nothing dealt the bloodthirst X row still races an opponent's Kismet" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          kismet <- S.printingOf s registry "Kismet"
          let (gs0, held, _) = bloodthirstXBoard forest woodKin sentry
              raced = snd (S.addPermanent kismet S.bob gs0)
              cast = S.cast S.alice held >> Stack.resolveTop
              after = S.runPure S.aggressiveAnswer raced cast
          Spec.assertBool s (wasAskedToReplace (answersFor S.aggressiveAnswer raced cast)) "a ChooseReplacement was raised"
          -- Non-vacuity, not the proof: the tap shows Kismet really was the second
          -- candidate, so "a prompt" is not a question about one row, and the zero
          -- pins the board this case is about. Neither reads the bloodthirst row's
          -- application -- zero counters is what refusing it would leave too,
          -- which is the whole reason the prompt is the assertion.
          case woodKinIn after of
            Nothing -> Spec.assertFailure s "Petrified Wood-Kin did not reach the battlefield"
            Just kin -> do
              Spec.assertEqWith s "CR 702.54b X is zero" (countersOn CounterKind.PlusOnePlusOne kin after) 0
              Spec.assertBool s (Game.isTapped kin after) "CR 614.1d Kismet tapped it"
        -- The DISCRIMINATING TWIN: the same board without Kismet, where rule
        -- 702.54b's row is alone in its bucket and there is nothing to ask.
        -- Without it "a prompt was raised" above would pass under a recorder that
        -- reported one on every board.
        Spec.it s "CR 616.1 the bloodthirst X row alone asks nobody" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, _) = bloodthirstXBoard forest woodKin sentry
              cast = S.cast S.alice held >> Stack.resolveTop
          Spec.assertBool s (not (wasAskedToReplace (answersFor S.aggressiveAnswer gs0 cast))) "no ChooseReplacement was raised"
        -- CR 109.5: "your opponents" excludes the entering permanent's own
        -- controller. 7 at alice and 3 at bob, so a sum over every player would be
        -- 10 and a sum over the opponents is 3.
        Spec.it s "CR 109.5 damage to the controller herself is not in X" $ do
          forest <- S.printingOf s registry "Forest"
          woodKin <- S.printingOf s registry "Petrified Wood-Kin"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstXBoard forest woodKin sentry
              damaged = hit bobsSentry (Recipient.ToPlayer S.bob) 3 (hit bobsSentry (Recipient.ToPlayer S.alice) 7 gs0)
              after = enters damaged held
          case woodKinIn after of
            Nothing -> Spec.assertFailure s "Petrified Wood-Kin did not reach the battlefield"
            Just kin -> Spec.assertEqWith s "three +1/+1 counters, not ten" (countersOn CounterKind.PlusOnePlusOne kin after) 3

-- The one permanent a move added to the battlefield, or Nothing when it added
-- none or several. `newestNamed` above cannot answer this one: a face-down
-- permanent has no name at all (CR 708.2a).
arrivedOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
arrivedOne before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    [oid] -> Just oid
    _ -> Nothing

-- The board at the START of each of the next `n` turns, oldest first -- so the
-- board a turn LEFT is the next element, and a turn's own first step is still
-- ahead of the element that names it. Top-level rather than a `where` binding
-- because the answer is rank-2 and GHC will not infer it, the same reason
-- castEach above is.
turnStarts :: (forall r. Prompt.Prompt r -> r) -> Int -> GameState.GameState -> [GameState.GameState]
turnStarts answer n gs =
  if n <= 0
    then []
    else let next = nextTurn answer gs in next : turnStarts answer (n - 1) next

-- Answer CR 616.1's choice between two untap-step skips by the row's SOURCE:
-- True takes Brine Elemental's row, False the other one, which on that board is
-- Savor the Moment's. By source and not by index, because Replacement.collect
-- emits the floating store newest-first while installTurnSkips prepends -- so a
-- change in either order must not silently swap what an answer means.
--
-- Savor's row is named by exclusion rather than by its own id: CR 400.7 minted a
-- new object as the card moved to the stack, and the id the fixture holds is the
-- hand card's.
skipAnswer :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
skipAnswer wantBrine brine p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    let wanted = if wantBrine then (== brine) else (/= brine)
     in maybe 0 Int.toNaturalSaturating (List.findIndex (wanted . ReplacementEntry.source) entries)
  _ -> S.identityAnswer p

-- Three seats (CR 800.1), alice active in her precombat main phase:
--
--   * alice holds Savor the Moment and has three untapped Islands for its
--     {1}{U}{U}, plus one TAPPED Goblin Piker as her observable;
--   * bob holds Brine Elemental and has ten untapped Islands -- CR 702.37a's {3}
--     for the face-down cast, plus CR 702.37e's {5}{U}{U} to turn it back up;
--   * carol has a TAPPED Goblin Piker of her own and nothing else.
--
-- Three seats and not two: at two, "each opponent" and "each player" name the
-- same set, and carol is what shows the trigger reached bob's opponents rather
-- than everybody.
--
-- The Pikers are the observable, for the reason Pawl.TurnSpec's savorBoard has
-- one: CR 502.3 untaps the ACTIVE player's permanents, so a turn whose untap step
-- happened leaves its player's Piker untapped and a turn whose untap step was
-- skipped leaves it tapped -- and nothing else in these cases taps or untaps
-- either. Lands go in through S.addPermanent too, which puts any printing on the
-- battlefield untapped; only the mana matters here, never the card type.
--
-- Libraries are stocked because these cases run seven whole turns, and CR 104.3c
-- decks a player who draws from an empty one.
brineBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
brineBoard island savor brine piker =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addPermanent island pid acc)) g [1 .. (n :: Int)]
      withLands = addLands S.bob 10 (addLands S.alice 3 S.threePlayerGame)
      (alicePiker, g1) = S.addPermanent piker S.alice withLands
      (carolPiker, g2) = S.addPermanent piker S.carol g1
      (savorId, g3) = S.addHandCard savor S.alice g2
      (brineId, g4) = S.addHandCard brine S.bob g3
      stock g pid = List.foldl' (\g' _ -> snd (S.addLibraryCard piker pid g')) g [1 .. (15 :: Int)]
      stocked = List.foldl' stock g4 [S.alice, S.bob, S.carol]
   in ( (S.tapObject carolPiker (S.tapObject alicePiker stocked))
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.turnNumber = 1
          },
        savorId,
        brineId,
        alicePiker,
        carolPiker
      )

-- Arm both effects on brineBoard: alice casts Savor the Moment, then bob casts
-- Brine Elemental face down for CR 702.37a's {3} and turns it face up for CR
-- 702.37e's {5}{U}{U}. Nothing when the face-down cast did not land. Hands back
-- the board and the Brine Elemental permanent, whose id `skipAnswer` names its
-- row by.
--
-- The turn-face-up special action is taken through Pawl.Engine.FaceDown directly,
-- as Pawl.FaceDownSpec's cases do: CR 702.37e's "any time you have priority" is
-- satisfied by the engine offering the action to the priority holder alone, and
-- what this fixture needs is bob taking it during ALICE's turn. The card's
-- "when this creature is turned face up" ability triggers on the special action
-- and resolves in the priority loop like any other (CR 603.2).
brineArmed ::
  Printing.Printing ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  Maybe (GameState.GameState, ObjectId.ObjectId)
brineArmed brine gs savorId brineId =
  let withSavor = S.runPure S.identityAnswer gs (S.cast S.alice savorId >> Stack.resolveTop)
      down =
        S.runPure
          S.identityAnswer
          withSavor
          (Cast.castSpell S.manaPerformer S.bob brineId (S.printingName brine) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
   in do
        permanent <- arrivedOne withSavor down
        pure (S.runPure S.identityAnswer down (FaceDown.turnFaceUp S.manaPerformer S.bob TurnUpProcedure.Morph permanent >> Engine.priorityLoop), permanent)

-- The seven-turn timeline the two answering cases below share, plus carol's
-- negative control alongside it: alice's own turn (1), Savor's extra turn (2),
-- bob's (3), carol's (4), alice's again (5), bob's (6), carol's again (7).
--
-- `aliceAfter` is what the answer decided -- alice's Piker once her turn 5 has run
-- -- and `left` is how many floating rows the extra turn's cleanup left behind.
--
-- Top-level rather than a `where` binding because the answer is rank-2 and GHC
-- will not infer it, the same reason castEach above is.
assertBrineRun ::
  (Monad m) =>
  Spec.Spec m n ->
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  Maybe TapState.TapState ->
  Int ->
  m ()
assertBrineRun s answer gs alicePiker carolPiker aliceAfter left =
  case turnStarts answer 7 gs of
    [startExtra, startBob, startCarol, startAlice, startBobAgain, startCarolAgain, startLast] -> do
      -- The schedule the rest is read against, pinned first so a mis-ordered turn
      -- cannot be mistaken for a mis-applied skip.
      Spec.assertEqWith
        s
        "the turns run alice (extra), bob, carol, alice, bob, carol, alice"
        (fmap GameState.activePlayer [startExtra, startBob, startCarol, startAlice, startBobAgain, startCarolAgain, startLast])
        [S.alice, S.bob, S.carol, S.alice, S.bob, S.carol, S.alice]
      -- CR 500.11: the extra turn's untap step is skipped either way -- one of the
      -- two rows takes it, and WHICH one is exactly what is not observable yet.
      Spec.assertEqWith s "the extra turn untapped nothing of alice's" (tapStateOf alicePiker startBob) (Just TapState.Tapped)
      -- carol's negative control: ONE row applies to her untap step, so CR 616.1
      -- has nothing to choose and the prompt is correctly elided -- and that one
      -- row takes exactly one untap step of hers.
      Spec.assertBool
        s
        (not (wasAskedToReplace (answersFor answer startCarol Engine.runStep)))
        "carol's lone skip is not a choice, so she is asked nothing"
      Spec.assertEqWith s "carol's own untap step was skipped" (tapStateOf carolPiker startAlice) (Just TapState.Tapped)
      Spec.assertEqWith s "and her NEXT one happened" (tapStateOf carolPiker startLast) (Just TapState.Untapped)
      -- THE OBSERVABLE, and read off the BOARD rather than off a row index:
      -- `collect` and installTurnSkips disagree about which end of the store they
      -- work from, so an ordering change must be unable to flip this. alice's turn
      -- 5 has run by startBobAgain.
      Spec.assertEqWith s "alice's following untap step, as the answer decided it" (tapStateOf alicePiker startBobAgain) aliceAfter
      -- The store behind it, asserted last so the board is what fails first: the
      -- extra turn's cleanup swept Savor's Expiry.AtCleanup row if it was still
      -- there, and kept every Expiry.Never one.
      Spec.assertEqWith s "and the extra turn's cleanup left this many rows" (length (GameState.replacements startBob)) left
    _ -> Spec.assertFailure s "the seven turns did not run"

-- Brine Elemental {4}{U}{U} Creature -- Elemental 5/4: "Morph {5}{U}{U} / When
-- this creature is turned face up, each opponent skips their next untap step",
-- alongside Savor the Moment ({1}{U}{U} Sorcery: "Take an extra turn after this
-- one. Skip the untap step of that turn.").
--
-- CR 614.10a states the outcome outright: "if two effects each cause a player to
-- skip their next occurrence, that player must skip the next two; one effect will
-- be satisfied in skipping the first occurrence, while the other will remain until
-- another occurrence can be skipped."
--
-- What makes this pair the discriminating one is that the two rows are equal in
-- their `effect` -- each a PhaseR naming alice's untap step -- and unequal in
-- LIFETIME. Brine's is Expiry.Never, so it waits however many turns it must;
-- Savor's is Expiry.AtCleanup, because "the untap step of THAT turn" dies with the
-- turn it named. Which of the two CR 616.1 spends is therefore observable a turn
-- of alice's later, and the choice is hers to make -- CR 616.1's "affected
-- player", which for a step beginning is whose step it is.
brineElementalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
brineElementalSpec s registry = Spec.describe s "BrineElemental" $ do
  let boardOf = do
        island <- S.printingOf s registry "Island"
        savor <- S.printingOf s registry "Savor the Moment"
        brine <- S.printingOf s registry "Brine Elemental"
        piker <- S.printingOf s registry "Goblin Piker"
        let (gs, savorId, brineId, alicePiker, carolPiker) = brineBoard island savor brine piker
        pure (brineArmed brine gs savorId brineId, alicePiker, carolPiker)
  -- The setup control: the two spells armed three rows, and not four. bob is the
  -- ability's controller, so CR 806.1's free-for-all opponents are alice and carol
  -- while bob himself gets nothing; Savor's own row is added as its extra turn
  -- BEGINS (CR 500.11), which is why it is missing until the first handoff ran.
  Spec.it s "CR 614.1b Brine Elemental arms one skip per opponent, and Savor's turn one more" $ do
    (armed, _, _) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, _) -> do
        Spec.assertEqWith s "the trigger armed one row per opponent, so two" (length (GameState.replacements gs)) 2
        case turnStarts S.identityAnswer 1 gs of
          [atExtra] -> do
            Spec.assertEqWith
              s
              "and the extra turn is alice's second"
              (GameState.turnNumber atExtra, GameState.activePlayer atExtra)
              (2, S.alice)
            Spec.assertEqWith s "whose beginning added Savor's own, for three" (length (GameState.replacements atExtra)) 3
          _ -> Spec.assertFailure s "the extra turn did not begin"
  -- THE ELISION GOING AWAY. Two rows apply to one untap step and they are not
  -- interchangeable, so CR 616.1's choice is a real one and the affected player
  -- has to be asked for it.
  --
  -- Fails against a `distinguishing` that compares `effect` alone: the two rows
  -- are equal in `effect`, so the prompt is elided and the engine spends whichever
  -- row it happened to collect first.
  Spec.it s "CR 616.1 two skips alike in effect but not in lifetime raise a choice" $ do
    (armed, _, _) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, brine) ->
        case turnStarts (skipAnswer True brine) 1 gs of
          [atExtra] ->
            Spec.assertBool
              s
              (wasAskedToReplace (answersFor (skipAnswer True brine) atExtra Engine.runStep))
              "a ChooseReplacement was raised for the extra turn's untap step"
          _ -> Spec.assertFailure s "the extra turn did not begin"
  -- Answering SAVOR's row: it is the one consumed, and Brine's Expiry.Never row
  -- remains to take alice's NEXT untap step -- CR 614.10a's "the other will remain
  -- until another occurrence can be skipped". So her Piker is still tapped after
  -- the following turn of hers. Two rows survive the extra turn's cleanup, both of
  -- them Brine's.
  Spec.it s "CR 614.10a spending Savor's row leaves Brine's to take the following untap step" $ do
    (armed, alicePiker, carolPiker) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, brine) ->
        assertBrineRun s (skipAnswer False brine) gs alicePiker carolPiker (Just TapState.Tapped) 2
  -- Answering BRINE's row: it is consumed, and Savor's row survives the untap step
  -- only to be swept at that same turn's cleanup, since "the untap step of THAT
  -- turn" is scoped to the turn it named (Expiry.AtCleanup; CR 514.2). Nothing is
  -- left to take alice's following untap step, so it happens and her Piker untaps.
  -- One row survives the cleanup, carol's.
  --
  -- This is the half that reads LIFETIME rather than merely which row was picked:
  -- were Savor's row armed Expiry.Never like Brine's, it would survive the cleanup,
  -- take the following untap step too, and leave this case reading Tapped.
  Spec.it s "CR 614.10a spending Brine's row lets Savor's expire with its own turn" $ do
    (armed, alicePiker, carolPiker) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, brine) ->
        assertBrineRun s (skipAnswer True brine) gs alicePiker carolPiker (Just TapState.Untapped) 1

-- alice casts a Coldsteel Heart off two Mountains and resolves it, answering CR
-- 616.1's race with `pick` and CR 614.1c's colour with blue. Returns every
-- ChooseReplacement payload raised, the finished board, and the permanent that
-- entered.
--
-- BLUE and not the default: Replay.defaultAnswer falls back to white, so a
-- colour assertion against white would pass on a game that asked nothing.
--
-- The payloads are recorded through State the way choosersAsked records player
-- ids -- mid-game, because the discriminating observable here is what reached
-- the wire, not what the board settled on (see the group below).
castColdsteel ::
  Printing.Printing ->
  Printing.Printing ->
  ([ReplacementEntry.ReplacementEntry] -> Natural.Natural) ->
  ([[ReplacementEntry.ReplacementEntry]], GameState.GameState, Maybe ObjectId.ObjectId)
castColdsteel mountain coldsteel pick =
  let (withCard, oid) = S.handOne coldsteel (S.landsInPlay mountain 2)
      step :: Prompt.Prompt r -> State.State [[ReplacementEntry.ReplacementEntry]] r
      step p = case p of
        Prompt.ChooseReplacement _ _ entries -> do
          State.modify' (<> [entries])
          pure (pick entries)
        Prompt.ChooseColor {} -> pure Color.Blue
        _ -> pure (S.identityAnswer p)
      ((_, after), payloads) = State.runState (Engine.runGame step withCard (S.cast S.alice oid >> Stack.resolveTop)) []
      entered = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield withCard)) of
        o : _ -> Just o
        [] -> Nothing
   in (payloads, after, entered)

-- Take the candidate carrying `rewrite`. Total, falling back on the canonical
-- first the way the engine's own out-of-range handling does.
pickRewrite :: EntryRewrite.EntryRewrite (Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)) -> [ReplacementEntry.ReplacementEntry] -> Natural.Natural
pickRewrite rewrite entries =
  let wanted e = ReplacementEntry.effect e == ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource rewrite)
   in maybe 0 Int.toNaturalSaturating (List.findIndex wanted entries)

-- CR 616.1 with CR 614.1c and CR 614.1d. Coldsteel Heart ({2} Snow Artifact,
-- "This artifact enters tapped." / "As this artifact enters, choose a color." /
-- "{T}: Add one mana of the chosen color.") is one source with TWO applicable
-- replacement effects for one entry event -- both ReplacementBucket.Other, both
-- ReplacementOrigin.Other -- so both reach `choose` in a single iteration and
-- the payload must say which is which (#74).
coldsteelHeartSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
coldsteelHeartSpec s registry = Spec.describe s "Coldsteel Heart (CR 616.1)" $ do
  -- THE PROVING CASE. Asserted on the PROMPT PAYLOAD and not on the board, and
  -- that is not a shortcut: CR 616.1f re-collects and CR 614.5 gives each effect
  -- one opportunity, so the artifact ends up tapped with the chosen colour
  -- whichever candidate is picked. A board-level assertion here would be
  -- over-determined and would pass under the [ObjectId] payload this replaces.
  --
  -- The payload LIST is asserted to have exactly one element before anything is
  -- said about its contents: a card file that lost one of the two replacements
  -- would leave one candidate, elide the prompt, and make a "every payload had
  -- two distinct entries" assertion pass over zero payloads.
  Spec.it s "CR 616.1 two entry replacements of ONE source are distinct entries" $ do
    mountain <- S.printingOf s registry "Mountain"
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    case castColdsteel mountain coldsteel (const 0) of
      ([entries], _, _) -> do
        Spec.assertEqWith s "CR 616.1e offered both candidates" (length entries) 2
        Spec.assertEqWith s "and the player can tell them apart" (length (List.nub entries)) 2
        Spec.assertEqWith s "though both come from the same permanent" (length (List.nub (fmap ReplacementEntry.source entries))) 1
      (payloads, _, _) -> Spec.assertFailure s ("expected exactly one ChooseReplacement, got " <> show (length payloads))
  -- The card-data control: independent of any payload assertion, so a JSON typo
  -- cannot hide behind a green one. Both rewrites ran.
  Spec.it s "CR 614.1c/614.1d both replacements applied, whichever was chosen" $ do
    mountain <- S.printingOf s registry "Mountain"
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    let assertBoth label pick = case castColdsteel mountain coldsteel pick of
          (_, after, Just oid) -> case Game.lookupObject oid after of
            Nothing -> Spec.assertFailure s (label <> ": the artifact left the battlefield")
            Just obj -> do
              Spec.assertEqWith s (label <> ": CR 614.1d it entered tapped") (Object.tapped obj) TapState.Tapped
              Spec.assertEqWith s (label <> ": CR 614.1c it chose blue") (Object.chosenColor obj) (Just Color.Blue)
          _ -> Spec.assertFailure s (label <> ": the artifact did not reach the battlefield")
    -- OVER-DETERMINED BY DESIGN, and named as such: this passes under the broken
    -- payload too. Its job is to catch a mis-indexing regression in the answerers
    -- migrated to ReplacementEntry, not to prove anything about #74.
    assertBoth "tapped first" (pickRewrite EntryRewrite.Tapped)
    assertBoth "colour first" (pickRewrite EntryRewrite.ChooseColor)

-- CR 614.1c with CR 120.3a. Stuffy Doll, {5} Artifact Creature -- Construct 0/1,
-- whole text: "Indestructible / As this creature enters, choose a player. /
-- Whenever this creature is dealt damage, it deals that much damage to the chosen
-- player. / {T}: This creature deals 1 damage to itself." (oracle checked on
-- Scryfall)
--
-- The first card whose as-enters choice is a PLAYER, and the first whose payload
-- reads one back. Both halves are proved by ONE observable -- whose life total
-- moved -- so a stamp with no reader, and a reader with no stamp, both fail.
--
-- THREE SEATS, and the pair of boards differs in exactly one thing: WHOM the
-- ChoosePlayer answer names. Two seats would collapse "the chosen player" onto
-- the only opponent, and the assertion would pass under a hard-coded "an
-- opponent" as happily as under the field.
--
-- The amount is 3, which is neither the Doll's power (0) nor its toughness (1)
-- nor the {T} ability's 1, so a payload reading a characteristic instead of CR
-- 603.2's captured binding fails rather than passing by luck.
stuffyDollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stuffyDollSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settleAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- A noncombat event's own path, enrageSpec's: applyDamage records the
      -- DamageDealt entries, the settle puts what they triggered on the stack
      -- (CR 603.3), and the priority loop resolves it.
      dealing events gs = resolveAll (settleAll (S.runPure S.identityAnswer gs (Damage.applyDamage events)))
      noncombat src target amount = DamageEvent.MkDamageEvent src (Recipient.ToCreature target) amount False False False 0 Nothing DamageKind.Noncombat
      lives g = (S.lifeOf S.alice g, S.lifeOf S.bob g, S.lifeOf S.carol g)
      chosenOn oid g = Game.lookupObject oid g >>= Object.chosenPlayer
      -- alice casts the Doll off five Mountains on a three-seat board and answers
      -- CR 614.12a's choice with `who`. The Doll must be CAST: S.addPermanent puts
      -- an object straight onto the battlefield without running the entry loop,
      -- so it would choose nobody.
      --
      -- The answer is pinned to a PlayerId by identity rather than by an index
      -- into the offer: an answerer that searched the candidate list for a legal
      -- seat would find a different one after a mutation and repair the assertion.
      castDoll :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (GameState.GameState, Maybe ObjectId.ObjectId, [Response.Response])
      castDoll doll mountain who =
        let (withCard, oid) = S.handOne doll (S.landsFor mountain S.alice 5 S.threePlayerGame)
            step :: Prompt.Prompt r -> r
            step p = case p of
              Prompt.ChoosePlayer {} -> who
              _ -> S.identityAnswer p
            ((_, after), answers) = Replay.record step withCard (S.cast S.alice oid >> Stack.resolveTop)
            entered = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield withCard)) of
              o : _ -> Just o
              [] -> Nothing
         in (after, entered, answers)
   in Spec.describe s "Stuffy Doll (CR 614.1c)" $ do
        -- THE PROVING CASE, and a pair of boards differing only in the answer.
        Spec.it s "CR 614.1c the chosen player, and only they, take the damage" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let hit who = case castDoll doll mountain who of
                (gs, Just dollId, _) ->
                  let (pikerId, board) = S.addPermanent piker S.bob gs
                   in Just (board, dollId, dealing [noncombat pikerId dollId 3] board)
                _ -> Nothing
          case (hit S.bob, hit S.carol) of
            (Just (before, dollId, chosenBob), Just (_, _, chosenCarol)) -> do
              Spec.assertEqWith s "all three seats start at 20" (lives before) (Just 20, Just 20, Just 20)
              Spec.assertEqWith s "CR 120.3a bob was chosen, so bob loses 3" (lives chosenBob) (Just 20, Just 17, Just 20)
              -- The same board and the same amount, one different answer.
              Spec.assertEqWith s "carol chosen instead, so carol loses 3" (lives chosenCarol) (Just 20, Just 20, Just 17)
              -- CR 120.3e: the damage really landed on the Doll, so the two
              -- assertions above are this trigger and not bookkeeping.
              Spec.assertEqWith s "CR 120.3e and the 3 is marked on the Doll" (fmap Object.damage (Game.lookupObject dollId chosenBob)) (Just 3)
              -- CR 702.12b: 3 over a toughness of 1 is lethal, and indestructible
              -- keeps it on the battlefield anyway.
              Spec.assertBool s (Set.member dollId (GameState.battlefield chosenBob)) "CR 702.12b indestructible: still on the battlefield"
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- The STAMP, asserted independently of the payload, so a JSON typo in the
        -- trigger cannot hide behind a green read-back -- and the other way round.
        Spec.it s "CR 614.12a the choice is made before the permanent enters" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.carol of
            (gs, Just dollId, answers) -> do
              Spec.assertEqWith s "CR 614.1c the Doll remembers carol" (chosenOn dollId gs) (Just S.carol)
              -- The SECOND INVARIANT, asserted directly: the engine did not pick a
              -- seat, it asked. Three seats make CR 102.1's offer three wide, so
              -- the prompt is a real question rather than an elided one.
              Spec.assertBool s (List.elem (Response.ChosePlayer S.carol) answers) "CR 614.12a the engine raised a ChoosePlayer prompt"
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- CR 400.7: a NEW object forgets the choice. Object.newIncarnation is a
        -- record UPDATE, so omitting the field there compiles and silently carries
        -- a chosen player across a zone change; -Werror cannot name that site, and
        -- this is what stands in its place. Mirrors Pawl.GameSpec's Painter's
        -- Servant chosenColor case.
        Spec.it s "CR 400.7 a new incarnation has chosen nobody" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.bob of
            (gs, Just dollId, _) -> do
              -- The discriminator: without it an assertion over an object that
              -- never chose anybody would pass whatever newIncarnation does.
              Spec.assertEqWith s "the object going in genuinely carried a chosen player" (chosenOn dollId gs) (Just S.bob)
              Spec.assertEqWith
                s
                "CR 400.7 the rebuilt object forgot the player it chose"
                (fmap (Object.chosenPlayer . Object.newIncarnation) (Game.lookupObject dollId gs))
                (Just Nothing)
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- The fourth clause, and the one that makes the card self-contained: {T}
        -- deals 1 to itself, which re-enters the trigger with a DIFFERENT number.
        -- 1 against the case above's 3 separates "reads the event" from "reads a
        -- constant".
        Spec.it s "CR 120.1 the Doll's own {T} ability feeds its own trigger" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.bob of
            (gs, Just dollId, _) -> do
              -- CR 302.6: the Doll was cast this turn, so its {T} ability is
              -- unactivatable until its controller's next turn begins. Settled by
              -- hand rather than by driving a turn cycle, which would add a draw
              -- step and CR 104.3c to a case about one printed clause.
              let unsick g = g {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Settled S.alice}) dollId (GameState.objects g)}
                  activated = S.runPure S.identityAnswer (unsick gs) (Activate.activateAbility S.alice dollId (theAbility doll))
                  after = resolveAll (settleAll activated)
              Spec.assertEqWith s "CR 120.3a bob loses exactly 1, not 3" (lives after) (Just 20, Just 19, Just 20)
              Spec.assertEqWith s "CR 120.3e and 1 is marked on the Doll" (fmap Object.damage (Game.lookupObject dollId after)) (Just 1)
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"

-- CR 122.1 / 122.6 with CR 614.1: Vorinclex, Monstrous Raider ({4}{G}{G}
-- Legendary Creature -- Phyrexian Praetor 6/6, "Trample, haste. If you would put
-- one or more counters on a permanent or player, put twice that many . . .
-- instead. If an opponent would put one or more counters on a permanent or
-- player, they put half that many . . . instead, rounded down.").
--
-- The card the player-counter funnel needed: its own ruling says it "cares deeply
-- about who is putting the counters", so a board that reads the RECIPIENT instead
-- answers differently on every case below but the first.
--
-- Every count is chosen so the three readings differ: three energy is six
-- doubled, one halved and three unreplaced, and one poison counter is two, zero
-- and one.
vorinclexSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vorinclexSpec s registry = Spec.describe s "Vorinclex, Monstrous Raider (CR 122.1)" $ do
  let -- THREE seats: at two, "an opponent" and "the other player" are the same
      -- seat, and carol is what tells CR 122.6a's putter apart from a reading
      -- that asks who is affected.
      --
      -- `withVorinclex` is the only difference between a case and its control --
      -- same printing, same seat, same trigger.
      board withVorinclex printing pid = do
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let seated = if withVorinclex then snd (S.addPermanent vorinclex S.alice S.threePlayerGame) else S.threePlayerGame
            (oid, entered) = S.entersWithTrigger printing pid seated
        pure (oid, S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority))
      energyIn = S.playerCounterOf PlayerCounterKind.Energy S.alice
      poisonIn g = (S.playerCounterOf PlayerCounterKind.Poison S.alice g, S.playerCounterOf PlayerCounterKind.Poison S.bob g, S.playerCounterOf PlayerCounterKind.Poison S.carol g)
  -- CR 122.6: the gain reaches the CR 616.1 loop at all. Without the funnel the
  -- energy lands unreplaced at three, which is the control below.
  Spec.it s "CR 614.1 alice's own three energy are doubled to six" $ do
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    (_, doubled) <- board True sage S.alice
    (_, plain) <- board False sage S.alice
    Spec.assertEqWith s "twice that many" (energyIn doubled) 6
    Spec.assertEqWith s "and three without the praetor" (energyIn plain) 3
  -- CR 107.1a's rounding. bob is putting these on HIMSELF, so this case does not
  -- separate the putter from the recipient -- both are bob, and either reading
  -- halves. What it does prove is the halving clause and its rounding: three is
  -- one, not two and not three. The putter axis is the Ichor Rats case below.
  Spec.it s "CR 107.1a bob's three energy are halved to one, rounded down" $ do
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    (_, halved) <- board True sage S.bob
    (_, plain) <- board False sage S.bob
    Spec.assertEqWith s "half of three, rounded down" (S.playerCounterOf PlayerCounterKind.Energy S.bob halved) 1
    Spec.assertEqWith s "and three without the praetor" (S.playerCounterOf PlayerCounterKind.Energy S.bob plain) 3
    Spec.assertEqWith s "alice, who put none on herself, has none" (energyIn halved) 0
  -- CR 122.6a: alice is the one PUTTING all three counters, so all three are
  -- doubled -- including the ones bob and carol receive, who are her opponents.
  -- This is the case a recipient-based reading gets wrong in both directions at
  -- once: bob's and carol's would be halved to zero instead.
  Spec.it s "CR 122.6a alice's Ichor Rats poisons the whole table twice over" $ do
    ichorRats <- S.printingOf s registry "Ichor Rats"
    (rats, doubled) <- board True ichorRats S.alice
    (_, plain) <- board False ichorRats S.alice
    Spec.assertBool s (S.onBattlefield rats doubled) "the Rats are on the battlefield"
    Spec.assertEqWith s "twice that many, for opponents too" (poisonIn doubled) (2, 2, 2)
    Spec.assertEqWith s "one each without the praetor" (poisonIn plain) (1, 1, 1)
  -- CR 614.1a's "instead" taken to zero: half of one, rounded down, is a
  -- replacement that removes the event rather than resizing it. The Rats still
  -- entered and their trigger still resolved, which is what separates this from a
  -- trigger that never ran.
  Spec.it s "CR 107.1a bob's Ichor Rats poisons nobody: half of one is zero" $ do
    ichorRats <- S.printingOf s registry "Ichor Rats"
    (rats, erased) <- board True ichorRats S.bob
    (_, plain) <- board False ichorRats S.bob
    Spec.assertBool s (S.onBattlefield rats erased) "the Rats are on the battlefield"
    Spec.assertEqWith s "nobody is poisoned" (poisonIn erased) (0, 0, 0)
    Spec.assertEqWith s "one each without the praetor" (poisonIn plain) (1, 1, 1)
  -- CR 122.6's OBJECT half, on the same axis: alice casts Battlegrowth at bob's
  -- creature, so the counters go on a permanent she does not control and are
  -- doubled anyway. Doubling Season's clause -- which reads whose permanent it is
  -- -- would leave this one alone.
  Spec.it s "CR 122.6 alice doubles a counter she puts on bob's creature" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, theirs) = counterBoard forest battlegrowth [vorinclex] [pikerPrinting]
        (bare, bareSpell, _, bareTheirs) = counterBoard forest battlegrowth [] [pikerPrinting]
    case (mine, theirs, bareTheirs) of
      (praetor : _, piker : _, barePiker : _) -> do
        Spec.assertEqWith s "1 * 2" (countersOn CounterKind.PlusOnePlusOne piker (castAndResolve (raceAnswer praetor piker) gs spellId)) 2
        Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne barePiker (castAndResolve (raceAnswer barePiker barePiker) bare bareSpell)) 1
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The object half's other direction, and the pair the subject check on that arm
  -- turns on: BOB casts the Battlegrowth, at his own creature, with alice's
  -- praetor watching. Half of one counter is none. The two boards differ in the
  -- praetor alone.
  Spec.it s "CR 122.6 bob's counter on bob's own creature is halved away" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let bobsBoard withVorinclex =
          let (_, g1) = S.addPermanent forest S.bob (S.landsInPlay forest 1)
              (piker, g2) = S.addPermanent pikerPrinting S.bob g1
              g3 = if withVorinclex then snd (S.addPermanent vorinclex S.alice g2) else g2
              (spell, g4) = S.addHandCard battlegrowth S.bob g3
           in (piker, S.runPure (raceAnswer piker piker) g4 (S.cast S.bob spell >> Stack.resolveTop))
        (halvedPiker, halved) = bobsBoard True
        (plainPiker, plain) = bobsBoard False
    Spec.assertEqWith s "half of one, rounded down" (countersOn CounterKind.PlusOnePlusOne halvedPiker halved) 0
    Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne plainPiker plain) 1
  -- The negative control for CounterPattern.onWho: Doubling Season says "on a
  -- permanent you control", which no player is, so alice's own energy is
  -- untouched by it. Same seat and same trigger as the doubling case above.
  Spec.it s "CR 614.16 Doubling Season does not reach a player's counters" $ do
    doublingSeason <- S.printingOf s registry "Doubling Season"
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    let (_, seated) = S.addPermanent doublingSeason S.alice S.threePlayerGame
        (_, entered) = S.entersWithTrigger sage S.alice seated
        after = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith s "three, not six" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 3
  -- CR 122.6a's default putter, which is the one thing the cases above cannot
  -- see: an entering permanent's counters are put on by ITS controller, so bob's
  -- riot counter is halved by alice's praetor. A putter read off the active
  -- player, or off the applying row's own controller, is alice here and would
  -- DOUBLE the counter instead.
  --
  -- The goblin ends with neither the counter nor haste, which is what taking
  -- CR 702.136a's first half and having it halved away means.
  Spec.it s "CR 702.136a bob's riot counter is halved away before it lands" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let goblinBoard withVorinclex =
          let (_, g1) = S.addPermanent mountain S.bob (S.landsInPlay forest 1)
              (_, g2) = S.addPermanent forest S.bob g1
              g3 = if withVorinclex then snd (S.addPermanent vorinclex S.alice g2) else g2
              (held, g4) = S.addHandCard zhurTaa S.bob g3
              after = S.runPure (riotChoosing OptionalDecision.Exercises) g4 (S.cast S.bob held >> Stack.resolveTop)
           in (newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after, after)
    case (goblinBoard True, goblinBoard False) of
      ((Just halvedGoblin, halved), (Just plainGoblin, plain)) -> do
        Spec.assertEqWith s "half of one, rounded down" (countersOn CounterKind.PlusOnePlusOne halvedGoblin halved) 0
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste halvedGoblin halved)) "and no haste: the counter was taken, not declined"
        Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne plainGoblin plain) 1
      _ -> Spec.assertFailure s "the goblin did not reach the battlefield"

-- CR 120.3b / 120.3d with CR 122.6: the counters a DAMAGE event causes are put on
-- through the same two placement funnels every other counter goes through, so a
-- counter replacement reaches them.
--
-- Ichor Rats ({1}{B}{B} Creature -- Phyrexian Rat 2/1, "Infect. When this
-- creature enters, each player gets a poison counter.") is the source, and its two
-- power is what keeps the three readings apart: two unreplaced, four doubled, one
-- halved. One power would make the halved reading zero, which no board can tell
-- from a placement that never happened.
--
-- The praetor's SEAT is the only difference between each pair, and it is the axis
-- CR 120.3b and CR 120.3d both name: "that source's controller" puts these
-- counters, so alice's praetor doubles them wherever they land, and bob's halves
-- the ones he himself receives. A reading that asked whose permanent or player
-- was AFFECTED would double bob's boards instead.
--
-- Three seats, so the praetor's controller can be a third party to the placement
-- rather than always one of its two ends.
damageCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageCountersSpec s registry = Spec.describe s "Counters damage causes (CR 120.3b, CR 120.3d)" $ do
  let -- alice attacks bob with one Ichor Rats; `watcher` seats that card under that
      -- player, Nothing being the control board. bob gets one creature per printing
      -- in `blockers` and blocks with all of them.
      board watcher blockers = do
        rats <- S.printingOf s registry "Ichor Rats"
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) watcher
        printings <- Monad.mapM (S.printingOf s registry) blockers
        let (gs0, mine, theirs, _) = S.threePlayerCombat [rats] printings []
            seated = case (seat, watcher) of
              (Just printing, Just (_, pid)) -> snd (S.addPermanent printing pid gs0)
              _ -> gs0
        pure $ case mine of
          [rat] -> Just (theirs, S.runCombat (fights rat theirs) seated)
          _ -> Nothing
      -- ONLY the Rats attack and only bob's `blockers` block, so a hasty praetor on
      -- either side neither attacks nor blocks and the boards differ in nothing but
      -- the replacement. With one blocker CR 510.1c forces the whole assignment, so
      -- no division is asked for.
      fights :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      fights rat blockers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== rat) ids
        Prompt.DeclareBlockers _ _ mine _ ->
          Map.fromList (fmap (\b -> (b, Set.singleton rat)) (filter (\b -> elem b blockers) mine))
        Prompt.ChooseDefender {} -> S.bob
        _ -> S.identityAnswer p
      poisonOn = S.playerCounterOf PlayerCounterKind.Poison
      shrunk = countersOn CounterKind.MinusOneMinusOne
  Spec.it s "CR 120.3b alice's praetor doubles the poison her Ichor Rats deals bob" $ do
    doubled <- board (Just ("Vorinclex, Monstrous Raider", S.alice)) []
    plain <- board Nothing []
    case (doubled, plain) of
      (Just (_, twice), Just (_, once)) -> do
        Spec.assertEqWith s "twice that many" (poisonOn S.bob twice) 4
        Spec.assertEqWith s "and two without the praetor" (poisonOn S.bob once) 2
        Spec.assertEqWith s "CR 120.3b's poison instead of the life loss" (S.lifeOf S.bob twice) (Just 20)
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The putter axis: bob's own praetor HALVES the poison bob receives, because
  -- alice is the one putting it. This is the case a recipient-based reading gets
  -- backwards -- bob is its controller's "you", so it would double instead.
  Spec.it s "CR 120.3b bob's praetor halves the poison he receives" $ do
    halved <- board (Just ("Vorinclex, Monstrous Raider", S.bob)) []
    plain <- board Nothing []
    case (halved, plain) of
      (Just (_, half), Just (_, once)) -> do
        Spec.assertEqWith s "half of two, rounded down" (poisonOn S.bob half) 1
        Spec.assertEqWith s "and two without the praetor" (poisonOn S.bob once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 120.3d's creature half, on the same axis. Wall of Stone ({1}{R}{R} Creature
  -- -- Wall 0/8, defender) is the blocker: it survives every reading, so the count
  -- is readable rather than gone with the creature, and its zero power assigns no
  -- damage back for CR 704.5h to act on.
  Spec.it s "CR 120.3d alice's praetor doubles the -1/-1 counters her Ichor Rats causes" $ do
    doubled <- board (Just ("Vorinclex, Monstrous Raider", S.alice)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (doubled, plain) of
      (Just ([wall], twice), Just ([bare], once)) -> do
        Spec.assertEqWith s "twice that many" (shrunk wall twice) 4
        Spec.assertEqWith s "and two without the praetor" (shrunk bare once) 2
        Spec.assertEqWith s "CR 120.3d's counters instead of marked damage" (S.damageOf wall twice) (Just 0)
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The creature half's putter axis, and the pair that settles it: the Wall is
  -- BOB's permanent, so a recipient-based reading would double these four times
  -- over. Alice is putting them, so bob's praetor halves them.
  Spec.it s "CR 120.3d bob's praetor halves the counters put on his own Wall of Stone" $ do
    halved <- board (Just ("Vorinclex, Monstrous Raider", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (halved, plain) of
      (Just ([wall], half), Just ([bare], once)) -> do
        Spec.assertEqWith s "half of two, rounded down" (shrunk wall half) 1
        Spec.assertEqWith s "and two without the praetor" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 614.16 versus CR 614.1, which is what CounterCause.ByRule settles here: rule
  -- 120.3's results are dictated by the rules, so Doubling Season -- "if an EFFECT
  -- would put one or more counters on a permanent you control" -- does not reach
  -- them, where the praetor's clauses name a player and do. Bob controls the Season
  -- and the Wall, so the only thing keeping it off is the cause.
  --
  -- Not vacuous: the cases above are the same board with the praetor in the same
  -- seat, and they move the count in both directions.
  Spec.it s "CR 614.16 Doubling Season does not reach the counters damage causes" $ do
    seasoned <- board (Just ("Doubling Season", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (seasoned, plain) of
      (Just ([wall], watched), Just ([bare], once)) -> do
        Spec.assertEqWith s "two, not four" (shrunk wall watched) 2
        Spec.assertEqWith s "the same two the bare board shows" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 614.1's third subject, and the case that needs it: Vizier of Remedies
  -- ({1}{W} Creature -- Human Cleric 2/1, "If one or more -1/-1 counters would be
  -- put on a creature you control, that many -1/-1 counters minus one are put on
  -- it instead.") names neither an effect nor a player, so it reaches a placement
  -- rule 120.3d dictates -- which the case above shows CR 614.16's subject does
  -- not.
  --
  -- Both other subjects would leave these alone, so the board separates all three:
  -- the cause is CounterCause.ByRule, which rule 614.16's subject refuses, and the
  -- putter is ALICE (the damage source's controller), where bob controls the
  -- vizier -- so a "if you would put" clause in bob's seat would miss them too.
  --
  -- The vizier neither attacks nor blocks, for the reason `fights` gives, so the
  -- pair of boards differs in the replacement alone.
  Spec.it s "CR 614.1 bob's Vizier of Remedies shrinks the counters damage causes" $ do
    shrinking <- board (Just ("Vizier of Remedies", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (shrinking, plain) of
      (Just ([wall], fewer), Just ([bare], once)) -> do
        Spec.assertEqWith s "that many minus one" (shrunk wall fewer) 1
        Spec.assertEqWith s "and two without the vizier" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"

-- CR 122.6 with CR 701.53a: the counters an EFFECT says a token enters the
-- battlefield with. "To incubate N, create an Incubator token that enters the
-- battlefield with N +1/+1 counters on it" (CR 701.53a) is one of the two
-- wordings in data/cards that write Pawl.Types.EntryRiders' `counters` onto an
-- Effect.Create -- Printlifter Ooze's is the other, in printlifterSpec below, and
-- undying and persist write the rider onto a MoveToZone -- and Eyes of Gitaxias
-- ({2}{U} Sorcery, "Incubate 3. Draw a card.") is the producer picked for it: its
-- other sentence asks for no choice, and its THREE is the count that tells the
-- readings apart.
--
-- Six doubled, one halved and three unreplaced are three different numbers, which
-- is what a board with one or two counters could not say.
--
-- The token is CR 111.10i's predefined Incubator token: a colorless Incubator
-- artifact with "{2}: Transform this token", whose back face is a 0/0 colorless
-- Phyrexian artifact creature named Phyrexian Token. The transform case is what
-- makes the count matter rather than merely be readable: a 0/0 wearing these
-- counters is the P/T the rest of the game sees.
--
-- Every assertion reads a permanent on the BATTLEFIELD: a token that left it
-- would cease to exist (CR 111.7) and take the assertion with it.
entryCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entryCountersSpec s registry = Spec.describe s "The counters a Create says its token enters with (CR 122.6)" $ do
  let incubatorName = CardName.MkCardName (Text.pack "Incubator Token")
      phyrexianName = CardName.MkCardName (Text.pack "Phyrexian Token")
      -- BOTH seats hold five untapped Islands, so `caster` moves who is paying and
      -- nothing else; `watcher` seats one printing under one player, and Nothing is
      -- the control board. Five rather than three, because the transform case below
      -- pays {2} after the {2}{U}.
      --
      -- The caster's library is stocked because the sorcery's second sentence draws
      -- (CR 104.3c).
      board caster watcher = do
        island <- S.printingOf s registry "Island"
        eyes <- S.printingOf s registry "Eyes of Gitaxias"
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) watcher
        let bothSeated = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) (S.landsInPlay island 5) [1 .. 5 :: Int]
            seated = case (seat, watcher) of
              (Just printing, Just (_, pid)) -> snd (S.addPermanent printing pid bothSeated)
              _ -> bothSeated
            (held, g1) = S.addHandCard eyes caster seated
            (_, g2) = S.addLibraryCard island caster g1
            after = S.runPure S.identityAnswer g2 (S.cast caster held >> Stack.resolveTop)
        pure (newestNamed incubatorName after, after)
      plusOnes = countersOn CounterKind.PlusOnePlusOne
      -- The token's own "{2}: Transform this token" (CR 111.10i), activated by the
      -- player who created it -- CR 111.2 makes that its controller -- and resolved.
      flipToken pid oid gs = case Game.faceOf oid gs >>= Maybe.listToMaybe . Face.activatedAbilities of
        Nothing -> gs
        Just ability -> S.runPure S.identityAnswer gs (Activate.activateAbility pid oid ability >> Stack.resolveTop)
  -- CR 614.16 reaches the placement at all, which is the whole of what the rider
  -- being routed through Event.putCounters buys: without it the token arrives with
  -- the three the effect asked for and no praetor can move them.
  Spec.it s "CR 122.6a alice's praetor doubles the three counters her Incubator token enters with" $ do
    (doubledToken, doubled) <- board S.alice (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.alice Nothing
    case (doubledToken, plainToken) of
      (Just twice, Just once) -> do
        Spec.assertEqWith s "twice that many" (plusOnes twice doubled) 6
        Spec.assertEqWith s "and three without the praetor" (plusOnes once plain) 3
      _ -> Spec.assertFailure s "the token did not reach the battlefield"
  -- CR 122.6a's default putter: "if the effect doesn't specify a player, the
  -- object's controller puts those counters on it", and CR 111.2 makes that bob.
  -- So ALICE's praetor halves them, which is the direction a putter read off the
  -- praetor's own controller gets backwards -- it would double these instead.
  Spec.it s "CR 107.1a alice's praetor halves the counters on bob's Incubator token" $ do
    (halvedToken, halved) <- board S.bob (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.bob Nothing
    case (halvedToken, plainToken) of
      (Just half, Just once) -> do
        Spec.assertEqWith s "half of three, rounded down" (plusOnes half halved) 1
        Spec.assertEqWith s "and three without the praetor" (plusOnes once plain) 3
      _ -> Spec.assertFailure s "the token did not reach the battlefield"
  -- What the counters are FOR: CR 111.10i's back face is a 0/0, so the count the
  -- funnel settled is the creature's power and toughness once the token turns over
  -- (CR 613.4c). Six and three, from the same pair of boards as the first case.
  Spec.it s "CR 701.53a the doubled counters are the transformed token's power and toughness" $ do
    (doubledToken, doubled) <- board S.alice (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.alice Nothing
    case (doubledToken, plainToken) of
      (Just twice, Just once) -> do
        let flipped = flipToken S.alice twice doubled
            bare = flipToken S.alice once plain
        Spec.assertEqWith s "the token turned over" (fmap Face.name (Game.faceOf twice flipped)) (Just phyrexianName)
        Spec.assertEqWith s "0/0 plus six counters" (S.powerToughnessOf twice flipped) (Just (6, 6))
        Spec.assertEqWith s "and 0/0 plus three without the praetor" (S.powerToughnessOf once bare) (Just (3, 3))
      _ -> Spec.assertFailure s "the token did not reach the battlefield"

-- CR 614.5's "only one opportunity", at the entry level. Perennation
-- ({3}{W}{B}{G} Sorcery, whole text: "Return target permanent card from your
-- graveyard to the battlefield with a hexproof counter and an indestructible
-- counter on it." -- oracle checked on Scryfall) gives one permanent TWO KINDS
-- of entry counter from ONE effect, which is the board the rule needs:
-- CR 122.6 makes both counters part of the permanent's single entry event even
-- though the rider comes from Perennation rather than the permanent's own text
-- (CR 614.12), so a scaling row whose pattern matches every kind gets ONE
-- opportunity covering both, not one per kind.
--
-- The arithmetic alone cannot say that -- scaling two kinds together and scaling
-- each separately give the same numbers -- so the board makes the NUMBER of
-- opportunities observable instead: two order-sensitive rows, alice's Doubling
-- Season and bob's Vorinclex (CR 616.1e orders them), answered so that the first
-- order taken and any second order taken would disagree. One opportunity means
-- one order asked and both kinds on the side it chose; an opportunity per kind
-- would ask twice and let the kinds disagree.
--
-- Odd counts throughout: Replacement.scale rounds Halve down, so 1 doubled then
-- halved is 1 where 1 halved then doubled is 0. An even count would read the
-- same under either order and prove nothing.
--
-- The returned card is read by NAME: CR 400.7 mints a new object as it leaves
-- the graveyard, so the id the fixture buried names nothing on the battlefield.
perennationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
perennationSpec s registry = Spec.describe s "Perennation (CR 614.5)" $ do
  let pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      hexproofs = countersOn (CounterKind.Keyword (Keyword.Hexproof Nothing))
      indestructibles = countersOn (CounterKind.Keyword Keyword.Indestructible)
      -- Six untapped lands in three colours pay the {3}{W}{B}{G}; the Piker in
      -- alice's graveyard is the permanent card the sorcery targets, and the only
      -- card there, so the identity answer cannot aim it anywhere else.
      --
      -- `scalers` is the only difference between the two boards: alice's Doubling
      -- Season ("counters would be put on a permanent YOU control") and bob's
      -- Vorinclex ("if an OPPONENT would put") both reach alice's placement, and
      -- CR 122.6a's default putter -- the entering permanent's controller -- is
      -- what makes the praetor's second clause the one that applies.
      board scalers = do
        plains <- S.printingOf s registry "Plains"
        swamp <- S.printingOf s registry "Swamp"
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        perennation <- S.printingOf s registry "Perennation"
        doublingSeason <- S.printingOf s registry "Doubling Season"
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let bare = S.landsFor forest S.alice 2 (S.landsFor swamp S.alice 2 (S.landsInPlay plains 2))
            (seasonId, withSeason) = S.addPermanent doublingSeason S.alice bare
            (_, withPraetor) = S.addPermanent vorinclex S.bob withSeason
            (_, buried) = S.addGraveyardCard piker S.alice (if scalers then withPraetor else bare)
            (held, ready) = S.addHandCard perennation S.alice buried
        pure (seasonId, held, ready)
      -- Cast and resolve, answering the entry's CR 616.1 orders with
      -- `ordersEntry` and counting them.
      returnIt seasonFirst (seasonId, held, ready) =
        let ((_, after), asked) = State.runState (Engine.runGame (ordersEntry seasonFirst seasonId) ready (S.cast S.alice held >> Stack.resolveTop)) 0
         in (asked, newestNamed pikerName after, after)
  -- The control, and the setup every case below rests on: one effect, two kinds,
  -- one counter each, and nothing on the board that could scale either.
  Spec.it s "CR 122.6 the returned permanent enters with one counter of each kind" $ do
    built <- board False
    case returnIt True built of
      (asked, Just oid, after) -> do
        Spec.assertEqWith s "a hexproof counter and an indestructible counter" (hexproofs oid after, indestructibles oid after) (1, 1)
        Spec.assertEqWith s "and with no row in the CR 616.1 pool there was nothing to order" asked 0
      _ -> Spec.assertFailure s "the card did not return to the battlefield"
  -- The rule itself. Both kinds move together under whichever row the ONE order
  -- put first, and the mixed pairs (1, 0) and (0, 1) -- which a per-kind
  -- opportunity would make reachable, since the second order taken answers the
  -- other way -- are not.
  Spec.it s "CR 614.5 one entry is one event, so a multiplier scales both kinds in its one application" $ do
    built <- board True
    case (returnIt True built, returnIt False built) of
      ((seasonAsked, Just seasoned, seasonBoard), (praetorAsked, Just halved, praetorBoard)) -> do
        Spec.assertEqWith s "Doubling Season first: one doubled is two, halved is one -- both kinds" (hexproofs seasoned seasonBoard, indestructibles seasoned seasonBoard) (1, 1)
        Spec.assertEqWith s "the praetor first: one halved is none, doubled is none -- both kinds" (hexproofs halved praetorBoard, indestructibles halved praetorBoard) (0, 0)
        Spec.assertEqWith s "and each board asked for exactly ONE order, not one per kind" (seasonAsked, praetorAsked) (1, 1)
      _ -> Spec.assertFailure s "the card did not return to the battlefield"

-- CR 614.5 again, on the permanent's OWN text rather than on a rider an effect
-- supplied. CR 614.1c's "as this permanent enters" clause can name SEVERAL KINDS
-- of counter in one sentence, and it is then one row, one candidate in CR 616.1's
-- pool, and one opportunity for a scaling replacement across every kind (#2314).
-- A kind per row would put an ordering in that pool the sentence does not have.
--
-- Agent's Toolkit {1}{G}{U} Artifact - Clue (New Capenna Commander; name, cost,
-- type line and oracle text checked against Scryfall 2026-08-25), whose entry
-- line is "this artifact enters with a +1/+1 counter, a flying counter, a
-- deathtouch counter, and a shield counter on it". Its other two lines are
-- Pawl.MoveCounterSpec's -- CR 122.5's move, and the Clue line its sacrifice
-- case spends.
--
-- Scryfall `o:/enters with .*counter.? and a .*counter/ -is:digital`,
-- 2026-08-25, answers three printings and no more: this one, Voidpouncer and
-- Dust Animus. The other two condition the clause, which is CR 604.2's clause on
-- Pawl.Types.PrintedReplacement rather than anything an EntryRewrite carries --
-- so Dust Animus is in the pool (dustAnimusSpec), and Voidpouncer is not, for
-- the reason unevenToolkitSpec gives.
--
-- THE BOARD IS PERENNATION'S, and for its reasons: alice's Doubling Season and
-- bob's Vorinclex are order-sensitive against each other (CR 616.1e), the counts
-- are odd so Replacement.scale's rounding makes the two orders disagree, and
-- `ordersEntry` answers the first order one way and every later order the other,
-- so a second opportunity would move the kinds apart. What differs is where the
-- counters come from: here the entering permanent's own text (CR 614.1c), which
-- is the half the multi-kind row is for.
toolkitSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
toolkitSpec s registry = Spec.describe s "Agent's Toolkit (CR 614.1c)" $ do
  let toolkitName = CardName.MkCardName (Text.pack "Agent's Toolkit")
      -- All four kinds the row names, read off the permanent that entered.
      kindsOn oid gs =
        ( countersOn CounterKind.PlusOnePlusOne oid gs,
          countersOn (CounterKind.Keyword Keyword.Deathtouch) oid gs,
          countersOn (CounterKind.Keyword Keyword.Flying) oid gs,
          countersOn CounterKind.Shield oid gs
        )
      -- Three untapped lands pay the {1}{G}{U}. `scalers` is the only difference
      -- between the two boards, exactly as in perennationSpec.
      board scalers = do
        plains <- S.printingOf s registry "Plains"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        toolkit <- S.printingOf s registry "Agent's Toolkit"
        doublingSeason <- S.printingOf s registry "Doubling Season"
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let bare = S.landsFor forest S.alice 1 (S.landsFor island S.alice 1 (S.landsInPlay plains 1))
            (seasonId, withSeason) = S.addPermanent doublingSeason S.alice bare
            (_, withPraetor) = S.addPermanent vorinclex S.bob withSeason
            (held, ready) = S.addHandCard toolkit S.alice (if scalers then withPraetor else bare)
        pure (seasonId, held, ready)
      castIt seasonFirst (seasonId, held, ready) =
        let ((_, after), asked) = State.runState (Engine.runGame (toolkitOrders seasonFirst seasonId) ready (S.cast S.alice held >> Stack.resolveTop)) 0
         in (asked, newestNamed toolkitName after, after)
  -- The control: one counter of each of the four kinds, and nothing to order.
  Spec.it s "CR 614.1c the artifact enters with one counter of each kind it names" $ do
    built <- board False
    case castIt True built of
      (asked, Just oid, after) -> do
        Spec.assertEqWith s "a +1/+1, a deathtouch, a flying and a shield counter" (kindsOn oid after) (1, 1, 1, 1)
        Spec.assertEqWith s "and with no scaling row in the CR 616.1 pool there was nothing to order" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- The rule. All four kinds move together under whichever row the ONE order put
  -- first; a per-kind opportunity would ask four times and, since every order
  -- after the first is answered the other way, would leave the kinds disagreeing.
  Spec.it s "CR 614.5 one entry is one event, so a multiplier scales all four kinds in its one application" $ do
    built <- board True
    case (castIt True built, castIt False built) of
      ((seasonAsked, Just seasoned, seasonBoard), (praetorAsked, Just halved, praetorBoard)) -> do
        Spec.assertEqWith s "Doubling Season first: one doubled is two, halved is one -- every kind" (kindsOn seasoned seasonBoard) (1, 1, 1, 1)
        Spec.assertEqWith s "the praetor first: one halved is none, doubled is none -- every kind" (kindsOn halved praetorBoard) (0, 0, 0, 0)
        Spec.assertEqWith s "and each board asked for exactly ONE order, not one per kind" (seasonAsked, praetorAsked) (1, 1)
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"

-- ordersEntry's answerer, and TOTAL where that one errors: a row per kind offers
-- pools this one's `wantSeason` is not in, so an error there would preempt the
-- counter assertions and report itself instead of the behaviour. It falls back to
-- the first row offered, which leaves the counts free to disagree and the count of
-- orders free to grow -- both of which the cases below read.
toolkitOrders :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
toolkitOrders seasonFirst seasonId p = case p of
  Prompt.ChooseReplacement _ _ entries -> do
    asked <- State.get
    State.put (asked + 1)
    let wantSeason = if asked <= (0 :: Int) then seasonFirst else not seasonFirst
    pure (maybe 0 Int.toNaturalSaturating (List.findIndex ((== wantSeason) . (== seasonId) . ReplacementEntry.source) entries))
  -- Unreachable in a green run: the prompt fires exactly once per `castIt`
  -- call, on a pool where `wantSeason` matches one of the two rows offered
  -- (Doubling Season's and Vorinclex's), so `findIndex` always answers `Just`.
  -- Kept rather than `error`, per toolkitSpec's own comment above: a wrong
  -- fallback here would fail the counter assertions in a readable way instead
  -- of an opaque one.
  _ -> pure (S.identityAnswer p)

-- CR 614.5 again, this time with UNEQUAL counts per kind, so halving and
-- doubling answer DIFFERENTLY for each -- toolkitSpec's four kinds all carry
-- Literal 1, so an arm that used one kind's evaluated amount for every kind
-- would still pass it.
--
-- Synthetic Uneven Toolkit -- {1}{G}{U} Artifact, whole text: "this artifact
-- enters with two +1/+1 counters and a trample counter on it" -- STANDS IN FOR
-- Voidpouncer ({1}{R} Creature - Eldrazi, oracle checked on Scryfall
-- 2026-08-25), whose kicked clause reads exactly that. The kicked condition
-- itself is writable -- CR 604.2's clause on Pawl.Types.PrintedReplacement, the
-- way Monstrous War-Leech writes it -- and this test does not exercise it: the
-- counter placement is unconditional here. The rest of that sentence, "and with
-- haste", is writable too since EntryRewrite.WithKeywords landed, so the real
-- card now outranks this synthetic and the swap is what #3245 tracks.
unevenToolkitSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
unevenToolkitSpec s registry = Spec.describe s "Synthetic Uneven Toolkit (CR 614.5)" $ do
  let toolkitName = CardName.MkCardName (Text.pack "Synthetic Uneven Toolkit")
      -- The two kinds the row names, at their DIFFERENT printed counts.
      kindsOn oid gs =
        ( countersOn CounterKind.PlusOnePlusOne oid gs,
          countersOn (CounterKind.Keyword Keyword.Trample) oid gs
        )
      -- Same board shape as toolkitSpec's, against the uneven printing.
      board scalers = do
        plains <- S.printingOf s registry "Plains"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        toolkit <- S.printingOf s registry "Synthetic Uneven Toolkit"
        doublingSeason <- S.printingOf s registry "Doubling Season"
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let bare = S.landsFor forest S.alice 1 (S.landsFor island S.alice 1 (S.landsInPlay plains 1))
            (seasonId, withSeason) = S.addPermanent doublingSeason S.alice bare
            (_, withPraetor) = S.addPermanent vorinclex S.bob withSeason
            (held, ready) = S.addHandCard toolkit S.alice (if scalers then withPraetor else bare)
        pure (seasonId, held, ready)
      castIt seasonFirst (seasonId, held, ready) =
        let ((_, after), asked) = State.runState (Engine.runGame (toolkitOrders seasonFirst seasonId) ready (S.cast S.alice held >> Stack.resolveTop)) 0
         in (asked, newestNamed toolkitName after, after)
  -- The control: two +1/+1 counters and one trample counter, and nothing to
  -- order.
  Spec.it s "CR 614.1c the artifact enters with two +1/+1 counters and one trample counter" $ do
    built <- board False
    case castIt True built of
      (asked, Just oid, after) -> do
        Spec.assertEqWith s "two +1/+1 counters and a trample counter" (kindsOn oid after) (2, 1)
        Spec.assertEqWith s "and with no scaling row in the CR 616.1 pool there was nothing to order" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- The rule, made observable per-kind. Both scaling rows are unconditional on
  -- kind, so BOTH apply in the chosen order (CR 616.1's loop reconsiders the
  -- modified event): doubling then halving is lossless for any count (2N/2 = N
  -- exactly), so Doubling Season first leaves every kind exactly as printed.
  -- Halving then doubling is lossy only for an ODD count (Replacement.scale
  -- rounds Halve down), so the praetor first leaves the EVEN +1/+1 count of two
  -- alone and rounds the ODD trample count of one away to zero -- the two kinds
  -- disagreeing under the SAME order is what an arm using one kind's evaluated
  -- amount for every kind would miss: it would answer (2, 2) instead of (2, 0).
  Spec.it s "CR 614.5 one entry is one event, so a multiplier scales each kind by its OWN count" $ do
    built <- board True
    case (castIt True built, castIt False built) of
      ((seasonAsked, Just seasoned, seasonBoard), (praetorAsked, Just halved, praetorBoard)) -> do
        Spec.assertEqWith s "Doubling Season first: doubling then halving is lossless, so both kinds are as printed" (kindsOn seasoned seasonBoard) (2, 1)
        Spec.assertEqWith s "the praetor first: the even +1/+1 count survives, the odd trample count rounds to zero" (kindsOn halved praetorBoard) (2, 0)
        Spec.assertEqWith s "and each board asked for exactly ONE order, not one per kind" (seasonAsked, praetorAsked) (1, 1)
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"

-- Answer an entry's CR 616.1 orders and COUNT them. The first order taken is
-- Doubling Season's row when `seasonFirst`, and every later order taken is the
-- other row -- deliberately inconsistent, so that a second order, if the engine
-- ever asked for one, would move the two kinds of counter apart. Stateful rather
-- than a pure Prompt r -> r for that reason: the two orders are structurally
-- identical, and a pure answerer would answer them the same way and see nothing.
ordersEntry :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
ordersEntry seasonFirst seasonId p = case p of
  Prompt.ChooseReplacement _ _ entries -> do
    asked <- State.get
    State.put (asked + 1)
    let wantSeason = if asked <= (0 :: Int) then seasonFirst else not seasonFirst
    pure
      ( maybe
          (error "Pawl.EntryReplacementSpec.ordersEntry: no matching row offered")
          Int.toNaturalSaturating
          (List.findIndex ((== wantSeason) . (== seasonId) . ReplacementEntry.source) entries)
      )
  _ -> pure (S.identityAnswer p)

-- CR 612.2 reaching an entry row's counter KINDS, at the one place a swap can
-- land two of them on the same key. CR 122.1b's keyword counter is the only kind
-- holding a word, and of the fifteen keywords that rule admits only hexproof
-- carries a payload a pair can reach: Pawl.Engine.Filter.rewriteKeyword descends
-- into a Filter for landwalk, [type]cycling, hexproof and protection, and
-- hexproof is the only one of those four CR 122.1b lists. So a collision needs
-- one permanent entering with two hexproof counters whose qualities the same pair
-- maps together.
--
-- CR 122.1's last sentence is what the merge owes: "Counters with the same name
-- or description are interchangeable", so after the swap the two rows are one
-- tally of one kind rather than a choice of which row survives. Map.mapKeys kept
-- an arbitrary survivor, which is the tally this proves is not what happens.
--
-- Synthetic Warding Beacon -- {2}{U} Artifact, whole text: "Each creature you
-- control enters with a hexproof from Islands counter and two hexproof from
-- Swamps counters on it." SYNTHETIC because no printing enters with two keyword
-- counters that one swap can collide: Scryfall `o:"hexproof counter"`,
-- 2026-08-25, answers four cards, and the only one placing a second keyword
-- counter beside a hexproof one is Perennation, whose other is indestructible --
-- a keyword with no payload, so no pair can bring the two together. Nothing in
-- the CR forbids the card: CR 702.11d makes "hexproof from [quality]" a variant
-- of hexproof, which is what CR 122.1b's closing "as well as any variants of
-- those keywords" admits as a counter, and CR 612.2 swaps the Island in it as a
-- land type word used as a land type.
--
-- IT WATCHES OTHER OBJECTS, Dragonstorm Globe's shape rather than an IsSource
-- row, and for globeChain's reason: the permanent holding the row is on the
-- battlefield for the text change to point at, where hacking the SPELL and
-- letting its own row see the swap is tidewalkerSpec's shape below.
-- Same rewrite either way -- Projection's TurnUpRewrite.WithCounters arm and its
-- EntryRewrite.WithCounters arm both call rewriteWithCounters.
--
-- THE COUNTS ARE UNEQUAL, one and two, so an arbitrary survivor is caught
-- whichever row it kept: the merged tally is three and neither printed count is.
wardingBeaconSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wardingBeaconSpec s registry = Spec.describe s "Synthetic Warding Beacon (CR 612.2)" $ do
  let wardFrom subtype = CounterKind.Keyword (Keyword.Hexproof (Just (Filter.Type.HasSubtype subtype)))
      -- Both kinds the row names, read off the creature that entered.
      wardsOn oid gs = (countersOn (wardFrom Subtype.Island) oid gs, countersOn (wardFrom Subtype.Swamp) oid gs)
  -- The control: unhacked, the two qualities are two kinds at their printed
  -- counts.
  Spec.it s "CR 614.1c unhacked, the two hexproof counters stay two kinds" $ do
    (after, entered) <- beaconChain s registry False
    case entered of
      Nothing -> Spec.assertFailure s "the Goblin Piker did not reach the battlefield"
      Just pikerId -> Spec.assertEqWith s "one hexproof from Islands counter and two hexproof from Swamps counters" (wardsOn pikerId after) (1, 2)
  -- The rule. Hacked Island -> Swamp, both kinds map onto one key and CR 122.1
  -- makes that one tally of three.
  Spec.it s "CR 612.2/122.1 hacked Island -> Swamp, the two rows merge into one tally of three" $ do
    (after, entered) <- beaconChain s registry True
    case entered of
      Nothing -> Spec.assertFailure s "the Goblin Piker did not reach the battlefield"
      Just pikerId -> Spec.assertEqWith s "no hexproof from Islands counter left, and all three counters on hexproof from Swamps" (wardsOn pikerId after) (0, 3)

-- globeChain's board with Magical Hack ({U}) in place of Artificial Evolution:
-- alice controls the Beacon, an Island and six Mountains, and holds the Hack and
-- a Goblin Piker ({2}{R} Creature -- Goblin Warrior). The Beacon is on the
-- battlefield before either spell is cast, which is what makes its row live when
-- the entry loop runs, and hacking the PERMANENT needs no CR 400.7 exception at
-- all. The lands are Islands and Mountains and the swap names Island: it reaches
-- neither, since CR 612.1 applies a text change to the object it is on.
beaconChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, Maybe ObjectId.ObjectId)
beaconChain s registry hack = do
  island <- S.printingOf s registry "Island"
  mountain <- S.printingOf s registry "Mountain"
  beacon <- S.printingOf s registry "Synthetic Warding Beacon"
  magicalHack <- S.printingOf s registry "Magical Hack"
  piker <- S.printingOf s registry "Goblin Piker"
  let base = S.landsFor mountain S.alice 6 (S.landsInPlay island 1)
      (beaconId, g1) = S.addPermanent beacon S.alice base
      (hackId, g2) = S.addHandCard magicalHack S.alice g1
      (pikerId, g3) = S.addHandCard piker S.alice g2
      ready =
        g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      hacked = if hack then castAndResolve (hackAt beaconId Subtype.Island Subtype.Swamp) ready hackId else ready
      after = castAndResolve S.identityAnswer hacked pikerId
  pure (after, newestNamed (S.printingName piker) after)

-- evolveAt's land-type twin, and the target is FILTERED out of the offered set
-- rather than rebuilt: a hand-built recipient of the wrong shape would be dropped
-- at CR 608.2b's re-read with no error, where an empty offered set fails the cast
-- where a reader can see it.
hackAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
hackAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- CR 614.1c's counter row with a CARD-AUTHORED condition on it, which is CR
-- 604.2's clause on Pawl.Types.PrintedReplacement -- the same clause Monstrous
-- War-Leech's "if it was kicked" rides above, over a WithCounters rewrite
-- instead of a RunEffects one (see #2317). Nothing on Pawl.Types.EntryRewrite says
-- when the row applies: rule 702.54a's bloodthirst and rule 702.150a's
-- compleated ask their conditions in Pawl.Engine.Replacement.admitsEntry
-- because those conditions are RULES', with nowhere on a card to live, and this
-- one is the card's.
--
-- Dust Animus {1}{W} Creature -- Spirit 2/3, whole text: "Flying. If you control
-- five or more untapped lands, this creature enters with two +1/+1 counters and
-- a lifelink counter on it. Plot {1}{W}." (oracle checked on Scryfall
-- 2026-08-25)
--
-- THE TWO BOARDS DIFFER IN ONE TAP. Both give alice seven Plains and the Animus
-- in hand; the negative board taps one Plains first. Casting spends {1}{W}, so
-- five lands are untapped as the Animus enters on the first board and four on
-- the second -- CR 614.12 checks the condition against the board the permanent
-- would enter on, which is the board AFTER the cast's own lands were tapped.
-- Same land count, same mana available, same spell: only the tap differs, so a
-- negative that passed because the spell was uncastable is ruled out.
--
-- Seven and five and two and one are distinct, and the readout is doubled: the
-- counters themselves, and the CR 613.4c power and toughness they move in
-- layer 613.1g (4/5 against the printed 2/3).
dustAnimusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dustAnimusSpec s registry = Spec.describe s "Dust Animus (CR 614.1c)" $ do
  let animusName = CardName.MkCardName (Text.pack "Dust Animus")
      countsOn oid gs =
        ( countersOn CounterKind.PlusOnePlusOne oid gs,
          countersOn (CounterKind.Keyword Keyword.Lifelink) oid gs
        )
      board tapOne = do
        plains <- S.printingOf s registry "Plains"
        animus <- S.printingOf s registry "Dust Animus"
        let lands = S.landsInPlay plains 7
            oneLand = List.sort (Set.toList (GameState.battlefield lands))
            tapped = case oneLand of
              landId : _ | tapOne -> S.tapObject landId lands
              _ -> lands
            (held, ready) = S.addHandCard animus S.alice tapped
        pure (held, ready)
      castIt (held, ready) =
        let after = S.runPure S.identityAnswer ready (S.cast S.alice held >> Stack.resolveTop)
         in (newestNamed animusName after, after)
  Spec.it s "CR 614.1c five untapped lands as it enters, so the row applies: two +1/+1 counters and a lifelink counter" $ do
    built <- board False
    case castIt built of
      (Just oid, after) -> do
        Spec.assertEqWith s "two +1/+1 counters and one lifelink counter" (countsOn oid after) (2, 1)
        Spec.assertEqWith s "so the printed 2/3 is a 4/5" (S.powerToughnessOf oid after) (Just (4, 5))
        Spec.assertEqWith s "and the cast tapped two of the seven Plains, leaving five untapped" (S.tappedCount S.alice after) 2
      _ -> Spec.assertFailure s "the Spirit did not reach the battlefield"
  -- The same board and the same seven lands, one of them already tapped. The
  -- Animus ENTERS HERE TOO, so what the pair tells apart is whether the row
  -- applied and not whether the permanent arrived.
  Spec.it s "CR 614.1c only four untapped lands as it enters, so the row does not apply: no counters at all" $ do
    built <- board True
    case castIt built of
      (Just oid, after) -> do
        Spec.assertEqWith s "neither kind of counter arrives" (countsOn oid after) (0, 0)
        Spec.assertEqWith s "so it is the printed 2/3" (S.powerToughnessOf oid after) (Just (2, 3))
        Spec.assertEqWith s "and three of the seven Plains are tapped, leaving four untapped" (S.tappedCount S.alice after) 3
      _ -> Spec.assertFailure s "the Spirit did not reach the battlefield"

-- CR 614.12's OTHER reading of the same clause dustAnimusSpec above rides: WHICH
-- BOARD a card-authored entry condition counts over. A permanent being
-- materialized before its entry loop runs (Pawl.Engine.Event.runEntry) is what
-- lets CR 614.12's "characteristics of the permanent as it would exist on the
-- battlefield" be a plain projection, and it is exactly what must not make the
-- permanent countable -- Pawl.Engine.Projection.boardAsEntering.
--
-- Frontier Mastodon {2}{G} Creature -- Elephant 3/2, whole text: "Ferocious --
-- This creature enters with a +1/+1 counter on it if you control a creature with
-- power 4 or greater." (oracle checked on Scryfall 2026-08-27; "Ferocious" is an
-- ability word and means nothing). The card carries the ruling for one direction
-- and the fast lands carry the other: "Frontier Mastodon's ferocious ability
-- checks if you control a creature with power 4 or greater as Frontier Mastodon
-- enters the battlefield. Because Frontier Mastodon isn't on the battlefield at
-- this time, it won't count itself" (Gatherer 2014-11-24), and "if one of these
-- lands enters the battlefield at the same time as one or more other lands ... it
-- doesn't take those lands into consideration" (Blackcleave Cliffs, Gatherer
-- 2023-02-04).
--
-- TWO PAIRS, one per direction, because either exclusion alone would leave the
-- other's pair green:
--
--   * THE BATCH. Rise of the Dark Realms reanimates Jedit Ojanen (5/5) and the
--     Mastodon out of one graveyard, as one CR 608.2f sweep. The printed 3/2
--     cannot reach power 4 on this board, so only the SIBLING is in question.
--     The positive board moves the Ojanen: same nine Swamps, same cast, same
--     reanimated Mastodon, but the Cat is already on the battlefield.
--   * THE SUBJECT. Glorious Anthem makes the entering Mastodon a 4/3, so a board
--     that could see it would answer its own condition. Nothing else alice
--     controls is a creature. The positive board adds the Ojanen, which under the
--     same Anthem is a 6/6 -- so the pair differs in one permanent, and the
--     numbers 3, 4, 5 and 6 stay distinct.
--
-- The readout is doubled throughout, as dustAnimusSpec's is: the counter itself,
-- and the CR 613.4c power and toughness it moves in layer 613.1g.
frontierMastodonSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
frontierMastodonSpec s registry = Spec.describe s "Frontier Mastodon (CR 614.12)" $ do
  let mastodonName = CardName.MkCardName (Text.pack "Frontier Mastodon")
      -- alice's nine Swamps, Rise of the Dark Realms in her hand, the Mastodon in
      -- her graveyard, and Jedit Ojanen wherever `buried` says. Buried BEFORE the
      -- Mastodon so that the sweep has already put the Cat onto the battlefield
      -- when the Mastodon's own entry loop runs; that ordering is what the
      -- negative rests on, and mutating boardAsEntering away reddens it.
      reanimated buried = do
        swamp <- S.printingOf s registry "Swamp"
        rise <- S.printingOf s registry "Rise of the Dark Realms"
        mastodon <- S.printingOf s registry "Frontier Mastodon"
        ojanen <- S.printingOf s registry "Jedit Ojanen"
        let lands = List.foldl' (\g _ -> snd (S.addPermanent swamp S.alice g)) S.threePlayerGame [1 .. (9 :: Int)]
            withCat = if buried then snd (S.addGraveyardCard ojanen S.alice lands) else snd (S.addPermanent ojanen S.alice lands)
            (_, withMastodon) = S.addGraveyardCard mastodon S.alice withCat
            (ready, held) = S.handOne rise withMastodon
            after = S.runPure S.identityAnswer ready (S.cast S.alice held >> Stack.resolveTop)
        pure (newestNamed mastodonName after, after)
      -- Three Forests for the {2}{G}, Glorious Anthem, and the Mastodon cast from
      -- hand -- one entry, no batch, so only the subject is in question.
      castUnderAnthem withCat = do
        forest <- S.printingOf s registry "Forest"
        anthem <- S.printingOf s registry "Glorious Anthem"
        mastodon <- S.printingOf s registry "Frontier Mastodon"
        ojanen <- S.printingOf s registry "Jedit Ojanen"
        let (_, enchanted) = S.addPermanent anthem S.alice (S.landsInPlay forest 3)
            withOjanen = if withCat then snd (S.addPermanent ojanen S.alice enchanted) else enchanted
            (ready, held) = S.handOne mastodon withOjanen
            after = S.runPure S.identityAnswer ready (S.cast S.alice held >> Stack.resolveTop)
        pure (newestNamed mastodonName after, after)
  Spec.it s "CR 614.12 the 5/5 reanimated in the same sweep is not on the battlefield yet, so the row does not apply" $ do
    (found, after) <- reanimated True
    case found of
      Just oid -> do
        Spec.assertEqWith s "no +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne oid after) 0
        Spec.assertEqWith s "so it is the printed 3/2" (S.powerToughnessOf oid after) (Just (3, 2))
        Spec.assertEqWith s "and the Cat was reanimated beside it" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Jedit Ojanen")) S.alice after) 1
      _ -> Spec.assertFailure s "the Elephant did not reach the battlefield"
  Spec.it s "CR 614.12 the same 5/5 already on the battlefield does count, so the row applies" $ do
    (found, after) <- reanimated False
    case found of
      Just oid -> do
        Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne oid after) 1
        Spec.assertEqWith s "so the printed 3/2 is a 4/3" (S.powerToughnessOf oid after) (Just (4, 3))
      _ -> Spec.assertFailure s "the Elephant did not reach the battlefield"
  Spec.it s "CR 614.12 a 4/3 under Glorious Anthem does not count itself, so the row does not apply" $ do
    (found, after) <- castUnderAnthem False
    case found of
      Just oid -> do
        Spec.assertEqWith s "no +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne oid after) 0
        Spec.assertEqWith s "so the Anthem alone makes the printed 3/2 a 4/3" (S.powerToughnessOf oid after) (Just (4, 3))
      _ -> Spec.assertFailure s "the Elephant did not reach the battlefield"
  Spec.it s "CR 614.12 another creature under the same Anthem does count, so the row applies" $ do
    (found, after) <- castUnderAnthem True
    case found of
      Just oid -> do
        Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne oid after) 1
        Spec.assertEqWith s "so the Anthem and the counter make it a 5/4" (S.powerToughnessOf oid after) (Just (5, 4))
      _ -> Spec.assertFailure s "the Elephant did not reach the battlefield"

-- Synthetic Magnetic Lockdown {2}{W} Instant: "Until end of turn, if you control
-- three or more artifacts, artifacts enter the battlefield tapped." The FLOATING
-- twin of frontierMastodonSpec above: the same CR 614.12a board question, asked
-- of a row CR 614.3 installed rather than of a permanent's static ability.
--
-- SYNTHETIC because the shape has no printing. Both halves are printed, and by
-- different cards -- Kismet writes the EntryR/Tapped rewrite, Galvanic Blast's
-- metalcraft writes the "if you control three or more artifacts" clause -- and
-- what nothing prints is a DURATION-bounded row carrying such a clause. Scryfall
-- with a User-Agent, 2026-08-27: `(t:instant or t:sorcery) o:"would enter the
-- battlefield" o:instead` answers one card, Gather Specimens, which prints no
-- "if"; `o:"instead if you control"` answers fourteen, every one a one-shot
-- instead settled inside its own resolution (Galvanic Blast, Crater's Claws,
-- Mirran Mettle, ...); `o:"until end of turn" o:instead o:"if you control"`
-- answers nineteen modal or pump-vs-pump spells; `o:enters o:"this turn"
-- (t:instant or t:sorcery)` answers seventeen, of which Gather Specimens alone
-- installs an entry REPLACEMENT -- the near miss, Theoretical Duplication, prints
-- "whenever a nontoken creature an opponent controls enters this turn, create a
-- token that's a copy of that creature", which is a delayed trigger and replaces
-- nothing. A printing of any of those families with a stated duration AND a
-- printed "if" would refute this and replace the synthetic. The same enumeration
-- inside data/cards: only Galvanic Blast and Synthetic Voltaic Surge write a
-- condition on an Effect.Replace at all, and both gate a DamageR.
--
-- THE PAIR differs in exactly one permanent, and the entering artifact is what
-- either reading disagrees about. alice controls two Bonesplitters, casts the
-- Lockdown, then casts a Conjurer's Bauble: as the Bauble's entry loop runs she
-- controls TWO artifacts under CR 614.12a and THREE under a reading that counts
-- the permanent mid-entry, so the clause is false one way and true the other and
-- the Bauble enters untapped or tapped accordingly. The positive board is the
-- same board with a third Bonesplitter, where the clause is true either way --
-- so the negative is not the row being off, missing, or expired.
--
-- Conjurer's Bauble rather than a third Bonesplitter: distinct names make
-- `newestNamed` name the entering permanent rather than trust an id order, and
-- {1} keeps the mana apart from the Lockdown's {2}{W} out of four Plains.
magneticLockdownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magneticLockdownSpec s registry = Spec.describe s "Synthetic Magnetic Lockdown (CR 614.12a)" $ do
  let baubleName = CardName.MkCardName (Text.pack "Conjurer's Bauble")
      splitterName = CardName.MkCardName (Text.pack "Bonesplitter")
      board owned = do
        plains <- S.printingOf s registry "Plains"
        splitter <- S.printingOf s registry "Bonesplitter"
        lockdown <- S.printingOf s registry "Synthetic Magnetic Lockdown"
        bauble <- S.printingOf s registry "Conjurer's Bauble"
        let lands = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
            stocked = List.foldl' (\g _ -> snd (S.addPermanent splitter S.alice g)) lands [1 .. owned]
            (lockdownId, g1) = S.addHandCard lockdown S.alice stocked
            (baubleId, g2) = S.addHandCard bauble S.alice g1
            ready =
              g2
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            armed = castAndResolve S.identityAnswer ready lockdownId
        pure (armed, castAndResolve S.identityAnswer armed baubleId)
  -- THE PROVING CASE.
  Spec.it s "CR 614.12a the artifact mid-entry is not one of the three its own row counts, so it enters untapped" $ do
    (armed, after) <- board (2 :: Int)
    case newestNamed baubleName after of
      Just oid -> do
        Spec.assertEqWith s "the Bauble entered untapped" (fmap Object.tapped (Game.lookupObject oid after)) (Just TapState.Untapped)
        Spec.assertEqWith s "setup: alice controlled two artifacts as it entered" (controlledNamed splitterName S.alice after) 2
        Spec.assertEqWith s "setup: the row was installed and is still installed" (length (GameState.replacements armed), length (GameState.replacements after)) (1, 1)
      _ -> Spec.assertFailure s "the Bauble did not reach the battlefield"
  Spec.it s "CR 614.12a a third artifact already on the battlefield does count, so the same row taps it" $ do
    (_, after) <- board (3 :: Int)
    case newestNamed baubleName after of
      Just oid -> do
        Spec.assertEqWith s "the Bauble entered tapped" (fmap Object.tapped (Game.lookupObject oid after)) (Just TapState.Tapped)
        Spec.assertEqWith s "setup: alice controlled three artifacts as it entered" (controlledNamed splitterName S.alice after) 3
      _ -> Spec.assertFailure s "the Bauble did not reach the battlefield"

-- Squad Captain {4}{W} Creature -- Human Soldier 2/2, whole text: "Vigilance
-- (Attacking doesn't cause this creature to tap.) / This creature enters with a
-- +1/+1 counter on it for each other creature you control." (oracle checked on
-- Scryfall 2026-08-28)
--
-- The AMOUNT half of the board question frontierMastodonSpec and
-- magneticLockdownSpec above ask of a CLAUSE: CR 614.12's "how they apply", where
-- what a rewrite reads is not whether a row applies but how many counters it
-- places. Same board, same rule, same exclusion --
-- Pawl.Engine.Projection.boardAsEntering -- reached through
-- Pawl.Engine.Event's EntryRewrite.WithCounters arm rather than through a
-- condition. Until this card the pool's two board-counting rewrites were
-- Tidewalker ("for each Island you control") and Undergrowth Scavenger, whose
-- count folds GRAVEYARDS and so cannot tell the two boards apart at all; and
-- every simultaneous entry data/cards can build is single-typed -- Rise of the
-- Dark Realms' creature cards, Open the Way's land cards, a Create's tokens,
-- kicked Rite of Replication's copies of one permanent -- so none of them puts
-- an Island onto the battlefield beside a Tidewalker. A printing that put a
-- creature and a land onto the battlefield at once would refute that.
-- Squad Captain counts CREATURES, which is exactly what Rise of the Dark Realms
-- reanimates, so one card closes the gap; see #2431.
--
-- THE PAIR differs in one thing: where alice's two other creatures are as the
-- Captain's entry loop runs. Rise of the Dark Realms reanimates the Captain out
-- of a graveyard holding Jedit Ojanen and a Goblin Piker as one CR 608.2f sweep,
-- and the sweep accumulates the battlefield as each member arrives -- both are
-- buried BEFORE the Captain, so both are already sitting there, materialized and
-- unentered, when its rewrite reads. The CR 614.12 determination the rewrite IS
-- runs while all three are materialized and none has entered, so the two are not
-- on the battlefield relative to the Captain: the count is ZERO and the printed
-- 2/2 stays a 2/2. The positive board is the same nine Swamps and the same cast,
-- with those two on the battlefield to start with, where the count is TWO and CR
-- 613.4c's layer 7c makes it a 4/4. Nothing else alice controls is a creature:
-- the nine Swamps are lands, and the Captain's own row says "other" (Filter.Not
-- Filter.IsSource), so 0, 2 and 4 stay distinct from any reading that counted the
-- subject as well.
squadCaptainSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
squadCaptainSpec s registry = Spec.describe s "Squad Captain (CR 614.12)" $ do
  let captainName = CardName.MkCardName (Text.pack "Squad Captain")
      -- alice's nine Swamps for the {7}{B}{B}, Rise of the Dark Realms in hand,
      -- the Captain in her graveyard, and the other two creatures wherever
      -- `buried` says. Buried before the Captain so the sweep has already put
      -- them onto the battlefield when its own entry loop runs; that ordering is
      -- what the negative rests on.
      reanimated buried = do
        swamp <- S.printingOf s registry "Swamp"
        rise <- S.printingOf s registry "Rise of the Dark Realms"
        captain <- S.printingOf s registry "Squad Captain"
        ojanen <- S.printingOf s registry "Jedit Ojanen"
        piker <- S.printingOf s registry "Goblin Piker"
        let lands = List.foldl' (\g _ -> snd (S.addPermanent swamp S.alice g)) S.threePlayerGame [1 .. (9 :: Int)]
            place printing g = if buried then snd (S.addGraveyardCard printing S.alice g) else snd (S.addPermanent printing S.alice g)
            withOthers = place piker (place ojanen lands)
            (_, withCaptain) = S.addGraveyardCard captain S.alice withOthers
            (ready, held) = S.handOne rise withCaptain
            after = S.runPure S.identityAnswer ready (S.cast S.alice held >> Stack.resolveTop)
        pure (newestNamed captainName after, after)
  Spec.it s "CR 614.12 the two creatures reanimated in the same sweep are not on the battlefield yet, so its own count is zero" $ do
    (found, after) <- reanimated True
    case found of
      Just oid -> do
        Spec.assertEqWith s "no +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne oid after) 0
        Spec.assertEqWith s "so it is the printed 2/2" (S.powerToughnessOf oid after) (Just (2, 2))
        Spec.assertEqWith s "and both were reanimated beside it" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Jedit Ojanen")) S.alice after, S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) (1, 1)
      _ -> Spec.assertFailure s "the Captain did not reach the battlefield"
  Spec.it s "CR 614.12 the same two already on the battlefield do count, so it enters with two counters" $ do
    (found, after) <- reanimated False
    case found of
      Just oid -> do
        Spec.assertEqWith s "two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne oid after) 2
        Spec.assertEqWith s "so the printed 2/2 is a 4/4" (S.powerToughnessOf oid after) (Just (4, 4))
      _ -> Spec.assertFailure s "the Captain did not reach the battlefield"

-- CR 122.6 with CR 107.3c: an entry rider whose COUNT is not a number. Printlifter
-- Ooze -- {1}{G} 2/2 Creature -- Ooze with deathtouch and disguise {3}{G} --
-- prints "whenever this creature or another creature you control is turned face
-- up, create a 0/0 green Ooze creature token with trample. The token enters with
-- X +1/+1 counters on it, where X is the number of other creatures you control",
-- and is the printing that puts the count INSIDE the rider: Scryfall
-- `o:/token enters with/ -is:digital`, 2026-08-22, answers this card and Ochre
-- Jelly, whose split token is an Effect.CreateCopy -- the OTHER opcode that now
-- carries the same rider (Pawl.CopySpec, Littjara Mirrorlake). The other
-- "X +1/+1 counters on it, where X" printings say it in a SECOND SENTENCE, which
-- is an Effect.PutCounters after the Create and needs no rider at all -- CR 704.3
-- is why the 0/0 survives in between.
--
-- X is defined by the ability's own text (CR 107.3c) and answered once, when the
-- effect is applied (CR 608.2h) -- Pawl.Engine.Resolve.Effect.freezeRiders, against the
-- resolution's own context, which is what makes Filter.IsSource and CR 109.5's
-- "you" live on this path.
--
-- THE BOARD SEPARATES FIVE READINGS. alice controls Printlifter Ooze, an Ainok
-- Tracker cast face down for CR 702.37a's {3}, and two Goblin Pikers; bob
-- controls two Goblin Pikers of his own. Turning the Ainok face up (CR 702.37e,
-- paying {4}{R} out of the eight Mountains) fires the trigger, and X is THREE --
-- the other three creatures alice controls. A reading that counts the source too
-- says four, as does one that counts after the token arrived; one that drops CR
-- 109.5's "you" says five; one that reads only the permanent that turned over
-- says one; and a rider dropped entirely says zero, which CR 704.5f then puts in
-- the graveyard before anything can read it.
--
-- Two rather than one Goblin Piker per seat so that those five are five different
-- numbers, and a face-down permanent is CR 708.2a's 2/2, so three is not a
-- toughness on the board either.
printlifterSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
printlifterSpec s registry = Spec.describe s "The counters a Create says its token enters with (CR 122.6)" $ do
  let oozeTokenName = CardName.MkCardName (Text.pack "Ooze Token")
      -- The permanent a move added to the battlefield, or Nothing when it added
      -- none or several. Identifies the new incarnation without asking what card
      -- is under it, which is the whole point of a face-down one.
      enteredOne gs after = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield gs)) of
        [oid] -> Just oid
        _ -> Nothing
      -- Eight Mountains: three for CR 702.37a's morph cast and five for CR
      -- 702.37e's {4}{R} turn-up cost. Answers the settled board and the
      -- face-down permanent, or Nothing if the morph cast did not land.
      board = do
        mountain <- S.printingOf s registry "Mountain"
        ooze <- S.printingOf s registry "Printlifter Ooze"
        ainok <- S.printingOf s registry "Ainok Tracker"
        piker <- S.printingOf s registry "Goblin Piker"
        let (handed, morphCard) = S.handOne ainok (S.landsInPlay mountain 8)
            seat pid gs = snd (S.addPermanent piker pid gs)
            seated = seat S.bob (seat S.bob (seat S.alice (seat S.alice (snd (S.addPermanent ooze S.alice handed)))))
            after = S.runPure S.identityAnswer seated (Cast.castSpell S.manaPerformer S.alice morphCard (S.printingName ainok) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
        pure (fmap (\permanent -> (after, permanent)) (enteredOne seated after))
  Spec.it s "CR 122.6 the token enters with one +1/+1 counter per OTHER creature alice controls" $ do
    made <- board
    case made of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (settled, morphling) -> do
        -- The controls: the action really is on offer beforehand, so a board with
        -- no token below is the rider and not a permanent that never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice settled) [(morphling, TurnUpProcedure.Morph)]
        let after = S.runPure S.identityAnswer settled (FaceDown.turnFaceUp S.manaPerformer S.alice TurnUpProcedure.Morph morphling >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 110.5 it turned face up" (fmap Object.facing (Game.lookupObject morphling after)) (Just Facing.FaceUp)
        case newestNamed oozeTokenName after of
          Nothing -> Spec.assertFailure s "CR 111.1 the Ooze token did not reach the battlefield"
          Just token -> do
            -- THE DISCRIMINATING ASSERTION. Three: the Ainok, and alice's two
            -- Goblin Pikers. Not the Ooze itself (CR 122.6 through the ability's
            -- own Filter.IsSource), not bob's two, and not the token.
            Spec.assertEqWith s "CR 122.6 three +1/+1 counters, one per other creature alice controls" (countersOn CounterKind.PlusOnePlusOne token after) 3
            -- What the count is FOR: the token is printed 0/0, so CR 613.4c's
            -- layer 7c leaves a 3/3 rather than something CR 704.5f removes.
            Spec.assertEqWith s "CR 613.4c so the 0/0 token is a 3/3" (S.powerToughnessOf token after) (Just (3, 3))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  riotSpec s registry
  unleashSpec s registry
  bloodthirstSpec s registry
  brineElementalSpec s registry
  coldsteelHeartSpec s registry
  stuffyDollSpec s registry
  vorinclexSpec s registry
  damageCountersSpec s registry
  entryCountersSpec s registry
  perennationSpec s registry
  toolkitSpec s registry
  unevenToolkitSpec s registry
  wardingBeaconSpec s registry
  dustAnimusSpec s registry
  frontierMastodonSpec s registry
  magneticLockdownSpec s registry
  squadCaptainSpec s registry
  printlifterSpec s registry
