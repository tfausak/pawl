{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 710 flip cards end to end: Pawl.Types.Layout's Flip arm and the two
-- Pawl.Engine.Card functions that read it (CR 710.2's normal view through
-- `combined`, CR 710.1b/710.1c's alternative view through `flippedFace`),
-- Pawl.Types.Object's flipped status (CR 110.5) with the substitution
-- Pawl.Engine.Game.resolveFaceFor and `namesFor` make of it, the writer
-- Pawl.Engine.Game.flipPermanent with its `flipsOver` gate, the Effect.Flip arm
-- of Pawl.Engine.Resolve, and CR 710.4's clearing on departure through
-- Pawl.Types.Object.newIncarnation.
--
-- Every case runs against the printed Akki Lavarunner // Tok-Tok, Volcano Born,
-- which is CR 710.2's own worked Example: a {3}{R} 1/1 Goblin Warrior with haste
-- and "whenever this creature deals damage to an opponent, flip it", whose
-- alternative half is the legendary 2/2 Goblin Shaman Tok-Tok, Volcano Born with
-- protection from red. It was picked because the rule prints it, and because the
-- two halves disagree about every characteristic CR 710.2 swaps while agreeing
-- about the two CR 710.1c withholds.
--
-- Also CR 603.2's recipient half: Akki's trigger names an OPPONENT, which the
-- card spells as TriggerCondition.SelfDealsCombatDamageToPlayer's Opponent
-- relation. CR 614.9's redirection is what tells that apart from "a player" --
-- see the Harm's Way pair at the end.
--
-- Not implemented: Akki's printed trigger is "whenever this creature deals
-- damage to an opponent", and the card carries the COMBAT-damage condition, so
-- noncombat damage it deals to an opponent does not flip it (#3363). Nor CR
-- 710.5's alternative name, which is #679's, nor whether a permanent that COPIED
-- a flip card may flip at all (#3366).
--
-- CR 730.2h's merged permanent containing a flip card is Pawl.MutateSpec's, this
-- card being the pool's only flip printing and Cubwarden what merges with it.
module Pawl.FlipSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Protection as Protection
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Zone as Zone

akkiName, tokTokName :: CardName.CardName
akkiName = CardName.MkCardName (Text.pack "Akki Lavarunner")
tokTokName = CardName.MkCardName (Text.pack "Tok-Tok, Volcano Born")

-- Everything the two halves disagree about, read through the projection, as one
-- tuple: names, power and toughness, subtypes, the legendary supertype, and
-- protection from red. Asserted whole so a case names the half rather than five
-- independent facts, and so a change that moved only one of them cannot pass.
halfReadings ::
  ObjectId.ObjectId ->
  GameState.GameState ->
  (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Set.Set Supertype.Supertype, Bool)
halfReadings oid gs =
  ( Projection.namesOf oid gs,
    S.powerToughnessOf oid gs,
    Projection.subtypesOf oid gs,
    Projection.supertypesOf oid gs,
    Projection.hasKeyword (Keyword.Protection Protection.MkProtection {Protection.quality = Filter.Type.HasColor Color.Red, Protection.spares = Nothing}) oid gs
  )

normalHalf, alternativeHalf :: (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Set.Set Supertype.Supertype, Bool)
normalHalf = (Set.singleton akkiName, Just (1, 1), Set.fromList [Subtype.Goblin, Subtype.Warrior], Set.empty, False)
alternativeHalf = (Set.singleton tokTokName, Just (2, 2), Set.fromList [Subtype.Goblin, Subtype.Shaman], Set.singleton Supertype.Legendary, True)

-- The two characteristics CR 710.1c withholds from the swap, read as a pair:
-- mana value and colour. The DISCRIMINATOR this whole unit turns on -- an engine
-- that implemented flipping as a face swap gets `halfReadings` entirely right and
-- answers 0 and colourless here, Tok-Tok's half printing no mana cost.
costReadings :: ObjectId.ObjectId -> GameState.GameState -> (Maybe Integer, Set.Set Color.Color)
costReadings oid gs = (PC.manaValue (Projection.project oid gs), Projection.colorsOf oid gs)

-- CR 710.2's Example asks two effects about the card, and both go through one
-- predicate: "search your library for a legendary card" and "legendary creatures
-- get +2/+2" each evaluate the supertype the object HAS. Asked here as the
-- filter atom a card's own text compiles to, against the projection's view.
--
-- Read through the filter rather than through a card, because no printing in
-- data/cards/ says either of the Example's two sentences: grepping the corpus
-- for HasSupertype (2026-09-07) returns Kellan Joins Up's enters TRIGGER,
-- Cleopatra, Exiled Pharaoh's target pool, and Undercity's and Synthetic Tidal
-- Waste's nonbasic-land clauses -- a legendary-filtered library search and a
-- static pump over legendary creatures are both absent. A card printing either
-- would refute the claim, not the rule; Cleopatra's "up to two other target
-- legendary creatures" is the nearest the pool comes, and it reaches this same
-- predicate.
isLegendary :: ObjectId.ObjectId -> GameState.GameState -> Bool
isLegendary oid gs =
  Filter.matches
    (Filter.contextFor (Game.teams gs) (Just S.alice) Nothing)
    (Projection.viewOfObject oid gs)
    (Filter.Type.HasSupertype Supertype.Legendary)

-- Aim Harm's Way at alice and choose `src` as CR 609.7a's source -- the answerer
-- Pawl.DamageReplacementSpec's harmsWaySpec uses, pointed the other way. FILTERED
-- out of the offered set rather than built, so a recipient the engine never
-- offered cannot be smuggled in (#222).
harmsWayAtAlice :: ObjectId.ObjectId -> Prompt.Prompt r -> r
harmsWayAtAlice src p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.alice) . snd) sets
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> S.identityAnswer p

-- The CR 614.9 pair: alice's Akki attacking bob, with and without a resolved
-- Harm's Way redirecting Akki's combat damage onto alice. The two boards differ
-- in exactly one thing -- whether bob's instant resolved -- and everything else,
-- Harm's Way in bob's hand and the Plains that pays for it included, is on both.
--
-- Built on S.combatBoardOf rather than through Pawl.Support's script harness,
-- which has no vocabulary for CR 609.7a's source choice: bob's instant is cast
-- and resolved at the declare-attackers board, and combat is then run whole, so
-- CR 510.2's damage, CR 603.3's trigger placement and CR 608's resolution all
-- still happen inside the engine.
redirectPair :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, GameState.GameState, GameState.GameState)
redirectPair s registry = do
  akki <- S.printingOf s registry "Akki Lavarunner"
  plains <- S.printingOf s registry "Plains"
  harmsWay <- S.printingOf s registry "Harm's Way"
  let (base, mine, _) = S.combatBoardOf [akki] []
      g1 = S.landsFor plains S.bob 1 base
      (harmsWayId, ready) = S.addHandCard harmsWay S.bob g1
      cast gs akkiId = S.runPure (harmsWayAtAlice akkiId) (gs {GameState.priority = Just S.bob}) (S.cast S.bob harmsWayId *> Stack.resolveTop)
      fight = S.runCombat (S.attackTo S.bob)
  case mine of
    [akkiId] -> pure (akkiId, fight ready, fight (cast ready akkiId))
    _ -> Spec.assertFailure s "the fixture should have exactly one attacker"

-- alice's Akki alone against bob, with combat about to start. Nothing else is on
-- the battlefield, so the only thing that can deal bob combat damage is the
-- creature this unit is about.
akkiDuel :: S.Board
akkiDuel = S.duel S.beginningOfCombat [S.settled "akki" "Akki Lavarunner"] []

-- Akki attacks bob, unblocked, and the whole combat phase runs -- so CR 510.2's
-- damage, CR 603.3's trigger placement and CR 608's resolution all happen inside
-- the engine rather than being poked in.
attackScript :: Seq.Seq S.Timed
attackScript = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "akki"])]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Flip" $ do
  -- CR 710.2, first sentence: "in every zone other than the battlefield, and
  -- also on the battlefield before the permanent flips, a flip card has only the
  -- normal characteristics of the card."
  --
  -- The falsifier is CR 709.4's reading, which is what the pool's other
  -- two-frame layout would take: a combined view would name this permanent
  -- "Akki Lavarunner//Tok-Tok, Volcano Born", make it legendary, and give it
  -- protection from red while it is still Akki.
  Spec.it s "CR 710.2 an unflipped permanent shows its normal half and only that" $ do
    akki <- S.printingOf s registry "Akki Lavarunner"
    let (oid, gs) = S.addPermanent akki S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "every reader sees Akki Lavarunner" (halfReadings oid gs) normalHalf
    Spec.assertEqWith s "CR 710.1c: {3}{R} is mana value 4 and red" (costReadings oid gs) (Just 4, Set.singleton Color.Red)
  -- CR 710.2's Example, the search half: "an effect that says 'search your
  -- library for a legendary card' can't find this flip card." A card in a
  -- library is in a zone other than the battlefield, so rule 710.2's first
  -- sentence leaves it the normal half's type line -- which prints no supertype.
  Spec.it s "CR 710.2 the flip card in a library is not legendary" $ do
    akki <- S.printingOf s registry "Akki Lavarunner"
    let (oid, gs) = S.addObjectIn Zone.Library akki S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertBool s (not (isLegendary oid gs)) "a legendary search must not find the flip card"
    Spec.assertEqWith s "and it is named for its normal half" (Projection.namesOf oid gs) (Set.singleton akkiName)
  -- THE proving case. CR 710.2: "once a permanent is flipped, its normal name,
  -- text box, type line, power, and toughness don't apply and the alternative
  -- versions of those characteristics apply instead."
  --
  -- Played out from the card's own text: Akki attacks unblocked, CR 510.2 deals
  -- bob 1 combat damage, the printed trigger fires and resolves, and its Flip
  -- opcode sets the status. Every characteristic reader is asked before and
  -- after, on the SAME ObjectId -- CR 110.5 makes flipping a status change, not
  -- a new object -- so an engine that answered by replacing the permanent fails
  -- here too.
  --
  -- The mana-value pair is what separates this rule from a face swap, and it is
  -- asserted at both ends: Tok-Tok's half prints no mana cost, so an
  -- implementation that reached the alternative half by pointing Object.face at
  -- it would answer mana value 0 and colourless after the flip and pass every
  -- other assertion in this case.
  Spec.it s "CR 710.2 Akki flips into Tok-Tok, keeping CR 710.1c's mana value and colour" $ do
    built <- S.buildBoardOrFail s registry akkiDuel
    case Map.lookup (S.MkObjectAlias (Text.pack "akki")) (S.builtAliases built) of
      Nothing -> Spec.assertFailure s "the board omitted the Akki alias"
      Just oid -> do
        Spec.assertEqWith s "before: Akki Lavarunner" (halfReadings oid (S.builtState built)) normalHalf
        Spec.assertBool s (not (isLegendary oid (S.builtState built))) "and a legendary pump does not reach Akki"
        (_, after) <- S.runScriptOrFail s attackScript built S.combatGame
        Spec.assertEqWith s "the 1/1 connected" (S.lifeOf S.bob after) (Just 19)
        Spec.assertEqWith s "after: Tok-Tok, Volcano Born" (halfReadings oid after) alternativeHalf
        Spec.assertBool s (isLegendary oid after) "CR 710.2's Example: a legendary pump does reach Tok-Tok"
        Spec.assertEqWith
          s
          "CR 710.1c: the flipped permanent is still mana value 4 and still red"
          (costReadings oid after)
          (Just 4, Set.singleton Color.Red)
        -- CR 110.5 with CR 710.2: what changed is the object's STATUS, so the
        -- permanent that reads as Tok-Tok is the one the flag is on.
        Spec.assertEqWith s "and the status itself is set" (fmap Object.flipped (Game.lookupObject oid after)) (Just True)
  -- CR 710.4: "if a flipped permanent leaves the battlefield, it retains no
  -- memory of its status." The flipped permanent is put into its owner's hand
  -- and then back onto the battlefield; CR 400.7's new object arrives unflipped,
  -- so every reader is Akki's again.
  Spec.it s "CR 710.4 a flipped permanent that leaves the battlefield comes back unflipped" $ do
    built <- S.buildBoardOrFail s registry akkiDuel
    case Map.lookup (S.MkObjectAlias (Text.pack "akki")) (S.builtAliases built) of
      Nothing -> Spec.assertFailure s "the board omitted the Akki alias"
      Just oid -> do
        (_, flipped) <- S.runScriptOrFail s attackScript built S.combatGame
        Spec.assertEqWith s "flipped first" (halfReadings oid flipped) alternativeHalf
        let (inHand, bounced) = S.runPureWith S.identityAnswer flipped (Event.changeZoneReturning oid Zone.Hand)
        case Foldable.toList inHand of
          [handId] -> do
            let (back, returned) = S.runPureWith S.identityAnswer bounced (Event.changeZoneReturning handId Zone.Battlefield)
            case Foldable.toList back of
              [returnedId] -> do
                Spec.assertEqWith s "CR 710.4: the returning permanent is Akki Lavarunner again" (halfReadings returnedId returned) normalHalf
                Spec.assertEqWith s "and its status is unflipped" (fmap Object.flipped (Game.lookupObject returnedId returned)) (Just False)
                Spec.assertBool s (not (isLegendary returnedId returned)) "so a legendary pump no longer reaches it"
              other -> Spec.assertFailure s ("expected one permanent back, got " <> show (length other))
          other -> Spec.assertFailure s ("expected one card in hand, got " <> show (length other))
  -- CR 603.2: the trigger fires on the event its printed condition names, and
  -- Akki's names an OPPONENT. CR 614.9's redirection is what tells that apart
  -- from "a player": Harm's Way "is dealt to any target instead" replaces the
  -- damage event's recipient and nothing else, so bob can make Akki's combat
  -- damage land on alice -- Akki's own controller, and no opponent of hers.
  --
  -- A PAIR of boards differing in exactly one thing, whether bob's instant
  -- resolved. The control is what keeps the negative honest: on the same board
  -- with the redirect never cast, the same attack does flip Akki, so the
  -- no-flip below is the recipient and not a combat that failed to happen.
  Spec.it s "CR 603.2 Harm's Way sends Akki's combat damage to alice, and Akki does not flip" $ do
    (oid, plain, redirected) <- redirectPair s registry
    -- The control first, so a board that never fought is caught here.
    Spec.assertEqWith s "control: bob took the 1 and Akki flipped" (halfReadings oid plain) alternativeHalf
    Spec.assertEqWith s "control: bob is the one who lost life" (S.lifeOf S.bob plain, S.lifeOf S.alice plain) (Just 19, Just 20)
    -- THE gameplay assertion: same attack, recipient moved, no flip.
    Spec.assertEqWith s "CR 603.2: the damage went to alice, so Akki is still Akki" (halfReadings oid redirected) normalHalf
    Spec.assertEqWith s "and the status was never set" (fmap Object.flipped (Game.lookupObject oid redirected)) (Just False)
    -- The proxy, after the behaviour: the redirect really moved the event, so
    -- the negative is not a combat that failed to deal damage.
    Spec.assertEqWith s "CR 614.9: alice took the 1 and bob took none" (S.lifeOf S.bob redirected, S.lifeOf S.alice redirected) (Just 20, Just 19)
