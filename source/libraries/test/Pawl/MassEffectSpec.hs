{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over the effects with no target that sweep a whole zone:
-- mass destruction and return, reanimation, and the exile-and-play effects.
-- The machinery is Pawl.ResolveSpec.
module Pawl.MassEffectSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Whether a seat is still playing, and if not why it left (CR 800.4a). Nothing
-- for a PlayerId no roster holds.
statusOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Status.Status
statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))

-- The one activated ability of a printing that declares exactly one -- Prodigal
-- Sorcerer's {T}, which is all these fixtures reach for. Nothing for any other
-- printing, so a card that grew a second ability fails the case that names it
-- rather than silently picking whichever came first (Pawl.TargetSpec's
-- soleTargetSlot is the same shape for the same reason).
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
soleActivatedAbility p = case Face.activatedAbilities (S.combinedFace p) of
  [only] -> Just only
  _ -> Nothing

-- Day of Judgment, cast off four Plains from alice's hand and resolved. Every
-- test in the group below goes through the whole card -- cast, pay, resolve --
-- because "Destroy all creatures" has nothing to exercise at the opcode level
-- that the card does not exercise better: it takes no target and prompts for
-- nothing, so a hand-built applyEffect call would differ from a real cast only
-- in the mana.
castDayOfJudgment :: Printing.Printing -> Printing.Printing -> GameState.GameState -> GameState.GameState
castDayOfJudgment plains dayOfJudgment board =
  let (withSpell, spell) = S.handOne dayOfJudgment (List.foldl' (\gs _ -> snd (S.addPermanent plains S.alice gs)) board [1 :: Int .. 4])
      afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
   in S.runPure S.identityAnswer afterCast Stack.resolveTop

destroyAllSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
destroyAllSpec s registry = Spec.describe s "DestroyAll" $ do
  -- CR 115.10a: "Unless that object or player is identified by the word
  -- 'target' ... it's not a target." "All creatures" is not a target, so the
  -- card declares no target slot and the cast never raises a target prompt
  -- -- and CR 608.2b, which is about targets, has nothing to fizzle.
  Spec.it s "CR 115.10a Day of Judgment targets nothing: no target slot and no target prompt" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let card = Printing.card dayOfJudgment
        (his, g1) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        (withSpell, spell) = S.handOne dayOfJudgment (List.foldl' (\gs _ -> snd (S.addPermanent plains S.alice gs)) g1 [1 :: Int .. 4])
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseTargets {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asked = State.execState (Engine.runGame countingAnswer withSpell (S.cast S.alice spell)) 0
    Spec.assertEqWith s "no target slot anywhere on the card" (Modal.allTargetSlots (Face.spell (Card.combined card))) Map.empty
    Spec.assertEqWith s "and nothing was asked to target" asked 0
    -- The board still resolves the way the first test says it does, from the
    -- same cast -- so "targets nothing" is not "affects nothing".
    Spec.assertBool s (not (S.onBattlefield his (castDayOfJudgment plains dayOfJudgment g1))) "the creature still died"

-- The same CR 109.2a sweep with its scope taken from another SLOT of the same
-- announcement rather than from CR 109.5's perspective: Rise of the Dark Realms
-- (data/scenarios/mass-effect/) names the whole table, this names the one player
-- the trigger targeted; see #1310.
--
-- Angel of Finality {3}{W} Creature -- Angel 3/4 -- "Flying / When this creature
-- enters, exile target player's graveyard." (name, cost, type line, power,
-- toughness and Oracle text checked against api.scryfall.com, 2026-08-20). The
-- whole card is transcribed, and the only clause with a resolution-time effect is
-- the trigger, so nothing else on it can be what these assertions read.
--
-- THREE SEATS, and a graveyard per seat, because the board has to tell four
-- readings of "target player's graveyard" apart:
--
--   * THE TARGETED SEAT versus YOUR OWN. alice controls the Angel and targets
--     bob, so a scope that had stayed CR 109.5's "you" would empty the wrong
--     graveyard.
--   * THE TARGETED SEAT versus EACH PLAYER'S. carol's graveyard is stocked too
--     and must survive -- the reading Rise of the Dark Realms takes.
--   * THE TARGETED SEAT versus OPPONENTS'. carol is alice's opponent as much as
--     bob is (CR 806.1), so her surviving separates those two readings as well;
--     a two-seat board could not.
--   * A GRAVEYARD versus the battlefield. bob controls a Benalish Hero, which
--     stays put: the sweep reads CR 400.1's per-player graveyard, not the seat's
--     permanents.
--
-- Every buried card is of a printing nobody else has, so the graveyard assertion
-- names which seat lost what rather than counting.
angelOfFinalitySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
angelOfFinalitySpec s registry = Spec.describe s "AngelOfFinality" $ do
  Spec.it s "CR 109.2a only the targeted player's graveyard is exiled" $ do
    angel <- S.printingOf s registry "Angel of Finality"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    sentry <- S.printingOf s registry "Ogre Sentry"
    hero <- S.printingOf s registry "Benalish Hero"
    murder <- S.printingOf s registry "Murder"
    judgment <- S.printingOf s registry "Day of Judgment"
    forest <- S.printingOf s registry "Forest"
    let (heroId, withHero) = S.addPermanent hero S.bob S.threePlayerGame
        buried =
          List.foldl'
            (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g))
            withHero
            [ (piker, S.alice),
              (murder, S.alice),
              (maiden, S.bob),
              (judgment, S.bob),
              (sentry, S.carol),
              (forest, S.carol)
            ]
        (_, entered) = S.entersWithTrigger angel S.alice buried
        -- The offered set is FILTERED rather than rebuilt, so the answer is a
        -- recipient the engine itself minted; three seats are offered where one
        -- is wanted, so the prompt is a real choice rather than an elision.
        atBob :: Prompt.Prompt r -> r
        atBob p = case p of
          Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
          _ -> S.identityAnswer p
        placed = S.runPure atBob entered Engine.placePendingTriggers
        after = S.runPure atBob placed Stack.resolveTop
        named = Just . CardName.MkCardName . Text.pack
        exiled gs = List.sort (fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Set.toList (GameState.exile gs)))
    Spec.assertEqWith
      s
      "bob's graveyard is empty and the other two keep every card"
      ( namesIn Zone.Graveyard S.alice after,
        namesIn Zone.Graveyard S.bob after,
        namesIn Zone.Graveyard S.carol after
      )
      ( [named "Goblin Piker", named "Murder"],
        [],
        [named "Ogre Sentry", named "Forest"]
      )
    Spec.assertEqWith
      s
      "the two cards that left bob's graveyard are the two now in exile"
      (exiled after)
      (List.sort [named "Bird Maiden", named "Day of Judgment"])
    Spec.assertBool s (S.onBattlefield heroId after) "bob's creature was never moved, so nothing swept the battlefield"

-- CR 401.4's arrangement of two or more simultaneous library arrivals, taken back
-- from the owner by text that states a RANDOM order -- the placement angel above
-- has no reason to state, its destination being exile.
--
-- Endurance {1}{G}{G} Creature -- Elemental Incarnation 3/4, "Flash / Reach /
-- When this creature enters, up to one target player puts all the cards from
-- their graveyard on the bottom of their library in a random order. / Evoke--
-- Exile a green card from your hand." (name, cost, type line, power, toughness
-- and Oracle text checked against api.scryfall.com, 2026-08-20). Evoke is
-- Pawl.CastSpec's "Evoke" group's; this group puts Endurance straight onto the
-- battlefield.
--
-- THE RANDOMNESS IS THE ANSWERER'S, which is what makes this observable at all:
-- the engine rolls nothing, it asks Prompt.Shuffle, so a fixture that names a
-- permutation names the resulting library. The answer below is built from the
-- object ids rather than from the batch's own order, so it is neither the batch
-- nor its reverse under any sweep order -- an engine that ignored the answer, and
-- one that asked CR 401.4's owner instead, each leave a DIFFERENT library.
--
-- THREE SEATS: alice controls the Endurance and targets bob, and carol's
-- graveyard is stocked too, so "the targeted player's graveyard" is told apart
-- from "yours", "each player's" and "your opponents'" (CR 102.3 read through CR
-- 806.1's free-for-all makes carol an opponent as much as bob). bob's library is
-- stocked with one card the trigger never touches, so the three arrivals are read
-- as the BOTTOM of a library rather than as the whole of one.
enduranceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
enduranceSpec s registry = Spec.describe s "Endurance" $ do
  Spec.it s "CR 401.4 a stated random order puts the batch on the bottom in the order the randomness named" $ do
    endurance <- S.printingOf s registry "Endurance"
    sentry <- S.printingOf s registry "Ogre Sentry"
    maiden <- S.printingOf s registry "Bird Maiden"
    judgment <- S.printingOf s registry "Day of Judgment"
    murder <- S.printingOf s registry "Murder"
    piker <- S.printingOf s registry "Goblin Piker"
    forest <- S.printingOf s registry "Forest"
    let stocked = snd (S.addLibraryCard sentry S.bob S.threePlayerGame)
        (maidenId, g1) = S.addGraveyardCard maiden S.bob stocked
        (judgmentId, g2) = S.addGraveyardCard judgment S.bob g1
        (murderId, g3) = S.addGraveyardCard murder S.bob g2
        elsewhere =
          List.foldl'
            (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g))
            g3
            [(piker, S.alice), (forest, S.carol)]
        (_, entered) = S.entersWithTrigger endurance S.alice elsewhere
        -- The arrangement, from the chosen end INWARD: the Day of Judgment ends
        -- up deepest and the Bird Maiden nearest the top of the three.
        ordering :: Prompt.Prompt r -> r
        ordering p = case p of
          Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
          Prompt.Shuffle _ -> [judgmentId, murderId, maidenId]
          _ -> S.identityAnswer p
        placed = S.runPure ordering entered Engine.placePendingTriggers
        after = S.runPure ordering placed Stack.resolveTop
        named = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "bob's library, top first, is the card that was already there and then the three arrivals in the named order"
      (namesIn Zone.Library S.bob after)
      [named "Ogre Sentry", named "Bird Maiden", named "Murder", named "Day of Judgment"]
    Spec.assertEqWith
      s
      "bob's graveyard is empty and the other two seats keep theirs"
      ( namesIn Zone.Graveyard S.bob after,
        namesIn Zone.Graveyard S.alice after,
        namesIn Zone.Graveyard S.carol after
      )
      ([], [named "Goblin Piker"], [named "Forest"])
    Spec.assertEqWith s "and nothing arrived in alice's or carol's library" (namesIn Zone.Library S.alice after, namesIn Zone.Library S.carol after) ([], [])

-- CR 608.2d's choice made WHILE APPLYING an effect, over a graveyard:
-- ObjectRef.ChosenCardInGraveyard, where Rise of the Dark Realms
-- (data/scenarios/mass-effect/) is the same zone swept as a set.
--
-- Port of Karfell -- Land, "This land enters tapped. {T}: Add {U}. {3}{U}{B}{B},
-- {T}, Sacrifice this land: Mill four cards, then return a creature card from
-- your graveyard to the battlefield tapped." (name, type line and Oracle text
-- checked against api.scryfall.com). The whole card is transcribed.
--
-- NOT A TARGET, which is the distinction the arm exists for: the card never says
-- the word, so CR 115.1c leaves the ability untargeted, nothing is announced as
-- it goes on the stack (CR 601.2c) and nothing is re-checked at resolution (CR
-- 608.2b). A graveyard being a public zone (CR 400.2) is what would ALLOW such a
-- card to target -- it is not what makes this one choose.
--
-- THREE SEATS, and a board built so that five readings of "a creature card from
-- your graveyard" are told apart:
--
--   * THE CHOSEN card versus the FIRST matching one. alice buries two creature
--     cards; the answer is pinned to the second, and Replay.defaultAnswer -- what
--     S.identityAnswer falls through to -- picks the first. The two legs below
--     differ in the answerer and in nothing else, so an engine that picked for
--     the player would give the same card twice.
--   * A CREATURE CARD versus the whole zone. alice buries a Murder as well, and
--     the four Swamps her own mill puts there are candidates for no reading.
--   * YOUR graveyard versus each player's, and versus an opponent's. bob and
--     carol each bury a creature card of a printing alice does not have, and
--     both must stay buried.
--   * A GRAVEYARD versus the battlefield. carol controls a Benalish Hero, which
--     a battlefield reading of the same sentence could hand to alice.
--   * A CHOICE versus a sweep. Exactly one card comes back, though two match.
--
-- Ten lands rather than the six the ability costs: the payment taps sources one
-- prompt at a time, and a board with no slack could fail to cover {U}{B}{B} for
-- reasons that have nothing to do with what is under test.
portOfKarfellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
portOfKarfellSpec s registry =
  let -- alice controls five Swamps, five Islands and one untapped Port of
      -- Karfell; `buried` goes into the named graveyards in the order given, and
      -- `stock` into alice's library. Returns the Port's id.
      board port swamp island hero buried stock =
        let mana = S.landsFor island S.alice 5 (S.landsFor swamp S.alice 5 S.threePlayerGame)
            (_, withHero) = S.addPermanent hero S.carol mana
            (portId, withPort) = S.addPermanent port S.alice withHero
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) withPort buried
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) withGraves stock
         in (portId, withStock {GameState.priority = Just S.alice})
      -- The ability that mills and returns, told from the mana ability by the
      -- sacrifice its cost carries -- never by position in the list, which no
      -- rule fixes.
      returnAbility portId gs =
        filter
          (elem CostComponent.SacrificeThis . Cost.Type.components . ActivatedAbility.cost)
          (Activatable.abilitiesFor portId gs)
      -- Activate the ability and resolve it, keeping the RESPONSES beside the
      -- board so the same call answers both "what happened" and "who was asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Maybe (GameState.GameState, [Response.Response])
      run answer portId gs = case returnAbility portId gs of
        [ability] ->
          let ((_, after), responses) = Replay.record answer gs (Activate.activateAbility S.alice portId ability >> Stack.resolveTop)
           in Just (after, responses)
        _ -> Nothing
      named = Just . CardName.MkCardName . Text.pack
      -- The WHOLE battlefield minus the basic lands: after the ability resolves
      -- that is carol's Benalish Hero, which nothing may move, and whatever came
      -- back -- the Port sacrificed itself to pay for the ability. By NAME, TAP
      -- STATE and CONTROLLER, because CR 400.7 mints a fresh id at the
      -- destination and CR 110.2a is what decides whose the arrival is.
      arrivals gs =
        List.sort
          ( fmap
              (\oid -> (fmap S.nameOf (Game.cardOf oid gs), fmap Object.tapped (Game.lookupObject oid gs), Projection.controllerOf oid gs))
              (filter (\oid -> notElem (fmap S.nameOf (Game.cardOf oid gs)) [named "Swamp", named "Island"]) (Set.toList (GameState.battlefield gs)))
          )
      -- The board with nothing returned: carol's creature and nothing else.
      untouched = [(named "Benalish Hero", Just TapState.Untapped, Just S.carol)]
      wasAsked responses =
        let isChoice r = case r of
              Response.ChoseCardInGraveyard _ -> True
              _ -> False
         in any isChoice responses
   in Spec.describe s "PortOfKarfell" $ do
        -- The headline: the SECOND buried creature card comes back, tapped, and
        -- everything else stays where it was.
        Spec.it s "CR 608.2d the creature card the controller chose returns tapped" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice), (maiden, S.alice), (sentry, S.bob), (cavalry, S.carol)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
              -- The Bird Maiden's id, which is the SECOND of alice's two
              -- creature cards in ascending order -- graveyardCards' own order,
              -- and the order the prompt offers.
              maidenId = case Game.zoneMembers Zone.Graveyard S.alice gs of
                [_, _, third] -> Just third
                _ -> Nothing
              choosing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
              choosing wanted p = case p of
                Prompt.ChooseCardInGraveyard {} -> wanted
                _ -> S.identityAnswer p
          case (maidenId, maidenId >>= \wanted -> run (choosing wanted) portId gs) of
            (Just _, Just (after, responses)) -> do
              Spec.assertBool s (wasAsked responses) "the controller was asked which card to return"
              Spec.assertEqWith
                s
                "the Bird Maiden is on alice's battlefield, tapped, and nothing else arrived"
                (arrivals after)
                (List.sort ((named "Bird Maiden", Just TapState.Tapped, Just S.alice) : untouched))
              Spec.assertEqWith
                s
                "the unchosen creature card, the noncreature card, the four milled Swamps and the spent land stay in alice's graveyard"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort ([named "Goblin Piker", named "Murder", named "Port of Karfell"] <> replicate 4 (named "Swamp")))
              Spec.assertEqWith
                s
                "and neither opponent's graveyard was touched"
                (namesIn Zone.Graveyard S.bob after, namesIn Zone.Graveyard S.carol after)
                ([named "Ogre Sentry"], [named "Benalish Cavalry"])
            _ -> Spec.assertBool s False "expected exactly one returning ability and three cards in alice's graveyard"
        -- The paired control, and the whole reason the board buries TWO creature
        -- cards: the same activation on the same board with the DEFAULT answerer
        -- brings back the other one. If the engine were picking, both legs would
        -- name the same card.
        Spec.it s "CR 608.2d the engine does not pick: another answer returns the other card" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice), (maiden, S.alice)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, _) ->
              Spec.assertEqWith
                s
                "the first candidate comes back instead"
                (arrivals after)
                (List.sort ((named "Goblin Piker", Just TapState.Tapped, Just S.alice) : untouched))
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- Where the rules leave nothing to ask, don't prompt: one matching card
        -- is the whole of "a creature card in your graveyard". The board differs
        -- from the leg above in the Bird Maiden and nothing else.
        Spec.it s "one candidate elides the prompt and still returns the card" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (murder, S.alice)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith
                s
                "the lone candidate came back anyway"
                (arrivals after)
                (List.sort ((named "Goblin Piker", Just TapState.Tapped, Just S.alice) : untouched))
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- CR 101.3 and CR 609.3: a graveyard with nothing matching makes the
        -- instruction impossible, so it is ignored -- and nobody is asked. The
        -- mill still happens, which is what keeps this from passing because the
        -- ability never resolved at all.
        Spec.it s "CR 101.3 no matching card returns nothing and asks nothing" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(murder, S.alice), (sentry, S.bob)]
              (portId, gs) = board port swamp island hero buried (replicate 4 swamp)
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith s "nothing arrived, and carol keeps the creature she controls" (arrivals after) untouched
              Spec.assertEqWith
                s
                "the mill still ran, so the ability really did resolve"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort ([named "Murder", named "Port of Karfell"] <> replicate 4 (named "Swamp")))
              Spec.assertEqWith s "and the opponent's creature card is not a candidate" (namesIn Zone.Graveyard S.bob after) [named "Ogre Sentry"]
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"
        -- An EMPTY graveyard and an empty library: the ability resolves, mills
        -- nothing (CR 701.17b), and returns nothing.
        Spec.it s "CR 609.3 an empty graveyard is a no-op rather than a failure" $ do
          port <- S.printingOf s registry "Port of Karfell"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          hero <- S.printingOf s registry "Benalish Hero"
          let (portId, gs) = board port swamp island hero [] []
          case run S.identityAnswer portId gs of
            Just (after, responses) -> do
              Spec.assertBool s (not (wasAsked responses)) "no choice was put to the player"
              Spec.assertEqWith s "nothing arrived, and carol keeps the creature she controls" (arrivals after) untouched
              Spec.assertEqWith s "only the land that paid for the ability is in the graveyard" (namesIn Zone.Graveyard S.alice after) [named "Port of Karfell"]
            Nothing -> Spec.assertBool s False "expected exactly one returning ability"

-- CR 400.1's WHOSE said by a SLOT for a resolution-time CHOICE: the graveyard
-- the candidates come from is the one this spell's own target names, where
-- portOfKarfellSpec above says "your graveyard" and exhumeSpec below says "each
-- player's".
--
-- Grasping Tentacles {1}{U}{B} Sorcery, "Target opponent mills eight cards. You
-- may put an artifact card from that player's graveyard onto the battlefield
-- under your control." (name, cost, type line and Oracle text checked against
-- api.scryfall.com, 2026-09-01). The whole card is transcribed: "under your
-- control" is CR 110.2a's default for a card the effect's controller puts onto
-- the battlefield, so it states no rider, and the "may" is a CR 608.2d choice
-- scoped to the second clause.
--
-- THE CHOOSER IS NOT THE SCOPE, which is the pair no arm could state before:
-- alice chooses (CR 608.2d) out of a graveyard that is not hers, and the seat
-- whose graveyard it is comes from the slot the FIRST clause targeted.
--
-- NOT A TARGET, portOfKarfellSpec's distinction: the card says "target" of the
-- opponent alone, so CR 115.1a leaves the artifact card unannounced and CR
-- 608.2b has nothing to re-check about it.
--
-- THREE SEATS, with an artifact card in every graveyard a wider reading would
-- reach: alice's own (which "your graveyard" would take), carol's (which "each
-- opponent's" or "each player's" would take) and bob's two. The two legs below
-- pin their answer to the LAST and the FIRST candidate offered, and
-- Resolve.zoneScopePlayers offers them in APNAP order (CR 101.4), so a scope
-- wider than the slot hands back carol's card on one leg and alice's on the
-- other rather than bob's on both.
--
-- Bob's eight milled Swamps are the filter's other half -- cards in the very
-- graveyard the choice reads that "artifact card" must leave standing -- and the
-- witness that both clauses read the SAME slot.
graspingTentaclesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graspingTentaclesSpec s registry =
  let -- alice holds the spell and has six lands for its {1}{U}{B}, a payment
      -- that taps one source at a time having no slack of its own; `buried` goes
      -- into the named graveyards in the order given, and bob's library is
      -- stocked with `stock`. Returns the spell's id.
      board tentacles island swamp buried stock =
        let mana = S.landsFor island S.alice 3 (S.landsFor swamp S.alice 3 S.threePlayerGame)
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) mana buried
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withGraves stock
            (withSpell, spell) = S.handOne tentacles withStock
         in (spell, withSpell {GameState.priority = Just S.alice})
      -- Cast and resolve, keeping the RESPONSES beside the board so the same call
      -- answers both "what came back" and "was anybody asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Response.Response])
      run answer spell gs =
        let ((_, after), responses) = Replay.record answer gs (S.cast S.alice spell >> Stack.resolveTop)
         in (after, responses)
      -- The offered set is FILTERED rather than rebuilt, so the target is a
      -- recipient the engine itself minted (CR 608.2b).
      answering :: (NonEmpty.NonEmpty ObjectId.ObjectId -> ObjectId.ObjectId) -> Prompt.Prompt r -> r
      answering pick p = case p of
        Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseCardInGraveyard _ _ _ offered _ -> pick offered
        _ -> S.identityAnswer p
      named = Just . CardName.MkCardName . Text.pack
      -- The whole battlefield minus alice's six lands, by NAME and CONTROLLER:
      -- CR 400.7 mints a fresh id at the destination, and CR 110.2a is what
      -- decides whose the arrival is.
      arrivals gs =
        List.sort
          ( fmap
              (\oid -> (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs))
              ( filter
                  (\oid -> notElem (fmap S.nameOf (Game.cardOf oid gs)) [named "Island", named "Swamp"])
                  (Set.toList (GameState.battlefield gs))
              )
          )
      wasAsked responses =
        let isChoice r = case r of
              Response.ChoseCardInGraveyard _ -> True
              _ -> False
         in any isChoice responses
   in Spec.describe s "GraspingTentacles" $ do
        -- The headline: the LAST candidate is bob's second artifact card, not
        -- carol's, so the scope is the slot rather than the table.
        Spec.it s "CR 608.2d the candidates come from the graveyard the target slot names" $ do
          tentacles <- S.printingOf s registry "Grasping Tentacles"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          medallion <- S.printingOf s registry "Sapphire Medallion"
          meekstone <- S.printingOf s registry "Meekstone"
          heartstone <- S.printingOf s registry "Heartstone"
          crucible <- S.printingOf s registry "Crucible of Worlds"
          let buried = [(medallion, S.alice), (meekstone, S.bob), (heartstone, S.bob), (crucible, S.carol)]
              (spell, gs) = board tentacles island swamp buried (replicate 8 swamp)
              (after, responses) = run (answering NonEmpty.last) spell gs
          Spec.assertEqWith
            s
            "bob's last artifact card is on alice's battlefield: carol's, which a wider scope would have offered last, is not"
            (arrivals after)
            [(named "Heartstone", Just S.alice)]
          Spec.assertEqWith
            s
            "alice's and carol's own artifact cards were never candidates and are still buried"
            (List.sort (namesIn Zone.Graveyard S.alice after), namesIn Zone.Graveyard S.carol after)
            (List.sort [named "Grasping Tentacles", named "Sapphire Medallion"], [named "Crucible of Worlds"])
          Spec.assertEqWith
            s
            "bob's unchosen artifact and his eight milled Swamps stay in his graveyard, and his library is empty"
            (List.sort (namesIn Zone.Graveyard S.bob after), namesIn Zone.Library S.bob after)
            (List.sort (named "Meekstone" : replicate 8 (named "Swamp")), [])
          Spec.assertBool s (wasAsked responses) "alice was asked which card to take"
        -- The paired control, and the whole reason bob buries TWO artifact cards:
        -- the same cast on the same board answered with the FIRST candidate takes
        -- the other one. A wider scope would offer alice's own Medallion first,
        -- so this leg fails on the same reading the one above does -- and if the
        -- engine were picking, both legs would name one card.
        Spec.it s "CR 608.2d the engine does not pick: another answer takes bob's other artifact card" $ do
          tentacles <- S.printingOf s registry "Grasping Tentacles"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          medallion <- S.printingOf s registry "Sapphire Medallion"
          meekstone <- S.printingOf s registry "Meekstone"
          heartstone <- S.printingOf s registry "Heartstone"
          crucible <- S.printingOf s registry "Crucible of Worlds"
          let buried = [(medallion, S.alice), (meekstone, S.bob), (heartstone, S.bob), (crucible, S.carol)]
              (spell, gs) = board tentacles island swamp buried (replicate 8 swamp)
              (after, _) = run (answering NonEmpty.head) spell gs
          Spec.assertEqWith
            s
            "bob's first artifact card comes instead, where alice's own Medallion is what a wider scope would have offered first"
            (arrivals after)
            [(named "Meekstone", Just S.alice)]
        -- The "may", declined: the mill is a clause of its own and stands.
        Spec.it s "CR 608.2d a declined may leaves the mill done and nothing taken" $ do
          tentacles <- S.printingOf s registry "Grasping Tentacles"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          meekstone <- S.printingOf s registry "Meekstone"
          heartstone <- S.printingOf s registry "Heartstone"
          let buried = [(meekstone, S.bob), (heartstone, S.bob)]
              (spell, gs) = board tentacles island swamp buried (replicate 8 swamp)
              declining p = case p of
                Prompt.ChooseTargets _ _ _ slots -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) slots
                _ -> S.identityAnswer p
              (after, responses) = run declining spell gs
          Spec.assertEqWith s "nothing arrived on any battlefield" (arrivals after) []
          Spec.assertEqWith
            s
            "and bob's eight milled Swamps joined the two artifact cards he had buried"
            (List.sort (namesIn Zone.Graveyard S.bob after))
            (List.sort ([named "Heartstone", named "Meekstone"] <> replicate 8 (named "Swamp")))
          Spec.assertBool s (not (wasAsked responses)) "the declined may asked nothing about which card"

-- CR 701.17c's "from among them", which is the group read Filter.IsBound could
-- not do: a slot bound to the WHOLE batch a mill put in the graveyard, named by a
-- later clause's filter over candidates that batch does not exhaust.
--
-- Midnight Tilling {1}{G} Instant, "Mill four cards, then you may return a
-- permanent card from among them to your hand." (name, cost, type line and
-- Oracle text checked against api.scryfall.com, 2026-08-20). The whole card is
-- transcribed; "a permanent card" is CR 110.4a's six card types written out as an
-- Or, there being no atom that says it in one word.
--
-- Corpse Churn is the printing the pool already had that separates the two
-- readings -- "Mill three cards, then you may return a creature card FROM YOUR
-- GRAVEYARD to your hand", the same sentence with the batch swapped for the
-- zone. So the board buries a permanent card BEFORE the mill: every other clause
-- of Midnight Tilling's sentence admits it, and only "from among them" keeps it
-- out. A reading that ignored the slot would offer four candidates where this one
-- offers three, and the answers below are pinned by INDEX into the offer, so the
-- two readings hand back different cards rather than the same one.
--
-- The milled Murder is the type half of the same filter, kept honest by a batch
-- that is not all permanent cards; bob's buried Ogre Sentry is CR 400.1's other
-- graveyard, which "your" excludes.
midnightTillingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
midnightTillingSpec s registry =
  let -- alice: four Forests for the {1}{G}, one permanent card already in her
      -- graveyard, `stock` into her library BOTTOM FIRST (S.addLibraryCard puts
      -- each new card on top), Midnight Tilling in hand. bob buries one card of
      -- his own. Returns the spell's id.
      board forest tilling decoy sentry stock =
        let mana = S.landsFor forest S.alice 4 S.threePlayerGame
            (_, withDecoy) = S.addGraveyardCard decoy S.alice mana
            (_, withTheirs) = S.addGraveyardCard sentry S.bob withDecoy
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) withTheirs stock
            (withSpell, spellId) = S.handOne tilling withStock
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      -- The candidates in the order the prompt offers them, which is
      -- Resolve.graveyardCardsOf's ascending ObjectId -- and, the mill having
      -- minted fresh ids in milling order (CR 400.7), the order the cards were
      -- milled in. Pinned by index: an answerer that went looking for a legal
      -- card would find one again under either reading.
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- Takes the printed "may" -- clause 1, the return; clause 0 is the mandatory
      -- mill -- and answers the graveyard choice with the nth card offered.
      taking :: Int -> Prompt.Prompt r -> r
      taking n p = case p of
        Prompt.ChooseOptional _ _ _ _ clause _
          | clause == ClauseIndex.MkClauseIndex 1 -> OptionalDecision.Exercises
        Prompt.ChooseCardInGraveyard _ _ _ offered _ -> nth n offered
        _ -> S.identityAnswer p
      cast :: (forall r. Prompt.Prompt r -> r) -> (ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
      cast answer (spellId, gs) =
        let announced = S.runPure answer gs (S.cast S.alice spellId)
         in S.runPure answer announced Stack.resolveTop
      setup = do
        forest <- S.printingOf s registry "Forest"
        tilling <- S.printingOf s registry "Midnight Tilling"
        hero <- S.printingOf s registry "Benalish Hero"
        sentry <- S.printingOf s registry "Ogre Sentry"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        maiden <- S.printingOf s registry "Bird Maiden"
        murder <- S.printingOf s registry "Murder"
        piker <- S.printingOf s registry "Goblin Piker"
        -- Bottom to top: the Island is never reached, and the top four are milled
        -- in the order Goblin Piker, Murder, Bird Maiden, Swamp.
        pure (board forest tilling hero sentry [island, swamp, maiden, murder, piker])
      -- What stays behind when nothing is returned: the card buried before the
      -- mill, all four milled cards, and the spell itself (CR 608.2n).
      allBuried = List.sort ([named "Benalish Hero", named "Bird Maiden", named "Goblin Piker", named "Midnight Tilling", named "Murder"] <> [named "Swamp"])
   in Spec.describe s "MidnightTilling" $ do
        -- The headline, and the case the whole unit exists for: the SECOND card
        -- the offer names is the second MILLED permanent card, not the second
        -- permanent card in the graveyard.
        Spec.it s "CR 701.17c the return chooses among the milled cards, not among the graveyard" $ do
          gs <- setup
          let after = cast (taking 1) gs
          Spec.assertEqWith s "the second milled permanent card is the one in alice's hand" (namesIn Zone.Hand S.alice after) [named "Bird Maiden"]
          Spec.assertEqWith
            s
            "the permanent card buried before the mill was never a candidate, and neither was the milled Murder"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Bird Maiden") allBuried)
          Spec.assertEqWith s "and the other graveyard was not looked in" (namesIn Zone.Graveyard S.bob after) [named "Ogre Sentry"]
        -- CR 608.2d: four milled Murders leave no permanent card among them, so
        -- the return is impossible and the "may" is not put -- though the
        -- graveyard holds the Benalish Hero buried before the mill, which a pool
        -- read ignoring "from among them" would count. The cases above, which
        -- take the "may", are the control.
        Spec.it s "CR 608.2d the return is not offered when no permanent card was milled" $ do
          forest <- S.printingOf s registry "Forest"
          tilling <- S.printingOf s registry "Midnight Tilling"
          hero <- S.printingOf s registry "Benalish Hero"
          sentry <- S.printingOf s registry "Ogre Sentry"
          island <- S.printingOf s registry "Island"
          murder <- S.printingOf s registry "Murder"
          let (spellId, gs) = board forest tilling hero sentry [island, murder, murder, murder, murder]
              answer :: Prompt.Prompt r -> r
              answer = taking 0
              ((_, after), asked) = Replay.record answer gs (S.cast S.alice spellId >> Stack.resolveTop)
          Spec.assertEqWith s "CR 608.2d the may was never put" [d | Response.ChoseOptional d <- asked] []
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith s "and the four Murders were milled" (length (filter (== named "Murder") (namesIn Zone.Graveyard S.alice after))) 4

-- CR 701.20e's "from among them" over a group that never left the LIBRARY, which
-- is the read no zone-keyed ObjectRef can do: ObjectRef.ChosenCardFromAmong.
--
-- Commune with the Gods {1}{G} Sorcery, "Reveal the top five cards of your
-- library. You may put a creature or enchantment card from among them into your
-- hand. Put the rest into your graveyard." (name, cost, type line and Oracle text
-- checked against api.scryfall.com, 2026-08-20). The whole card is transcribed.
--
-- Three clauses, and the middle one is this unit: the reveal binds the five as a
-- group and leaves them where they are (CR 701.20b), the choice picks one of them
-- by the card's own filter, and "the rest" is the SAME slot read by
-- ObjectRef.InSlot -- which finds the chosen card gone, CR 400.7 having minted a
-- new object for it on the way to the hand.
--
-- The library is stocked so that the offer and the group differ: an Island and a
-- Murder sit among the five and match neither card type, so a reading that
-- ignored the filter would offer five cards where this one offers three. The
-- answers below are pinned by INDEX into the offer, so the two readings hand back
-- different cards rather than the same one. A Swamp sits SIXTH, below the five, so
-- a reveal of the wrong depth is visible in the library as well as in the
-- graveyard.
communeWithTheGodsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
communeWithTheGodsSpec s registry =
  let -- alice: two Forests for the {1}{G}, `stock` into her library BOTTOM FIRST
      -- (S.addLibraryCard puts each new card on top), Commune with the Gods in
      -- hand. Returns the spell's id.
      board forest commune stock =
        let mana = S.landsFor forest S.alice 2 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            (withSpell, spellId) = S.handOne commune withStock
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      -- The candidates in the order the prompt offers them: the group's own mint
      -- order, which for a reveal of the top five is the library's, top first (CR
      -- 401.2). Pinned by index, since an answerer that went looking for a legal
      -- card would find one again under either reading.
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- Takes the printed "may" -- clause 1, the move to hand; clause 0 is the
      -- reveal and clause 2 the rest -- and answers the group choice with the nth
      -- card offered.
      taking :: Int -> Prompt.Prompt r -> r
      taking n p = case p of
        Prompt.ChooseOptional _ _ _ _ clause _
          | clause == ClauseIndex.MkClauseIndex 1 -> OptionalDecision.Exercises
        Prompt.ChooseCardFromAmong _ _ _ offered -> nth n offered
        _ -> S.identityAnswer p
      cast :: (forall r. Prompt.Prompt r -> r) -> (ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
      cast answer (spellId, gs) =
        let announced = S.runPure answer gs (S.cast S.alice spellId)
         in S.runPure answer announced Stack.resolveTop
      setup = do
        forest <- S.printingOf s registry "Forest"
        commune <- S.printingOf s registry "Commune with the Gods"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        maiden <- S.printingOf s registry "Bird Maiden"
        moon <- S.printingOf s registry "Bad Moon"
        murder <- S.printingOf s registry "Murder"
        piker <- S.printingOf s registry "Goblin Piker"
        -- Bottom to top: the Swamp is never revealed, and the five above it are
        -- Island, Goblin Piker, Murder, Bad Moon, Bird Maiden -- so the offer is
        -- Goblin Piker, Bad Moon, Bird Maiden.
        pure (board forest commune [swamp, maiden, moon, murder, piker, island])
      -- What the graveyard holds when nothing is taken: all five revealed cards
      -- and the spell itself (CR 608.2n).
      allBuried = List.sort [named "Bad Moon", named "Bird Maiden", named "Commune with the Gods", named "Goblin Piker", named "Island", named "Murder"]
   in Spec.describe s "CommuneWithTheGods" $ do
        -- The headline: the SECOND card the offer names is the second revealed
        -- card matching the filter, not the second revealed card.
        Spec.it s "CR 701.20e the choice ranges over the matching revealed cards, not over all five" $ do
          gs <- setup
          let after = cast (taking 1) gs
          Spec.assertEqWith s "the second matching revealed card is the one in alice's hand" (namesIn Zone.Hand S.alice after) [named "Bad Moon"]
          Spec.assertEqWith
            s
            "and the rest -- the two unmatched cards included -- are in the graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.delete (named "Bad Moon") allBuried)
          Spec.assertEqWith s "the sixth card was never revealed" (namesIn Zone.Library S.alice after) [named "Swamp"]
        -- CR 608.2d: a group holding no matching card makes the move impossible,
        -- so the "may" is not put and the rest is all of it. The pair with the
        -- headline differs in exactly one thing -- which cards are stocked.
        Spec.it s "CR 608.2d a group with no matching card is not offered and buries all five" $ do
          forest <- S.printingOf s registry "Forest"
          commune <- S.printingOf s registry "Commune with the Gods"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          let (spellId, gs) = board forest commune [swamp, murder, island, murder, island, murder]
              answer :: Prompt.Prompt r -> r
              answer = taking 0
              ((_, after), asked) = Replay.record answer gs (S.cast S.alice spellId >> Stack.resolveTop)
          Spec.assertEqWith s "CR 608.2d the may was never put" [d | Response.ChoseOptional d <- asked] []
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith
            s
            "and all five revealed cards are in the graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.sort [named "Commune with the Gods", named "Island", named "Island", named "Murder", named "Murder", named "Murder"])

-- ObjectRef.ChosenCardInGraveyard's COUNT: one gather taking more than one card
-- out of each named graveyard, where portOfKarfellSpec and graspingTentaclesSpec
-- above take one. ancestralMemoriesSpec below is the same count over a bound
-- group rather than a zone.
--
-- Fall of the Thran {5}{W} Enchantment - Saga, "I -- Destroy all lands. II, III
-- -- Each player returns two land cards from their graveyard to the
-- battlefield." (name, cost, type line and Oracle text checked against
-- api.scryfall.com, 2026-09-14). The whole card is transcribed; CR 714.2c's "II,
-- III --" shorthand is written as the two abilities that rule says it means, and
-- "to the battlefield" with no controller named goes to the player the effect
-- instructed (CR 110.2a) -- here each player over their own graveyard, which is
-- the card's owner (CR 400.3) and what the underOwner rider states.
--
-- THREE SEATS, each with THREE land cards and a creature card buried: the count
-- is two, so every seat has a real choice (an offer equal to the count would
-- elide the second ask and the case would pass whatever the engine did), and the
-- creature card is the filter's witness in the very graveyard the choice reads.
--
-- Driven through the turn-based action rather than a cast, which is what
-- advanceSpec in Pawl.SagaSpec does: the Saga enters with one lore counter
-- already on it, CR 714.3c adds the second, and the chapter II trigger that
-- crossing fires is what resolves. The asks are pinned by INDEX through a
-- State-threaded answerer, since a pure one cannot tell a seat's second ask from
-- its first.
fallOfTheThranSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fallOfTheThranSpec s registry =
  let -- Fall of the Thran on alice's battlefield with chapter I already behind
      -- it, and `buried` into the named graveyards in the order given. The
      -- precombat main phase is alice's, so CR 714.3c's action is hers to take.
      board thran buried =
        let (oid, base) = S.addPermanent thran S.alice S.threePlayerGame
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) base buried
         in ( oid,
              (S.addCounter CounterKind.Lore 1 oid withGraves)
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      named = Just . CardName.MkCardName . Text.pack
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- One index per ask, taken in order, and the SIZE of each offer recorded
      -- beside it -- so a second ask that never happened and a second ask over
      -- the wrong candidates are both visible.
      taking :: Prompt.Prompt r -> State.State ([Int], [Int]) r
      taking p = case p of
        Prompt.ChooseCardInGraveyard _ _ _ offered _ -> do
          (indices, sizes) <- State.get
          State.put (drop 1 indices, sizes <> [length (NonEmpty.toList offered)])
          pure (nth (Maybe.fromMaybe 0 (Maybe.listToMaybe indices)) offered)
        _ -> pure (S.identityAnswer p)
      -- CR 714.3c's counter, then the priority round that resolves the chapter
      -- ability it fired.
      advance :: [Int] -> (ObjectId.ObjectId, GameState.GameState) -> (GameState.GameState, [Int])
      advance script (_, gs) =
        let ((_, advanced), afterTba) = State.runState (Engine.runGame taking gs (Engine.runTurnBasedActions Phase.PrecombatMain)) (script, [])
            ((_, after), afterLoop) = State.runState (Engine.runGame taking advanced Engine.priorityLoop) afterTba
         in (after, snd afterLoop)
      -- Every permanent on the battlefield by NAME and CONTROLLER (CR 110.2a),
      -- which is what a return "to the battlefield" has to be read by: CR 400.7
      -- mints a fresh id at the destination.
      onBattlefield gs =
        List.sort
          ( fmap
              (\oid -> (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs))
              (Set.toList (GameState.battlefield gs))
          )
      setup = do
        thran <- S.printingOf s registry "Fall of the Thran"
        plains <- S.printingOf s registry "Plains"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        hero <- S.printingOf s registry "Benalish Hero"
        pure
          ( board
              thran
              [ (plains, S.alice),
                (island, S.alice),
                (swamp, S.alice),
                (hero, S.alice),
                (forest, S.bob),
                (mountain, S.bob),
                (plains, S.bob),
                (hero, S.bob),
                (island, S.carol),
                (swamp, S.carol),
                (forest, S.carol),
                (hero, S.carol)
              ]
          )
   in Spec.describe s "FallOfTheThran" $ do
        -- The headline: TWO cards come back per seat, and they are DIFFERENT
        -- cards. Every ask takes index 0, so an implementation whose second ask
        -- re-offered the card the first took would name one card twice and return
        -- one land per seat (CR 400.7 retiring the id the first move minted).
        Spec.it s "CR 608.2d each player returns two DISTINCT land cards from their own graveyard" $ do
          gs <- setup
          let (after, sizes) = advance [0, 0, 0, 0, 0, 0] gs
          Spec.assertEqWith
            s
            "two lands each, under their own owners, beside the Saga"
            (onBattlefield after)
            ( List.sort
                [ (named "Fall of the Thran", Just S.alice),
                  (named "Plains", Just S.alice),
                  (named "Island", Just S.alice),
                  (named "Forest", Just S.bob),
                  (named "Mountain", Just S.bob),
                  (named "Island", Just S.carol),
                  (named "Swamp", Just S.carol)
                ]
            )
          Spec.assertEqWith
            s
            "each graveyard keeps its third land and its creature card"
            (namesIn Zone.Graveyard S.alice after, namesIn Zone.Graveyard S.bob after, namesIn Zone.Graveyard S.carol after)
            ([named "Swamp", named "Benalish Hero"], [named "Plains", named "Benalish Hero"], [named "Forest", named "Benalish Hero"])
          Spec.assertEqWith s "six asks in APNAP order, each seat's second over one fewer candidate" sizes [3, 2, 3, 2, 3, 2]
        -- The paired control, on the same board: other answers take other cards,
        -- so the engine is not picking. Index 2 of alice's untouched three is the
        -- Swamp, and index 1 of what its removal leaves is the Island.
        Spec.it s "CR 608.2d the engine does not pick: other answers return the other lands" $ do
          gs <- setup
          let (after, _) = advance [2, 1, 2, 1, 2, 1] gs
          Spec.assertEqWith
            s
            "each seat's third and second land came back instead"
            (onBattlefield after)
            ( List.sort
                [ (named "Fall of the Thran", Just S.alice),
                  (named "Swamp", Just S.alice),
                  (named "Island", Just S.alice),
                  (named "Plains", Just S.bob),
                  (named "Mountain", Just S.bob),
                  (named "Forest", Just S.carol),
                  (named "Swamp", Just S.carol)
                ]
            )
        -- CR 609.3: a graveyard holding fewer matching cards than the count gives
        -- what it has, and the rest of the instruction is performed on that (CR
        -- 101.3). One candidate elides the ask entirely and none skips it, so
        -- neither seat consumes an index.
        Spec.it s "CR 609.3 one land card gives one, and an empty graveyard gives none" $ do
          thran <- S.printingOf s registry "Fall of the Thran"
          plains <- S.printingOf s registry "Plains"
          hero <- S.printingOf s registry "Benalish Hero"
          let (after, sizes) = advance [] (board thran [(plains, S.alice), (hero, S.bob)])
          Spec.assertEqWith
            s
            "alice's one land came back and bob's creature card stayed put"
            (onBattlefield after)
            (List.sort [(named "Fall of the Thran", Just S.alice), (named "Plains", Just S.alice)])
          Spec.assertEqWith s "and nobody was asked anything" sizes []

-- ObjectRef.ChosenCardFromAmong's COUNT: "from among them" taking more than one
-- card out of one bound group, where communeWithTheGodsSpec above takes one.
--
-- Ancestral Memories {2}{U}{U}{U} Sorcery, "Look at the top seven cards of your
-- library. Put two of them into your hand and the rest into your graveyard."
-- (name, cost, type line and Oracle text checked against api.scryfall.com,
-- 2026-09-01). The whole card is transcribed.
--
-- Three clauses: CR 701.20e's look binds the seven as a group and leaves them in
-- the library (rule 701.20b), the choice takes two of them, and "the rest" is the
-- SAME slot read by ObjectRef.InSlot -- which finds both chosen cards gone, CR
-- 400.7 having minted new objects for them on the way to the hand.
--
-- The two asks are pinned by INDEX through a State-threaded answerer, since a
-- pure one cannot tell the second ask from the first. Both legs below index 5 and
-- 0 of the offers, in the two orders, and the second leg is what proves the
-- EXCLUSION: index 5 of the untouched seven is the Bad Moon, and index 5 of the
-- six the first ask left is the Bird Maiden, so an implementation that re-offered
-- the taken card would name a different pair. An eighth card sits below the seven
-- so the look's own depth stays observable.
ancestralMemoriesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ancestralMemoriesSpec s registry =
  let -- alice: five Islands for the {2}{U}{U}{U}, `stock` into her library BOTTOM
      -- FIRST (S.addLibraryCard puts each new card on top), Ancestral Memories in
      -- hand. Returns the spell's id.
      board island memories stock =
        let mana = S.landsFor island S.alice 5 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            (withSpell, spellId) = S.handOne memories withStock
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- One index per ask, taken in order, and the SIZE of each offer recorded
      -- beside it -- so a second ask that never happened and a second ask over the
      -- wrong candidates are both visible.
      taking :: Prompt.Prompt r -> State.State ([Int], [Int]) r
      taking p = case p of
        Prompt.ChooseCardFromAmong _ _ _ offered -> do
          (indices, sizes) <- State.get
          State.put (drop 1 indices, sizes <> [length (NonEmpty.toList offered)])
          pure (nth (Maybe.fromMaybe 0 (Maybe.listToMaybe indices)) offered)
        _ -> pure (S.identityAnswer p)
      cast :: [Int] -> (ObjectId.ObjectId, GameState.GameState) -> (GameState.GameState, [Int])
      cast script (spellId, gs) =
        let ((_, announced), afterCast) = State.runState (Engine.runGame taking gs (S.cast S.alice spellId)) (script, [])
            ((_, after), afterResolve) = State.runState (Engine.runGame taking announced Stack.resolveTop) afterCast
         in (after, snd afterResolve)
      setup = do
        island <- S.printingOf s registry "Island"
        memories <- S.printingOf s registry "Ancestral Memories"
        swamp <- S.printingOf s registry "Swamp"
        maiden <- S.printingOf s registry "Bird Maiden"
        moon <- S.printingOf s registry "Bad Moon"
        murder <- S.printingOf s registry "Murder"
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Hill Giant"
        forest <- S.printingOf s registry "Forest"
        mountain <- S.printingOf s registry "Mountain"
        -- Bottom to top: the Swamp is never looked at, and the seven above it are
        -- Mountain, Forest, Hill Giant, Goblin Piker, Murder, Bad Moon, Bird
        -- Maiden read top down, which is the order the offer takes.
        pure (board island memories [swamp, maiden, moon, murder, piker, giant, forest, mountain])
      -- The graveyard when nothing is taken: the seven looked-at cards and the
      -- spell itself (CR 608.2n).
      allBuried =
        List.sort
          [ named "Ancestral Memories",
            named "Bad Moon",
            named "Bird Maiden",
            named "Forest",
            named "Goblin Piker",
            named "Hill Giant",
            named "Mountain",
            named "Murder"
          ]
      burying takens = List.sort (List.foldl' (flip List.delete) allBuried takens)
   in Spec.describe s "AncestralMemories" $ do
        -- The headline: TWO cards come out of the one group, and both are the ones
        -- the answers named.
        Spec.it s "CR 608.2d two cards are taken from among the seven, both of them chosen" $ do
          gs <- setup
          let (after, sizes) = cast [5, 0] gs
          Spec.assertEqWith
            s
            "both chosen cards are in alice's hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Bad Moon", named "Mountain"])
          Spec.assertEqWith
            s
            "and the other five are in the graveyard"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (burying [named "Bad Moon", named "Mountain"])
          Spec.assertEqWith s "the eighth card was never looked at" (namesIn Zone.Library S.alice after) [named "Swamp"]
          Spec.assertEqWith s "two asks, the second over one fewer candidate" sizes [7, 6]
        -- The paired control, and the proof that the second ask cannot re-offer the
        -- first ask's card: the SAME two indices in the other order. Index 5 of the
        -- untouched seven is the Bad Moon; index 5 of what the Mountain's removal
        -- leaves is the Bird Maiden.
        Spec.it s "CR 608.2d the second choice is made among the cards the first left" $ do
          gs <- setup
          let (after, _) = cast [0, 5] gs
          Spec.assertEqWith
            s
            "the Mountain and the Bird Maiden are in alice's hand"
            (List.sort (namesIn Zone.Hand S.alice after))
            (List.sort [named "Bird Maiden", named "Mountain"])
          Spec.assertEqWith
            s
            "and the Bad Moon is among the rest"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (burying [named "Bird Maiden", named "Mountain"])

-- The counted reveal-until walk: ObjectRef.TopOfLibraryUntil's Quantity counting
-- MATCHES, where data/scenarios/mass-effect pins the same walk at one match
-- (Treasure Hunt) and the split of what it bound (Mulch). The three halves of Open the Way's
-- sentence are those two arms plus CR 401.4's random bottoming enduranceSpec
-- pins, and it is the only card in `data/cards/` that writes all three at once.
--
-- Open the Way {X}{G}{G} Sorcery, "X can't be greater than the number of players
-- in the game. / Reveal cards from the top of your library until you reveal X
-- land cards. Put those land cards onto the battlefield tapped and the rest on
-- the bottom of your library in a random order." (name, cost, type line and
-- Oracle text checked against api.scryfall.com, 2026-08-20). The whole card is
-- transcribed: the first sentence is Face.maximumX, and the second is three
-- clauses -- CR 701.20a's reveal binding the walked cards as a group, the
-- matching half moved to the battlefield tapped, and "the rest" as
-- ObjectRef.InSlot over the SAME slot, which finds the lands gone because CR
-- 400.7 minted new objects for them on the battlefield.
--
-- THREE SEATS, and they are load-bearing twice over. CR 101.1's ceiling IS the
-- seat count, so a two-seat board could not tell an X of 3 that the card refuses
-- from one it permits; and "your library" must not collapse onto the table's, so
-- bob's library is stocked with cards that would all have matched.
--
-- THE CEILING IS READ ONCE, at CR 601.2b's announcement, and never again --
-- Face.maximumX has no resolution-time reader at all. The departure pair below
-- is what makes that observable: carol leaving with the spell already on the
-- stack does not shrink the X alice announced, where carol leaving BEFORE the
-- cast refuses that same X.
--
-- THE RANDOMNESS IS THE ANSWERER'S, as it is for Endurance above: the engine
-- rolls nothing, it asks Prompt.Shuffle, so the fixture's permutation names the
-- resulting library. The answer ROTATES the batch, so it is neither the batch's
-- own order nor its reverse -- an engine that ignored the answer, and one that
-- handed CR 401.4's arrangement to the owner, each leave a different library.
openTheWaySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
openTheWaySpec s registry =
  let -- alice: seven Forests, so {4}{G}{G} is as affordable as {3}{G}{G} and
      -- nothing below turns on mana; `stock` into her library BOTTOM FIRST
      -- (S.addLibraryCard puts each new card on top), Open the Way in hand.
      -- `decoy` goes into BOB's library, which alice's "your library" must not
      -- reach.
      board forest openTheWay stock decoy =
        let mana = S.landsFor forest S.alice 7 S.threePlayerGame
            withStock = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) mana stock
            withDecoy = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withStock decoy
            (withSpell, spellId) = S.handOne openTheWay withDecoy
         in (spellId, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      -- Announces this X, and rotates whatever batch the bottoming offers.
      answering :: Natural -> Prompt.Prompt r -> r
      answering x p = case p of
        Prompt.ChooseX {} -> x
        Prompt.Shuffle batch -> case batch of
          a : rest -> rest <> [a]
          [] -> []
        _ -> S.identityAnswer p
      -- Cast with this X, let anything in `between` happen while the spell sits
      -- on the stack, then resolve it.
      cast x between (spellId, gs) =
        let announced = S.runPure (answering x) gs (S.cast S.alice spellId)
         in S.runPure (answering x) (between announced) Stack.resolveTop
      -- The tap state of every battlefield object alice owns that carries this
      -- name. Empty where nothing of that name is there, which is how a card the
      -- walk revealed but did NOT match is told from one it did.
      tapOf name pid gs = do
        oid <- Game.zoneMembers Zone.Battlefield pid gs
        Monad.guard (fmap S.nameOf (Game.cardOf oid gs) == named name)
        o <- Maybe.maybeToList (Game.lookupObject oid gs)
        pure (Object.tapped o)
   in Spec.describe s "Open the Way" $ do
        -- CR 101.1 read against CR 601.2b, on a board where the ONLY thing that
        -- can refuse the announcement is the card's own sentence: {4}{G}{G} is
        -- affordable off seven Forests, so an X of 4 in a three-player game is
        -- refused for the ceiling and nothing else, and CR 601.2 returns the game
        -- to before the casting was proposed.
        Spec.it s "CR 101.1 an X above the number of players reverses the cast" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          murder <- S.printingOf s registry "Murder"
          let stock = [murder, mountain, murder, swamp, murder, island]
              after = cast 4 id (board forest openTheWay stock [])
          Spec.assertEqWith
            s
            "no land arrived on the battlefield"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after, tapOf "Mountain" S.alice after)
            ([], [], [])
          Spec.assertEqWith
            s
            "the library is exactly as it was stocked"
            (namesIn Zone.Library S.alice after)
            [named "Island", named "Murder", named "Swamp", named "Murder", named "Mountain", named "Murder"]
          Spec.assertEqWith s "and the card is still in alice's hand" (namesIn Zone.Hand S.alice after) [named "Open the Way"]
        -- The discriminating twin, differing in exactly one thing: carol leaves
        -- BEFORE the cast rather than after it, so the ceiling she is counted in
        -- is 2 and CR 101.1 refuses the same X of 3 the case above honoured.
        Spec.it s "CR 101.1 the same X is refused where the departure came first" $ do
          forest <- S.printingOf s registry "Forest"
          openTheWay <- S.printingOf s registry "Open the Way"
          island <- S.printingOf s registry "Island"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          murder <- S.printingOf s registry "Murder"
          let stock = [murder, mountain, murder, swamp, murder, island]
              (spellId, gs) = board forest openTheWay stock []
              after = cast 3 id (spellId, S.departs Departure.Type.Conceded S.carol gs)
          Spec.assertEqWith
            s
            "no land arrived on the battlefield"
            (tapOf "Island" S.alice after, tapOf "Swamp" S.alice after, tapOf "Mountain" S.alice after)
            ([], [], [])
          Spec.assertEqWith s "and the card is still in alice's hand" (namesIn Zone.Hand S.alice after) [named "Open the Way"]

-- CR 701.20e's look, CR 701.20a's reveal of ONE card chosen from among what it
-- showed, and CR 401.4's arrangement handed to randomness -- the three halves
-- communeWithTheGodsSpec and enduranceSpec above each carry one of, on the
-- printing that carries all three at once.
--
-- Carth the Lion {2}{B}{G} Legendary Creature -- Human Warrior 3/5, "Whenever
-- Carth enters or a planeswalker you control dies, look at the top seven cards
-- of your library. You may reveal a planeswalker card from among them and put it
-- into your hand. Put the rest on the bottom of your library in a random order. /
-- Planeswalkers' loyalty abilities you activate cost an additional [+1] to
-- activate." (name, cost, type line, power, toughness and Oracle text checked
-- against api.scryfall.com, 2026-08-20). The second sentence is
-- Pawl.PlaneswalkerSpec's, and data/scenarios/planeswalker is what proves CR
-- 606.2's "loyalty" narrows it.
--
-- ONE CHOICE, revealed AND moved: the reveal names ObjectRef.ChosenCardFromAmong
-- and binds what it showed to a slot, and the move reads that slot. A second
-- ChosenCardFromAmong under the move would be a second, independent choice, which
-- the printed "and" forbids -- so the reveal event and the card in hand must name
-- the SAME object, which is what the second assertion of each case below checks.
--
-- The look records nothing (CR 701.20e is private, #1412), so exactly one
-- GameEvent.Revealed is the whole of what the trigger shows -- the six cards left
-- over stay unrevealed however public the bottoming makes their destination.
--
-- THE RANDOMNESS IS THE ANSWERER'S, as it is for Endurance above: the fixture
-- names a permutation built from the OBJECT IDS, so the resulting library is
-- neither the batch's order nor its reverse.
carthTheLionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
carthTheLionSpec s registry =
  let named = Just . CardName.MkCardName . Text.pack
      -- alice's library, BOTTOM FIRST -- S.addLibraryCard puts each new card on
      -- top -- so the top seven, top first, are Island, Jace Beleren, Murder,
      -- Goblin Piker, Chandra, Forest, Bird Maiden, and the Swamp beneath them is
      -- never looked at. Two planeswalker cards among seven, five cards matching
      -- nothing, and the offer is therefore [Jace Beleren, Chandra] where the
      -- group is all seven.
      stockNames = ["Swamp", "Bird Maiden", "Forest", "Chandra, Fire Artisan", "Goblin Piker", "Murder", "Jace Beleren", "Island"]
      stock printings gs = List.mapAccumL (\g p -> let (oid, g2) = S.addLibraryCard p S.alice g in (g2, oid)) gs printings
      -- The stocked card of a given name, by the id S.addLibraryCard minted for
      -- it: the permutation below is built from these rather than from the
      -- batch's own order.
      idOf ids name = Maybe.fromMaybe S.noSource (Maybe.listToMaybe (fmap snd (filter (\(n, _) -> n == name) (zip stockNames ids))))
      nth n offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop n (NonEmpty.toList offered)))
      -- Takes the printed "may" -- clause 1, the reveal and the move to hand;
      -- clause 0 is the look and clause 2 the rest -- answers the group choice
      -- with the nth card offered, and names `order` as the random order, deepest
      -- card first. A `Nothing` order leaves Pawl.Engine.Game.honourShuffle the
      -- batch it offered.
      answering :: Maybe Int -> Maybe [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      answering mTake order p = case p of
        Prompt.ChooseOptional _ _ _ _ clause _
          | clause == ClauseIndex.MkClauseIndex 1 && Maybe.isJust mTake -> OptionalDecision.Exercises
        Prompt.ChooseCardFromAmong _ _ _ offered -> nth (Maybe.fromMaybe 0 mTake) offered
        Prompt.Shuffle offered -> Maybe.fromMaybe offered order
        _ -> S.identityAnswer p
      -- Which objects a CR 701.20a reveal has shown so far. A look shows nobody
      -- anything and appends no event, so this counts the reveal alone.
      revealed gs =
        Maybe.mapMaybe
          ( \event -> case event of
              GameEvent.Revealed (Revealed.MkRevealed _ oid _ _) -> Just oid
              _ -> Nothing
          )
          (S.eventsOf gs)
   in Spec.describe s "CarthTheLion" $ do
        -- The headline: the SECOND planeswalker card among the seven is revealed
        -- and taken, and the six left over reach the bottom in the order the
        -- randomness named.
        Spec.it s "CR 701.20a the enters trigger reveals the chosen card and puts that same object into its controller's hand" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger carth S.alice stocked
              -- Deepest first, and neither the batch's order nor its reverse.
              order = fmap (idOf ids) ["Murder", "Bird Maiden", "Island", "Forest", "Jace Beleren", "Goblin Piker"]
              answer :: Prompt.Prompt r -> r
              answer = answering (Just 1) (Just order)
              placed = S.runPure answer entered Engine.placePendingTriggers
              after = S.runPure answer placed Stack.resolveTop
          Spec.assertEqWith s "the second planeswalker card among the seven is the one in alice's hand" (namesIn Zone.Hand S.alice after) [named "Chandra, Fire Artisan"]
          Spec.assertEqWith s "and it is the one object the trigger revealed -- one reveal, not seven" (revealed after) [idOf ids "Chandra, Fire Artisan"]
          Spec.assertEqWith
            s
            "alice's library, top first, is the card the look never reached and then the six left over in the named order"
            (namesIn Zone.Library S.alice after)
            [named "Swamp", named "Goblin Piker", named "Jace Beleren", named "Forest", named "Island", named "Bird Maiden", named "Murder"]
          Spec.assertEqWith s "nothing was put into a graveyard" (namesIn Zone.Graveyard S.alice after) []
        -- The paired control: the same board and the same offer, answered at
        -- index 0. If the engine were picking, both legs would name one card.
        Spec.it s "CR 608.2d the engine does not pick: another answer reveals and takes the other planeswalker card" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger carth S.alice stocked
              answer :: Prompt.Prompt r -> r
              answer = answering (Just 0) Nothing
              placed = S.runPure answer entered Engine.placePendingTriggers
              after = S.runPure answer placed Stack.resolveTop
          Spec.assertEqWith s "the first planeswalker card comes to hand instead" (namesIn Zone.Hand S.alice after) [named "Jace Beleren"]
          Spec.assertEqWith s "and that is the object revealed" (revealed after) [idOf ids "Jace Beleren"]
        -- CR 608.2d: no planeswalker card among the seven, so the reveal is
        -- impossible and the move hanging on it with it; the "may" is not put.
        -- The headline, which takes the "may", is the control.
        Spec.it s "CR 608.2d Carth the Lion's reveal is not offered without a planeswalker among them" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          printings <- Monad.mapM (S.printingOf s registry) ["Swamp", "Bird Maiden", "Forest", "Murder", "Goblin Piker", "Murder", "Island", "Island"]
          let (stocked, _) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger carth S.alice stocked
              answer :: Prompt.Prompt r -> r
              answer = answering (Just 0) Nothing
              placed = S.runPure answer entered Engine.placePendingTriggers
              ((_, after), asked) = Replay.record answer placed Stack.resolveTop
          Spec.assertEqWith s "CR 608.2d the may was never put" [d | Response.ChoseOptional d <- asked] []
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith s "and nothing was revealed" (revealed after) []
        -- CR 603.5: the printed "may" is a real choice. Declining reveals nothing
        -- and sends all seven to the bottom -- the look still ran, so this cannot
        -- pass because the trigger never resolved.
        Spec.it s "CR 603.5 declining the may reveals nothing and bottoms all seven" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger carth S.alice stocked
              order = fmap (idOf ids) ["Chandra, Fire Artisan", "Island", "Bird Maiden", "Jace Beleren", "Murder", "Forest", "Goblin Piker"]
              answer :: Prompt.Prompt r -> r
              answer = answering Nothing (Just order)
              placed = S.runPure answer entered Engine.placePendingTriggers
              after = S.runPure answer placed Stack.resolveTop
          Spec.assertEqWith s "nothing reached alice's hand" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith s "and nothing was revealed" (revealed after) []
          Spec.assertEqWith
            s
            "all seven are on the bottom in the named order"
            (namesIn Zone.Library S.alice after)
            [named "Swamp", named "Goblin Piker", named "Forest", named "Murder", named "Jace Beleren", named "Bird Maiden", named "Island", named "Chandra, Fire Artisan"]
        -- The condition's OTHER disjunct (CR 603.1b read as "any"): a planeswalker
        -- alice controls dying fires the same ability, with Carth long settled and
        -- entering nothing.
        --
        -- Both Jaces are placed with no loyalty counters -- the fixture puts them
        -- there rather than an entry rider -- so CR 704.5i buries both in one
        -- state-based check. That is the pair the case turns on: bob's dies in the
        -- same batch as alice's, and only alice's is a planeswalker SHE controls,
        -- so an ability that read the filter as "a planeswalker" would resolve
        -- TWICE and put two cards in her hand.
        Spec.it s "CR 603.2 a planeswalker its controller controls dying fires the same ability, and an opponent's does not" $ do
          carth <- S.printingOf s registry "Carth the Lion"
          jace <- S.printingOf s registry "Jace Beleren"
          printings <- Monad.mapM (S.printingOf s registry) stockNames
          let (stocked, ids) = stock printings (Setup.emptyGame S.bothPlayers)
              (_, withCarth) = S.addPermanent carth S.alice stocked
              (_, withHers) = S.addPermanent jace S.alice withCarth
              (_, withHis) = S.addPermanent jace S.bob withHers
              buried = S.settleSba withHis
              answer :: Prompt.Prompt r -> r
              answer = answering (Just 1) Nothing
              placed = S.runPure answer buried Engine.placePendingTriggers
              after = S.runPure answer placed (Monad.replicateM_ 2 Stack.resolveTop)
          Spec.assertEqWith s "one trigger resolved, so exactly the chosen planeswalker card is in alice's hand" (namesIn Zone.Hand S.alice after) [named "Chandra, Fire Artisan"]
          Spec.assertEqWith s "and exactly one card was revealed" (revealed after) [idOf ids "Chandra, Fire Artisan"]
          Spec.assertEqWith s "both planeswalkers died" (namesIn Zone.Graveyard S.alice after, namesIn Zone.Graveyard S.bob after) ([named "Jace Beleren"], [named "Jace Beleren"])

-- Uncovered Clues {2}{U} Sorcery, "Look at the top four cards of your library.
-- You may reveal up to two instant and/or sorcery cards from among them and put
-- the revealed cards into your hand. Put the rest on the bottom of your library
-- in any order." (Oracle text checked against api.scryfall.com.) The reveal names
-- SEVERAL cards of the group, and "the revealed cards" reads every one of them.
--
-- The top four, top first, are Murder, Divination, Lightning Bolt and Forest, so
-- three cards match and the count of two leaves the chooser a real choice; the
-- Island beneath them is never looked at.
uncoveredCluesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
uncoveredCluesSpec s registry =
  let named = Just . CardName.MkCardName . Text.pack
      -- Bottom first, S.addLibraryCard putting each new card on top.
      stockNames = ["Island", "Forest", "Lightning Bolt", "Divination", "Murder"]
      -- Casts and resolves the spell with `wanted`, by name, as the answer to
      -- the one pick, and names the cards the reveal showed.
      run wanted = do
        clues <- S.printingOf s registry "Uncovered Clues"
        island <- S.printingOf s registry "Island"
        printings <- Monad.mapM (S.printingOf s registry) stockNames
        let stocked = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) (S.landsFor island S.alice 3 (Setup.emptyGame S.bothPlayers)) printings
            (withSpell, spell) = S.handOne clues stocked
            nameOfId gs oid = fmap S.nameOf (Game.cardOf oid gs)
            answer :: Prompt.Prompt r -> r
            answer p = case p of
              Prompt.ChooseCardsFromAmong _ _ _ offered _ -> Set.fromList (filter (\oid -> List.elem (nameOfId withSpell oid) (fmap named wanted)) offered)
              _ -> S.identityAnswer p
            after = S.runPure answer withSpell (S.cast S.alice spell >> Stack.resolveTop)
            revealed =
              Maybe.mapMaybe
                ( \event -> case event of
                    GameEvent.Revealed (Revealed.MkRevealed _ oid _ _) -> nameOfId withSpell oid
                    _ -> Nothing
                )
                (S.eventsOf after)
        pure (after, revealed)
   in Spec.describe s "UncoveredClues" $ do
        -- The headline: two cards picked, the first and the third match, so
        -- neither the offer's head nor its first two could stand in for them.
        Spec.it s "CR 701.20a both revealed cards are put into the hand, not only the last" $ do
          (after, revealed) <- run ["Murder", "Lightning Bolt"]
          Spec.assertEqWith s "alice's hand holds both revealed cards" (List.sort (namesIn Zone.Hand S.alice after)) [named "Lightning Bolt", named "Murder"]
          Spec.assertEqWith s "and those two were the cards revealed" (List.sort revealed) (List.sort (Maybe.catMaybes [named "Lightning Bolt", named "Murder"]))
          Spec.assertEqWith s "the other two go beneath the Island" (List.sort (drop 1 (namesIn Zone.Library S.alice after))) [named "Divination", named "Forest"]
        -- CR 608.2d: "up to two" is a ceiling, so one card is a legal answer --
        -- the per-card ask would have forced a second.
        Spec.it s "CR 608.2d up to two admits one: the other matches go to the bottom" $ do
          (after, _) <- run ["Divination"]
          Spec.assertEqWith s "alice's hand holds the one card she chose" (namesIn Zone.Hand S.alice after) [named "Divination"]
          Spec.assertEqWith s "and the two other matches reach the bottom with the Forest" (List.sort (drop 1 (namesIn Zone.Library S.alice after))) [named "Forest", named "Lightning Bolt", named "Murder"]
        -- The printed "may": none is an answer too, and "the revealed cards" then
        -- names nothing rather than failing on a slot never bound.
        Spec.it s "CR 608.2d revealing none puts all four on the bottom" $ do
          (after, revealed) <- run []
          Spec.assertEqWith s "alice's hand is empty" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith s "nothing was revealed" revealed []
          Spec.assertEqWith s "all four are beneath the Island" (List.sort (drop 1 (namesIn Zone.Library S.alice after))) [named "Divination", named "Forest", named "Lightning Bolt", named "Murder"]

-- Zimone's Experiment {3}{G} Sorcery, "Look at the top five cards of your
-- library. You may reveal up to two creature and/or land cards from among them,
-- then put the rest on the bottom of your library in a random order. Put all
-- land cards revealed this way onto the battlefield tapped and put all creature
-- cards revealed this way into your hand." (Oracle text checked against
-- api.scryfall.com.) "The rest" is the looked-at group less the revealed one,
-- and the revealed group is split by card type.
--
-- The top five, top first, are Forest, Goblin Piker, Murder, Bird Maiden and
-- Island, with a Swamp beneath them that is never looked at. Three cards match,
-- so the count of two leaves a real choice.
zimonesExperimentSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
zimonesExperimentSpec s registry =
  let named = Just . CardName.MkCardName . Text.pack
   in Spec.describe s "ZimonesExperiment" $ do
        Spec.it s "CR 608.2c the revealed land enters, the revealed creature is taken, and only the rest go to the bottom" $ do
          experiment <- S.printingOf s registry "Zimone's Experiment"
          forest <- S.printingOf s registry "Forest"
          printings <- Monad.mapM (S.printingOf s registry) ["Swamp", "Island", "Bird Maiden", "Murder", "Goblin Piker", "Forest"]
          let stocked = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) (S.landsFor forest S.alice 4 (Setup.emptyGame S.bothPlayers)) printings
              (withSpell, spell) = S.handOne experiment stocked
              wanted = [named "Forest", named "Goblin Piker"]
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseCardsFromAmong _ _ _ offered _ -> Set.fromList (filter (\oid -> List.elem (fmap S.nameOf (Game.cardOf oid withSpell)) wanted) offered)
                _ -> S.identityAnswer p
              after = S.runPure answer withSpell (S.cast S.alice spell >> Stack.resolveTop)
          Spec.assertEqWith s "the revealed creature card is in alice's hand" (namesIn Zone.Hand S.alice after) [named "Goblin Piker"]
          Spec.assertEqWith s "the revealed land card is on the battlefield beside the four that paid" (length (filter (== named "Forest") (namesIn Zone.Battlefield S.alice after))) 5
          Spec.assertEqWith s "only the three unrevealed cards went beneath the Swamp" (List.sort (drop 1 (namesIn Zone.Library S.alice after))) [named "Bird Maiden", named "Island", named "Murder"]

-- The same arm reached from a TRIGGER rather than an activated ability, and over
-- LAND cards rather than creature cards -- the two axes portOfKarfellSpec above
-- holds fixed.
--
-- Blossoming Tortoise {2}{G}{G} Creature -- Turtle 3/3, "Whenever this creature
-- enters or attacks, mill three cards, then return a land card from your
-- graveyard to the battlefield tapped. Activated abilities of lands you control
-- cost {1} less to activate. Land creatures you control get +1/+1." (name, cost,
-- type line and Oracle text checked against api.scryfall.com). Only the trigger
-- is read here; the two static abilities are Pawl.ActivateSpec's.
--
-- Two land cards are buried and the answer is pinned to the SECOND, so the
-- assertion cannot be met by taking the first; a creature card is buried beside
-- them and the three cards the trigger's own mill adds are creature cards too, so
-- an arm that ignored the Filter would have five candidates rather than two.
blossomingTortoiseSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
blossomingTortoiseSpec s registry = Spec.describe s "BlossomingTortoise" $ do
  Spec.it s "CR 608.2d the enters trigger returns the land card its controller chose, tapped" $ do
    tortoise <- S.printingOf s registry "Blossoming Tortoise"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    let (tortoiseId, entered) = S.entersWithTrigger tortoise S.alice (Setup.emptyGame S.bothPlayers)
        buried = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) entered [forest, piker, island]
        gs = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) buried [piker, piker, piker]
        named = Just . CardName.MkCardName . Text.pack
        -- The Island, buried last and so the SECOND of the two land cards in the
        -- ascending order the prompt offers.
        wanted = case Game.zoneMembers Zone.Graveyard S.alice gs of
          [_, _, third] -> Just third
          _ -> Nothing
        choosing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
        choosing chosen p = case p of
          Prompt.ChooseCardInGraveyard {} -> chosen
          _ -> S.identityAnswer p
        onBattlefield gs1 =
          List.sort
            ( fmap
                (\oid -> (fmap S.nameOf (Game.cardOf oid gs1), fmap Object.tapped (Game.lookupObject oid gs1)))
                (Set.toList (GameState.battlefield gs1))
            )
    case wanted of
      Nothing -> Spec.assertBool s False "expected three cards in alice's graveyard"
      Just chosen ->
        let answer :: Prompt.Prompt r -> r
            answer = choosing chosen
            placed = S.runPure answer gs Engine.placePendingTriggers
            resolved = S.runPure answer placed Stack.resolveTop
         in do
              Spec.assertEqWith
                s
                "the Island is on the battlefield tapped, beside the untapped Tortoise"
                (onBattlefield resolved)
                (List.sort [(named "Blossoming Tortoise", Just TapState.Untapped), (named "Island", Just TapState.Tapped)])
              Spec.assertEqWith
                s
                "the unchosen Forest, the buried creature card and the three milled ones stay put"
                (List.sort (namesIn Zone.Graveyard S.alice resolved))
                (List.sort (named "Forest" : replicate 4 (named "Goblin Piker")))
              Spec.assertBool s (S.onBattlefield tortoiseId resolved) "and the Tortoise itself never moved"

-- Timetwister {2}{U} Sorcery, "Each player shuffles their hand and graveyard
-- into their library, then draws seven cards." Commit // Memory: Commit {3}{U}
-- Instant, "Put target spell or nonland permanent into its owner's library
-- second from the top."; Memory {4}{U}{U} Sorcery, "Aftermath / Each player
-- shuffles their hand and graveyard into their library, then draws seven
-- cards." (names, costs, type lines and Oracle text checked against
-- api.scryfall.com, 2026-09-27.)
--
-- CR 701.24 over TWO sets in one instruction. Written as two instructions, the
-- hand and the graveyard would arrive as two events and each library would be
-- shuffled twice; the Shuffle log and bob's Dutiful Knowledge Seeker, which
-- counts arrival events (CR 603.2c), are what tell the readings apart.
--
-- THREE SEATS with distinct hand, graveyard and library sizes, so no per-player
-- figure coincides with another's, and every library holds more than seven.
timetwisterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
timetwisterSpec s registry = Spec.describe s "Timetwister" $ do
  let stock printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 :: Int .. n]
      -- Every Prompt.Shuffle is logged by the size of the library it randomizes,
      -- in the order the engine asks.
      logging :: Prompt.Prompt r -> State.State [Int] r
      logging p = case p of
        Prompt.Shuffle ids -> State.modify' (length ids :) >> pure ids
        _ -> pure (S.identityAnswer p)
      runLogged gs game =
        let ((_, after), shuffles) = State.runState (Engine.runGame logging gs game) []
         in (after, reverse shuffles)
      -- Every trigger placed is resolved, so a Seeker that fired twice would
      -- show two counters rather than one left on the stack.
      settle gs =
        let placed = S.runPure S.identityAnswer gs Engine.settleForPriority
         in if null (GameState.stack placed) then placed else settle (S.runPure S.identityAnswer placed Stack.resolveTop)
      -- alice: hand 1 (beside the spell), graveyard 1, library 9. bob: hand 2,
      -- graveyard 3, library 8, and the Seeker. carol: hand 0, graveyard 2,
      -- library 10.
      board island forest piker bolt seeker base =
        let (_, a1) = S.addHandCard piker S.alice base
            (_, a2) = S.addGraveyardCard bolt S.alice a1
            (_, b1) = S.addHandCard piker S.bob a2
            (_, b2) = S.addHandCard bolt S.bob b1
            (_, b3) = S.addGraveyardCard piker S.bob b2
            (_, b4) = S.addGraveyardCard bolt S.bob b3
            (_, b5) = S.addGraveyardCard island S.bob b4
            (seekerId, b6) = S.addPermanent seeker S.bob b5
            (_, c1) = S.addGraveyardCard piker S.carol b6
            (_, c2) = S.addGraveyardCard bolt S.carol c1
         in (seekerId, stock forest S.alice 9 (stock forest S.bob 8 (stock forest S.carol 10 c2)))
      sizes zone gs = fmap (\pid -> length (Game.zoneMembers zone pid gs)) [S.alice, S.bob, S.carol]
  -- Memory, cast off the graveyard (CR 702.127a) for the same instruction, and
  -- exiled on the way out rather than put back.
  Spec.it s "CR 702.127a Memory cast from a graveyard shuffles every hand and graveyard in, and is exiled" $ do
    commitMemory <- S.printingOf s registry "Commit"
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    seeker <- S.printingOf s registry "Dutiful Knowledge Seeker"
    let (withIslands, _) = S.handOne piker (S.landsFor island S.alice 6 S.threePlayerGame)
        (spell, withCard) = S.addGraveyardCard commitMemory S.alice withIslands
        (seekerId, ready) = board island forest piker bolt seeker withCard
        memory = CardName.MkCardName (Text.pack "Memory")
        (resolved, shuffles) = runLogged ready (Cast.castSpell S.manaPerformer S.alice spell memory Facing.FaceUp >> Stack.resolveTop)
        after = settle resolved
    Spec.assertEqWith s "CR 701.24a one shuffle per library, over the combined set" shuffles [9 + 2 + 1, 8 + 2 + 3, 10 + 0 + 2]
    Spec.assertEqWith s "CR 603.2c one arrival event for the Seeker" (S.counterOf CounterKind.PlusOnePlusOne seekerId after) 1
    Spec.assertEqWith s "each player drew seven" (sizes Zone.Hand after) [7, 7, 7]
    Spec.assertEqWith s "every graveyard is empty" (sizes Zone.Graveyard after) [0, 0, 0]
    Spec.assertEqWith s "CR 702.127a and Commit // Memory is in exile" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
  -- Commit's own half: CR 401.7's "second from the top", read off a SPELL, which
  -- leaves the stack for its owner's library without being countered.
  Spec.it s "CR 401.7 Commit puts target spell into its owner's library second from the top" $ do
    commitMemory <- S.printingOf s registry "Commit"
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (withSpell, spell) = S.handOne commitMemory (S.landsFor island S.alice 4 (Setup.emptyGame S.bothPlayers))
        (_, withLibrary) = S.addLibraryCard forest S.bob (snd (S.addLibraryCard forest S.bob withSpell))
        (_, ready) = S.spellOnStack piker S.bob withLibrary
        cast = S.runPure S.identityAnswer ready (Cast.castSpell S.manaPerformer S.alice spell (CardName.MkCardName (Text.pack "Commit")) Facing.FaceUp)
        after = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith
      s
      "bob's library reads Forest, Goblin Piker, Forest from the top"
      (namesIn Zone.Library S.bob after)
      (fmap Just [S.printingName forest, S.printingName piker, S.printingName forest])
    Spec.assertEqWith s "and the stack is empty" (length (GameState.stack after)) 0

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  destroyAllSpec s registry
  angelOfFinalitySpec s registry
  enduranceSpec s registry
  portOfKarfellSpec s registry
  graspingTentaclesSpec s registry
  fallOfTheThranSpec s registry
  midnightTillingSpec s registry
  communeWithTheGodsSpec s registry
  ancestralMemoriesSpec s registry
  openTheWaySpec s registry
  carthTheLionSpec s registry
  uncoveredCluesSpec s registry
  zimonesExperimentSpec s registry
  blossomingTortoiseSpec s registry
  timetwisterSpec s registry
