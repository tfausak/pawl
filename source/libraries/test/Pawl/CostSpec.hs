{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Cost and the three types it cases on (Pawl.Types.Cost,
-- Pawl.Types.CostComponent, Pawl.Types.Payment), plus the prompts the axis
-- adds. CR 118: what a cost IS, what it takes to pay one, and the alternative
-- and additional costs that change the answer.
--
-- The five gate cards: Greed (an amount-bearing component), Village Rites (a
-- mandatory spell-side additional cost), Headless Skaab (an additional cost paid
-- out of a zone that is not the battlefield), Fireblast (an alternative cost
-- with no mana in it at all) and Asmoranomardicadaistinaculdacar (an alternative
-- cost applied to an unpayable one, CR 118.6a). Asmoranomardicadaistinaculdacar
-- carries a sixth gate on its other ability: a Sacrifice component with a count
-- and a criterion, paid with Golden Eggs.
module Pawl.CostSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapPermanents as TapPermanents
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.Zone as Zone

-- The single activated ability of a printing. Total: the fallback is unreachable
-- in these fixtures. Duplicated per this suite's convention of group-local
-- helpers (ActivateSpec and ReplacementSpec each carry their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Face.spell (S.combinedFace p)) [] Nothing Nothing

doorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
doorSpec s registry =
  Spec.describe s "Door" $ do
    -- CR 118.3's own second example: "a permanent that's already tapped can't
    -- be tapped to pay a cost" (CR 107.5 says the same for the {T} symbol).
    Spec.it s "CR 107.5 TapThis is payable only while the permanent is untapped" $ do
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      let (oid, gs) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
          tapped = S.tapObject oid gs
      Spec.assertBool s (Cost.canPayComponent S.alice oid CostComponent.TapThis gs) "untapped pays"
      Spec.assertBool s (not (Cost.canPayComponent S.alice oid CostComponent.TapThis tapped)) "tapped does not"
    -- CR 701.21a: "A player can't sacrifice something that isn't a permanent,
    -- or something that's a permanent they don't control."
    Spec.it s "CR 701.21a SacrificeThis needs a permanent this player controls" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent S.alice onField CostComponent.SacrificeThis gs1) "a controlled permanent pays"
      Spec.assertBool s (not (Cost.canPayComponent S.alice inHand CostComponent.SacrificeThis gs1)) "a card in hand does not"
      Spec.assertBool s (not (Cost.canPayComponent S.bob onField CostComponent.SacrificeThis gs1)) "another player's permanent does not"
    -- CR 701.68b: "if a player is given the choice to blight but is unable to
    -- put N -1/-1 counters on a creature they control (usually because they
    -- control no creatures), they can't choose to blight."
    --
    -- The one component whose payability asks about the payer's WHOLE
    -- battlefield rather than about the object the cost is on -- which is why
    -- `oid` is the same Piker in all four readings and only the SEATS move. The
    -- Piker is on the battlefield throughout, so nothing here answers False for
    -- want of a permanent.
    Spec.it s "CR 701.68b Blight is payable only by a player who controls a creature" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertBool s (Cost.canPayComponent S.alice oid (CostComponent.Blight 1) gs) "a creature its payer controls pays"
      Spec.assertBool s (not (Cost.canPayComponent S.bob oid (CostComponent.Blight 1) gs)) "an opponent's creature does not"
      -- CR 122.6 puts any number of counters on any creature, so no N outruns a
      -- 2/1 -- rule 701.68b's "unable" has only the cause the rule itself names.
      Spec.assertBool s (Cost.canPayComponent S.alice oid (CostComponent.Blight 9) gs) "and no N is too large for a candidate that exists"
    -- CR 702.29a's "Discard this card", the exact mirror of SacrificeThis
    -- above: one names a permanent its controller owns the choice of, the other
    -- names a card in a hand. Asked of the ZONE and the OWNER, because CR 108.4
    -- gives a card in a hand no controller for a control-shaped gate to read.
    --
    -- Tested directly rather than only through cycling, because the two gates
    -- an activation passes -- this one and Activate.abilitiesFor's -- would
    -- otherwise cover for each other, and either alone would look correct.
    Spec.it s "CR 702.29a DiscardThis needs the card in this player's hand" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent S.alice inHand (CostComponent.DiscardThis DiscardCause.Ordinary) gs1) "a card in hand pays"
      Spec.assertBool s (not (Cost.canPayComponent S.alice onField (CostComponent.DiscardThis DiscardCause.Ordinary) gs1)) "a permanent does not"
      Spec.assertBool s (not (Cost.canPayComponent S.bob inHand (CostComponent.DiscardThis DiscardCause.Ordinary) gs1)) "and it is not the other player's to discard"
    -- CR 701.9a through Event.changeZone, the CR 400.7 funnel: the discarded
    -- card lands in its owner's graveyard as a new incarnation, so the old id
    -- is gone rather than moved.
    Spec.it s "CR 701.9a paying DiscardThis puts that card in the graveyard" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (inHand, gs0) = S.addHandCard piker S.alice (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs0 (Cost.payComponent S.alice inHand (CostComponent.DiscardThis DiscardCause.Ordinary))
      Spec.assertEqWith s "the hand is empty" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
      Spec.assertEqWith s "and the card is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- CR 118.6 vs CR 118.5a: the distinction the Maybe carries. Nothing is an
    -- unpayable cost; an empty ManaCost is {0} and is payable.
    Spec.it s "CR 118.6 an unpayable cost can never be paid" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 5
      Spec.assertBool
        s
        (not (Cost.canPay S.alice S.noSource (Cost.Type.MkCost Nothing []) gs))
        "Nothing is unpayable"
    Spec.it s "CR 118.5a a {0} cost is payable" $
      let gs = Setup.emptyGame S.bothPlayers
       in Spec.assertBool
            s
            (Cost.canPay S.alice S.noSource (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) gs)
            "an empty ManaCost is {0}"
    -- CR 118.6a: "If an unpayable cost is increased by an effect or an
    -- additional cost is imposed, the cost is still unpayable." total maps over
    -- the Maybe, so there is no special case to get wrong.
    Spec.it s "CR 118.6a Thalia's increase leaves an unpayable cost unpayable" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let base = S.landsInPlay mountain 5
          (_, gs) = S.addCreature thalia S.alice base
          (bolt, withBolt) = S.addHandCard lightningBolt S.alice gs
      Spec.assertEqWith
        s
        "still Nothing"
        (Cost.Type.mana (Cost.total S.alice bolt (Cost.Type.MkCost Nothing []) withBolt))
        Nothing
    -- The classification Pawl.Engine.Activate reads instead of matching a constructor.
    Spec.it s "CR 302.6 requiresSicknessCheck classifies a cost, and Greed's counterpart proves it" $ do
      llanowarElves <- S.printingOf s registry "Llanowar Elves"
      drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
      let elves = ActivatedAbility.cost (theAbility llanowarElves)
          skeletons = ActivatedAbility.cost (theAbility drudgeSkeletons)
      Spec.assertBool s (Cost.requiresSicknessCheck elves) "Llanowar Elves' {T} cost requires the tap symbol"
      Spec.assertBool s (not (Cost.requiresSicknessCheck skeletons)) "Drudge Skeletons' {B} regenerate cost does not"
    -- Departure 1: an activation cost is totalled against the ACTIVATION
    -- adjustments (Cost.activationAdjustments) and never the spell's, which is
    -- the whole of the discriminator #90 landed. PlayerEffect.matchesObject
    -- classifies an OBJECT, not a spell, so a noncreature PERMANENT matches
    -- Thalia's Not (HasCardType Creature) filter as readily as a noncreature
    -- spell does -- and Thalia taxes noncreature SPELLS, never abilities. Four
    -- Mountains must still afford Mindslaver's {4}; a fifth would be needed if
    -- the tax reached the activation. Pawl.ActivateSpec's Heartstone group is the
    -- other side of the same board: a reduction that DOES reach it.
    Spec.it s "CR 613.11 Thalia does not tax a noncreature permanent's activated ability" $ do
      mountain <- S.printingOf s registry "Mountain"
      mindslaver <- S.printingOf s registry "Mindslaver"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      let base = S.landsInPlay mountain 4
          (slaver, gs1) = S.addCreature mindslaver S.alice base
          (_, gs2) = S.addCreature thalia S.alice gs1
      Spec.assertBool
        s
        (Activate.activatable S.alice slaver (theAbility mindslaver) gs2)
        "four Mountains still pay {4}"
    -- Departure 2: an Unpaid payment is a complete no-op, never a partial one.
    Spec.it s "CR 118.6 paying an unpayable cost changes nothing" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 3
          (outcome, after) = S.runPureWith S.identityAnswer gs (Cost.pay Nothing ManaSpending.AsProduced S.alice S.noSource (Cost.Type.MkCost Nothing []))
      Spec.assertEqWith s "Unpaid" outcome Payment.Unpaid
      Spec.assertEqWith s "no land tapped" (S.tappedCount S.alice after) 0
    -- CR 701.21a: enough controlled permanents matching the criterion.
    Spec.it s "CR 118.3 a Sacrifice component counts matching permanents this player controls" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 2
          two = CostComponent.Sacrifice (Sacrifice.MkSacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain))
          three = CostComponent.Sacrifice (Sacrifice.MkSacrifice 3 (Filter.Type.HasSubtype Subtype.Mountain))
          islands = CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 (Filter.Type.HasSubtype Subtype.Island))
      Spec.assertBool s (Cost.canPayComponent S.alice S.noSource two gs) "two Mountains pay for two"
      Spec.assertBool s (not (Cost.canPayComponent S.alice S.noSource three gs)) "but not for three"
      Spec.assertBool s (not (Cost.canPayComponent S.alice S.noSource islands gs)) "and not for an Island"
      Spec.assertBool s (not (Cost.canPayComponent S.bob S.noSource two gs)) "and bob controls none of them"
    -- CR 118.6: unpayable below the count, payable at or above it -- the same
    -- shape CR 118.3's Sacrifice test above takes, for the SPENT direction of
    -- the player-counter substrate (P10 #37 GainPlayerCounters is the ADD
    -- direction).
    Spec.it s "CR 118.6 PayEnergy is unpayable below the count and payable at or above" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          two = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice gs0
          one = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice gs0
      Spec.assertBool s (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) two) "two energy pays PayEnergy 2"
      Spec.assertBool s (not (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) one)) "one energy cannot"
    -- CR 107.14: paying energy removes exactly that many counters.
    Spec.it s "CR 107.14 paying PayEnergy removes that many energy counters" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          three = S.addPlayerCounter PlayerCounterKind.Energy 3 S.alice gs0
          after = S.runPure S.identityAnswer three (Monad.void (Cost.payComponent S.alice oid (CostComponent.PayEnergy 2)))
      Spec.assertEqWith s "one energy left" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 1
    -- CR 118.12's counter-placing cost (CR 701.63a's endure). The gate is the
    -- permanent still being on the battlefield to take the counters, and NOT
    -- control -- Pawl.ResolveSpec's Fortress Kin-Guard cases prove the two
    -- branches at gameplay level, and this is where the control reading is ruled
    -- out, since on that card the payer is the controller either way.
    Spec.it s "CR 118.12 PutPlusOneCountersOnThis needs the permanent on the battlefield, whoever controls it" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent S.alice onField (CostComponent.PutPlusOneCountersOnThis 1) gs1) "a permanent on the battlefield pays"
      Spec.assertBool s (Cost.canPayComponent S.bob onField (CostComponent.PutPlusOneCountersOnThis 1) gs1) "and pays for a player who does not control it"
      Spec.assertBool s (not (Cost.canPayComponent S.alice inHand (CostComponent.PutPlusOneCountersOnThis 1) gs1)) "a card in hand does not"

-- Greed {3}{B} Enchantment: "{B}, Pay 2 life: Draw a card."
--
-- Scryfall returned no rulings for this card; CR 118.3's own worked example is
-- the specification of the discriminating test.
-- alice controls Greed and one untapped Swamp, with three cards in her
-- library so a draw is never a CR 121.3 loss, and priority in her own
-- precombat main phase. Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
greedBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Integer -> (ObjectId.ObjectId, GameState.GameState)
greedBoard swamp greed piker life =
  let base = S.landsInPlay swamp 1
      (greedId, gs1) = S.addCreature greed S.alice base
      (_, gs2) = S.addLibraryCard piker S.alice gs1
      (_, gs3) = S.addLibraryCard piker S.alice gs2
      (_, gs4) = S.addLibraryCard piker S.alice gs3
   in ( greedId,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.players = Map.adjust (\p -> p {Player.life = life}) S.alice (GameState.players gs4)
          }
      )

greedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
greedSpec s registry =
  Spec.describe s "Greed" $ do
    let isActivate a = case a of
          Action.Type.Activate _ _ -> True
          _ -> False
    Spec.it s "CR 119.4 activating draws a card and subtracts the life" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 20
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greedId (theAbility greed))
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertEqWith s "life 20 - 2" (S.lifeOf S.alice resolved) (Just 18)
      Spec.assertEqWith s "one card drawn" (S.handSize S.alice resolved) 1
      Spec.assertEqWith s "the Swamp is tapped" (S.tappedCount S.alice resolved) 1
    -- CR 118.3: "A player can't pay a cost without having the necessary
    -- resources to pay it fully. For example, a player with only 1 life
    -- can't pay a cost of 2 life." THE discriminating test: a payability
    -- check that ignores the amount passes the case above and fails here.
    Spec.it s "CR 118.3 at 1 life the ability is not offered" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 1
      Spec.assertBool
        s
        (not (Activate.activatable S.alice greedId (theAbility greed) gs))
        "not activatable"
      Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "no Activate action offered"
    Spec.it s "CR 119.4b at 2 life the ability IS offered" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 2
      Spec.assertBool
        s
        (Activate.activatable S.alice greedId (theAbility greed) gs)
        "activatable"
    -- CR 704.5a: "If a player has 0 or less life, that player loses the
    -- game." Paying life is a real life-total change, and a cost may
    -- legally kill its payer.
    Spec.it s "CR 704.5a paying the last 2 life is legal and loses the game" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 2
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greedId (theAbility greed))
          settled = S.settleSba activated
      Spec.assertEqWith s "life 0" (S.lifeOf S.alice activated) (Just 0)
      Spec.assertEqWith
        s
        "alice has lost"
        (fmap Player.status (Map.lookup S.alice (GameState.players settled)))
        (Just (Status.Departed Departure.Lost))
    -- Greed has no {T} in its cost, so CR 302.6 never applies -- the
    -- counterpart to Llanowar Elves, whose cost is Just [] plus TapThis.
    Spec.it s "CR 302.6 Greed's cost requires no tap symbol" $ do
      greed <- S.printingOf s registry "Greed"
      Spec.assertBool
        s
        (not (Cost.requiresSicknessCheck (ActivatedAbility.cost (theAbility greed))))
        "no {T}"

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- forced and correctly elided). The Pawl.ReplacementSpec shape.
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

wasAskedToSacrifice :: [Response.Response] -> Bool
wasAskedToSacrifice responses = sacrificePromptCount responses > 0

-- How MANY times, which is what a cost with two Sacrifice components needs:
-- "asked at all" cannot tell one prompt from two.
sacrificePromptCount :: [Response.Response] -> Int
sacrificePromptCount responses =
  let isSacrifice r = case r of
        Response.ChoseSacrifices _ -> True
        _ -> False
   in length (filter isSacrifice responses)

-- CR 601.2h: was the payer asked which order to pay a cost's parts in?
wasAskedForOrder :: [Response.Response] -> Bool
wasAskedForOrder responses =
  let isOrder r = case r of
        Response.OrderedCostComponents _ -> True
        _ -> False
   in any isOrder responses

wasAskedToChooseCost :: [Response.Response] -> Bool
wasAskedToChooseCost responses =
  let isCost r = case r of
        Response.ChoseCost _ -> True
        _ -> False
   in any isCost responses

-- Village Rites {B} Instant: "As an additional cost to cast this spell,
-- sacrifice a creature. Draw two cards."
--
-- Its one ruling: "You must sacrifice exactly one creature to cast this spell;
-- you can't cast it without sacrificing a creature, and you can't sacrifice
-- additional creatures."
-- alice controls one untapped Swamp and `n` Pikers, holds one Village Rites,
-- and has three cards in her library so the draw is never a CR 104.3c loss.
-- Loaded fresh inside each case that needs it -- equivalent because loading
-- is deterministic and cached (batch-recipe.md).
villageRitesBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
villageRitesBoard swamp piker villageRites n =
  let base = S.landsInPlay swamp 1
      addPiker (ids, gs) _ = let (oid, gs') = S.addCreature piker S.alice gs in (ids <> [oid], gs')
      (pikers, withPikers) = List.foldl' addPiker ([], base) [1 .. n]
      (rites, gs1) = S.addHandCard villageRites S.alice withPikers
      (_, gs2) = S.addLibraryCard piker S.alice gs1
      (_, gs3) = S.addLibraryCard piker S.alice gs2
      (_, gs4) = S.addLibraryCard piker S.alice gs3
   in ( rites,
        pikers,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Village Rites {B} Instant: "As an additional cost to cast this spell,
-- sacrifice a creature. Draw two cards."
--
-- Its one ruling: "You must sacrifice exactly one creature to cast this spell;
-- you can't cast it without sacrificing a creature, and you can't sacrifice
-- additional creatures."
villageRitesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
villageRitesSpec s registry =
  Spec.describe s "Village Rites" $ do
    Spec.it s "CR 118.8 the additional cost is paid and the spell resolves" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (rites, pikers, gs) = villageRitesBoard swamp piker villageRites 1
          cast = S.runPure S.identityAnswer gs (S.cast S.alice rites)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertEqWith s "no creature left on the battlefield" (S.creaturesInPlay S.alice resolved) 0
      -- Plan-bug fix: CR 400.7 gives the sacrificed permanent a NEW
      -- object id in the graveyard (Pawl.Engine.Event.changeZone), so the
      -- brief's own membership check (the OLD battlefield id inside
      -- Zone.Graveyard) is unsatisfiable by construction -- it fails
      -- even against correct code, matching Pawl.TriggerSpec's own
      -- "a sacrificed permanent goes to its owner's graveyard" (a
      -- COUNT, never an id match). Counting preserves the assertion's
      -- intent (a Piker was sacrificed into the graveyard). The +1 is
      -- Village Rites itself: CR 608.2n, as the final part of an
      -- instant's resolution the spell is put into its owner's
      -- graveyard.
      Spec.assertEqWith
        s
        "the sacrificed Piker(s) and the resolved instant are now in alice's graveyard"
        (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
        (length pikers + 1)
      Spec.assertEqWith s "two cards drawn" (S.handSize S.alice resolved) 2
    -- The ruling's second clause, and CR 601.2f's placement of an
    -- additional cost INSIDE the total cost: an implementation that pays
    -- additional costs after announcement offers this cast.
    Spec.it s "CR 601.2f with no creature to sacrifice the spell is not castable" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (rites, _, gs) = villageRitesBoard swamp piker villageRites 0
      Spec.assertBool s (not (S.castable S.alice rites gs)) "not castable"
      Spec.assertEqWith s "and not offered" (filter (S.isCastOf rites) (Action.legalActions S.alice gs)) []
    -- The cost payment went through Event.sacrifice, the CR 701.21 funnel,
    -- so the turn history saw it. A direct zone poke passes both cases
    -- above and fails this one. The settle/resolve shape is
    -- Pawl.TriggerSpec's historySpec, verbatim.
    Spec.it s "CR 608.2i Khabál Ghoul counts a creature sacrificed to pay a cost" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (rites, _, gs0) = villageRitesBoard swamp piker villageRites 1
          (ghoul, gs1) = S.addCreature khabalGhoul S.alice gs0
          cast = S.runPure S.identityAnswer gs1 (S.cast S.alice rites)
          endStep = Phase.Ending EndingStep.EndStep
          beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
          settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
          resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
          atEnd = resolveAll (settle (beginEndStep (settle cast)))
          counters = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject ghoul atEnd)
      Spec.assertEqWith s "one +1/+1 counter for the sacrificed Piker" counters 1
    -- CR 701.21a lets the player choose which of their permanents dies, so
    -- two candidates is a real choice; one is not, and where the rules
    -- leave nothing to ask, don't prompt.
    Spec.it s "CR 701.21a two creatures raise ChooseSacrifices; one elides it" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (ritesTwo, _, twoPikers) = villageRitesBoard swamp piker villageRites 2
          (ritesOne, _, onePiker) = villageRitesBoard swamp piker villageRites 1
          askedTwo = answersFor S.identityAnswer twoPikers (S.cast S.alice ritesTwo)
          askedOne = answersFor S.identityAnswer onePiker (S.cast S.alice ritesOne)
      Spec.assertBool s (wasAskedToSacrifice askedTwo) "asked with two"
      Spec.assertBool s (not (wasAskedToSacrifice askedOne)) "not asked with one"
    -- CR 115.1 makes a target only what the word "target" names: a
    -- sacrifice choice is not one, so it must not travel as a target.
    Spec.it s "CR 115.1 the sacrifice choice is not a target choice" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (rites, _, gs) = villageRitesBoard swamp piker villageRites 2
          asked = answersFor S.identityAnswer gs (S.cast S.alice rites)
          isTargets r = case r of
            Response.ChoseTargets _ -> True
            _ -> False
      Spec.assertBool s (not (any isTargets asked)) "no ChooseTargets was raised"

-- Altar's Reap {1}{B} Instant: "As an additional cost to cast this spell,
-- sacrifice a creature. Draw two cards."
--
-- alice controls `lands` untapped Swamps and exactly one creature -- the leg's
-- only variable -- holds one Altar's Reap, and has three cards in her library so
-- the draw is never a CR 104.3c loss. The creature is the ONLY sacrifice
-- candidate, so CR 701.21a's choice is elided and no answerer can pick a
-- different victim. Loaded fresh inside each case that needs it -- equivalent
-- because loading is deterministic and cached (batch-recipe.md).
altarsReapBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (ObjectId.ObjectId, GameState.GameState)
altarsReapBoard swamp creature altarsReap lands =
  let base = S.landsInPlay swamp lands
      (_, withCreature) = S.addCreature creature S.alice base
      (reap, gs1) = S.addHandCard altarsReap S.alice withCreature
      stock gs _ = snd (S.addLibraryCard swamp S.alice gs)
      gs2 = List.foldl' stock gs1 [1 .. (3 :: Int)]
   in ( reap,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 601.2f's lock-in, in the shape of the rule's own example: the creature
-- paying the additional cost IS the cost reducer, so the total determined at CR
-- 601.2f and the total a recomputation would reach once CR 601.2h's sacrifice
-- has happened are different numbers. Baral, Chief of Compliance -- "Instant and
-- sorcery spells you cast cost {1} less to cast" -- is pawl's Thunderscape
-- Familiar, and Altar's Reap is an Instant: the locked total is {B}, a
-- recomputed one {1}{B}.
--
-- The two readings are told apart by MANA SPENT, not by a board that only one of
-- them reaches: one Swamp tapped against two.
--
-- What holds it is that Pawl.Engine.Cast.castProposed determines the total ONCE
-- -- `paidCost`, off the adjustments read before any payment -- and hands that
-- VALUE to Cost.pay, which never re-reads the game state for it. Mutating
-- castProposed to pay the components first and then re-total the announced cost
-- against the state that leaves (#94's own description of the defect: a total
-- recomputed on demand) fails the first two legs below, and only those: it taps
-- two Swamps where the first expects one, and loses the second's payment
-- outright.
altarsReapSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
altarsReapSpec s registry =
  Spec.describe s "Altar's Reap" $ do
    -- Two Swamps, so the recomputing reading has the mana it would need and the
    -- legs differ in what was SPENT rather than in whether the spell resolved.
    Spec.it s "CR 601.2f sacrificing the reducer to the additional cost does not raise the total" $ do
      swamp <- S.printingOf s registry "Swamp"
      baral <- S.printingOf s registry "Baral, Chief of Compliance"
      altarsReap <- S.printingOf s registry "Altar's Reap"
      let (reap, gs) = altarsReapBoard swamp baral altarsReap 2
          cast = S.runPure S.identityAnswer gs (S.cast S.alice reap)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertBool s (S.castable S.alice reap gs) "the Reap is offered"
      Spec.assertEqWith s "one Swamp paid for it, not two" (S.tappedCount S.alice resolved) 1
      Spec.assertEqWith
        s
        "Baral paid the additional cost, and the Reap followed it"
        (namesIn Zone.Graveyard resolved)
        (fmap (CardName.MkCardName . Text.pack) ["Altar's Reap", "Baral, Chief of Compliance"])
      Spec.assertEqWith s "and two cards were drawn" (S.handSize S.alice resolved) 2
    -- The same claim where it decides the cast rather than the change: one
    -- Swamp is the whole of alice's mana, so a total re-read after the
    -- sacrifice is {1}{B} and CR 601.2h's payment fails -- which rewinds the
    -- cast and leaves the Reap in her hand.
    Spec.it s "CR 601.2f the locked total is what CR 601.2h pays, off one Swamp" $ do
      swamp <- S.printingOf s registry "Swamp"
      baral <- S.printingOf s registry "Baral, Chief of Compliance"
      altarsReap <- S.printingOf s registry "Altar's Reap"
      let (reap, gs) = altarsReapBoard swamp baral altarsReap 1
          cast = S.runPure S.identityAnswer gs (S.cast S.alice reap)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertBool s (S.castable S.alice reap gs) "the Reap is offered"
      Spec.assertEqWith s "off the one Swamp" (S.tappedCount S.alice resolved) 1
      -- Not the stack's emptiness, which a REWOUND cast leaves too: the Reap in
      -- the graveyard and the two cards in hand are what only a completed
      -- payment produces.
      Spec.assertEqWith
        s
        "Baral and the resolved Reap are in the graveyard"
        (namesIn Zone.Graveyard resolved)
        (fmap (CardName.MkCardName . Text.pack) ["Altar's Reap", "Baral, Chief of Compliance"])
      Spec.assertEqWith s "and two cards were drawn" (S.handSize S.alice resolved) 2
    -- The CONTROL on the two legs above, and not a third witness to the lock:
    -- with no reducer on the board the two readings of CR 601.2f agree, so the
    -- mutation that reddens them leaves this one green. What it establishes is
    -- that one Swamp is genuinely short of an unreduced Altar's Reap -- so the
    -- leg above succeeded on the locked total and not on mana it had spare.
    --
    -- The board is that leg with ONE thing changed, a Goblin Piker where Baral
    -- stood: the same one Swamp, the same seats, the same phase, the same
    -- library, the same single sacrifice candidate. The second pair pins the
    -- refusal to the AMOUNT -- a second Swamp buys the very same Piker board the
    -- cast.
    Spec.it s "CR 601.2f with no reducer to lock in, the same one Swamp does not pay" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      altarsReap <- S.printingOf s registry "Altar's Reap"
      let (oneReap, one) = altarsReapBoard swamp piker altarsReap 1
          (twoReap, two) = altarsReapBoard swamp piker altarsReap 2
      Spec.assertBool s (not (S.castable S.alice oneReap one)) "one Swamp: refused"
      Spec.assertEqWith s "and not offered" (filter (S.isCastOf oneReap) (Action.legalActions S.alice one)) []
      Spec.assertBool s (S.castable S.alice twoReap two) "two Swamps: offered"
      let resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer two (S.cast S.alice twoReap)) Stack.resolveTop
      Spec.assertEqWith s "and both Swamps paid for it" (S.tappedCount S.alice resolved) 2

-- Headless Skaab {2}{U} Creature -- Zombie Warrior 3/6: "As an additional cost
-- to cast this spell, exile a creature card from your graveyard. This creature
-- enters tapped."
--
-- alice controls three untapped Islands and holds one Headless Skaab, with
-- priority in her own precombat main phase. `seeds` are the cards put into
-- graveyards, and they are the ONLY thing a case varies: every leg pays {2}{U}
-- off the SAME three Islands, which is what keeps a negative castability
-- assertion from passing on unaffordable mana rather than on the missing
-- creature card. Loaded fresh inside each case that needs it -- equivalent
-- because loading is deterministic and cached (batch-recipe.md).
headlessSkaabBoard ::
  Printing.Printing ->
  Printing.Printing ->
  [(Printing.Printing, PlayerId.PlayerId)] ->
  (ObjectId.ObjectId, GameState.GameState)
headlessSkaabBoard island headlessSkaab seeds =
  let base = S.landsInPlay island 3
      seed gs (printing, pid) = snd (S.addGraveyardCard printing pid gs)
      seeded = List.foldl' seed base seeds
      (skaab, gs1) = S.addHandCard headlessSkaab S.alice seeded
   in ( skaab,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Headless Skaab {2}{U} Creature -- Zombie Warrior 3/6: "As an additional cost
-- to cast this spell, exile a creature card from your graveyard. This creature
-- enters tapped."
--
-- Its rulings: "You must exile exactly one creature card from your graveyard to
-- cast this spell; you cannot cast it without exiling a creature card, and you
-- cannot exile additional creature cards." And: "Players can only respond once
-- this spell has been cast and all its costs have been paid. No one can try to
-- otherwise remove the creature card you exiled in order to prevent you from
-- casting this spell."
--
-- The second is CR 601.2h and needs nothing of its own here: Pawl.Engine.Cast
-- pays the whole cost inside the cast, with no priority round between the
-- payment and the spell becoming cast, so there is no window for a response to
-- open in. The first is the "exactly one" case below.
--
-- The gate card for CostComponent.ExileCardsFromGraveyard, the first component
-- that exiles a CHOSEN card from a zone. Two assertions are kept deliberately
-- apart in every negative leg: castability is asked DIRECTLY, and the cast is
-- then attempted -- a cost that is offered and merely goes unpaid leaves an
-- empty stack too, so the stack check alone proves nothing about the gate.
headlessSkaabSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
headlessSkaabSpec s registry =
  Spec.describe s "Headless Skaab" $ do
    Spec.it s "CR 118.8 a creature card in the graveyard pays the additional cost" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      headlessSkaab <- S.printingOf s registry "Headless Skaab"
      let (skaab, gs) = headlessSkaabBoard island headlessSkaab [(piker, S.alice)]
          cast = S.runPure S.identityAnswer gs (S.cast S.alice skaab)
      Spec.assertBool s (S.castable S.alice skaab gs) "castable"
      Spec.assertEqWith s "CR 406.2 the creature card is in exile" (length (Game.zoneMembers Zone.Exile S.alice cast)) 1
      Spec.assertEqWith s "and the graveyard no longer holds it" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 0
      Spec.assertEqWith s "CR 601.2h the mana cost was paid too" (S.tappedCount S.alice cast) 3
    -- The PRIMARY negative, and deliberately not the empty-graveyard one below:
    -- an implementation that ignores the component's Filter entirely still
    -- refuses an empty graveyard, and only a graveyard holding exactly one
    -- INELIGIBLE card tells the two apart.
    Spec.it s "CR 601.2f a noncreature card in the graveyard does not pay it" $ do
      island <- S.printingOf s registry "Island"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      headlessSkaab <- S.printingOf s registry "Headless Skaab"
      let (skaab, gs) = headlessSkaabBoard island headlessSkaab [(lightningBolt, S.alice)]
          cast = S.runPure S.identityAnswer gs (S.cast S.alice skaab)
      Spec.assertBool s (not (S.castable S.alice skaab gs)) "not castable"
      Spec.assertEqWith s "and not offered" (filter (S.isCastOf skaab) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and the Skaab is still in hand" (S.handSize S.alice cast) 1
    Spec.it s "CR 601.2f an empty graveyard does not pay it either" $ do
      island <- S.printingOf s registry "Island"
      headlessSkaab <- S.printingOf s registry "Headless Skaab"
      let (skaab, gs) = headlessSkaabBoard island headlessSkaab []
          cast = S.runPure S.identityAnswer gs (S.cast S.alice skaab)
      Spec.assertBool s (not (S.castable S.alice skaab gs)) "not castable"
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and the Skaab is still in hand" (S.handSize S.alice cast) 1
    -- CR 400.3 / CR 108.4: "your graveyard" is per-OWNER, and a graveyard is not
    -- a shared zone. An implementation that swept every graveyard on the table
    -- passes both negatives above and fails this one.
    Spec.it s "CR 400.3 a creature card in the OPPONENT's graveyard does not pay it" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      headlessSkaab <- S.printingOf s registry "Headless Skaab"
      let (skaab, gs) = headlessSkaabBoard island headlessSkaab [(piker, S.bob)]
          cast = S.runPure S.identityAnswer gs (S.cast S.alice skaab)
      Spec.assertBool s (not (S.castable S.alice skaab gs)) "not castable"
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and bob's graveyard is untouched" (length (Game.zoneMembers Zone.Graveyard S.bob cast)) 1
    -- The first ruling's second clause: "you cannot exile additional creature
    -- cards". The count is 1, which a one-card graveyard cannot tell apart from
    -- "exile them all"; two eligible cards can, since exactly one must remain.
    Spec.it s "CR 601.2f the count is ONE, not the whole graveyard" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      headlessSkaab <- S.printingOf s registry "Headless Skaab"
      let (skaab, gs) = headlessSkaabBoard island headlessSkaab [(piker, S.alice), (piker, S.alice)]
          cast = S.runPure S.identityAnswer gs (S.cast S.alice skaab)
      Spec.assertBool s (S.castable S.alice skaab gs) "castable"
      Spec.assertEqWith s "one creature card exiled" (length (Game.zoneMembers Zone.Exile S.alice cast)) 1
      Spec.assertEqWith s "and the other is still in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 1
    -- The second sentence, on EntryRewrite.Tapped -- the arm Zof Bloodbog
    -- produced and this creature reuses (CR 614.1d, with CR 110.5b's default
    -- that it overrides). The power/toughness assertion is the control: a
    -- Skaab that entered wrong in some other way would not read 3/6.
    Spec.it s "CR 614.1d the Skaab enters tapped, and is 3/6" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      headlessSkaab <- S.printingOf s registry "Headless Skaab"
      let (skaab, gs) = headlessSkaabBoard island headlessSkaab [(piker, S.alice)]
          cast = S.runPure S.identityAnswer gs (S.cast S.alice skaab)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
          -- CR 400.7 mints the resolving spell a NEW id on the battlefield, so
          -- the permanent is identified as the battlefield's one new member
          -- rather than by the hand id the cast started from.
          entered =
            Set.lookupMin (Set.difference (GameState.battlefield resolved) (GameState.battlefield gs))
      Spec.assertEqWith
        s
        "CR 614.1d it entered tapped"
        (entered >>= \oid -> Object.tapped <$> Game.lookupObject oid resolved)
        (Just TapState.Tapped)
      Spec.assertEqWith s "and it is 3/6" (entered >>= \oid -> S.powerToughnessOf oid resolved) (Just (3, 6))

-- Synthetic Frail Exhumation {1}{B} Creature -- Zombie 2/2: "As an additional
-- cost to cast this spell, exile a creature card with power 2 or less from your
-- graveyard."
--
-- alice holds it with a Nightmare in her graveyard, with priority in her own
-- precombat main phase. `battlefield` is the only thing a case varies, and every
-- board pays the same {1}{B} off the same basic Swamp and Mountain -- so a
-- negative castability assertion cannot pass on unaffordable mana. bob's two
-- Swamps are on every board, so "Swamps you control" and "Swamps anyone
-- controls" never agree on a number.
frailExhumationBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState -> GameState.GameState) ->
  [Printing.Printing] ->
  (ObjectId.ObjectId, GameState.GameState)
frailExhumationBoard swamp nightmare exhumation battlefield buried =
  let base = battlefield (S.landsFor swamp S.bob 2 (Setup.emptyGame S.bothPlayers))
      (_, gs0) = S.addGraveyardCard nightmare S.alice base
      seeded = List.foldl' (\gs printing -> snd (S.addGraveyardCard printing S.alice gs)) gs0 buried
      (oid, gs1) = S.addHandCard exhumation S.alice seeded
   in ( oid,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Synthetic Frail Exhumation, the observer for CR 613.1 over the CANDIDATES a
-- characteristic-defining ability sweeps from OFF the battlefield. Nightmare's CR
-- 208.2a power counts "Swamps you control" -- battlefield objects -- and
-- Cost.exileCandidates reads that power for a Nightmare in a graveyard, so each
-- land has to be described by its CR 613 projection rather than by its printed
-- face.
--
-- It is also what pins CR 208.2a's power still ARRIVING now that the criterion
-- reads Projection.viewOfObject: the number comes from layer 7a of the graveyard
-- card's own fold rather than from a printed-card view, and these five cases say
-- it is the same number.
--
-- Why the card is synthetic: no printing exiles a graveyard card qualified by
-- POWER as a cost. Every printed power criterion over a graveyard -- Alesha's and
-- Reveillark's "target creature card with power 2 or less" -- is a TARGET,
-- admitted off the full projection, and Imperial Recruiter's search reads one
-- too. Nothing in the CR forbids the card: CR 601.2f admits any additional cost,
-- CR 406.2 lets one move a card out of a graveyard, and Headless Skaab above is
-- the same component with the qualifier dropped.
frailExhumationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
frailExhumationSpec s registry =
  Spec.describe s "Synthetic Frail Exhumation" $ do
    -- CR 613.1d layer 4: Urborg makes each of alice's four lands a Swamp, so the
    -- Nightmare in her graveyard is a 4/4 and the criterion refuses it. Read as
    -- printed the count reads 0 instead -- a printed-card candidate has no
    -- controller for "you control" to match either -- and the cost was payable.
    Spec.it s "CR 613.1 Urborg makes the graveyard Nightmare too big to exile" $ do
      swamp <- S.printingOf s registry "Swamp"
      mountain <- S.printingOf s registry "Mountain"
      urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
      nightmare <- S.printingOf s registry "Nightmare"
      exhumation <- S.printingOf s registry "Synthetic Frail Exhumation"
      let onBoard board = snd (S.addCreature urborg S.alice (S.landsFor mountain S.alice 2 (S.landsFor swamp S.alice 1 board)))
          (spell, gs) = frailExhumationBoard swamp nightmare exhumation onBoard []
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertBool s (not (S.castable S.alice spell gs)) "not castable"
      Spec.assertEqWith s "and not offered" (filter (S.isCastOf spell) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and the Nightmare is still in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 1
    -- The pair's other half, differing in exactly one permanent: a fourth Mountain
    -- where Urborg stood. Same land count, same mana, same graveyard -- the
    -- Nightmare is a 1/1 and pays the cost.
    Spec.it s "CR 208.2a without Urborg the same Nightmare is a 1/1 and pays it" $ do
      swamp <- S.printingOf s registry "Swamp"
      mountain <- S.printingOf s registry "Mountain"
      nightmare <- S.printingOf s registry "Nightmare"
      exhumation <- S.printingOf s registry "Synthetic Frail Exhumation"
      let onBoard = S.landsFor mountain S.alice 3 . S.landsFor swamp S.alice 1
          (spell, gs) = frailExhumationBoard swamp nightmare exhumation onBoard []
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertBool s (S.castable S.alice spell gs) "castable"
      Spec.assertEqWith s "CR 406.2 the Nightmare is in exile" (length (Game.zoneMembers Zone.Exile S.alice cast)) 1
      Spec.assertEqWith s "and the graveyard no longer holds it" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 0
    -- The control for the Urborg leg: the same board with a Goblin Piker also
    -- buried is castable, so the refusal above is about the Nightmare's power and
    -- not about the mana, the phase or the component.
    Spec.it s "CR 601.2f the Urborg board still pays with a Goblin Piker buried" $ do
      swamp <- S.printingOf s registry "Swamp"
      mountain <- S.printingOf s registry "Mountain"
      urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
      nightmare <- S.printingOf s registry "Nightmare"
      piker <- S.printingOf s registry "Goblin Piker"
      exhumation <- S.printingOf s registry "Synthetic Frail Exhumation"
      let onBoard board = snd (S.addCreature urborg S.alice (S.landsFor mountain S.alice 2 (S.landsFor swamp S.alice 1 board)))
          (spell, gs) = frailExhumationBoard swamp nightmare exhumation onBoard [piker]
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertBool s (S.castable S.alice spell gs) "castable"
      Spec.assertEqWith s "one card exiled" (length (Game.zoneMembers Zone.Exile S.alice cast)) 1
      Spec.assertEqWith s "and the Nightmare stayed behind" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 1
    -- The divergence the other way (CR 613.1d again): Blood Moon takes the Swamp
    -- subtype OFF three Bayous, so the Nightmare drops from a 4/4 to a 1/1 and
    -- becomes exilable. Read as printed, the Bayous are Swamps on both boards.
    Spec.it s "CR 613.1 Blood Moon shrinks the graveyard Nightmare to a 1/1" $ do
      swamp <- S.printingOf s registry "Swamp"
      mountain <- S.printingOf s registry "Mountain"
      bayou <- S.printingOf s registry "Bayou"
      bloodMoon <- S.printingOf s registry "Blood Moon"
      nightmare <- S.printingOf s registry "Nightmare"
      exhumation <- S.printingOf s registry "Synthetic Frail Exhumation"
      let lands = S.landsFor bayou S.alice 3 . S.landsFor mountain S.alice 1 . S.landsFor swamp S.alice 1
          onBoard board = snd (S.addCreature bloodMoon S.alice (lands board))
          (spell, gs) = frailExhumationBoard swamp nightmare exhumation onBoard []
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertBool s (S.castable S.alice spell gs) "castable"
      Spec.assertEqWith s "CR 406.2 the Nightmare is in exile" (length (Game.zoneMembers Zone.Exile S.alice cast)) 1
      Spec.assertEqWith s "and the graveyard no longer holds it" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 0
    -- That pair's other half, differing in exactly one permanent: no Blood Moon.
    -- The three Bayous are Swamps again and the Nightmare is a 4/4.
    Spec.it s "CR 208.2a without Blood Moon the Bayous are Swamps and the cost is unpayable" $ do
      swamp <- S.printingOf s registry "Swamp"
      mountain <- S.printingOf s registry "Mountain"
      bayou <- S.printingOf s registry "Bayou"
      nightmare <- S.printingOf s registry "Nightmare"
      exhumation <- S.printingOf s registry "Synthetic Frail Exhumation"
      let onBoard = S.landsFor bayou S.alice 3 . S.landsFor mountain S.alice 1 . S.landsFor swamp S.alice 1
          (spell, gs) = frailExhumationBoard swamp nightmare exhumation onBoard []
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertBool s (not (S.castable S.alice spell gs)) "not castable"
      Spec.assertEqWith s "and not offered" (filter (S.isCastOf spell) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and the Nightmare is still in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 1

-- alice controls `n` Mountains (all tapped when `tap` is True) and holds one
-- Fireblast, with priority in her own precombat main phase. Loaded fresh
-- inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
fireblastBoard :: Printing.Printing -> Printing.Printing -> Int -> Bool -> (ObjectId.ObjectId, GameState.GameState)
fireblastBoard mountain fireblastPrinting n tap =
  let base = S.landsInPlay mountain n
      tapAll gs = List.foldl' (flip S.tapObject) gs (Set.toList (GameState.battlefield gs))
      tapped = if tap then tapAll base else base
      (fireblast, gs1) = S.addHandCard fireblastPrinting S.alice tapped
   in ( fireblast,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Fireblast {4}{R}{R} Instant: "You may sacrifice two Mountains rather than pay
-- this spell's mana cost. Fireblast deals 4 damage to any target."
--
-- Scryfall returned no rulings for this card.
fireblastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fireblastSpec s registry =
  Spec.describe s "Fireblast" $ do
    -- The headline test: the printed cost is unaffordable and the spell is
    -- castable anyway. Kills "castability is mana affordability" and "an
    -- alternative cost is a different ManaCost" at once.
    Spec.it s "CR 118.9 two TAPPED Mountains and an empty pool still cast it, and it deals 4" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 2 True
          cast = S.runPure S.identityAnswer gs (S.cast S.alice fireblast)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertBool s (S.castable S.alice fireblast gs) "castable"
      Spec.assertEqWith s "both Mountains sacrificed" (length (Game.zoneMembers Zone.Battlefield S.alice resolved)) 0
      Spec.assertEqWith s "alice took 4 (identityAnswer targets the lowest recipient)" (S.lifeOf S.alice resolved) (Just 16)
    -- CR 118.9b: an alternative cost is optional, so a player who can
    -- afford both is really choosing.
    Spec.it s "CR 118.9b both costs payable raises ChooseCost; one payable elides it" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (both, sixUntapped) = fireblastBoard mountain fireblastPrinting 6 False
          (onlyAlternative, twoTapped) = fireblastBoard mountain fireblastPrinting 2 True
          askedBoth = answersFor S.identityAnswer sixUntapped (S.cast S.alice both)
          askedOne = answersFor S.identityAnswer twoTapped (S.cast S.alice onlyAlternative)
      Spec.assertBool s (wasAskedToChooseCost askedBoth) "asked when both are payable"
      Spec.assertBool s (not (wasAskedToChooseCost askedOne)) "not asked when only one is"
    -- CR 118.9a: "Only one alternative cost can be applied to any one spell
    -- as it's being cast" -- the list-of-candidates shape itself. The
    -- printed cost is offered FIRST.
    Spec.it s "CR 118.9a costsFor offers the printed cost first, then each alternative" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 2 True
          candidates = Cost.costsFor (S.printingName fireblastPrinting) fireblast gs
          red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      Spec.assertEqWith s "two candidates" (length candidates) 2
      Spec.assertEqWith
        s
        "the printed one first"
        (fmap Cost.Type.mana candidates)
        [Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, red, red]), Just (ManaCost.MkManaCost [])]
    -- CR 701.21a again, on the alternative's own component.
    Spec.it s "CR 701.21a three Mountains raise ChooseSacrifices; exactly two elide it" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (three, threeMountains) = fireblastBoard mountain fireblastPrinting 3 True
          (two, twoMountains) = fireblastBoard mountain fireblastPrinting 2 True
          askedThree = answersFor S.identityAnswer threeMountains (S.cast S.alice three)
          askedTwo = answersFor S.identityAnswer twoMountains (S.cast S.alice two)
      Spec.assertBool s (wasAskedToSacrifice askedThree) "asked with three"
      Spec.assertBool s (not (wasAskedToSacrifice askedTwo)) "not asked with exactly two"
    Spec.it s "CR 118.3 one Mountain pays neither cost" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 1 True
      Spec.assertBool s (not (S.castable S.alice fireblast gs)) "not castable"

-- alice controls one untapped Swamp -- the {B} half of Asmoranomardicadaistinaculdacar's
-- {B/R} -- and holds the card itself plus a Circling Vultures, with priority in
-- her own precombat main phase and an empty stack, which is where CR 302.1 lets a
-- creature spell be cast.
--
-- The Vultures are the DISCARD: their CR 116.2e special action is the only way a
-- card in the pool puts a card into a graveyard from a hand at no mana cost, so
-- the same board serves the discarded and the undiscarded case without changing
-- the mana available to pay with (see asmorSpec's own note).
asmorBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
asmorBoard swamp asmorPrinting vultures =
  let base = S.landsInPlay swamp 1
      (asmor, gs1) = S.addHandCard asmorPrinting S.alice base
      (vulturesId, gs2) = S.addHandCard vultures S.alice gs1
   in ( asmor,
        vulturesId,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Asmoranomardicadaistinaculdacar (MH2 186), a Legendary Creature -- Human Wizard
-- with NO mana cost: "As long as you've discarded a card this turn, you may pay
-- {B/R} to cast this spell." Its own rulings say the rest outright -- "it cannot
-- be cast normally. You'll need an alternative cost" and "Asmoranomardicadaistinaculdacar
-- doesn't allow you to discard cards" -- which together are CR 118.6a's second
-- sentence and nothing else.
--
-- Both of its other abilities are transcribed too: the enters-the-battlefield
-- tutor, whose Filter.HasName is proved by Pawl.ResolveSpec's
-- TheUnderworldCookbook group, and "Sacrifice two Foods: Target creature deals 6
-- damage to itself", which asmorFoodSpec below exercises.
asmorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
asmorSpec s registry =
  Spec.describe s "Asmoranomardicadaistinaculdacar" $ do
    -- The headline test, and CR 118.6a's second sentence: the printed cost is
    -- absent and so unpayable (CR 118.6, CR 202.1), and the alternative cost is
    -- what makes the card castable at all.
    --
    -- FOUR boards over one fixture, differing in one thing each, because a
    -- negative cast assertion is worthless otherwise. `discarded` and `binned`
    -- move the SAME card to the SAME zone and differ only in whether the move was
    -- a discard (CR 701.1, CR 701.9a); `nextTurn` is `discarded` with the event log cleared
    -- at the handoff and the same window restored, so only "this turn" separates
    -- them; and `poor` has discarded and cannot pay, which is what proves the
    -- {B/R} is demanded rather than the condition alone being enough.
    Spec.it s "CR 118.6a the alternative cost is what makes an unpayable card castable" $ do
      swamp <- S.printingOf s registry "Swamp"
      asmorPrinting <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      vultures <- S.printingOf s registry "Circling Vultures"
      let (asmor, vulturesId, gs) = asmorBoard swamp asmorPrinting vultures
          discarded = S.runPure S.identityAnswer gs (Event.discard DiscardCause.Ordinary S.alice vulturesId)
          binned = S.runPure S.identityAnswer gs (Event.changeZone vulturesId Zone.Graveyard)
          nextTurn =
            (Engine.beginTurnOf S.alice discarded)
              { GameState.phase = Phase.PrecombatMain,
                GameState.priority = Just S.alice
              }
          poor = List.foldl' (flip S.tapObject) discarded (Set.toList (GameState.battlefield discarded))
      Spec.assertBool s (not (S.castable S.alice asmor gs)) "no discard: not castable"
      Spec.assertBool s (S.castable S.alice asmor discarded) "discarded a card this turn: castable"
      Spec.assertBool s (not (S.castable S.alice asmor binned)) "CR 701.9a the same card put into the graveyard without being discarded does not count"
      Spec.assertBool s (not (S.castable S.alice asmor nextTurn)) "CR 608.2i the discard was last turn, so it does not count"
      Spec.assertBool s (not (S.castable S.alice asmor poor)) "and with the Swamp tapped the {B/R} cannot be paid"
    -- CR 118.9a's candidate list on the card CR 118.6 makes interesting: the
    -- printed cost is offered first and its mana part is Nothing -- an UNPAYABLE
    -- cost rather than {0} -- and the alternative appears beside it only while its
    -- CR 604.2 condition holds.
    Spec.it s "CR 118.9a costsFor offers the unpayable printed cost, and the alternative only once the condition holds" $ do
      swamp <- S.printingOf s registry "Swamp"
      asmorPrinting <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      vultures <- S.printingOf s registry "Circling Vultures"
      let (asmor, vulturesId, gs) = asmorBoard swamp asmorPrinting vultures
          discarded = S.runPure S.identityAnswer gs (Event.discard DiscardCause.Ordinary S.alice vulturesId)
          manaOf state = fmap Cost.Type.mana (Cost.costsFor (S.printingName asmorPrinting) asmor state)
          blackRed = ManaSymbol.Hybrid (Hybrid.MkHybrid (ManaType.Colored Color.Black) (ManaType.Colored Color.Red))
      Spec.assertEqWith s "undiscarded: the printed cost alone, unpayable" (manaOf gs) [Nothing]
      Spec.assertEqWith s "discarded: the printed cost first, then the {B/R}" (manaOf discarded) [Nothing, Just (ManaCost.MkManaCost [blackRed])]
    -- The cast itself, at gameplay level: the spell reaches the stack and the
    -- Swamp is tapped for it, so the alternative cost was really paid. Resolved
    -- too, since a 3/3 on the battlefield is what a caster is after.
    Spec.it s "CR 118.9 paying the alternative puts the spell on the stack, and it resolves as a 3/3" $ do
      swamp <- S.printingOf s registry "Swamp"
      asmorPrinting <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      vultures <- S.printingOf s registry "Circling Vultures"
      let (asmor, vulturesId, gs) = asmorBoard swamp asmorPrinting vultures
          discarded = S.runPure S.identityAnswer gs (Event.discard DiscardCause.Ordinary S.alice vulturesId)
          cast = S.runPure S.identityAnswer discarded (S.cast S.alice asmor)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
          -- CR 400.7 mints a new id on each move, so the permanent is the
          -- battlefield's one new member rather than the hand id.
          entered = Set.lookupMin (Set.difference (GameState.battlefield resolved) (GameState.battlefield gs))
      Spec.assertEqWith s "one spell on the stack" (length (GameState.stack cast)) 1
      Spec.assertEqWith s "the Swamp paid for it" (S.tappedCount S.alice cast) 1
      Spec.assertEqWith s "and it resolved as a 3/3" (entered >>= \oid -> S.powerToughnessOf oid resolved) (Just (3, 3))

-- alice controls Asmoranomardicadaistinaculdacar and, beside it, `foods` Golden
-- Eggs ({2} Artifact -- Food) and `others` Chromatic Spheres ({1} Artifact, no
-- subtype at all). The two counts are what a paired board varies: keeping
-- `foods + others` fixed leaves the boards identical in seats, phase, priority,
-- stack and artifact count, so the only thing left to flip a gate is CR 205.3g's
-- subtype.
--
-- bob controls the Child of Night -- a 2/1 with lifelink, and the OBSERVER: CR
-- 120.3f pays the damage source's controller, so bob's life total answers only
-- if the creature dealt the damage. alice's ability has no lifelink, and alice
-- is not bob.
--
-- No mana anywhere on the board, deliberately: "Sacrifice two Foods" has no mana
-- part, so a negative built on this board cannot be failing for want of mana.
asmorFoodBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  Int ->
  (ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
asmorFoodBoard asmorPrinting goldenEgg sphere childOfNight foods others =
  let addEach printing n gs0 =
        List.foldl'
          (\(oids, g) _ -> let (oid, g') = S.addCreature printing S.alice g in (oids <> [oid], g'))
          ([], gs0)
          (replicate n ())
      (asmor, gs1) = S.addCreature asmorPrinting S.alice (Setup.emptyGame S.bothPlayers)
      (eggs, gs2) = addEach goldenEgg foods gs1
      (_, gs3) = addEach sphere others gs2
      (victim, gs4) = S.addCreature childOfNight S.bob gs3
   in ( asmor,
        eggs,
        victim,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Both choices Asmor's Food ability raises, PINNED rather than searched: an
-- answerer that hunted for a legal option would repair a mutation by finding
-- another one, and the test would stay green while the engine's own choice was
-- broken.
asmorFoodAnswer :: Set.Set ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
asmorFoodAnswer eggs victim p = case p of
  Prompt.ChooseSacrifices {} -> eggs
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> S.identityAnswer p

-- An Activate of this source, among the actions on offer.
isActivateOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isActivateOf oid action = case action of
  Action.Type.Activate src _ -> src == oid
  _ -> False

-- "Sacrifice two Foods: Target creature deals 6 damage to itself" -- CR 701.21a
-- as a cost with a count and a criterion, and CR 120.2b as the effect ("the
-- spell or ability will specify which object deals that damage"), on the one
-- printing that writes both.
asmorFoodSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
asmorFoodSpec s registry =
  Spec.describe s "AsmoranomardicadaistinaculdacarFood" $ do
    -- The whole card. THREE Eggs, so which two pay is a real choice; the pair is
    -- pinned, and both halves are asserted -- how many Foods left, and that they
    -- were the two named. The damage is read through the Child of Night's own
    -- lifelink BEFORE the marked damage, because the marked damage alone would
    -- pass whichever object the engine credited.
    Spec.it s "CR 701.21a/120.2b whole card: two Foods pay, and the target deals itself 6" $ do
      asmorPrinting <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      goldenEgg <- S.printingOf s registry "Golden Egg"
      sphere <- S.printingOf s registry "Chromatic Sphere"
      childOfNight <- S.printingOf s registry "Child of Night"
      let (asmor, eggs, victim, gs) = asmorFoodBoard asmorPrinting goldenEgg sphere childOfNight 3 0
          pick = Set.fromList (take 2 eggs)
          answer :: Prompt.Prompt r -> r
          answer = asmorFoodAnswer pick victim
          activated = S.runPure answer gs (Activate.activateAbility S.alice asmor (theAbility asmorPrinting))
          resolved = S.runPure answer activated Stack.resolveTop
          eggName = CardName.MkCardName (Text.pack "Golden Egg")
      Spec.assertEqWith s "exactly two of the three Foods were sacrificed" (S.countOnBattlefieldByName eggName S.alice activated) 1
      Spec.assertEqWith s "and they are the two she named" (Set.intersection pick (GameState.battlefield activated)) Set.empty
      Spec.assertEqWith s "the ability is on the stack, so the sacrifice was a COST" (length (GameState.stack activated)) 1
      Spec.assertEqWith s "CR 120.3f bob gained six off his own creature's lifelink" (S.lifeOf S.bob resolved) (Just 26)
      Spec.assertEqWith s "and alice, who controls the ability, gained nothing" (S.lifeOf S.alice resolved) (Just 20)
      Spec.assertEqWith s "the Child of Night dealt it, not the ability" (fmap DamageEvent.source (S.damageEventsOf resolved)) [victim]
      Spec.assertEqWith s "six marked on itself" (S.damageOf victim resolved) (Just 6)
      Spec.assertBool s (not (S.onBattlefield victim (S.settleSba resolved))) "CR 704.5g the 2/1 dies to its own six"
    -- CR 701.21a's prompt, on the same fixture: three candidates for two make it
    -- a real choice, and exactly two elide it. The boards carry three artifacts
    -- either way.
    Spec.it s "CR 701.21a three Foods raise ChooseSacrifices; exactly two elide it" $ do
      asmorPrinting <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      goldenEgg <- S.printingOf s registry "Golden Egg"
      sphere <- S.printingOf s registry "Chromatic Sphere"
      childOfNight <- S.printingOf s registry "Child of Night"
      let (asmorThree, _, _, three) = asmorFoodBoard asmorPrinting goldenEgg sphere childOfNight 3 0
          (asmorTwo, _, _, two) = asmorFoodBoard asmorPrinting goldenEgg sphere childOfNight 2 1
          ability = theAbility asmorPrinting
          askedThree = answersFor S.identityAnswer three (Activate.activateAbility S.alice asmorThree ability)
          askedTwo = answersFor S.identityAnswer two (Activate.activateAbility S.alice asmorTwo ability)
      Spec.assertBool s (wasAskedToSacrifice askedThree) "asked with three"
      Spec.assertBool s (not (wasAskedToSacrifice askedTwo)) "not asked with exactly two"
    -- The negative, as a pair differing in exactly one thing: three Golden Eggs
    -- against one Golden Egg and two Chromatic Spheres. Same seats, same phase,
    -- same priority, same empty stack, three artifacts under alice either way --
    -- and the cost, asserted here, has an EMPTY mana part, so the gate cannot be
    -- turning on mana. What is left is how many of those artifacts are Foods.
    Spec.it s "CR 118.3 one Food cannot pay 'Sacrifice two Foods'; three can" $ do
      asmorPrinting <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      goldenEgg <- S.printingOf s registry "Golden Egg"
      sphere <- S.printingOf s registry "Chromatic Sphere"
      childOfNight <- S.printingOf s registry "Child of Night"
      let (asmorThree, _, _, three) = asmorFoodBoard asmorPrinting goldenEgg sphere childOfNight 3 0
          (asmorOne, _, _, one) = asmorFoodBoard asmorPrinting goldenEgg sphere childOfNight 1 2
          ability = theAbility asmorPrinting
          component = CostComponent.Sacrifice (Sacrifice.MkSacrifice 2 (Filter.Type.HasSubtype Subtype.Food))
      Spec.assertEqWith s "the cost is two Foods and no mana at all" (ActivatedAbility.cost ability) (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [component])
      Spec.assertEqWith s "both boards have an empty stack" (GameState.stack three, GameState.stack one) ([], [])
      Spec.assertEqWith s "and alice has priority on both" (GameState.priority three, GameState.priority one) (Just S.alice, Just S.alice)
      Spec.assertBool s (Cost.canPayComponent S.alice asmorThree component three) "three Foods pay the component"
      Spec.assertBool s (not (Cost.canPayComponent S.alice asmorOne component one)) "one Food beside two non-Foods does not"
      Spec.assertBool s (Activate.activatable S.alice asmorThree ability three) "so the ability is activatable with three"
      Spec.assertBool s (not (Activate.activatable S.alice asmorOne ability one)) "and is not with one"
      Spec.assertBool s (any (isActivateOf asmorThree) (Action.legalActions S.alice three)) "and it is menued with three"
      Spec.assertBool s (not (any (isActivateOf asmorOne) (Action.legalActions S.alice one))) "and not menued with one"

-- The two cross-checks: Fireblast's alternative cost against the projection
-- (Blood Moon, CR 613 layer 4) and against P7's cost modification (Thalia, CR
-- 118.9d).
crossCheckWithPriority :: GameState.GameState -> GameState.GameState
crossCheckWithPriority gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

crossCheckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
crossCheckSpec s registry =
  Spec.describe s "CrossChecks" $ do
    -- Blood Moon: "Nonbasic lands are Mountains." Evolving Wilds is a
    -- nonbasic land, so layer 4 makes it a Mountain and it may be
    -- sacrificed to Fireblast's alternative. The pair is what
    -- discriminates: WITHOUT Blood Moon the same board has one Mountain
    -- and the spell is not castable.
    --
    -- Blood Moon affects only NONBASIC lands, which is why the second
    -- permanent is Evolving Wilds and not an Island.
    Spec.it s "CR 613.1d PermanentOfSubtype reads the projection, not the printed type line" $ do
      mountain <- S.printingOf s registry "Mountain"
      evolvingWilds <- S.printingOf s registry "Evolving Wilds"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = S.landsInPlay mountain 1
          (wilds, gs1) = S.addCreature evolvingWilds S.alice base
          (fireblast, gs2) = S.addHandCard fireblastPrinting S.alice gs1
          withoutMoon = crossCheckWithPriority gs2
          (_, gs3) = S.addCreature bloodMoon S.alice gs2
          withMoon = crossCheckWithPriority gs3
          cast = S.runPure S.identityAnswer withMoon (S.cast S.alice fireblast)
      Spec.assertBool
        s
        (not (S.castable S.alice fireblast withoutMoon))
        "without Blood Moon, Evolving Wilds is not a Mountain and one Mountain is not two"
      Spec.assertBool s (S.castable S.alice fireblast withMoon) "with Blood Moon it is castable"
      Spec.assertBool s (not (Set.member wilds (GameState.battlefield cast))) "and Evolving Wilds was sacrificed as a Mountain"
    -- CR 118.9d: "If an alternative cost is being paid to cast a spell,
    -- any additional costs, cost increases, and cost reductions that
    -- affect that spell are applied to that alternative cost." Fireblast
    -- is an instant, so Thalia's noncreature tax reaches it, and the
    -- alternative's ABSENT mana component is a real, taxable {0} raised to
    -- {1}. This is the test that requires Just [] rather than Nothing.
    Spec.it s "CR 118.9d Thalia raises the alternative cost's {0} to {1}" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let tapAll gs = List.foldl' (flip S.tapObject) gs (Set.toList (GameState.battlefield gs))
          twoTapped = tapAll (S.landsInPlay mountain 2)
          (_, taxedTwo) = S.addCreature thalia S.alice twoTapped
          (fireblastTwo, gsTwo) = S.addHandCard fireblastPrinting S.alice taxedTwo
          -- The same board plus one UNTAPPED Mountain, which can pay the {1}.
          (_, threeMountains) = S.addCreature mountain S.alice twoTapped
          (_, taxedThree) = S.addCreature thalia S.alice threeMountains
          (fireblastThree, gsThree) = S.addHandCard fireblastPrinting S.alice taxedThree
          alternativeOf oid gs = case Cost.costsFor (S.printingName fireblastPrinting) oid gs of
            _ : alt : _ -> Just (Cost.Type.mana (Cost.total S.alice oid alt gs))
            _ -> Nothing
      Spec.assertEqWith
        s
        "the alternative's {0} is taxed to {1}"
        (alternativeOf fireblastTwo (crossCheckWithPriority gsTwo))
        (Just (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])))
      Spec.assertBool
        s
        (not (S.castable S.alice fireblastTwo (crossCheckWithPriority gsTwo)))
        "with nothing untapped the taxed alternative is unpayable, so Fireblast is not castable"
      Spec.assertBool
        s
        (S.castable S.alice fireblastThree (crossCheckWithPriority gsThree))
        "a third, untapped Mountain pays the {1} and it is castable again"

-- Longtusk Cub, the P10 capstone: an energy trigger (CR 603.2 / 509-510) that
-- feeds an energy-paid pump (CR 118 / 122.6). The ability is extracted via the
-- file-local total `theAbility` (no partial functions); the card-characteristics
-- case guards that the extraction sees a real ability.
longtuskCubSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
longtuskCubSpec s registry =
  Spec.describe s "LongtuskCub" $ do
    Spec.it s "Longtusk Cub is a {1}{G} 2/2 Cat with a pay-energy ability" $ do
      longtuskCub <- S.printingOf s registry "Longtusk Cub"
      Spec.assertEqWith s "name" (Face.name (S.combinedFace longtuskCub)) (CardName.MkCardName $ Text.pack "Longtusk Cub")
      Spec.assertEqWith s "power" (Face.power (S.combinedFace longtuskCub)) (Just (Power.MkPower (Quantity.Type.Literal 2)))
      Spec.assertEqWith s "one activated ability" (length (Face.activatedAbilities (S.combinedFace longtuskCub))) 1
    Spec.it s "CR 118.6 the pay-energy ability is payable at two energy, not at one, and grows the Cub" $ do
      longtuskCub <- S.printingOf s registry "Longtusk Cub"
      let (cubId, base) = S.addCreature longtuskCub S.alice (Setup.emptyGame S.bothPlayers)
          ability = theAbility longtuskCub
          withTwo = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice base
          withOne = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice base
          activated = S.runPure S.identityAnswer withTwo (Activate.activateAbility S.alice cubId ability)
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertBool s (Activate.activatable S.alice cubId ability withTwo) "payable at two"
      Spec.assertBool s (not (Activate.activatable S.alice cubId ability withOne)) "unpayable at one"
      Spec.assertEqWith s "energy spent" (S.playerCounterOf PlayerCounterKind.Energy S.alice activated) 0
      Spec.assertEqWith s "Cub grew a +1/+1 counter" (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject cubId resolved)) (Just 1)
    Spec.it s "CR 603.2 Longtusk Cub gains two energy when it connects" $ do
      longtuskCub <- S.printingOf s registry "Longtusk Cub"
      let (gs, _, _) = S.combatBoardOf [longtuskCub] []
          after = S.runCombat S.aggressiveAnswer gs
      Spec.assertEqWith s "alice gained two energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2

-- Jarad, Golgari Lich Lord {B}{B}{G}{G} Legendary Creature -- Zombie Elf 2/2,
-- "Sacrifice a Swamp and a Forest: Return this card from your graveyard to your
-- hand" (Oracle text checked against Scryfall). alice has Jarad in her
-- graveyard, one untapped Bayou, and whatever `extras` adds beside it, with
-- priority in her own precombat main phase.
--
-- Jarad's OTHER activated ability, "{1}{B}{G}, Sacrifice another creature: Each
-- opponent loses life equal to the sacrificed creature's power", is the card's
-- first and is proved by jaradDrainSpec below; this group's helper reaches the
-- second through `swampAndForest`.
--
-- THE card for CR 118.3 across two components, and a Bayou is why: `Land --
-- Forest Swamp` is one permanent that answers BOTH halves of the cost, so a gate
-- that asks each half on its own says yes and a gate that asks them together
-- says no. The Bayou is added first, so it sorts ahead of every extra and is
-- what Replay.defaultAnswer picks out of a ChooseSacrifices.
--
-- Its id comes back so a case can PIN a sacrifice answer to it rather than
-- letting an answerer search for whatever land is still legal (#105).
jaradBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
jaradBoard jarad bayou extras =
  let add gs printing = snd (S.addCreature printing S.alice gs)
      (bayouId, withBayou) = S.addCreature bayou S.alice (Setup.emptyGame S.bothPlayers)
      withExtras = List.foldl' add withBayou extras
      (jaradId, withJarad) = S.addGraveyardCard jarad S.alice withExtras
   in ( bayouId,
        jaradId,
        withJarad
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 601.2h: pay a cost's parts in `order`, and sacrifice `bayou` whenever asked
-- to sacrifice anything.
--
-- PINNED, not searching: the sacrifice answer is the same set on every board, so
-- an engine that pays the parts in some other order cannot be rescued by the
-- answerer finding whichever land is still legal. An answer naming a permanent
-- that is no longer a candidate is rejected by Cost.payComponent, which is
-- exactly the failure the printed order runs into below.
payingInOrder :: [Natural.Natural] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
payingInOrder order bayou p = case p of
  Prompt.OrderCostComponents {} -> order
  Prompt.ChooseSacrifices {} -> Set.singleton bayou
  _ -> S.identityAnswer p

-- The names of alice's objects in one zone, sorted. CR 400.7 mints a fresh id
-- on every zone change, so a permanent that paid a cost has to be found in the
-- graveyard by name rather than by the id it was sacrificed under.
namesIn :: Zone.Zone -> GameState.GameState -> [CardName.CardName]
namesIn zone gs =
  List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone S.alice gs))

-- Jarad's SECOND activated ability, "Sacrifice a Swamp and a Forest". The file-local
-- `theAbility` names the FIRST, which on this card is the drain the group below
-- proves. Total: the fallback is unreachable on this printing.
swampAndForest :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
swampAndForest p = case Face.activatedAbilities (S.combinedFace p) of
  _ : ability : _ -> ability
  _ -> theAbility p

jaradSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
jaradSpec s registry =
  Spec.describe s "Jarad, Golgari Lich Lord" $ do
    -- CR 118.3: "A player can't pay a cost without having the necessary
    -- resources to pay it FULLY", read over the whole cost -- with CR 601.2h's
    -- "partial payments are not allowed" and its permission to pay the parts "in
    -- any order". NOT CR 118.10, which is about two different abilities.
    Spec.it s "CR 118.3 one Bayou does not pay for a Swamp and a Forest" $ do
      jarad <- S.printingOf s registry "Jarad, Golgari Lich Lord"
      bayou <- S.printingOf s registry "Bayou"
      forest <- S.printingOf s registry "Forest"
      let cost = ActivatedAbility.cost (swampAndForest jarad)
          (_, loneId, lone) = jaradBoard jarad bayou []
          (_, pairId, pair) = jaradBoard jarad bayou [forest]
      -- Guards the two below against passing vacuously off a card file that
      -- states one component, or none.
      Spec.assertEqWith s "the cost really has two components" (length (Cost.Type.components cost)) 2
      -- And the control on the control: each component ALONE is payable off the
      -- lone Bayou, so what refuses the cost is the joint reading and nothing
      -- else -- not the mana part, not a zone gate, not a missing candidate.
      Spec.assertBool
        s
        (all (\c -> Cost.canPayComponent S.alice loneId c lone) (Cost.Type.components cost))
        "each component on its own is payable off the one Bayou"
      Spec.assertBool s (not (Cost.canPay S.alice loneId cost lone)) "but the cost as a whole is not"
      Spec.assertBool s (Cost.canPay S.alice pairId cost pair) "and a Forest beside the Bayou pays it"
    -- The prompt-side half (#112). It holds by CONSTRUCTION rather than by any
    -- guard: Cost.payComponents folds the components in the Game state monad and
    -- Cost.payComponent's Sacrifice arm reads the state afresh, so the second
    -- component's candidates are computed after the first component's
    -- Event.sacrifice has moved its permanent off the battlefield. It still
    -- DISCRIMINATES: threading the pre-payment state down from Cost.pay and
    -- computing the candidates off that snapshot instead makes this ask twice
    -- and sacrifice the Bayou twice.
    --
    -- Bayou, Swamp and Forest is the board that shows it: the Swamp component is
    -- asked (two candidates for one slot), the answer takes the Bayou, and the
    -- Forest component is then down to one candidate and elided. A gate that
    -- offered the Bayou twice would ask twice and sacrifice one land.
    Spec.it s "CR 601.2h the second sacrifice cannot be paid with what the first consumed" $ do
      jarad <- S.printingOf s registry "Jarad, Golgari Lich Lord"
      bayou <- S.printingOf s registry "Bayou"
      forest <- S.printingOf s registry "Forest"
      swamp <- S.printingOf s registry "Swamp"
      let (_, jaradId, gs) = jaradBoard jarad bayou [swamp, forest]
          activating = Activate.activateAbility S.alice jaradId (swampAndForest jarad)
          asked = answersFor S.identityAnswer gs activating
          after = S.runPure S.identityAnswer gs activating
      Spec.assertEqWith s "asked to sacrifice exactly once" (sacrificePromptCount asked) 1
      Spec.assertEqWith
        s
        "the Swamp is still on the battlefield"
        (namesIn Zone.Battlefield after)
        [CardName.MkCardName (Text.pack "Swamp")]
      Spec.assertEqWith
        s
        "and the Bayou and the Forest are the two lands that paid"
        (namesIn Zone.Graveyard after)
        (fmap (CardName.MkCardName . Text.pack) ["Bayou", "Forest", "Jarad, Golgari Lich Lord"])
    -- CR 601.2h: the parts are paid "in any order", and the ORDER IS THE
    -- PAYER'S. One Bayou and one plain Swamp is the board where it shows: the
    -- cost is payable (Bayou for the Forest half, Swamp for the Swamp half), and
    -- ONLY in that order -- spend the Bayou on the Swamp half and the Forest
    -- half has nothing left.
    --
    -- A PAIR of legs differing in exactly one thing: the same board, the same
    -- pinned sacrifice answer, and only the order answered. The sacrifice answer
    -- names the Bayou on both legs, so nothing about the outcome comes from the
    -- answerer picking a different land.
    --
    -- The assertion reads the ENGINE's output -- what is on the battlefield and
    -- in the graveyard afterwards -- and not the answers it was given.
    Spec.it s "CR 601.2h the payer's order decides whether one Bayou and one Swamp pay for a Swamp and a Forest" $ do
      jarad <- S.printingOf s registry "Jarad, Golgari Lich Lord"
      bayou <- S.printingOf s registry "Bayou"
      swamp <- S.printingOf s registry "Swamp"
      let (bayouId, jaradId, gs) = jaradBoard jarad bayou [swamp]
          cost = ActivatedAbility.cost (swampAndForest jarad)
          activating = Activate.activateAbility S.alice jaradId (swampAndForest jarad)
          run order = S.runPure (payingInOrder order bayouId) gs activating
          forestFirst = run [1, 0]
          printedOrder = run [0, 1]
          names :: [String] -> [CardName.CardName]
          names = fmap (CardName.MkCardName . Text.pack)
      -- The board is payable, so a leg that fails fails on the ORDER and not on
      -- resources CR 118.3 never had.
      Spec.assertBool s (Cost.canPay S.alice jaradId cost gs) "the cost is payable on this board"
      Spec.assertBool
        s
        (wasAskedForOrder (answersFor (payingInOrder [1, 0] bayouId) gs activating))
        "and the payer is asked for the order"
      Spec.assertEqWith s "paying the Forest half first spends both lands" (namesIn Zone.Battlefield forestFirst) []
      Spec.assertEqWith
        s
        "the Bayou and the Swamp are what paid"
        (namesIn Zone.Graveyard forestFirst)
        (names ["Bayou", "Jarad, Golgari Lich Lord", "Swamp"])
      -- The other order is a legal answer that loses the payment, which is the
      -- player's own doing: Cost.pay restores the entry state, so the Bayou it
      -- spent on the Swamp half is back and nothing was paid.
      Spec.assertEqWith
        s
        "paying the Swamp half first pays nothing"
        (namesIn Zone.Battlefield printedOrder)
        (names ["Bayou", "Swamp"])
      Spec.assertEqWith
        s
        "and Jarad is still in the graveyard"
        (namesIn Zone.Graveyard printedOrder)
        (names ["Jarad, Golgari Lich Lord"])

-- alice controls Jarad, one other creature (`victim`), exactly {1}{B}{G} of
-- lands, and whatever `extras` name. Two seats: "each opponent" needs one
-- opponent and bob's life total is the read.
--
-- EXACTLY ONE other creature, so Cost.payComponent's Sacrifice arm elides its
-- prompt (candidates == count) and S.identityAnswer scripts no sacrifice -- the
-- test asserts the rule rather than an answerer's pick. EXACTLY three lands, one
-- Swamp and two Forests, which is the minimum that pays {1}{B}{G} and leaves the
-- mana window nothing to decide.
jaradDrainBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
jaradDrainBoard jarad swamp forest victim extras =
  let lands = S.landsFor forest S.alice 2 (S.landsFor swamp S.alice 1 (Setup.emptyGame S.bothPlayers))
      (jaradId, withJarad) = S.addCreature jarad S.alice lands
      (preyId, withPrey) = S.addCreature victim S.alice withJarad
   in (jaradId, preyId, foldl (\g printing -> snd (S.addCreature printing S.alice g)) withPrey extras)

-- CR 602.2b pays an activation cost at CR 601.2h, so by the time Jarad's drain
-- resolves the creature it sacrificed is a card in a graveyard and CR 608.2h's
-- last known information is the only reading of its power there is.
jaradDrainSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
jaradDrainSpec s registry =
  Spec.describe s "Jarad, Golgari Lich Lord's drain" $ do
    -- The base case: nothing modifies the prey's power, so this separates "the
    -- cost payment binds the permanent at all" from "the slot is empty and the
    -- quantity silently answers nothing".
    Spec.it s "CR 601.2h a creature sacrificed to pay the cost is still readable when the ability resolves" $ do
      jarad <- S.printingOf s registry "Jarad, Golgari Lich Lord"
      swamp <- S.printingOf s registry "Swamp"
      forest <- S.printingOf s registry "Forest"
      piker <- S.printingOf s registry "Goblin Piker"
      let (jaradId, preyId, gs) = jaradDrainBoard jarad swamp forest piker []
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice jaradId (theAbility jarad))
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertEqWith s "bob lost the Piker's 2 power" (S.lifeOf S.bob resolved) (Just 18)
      -- CR 109.5: "each opponent" must not reach the controller. Without this a
      -- fix that spelled the recipient EachPlayer would pass the assertion above.
      Spec.assertEqWith s "alice, who is not an opponent of herself, lost nothing" (S.lifeOf S.alice resolved) (Just 20)
      Spec.assertEqWith s "the Piker really was sacrificed, and as a COST" (Game.lookupObject preyId activated) Nothing
      Spec.assertEqWith s "so the ability was on the stack with the Piker already gone" (length (GameState.stack activated)) 1
    -- The discriminating leg. Night of Souls' Betrayal ("All creatures get
    -- -1/-1") makes the Sentry's LAST KNOWN power 2 where its PRINTED power is
    -- 3, so the two readings of CR 608.2h give bob 18 and 17 -- and an unbound
    -- slot gives 20. Three implementations, three life totals.
    --
    -- Ogre Sentry rather than the Goblin Piker above: the Piker is 1/0 under the
    -- Betrayal and dies to CR 704.5f before the activation, which would make this
    -- leg unreachable rather than discriminating. Its defender is inert here,
    -- nothing attacking.
    Spec.it s "CR 608.2h the power read is the one it last had, not the one it printed" $ do
      jarad <- S.printingOf s registry "Jarad, Golgari Lich Lord"
      swamp <- S.printingOf s registry "Swamp"
      forest <- S.printingOf s registry "Forest"
      sentry <- S.printingOf s registry "Ogre Sentry"
      betrayal <- S.printingOf s registry "Night of Souls' Betrayal"
      let (jaradId, preyId, gs) = jaradDrainBoard jarad swamp forest sentry [betrayal]
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice jaradId (theAbility jarad))
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      -- Guards the leg against passing off a board where the anthem is not
      -- applying: 3 printed less 1 is what makes 18 differ from 17.
      Spec.assertEqWith s "the Sentry is 2/2 under the Betrayal, not 3/3" (S.powerToughnessOf preyId gs) (Just (2, 2))
      Spec.assertEqWith s "bob lost 2 -- the Sentry's 3 printed power less the anthem's -1" (S.lifeOf S.bob resolved) (Just 18)
      Spec.assertEqWith s "alice lost nothing" (S.lifeOf S.alice resolved) (Just 20)

-- Chooses this value of X; every other prompt takes the identity fallback, which
-- aims Hatred's one target slot at the only creature on the board. The liar
-- pattern ProjectionSpec's answerX4 uses.
answerHatredXOf :: Natural.Natural -> Prompt.Prompt r -> r
answerHatredXOf n p = case p of
  Prompt.ChooseX {} -> n
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseX with the bound the prompt carries and RECORDS it. An
-- empty log is how a test sees that the question was never put at all, which is
-- what the mana-cost-only reading of CR 601.2b leaves behind on this card.
answerHatredAtBound :: Prompt.Prompt r -> State.State [Natural.Natural] r
answerHatredAtBound p = case p of
  Prompt.ChooseX _ _ _ bound -> do
    State.modify' (\seen -> seen <> [bound])
    pure bound
  _ -> pure (S.identityAnswer p)

-- alice controls five untapped Swamps -- exactly {3}{B}{B} -- and a Goblin Piker
-- (2/1), with Hatred in hand and this life total, in her own precombat main
-- phase.
--
-- FIVE Swamps and no more, so the mana is fixed across every case below: X moves
-- only what the LIFE half of the cost can pay, which is what makes the announced
-- value's bound a fact about life rather than about mana.
hatredBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Integer -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hatredBoard swamp piker hatred life =
  let base = S.landsInPlay swamp 5
      (pikerId, withPiker) = S.addCreature piker S.alice base
      (gs, hatredId) = S.handOne hatred withPiker
   in ( pikerId,
        hatredId,
        gs {GameState.players = Map.adjust (\p -> p {Player.life = life}) S.alice (GameState.players gs)}
      )

-- Hatred {3}{B}{B} Instant: "As an additional cost to cast this spell, pay X
-- life. Target creature gets +X/+0 until end of turn."
--
-- The card CR 601.2b's variable ADDITIONAL cost was waiting for: its mana cost
-- holds no {X} at all, so an engine reading that rule's parenthetical ("such as
-- an {X} in its mana cost") as the rule rather than as an example never asks for
-- the value. CR 107.3a is the general statement -- "a mana cost, alternative
-- cost, additional cost, and/or activation cost with an {X}, [-X], or X in it".
hatredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hatredSpec s registry =
  Spec.describe s "Hatred" $ do
    -- The whole card. Falsifiers, in order: an X read as 0 leaves a 2/1 at 20
    -- life; an X paid but not read back leaves a 2/1 at 17; an X read but not
    -- paid leaves a 5/1 at 20.
    Spec.it s "CR 601.2b/107.3a whole card: X=3 pays 3 life and the Piker is 5/1" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let (pikerId, hatredId, gs) = hatredBoard swamp piker hatred 20
          after = S.runPure (answerHatredXOf 3) gs (do S.cast S.alice hatredId; Stack.resolveTop)
      Spec.assertEqWith s "CR 119.4 subtracted the announced 3" (S.lifeOf S.alice after) (Just 17)
      Spec.assertEqWith s "power 2 + 3" (Projection.powerOf pikerId after) (Just 5)
      -- +X/+0 and not +X/+X: the toughness is the reading this card's own text
      -- distinguishes from Untamed Might's.
      Spec.assertEqWith s "toughness untouched" (Projection.toughnessOf pikerId after) (Just 1)
      Spec.assertEqWith s "five Swamps paid {3}{B}{B}" (S.tappedCount S.alice after) 5
      Spec.assertEqWith s "Hatred resolved out of hand" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
    -- The SAME board with one thing changed. CR 107.3i makes the cost's X and the
    -- effect's X one value, so both readings move together; a board on which X=3
    -- and X=5 agreed could not tell the announcement was read at all.
    Spec.it s "CR 107.3i the cost and the effect read ONE announced value" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let at x =
            let (pikerId, hatredId, gs) = hatredBoard swamp piker hatred 20
                after = S.runPure (answerHatredXOf x) gs (do S.cast S.alice hatredId; Stack.resolveTop)
             in (S.lifeOf S.alice after, Projection.powerOf pikerId after)
      Spec.assertEqWith s "X=1 pays 1 and pumps 1" (at 1) (Just 19, Just 3)
      Spec.assertEqWith s "X=5 pays 5 and pumps 5" (at 5) (Just 15, Just 7)
    -- CR 119.4b: "Players can always pay 0 life, no matter what their (or their
    -- team's) life total is." So X=0 is a legal announcement and the spell casts;
    -- a floor that required X > 0 would leave Hatred in hand instead.
    Spec.it s "CR 119.4b X=0 casts, pays nothing and pumps nothing" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let (pikerId, hatredId, gs) = hatredBoard swamp piker hatred 20
          after = S.runPure (answerHatredXOf 0) gs (do S.cast S.alice hatredId; Stack.resolveTop)
      Spec.assertEqWith s "life untouched" (S.lifeOf S.alice after) (Just 20)
      Spec.assertEqWith s "still a 2/1" (Projection.powerOf pikerId after, Projection.toughnessOf pikerId after) (Just 2, Just 1)
      Spec.assertEqWith s "five Swamps still paid {3}{B}{B}" (S.tappedCount S.alice after) 5
      Spec.assertEqWith s "Hatred resolved out of hand" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
    -- CR 119.4: "the player may do so only if their life total is greater than or
    -- equal to the amount of the payment". The PAIR is the assertion: one board,
    -- one seat, one mana supply, four life -- X=3 casts and X=5 does not, so the
    -- refusal cannot be want of mana, timing or a non-empty stack.
    --
    -- Four life and not five, so the paying half never reaches 0 and CR 704.5a
    -- never takes alice out from under the assertion.
    Spec.it s "CR 119.4 an X above the life total reverses the whole casting" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let at x =
            let (pikerId, hatredId, gs) = hatredBoard swamp piker hatred 4
                after = S.runPure (answerHatredXOf x) gs (do S.cast S.alice hatredId; Stack.resolveTop)
             in ( S.lifeOf S.alice after,
                  Projection.powerOf pikerId after,
                  S.tappedCount S.alice after,
                  length (Game.zoneMembers Zone.Hand S.alice after)
                )
      Spec.assertEqWith s "X=3 is affordable: 1 life left, a 5/1, five Swamps tapped, hand empty" (at 3) (Just 1, Just 5, 5, 0)
      Spec.assertEqWith s "X=5 is not: nothing paid, nothing pumped, Hatred still in hand" (at 5) (Just 4, Just 2, 0, 1)
    -- That the question is PUT at all, which no board state records. The bound is
    -- the life total rather than anything about the mana -- five Swamps pay
    -- {3}{B}{B} exactly at every value of X -- so it moves with the life and with
    -- nothing else.
    Spec.it s "CR 601.2b Hatred is asked for X, bounded by the life its cost can pay" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let boundsAt life =
            let (_, hatredId, gs) = hatredBoard swamp piker hatred life
             in State.execState (Engine.runGame answerHatredAtBound gs (S.cast S.alice hatredId)) []
      Spec.assertEqWith s "at 20 life the bound is 20" (boundsAt 20) [20]
      Spec.assertEqWith s "at 4 life the bound is 4" (boundsAt 4) [4]
    -- The fence under the two functions above. CR 601.2 reverses a casting whose
    -- steps a player cannot comply with; it never picks a value on their behalf,
    -- so an unannounced X is simply unpayable. Unreachable from either cast path
    -- -- hasVariable is what guarantees the announcement happens first -- and
    -- asserted here rather than at gameplay level for exactly that reason.
    Spec.it s "CR 601.2b an unannounced X is unpayable, and is what makes the cast ask" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let (_, _, gs) = hatredBoard swamp piker hatred 20
          printed = Cost.Type.MkCost (Face.manaCost (S.combinedFace hatred)) (Face.additionalCosts (S.combinedFace hatred))
      Spec.assertEqWith s "the printed additional cost is CR 601.2b's variable" (Face.additionalCosts (S.combinedFace hatred)) [CostComponent.PayLifeX]
      Spec.assertBool s (Cost.hasVariable printed) "so the cost has a variable, though its mana part has none"
      Spec.assertBool s (notElem ManaSymbol.Variable (foldMap ManaCost.unwrap (Face.manaCost (S.combinedFace hatred)))) "the mana part really has none"
      Spec.assertBool s (not (Cost.canPayComponent S.alice S.noSource CostComponent.PayLifeX gs)) "and it is unpayable until announced"
      Spec.assertBool s (Cost.canPayComponent S.alice S.noSource (CostComponent.PayLife 20) gs) "while the announced 20 it substitutes to is payable"
    -- The same fence one keyword action over, and the same board serves: alice
    -- controls a Goblin Piker, so CR 701.68b's only refusal does not apply and
    -- an announced blight IS payable -- which is what leaves the unannounced one
    -- unpayable for CR 601.2b's reason alone rather than for want of a creature.
    --
    -- The pair also states the fact Cost.greatestPayableX rests on: blight's
    -- payability does not move with the number, so a big enough announcement is
    -- refused by CR 101.1's ceiling and by nothing else.
    Spec.it s "CR 601.2b an unannounced blight X is unpayable, though every announced one is payable" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let (_, _, gs) = hatredBoard swamp piker hatred 20
      Spec.assertBool s (not (Cost.canPayComponent S.alice S.noSource CostComponent.BlightX gs)) "unpayable until announced"
      Spec.assertBool s (Cost.canPayComponent S.alice S.noSource (CostComponent.Blight 1) gs) "an announced 1 is payable"
      Spec.assertBool s (Cost.canPayComponent S.alice S.noSource (CostComponent.Blight 99) gs) "and so is an announced 99, CR 701.68b naming no number that is too many"
      Spec.assertBool s (Cost.hasVariable (Cost.Type.MkCost Nothing [CostComponent.BlightX])) "it is a CR 107.3 variable"
      Spec.assertBool s (not (Cost.demandGrowsWithX (Cost.Type.MkCost Nothing [CostComponent.BlightX]))) "whose demand never grows, so only CR 101.1 can refuse a value"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Cost" $ do
  doorSpec s registry
  jaradSpec s registry
  jaradDrainSpec s registry
  greedSpec s registry
  hatredSpec s registry
  villageRitesSpec s registry
  altarsReapSpec s registry
  headlessSkaabSpec s registry
  frailExhumationSpec s registry
  everbarkShamanSpec s registry
  putridRaptorSpec s registry
  catharticReunionSpec s registry
  magmaticInsightSpec s registry
  safeholdSentrySpec s registry
  fireblastSpec s registry
  asmorSpec s registry
  asmorFoodSpec s registry
  crossCheckSpec s registry
  longtuskCubSpec s registry
  thrastaSpec s registry
  omniscienceSpec s registry
  springleafDrumSpec s registry
  morcantSpec s registry
  unerringSlingSpec s registry

-- alice holds `card` and controls `n` untapped Mountains, plus Omniscience when
-- `granted` is True, with priority in her own precombat main phase so a sorcery
-- is castable (CR 307.1). The pairs below vary `granted` and NOTHING else:
-- every board is the same seats, the same stock and the same mana.
omniscienceBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Bool -> (ObjectId.ObjectId, GameState.GameState)
omniscienceBoard mountain omniscience card n granted =
  let base = S.landsInPlay mountain n
      withGrant = if granted then snd (S.addCreature omniscience S.alice base) else base
      (spell, gs) = S.addHandCard card S.alice withGrant
   in ( spell,
        gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Omniscience {7}{U}{U}{U} Enchantment: "You may cast spells from your hand
-- without paying their mana costs."
--
-- CR 118.9's other half -- an alternative cost "applied to it from another
-- effect" -- as a STANDING, player-scoped grant, which no per-card list can hold
-- because the effect never names the cards it applies to. The one-shot half was
-- already expressible (Effect.OfferCast).
--
-- EVERY POSITIVE BOARD BELOW HAS ZERO UNTAPPED MANA except the Blaze case, which
-- needs a payable printed cost to have a second answer to compare against. So a
-- cast that succeeds can only have succeeded through the grant, and the paired
-- negative -- the same board with the enchantment removed -- can only fail for
-- its absence.
omniscienceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
omniscienceSpec s registry =
  Spec.describe s "Omniscience" $ do
    -- The headline pair. No lands at all on either board, so mana, timing and
    -- stock are identical and the enchantment is the only difference.
    Spec.it s "CR 118.9 with no mana at all the grant casts a Lightning Bolt, and without it nothing is castable" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (withIt, granted) = omniscienceBoard mountain omniscience bolt 0 True
          (withoutIt, ungranted) = omniscienceBoard mountain omniscience bolt 0 False
          cast = S.runPure S.identityAnswer granted (S.cast S.alice withIt)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
          refused = S.runPure S.identityAnswer ungranted (S.cast S.alice withoutIt)
      Spec.assertBool s (S.castable S.alice withIt granted) "castable under the grant"
      Spec.assertBool s (any (S.isCastOf withIt) (Action.legalActions S.alice granted)) "and offered"
      Spec.assertEqWith s "it dealt 3 (identityAnswer targets the lowest recipient)" (S.lifeOf S.alice resolved) (Just 17)
      Spec.assertBool s (not (S.castable S.alice withoutIt ungranted)) "not castable without the grant"
      Spec.assertBool s (not (any (S.isCastOf withoutIt) (Action.legalActions S.alice ungranted))) "and not offered"
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack refused)) 0
      Spec.assertEqWith s "and bob was untouched either way" (S.lifeOf S.bob resolved) (Just 20)
    -- The grant is its CONTROLLER's, which is what the card's PlayerScope.You
    -- says. Asserted on the candidate list rather than on castability, because
    -- only one player holds priority on any one board and an instant bob cannot
    -- cast for want of priority would pass this for the wrong reason.
    Spec.it s "CR 118.9 the grant reaches its controller's hand alone" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (hers, granted) = omniscienceBoard mountain omniscience bolt 0 True
          (his, gs) = S.addHandCard bolt S.bob granted
          manaOf oid = fmap Cost.Type.mana (Cost.costsFor (S.printingName bolt) oid gs)
          red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      Spec.assertEqWith s "alice is offered the printed {R} and the grant's {0}" (manaOf hers) [Just (ManaCost.MkManaCost [red]), Just (ManaCost.MkManaCost [])]
      Spec.assertEqWith s "bob is offered the printed {R} alone" (manaOf his) [Just (ManaCost.MkManaCost [red])]
    -- CR 118.9a lets the controller announce WHICH alternative cost they pay, so
    -- the grant is appended to the card's own candidates rather than replacing
    -- them: Fireblast under Omniscience may still sacrifice two Mountains.
    Spec.it s "CR 118.9a the grant is offered beside the card's printed alternative, not instead of it" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = omniscienceBoard mountain omniscience fireblastPrinting 2 True
          -- The two {0} candidates are told apart by their COMPONENTS: the
          -- printed alternative sacrifices two Mountains, the grant asks nothing.
          shapeOf c = (Cost.Type.mana c, length (Cost.Type.components c))
          red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      Spec.assertEqWith
        s
        "printed, then the sacrifice alternative, then the grant"
        (fmap shapeOf (Cost.costsFor (S.printingName fireblastPrinting) fireblast gs))
        [ (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, red, red]), 0),
          (Just (ManaCost.MkManaCost []), 1),
          (Just (ManaCost.MkManaCost []), 0)
        ]
    -- CR 118.9d: "any additional costs ... that affect that spell are applied to
    -- that alternative cost". Cathartic Reunion {1}{R} discards two cards as an
    -- additional cost, and the grant replaces the mana half alone.
    Spec.it s "CR 118.9d the mana cost goes away and the printed additional cost does not" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      -- THREE spare cards rather than two, so CR 701.9b's discard is a real
      -- choice: a hand of exactly two elides the prompt and would prove the
      -- discard happened without proving anybody was asked which cards.
      let (reunion, bare) = omniscienceBoard mountain omniscience catharticReunion 0 True
          withHand = List.foldl' (\g _ -> snd (S.addHandCard piker S.alice g)) bare [1 .. (3 :: Int)]
          gs = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) withHand [1 .. (4 :: Int)]
          cast = S.runPure S.identityAnswer gs (S.cast S.alice reunion)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertEqWith s "four cards in hand before, Reunion included" (S.handSize S.alice gs) 4
      Spec.assertBool s (S.castable S.alice reunion gs) "castable with no mana at all"
      Spec.assertEqWith s "one card left in hand, so both discards were paid" (S.handSize S.alice cast) 1
      Spec.assertEqWith s "two discarded cards in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 2
      Spec.assertEqWith s "and three were drawn" (S.handSize S.alice resolved) 4
    -- The other half of CR 118.9d, one card apart: the additional cost is still
    -- a cost, so a hand that cannot pay it cannot cast the spell however free
    -- the mana part is. Both boards carry the grant and no mana.
    Spec.it s "CR 118.9d a hand that cannot pay the additional cost still cannot cast it" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, bare) = omniscienceBoard mountain omniscience catharticReunion 0 True
          withN n = List.foldl' (\g _ -> snd (S.addHandCard piker S.alice g)) bare [1 .. (n :: Int)]
      Spec.assertBool s (not (S.castable S.alice reunion (withN 1))) "one spare card cannot pay a discard of two"
      Spec.assertBool s (not (any (S.isCastOf reunion) (Action.legalActions S.alice (withN 1)))) "and no Cast is offered"
      Spec.assertBool s (S.castable S.alice reunion (withN 2)) "two spare cards can"
    -- CR 107.3b: "if ... an effect lets that player cast that spell while paying
    -- neither its mana cost nor an alternative cost that includes X, then the
    -- only legal choice for X is 0". It falls out of the cost this grant offers
    -- -- an empty ManaCost has no variable -- so Cast.castProposed never reaches
    -- its ChooseX at all.
    --
    -- ONE board and two answerers, so mana, seats, timing and stock cannot be
    -- the difference: both candidates are payable and the only thing that varies
    -- is which cost CR 601.2b's announcement names.
    Spec.it s "CR 107.3b a spell cast under the grant announces no X, where its printed cost does" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      blaze <- S.printingOf s registry "Blaze"
      let (spell, gs) = omniscienceBoard mountain omniscience blaze 2 True
          -- CR 601.2b's announcement, answered by naming a cost: the printed
          -- {X}{R} and the grant's {0} share no reading. X is answered 1 rather
          -- than 0 wherever it is asked, so the damage tells the two apart.
          paying :: [ManaSymbol.ManaSymbol] -> Prompt.Prompt r -> r
          paying wanted p = case p of
            Prompt.ChooseCost _ _ _ candidates ->
              Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just (ManaCost.MkManaCost wanted)) . Cost.Type.mana) candidates)
            Prompt.ChooseX {} -> 1
            _ -> S.identityAnswer p
          red = ManaSymbol.OfType (ManaType.Colored Color.Red)
          payingPrinted, payingGrant :: Prompt.Prompt r -> r
          payingPrinted = paying [ManaSymbol.Variable, red]
          payingGrant = paying []
          resolveWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
          resolveWith answer = S.runPure answer (S.runPure answer gs (S.cast S.alice spell)) Stack.resolveTop
          responsesFor :: (forall r. Prompt.Prompt r -> r) -> [Response.Response]
          responsesFor answer = answersFor answer gs (S.cast S.alice spell)
          wasAskedForX :: [Response.Response] -> Bool
          wasAskedForX = any (\r -> case r of Response.ChoseX _ -> True; _ -> False)
      Spec.assertBool s (wasAskedToChooseCost (responsesFor payingGrant)) "both costs are payable, so the choice is real"
      Spec.assertBool s (wasAskedForX (responsesFor payingPrinted)) "the printed {X}{R} asks for X"
      Spec.assertBool s (not (wasAskedForX (responsesFor payingGrant))) "the grant's {0} does not"
      Spec.assertEqWith s "so the printed cast deals its announced 1" (S.lifeOf S.alice (resolveWith payingPrinted)) (Just 19)
      Spec.assertEqWith s "and the free cast deals 0 (identityAnswer targets the lowest recipient)" (S.lifeOf S.alice (resolveWith payingGrant)) (Just 20)

-- alice holds Thrasta, Tempest's Roar ({10}{G}{G}) and `elves` copies of
-- Glistener Elf ({G}), with `forests` untapped Forests and priority in her own
-- precombat main phase. bob is the second seat every fixture in this file has.
--
-- ONE land type, so the mana a case leaves for Thrasta is a subtraction and not
-- a colour puzzle: each Elf taps one Forest, and what is left is what CR 601.2f's
-- total is measured against. The Elf is a vanilla 1/1 with one keyword and no
-- targets, so casting and resolving it changes nothing but the count.
thrastaBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
thrastaBoard forest glistenerElf thrastaPrinting forests elves =
  let base = S.landsInPlay forest forests
      (thrasta, gs1) = S.addHandCard thrastaPrinting S.alice base
      addElf (oids, gs) _ = let (oid, gs') = S.addHandCard glistenerElf S.alice gs in (oid : oids, gs')
      (elfIds, gs2) = List.foldl' addElf ([], gs1) [1 .. elves]
   in ( thrasta,
        reverse elfIds,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Cast each of those Elves and resolve it, so that CR 601.2i has filed one
-- GameEvent.SpellCast per Elf by the time Thrasta is priced.
castElves :: [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
castElves elfIds gs0 =
  let one gs oid = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast S.alice oid)) Stack.resolveTop
   in List.foldl' one gs0 elfIds

-- CR 601.2f's cost reduction where the AMOUNT scales with a count. Thrasta,
-- Tempest's Roar is {10}{G}{G} and reads "This spell costs {3} less to cast for
-- each other spell cast this turn", so its mana total steps 12, 9, 6, 3, 2, 2 ...
-- as the count climbs.
--
-- Every leg reads that total off S.tappedCount and not off castability alone: the
-- Forests the payment taps are what tells "reduced once" from "reduced twice",
-- from "not reduced at all", and from a count that swept Thrasta into its own
-- tally. Those four readings give four different numbers on each board below,
-- which is what the Forest counts were chosen for.
--
-- "OTHER" is a clause pawl writes nowhere: CR 601.2i files the cast event after
-- CR 601.2f has totalled, so the spell being priced is never in its own count.
-- The self-counting reading is what would show up if it were.
thrastaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
thrastaSpec s registry =
  Spec.describe s "Thrasta, Tempest's Roar" $ do
    Spec.it s "Thrasta is a {10}{G}{G} that reduces its own cost by {3} for each spell cast" $ do
      thrastaPrinting <- S.printingOf s registry "Thrasta, Tempest's Roar"
      let face = S.combinedFace thrastaPrinting
      Spec.assertEqWith
        s
        "the printed mana cost"
        (Face.manaCost face)
        (Just (ManaCost.MkManaCost (ManaSymbol.Generic 10 : replicate 2 (ManaSymbol.OfType (ManaType.Colored Color.Green)))))
      Spec.assertEqWith
        s
        "one self-reduction of {3} per spell cast this turn"
        (Face.costReductions face)
        [ CostReduction.MkCostReduction
            (ManaCost.MkManaCost [ManaSymbol.Generic 3])
            (Quantity.Type.Count (Count.Type.MkCount (Scope.InHistory EventShape.SpellCast) (Filter.Type.And []) Aggregation.Members))
        ]
    -- Nine Forests. Two Elves tap two of them and leave seven, so an UNREDUCED
    -- Thrasta (twelve) is out of reach and a once-reduced one ({4}{G}{G}, six) is
    -- not -- and the nine were chosen so that one Forest is left over, which is
    -- what an over-tapping payment could not produce. The tapped count separates
    -- every reading: 2+6 here, 2+2 for a reduction applied twice, 2+3 for a count
    -- that swept Thrasta in as a third spell.
    Spec.it s "CR 601.2f two other spells this turn take {6} off, and the total is what pays" $ do
      forest <- S.printingOf s registry "Forest"
      glistenerElf <- S.printingOf s registry "Glistener Elf"
      thrastaPrinting <- S.printingOf s registry "Thrasta, Tempest's Roar"
      let (thrasta, elfIds, gs) = thrastaBoard forest glistenerElf thrastaPrinting 9 2
          after = castElves elfIds gs
          cast = S.runPure S.identityAnswer after (S.cast S.alice thrasta)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertBool s (not (S.castable S.alice thrasta gs)) "before either Elf, the unreduced {10}{G}{G} is out of reach"
      Spec.assertEqWith s "two Elves cost two Forests" (S.tappedCount S.alice after) 2
      Spec.assertBool s (S.castable S.alice thrasta after) "after them, Thrasta is offered"
      Spec.assertEqWith s "and six more Forests paid for it, leaving one" (S.tappedCount S.alice resolved) 8
      Spec.assertEqWith
        s
        "Thrasta resolved onto the battlefield"
        (namesIn Zone.Battlefield resolved)
        (fmap (CardName.MkCardName . Text.pack) (replicate 9 "Forest" <> replicate 2 "Glistener Elf" <> ["Thrasta, Tempest's Roar"]))
    -- The negative, and the SAME board with one thing changed: one Elf instead of
    -- two. The same nine Forests, the same seats, phase and priority -- so the
    -- refusal is the {3} the second spell would have taken off and nothing else.
    -- One reduction leaves {7}{G}{G}, nine, against the eight Forests a single
    -- Elf leaves untapped.
    Spec.it s "CR 601.2f one other spell takes only {3} off, and that does not pay" $ do
      forest <- S.printingOf s registry "Forest"
      glistenerElf <- S.printingOf s registry "Glistener Elf"
      thrastaPrinting <- S.printingOf s registry "Thrasta, Tempest's Roar"
      let (thrasta, elfIds, gs) = thrastaBoard forest glistenerElf thrastaPrinting 9 1
          after = castElves elfIds gs
      Spec.assertEqWith s "one Elf cost one Forest" (S.tappedCount S.alice after) 1
      Spec.assertBool s (not (S.castable S.alice thrasta after)) "Thrasta is refused"
      Spec.assertEqWith s "and not offered" (filter (S.isCastOf thrasta) (Action.legalActions S.alice after)) []
    -- CR 601.2f's floor. Four Elves is a {12} reduction against a {10} generic
    -- component, so the generic part bottoms out at {0} and the two green symbols
    -- are untouched: Thrasta costs {G}{G}. Two more Forests pay it, for 4+2 --
    -- where a reduction that carried its surplus onto the coloured symbols would
    -- leave {0} and stop at 4.
    Spec.it s "CR 601.2f a reduction larger than the generic component floors at {0}" $ do
      forest <- S.printingOf s registry "Forest"
      glistenerElf <- S.printingOf s registry "Glistener Elf"
      thrastaPrinting <- S.printingOf s registry "Thrasta, Tempest's Roar"
      let (thrasta, elfIds, gs) = thrastaBoard forest glistenerElf thrastaPrinting 9 4
          after = castElves elfIds gs
          cast = S.runPure S.identityAnswer after (S.cast S.alice thrasta)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertEqWith s "four Elves cost four Forests" (S.tappedCount S.alice after) 4
      Spec.assertBool s (S.castable S.alice thrasta after) "Thrasta is offered"
      Spec.assertEqWith s "and exactly two more Forests paid the {G}{G}" (S.tappedCount S.alice resolved) 6
    -- The colour half of that floor, as a pair of boards differing in ONE Forest:
    -- four Elves either way, so the reduction is the same {12}. Five Forests
    -- leaves one green source for a {G}{G} and refuses; six leaves two and pays.
    -- A reduction that had spilled onto the green symbols would make the first
    -- board castable, since a {0} Thrasta needs no green at all.
    Spec.it s "CR 601.2f the reduction leaves the coloured requirement alone" $ do
      forest <- S.printingOf s registry "Forest"
      glistenerElf <- S.printingOf s registry "Glistener Elf"
      thrastaPrinting <- S.printingOf s registry "Thrasta, Tempest's Roar"
      let board forests =
            let (thrasta, elfIds, gs) = thrastaBoard forest glistenerElf thrastaPrinting forests 4
             in (thrasta, castElves elfIds gs)
          (fiveThrasta, five) = board 5
          (sixThrasta, six) = board 6
      Spec.assertBool s (not (S.castable S.alice fiveThrasta five)) "one green source left is not two"
      Spec.assertEqWith s "and Thrasta is not offered" (filter (S.isCastOf fiveThrasta) (Action.legalActions S.alice five)) []
      Spec.assertBool s (S.castable S.alice sixThrasta six) "the sixth Forest is the whole difference"

-- alice controls a Safehold Sentry and three Plains, all settled. `tapped` says
-- whether the Sentry itself starts tapped -- which for a {Q} cost is the payable
-- state, the exact inverse of every {T} fixture in this file.
sentryBoard :: Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, GameState.GameState)
sentryBoard plains safeholdSentry tapped =
  let base = S.landsInPlay plains 3
      (sentry, gs1) = S.addCreature safeholdSentry S.alice base
      turnTapped o = if tapped then o {Object.tapped = TapState.Tapped} else o
   in ( sentry,
        gs1
          { GameState.objects = Map.adjust turnTapped sentry (GameState.objects gs1),
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Safehold Sentry {1}{W} Creature -- Elf Warrior 2/2: "{2}{W}, {Q}: This creature
-- gets +0/+2 until end of turn." The card CR 107.6's untap symbol was waiting for.
safeholdSentrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
safeholdSentrySpec s registry =
  Spec.describe s "Safehold Sentry" $ do
    Spec.it s "CR 107.6 whole card: a TAPPED Sentry untaps to pay {Q} and gets +0/+2" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry True
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice sentry (theAbility safeholdSentry))
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertEqWith s "paying the cost UNTAPPED it" (fmap Object.tapped (Game.lookupObject sentry activated)) (Just TapState.Untapped)
      Spec.assertEqWith s "the three Plains paid the mana" (S.tappedCount S.alice activated) 3
      Spec.assertEqWith s "toughness 2 + 2" (Projection.toughnessOf sentry resolved) (Just 4)
      Spec.assertEqWith s "power unchanged" (Projection.powerOf sentry resolved) (Just 2)
    -- CR 107.6's second sentence, and the exact inverse of TapThis: "A permanent
    -- that's already untapped can't be untapped again to pay the cost."
    Spec.it s "CR 107.6 an UNTAPPED Sentry cannot pay {Q}, so the ability is not offered" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry False
      Spec.assertBool s (not (Cost.canPay S.alice sentry (ActivatedAbility.cost (theAbility safeholdSentry)) gs)) "canPay says no"
      Spec.assertBool s (not (any isActivateAction (Action.legalActions S.alice gs))) "and no Activate is offered"
    -- The issue itself (#204): CR 302.6 names the tap symbol AND the untap
    -- symbol, and only the first half had a producer. A summoning-sick Sentry
    -- must not be able to activate a {Q} ability.
    Spec.it s "CR 302.6 a summoning-sick Sentry's {Q} ability is not offered" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry True
          sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) sentry (GameState.objects gs)}
      Spec.assertBool s (Cost.canPay S.alice sentry (ActivatedAbility.cost (theAbility safeholdSentry)) sick) "the cost itself is still payable -- it is tapped"
      Spec.assertBool s (not (any isActivateAction (Action.legalActions S.alice sick))) "but CR 302.6 withholds the ability"

isActivateAction :: Action.Type.Action -> Bool
isActivateAction a = case a of
  Action.Type.Activate _ _ -> True
  _ -> False

-- alice has two untapped Mountains, holds one Cathartic Reunion plus `n` other
-- cards, and has four cards in her library so the draw of three is never a
-- CR 104.3c loss. The Village Rites board's shape, on the hand axis instead of
-- the battlefield one.
catharticBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
catharticBoard mountain piker catharticReunion n =
  let base = S.landsInPlay mountain 2
      (reunion, gs1) = S.addHandCard catharticReunion S.alice base
      withHand = List.foldl' (\g _ -> snd (S.addHandCard piker S.alice g)) gs1 [1 .. n]
      withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) withHand [1 .. (4 :: Int)]
   in ( reunion,
        withLibrary
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Answers ChooseDiscard with nothing at all, and everything else normally. Two
-- different jobs in this group: it proves the forced case is never ASKED (an
-- empty answer would otherwise discard nothing), and it drives the
-- reject-not-repair case where the prompt is real.
noDiscardAnswer :: Prompt.Prompt r -> r
noDiscardAnswer p = case p of
  Prompt.ChooseDiscard {} -> []
  _ -> S.identityAnswer p

-- Cathartic Reunion {1}{R} Sorcery: "As an additional cost to cast this spell,
-- discard two cards. Draw three cards." The card CR 601.2f's "discarding cards"
-- clause was waiting for.
catharticReunionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
catharticReunionSpec s registry =
  Spec.describe s "Cathartic Reunion" $ do
    Spec.it s "CR 118.8 whole card: the two cards are discarded and three are drawn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      -- Exactly two other cards, so CR 701.9b has no choice to offer and the
      -- prompt is elided -- which noDiscardAnswer proves, since an answer of
      -- [] would discard nothing if the prompt were actually raised.
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 2
          cast = S.runPure noDiscardAnswer gs (S.cast S.alice reunion)
          resolved = S.runPure noDiscardAnswer cast Stack.resolveTop
      Spec.assertEqWith s "the hand emptied as the cost was paid" (S.handSize S.alice cast) 0
      Spec.assertEqWith s "two discarded cards in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 2
      Spec.assertEqWith s "three cards drawn" (S.handSize S.alice resolved) 3
      -- CR 608.2n: the sorcery joins the two discards on resolution.
      Spec.assertEqWith s "and the spell itself is there too" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 3
    Spec.it s "CR 601.2f with only one other card in hand the spell is not castable" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 1
      -- Read through costsFor, so the assertion is against the cost the engine
      -- would actually offer (mana cost plus the printed additional cost),
      -- never a hand-built one.
      Spec.assertBool s (not (any (\c -> Cost.canPay S.alice reunion c gs) (Cost.costsFor (S.printingName catharticReunion) reunion gs))) "no offered cost is payable"
      Spec.assertBool s (not (any (S.isCastOf reunion) (Action.legalActions S.alice gs))) "and no Cast is offered"
    Spec.it s "CR 601.2h an undersized answer leaves the whole cast unpaid, not partly paid" $ do
      -- The COST path's reject-not-repair, and deliberately the opposite of what
      -- the Discard EFFECT does after #245: a cost may go unpaid, so Pawl.Engine.Cost.pay
      -- restores the entry state and nothing at all happened. Three other cards
      -- makes the prompt real (hand > count), unlike the forced case above.
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 3
          cast = S.runPure noDiscardAnswer gs (S.cast S.alice reunion)
      Spec.assertEqWith s "nothing was discarded" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 0
      Spec.assertEqWith s "the hand is untouched, Reunion included" (S.handSize S.alice cast) 4
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and the Mountains are untapped again" (S.tappedCount S.alice cast) 0
    -- Where the reject boundary actually falls, stated rather than inferred.
    -- The answer is read as a SET, so a duplicate is normalised, not repaired:
    -- naming two distinct cards across three entries pays, and naming one card
    -- twice does not. Prompt.ChooseSacrifices is answered with a Set, so an
    -- interpreter meaning [a,a,b] there builds {a,b} and the Sacrifice arm
    -- accepts it -- this keeps the two components answering alike.
    Spec.it s "CR 601.2h the answer is read as a set: [a,a,b] pays for two, [a,a] does not" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 3
          -- Repeat the first offered card, then add a second distinct one.
          duplicateThenDistinct q = case q of
            Prompt.ChooseDiscard _ _ ids _ -> case ids of
              a : b : _ -> [a, a, b]
              _ -> []
            _ -> S.identityAnswer q
          -- The same card twice and nothing else: one distinct card for a
          -- count of two.
          sameCardTwice q = case q of
            Prompt.ChooseDiscard _ _ ids _ -> concat (replicate 2 (take 1 ids))
            _ -> S.identityAnswer q
          paid = S.runPure duplicateThenDistinct gs (S.cast S.alice reunion)
          unpaid = S.runPure sameCardTwice gs (S.cast S.alice reunion)
      Spec.assertEqWith s "[a,a,b] names two distinct cards, so the cost is paid" (length (Game.zoneMembers Zone.Graveyard S.alice paid)) 2
      Spec.assertEqWith s "and the spell is on the stack" (length (GameState.stack paid)) 1
      Spec.assertEqWith s "[a,a] names one, so nothing is discarded" (length (Game.zoneMembers Zone.Graveyard S.alice unpaid)) 0
      Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack unpaid)) 0

-- alice has one untapped Mountain, holds Magmatic Insight plus `second` plus a
-- Goblin Piker, and has four cards in her library so the draw of two is never a
-- CR 104.3c loss. The catharticBoard's shape with `second` as the only variable:
-- a Forest makes the cost payable and a second Piker makes it unpayable, and the
-- two boards agree on everything else -- one red source, one seat, the same
-- phase, the same hand SIZE.
magmaticBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
magmaticBoard mountain piker magmaticInsight second =
  let base = S.landsInPlay mountain 1
      (insight, gs1) = S.addHandCard magmaticInsight S.alice base
      gs2 = snd (S.addHandCard second S.alice gs1)
      (other, gs3) = S.addHandCard piker S.alice gs2
      withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) gs3 [1 .. (4 :: Int)]
   in ( insight,
        other,
        withLibrary
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Magmatic Insight {R} Sorcery: "As an additional cost to cast this spell,
-- discard a land card. Draw two cards." The card the discard cost's criterion
-- was waiting for (#1620) -- Cathartic Reunion's cost names no quality, so
-- before this one the field had nothing to narrow.
magmaticInsightSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magmaticInsightSpec s registry =
  Spec.describe s "Magmatic Insight" $ do
    -- The criterion decides WHICH card pays, not just how many. The Piker is in
    -- hand throughout and is never a candidate, so the elision holds (one
    -- matching card for a count of one) and noDiscardAnswer proves it: were the
    -- Piker offered, the prompt would be real and an answer of [] would leave
    -- the whole cost unpaid.
    Spec.it s "CR 118.8 whole card: the land is the card discarded, and two are drawn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      forest <- S.printingOf s registry "Forest"
      magmaticInsight <- S.printingOf s registry "Magmatic Insight"
      let (insight, other, gs) = magmaticBoard mountain piker magmaticInsight forest
          cast = S.runPure noDiscardAnswer gs (S.cast S.alice insight)
          resolved = S.runPure noDiscardAnswer cast Stack.resolveTop
      -- By NAME, since CR 400.7 mints a fresh id for the discarded card.
      Spec.assertEqWith s "the Forest, and only the Forest, paid the cost" (namesIn Zone.Graveyard cast) [Face.name (S.combinedFace forest)]
      Spec.assertBool s (elem other (Game.zoneMembers Zone.Hand S.alice cast)) "and the Piker, which the criterion excludes, is still in hand"
      Spec.assertEqWith s "the spell is on the stack" (length (GameState.stack cast)) 1
      -- One card left in hand before the draw (the Piker), plus two drawn.
      Spec.assertEqWith s "two cards drawn" (S.handSize S.alice resolved) 3
    -- The negative, one card different: a hand with no land card at all. Same
    -- Mountain, same hand size, same phase -- so an unpayable cost is the only
    -- thing that can withhold the cast.
    Spec.it s "CR 601.2f a landless hand cannot pay, however many cards it holds" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      magmaticInsight <- S.printingOf s registry "Magmatic Insight"
      let (insight, _, gs) = magmaticBoard mountain piker magmaticInsight piker
      Spec.assertEqWith s "the hand is the same size as the payable board's" (S.handSize S.alice gs) 3
      Spec.assertBool s (not (any (\c -> Cost.canPay S.alice insight c gs) (Cost.costsFor (S.printingName magmaticInsight) insight gs))) "no offered cost is payable"
      Spec.assertBool s (not (any (S.isCastOf insight) (Action.legalActions S.alice gs))) "and no Cast is offered"

-- Springleaf Drum {1} Artifact: "{T}, Tap an untapped creature you control: Add
-- one mana of any color." The gate card for CostComponent's TapPermanents --
-- CR 601.2f's "tapping permanents" with a COUNT, which CR 702.122a's threshold
-- cannot express (it names no number of objects at all).
--
-- Alice's board is the Drum and TWO untapped creatures, one more than the cost
-- wants, so the prompt is a real choice rather than an elided one. Hill Giant
-- and Blind-Spot Giant are distinct printings, so which one was tapped is
-- visible.
--
-- Gameplay-level throughout, through Pawl.Engine.Cost.tapForMana: CR 605.3b
-- keeps a mana ability off the stack, so Activate.activatable answers False for
-- this ability on every board and no case may route through it.
springleafBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
springleafBoard drum first second =
  let (drumId, gs0) = S.addCreature drum S.alice (Setup.emptyGame S.bothPlayers)
      (firstId, gs1) = S.addCreature first S.alice gs0
      (secondId, gs2) = S.addCreature second S.alice gs1
   in (drumId, firstId, secondId, gs2 {GameState.priority = Just S.alice})

-- Answer Prompt.ChooseTaps with one named permanent, FILTERED against the
-- offer rather than hand-built: an answer the engine did not offer is rejected
-- by Cost.payComponent, so filtering is what keeps the assertion about the
-- engine's own candidates.
tapping :: ObjectId.ObjectId -> Prompt.Prompt r -> r
tapping wanted p = case p of
  Prompt.ChooseTaps _ _ _ candidates _ -> Set.fromList (filter (== wanted) candidates)
  _ -> S.identityAnswer p

-- Answer Prompt.ChooseTaps with nothing at all, which is not a size-1 subset --
-- the reject-not-repair probe.
tappingNothing :: Prompt.Prompt r -> r
tappingNothing p = case p of
  Prompt.ChooseTaps {} -> Set.empty
  _ -> S.identityAnswer p

-- How many mana this source put in alice's pool. The observable that says the
-- cost was PAID: Cost.pay restores the entry state for an unpaid one, so a
-- refused payment adds nothing and taps nothing.
pooledFrom :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Int
pooledFrom answer oid gs = case Game.poolOf S.alice (S.runPure answer gs (Cost.tapForMana oid)) of
  Mana.Type.MkMana units -> length units

isTapped :: ObjectId.ObjectId -> GameState.GameState -> Bool
isTapped oid gs = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped

-- The board after tapping the Drum for mana with `answer`.
afterDrum :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
afterDrum answer drumId gs = S.runPure answer gs (Cost.tapForMana drumId)

springleafDrumSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
springleafDrumSpec s registry = Spec.describe s "Springleaf Drum" $ do
  -- CR 601.2f: the cost names HOW MANY permanents are tapped, and the payer
  -- names WHICH. Two candidates and a count of one, so the prompt is raised.
  Spec.it s "CR 601.2f the payer chooses which creature the cost taps" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (drumId, giantId, spotId, gs) = springleafBoard drum hillGiant blindSpot
        after = afterDrum (tapping giantId) drumId gs
    Spec.assertEqWith s "one mana" (pooledFrom (tapping giantId) drumId gs) 1
    Spec.assertBool s (isTapped giantId after) "the chosen creature is tapped"
    Spec.assertBool s (not (isTapped spotId after)) "the other one is not"
    -- CR 107.5's own half of the cost, which is a separate component.
    Spec.assertBool s (isTapped drumId after) "and the Drum itself is tapped"
  -- The discriminating twin: the same board, one different answer. If the
  -- engine picked a creature itself, both cases would pass.
  Spec.it s "the choice is the player's: the other answer taps the other creature" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (drumId, giantId, spotId, gs) = springleafBoard drum hillGiant blindSpot
        after = afterDrum (tapping spotId) drumId gs
    Spec.assertEqWith s "one mana all the same" (pooledFrom (tapping spotId) drumId gs) 1
    Spec.assertBool s (isTapped spotId after) "the chosen creature is tapped"
    Spec.assertBool s (not (isTapped giantId after)) "the other one is not"
  -- Reject-not-repair, Cost.payComponent's posture: an answer that is not a
  -- size-1 subset of the offer leaves the whole cost unpaid, and CR 601.2h's
  -- ban on partial payments is what makes the Drum untapped afterwards.
  Spec.it s "CR 601.2h an answer of the wrong size pays nothing at all" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (drumId, giantId, spotId, gs) = springleafBoard drum hillGiant blindSpot
        after = afterDrum tappingNothing drumId gs
    Spec.assertEqWith s "no mana" (pooledFrom tappingNothing drumId gs) 0
    Spec.assertBool s (not (isTapped giantId after)) "neither creature is tapped"
    Spec.assertBool s (not (isTapped spotId after)) "nor the other"
    Spec.assertBool s (not (isTapped drumId after)) "and the payment was rolled back whole"
  -- The negative, built as a PAIR of boards differing in exactly one thing: the
  -- creatures' tap state. Same seats, same permanents, same ability -- so an
  -- unpayable component is the only thing that can withhold the mana.
  Spec.it s "CR 118.3 no untapped creature means no payment" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (drumId, giantId, spotId, payable) = springleafBoard drum hillGiant hillGiant
        -- The ONE thing the pair varies: both creatures tapped, which leaves
        -- the criterion's `Not IsTapped` with nothing to offer.
        unpayable = S.tapObject spotId (S.tapObject giantId payable)
        component = CostComponent.TapPermanents (TapPermanents.MkTapPermanents 1 (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You, Filter.Type.Not Filter.Type.IsTapped]))
    Spec.assertBool s (Cost.canPayComponent S.alice drumId component payable) "two untapped creatures pay"
    Spec.assertBool s (not (Cost.canPayComponent S.alice drumId component unpayable)) "two tapped ones do not"
    Spec.assertEqWith s "and the ability adds no mana" (pooledFrom S.identityAnswer drumId unpayable) 0
    Spec.assertBool s (not (isTapped drumId (afterDrum S.identityAnswer drumId unpayable))) "leaving the Drum untapped"
  -- CR 302.6 and CR 107.5 gate on the tap SYMBOL in the creature's OWN
  -- activation cost. This cost taps another creature by written instruction, so
  -- summoning sickness has nothing to say about it. Both creatures arrived this
  -- turn and either can still pay.
  --
  -- What this proves is that Cost.tapCandidates does not filter on sickness:
  -- mutating it to do so turns this case red. It does NOT discriminate on
  -- Cost.requiresSicknessCheck, and cannot -- rule 302.6 gates a CREATURE's
  -- ability, and Springleaf Drum is an artifact, so adding this component to
  -- that function leaves the suite green. A creature printing this cost
  -- (Aphetto Grifter) is what would tell the two apart.
  Spec.it s "CR 302.6 a creature that arrived this turn can still be tapped for the cost" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (drumId, giantId, spotId, gs0) = springleafBoard drum hillGiant blindSpot
        sicken oid g = g {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects g)}
        gs = sicken spotId (sicken giantId gs0)
        after = afterDrum (tapping giantId) drumId gs
    Spec.assertEqWith s "one mana" (pooledFrom (tapping giantId) drumId gs) 1
    Spec.assertBool s (isTapped giantId after) "the summoning-sick creature is tapped"

-- High Perfect Morcant {2}{B}{G} 4/4 Legendary Creature -- Elf Noble: "Tap three
-- untapped Elves you control: Proliferate. Activate only as a sorcery." The
-- second producer for CostComponent.TapPermanents, and the one that exercises a
-- COUNT ABOVE ONE -- Springleaf Drum's is one, where 1 and "some" cannot be told
-- apart.
--
-- Morcant is itself an Elf and the cost does not say "another", so it is one of
-- its own candidates. Four candidates against a count of three is what makes the
-- prompt a real choice.
--
-- Not a mana ability, so unlike the Drum this one is legitimately asked of
-- Activate.activatable (CR 605.3b is what bars that for the Drum).
morcantBoard :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
morcantBoard morcant elves =
  let (morcantId, gs0) = S.addCreature morcant S.alice (Setup.emptyGame S.bothPlayers)
      add (ids, g) p = let (oid, g1) = S.addCreature p S.alice g in (ids <> [oid], g1)
      (elfIds, gs1) = foldl add ([], gs0) elves
   in ( morcantId,
        elfIds,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Answer Prompt.ChooseTaps with the named permanents, filtered against the
-- offer -- `tapping`'s posture for a count above one.
tappingAll :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
tappingAll wanted p = case p of
  Prompt.ChooseTaps _ _ _ candidates _ -> Set.fromList (filter (\c -> elem c wanted) candidates)
  _ -> S.identityAnswer p

morcantSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
morcantSpec s registry = Spec.describe s "High Perfect Morcant" $ do
  -- CR 118.3: three untapped Elves are the necessary resources. The pair varies
  -- ONE thing -- how many Elves are on the battlefield beside Morcant.
  Spec.it s "CR 118.3 three Elves are needed and two are not enough" $ do
    morcant <- S.printingOf s registry "High Perfect Morcant"
    glistener <- S.printingOf s registry "Glistener Elf"
    hunter <- S.printingOf s registry "Elvish Hunter"
    let (enoughId, _, enough) = morcantBoard morcant [glistener, hunter]
        (shortId, _, short) = morcantBoard morcant [glistener]
    -- Morcant is an Elf and the cost does not say "another", so it counts
    -- itself: two other Elves make three candidates.
    Spec.assertBool s (Activate.activatable S.alice enoughId (theAbility morcant) enough) "Morcant and two Elves: activatable"
    Spec.assertBool s (not (Activate.activatable S.alice shortId (theAbility morcant) short)) "Morcant and one Elf: not"
  -- The payment: four candidates, three tapped, and the payer says which three.
  -- Morcant itself is a candidate and is the one left untapped here, so a case
  -- that tapped "the first three" would still pass -- which is why the twin
  -- below leaves a different Elf untapped.
  Spec.it s "the payer chooses which three of the four Elves are tapped" $ do
    morcant <- S.printingOf s registry "High Perfect Morcant"
    glistener <- S.printingOf s registry "Glistener Elf"
    hunter <- S.printingOf s registry "Elvish Hunter"
    augur <- S.printingOf s registry "Llanowar Augur"
    let (morcantId, elfIds, gs) = morcantBoard morcant [glistener, hunter, augur]
        after = S.runPure (tappingAll elfIds) gs (Activate.activateAbility S.alice morcantId (theAbility morcant))
    Spec.assertBool s (all (`isTapped` after) elfIds) "the three chosen Elves are tapped"
    Spec.assertBool s (not (isTapped morcantId after)) "and Morcant, which was offered too, is not"
    Spec.assertEqWith s "the ability is on the stack" (length (GameState.stack after)) 1
  -- The discriminating twin: the same board, a different three. Morcant pays
  -- this time and one Elf is spared.
  Spec.it s "the choice is the player's: a different three leaves a different Elf untapped" $ do
    morcant <- S.printingOf s registry "High Perfect Morcant"
    glistener <- S.printingOf s registry "Glistener Elf"
    hunter <- S.printingOf s registry "Elvish Hunter"
    augur <- S.printingOf s registry "Llanowar Augur"
    let (morcantId, elfIds, gs) = morcantBoard morcant [glistener, hunter, augur]
        spared = last elfIds
        after = S.runPure (tappingAll (morcantId : filter (/= spared) elfIds)) gs (Activate.activateAbility S.alice morcantId (theAbility morcant))
    Spec.assertBool s (isTapped morcantId after) "Morcant paid this time"
    Spec.assertBool s (not (isTapped spared after)) "and the Elf left out is untapped"
    Spec.assertEqWith s "the ability is on the stack" (length (GameState.stack after)) 1

-- Unerring Sling {3} Artifact: "{3}, {T}, Tap an untapped creature you control:
-- This artifact deals damage equal to the tapped creature's power to target
-- attacking or blocking creature with flying." The producer for
-- Binding.tappedPermanent, and the first card in `data/cards/` whose ability
-- reads a characteristic of what its OWN cost tapped.
--
-- alice controls the Sling, a Decorated Griffin (2/3 flier) and a Hill Giant
-- (3/3), plus exactly three untapped Forests -- the minimum that pays {3}, so
-- the mana window has nothing to decide. bob defends with nothing, CR 506.2's
-- defending player being all combat needs here.
--
-- The Griffin attacks, which taps it (CR 508.1f) and makes it the only creature
-- the ability's target filter admits (CR 508.1k, and it is the only flier). That
-- leaves the Hill Giant as the ONLY candidate the cost's `Not IsTapped` criterion
-- offers, so Cost.payComponent elides Prompt.ChooseTaps and no answerer picks
-- the creature whose power this case reads.
slingBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
slingBoard sling griffin hillGiant forest =
  let (combat, ours, _) = S.combatBoardOf [griffin, hillGiant] []
      (griffinId, giantId) = case ours of
        [a, b] -> (a, b)
        _ -> (S.noSource, S.noSource)
      (slingId, withSling) = S.addCreature sling S.alice combat
      withLands = S.landsFor forest S.alice 3 withSling
   in (slingId, griffinId, giantId, S.runPure (attackingWith griffinId) withLands (Combat.declareAttackers S.alice))

-- Attack with one named creature, FILTERED against the offer: an id the engine
-- did not offer is not a legal declaration, so filtering keeps the case about
-- the engine's own candidates.
attackingWith :: ObjectId.ObjectId -> Prompt.Prompt r -> r
attackingWith wanted p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== wanted) ids
  _ -> S.identityAnswer p

-- Target the named creature, FILTERED out of the offered recipients rather than
-- hand-built: a hand-built ToObject of the same permanent is a different
-- recipient and CR 608.2b would drop it at resolution with no error.
targeting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
targeting victim p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) asked
  _ -> S.identityAnswer p

unerringSlingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
unerringSlingSpec s registry = Spec.describe s "Unerring Sling" $ do
  -- CR 601.2f pays the cost by tapping a creature and CR 608.2h reads that
  -- creature's power as the ability resolves -- CURRENT information, the Giant
  -- never having left the battlefield.
  --
  -- Three implementations, three readings, which is what makes the damage the
  -- discriminating assertion: an unbound slot deals nothing (Quantity.evaluateFor
  -- answers Nothing and Resolve drops the recipient), a slot aimed at the SOURCE
  -- reads an artifact's absent power, and one aimed at the target reads the
  -- Griffin's own 2. Only the tapped Giant's 3 is lethal to a 2/3.
  Spec.it s "CR 608.2h the ability reads the power of the creature its own cost tapped" $ do
    sling <- S.printingOf s registry "Unerring Sling"
    griffin <- S.printingOf s registry "Decorated Griffin"
    hillGiant <- S.printingOf s registry "Hill Giant"
    forest <- S.printingOf s registry "Forest"
    let (slingId, griffinId, giantId, gs) = slingBoard sling griffin hillGiant forest
        activated = S.runPure (targeting griffinId) gs (Activate.activateAbility S.alice slingId (theAbility sling))
        resolved = S.runPure (targeting griffinId) activated Stack.resolveTop
        settled = S.settleSba resolved
    Spec.assertEqWith s "the Griffin took the tapped Hill Giant's 3 power" (S.damageOf griffinId resolved) (Just 3)
    Spec.assertBool s (not (S.onBattlefield griffinId settled)) "CR 704.5g so 3 damage on a 2/3 is lethal"
    Spec.assertBool s (isTapped giantId activated) "the cost really tapped the Giant"
    Spec.assertBool s (isTapped slingId activated) "and CR 107.5's own half of the cost tapped the Sling"
    Spec.assertEqWith s "the ability was on the stack with the payment already made" (length (GameState.stack activated)) 1
  -- The discriminating twin, a pair of boards differing in exactly one thing:
  -- WHICH creature the cost taps. Swapping the Hill Giant for a 1/1 changes the
  -- damage and nothing else -- so a fix that read a constant, the source or the
  -- target would give the same number on both boards.
  Spec.it s "CR 601.2f a different tapped creature is a different amount of damage" $ do
    sling <- S.printingOf s registry "Unerring Sling"
    griffin <- S.printingOf s registry "Decorated Griffin"
    elf <- S.printingOf s registry "Glistener Elf"
    forest <- S.printingOf s registry "Forest"
    let (slingId, griffinId, elfId, gs) = slingBoard sling griffin elf forest
        activated = S.runPure (targeting griffinId) gs (Activate.activateAbility S.alice slingId (theAbility sling))
        resolved = S.runPure (targeting griffinId) activated Stack.resolveTop
    Spec.assertEqWith s "the Elf's 1 power, not the Giant's 3" (S.damageOf griffinId resolved) (Just 1)
    Spec.assertBool s (S.onBattlefield griffinId (S.settleSba resolved)) "so the 2/3 Griffin survives"
    Spec.assertBool s (isTapped elfId activated) "the cost tapped the Elf"

-- alice with Everbark Shaman settled on the battlefield, `buried` in her own
-- graveyard, and Maskwood Nexus beside the Shaman when `withNexus`. Priority in
-- her own precombat main phase and an empty stack, on every board.
--
-- The Shaman's ability costs {T} and an exile, with an EMPTY mana part, so no
-- case here can turn on mana. Maskwood Nexus is the only thing a case varies
-- besides what is buried.
everbarkBoard :: Printing.Printing -> Printing.Printing -> Bool -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
everbarkBoard shaman nexus withNexus buried =
  let (shamanId, gs0) = S.addCreature shaman S.alice (Setup.emptyGame S.bothPlayers)
      gs1 = if withNexus then snd (S.addCreature nexus S.alice gs0) else gs0
      gs2 = List.foldl' (\gs printing -> snd (S.addGraveyardCard printing S.alice gs)) gs1 buried
   in ( shamanId,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Everbark Shaman {4}{G} Creature -- Treefolk Shaman 3/5: "{T}, Exile a Treefolk
-- card from your graveyard: Search your library for up to two Forest cards, put
-- them onto the battlefield tapped, then shuffle."
--
-- The producer for CR 613.1 read by Cost.exileCandidates. Maskwood Nexus makes
-- each creature card alice owns off the battlefield every creature type (CR
-- 613.1d, layer 4), so a Goblin Piker in her graveyard is a Treefolk and pays a
-- cost its printed type line refuses.
everbarkShamanSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
everbarkShamanSpec s registry =
  Spec.describe s "Everbark Shaman" $ do
    -- Three readings the pair below separates. The Piker is a printed Goblin
    -- Warrior, so "the card was always a Treefolk" pays on the Nexus-less board
    -- too. It is buried before the Nexus is seated, so "the effect applied to it
    -- as it arrived" pays on neither. Only a continuous effect applying to a card
    -- sitting in a graveyard pays on exactly one.
    Spec.it s "CR 613.1d Maskwood Nexus makes a graveyard Goblin a Treefolk, and Everbark Shaman's cost takes it" $ do
      shaman <- S.printingOf s registry "Everbark Shaman"
      nexus <- S.printingOf s registry "Maskwood Nexus"
      piker <- S.printingOf s registry "Goblin Piker"
      let (shamanId, gs) = everbarkBoard shaman nexus True [piker]
          ability = theAbility shaman
      Spec.assertEqWith s "the cost has no mana in it at all" (Cost.Type.mana (ActivatedAbility.cost ability)) (Just (ManaCost.MkManaCost []))
      Spec.assertBool s (Activate.activatable S.alice shamanId ability gs) "activatable"
      Spec.assertBool s (any (isActivateOf shamanId) (Action.legalActions S.alice gs)) "and menued"
      let after = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice shamanId ability)
      Spec.assertEqWith s "CR 602.2b the Piker was exiled to pay" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
      Spec.assertEqWith s "and the graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    -- The negative half, differing in exactly one permanent: no Nexus. Same
    -- graveyard, same Shaman, same empty mana cost.
    Spec.it s "CR 118.3 without the Nexus the printed Goblin is no Treefolk and the cost is unpayable" $ do
      shaman <- S.printingOf s registry "Everbark Shaman"
      nexus <- S.printingOf s registry "Maskwood Nexus"
      piker <- S.printingOf s registry "Goblin Piker"
      let (shamanId, gs) = everbarkBoard shaman nexus False [piker]
      Spec.assertBool s (not (Activate.activatable S.alice shamanId (theAbility shaman) gs)) "not activatable"
      Spec.assertBool s (not (any (isActivateOf shamanId) (Action.legalActions S.alice gs))) "and not menued"
    -- The other discriminator, differing from the positive case in exactly one
    -- buried card: Maskwood Nexus's set is CREATURE cards, so a Lightning Bolt in
    -- the graveyard is outside it and stays no Treefolk. Rules out "the Nexus
    -- makes every graveyard card every creature type".
    Spec.it s "CR 613.1d the Nexus leaves an instant in that graveyard alone" $ do
      shaman <- S.printingOf s registry "Everbark Shaman"
      nexus <- S.printingOf s registry "Maskwood Nexus"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (shamanId, gs) = everbarkBoard shaman nexus True [bolt]
      Spec.assertBool s (not (Activate.activatable S.alice shamanId (theAbility shaman) gs)) "not activatable"
      Spec.assertEqWith s "and the Bolt is still buried" (length (Game.zoneMembers Zone.Graveyard S.alice gs)) 1

-- alice with a face-down Putrid Raptor on the battlefield, `held` in hand, and
-- Maskwood Nexus beside it when `withNexus`. Three Mountains pay CR 702.37a's
-- {3} for the face-down cast; the morph cost itself has no mana in it, so the
-- turn-up gate cannot be turning on mana. Nothing if the face-down cast did not
-- land.
raptorBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> [Printing.Printing] -> Maybe (ObjectId.ObjectId, GameState.GameState)
raptorBoard mountain raptor nexus withNexus held =
  let (gs0, card) = S.handOne raptor (S.landsInPlay mountain 3)
      gs1 = if withNexus then snd (S.addCreature nexus S.alice gs0) else gs0
      gs2 = List.foldl' (\gs printing -> snd (S.addHandCard printing S.alice gs)) gs1 held
      before = Set.toList (GameState.battlefield gs2)
      after =
        S.runPure
          S.identityAnswer
          gs2
          (Cast.castSpell S.alice card (S.printingName raptor) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
      entered = Set.lookupMin (Set.difference (GameState.battlefield after) (Set.fromList before))
   in fmap (\permanent -> (permanent, after)) entered

-- Putrid Raptor {4}{B}{B} Creature -- Zombie Dinosaur Beast 4/4: "Morph--Discard
-- a Zombie card."
--
-- The producer for CR 613.1 read by Cost.discardCandidates. Maskwood Nexus makes
-- each creature card alice owns off the battlefield every creature type (CR
-- 613.1d, layer 4), so a Goblin Piker in her HAND is a Zombie and pays a morph
-- cost its printed type line refuses.
putridRaptorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
putridRaptorSpec s registry =
  Spec.describe s "Putrid Raptor" $ do
    -- The pair separates the same three readings the Shaman's does. The Piker is
    -- a printed Goblin Warrior; it is in hand before the Nexus is seated; and the
    -- Lightning Bolt beside it is a second card in hand on every board, so the
    -- gate is not "the hand is empty".
    Spec.it s "CR 613.1d Maskwood Nexus makes a Goblin card in hand a Zombie, and Putrid Raptor's morph cost takes it" $ do
      mountain <- S.printingOf s registry "Mountain"
      raptor <- S.printingOf s registry "Putrid Raptor"
      nexus <- S.printingOf s registry "Maskwood Nexus"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      case raptorBoard mountain raptor nexus True [piker, bolt] of
        Nothing -> Spec.assertFailure s "the morph cast of Putrid Raptor did not reach the battlefield"
        Just (permanent, gs) -> do
          Spec.assertEqWith s "CR 708.2a a 2/2 while face down" (S.powerToughnessOf permanent gs) (Just (2, 2))
          Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice gs) [(permanent, TurnUpProcedure.Morph)]
          let after = S.runPure S.identityAnswer gs (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent)
          Spec.assertEqWith s "CR 702.37e it is face up" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)
          Spec.assertEqWith s "and the printed 4/4" (S.powerToughnessOf permanent after) (Just (4, 4))
          Spec.assertEqWith s "CR 701.9a one card was discarded to pay" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- The negative half, differing in exactly one permanent: no Nexus. Same hand,
    -- same face-down Raptor, same lands.
    Spec.it s "CR 702.37e without the Nexus no card in that hand is a Zombie and the morph cost is unpayable" $ do
      mountain <- S.printingOf s registry "Mountain"
      raptor <- S.printingOf s registry "Putrid Raptor"
      nexus <- S.printingOf s registry "Maskwood Nexus"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      case raptorBoard mountain raptor nexus False [piker, bolt] of
        Nothing -> Spec.assertFailure s "the morph cast of Putrid Raptor did not reach the battlefield"
        Just (permanent, gs) -> do
          Spec.assertEqWith s "CR 708.2a a 2/2 while face down" (S.powerToughnessOf permanent gs) (Just (2, 2))
          Spec.assertEqWith s "CR 702.37e the action is withheld" (FaceDown.turnableFaceUp S.alice gs) []
          Spec.assertEqWith s "and the hand still holds both cards" (length (Game.zoneMembers Zone.Hand S.alice gs)) 2
    -- The other discriminator, differing from the positive case in the hand's
    -- contents alone: Maskwood Nexus's set is CREATURE cards, so a hand of two
    -- Lightning Bolts is outside it even with the Nexus out.
    Spec.it s "CR 613.1d the Nexus leaves the instants in that hand alone" $ do
      mountain <- S.printingOf s registry "Mountain"
      raptor <- S.printingOf s registry "Putrid Raptor"
      nexus <- S.printingOf s registry "Maskwood Nexus"
      bolt <- S.printingOf s registry "Lightning Bolt"
      case raptorBoard mountain raptor nexus True [bolt, bolt] of
        Nothing -> Spec.assertFailure s "the morph cast of Putrid Raptor did not reach the battlefield"
        Just (permanent, gs) -> do
          Spec.assertEqWith s "CR 708.2a a 2/2 while face down" (S.powerToughnessOf permanent gs) (Just (2, 2))
          Spec.assertEqWith s "CR 702.37e the action is withheld" (FaceDown.turnableFaceUp S.alice gs) []
